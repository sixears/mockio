{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE ViewPatterns         #-}

{- | Format Text or Strings, in a type-safe way, using Quasi Quotations.

    @    
    import Text.Fmt   ( fmt )
    import Data.Text  ( pack )

    -- fmt produces Text or String per type inference
    [fmt|This is some %t|] (pack "text") == "This is some text"
    @

    A module to provide type-safe printf-alike functionality in Haskell.  __Note
    that both the QuasiQuotes and OverloadedStrings extensions are necessary in
    practice__, frankly I'm not sure why OverloadedStrings is.  Without it, you
    may get errors a bit like this:

    @
      • Couldn't match expected type ‘Format (Integer → Text) (Int → t)’
                  with actual type ‘[Char]’
      • In the first argument of ‘(%)’, namely
    @
 -}

module Text.Fmt2
  ( -- * Format Specifiers

    -- $formatting

    ByteFmtBase(..), FormatTarget(..)
  , fmt, fmtS, fmtL, fmtT, formatBytes
  -- for testing only
  , Token(..), conversion, fill, sprintf, tokens )
where

import Prelude ( Double, Int, Integer, Integral, Real, RealFloat
               , (+), (-), (/), (^), (**)
               , abs, decodeFloat, error, floor, fromIntegral, toInteger
               )

-- base --------------------------------

import Control.Applicative    ( (<*), (<*>), (*>), (<|>), many )
import Data.Char              ( Char, toUpper )
import Data.Either            ( Either( Left, Right ) )
import Data.Eq                ( Eq, (==) )
import Data.Foldable          ( Foldable, foldr, toList )
import Data.Function          ( (.), ($), const, id )
import Data.Functor           ( (<$>), fmap )
import Data.List              ( concat, elem )
import Data.Maybe             ( Maybe( Just, Nothing ) )
import Data.Monoid            ( (<>) )
import Data.Ord               ( (<), (>) )
import Data.String            ( String )
import Data.Word              ( Word8 )
import Numeric                ( logBase )
import Numeric.Natural        ( Natural )
import Text.Read              ( read )
import Text.Show              ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toText )

-- formatting --------------------------

import qualified  Formatting.Formatters  as  Formatters

import Formatting             ( Format, (%), (%.)
                              , format, formatToString, later, mapf, sformat )
import Formatting.Formatters  ( Buildable, bin, fixed, hex, int, left
                              , oct, right, shortest, shown, stext, text )

-- number ------------------------------

import Number  ( ToNum( toNumI ) )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( eof, many1, option, optionMaybe )
import Text.Parsec.Error       ( ParseError )
import Text.Parsec.Prim        ( ParsecT, Stream, (<?>), parse, try )

-- template-haskell --------------------

import Language.Haskell.TH  ( ExpQ, Name, appE, charL, conE, infixE, integerL
                            , litE, stringL, varE )
import Language.Haskell.TH.Quote
                            ( QuasiQuoter( QuasiQuoter, quoteDec
                                         , quoteExp, quotePat, quoteType ) )

-- text --------------------------------

import qualified  Data.Text               as  Text
import qualified  Data.Text.Lazy          as  LazyText
import qualified  Data.Text.Lazy.Builder  as  LazyBuilder

import Data.Text  ( Text, dropWhileEnd, pack, unpack )

-- time --------------------------------

import Data.Time.Clock   ( UTCTime( UTCTime ) )
import Data.Time.Format  ( defaultTimeLocale, formatTime )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Text.Fmt2.Token  ( Token( Conversion, Str ) )

-------------------------------------------------------------------------------

-- | tokenize a string into strings & conversions
tokens ∷ Text → Either ParseError [Token]
tokens s = concatTokens <$> parse (tokenP <* eof) (unpack s) s

----------------------------------------

-- | squish consecutive Str together

concatTokens ∷ [Token] → [Token]
concatTokens (Str s : Str s' : ts) = concatTokens (Str (s <> s') : ts)
concatTokens (t : t' : ts)         = t : t' : ts
concatTokens ts                    = ts

----------------------------------------

-- | parse a string into tokens
tokenP ∷ Stream s m Char ⇒ ParsecT s u m [Token]
tokenP = many (simpleStr <|> try escapePC <|> try escapeSlash <|> conversion)

----------------------------------------

-- | parse a string intoa conversion specifier

conversion ∷ Stream s m Char ⇒ ParsecT s u m Token
conversion =
  Conversion <$> (string "%" *> optionMaybe fill)
             <*> optionMaybe precision
             <*> (oneOf "bdefIlLnostTwxyYzZ" <?> "valid conversion char")

----------------------------------------

-- | parser for the fill spec of a conversion (the -07 of "%-07s", for example)
fill ∷ Stream s m Char ⇒ ParsecT s u m (Integer, Char)
fill =
  (\ a b c d → (read (concat [a,[c],d]), b)) <$> option "" (string "-")
                                              <*> option ' ' (char '0')
                                              <*> oneOf "123456789"
                                              <*> many digit

----------------------------------------

-- | parse for the precision part of a conversion (.2 of "%3.2f", for example)

precision ∷ Stream s m Char ⇒ ParsecT s u m Natural
precision = read <$> (char '.' *> many digit)

----------------------------------------

-- | parser for an unadorned string (without any % chars)
simpleStr ∷ Stream s m Char ⇒ ParsecT s u m Token
simpleStr = Str <$> many1 (noneOf "%\\")

----------------------------------------

-- | parser for an escaped '%' (represented in the incoming string as "%%")
escapePC ∷ Stream s m Char ⇒ ParsecT s u m Token
escapePC = Str <$> const "%" <$> string "%%"

----------------------------------------

-- | parser for slash escapes, e.g., \\, \n, \t
escapeSlash ∷ Stream s m Char ⇒ ParsecT s u m Token
escapeSlash = Str . decode <$> (char '\\' *> oneOf "nt\\")
              where decode 'n'  = "\n"
                    decode 't'  = "\t"
                    decode '\\' = "\\"
                    decode c    = error $ "bad decode char: '" <> [c] <> "'"

----------------------------------------

{- | whether to format a bytes value in terms of powers of 10^3, or 2^10 -}
data ByteFmtBase = B_1000 | B_1024
  deriving Eq

-- | try really hard to fit within 7 chars
formatBytes ∷ (Buildable a, Integral a) ⇒ ByteFmtBase → a → Text
formatBytes _ (toInteger → 0) = "0"
formatBytes b bs =
    case b of
      B_1000 → go 1000 bs -- (byteSize bs)
      B_1024 → go 1024 bs -- (fromIntegral $ byteSize bs)
    where go ∷ (Buildable b, Integral b) ⇒ Double → b → Text
          go x bytes =
            let ex ∷ Word8 = floor (logBase x $ fromIntegral bytes)
                (pfx,exp) ∷ (Maybe Char, Word8)= case ex of
                              0 → (Nothing,  0)
                              1 → (Just 'k', 1)
                              2 → (Just 'M', 2)
                              3 → (Just 'G', 3)
                              4 → (Just 'T', 4)
                              5 → (Just 'P', 5)
                              6 → (Just 'E', 6)
                              7 → (Just 'Z', 7)
                              _ → (Just 'Y', 8)
                formatB n = fixed n % Formatters.char % Formatters.string % "B"
                i = if b == B_1024 then "i" else ""
             in case pfx of
                 Nothing → sformat (int % "B") bytes
                 Just c  → let mant = fromIntegral bytes / (x^exp)
                               c_   = if b == B_1024 then toUpper c else c
                             in if mant < 10
                                then -- [fmt|%3.2f%T%sB|]
                                     sformat (formatB 2) mant c_ i
                                else if mant < 100
                                     then -- [fmt|%4.1f%T%sB|]
                                          sformat (formatB 1) mant (toUpper c) i
                                     else -- [fmt|%4f%T%sB|]
                                          sformat (formatB 0) mant (toUpper c) i

----------------------------------------

class UTCTimeY α where
  toUTCTimeY ∷ α → Maybe UTCTime

instance UTCTimeY UTCTime where
  toUTCTimeY = Just

instance UTCTimeY (Maybe UTCTime) where
  toUTCTimeY = id

{- | Format a (Maybe UTCTime), in almost-ISO8601-without-fractional-seconds
     (always in Zulu). -}
formatUTCY ∷ UTCTimeY α ⇒ α → Text
formatUTCY (toUTCTimeY → Just t)  = pack $ formatTime defaultTimeLocale "%FZ%T" t
formatUTCY (toUTCTimeY → Nothing) = "-------------------"

{- | Format a (Maybe UTCTime), in ISO8601-without-fractional-seconds (always in
     Zulu), with a leading 3-letter day-of-week. -}
formatUTCYDoW ∷ UTCTimeY α ⇒ α → Text
formatUTCYDoW (toUTCTimeY → Just t) =
  pack $ formatTime defaultTimeLocale "%FZ%T %a" t
formatUTCYDoW (toUTCTimeY → Nothing)  = "-----------------------"

toFormatUTC ∷ UTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTC = later $ LazyBuilder.fromText . formatUTCY

toFormatUTCDoW ∷ UTCTimeY α ⇒ Format ρ (α → ρ)
toFormatUTCDoW = later $ LazyBuilder.fromText . formatUTCYDoW

----------------------------------------

-- | parse a fmt, return an ExpQ that when spliced, takes arguments to pass
--   to the formatter to provide a textlike thing (see `FormatTarget`)
sprintf ∷ Text → ExpQ
sprintf = sprintf_ 'output

-- | like `sprintf`, but always produces a String (to reduce scoped type
--   variables)
sprintfS ∷ Text → ExpQ
sprintfS = sprintf_ 'formatToString

sprintfT ∷ Text → ExpQ
sprintfT = sprintf_ 'sformat

sprintfL ∷ Text → ExpQ
sprintfL = sprintf_ 'format

sprintf_ ∷ Name → Text → ExpQ
sprintf_ fnam t =
  case tokens t of
    Left  e    → error $ show e
    Right toks → appE (varE fnam) $
                      foldr conjoin (litE $ stringL "") (fmap tokOp toks)
                  where conjoin = infixOp '(%)

-- | conversion token as formatter; e.g., %-3t ⇒ (left 3 ' ') %. stext
tokOp ∷ Token → ExpQ
tokOp (Str s)                       = litE $ stringL s
tokOp (Conversion Nothing p c)      = charOp c Nothing p
tokOp (Conversion (Just (i,c)) p o) =
  infixOp '(%.) (fillOp (i,c)) (charOp o (Just i) p)
-- tokOp (Conversion x y)         =
--   error $ "failed conversion: " <> show x <> ", " <> show y

----------------------------------------

-- create a fill expression
fillIt ∷ Name → Integer → Char → ExpQ
fillIt direction width chr =
  appE (appE (varE direction) (litE (integerL width))) (litE $ charL chr)

-- | conversion fill; -x → left, (+)x → right

fillOp ∷ (Integer,Char) → ExpQ
fillOp (i,c) | i < 0 = fillIt 'right (abs i) c
fillOp (i,c) | i > 0 = fillIt 'left       i  c
fillOp (_,_) = -- i == 0 : something's gone wrong!
               error "cannot fill with size 0"

----------------------------------------

toTextF ∷ Printable t ⇒ Format r (t → r)
toTextF = later $ LazyBuilder.fromText . toText

toTextListF ∷ (Foldable f, Printable t) ⇒ Format r (f t → r)
toTextListF =
  later $ LazyBuilder.fromText . Text.intercalate "," . fmap toText . toList

----------------------------------------

toFormatBytes ∷ (Buildable a, Integral a) ⇒ ByteFmtBase → Format r (a → r)
toFormatBytes b = later $ LazyBuilder.fromText . formatBytes b

----------------------------------------

{- $formatting

   Each specfier may be preceded by an integer value to specify padding and
   justification.  A positive integer pads to the left (thus, justifies to the
   right; and a negative integer pads to the right (thus, justifies to the
   left).

   >>> [fmtT|[%3s]|] "a"
   "[  a]"

   >>> [fmtT|[%-3s]|] "a"
   "[a  ]"

   Where noted below, some specifiers also allow a precision (after a 'decimal
   point').

   [@L@] - A `Foldable` of things, where the things are instances of
           `Printable`, joined with ',', thus
           @ (`Foldable` φ, Printable τ) ⇒ φ intercalate "," (fmap toText τ) @

   [@l@] - LazyText `LazyText.Text`

   [@s@] - `String`

   [@t@] - StrictText `Text`

   [@T@] - `Printable` @ τ ⇒ toText τ @

   [@w@] - `Show` @ ω ⇒ show ω @

   [@d@] - `Integral` α ⇒ render as denary

   [@n@] - `ToNum` α    ⇒ render as denary

   [@x@] - `Integral` α ⇒ render as hexadenary

   [@b@] - `Integral` α ⇒ render as binary

   [@o@] - `Integral` α ⇒ render as octal

   [@f@] - `Real` α ⇒ Render as decimal with as many decimal places as
                       necessary.  Beware floating-point representation which
                       may give lengthy results.

   [@f.n@] - `Real` α ⇒ Render as decimal with precisely /n/ decimal places.
                        Will round to the nearest decimal place as appropriate.

   [@e@] - `Real` α ⇒ Render as decimal in scientific notation with 0 decimal
                       places in the mantissa.

                       >>> [fmtT|[%-e]|] (3.14 ∷ Float)
                       "[3e0]"

                       Note that the padding width, if provided, applies to the
                       whole representation; thus the below adds one space
                       because "3e-1" is 4 characters.

                       >>> [fmtT|[%5e]|] (0.314 ∷ Float)
                       "[ 3e-1]"


   [@e.n@] - `Real` α ⇒ Render as decimal in scientific notation with precisely
                         /n/ decimal places in the mantissa.

                         >>> [fmtT|[%-.1e]|] (0.314 ∷ Float)
                         "[3.1e-1]"

   [@y@] - `Integral` α ⇒ Render as bytes, with a 2^10 multiplier.  This tries
                           to fit the value into 7 characters.

                           >>> [fmtT|%y|] (1024^(2∷Int) ∷ Integer)
                           "1.05MB"

                           >>> [fmtT|%y|] (1024 ∷ Integer)
                           "1.02kB"

                           >>> [fmtT|%y|] (999 ∷ Integer)
                           "999B"


   [@Y@] - `Integral` α ⇒ Render as bytes, with a 2^10 multiplier.  This tries
                           to fit the value into 7 characters.

                           >>> [fmtT|%Y|] (999 ∷ Integer)
                           "999B"

                           >>> [fmtT|%Y|] (1024*1023 ∷ Integer)
                           "1023KiB"

                           >>> [fmtT|%Y|] (1024*1024 ∷ Integer)
                           "1.00MiB"

   [@z@] - `UTCTime` α or `Maybe UTCTime` α ⇒ Render as UTCTime, in the form
                                              "YYYY-MM-DDZhh:mm:ss".
                           >>> getCurrentTime >>= return . [fmtT|%z|]
                           "2020-04-20Z05:58:47"

   [@Z@] - `UTCTime` α or `Maybe UTCTime` α ⇒ Render as UTCTime, in the form
                                              "YYYY-MM-DDZhh:mm:ss www" where
                                              www is three-letter day-of-week.
                           >>> getCurrentTime >>= return . [fmtT|%Z|]
                           "2020-04-20Z05:58:47 Mon"
-}

charOpNoPrecision ∷ ExpQ → Char → Maybe Natural → ExpQ
charOpNoPrecision f _ Nothing  = f
charOpNoPrecision _ c (Just p) = error $ "conversion char '" <> [c]
                                      <> "' does not admit precision ("
                                      <> show p <> ")"

-- | conversion character as formatter; e.g., 't' → stext; takes precision
--   too, lest that affect the conversion
charOp ∷ Char → Maybe Integer → Maybe Natural → ExpQ

charOp c@'L' _ p = charOpNoPrecision (varE 'toTextListF) c p
charOp c@'l' _ p = charOpNoPrecision (varE 'text) c p
charOp c@'s' _ p = charOpNoPrecision (varE 'Formatters.string) c p
charOp c@'t' _ p = charOpNoPrecision (varE 'stext) c p
charOp c@'T' _ p = charOpNoPrecision (varE 'toTextF) c p
charOp c@'w' _ p = charOpNoPrecision (varE 'shown) c p

charOp c@'d' _ p = charOpNoPrecision (varE 'int) c p
charOp c@'x' _ p = charOpNoPrecision (varE 'hex) c p
charOp c@'b' _ p = charOpNoPrecision (varE 'bin) c p
charOp c@'o' _ p = charOpNoPrecision (varE 'oct) c p
charOp c@'n' _ p = charOpNoPrecision (varE 'tonum) c p

charOp 'f' _ Nothing  = varE 'floatmin
charOp 'f' _ (Just i) = appE (varE 'fixed) (litE (integerL $ fromIntegral i))
charOp 'e' _ Nothing  = appE (varE 'expt) (litE (integerL 0))
charOp 'e' _ (Just i) = appE (varE 'expt) (litE (integerL $ fromIntegral i))

charOp 'y' _ Nothing = appE (varE 'toFormatBytes) (conE 'B_1000)
charOp 'y' _ (Just _) = error "y format does not handle precision"
charOp 'Y' _ Nothing = appE (varE 'toFormatBytes) (conE 'B_1024)
charOp 'Y' _ (Just _) = error "Y format does not handle precision"

charOp 'z' _ Nothing = varE 'toFormatUTC
charOp 'Z' _ Nothing = varE 'toFormatUTCDoW

charOp x _ _ = error $ "bad conversion char'" <> [x] <> "'"

floatmin ∷ Real α ⇒ Format r (α → r)
floatmin = let dropper = dropWhileEnd (`elem` (".0" ∷ String))
            in later $ LazyBuilder.fromText . dropper . sformat shortest

tonum ∷ ToNum α ⇒ Format r (α → r)
tonum = mapf toNumI int

expt ∷  RealFloat α ⇒ Int → Format r (α → r)
expt i = later (\ f →
  let (m,e ∷ Integer) = decompose f
   in LazyBuilder.fromText $ (sformat $ (fixed i % "e" % int)) m e)


-- | decompose a Real value into "engineering" notation; a mantissa between
--   (-10,10) and an exponent, as a power of 10
decompose ∷ (RealFloat α, Integral β) ⇒ α → (Double, β)
decompose val = let (mant2,ex2) = decodeFloat val
                    mant2d ∷ Double = fromIntegral(abs mant2)
                    ex2d   ∷ Double = fromIntegral ex2
                    res    ∷ Double = log10 mant2d + log10 (2**ex2d)
                    ex10             = floor res
                    log10  ∷ Double → Double = logBase 10
                    mant10 ∷ Double = 10**(res - (fromIntegral ex10∷Double))
                 in if mant2 > 0
                    then (mant10,ex10)
                    else (-mant10,ex10) 

----------------------------------------

-- | infix a function between two values
infixOp ∷ Name → ExpQ → ExpQ → ExpQ
infixOp op l r = infixE (Just l) (varE op) (Just r)

----------------------------------------

-- | Generate an instance of FormatTarget (e.g., Strict or Lazy Text, or String)
--   from a format and set of values.
--
--   >>> ([fmt|foo %s|] ("baz" ∷ String)) ∷ Text
--   "foo baz"
--
--   >>> :t [fmtS|bar %t|] ("quux" ∷ Text)
--   [fmtS|bar %t|] ("quux" ∷ Text) ∷ [Char]
--
--   >>> [fmtS|bar %t|] ("quux" ∷ Text)
--  "bar quux"

fmt ∷ QuasiQuoter
fmt =  QuasiQuoter { quoteDec  = error "not implemented"
                   , quoteType = error "not implemented"
                   , quotePat  = error "not implemented"
                   , quoteExp  = sprintf . pack
                   }

--------------------

-- | like `fmt`, but produces specifically a String
fmtS ∷ QuasiQuoter
fmtS =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfS . pack
                    }

--------------------

-- | like `fmt`, but produces specifically a Lazy Text
fmtL ∷ QuasiQuoter
fmtL =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfL . pack
                    }

--------------------

-- | like `fmt`, but produces specifically a Strict Text
fmtT ∷ QuasiQuoter
fmtT =  QuasiQuoter { quoteDec  = error "not implemented"
                    , quoteType = error "not implemented"
                    , quotePat  = error "not implemented"
                    , quoteExp  = sprintfT . pack
                    }

------------------------------------------------------------

-- | possible target of `fmt` or similar.

class FormatTarget t where
  output ∷ Format t a → a

instance FormatTarget Text where
  output = sformat

instance FormatTarget LazyText.Text where
  output = format

instance FormatTarget String where
  output = formatToString

-- that's all, folks! ---------------------------------------------------------
