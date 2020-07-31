{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UnicodeSyntax        #-}

module ParsecPlusBase2
  ( AsParseError(..), IOParseError, Parsecable(..), Parser
  , boundedDoubledChars, caseInsensitiveChar, caseInsensitiveString, digits
  , parens, __parsecN__, uniquePrefix
  )
where

-- base --------------------------------

import Control.Applicative    ( many )
import Data.Bifunctor         ( first )
import Data.Char              ( toLower, toUpper )
import Data.Functor.Identity  ( Identity )
import Data.List              ( isPrefixOf )
import Data.Word              ( Word8 )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- lens --------------------------------

import Control.Lens.Review   ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵), (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, digit, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( between, choice, count, eof, many1 )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, parse, try )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Parsec.Error  ( AsParseError( _ParseError ), IOParseError
                     , ParseError( ParseError ) )

-------------------------------------------------------------------------------

type Parser α = ∀ s u m . Stream s m Char ⇒ ParsecT s u m α

__right__ ∷ Printable ε ⇒ Either ε β → β
__right__ x = either (error ∘ toString) id x

class Parsecable χ where
  {- | Parser for a type.  Implement this to benefit from `parsec` and variants.
   -}
  parser ∷ Stream s Identity Char ⇒ Parsec s u χ

  ------------------

  {- | A bit like `Text.Read.read` for parsecable values -}
  parsec ∷ ∀ ε μ s σ .
           (AsParseError ε, MonadError ε μ, Stream s Identity Char, Printable σ)
         ⇒ σ → s → μ χ
  parsec sourceName t = case parse parser (toString sourceName) t of
                           Left  e → throwError (_ParseError # ParseError e)
                           Right s → return s
  ------------------

  {- | *PARTIAL*: `parsec`, will error on failure to parse -}
  __parsec__ ∷ ∀ s σ . (Printable σ, Stream s Identity Char) ⇒ σ → s → χ
  __parsec__ sourceName = __right__ ∘ parsec @_ @ParseError sourceName

----------------------------------------

instance Parsecable Word8 where
  {-| parse a word8 value in denary; that is, 0-255 -}
  parser =
    read ⊳ go
    where go = try (mappend ⊳ string "25" ⊵ (pure ⊳ oneOf "012345"))
             ∤ try ((:) ⊳ char '2' ⊵ ((:) ⊳ oneOf "01234" ⊵ count 1 digit))
             ∤ try ((:) ⊳ oneOf "01" ⊵ count 2 digit)
             ∤ try (count 2 digit)
             ∤ count 1 digit

------------------------------------------------------------

{- | *PARTIAL*: Parse a stream, throwing an error in case of failure.  No
     name for the stream is given -}

__parsecN__ ∷ (Stream s Identity Char, Parsecable χ) ⇒ s → χ
__parsecN__ t = __right__ ∘ first ParseError $ parse (parser ⋪ eof) "" t

----------------------------------------

{- | Parse between parentheses -}
parens ∷ Stream s m Char ⇒ ParsecT s u m a → ParsecT s u m a
parens = between (char '(') (char ')')

----------------------------------------

{- | Parse 1 or more digits -}
digits ∷ Stream s m Char ⇒ ParsecT s u m String
digits = many1 digit

{-
eChar ∷ Char
eChar = '\\'

escape :: Parser String
escape = pure ⊳ oneOf "\\\"0nrvtbf{}"

nonEscape :: Parser Char
nonEscape = noneOf "\\\"\0\n\r\v\t\b\f{}"

character :: Parser String
character = fmap return nonEscape <|> escape

parseEscaped ∷ String → String → Parser String
parseEscaped l r = do
    strings <- string l *> many character <* string r
    return $ concat strings
-}

----------------------------------------

{- | Parse any character except those in `cs`; they must be doubled.  Thus

     @ parse (many (try $ doubledChar "{}")) "test" "o}}{{p}" ≡ Right "o}{p" @

     Note the use of `try`; doubleChar will consume the first char of
     non-conformant input.
 -}
doubledChar ∷ [Char] → Parser Char
doubledChar cs = (choice $ (\ c → char c ⋫ char c) ⊳ cs) ∤ noneOf cs

----------------------------------------

{- | Parse many characters, most directly, but those in `cs` must be doubled up.

     @ parse (doubledChars "{}") "test" "o}}{{p}x" ≡ Right "o}{p" @
 -}
doubledChars ∷ [Char] → Parser [Char]
doubledChars cs = many (try $ doubledChar cs)

----------------------------------------

{- | Parse many characters, most directly, bounded by `l` on the left and `r`
     on the right; instances of `l` & `r` within the text must be doubled up.

     @ parse (boundedDoubledChars '{' '}') "test" "{o}}{{p}x" ≡ Right "o}{p" @

     @ parse (boundedDoubledChars '!' '!') "test" "!o}}!!p!" ≡ Right "o}}!p" @
 -}
boundedDoubledChars ∷ Char -> Char → Parser [Char]
boundedDoubledChars l r = char l ⋫ doubledChars [l,r] ⋪ char r

----------------------------------------

{- | Parse a uniquely matching prefix.

     Given a value table, and a parser; can we parse to something that uniquely
     provides a result?  The parser succeeds if the parse output prefixes
     precisely one result.
 -}
uniquePrefix ∷ ∀ α β χ σ τ η . (Eq α, Printable χ) ⇒
               [([α],β)] → ([α] → χ) → ParsecT σ τ η [α] → ParsecT σ τ η β
uniquePrefix ss e prs = do
  s ← prs
  case filter ((s `isPrefixOf`) ∘ fst) ss of
    [(_,y)] → return y
    _       → fail $ toString (e s)

----------------------------------------

{- | Parse the given character, or the same character in another case
     (upper or lower). -}
caseInsensitiveChar ∷ Stream σ η Char ⇒ Char → ParsecT σ υ η Char
caseInsensitiveChar c = do
  _ ← char (toLower c) ∤ char (toUpper c)
  return c

-- | Parse the given string, but with any combination of upper and lower case
-- characters.
caseInsensitiveString ∷ Stream σ η Char ⇒ String → ParsecT σ υ η String
caseInsensitiveString = sequence ∘ fmap caseInsensitiveChar

-- that's all, folks! ---------------------------------------------------------
