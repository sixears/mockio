{-# LANGUAGE Rank2Types           #-}
{-# LANGUAGE UnicodeSyntax        #-}

module ParsecPlusBase2
  ( Parser, boundedDoubledChars )
where

-- base --------------------------------

import Control.Applicative  ( many )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⋫), (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, noneOf )
import Text.Parsec.Combinator  ( choice )
import Text.Parsec.Prim        ( ParsecT, Stream, try )

-------------------------------------------------------------------------------

type Parser α = ∀ s u m . Stream s m Char ⇒ ParsecT s u m α

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

{- | Parse any character except those in `cs`; they must be doubled.  Thus

     @ parse (many (try $ doubledChar "{}")) "test" "o}}{{p}" ≡ Right "o}{p" @

     Note the use of `try`; doubleChar will consume the first char of
     non-conformant input.
 -}
doubledChar ∷ [Char] → Parser Char
doubledChar cs = (choice $ (\ c → char c ⋫ char c) ⊳ cs) ∤ noneOf cs

{- | Parse many characters, most directly, but those in `cs` must be doubled up.

     @ parse (doubledChars "{}") "test" "o}}{{p}x" ≡ Right "o}{p" @
 -}
doubledChars ∷ [Char] → Parser [Char]
doubledChars cs = many (try $ doubledChar cs)

{- | Parse many characters, most directly, bounded by `l` on the left and `r`
     on the right; instances of `l` & `r` within the text must be doubled up.

     @ parse (boundedDoubledChars '{' '}') "test" "{o}}{{p}x" ≡ Right "o}{p" @

     @ parse (boundedDoubledChars '!' '!') "test" "!o}}!!p!" ≡ Right "o}}!p" @
 -}
boundedDoubledChars ∷ Char -> Char → Parser [Char]
boundedDoubledChars l r = char l ⋫ doubledChars [l,r] ⋪ char r

-- that's all, folks! ---------------------------------------------------------
