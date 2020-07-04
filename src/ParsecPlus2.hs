{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE UnicodeSyntax    #-}

{- | Add simple file-handling on top of Base Parsecable class -}
module ParsecPlus2
  ( AsParseError(..), IOParseError, Parsecable(..), ParseError
  , digits, parens, parsecFUTF8, parsecFUTF8L, parsecFileUTF8L, parsecFileUTF8
  , __parsecN__
  , module ParsecPlusBase2
  )
where

import ParsecPlusBase2

-- base --------------------------------

import Data.Maybe   ( Maybe( Just, Nothing ) )
import Data.String  ( String )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.File        ( File )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO       ( MonadIO )
import MonadIO.File  ( getContentsUTF8, getContentsUTF8Lenient
                     , readFileUTF8, readFileUTF8Lenient )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens          ( (⫥) )
import Data.MoreUnicode.Monad         ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- parsec-plus-base --------------------

import Parsec.Error  ( AsParseError( _ParseError ), IOParseError, ParseError )

--------------------------------------------------------------------------------

{- | Parse a file whose contents are UTF8-encoded text. -}
parsecFileUTF8 ∷ ∀ χ ε μ . (MonadIO μ, Parsecable χ,
                            AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
                 File → μ χ
parsecFileUTF8 fn = readFileUTF8 fn ≫ parsec (fn ⫥ filepath)

----------------------------------------

{- | Parse a file whose contents are UTF8-encoded text; with lenient decoding
     (see `readFileUTF8Lenient`. -}
parsecFileUTF8L ∷ ∀ χ ε μ . (MonadIO μ, Parsecable χ,
                            AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
                 File → μ χ
parsecFileUTF8L fn = readFileUTF8Lenient fn ≫ parsec (fn ⫥ filepath)

----------------------------------------

{- | Parse a file whose contents are UTF8-encoded text; `Nothing` causes a parse
     of stdin. -}
parsecFUTF8L ∷ ∀ χ ε μ . (MonadIO μ, Parsecable χ,
                         AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
              Maybe File → μ χ
parsecFUTF8L Nothing   = getContentsUTF8Lenient ≫ parsec ("stdin" ∷ String)
parsecFUTF8L (Just fn) = parsecFileUTF8L fn

----------------------------------------

{- | Parse a file whose contents are UTF8-encoded text; `Nothing` causes a parse
     of stdin. -}
parsecFUTF8 ∷ ∀ χ ε μ . (MonadIO μ, Parsecable χ,
                         AsIOError ε, AsParseError ε, MonadError ε μ) ⇒
              Maybe File → μ χ
parsecFUTF8 Nothing   = getContentsUTF8 ≫ parsec ("stdin" ∷ String)
parsecFUTF8 (Just fn) = parsecFileUTF8 fn

-- that's all, folks! ----------------------------------------------------------
