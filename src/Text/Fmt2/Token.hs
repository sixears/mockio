{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE FlexibleContexts #-}

{- | a token in a format specifier -}

module Text.Fmt2.Token
  ( Token( .. ) )
where

-- base --------------------------------

import Numeric.Natural  ( Natural )

--------------------------------------------------------------------------------

-- | a token in a format specifier

data Token = -- | a conversion specifier, e.g., %3.2f; the first value is the
             --   fill, if any - the width, and the char (default ' ', may be
             --   '0')
             Conversion (Maybe (Integer, Char)) (Maybe Natural) Char
           | Str String
  deriving (Eq, Show)

-- that's all, folks! ----------------------------------------------------------
