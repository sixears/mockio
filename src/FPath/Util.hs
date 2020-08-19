{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

-- !! Included here just to serve reldir2.hs !!

module FPath.Util
  ( __ERROR__, __ERROR'__, mkVisS, mkVisT )
where

import Prelude  ( error, mod )

-- base --------------------------------

import Data.Char      ( Char, ord )
import Data.Foldable  ( length )
import Data.Functor   ( fmap )
import Data.List      ( (!!), elem )
import Data.String    ( String )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

--------------------------------------------------------------------------------

__ERROR__ ∷ Text → α
__ERROR__ = error ∘ toString

__ERROR'__ ∷ Printable ρ ⇒ ρ → α
__ERROR'__ = error ∘ toString

----------------------------------------

{-| a list of 'visible' (easily distinguishable, and typed, characters,
    available in any font) -}
visChars ∷ [Char]
visChars = "abcdefghijklmnopqrstuvwxyz01234567890-"

{-| replace a character with one from a 'visible' list, if necessary -}
mkVisC ∷ Char → Char
mkVisC c = if c `elem` visChars
           then c
           else visChars !! (ord c `mod` length visChars)

mkVisS ∷ String → String
mkVisS = fmap mkVisC

mkVisT ∷ Text → Text
mkVisT = pack ∘ mkVisS ∘ unpack

-- that's all, folks! ----------------------------------------------------------
