{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.Equish
  ( Equish( (≃) ) )
where

-- base --------------------------------

import Data.Bool  ( Bool )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode  ( (≡) )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

--------------------------------------------------------------------------------

{- | Approximately equal; FOR TESTING ONLY -}
infix 4 ≃
class Equish α where
  (≃) ∷ α → α → Bool

instance Equish ℕ where
  (≃) = (≡)

-- that's all, folks! ----------------------------------------------------------
