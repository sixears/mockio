{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Natural
  ( ℕ, length, fromEnum, toEnum )
where

import qualified  GHC.Enum
import qualified  GHC.Num
import qualified  GHC.Real

-- base --------------------------------

import qualified  Data.Foldable

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

--------------------------------------------------------------------------------

length ∷ Data.Foldable.Foldable ψ ⇒ ψ α → ℕ
length = GHC.Real.fromIntegral ∘ Data.Foldable.length

fromEnum ∷ GHC.Enum.Enum α ⇒ α → ℕ
fromEnum = GHC.Real.fromIntegral ∘ GHC.Enum.fromEnum

toEnum ∷ GHC.Enum.Enum α ⇒ ℕ → α
toEnum = GHC.Enum.toEnum ∘ GHC.Num.fromInteger ∘ GHC.Real.toInteger

-- that's all, folks! ----------------------------------------------------------
