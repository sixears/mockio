{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Natural
  ( AtMost( Cons, Nil ), Countable( count ), Nat( S, Z ), Natty( Sy, Zy), None
  , One, Two
  , ℕ
  , atMost, atMostOne, atMostTwo, fromEnum, length, none, one, toEnum, two
  , zeroOneOrTwo
  )
where

import qualified  GHC.Enum
import qualified  GHC.Num
import qualified  GHC.Real
import GHC.Num  ( (+), (-) )

-- base --------------------------------

import qualified  Data.Foldable

import Control.Applicative  ( Alternative, pure )
import Data.Bool            ( otherwise )
import Data.Eq              ( Eq( (==) ) )
import Data.Ord             ( Ord( (<=), (>) ) )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )

--------------------------------------------------------------------------------

length ∷ Data.Foldable.Foldable ψ ⇒ ψ α → ℕ
length = GHC.Real.fromIntegral ∘ Data.Foldable.length

fromEnum ∷ GHC.Enum.Enum α ⇒ α → ℕ
fromEnum = GHC.Real.fromIntegral ∘ GHC.Enum.fromEnum

toEnum ∷ GHC.Enum.Enum α ⇒ ℕ → α
toEnum = GHC.Enum.toEnum ∘ GHC.Num.fromInteger ∘ GHC.Real.toInteger

------------------------------------------------------------

{- Thank you to dfeuer, redneb, leftroundabout on StackOverflow
   http://stackoverflow.com/questions/39690844/haskell-how-do-i-create-a-function-that-allows-none-one-or-two-applicatives
-}

zeroOneOrTwo :: Alternative f => f a -> f [a]
zeroOneOrTwo a = go (2 :: ℕ)
  where
    go n
      | n > 0 = ((:) ⊳ a ⊵ go (n - 1)) ∤ pure []
      | otherwise = pure []

data Nat = Z | S Nat
  deriving (Eq, Ord, Show)

data Natty n where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)

data AtMost n a where
  Nil :: AtMost n a
  Cons :: a -> AtMost n a -> AtMost ('S n) a

class Countable c where
  count :: c -> ℕ

instance Countable (AtMost n a) where
  count Nil        = 0
  count (Cons _ x) = 1 + count x

instance Eq (AtMost n a) where
  a == b = (count a) == (count b)

instance Ord (AtMost n a) where
  a <= b = (count a) <= (count b)

atMost :: Alternative f => Natty n -> f a -> f (AtMost n a)
atMost Zy _ = pure Nil
atMost (Sy n) a = (Cons ⊳ a ⊵ atMost n a) ∤ pure Nil

atMostOne :: Alternative f => f a -> f (AtMost One a)
atMostOne = atMost (Sy Zy)
atMostTwo :: Alternative f => f a -> f (AtMost Two a)
atMostTwo = atMost (Sy (Sy Zy))

type None = 'Z
type One  = 'S None
type Two  = 'S One -- ('S 'Z)

none ∷ Natty 'Z
none = Zy

one ∷ Natty ('S 'Z)
one  = Sy none

two ∷ Natty ('S ('S 'Z))
two  = Sy one

-- that's all, folks! ----------------------------------------------------------
