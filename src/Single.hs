{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Single
  ( MonoSingle( osingle ), Single( single ), filt, filt', ofilt, ofilt' )
where

-- base --------------------------------

import Control.Applicative  ( pure )
import Data.Either          ( Either )
import Data.Foldable        ( Foldable( foldr ) )
import Data.List.NonEmpty   ( NonEmpty( (:|) ) )
import Data.Maybe           ( Maybe( Just ) )
import Data.Monoid          ( Monoid )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- containers --------------------------

import Data.Set  ( Set, singleton )

-- dlist -------------------------------

import Data.DList  ( DList )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable, ofoldr )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool    ( 𝔹 )
import Data.MoreUnicode.Monoid  ( ф )

--------------------------------------------------------------------------------

-- Alternative because it's a Monoidal Applicative; we need the monoidal for 
-- filt ∷ ∀ ψ φ α . (Foldable φ, Semigroup (ψ α), Alternative ψ) ⇒
--       (α → Bool) → ψ α → φ α → ψ α
class Single π where
  single ∷ α → π α

instance Single [] where
  single x = [x]

instance Single NonEmpty where
  single x = x :| []

instance Single Maybe where
  single = Just

instance Single (Either ω) where
  single = pure

instance Single Set where
  single = singleton

instance Single DList where
  single = pure

------------------------------------------------------------

{- | `MonoSingle` is to `Single` as `MonoFoldable` is to `Foldable`. -}

class MonoSingle π where
  osingle ∷ Element π → π

------------------------------------------------------------

{- | Generalization of `Data.List.filter` to any type is monoidal, with a
     `Single` instance. -}
filt ∷ ∀ ψ π α . (Foldable ψ, Monoid (π α), Single π) ⇒ (α → 𝔹) → ψ α → π α
filt p = foldr (\ x xs → if p x then single x ⊕ xs else xs) ф

{- | Specialization of `filt` to reap what we sow; return the same container
     as was given. -}
filt' ∷ ∀ ψ α . (Foldable ψ, Monoid (ψ α), Single ψ) ⇒ (α → 𝔹) → ψ α → ψ α
filt' = filt

{- | Application of `filt` to `MonoFoldable`. -}
ofilt ∷ ∀ ψ π α . (MonoFoldable ψ, Monoid (π α), Single π, α ~ Element ψ) ⇒
                  (α → 𝔹) → ψ → π α
ofilt p = ofoldr (\ x xs → if p x then single x ⊕ xs else xs) ф

{- | Specialization of `ofilt` to reap what we sow; return the same container
     as was given. -}
ofilt' ∷ ∀ ψ α . (MonoFoldable ψ, Monoid ψ, MonoSingle ψ, α ~ Element ψ) ⇒
                 (α → 𝔹) → ψ → ψ
ofilt' p = ofoldr (\ x xs → if p x then osingle x ⊕ xs else xs) ф


-- that's all, folks! ----------------------------------------------------------
