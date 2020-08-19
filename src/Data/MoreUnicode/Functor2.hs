{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Functor2
  ( (⊳), (⊳⊳), (⊳⊳⊳), (⊲), (⩺) )
where

import Prelude ()

-- base --------------------------------

import Data.Functor   ( Functor, fmap )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-------------------------------------------------------------------------------

infixl 4 ⊳
(⊳) ∷ Functor ψ ⇒ (α → β) → ψ α → ψ β
(⊳) = fmap

infixl 4 ⊳⊳
(⊳⊳) ∷ (Functor ψ, Functor φ) ⇒ (α → β) → φ (ψ α) → φ (ψ β)
(⊳⊳) = fmap ∘ fmap

infixl 4 ⊳⊳⊳
(⊳⊳⊳) ∷ (Functor ψ, Functor φ, Functor ζ) ⇒ (α → β) → ζ (φ (ψ α)) → ζ (φ (ψ β))
(⊳⊳⊳) = fmap ∘ fmap ∘ fmap

-- | Like `(⊳)`, but with the arguments flipped.  This might be useful, for
--   example as an infix version of `liftM`.  Designed to have similar flow to
--   `(Control.Monad.>>=)`.
--   Thus,
--     readFile "/etc/passwd" >>$ length :: IO Int
infixl 4 ⊲
(⊲) ∷ Functor ψ ⇒ ψ α → (α → β) → ψ β
as ⊲ f = fmap f as

----------------------------------------

{- | Functor combinator to lift f and fmap it across the result of g.
     This may be particularly useful for lifting into a monad, with
     mono-unsaturated functions (i.e., for point-free style; e.g.,
     
       \ fn -> length <$> readFile fn
  
    may be re-written as 
  
       length <$$> readFile
 -}

infixl 4 <$$>
(<$$>) ∷ Functor φ ⇒ (β → γ) → (α → φ β) → α → φ γ
f <$$> g = fmap f ∘ g

infixl 4 ⩺
(⩺) ∷ Functor φ ⇒ (β → γ) → (α → φ β) → α → φ γ
(⩺) = (<$$>)

-- that's all, folks! ---------------------------------------------------------
