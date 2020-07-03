{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

{- | Utilities for working with `Control.Monad.Except.MonadError` -}
module MonadError2
  ( MonadError
  , ѥ, ж, ѭ, ӂ
  , eToMaybe, fromMaybe, fromRight, mapMError, mapMError', eFromMaybe
  , __monadError__, mErrFail, splitMError, throwError
  )
where

import Prelude ( error )

-- base --------------------------------

import Control.Monad       ( Monad, join, return )
import Control.Monad.Fail  ( MonadFail, fail )
import Data.Bifunctor      ( first )
import Data.Either         ( Either( Left, Right ), either )
import Data.Function       ( id )
import Data.Functor        ( fmap )
import Data.Maybe          ( Maybe( Just, Nothing ), maybe )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, ExceptT, runExceptT, throwError )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )

-------------------------------------------------------------------------------

{- | Map the exception part of a MonadError; can also be used on MonadThrow
     to convert to a MonadError (or indeed any Either). -}
mapMError ∷ (MonadError β μ) ⇒ (α → β) → Either α x → μ x
mapMError f = fromRight ∘ first f

----------------------------------------

{- | Map a monad error that is embedded within or joined by another Monad -}

mapMError' ∷ MonadError ε μ ⇒ (ι → ε) → ExceptT ι μ α → μ α
mapMError' f = join ∘ (mapMError f ⩺ splitMError)

----------------------------------------

{- | `fromJust`, throwing an error on Nothing -}
eFromMaybe ∷ MonadError ε μ ⇒ ε → Maybe α → μ α
eFromMaybe e = maybe (throwError e) return

{-# DEPRECATED fromMaybe "use `eFromMaybe` to avoid clash with `Data.Maybe.fromMaybe`" #-}
fromMaybe ∷ MonadError ε μ ⇒ ε → Maybe α → μ α
fromMaybe = eFromMaybe

----------------------------------------

-- | fromRight, throwing an error on Left
fromRight ∷ MonadError ε μ ⇒ Either ε α → μ α
fromRight = either throwError return

----------------------------------------

{- | Split a `MonadError` out from a monad; that is, takes μ ... (which is a
     monad with an ExceptT constraint) and turns it into a layered μ (η ...) -}
splitMError ∷ (MonadError ε η, Monad μ) ⇒ ExceptT ε μ a → μ (η a)
splitMError f = either throwError return ⊳ runExceptT f

{- | Unicode alias for `splitMError` -}
ѥ ∷ (MonadError ε η, Monad μ) ⇒ ExceptT ε μ α → μ (η α)
ѥ = splitMError

----------------------------------------

{- | Turn an exception into an `error`. -}
__monadError__ ∷ (Monad η, Printable ε) ⇒ ExceptT ε η α → η α
__monadError__ = fmap (either (error ∘ toString) id) ⊳ splitMError

{- | Unicode alias for `__monadError__` -}
ж ∷ (Monad η, Printable ε) ⇒ ExceptT ε η α → η α
ж = __monadError__

----------------------------------------

{- | Convert an either to a maybe on the RHS. -}
eToMaybe ∷ Either χ α → Maybe α
eToMaybe (Left  _) = Nothing
eToMaybe (Right a) = Just a

-- | Pronounced 'maybe-funnel', or maybe 'yus', this is an alias for `eToMaybe`.
ѭ ∷ Either χ α → Maybe α
ѭ = eToMaybe

----------------------------------------

{- | Convert a MonadError (or, indeed, any `Either`) to a MonadFail. -}
mErrFail ∷ (MonadFail η, Printable τ) ⇒ Either τ β → η β
mErrFail = either (fail ∘ toString) return

ӂ ∷ (MonadFail η, Printable τ) ⇒ Either τ β → η β
ӂ = mErrFail

-- that's all, folks! ---------------------------------------------------------
