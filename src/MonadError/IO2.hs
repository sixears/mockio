{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO2
  ( ӝ, asIOError, asIOErrorY, eitherIOThrow, eitherIOThrowT, hoistMonadIOError
  , ioMonadErr, ioMonadError, ioThrow, __monadIOError__, wrapAsIOErr, wrapIOErr )
where

-- base --------------------------------

import qualified  System.IO.Error

import Control.Exception.Base  ( Exception, IOException, throwIO )
import Control.Monad           ( (>>=), join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bifunctor          ( first )
import Data.Either             ( Either, either )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe )
import System.IO               ( IO )
import System.IO.Error         ( catchIOError, userError )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT, throwError )
import Control.Lens.Review   ( (#) )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadError.IO.Error  ( AsIOError, IOError, _IOErr, squashNoSuchThing ) 
import MonadError           ( mapMError, splitMError )  

-------------------------------------------------------------------------------

{- | Perform some `IO`, catching any `IOError`s into a `MonadError`. -}
ioMonadError ∷ (MonadIO μ, MonadError IOException η) ⇒ IO α → μ (η α)
ioMonadError io = liftIO $ catchIOError (return ⊳ io) (return ∘ throwError)

----------------------------------------

{- | Hoist an IO (Either IOException α) to `MonadError`/`MonadIO`. -}
hoistMonadIOError ∷ (MonadIO μ, AsIOError ε, MonadError ε μ)
                  ⇒ IO (Either IOException α) → μ α
hoistMonadIOError eio = liftIO eio >>= mapMError (_IOErr #)

----------------------------------------

{- | Perform some `IO`, catching any `IOError`s into a `MonadError` (in a
     `MonadIO`) -}
asIOError ∷ (AsIOError ε, MonadIO μ, MonadError ε μ) ⇒ IO α → μ α
asIOError = hoistMonadIOError ∘ ioMonadError

----------------------------------------

{- | Perform some `IO`, catching any `IOError`s into a `MonadError` (in a
     `MonadIO`); BUT any 'NoSuchThing' error (e.g., DoesNotExist) will be
     converted to `Nothing`. -}
asIOErrorY ∷ ∀ α ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) =>
            IO α -> μ (Maybe α)

asIOErrorY = join ∘ fmap squashNoSuchThing ∘ splitMError ∘ asIOError

----------------------------------------

{- | `asIOError` specialized to `IOError` -}
{-# DEPRECATED ioMonadError "use `asIOError @IOError` instead" #-}
ioMonadErr ∷ (MonadIO μ, MonadError IOError μ) ⇒ IO α → μ α
ioMonadErr = asIOError

----------------------------------------

_wrapIOErr ∷ MonadError ε μ ⇒ (ι → ε) → (β → ExceptT ι μ α) → β → μ α
_wrapIOErr f g =
  join ∘ fmap (either throwError return) ∘ fmap (first f) ∘ splitMError ∘ g

----------------------------------------

{- | Take an IO Action, convert it to a MonadError-function, wrapping any
     `AsIOError` with a custom type -}
wrapAsIOErr ∷ (MonadIO μ, AsIOError ι, MonadError ε μ) ⇒ (ι → ε) → IO α → μ α
wrapAsIOErr f = _wrapIOErr f asIOError

----------------------------------------

{- | Take an IO Action, convert it to a MonadError-function, wrapping any
     `IOError` with a custom type.
 -}
{-# DEPRECATED wrapIOErr "use `wrapAsIOErr @IOError` instead" #-}
wrapIOErr ∷ (MonadIO μ, MonadError ε μ) ⇒ (IOError → ε) → IO α → μ α
wrapIOErr f = _wrapIOErr f ioMonadErr

----------------------------------------

{- | Turn an `Exception` into an `IOError` within IO. -}
__monadIOError__ ∷ ∀ ε μ α . (MonadIO μ, Exception ε) ⇒ ExceptT ε IO α → μ α
__monadIOError__ io = liftIO $ splitMError io ≫ either throwIO return

{- | Unicode alias for __monadIOError__ -}
ӝ ∷ (MonadIO μ, Exception ε) ⇒ ExceptT ε IO α → μ α
ӝ = __monadIOError__

----------------------------------------

{- | Throw a `Printable` as a user error in `IO`. -}
ioThrow ∷ (MonadIO μ, Printable τ) ⇒ τ → μ α
ioThrow = liftIO ∘ System.IO.Error.ioError ∘ System.IO.Error.userError ∘ toString

{- | Convert a left value to an `IOException`. -}
eitherIOThrow ∷ (Show e, MonadIO μ) ⇒ Either e a → μ a
eitherIOThrow =
  either (liftIO ∘ System.IO.Error.ioError ∘ userError ∘ show) return

{- | Convert a MonadError into a `userError` in `IO`. -}
eitherIOThrowT ∷ (MonadIO μ, Show ε) ⇒ ExceptT ε μ α → μ α
eitherIOThrowT f = runExceptT f >>= eitherIOThrow

-- that's all, folks! ---------------------------------------------------------
