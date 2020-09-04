{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO.Error2
  ( module MonadError.IO.Error
  , isInappropriateTypeError
  , squashInappropriateType, squashInappropriateTypeB, squashInappropriateTypeT
  )
where

import MonadError.IO.Error
import System.IO.Error.Lens  ( _InappropriateType )
import System.IO.Error         ( ioeGetErrorType )
import Data.MoreUnicode      ( 𝔹 )
import Data.Function.Unicode  ( (∘) )
import Control.Lens.Fold     ( has )
import Control.Lens.Getter   ( to )
import Data.Either           ( Either )
import Data.Maybe           ( Maybe )
import Data.Functor ( fmap )
import Control.Monad  ( join )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

import MonadError  ( splitMError )

--------------------------------------------------------------------------------

{- | Is a given IOError a NoSuchThing (DoesNotExist)? -}
isInappropriateTypeError ∷ AsIOError ε ⇒ ε → 𝔹
isInappropriateTypeError =
  has (_IOErr ∘ to ioeGetErrorType ∘ _InappropriateType )

{- | Given an Either IOError α (typically, a MonadError IOError μ ⇒ μ α),
     convert an 'InappropriateType' error to a Nothing of Maybe α.
 -}
squashInappropriateType ∷ (AsIOError ε, MonadError ε μ) ⇒
                          Either ε α → μ (Maybe α)
squashInappropriateType = squashIOErrs [isInappropriateTypeError]

{- | `squashInappropriateType` for `ExceptT` -}
squashInappropriateTypeT ∷ (AsIOError ε, MonadError ε μ) ⇒
                           ExceptT ε μ α → μ (Maybe α)
squashInappropriateTypeT = join ∘ fmap squashInappropriateType ∘ splitMError

{- | `squashInappropriateType` specialized to `𝔹` (akin to `squashIOErrsB` -}
squashInappropriateTypeB ∷ (AsIOError ε, MonadError ε μ) ⇒ Either ε 𝔹 → μ 𝔹
squashInappropriateTypeB = squashIOErrsB [isInappropriateTypeError]

-- that's all, folks! ----------------------------------------------------------
