{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain.UsageError
  ( AsUsageError( _UsageError ), UsageError
  , throwUsage, usageError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Text.Show          ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

--------------------------------------------------------------------------------

data UsageError = UsageError Text
  deriving (Eq,Show)

instance Exception UsageError

class AsUsageError ε where
  _UsageError ∷ Prism' ε UsageError

instance AsUsageError UsageError where
  _UsageError = id

instance Printable UsageError where
  print (UsageError txt) = P.text txt

usageError ∷ ∀ τ ε . (AsUsageError ε, Printable τ) ⇒ τ → ε
usageError t = _UsageError # UsageError (toText t)

throwUsage ∷ ∀ τ ε ω η . (Printable τ, AsUsageError ε, MonadError ε η) ⇒ τ → η ω
throwUsage t = throwError $ usageError t

-- that's all, folks! ----------------------------------------------------------
