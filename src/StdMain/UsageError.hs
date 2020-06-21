{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain.UsageError
  ( AsUsageError( _UsageError ), UsageError, throwUsage )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Function      ( ($), id )
import Text.Show          ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

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
  deriving Show

instance Exception UsageError

class AsUsageError ε where
  _UsageError ∷ Prism' ε UsageError

instance AsUsageError UsageError where
  _UsageError = id

instance Printable UsageError where
  print (UsageError txt) = P.text txt

usageError ∷ AsUsageError ε ⇒ Text → ε
usageError t = _UsageError # UsageError t

throwUsage ∷ (AsUsageError ε, MonadError ε η) ⇒ Text → η ω
throwUsage t = throwError $ usageError t

-- that's all, folks! ----------------------------------------------------------
