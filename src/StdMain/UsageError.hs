{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain.UsageError
  ( AsUsageError( _UsageError ), UsageError
  , readUsage, throwUsage, usageError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Maybe         ( maybe )
import Text.Read          ( Read, readMaybe )
import Text.Show          ( Show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

--------------------------------------------------------------------------------

data UsageError = UsageError Text
  deriving (Eq,Show)

instance Exception UsageError

----------------------------------------

class AsUsageError ε where
  _UsageError ∷ Prism' ε UsageError

--------------------

instance AsUsageError UsageError where
  _UsageError = id

--------------------

instance Printable UsageError where
  print (UsageError txt) = P.text txt

------------------------------------------------------------

usageError ∷ ∀ τ ε . (AsUsageError ε, Printable τ) ⇒ τ → ε
usageError t = _UsageError # UsageError (toText t)

----------------------------------------

throwUsage ∷ ∀ τ ε ω η . (Printable τ, AsUsageError ε, MonadError ε η) ⇒ τ → η ω
throwUsage t = throwError $ usageError t

----------------------------------------

readUsage ∷ ∀ τ ε ω η . (AsUsageError ε, MonadError ε η, Read ω, Printable τ) ⇒
            τ → η ω
readUsage s = let errMsg = [fmtT|failed to parse: '%T'|] s
               in maybe (throwUsage $ errMsg) return (readMaybe $ toString s)

-- that's all, folks! ----------------------------------------------------------
