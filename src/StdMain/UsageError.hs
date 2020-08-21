{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain.UsageError
  ( AsUsageError( _UsageError ), UsageError, UsageIOError
  , readUsage, throwUsage, usageError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Eq            ( Eq )
import Data.Function      ( ($), id )
import Data.Maybe         ( Maybe( Just, Nothing ), maybe )
import Text.Read          ( Read, readMaybe )
import Text.Show          ( Show( show ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString, toText )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism' )
import Control.Lens.Review  ( (#) )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ), IOError )

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

------------------------------------------------------------

data UsageIOError = UIOE_USAGE_ERROR UsageError
                  | UIOE_IO_ERROR    IOError

instance Exception UsageIOError

_UIOE_USAGE_ERROR ∷ Prism' UsageIOError UsageError
_UIOE_USAGE_ERROR = prism' (\ e → UIOE_USAGE_ERROR e)
                           (\ case UIOE_USAGE_ERROR e → Just e; _ → Nothing)

_UIOE_IO_ERROR ∷ Prism' UsageIOError IOError
_UIOE_IO_ERROR = prism' (\ e → UIOE_IO_ERROR e)
                        (\ case UIOE_IO_ERROR e → Just e; _ → Nothing)

instance Show UsageIOError where
  show (UIOE_USAGE_ERROR e) = show e
  show (UIOE_IO_ERROR    e) = show e

instance AsUsageError UsageIOError where
  _UsageError = _UIOE_USAGE_ERROR

instance AsIOError UsageIOError where
  _IOError = _UIOE_IO_ERROR

instance Printable UsageIOError where
  print (UIOE_USAGE_ERROR e) = print e
  print (UIOE_IO_ERROR    e) = print e

-- that's all, folks! ----------------------------------------------------------
