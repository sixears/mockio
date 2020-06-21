{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( doMain )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Control.Monad      ( return )
import Data.Function      ( ($) )
import System.IO          ( IO )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import qualified  Exited2  as  Exited

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions  ( StdOptions, filterVerbosity )
import StdMain.UsageError  ( AsUsageError )

--------------------------------------------------------------------------------

-- XXX Version that supplies o to each of filterVerbosity & io
-- XXX Version that expects () from IO, and specializes on UsageError
{- | The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
doMain ∷ ∀ ε ω . (Exception ε, Printable ε, AsUsageError ε) ⇒
     StdOptions → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) () → IO ()
doMain o io = Exited.doMain $ do
  filt ← filterVerbosity o
  logToStderr NoCallStack (filt io)
  return Exited.exitCodeSuccess


-- that's all, folks! ----------------------------------------------------------
