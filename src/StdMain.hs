{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( doMain, yy )
where

-- base --------------------------------

import Control.Exception       ( Exception )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Nothing ) )
import System.IO               ( IO )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import qualified  Exited2  as  Exited
import Exited2  ( ToExitCode )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio ------------------------------

import MockIO  ( DoMock )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions  ( HasDryRun( dryRun ), HasVerbosity
                           , filterVerbosity, options, parseSuperStdOptions )
import StdMain.UsageError  ( AsUsageError )

--------------------------------------------------------------------------------

-- XXX Do we still need the explicit return?
-- XXX Version that supplies o to each of filterVerbosity & io
-- XXX Version that expects () from IO, and specializes on UsageError
{- | Execute the 'main' of a standard program with standard options that returns
     a toExitCode, that may throw exceptions; logging as requested by cmdline
     options.

     The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}

doMain ∷ ∀ ε σ ω α μ .
         (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, HasVerbosity σ,
          ToExitCode α) ⇒
         σ → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α → μ ()
doMain o io = Exited.doMain $ do
  filt ← filterVerbosity o
  logToStderr NoCallStack (filt io)

yy ∷ ∀ ε α σ ω μ .
     (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
     Text → Parser α → (DoMock → α → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) σ) → μ ()
yy desc p io = do
  o ← parseOpts Nothing desc (parseSuperStdOptions p)
  doMain o (io (o ⊣ dryRun) (o ⊣ options))

-- that's all, folks! ----------------------------------------------------------
