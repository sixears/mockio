{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( stdMain, stdMain' )
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

import Log  ( CSOpt( NoCallStack ), Log, filterMinSeverity, logToStderr )

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

import StdMain.StdOptions  ( HasDryRun( dryRun ), StdOptions
                           , options, parseStdOptions )
import StdMain.UsageError  ( AsUsageError, UsageError )

--------------------------------------------------------------------------------

{- | Like `stdMain`, but gives the incoming `io` full access to the `StdOptions`
     object. -}
stdMain_ ∷ ∀ ε α σ ω μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
           Text
         → Parser α
         → (StdOptions α → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) σ)
         → μ ()
stdMain_ desc p io = do
  o ← parseOpts Nothing desc (parseStdOptions p)
  Exited.doMain $ do
    -- filt ← filterVerbosity o
    logToStderr NoCallStack (filterMinSeverity o (io o))

----------

{- | Execute the 'main' of a standard program with standard options that returns
     a toExitCode, that may throw exceptions; logging as requested by cmdline
     options.

     The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
stdMain ∷ ∀ ε α σ ω μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
          Text
        → Parser α
        → (DoMock → α → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) σ)
        → μ ()
stdMain desc p io = stdMain_ desc p (\ o → io (o ⊣ dryRun) (o ⊣ options))

----------

{- | Simpler type-signature for stdMain', where the io is expected to return
     `()`, and the error is specifically a `UsageError`; intended for simple IO
     programs.
 -}
stdMain' ∷ ∀ ε α ω μ .
           (MonadIO μ, ε ~ UsageError) ⇒
           Text
         → Parser α
         → (DoMock → α → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) ())
         → μ ()
stdMain' = stdMain

-- that's all, folks! ----------------------------------------------------------
