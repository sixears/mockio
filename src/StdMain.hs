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

import MockIO  ( DoMock( DoMock, NoMock ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens   ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )
import Control.Monad.Reader  ( ReaderT, runReaderT )

-- natural-plus ------------------------

import Natural  ( Natty, One, one )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions  ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                           , StdOptions, ifDryRun, options, parseStdOptions )
import StdMain.UsageError  ( AsUsageError, UsageError )

--------------------------------------------------------------------------------

{- | Like `stdMain`, but gives the incoming `io` full access to the `StdOptions`
     object. -}
stdMain_ ∷ ∀ ε α σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
           Natty ν
         → Text
         → Parser α
         → (StdOptions ν α → LoggingT (Log ω)(LoggingT (Log ω)(ExceptT ε IO)) σ)
         → μ ()
stdMain_ n desc p io = do
  o ← parseOpts Nothing desc (parseStdOptions n p)
  Exited.doMain $ logToStderr NoCallStack (filterMinSeverity o (io o))

----------

{- | Execute the 'main' of a standard program with standard options that returns
     a toExitCode, that may throw exceptions; logging as requested by cmdline
     options.

     The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
stdMain ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
          Natty ν
        → Text
        → Parser α
        → (DryRunLevel ν → α
                         → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) σ)
        → μ ()
stdMain n desc p io =
  stdMain_ n desc p (\ o → io (o ⊣ dryRunLevel) (o ⊣ options))

type LogTIO ω ε = (LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)))

stdMainx ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
          Natty ν
        → Text
        → Parser α
        → (α → ReaderT (DryRunLevel ν) (LogTIO ω ε) σ)
        → μ ()
stdMainx n desc p io =
  stdMain_ n desc p (\ o → runReaderT (io (o ⊣ options)) (o ⊣ dryRunLevel))

----------

{- | More simpley-typed version of `stdMain`, where the error is specifically a
     `UsageError`, and there is a single dry-run level which is translated to
     DoMock/NoMock; intended for simple IO programs.

     Note that although the `io` arg. is typed to a `ReaderT`, much simpler
     types - e.g., `MonadIO ⇒ μ ()`, or `MonadIO ⇒ μ ExitCode` - will suffice.
 -}
stdMain' ∷ ∀ ω ρ σ μ . (MonadIO μ, ToExitCode σ) ⇒
           Text
         → Parser ρ
         → (DoMock → ρ → ReaderT (DryRunLevel One) (LogTIO ω UsageError) σ)
         → μ ()
stdMain' desc parser io =
  let go opts = do
        mock ← ifDryRun DoMock NoMock
        io mock opts
   in stdMainx @UsageError one desc parser go

-- that's all, folks! ----------------------------------------------------------
