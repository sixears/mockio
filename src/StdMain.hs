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

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- exited ------------------------------

import qualified  Exited2  as  Exited
import Exited2  ( ToExitCode )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴) )
import Data.MoreUnicode.Lens         ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser
                            , execParser, fullDesc, helper, info, progDesc )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions  ( StdOptions, SuperStdOptions
                           , filterVerbosity, parseSuperStdOptions, stdOptions )
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

doMain ∷ ∀ ε ω σ μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
          StdOptions → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) σ
        → μ ()
doMain o io = Exited.doMain $ do
  filt ← filterVerbosity o
  logToStderr NoCallStack (filt io)


yy ∷ ∀ ε α σ ω .
     (Exception ε, Printable ε, AsUsageError ε, ToExitCode σ) ⇒
     Parser α
   → (SuperStdOptions α → LoggingT (Log ω) (LoggingT (Log ω)
                                                     (ExceptT ε IO)) σ)
   → IO ()
yy p io = do
  let -- opts = info (parseSuperStdOptions p ⊴ helper) (fullDesc ⊕ desc)
      desc = "simple 'head' re-implementation to test MockIO"
--  o ← execParser opts
  o ← parseOpts Nothing desc (parseSuperStdOptions p)
  doMain (o ⊣ stdOptions) (io o)

-- that's all, folks! ----------------------------------------------------------
