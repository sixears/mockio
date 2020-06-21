{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( doMain )
where

-- base --------------------------------

import Control.Applicative  ( many )
import Control.Exception    ( Exception )
import Control.Monad        ( return )
import Data.Function        ( ($), flip, id )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≤) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- exited ------------------------------

import qualified  Exited2  as  Exited

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, filterSeverity, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Debug, Informational
                                                        , Notice, Warning ) )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, execParser, flag, flag', fullDesc, help
                            , helper, info, long, metavar, progDesc, short
                            , strArgument, strOption
                            )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions  ( StdOptions, filterVerbosity, verbosityLevel )
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
