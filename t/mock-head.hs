{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Debug.Trace  ( trace, traceShow )

import Prelude  ( (-), error, fromIntegral, maxBound, minBound, pred, succ )

-- base --------------------------------

import qualified  GHC.Enum

import Control.Applicative     ( many, optional )
import Control.Exception       ( Exception )
import Control.Monad           ( Monad, forM_, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), otherwise )
import Data.Either             ( Either( Left, Right ) )
import Data.Function           ( ($), const, flip, id )
import Data.List               ( take )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Ord                ( (<), (>) )
import System.IO               ( IO, IOMode( WriteMode )
                               , hClose, openFile, stdout )
import Text.Show               ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡), (≢) )
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

import Control.Lens.Prism   ( Prism' )
import Control.Lens.Review  ( (#) )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, filterSeverity, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Debug, Informational
                                                        , Notice, Warning ) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- monaderror-io -----------------------

import MonadError  ( ѥ )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≪), (≫), (⪼) )
import Data.MoreUnicode.Natural      ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( execParser, flag, flag', fullDesc, help, helper, info
                            , long, metavar, progDesc, short, strArgument
                            , strOption
                            )

-- text --------------------------------

import Data.Text     ( Text, lines, unpack )
import Data.Text.IO  ( hPutStrLn, readFile )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( DoMock, NoMock ), mkIO, mkIO' )
import MockIO.IOClass  ( HasIOClass( ioClass ), IOClass( IORead, IOWrite ) )

--------------------------------------------------------------------------------

-- a very simple version of 'head', for testing MockIO

data Options = Options { fileName ∷ Text
                       , writeFileName ∷ Maybe Text
                       , verbose ∷ ℕ
                       , quiet   ∷ ℕ
                       , dryRun ∷ DoMock
                       }


fromEnum ∷ GHC.Enum.Enum α ⇒ α → ℕ
fromEnum = fromIntegral ∘ GHC.Enum.fromEnum

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

-- XXX Change error for MonadError
-- XXX AsUsageError
verbosityLevel ∷ (AsUsageError ε, MonadError ε η) ⇒ Options → η Severity
verbosityLevel opts =
  let v = verbose opts
      q = quiet opts
      warnTooLow  x = [fmt|warning: attempt to exceed min verbosity level %w|] x
      warnTooHigh x = [fmt|warning: attempt to exceed max verbosity level %w|] x
      succs 0 x = return x
      succs n x | x ≡ maxBound = throwUsage (warnTooHigh x)
                | otherwise    = succs (n-1) (succ x)
      preds 0 x = return x
      preds n x | x ≡ minBound = throwUsage (warnTooLow x)
                | otherwise    = preds (n-1) (pred x)
      l = case v > q of
            True  → succs (v-q) Notice
            False → preds (q-v) Notice
   in l

filterVerbosity ∷ ∀ ε η υ ω α .
                  (AsUsageError ε, MonadError ε η, MonadLog (Log ω) υ) ⇒
                  Options → η (LoggingT (Log ω) υ α → υ α)
filterVerbosity opts = verbosityLevel opts ≫ return ∘ filterSeverity ∘ flip (≤)

-- XXX Version that supplies o to each of filterVerbosity & io
-- XXX Version that expects () from IO, and specializes on UsageError
{- | The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
xx ∷ ∀ ε ω . (Exception ε, Printable ε, AsUsageError ε) ⇒
     Options → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) () → IO ()
xx o io = Exited.doMain $
  filterVerbosity o ≫ \ filt -> logToStderr NoCallStack (filt io) ⪼ return Exited.exitCodeSuccess

-- Exited.doMain (filterVerbosity @UsageError o >>= \ filt -> logToStderr NoCallStack (filt $ doMain o) >> return System.Exit.exitSuccess)

main ∷ IO ()
main = do o ← execParser opts
          -- XXX Tidy This Up
          -- XXX UsageError
          -- XXX Add CallStack Options
          -- XXX More verbose options, incl. file,level
--           ѥ (filterVerbosity @UsageError o ≫ \ filt → logToStderr NoCallStack (filt $ doMain o)) ≫ \ case { Left e → error $ show e; Right r → return r }
--          Exited.doMain (filterVerbosity @UsageError o ≫ \ filt -> logToStderr NoCallStack (filt $ doMain o) ⪼ return Exited.exitCodeSuccess)
          xx @UsageError o (doMain o)
       where desc   = progDesc "simple 'head' re-implementation to test MockIO"
             opts   = info (parser ⊴ helper) (fullDesc ⊕ desc)
             parser = Options ⊳ strArgument (metavar "FILE")
                              ⊵ optional (strOption ( long "output" ⊕ short 'o'
                                                    ⊕ metavar "FILE"
                                                    ⊕ help "write output here")
                                         )
                              ⊵ (length ⊳ many (flag' () (short 'v')))
                              ⊵ (length ⊳ many (flag' () (long "quiet")))
                              ⊵ (flag NoMock DoMock ( long "dry-run" ⊕ short 'n'
                                                    ⊕ help "dry run"))

doMain ∷ (MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒
         Options → μ ()
doMain opts = do
  let fn     = fileName opts
      dry_run = dryRun opts
  when False (throwUsage "fake error")
  fh ← case writeFileName opts of
         Nothing  → return stdout
         Just wfn → do
                  let logmsg DoMock = [fmtT|(write %t)|] wfn
                      logmsg NoMock = [fmtT|write %t|] wfn
                  mkIO' Notice IOWrite logmsg
                                (openFile "/dev/null" WriteMode)
                                (openFile (unpack wfn) WriteMode) dry_run
                      
  txt ← mkIO Informational IORead ([fmtT|read %t|] fn) "mock text"
             (readFile (unpack fn)) NoMock
  liftIO $ forM_ (take 10 (lines txt)) (hPutStrLn fh)
  when (fh ≢ stdout) $ liftIO (hClose fh)
  return ()

  
-- that's all, folks! ----------------------------------------------------------
