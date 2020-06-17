{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Debug.Trace  ( trace, traceShow )

import Prelude  ( (-), error, fromIntegral, maxBound, minBound, pred, succ )

-- base --------------------------------

import qualified  GHC.Enum

import Control.Applicative     ( many, optional )
import Control.Monad           ( forM_, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), otherwise )
import Data.Function           ( ($), const )
import Data.List               ( take )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Ord                ( (<), (>) )
import System.IO               ( IO, IOMode( WriteMode )
                               , hClose, openFile, stdout )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡), (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≤) )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, filterSeverity, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Debug, Informational
                                              , Notice, Warning ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≪) )
import Data.MoreUnicode.Natural      ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( execParser, flag, flag', fullDesc, help, helper, info
                            , long, metavar, progDesc, short, strArgument
                            , strOption
                            )

-- text --------------------------------

import Data.Text     ( Text, lines, unpack )
import Data.Text.IO  ( hPutStrLn, readFile )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( DoMock, NoMock ), mkIOL, mkIOL' )
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

-- XXX Change error for MonadError
verbosityLevel opts =
  let v = verbose opts
      q = quiet opts
      warnTooLow  x = [fmt|warning: attempt to exceed min verbosity level %w|] x
      warnTooHigh x = [fmt|warning: attempt to exceed max verbosity level %w|] x
      succs 0 x = x
      succs n x | x ≡ maxBound = error (warnTooHigh x)
                | otherwise    = succs (n-1) (succ x)
      preds 0 x = x
      preds n x | x ≡ minBound = error (warnTooLow x)
                | otherwise    = preds (n-1) (pred x)
      l = case v > q of
            True  → succs (v-q) Notice
            False → preds (q-v) Notice
   in l

filterVerbosity opts = filterSeverity (≤ verbosityLevel opts)

main ∷ IO ()
main = do o ← execParser opts
          -- writefn, Notice
          logToStderr NoCallStack (filterVerbosity o (doMain o)) -- ≪ execParser opts
       where desc   = progDesc "simple 'head' re-implementation to test MockIO"
             opts   = info (parser ⊴ helper) (fullDesc ⊕ desc)
             parser = Options ⊳ strArgument (metavar "FILE")
                              ⊵ optional (strOption ( long "output"
                                                    ⊕ metavar "FILE"
                                                    ⊕ help "write output here")
                                         )
                              ⊵ (length ⊳ many (flag' () (short 'v')))
                              ⊵ (length ⊳ many (flag' () (long "quiet")))
                              ⊵ (flag NoMock DoMock (long "dry-run" ⊕ short 'n' ⊕ help "dry run"))

doMain ∷ (MonadLog (Log IOClass) μ, MonadIO μ) ⇒ Options → μ ()
doMain opts = do
  let fn   = fileName opts
      mock = dryRun opts
  fh ← case writeFileName opts of
         Nothing  → return stdout
         Just wfn → do
                  let logmsg = [fmtT|write %t|] fn
                  mkIOL' Notice IOWrite (const logmsg)
                                (openFile "/dev/null" WriteMode)
                                (openFile (unpack wfn) WriteMode) mock
                      
  txt ← mkIOL Informational IORead ([fmtT|read %t|] fn) "mock text"
             (readFile (unpack fn)) NoMock -- XXX
  liftIO $ forM_ (take 10 (lines txt)) (hPutStrLn fh)
  when (fh ≢ stdout) $ liftIO (hClose fh)
  return ()

  
-- that's all, folks! ----------------------------------------------------------
