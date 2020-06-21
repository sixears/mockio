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

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )
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
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≪), (≫), (⪼) )
import Data.MoreUnicode.Natural      ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, execParser, flag, flag', fullDesc, help
                            , helper, info, long, metavar, progDesc, short
                            , strArgument, strOption
                            )

-- std-main ----------------------------

import StdMain             ( doMain )
import StdMain.StdOptions  ( HasDryRun( dryRun ), HasStdOptions( stdOptions )
                           , StdOptions, parseStdOptions, quietitude,verbosity )
import StdMain.UsageError  ( AsUsageError, UsageError, throwUsage )

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

data Options = Options { fileName      ∷ Text
                       , writeFileName ∷ Maybe Text
                       , _stdOptions    ∷ StdOptions
                       }

instance HasStdOptions Options where
  stdOptions = lens _stdOptions (\ o s → o { _stdOptions = s })

instance HasDryRun Options where
  dryRun = stdOptions ∘ dryRun


fromEnum ∷ GHC.Enum.Enum α ⇒ α → ℕ
fromEnum = fromIntegral ∘ GHC.Enum.fromEnum

-- XXX Version that supplies o to each of filterVerbosity & io
-- XXX Version that expects () from IO, and specializes on UsageError
{- | The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
{-
xx_ ∷ ∀ ε ω . (Exception ε, Printable ε, AsUsageError ε) ⇒
     StdOptions → LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) () → IO ()
xx_ o io = Exited.doMain $ do
  filt ← filterVerbosity o
  logToStderr NoCallStack (filt io)
  return Exited.exitCodeSuccess
-}

main ∷ IO ()
main = do o ← execParser opts
          -- XXX Tidy This Up
          -- XXX UsageError
          -- XXX Add CallStack Options
          -- XXX More verbose options, incl. file,level
          doMain @UsageError (o ⊣ stdOptions) (xx o)
       where desc   = progDesc "simple 'head' re-implementation to test MockIO"
             opts   = info (parser ⊴ helper) (fullDesc ⊕ desc)
             parser = Options ⊳ strArgument (metavar "FILE")
                              ⊵ optional (strOption ( long "output" ⊕ short 'o'
                                                    ⊕ metavar "FILE"
                                                    ⊕ help "write output here")
                                         )
                              ⊵ parseStdOptions

xx ∷ (MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒
         Options → μ ()
xx opts = do
  let fn      = fileName opts
      dry_run = opts ⊣ dryRun
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
