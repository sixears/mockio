{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Prelude  ( fromIntegral )

-- base --------------------------------

import qualified  GHC.Enum

import Control.Applicative     ( optional )
import Control.Monad           ( forM_, return, when )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False ) )
import Data.Function           ( ($) )
import Data.List               ( take )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import System.IO               ( IO, IOMode( WriteMode )
                               , hClose, openFile, stdout )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≢) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational, Notice ) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Natural      ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( execParser, fullDesc, help
                            , helper, info, long, metavar, progDesc, short
                            , strArgument, strOption
                            )

-- std-main ----------------------------

import StdMain             ( doMain )
import StdMain.StdOptions  ( HasDryRun( dryRun ), HasStdOptions( stdOptions )
                           , StdOptions, parseStdOptions )
import StdMain.UsageError  ( AsUsageError, UsageError, throwUsage )

-- text --------------------------------

import Data.Text     ( Text, lines, unpack )
import Data.Text.IO  ( hPutStrLn, readFile )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( DoMock, NoMock ), mkIO, mkIO' )
import MockIO.IOClass  ( IOClass( IORead, IOWrite ) )

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
