{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Monad        ( forM_, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function        ( ($), const )
import Data.List            ( take )
import Data.Ord             ( (<), (>) )
import System.IO            ( IO )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )
import Data.Ord.Unicode     ( (≤) )

-- log-plus ----------------------------

import Log  ( CSOpt( NoCallStack ), Log, filterSeverity, logToStderr )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Debug, Informational, Warning ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≪) )

-- optparse-applicative ----------------

import Options.Applicative  ( execParser, fullDesc, helper, info
                            , metavar, progDesc, strArgument )

-- text --------------------------------

import Data.Text     ( Text, lines, unpack )
import Data.Text.IO  ( putStrLn, readFile )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( DoMock, NoMock ), mkIO )
import MockIO.IOClass  ( HasIOClass( ioClass ), IOClass( IORead, IOWrite ) )

--------------------------------------------------------------------------------

-- a very simple version of 'head', for testing MockIO

data Options = Options { fileName ∷ Text }


main ∷ IO ()
main = do o ← execParser opts
          -- writefn, Notice
          -- turn off batching?
          logToStderr NoCallStack (filterSeverity (≤ Informational) (doMain o)) -- ≪ execParser opts
       where desc   = progDesc "simple 'head' re-implementation to test MockIO"
             opts   = info (parser ⊴ helper) (fullDesc ⊕ desc)
             parser = Options ⊳ strArgument (metavar "FILE")

doMain ∷ (MonadLog (Log IOClass) μ, MonadIO μ) ⇒ Options → μ ()
doMain opts = do
  let fn = fileName opts
  txt ← mkIO Informational IORead (const $ [fmtT|read %t|] fn) "mock text"
             (readFile (unpack fn)) NoMock -- XXX
  liftIO $ forM_ (take 10 (lines txt)) putStrLn
  mkIO Warning IOWrite (\ _ → "boing, said Zebedee" ∷ Text) "me-ma" (return "obo") DoMock
  mkIO Warning IOWrite (\ _ → "urmentrude" ∷ Text) "me-ma" (return "obo") DoMock
  return ()

  
-- that's all, folks! ----------------------------------------------------------
