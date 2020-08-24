{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import qualified  System.IO

import Control.Applicative     ( optional )
import Control.Monad           ( return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Function           ( ($) )
import Data.List               ( take )
import Data.Maybe              ( Maybe( Just, Nothing ), maybe )
import System.IO               ( Handle, IO, IOMode( WriteMode ), stdout )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- fpath -------------------------------

import FPath.AbsFile    ( AbsFile )
import FPath.Parseable  ( readM )

-- log-plus ----------------------------

import Log  ( Log )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational, Warning ) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monoid       ( ю )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser
                            , help, long, metavar, option, short, strArgument )

-- std-main ----------------------------

import StdMain             ( stdMain' )
import StdMain.UsageError  ( AsUsageError )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text, lines, unlines, unpack )
import Data.Text.IO  ( hPutStr )

-- tfmt --------------------------------

import Text.Fmt  ( fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO          ( DoMock( DoMock, NoMock ), mkIO, mkIO' )
import MockIO.IOClass  ( IOClass( IORead, IOWrite ) )

--------------------------------------------------------------------------------

-- a very simple version of 'head', for testing MockIO

data Options = Options { fileName  ∷ Text
                       , outputFile ∷ Maybe AbsFile
                       }

parseOptions ∷ Parser Options
parseOptions = Options ⊳ strArgument (metavar "FILE")
                       ⊵ optional (option readM (ю [ long "output"
                                                   , short 'o'
                                                   , metavar "FILE"
                                                   , help "write output here"
                                                   ])
                                  )

main ∷ IO ()
main = -- XXX Tidy This Up
       -- XXX UsageError
       -- XXX Add CallStack Options
       -- XXX More verbose options, incl. file,level,callstack,ioclass
       -- verbose=[3|WARNING|wARn]:{@callstack/ioread}:/
       -- XXX compress main, xx
       -- decent help texts for std options, particualrly --verbose with all its
       -- ioclasses, etc.
       -- make {} in verbose options optional (if ':' not used within); use + to separate
       -- add 'append' to log file options
       -- better emsg for failing to open log file
       -- add ioclass to log message
       -- more visually obvious dry-runness; e.g., colour, italics?
       -- log rolling!
       stdMain' "simple 'head' re-implementation to test MockIO" parseOptions go

go ∷ (MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒
     DoMock → Options → μ ()
go mck opts = do
  let fn = fileName opts
  txt ← take 10 ∘ lines ⊳ readFile fn
  writeFile mck (outputFile opts) (unlines txt)

withFile ∷ MonadIO μ ⇒ AbsFile → IOMode → (Handle → IO ω) → μ ω
withFile fn mode = liftIO ∘ System.IO.withFile (toString fn) mode

withWriteFile ∷ (MonadIO μ, MonadLog (Log IOClass) μ) ⇒
                DoMock → α → Maybe AbsFile → (Handle → IO α) → μ α
withWriteFile mck a fn io = do
  let fname         = maybe "-STDOUT-" toText fn
      logmsg DoMock = [fmtT|(write %t)|] fname
      logmsg NoMock = [fmtT|write %t|]   fname
  case fn of
    Nothing  → liftIO $ io stdout
    Just wfn → 
        mkIO' Warning IOWrite logmsg (return a) (withFile wfn WriteMode io) mck

writeFile ∷ (MonadIO μ, MonadLog (Log IOClass) μ) ⇒
            DoMock → Maybe AbsFile → Text → μ ()
writeFile mck fnY txt = withWriteFile mck () fnY (\ h → hPutStr h txt)

readFile ∷ (MonadIO μ, MonadLog (Log IOClass) μ) ⇒ Text → μ Text
readFile fn = let logmsg = [fmtT|read %t|] fn
                  result = Data.Text.IO.readFile (unpack fn)
               in mkIO Informational IORead logmsg "" result NoMock

-- that's all, folks! ----------------------------------------------------------
