{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO
  ( DoMock(..), mkIO, tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Eq        ( Eq )
import Data.Function  ( ($), (&), const )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import GHC.Exts       ( fromList )
import GHC.Stack      ( SrcLoc( SrcLoc ) )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- log-plus ----------------------------

import Log           ( CSOpt( FullCallStack ), Log, ToDoc_
                     , logIO, logIOT, logToStderr )
import Log.LogEntry  ( logEntry )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational ), logMessage
                          , runPureLoggingT )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊢) )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( pretty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus         ( (≟), runTestsP, runTestsReplay
                         , runTestTree,withResource' )
import TastyPlus2        ( withResource2' )
import TastyPlus.Equish  ( Equish( (≃) ) )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( putStrLn, readFile )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime, getCurrentTime )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.IOClass  ( HasIOClass( ioClass ), IOClass( IORead ) )

--------------------------------------------------------------------------------

-- TODO:
-- add logIO' to carry arbitrary data; same for log & log'
-- tighten up naming; split out mocking & logging; terminal colouring
-- demo logging methods incl. stderr
-- clearer mock logging (e.g. (CMD) vs [CMD]); options handlers for logs
-- cmd logging using showcmdforuser

_li0 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li0 = logIOT Informational "li0" ⪼ return "Godzilla"

_li1 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li1 = do
  _ ← _li0
  logIOT Informational "li1"
  return "MUTO"

-- given:
--    -) some logging text (actually, a function from "was it a mock?" to
--       logging text (Mocked → Text)
--    -) a mock value (ω)
--    -) an IO action (IO ω)
-- return:
--    -) A monad which, when told whether to mock, will (a) act (b) log (c)
--       return a value

data DoMock = DoMock | NoMock
  deriving (Eq,Show)

mkIO ∷ ∀ ω τ μ α .
       (MonadIO μ, MonadLog (Log ω) μ, Default ω, HasIOClass ω, ToDoc_ τ) ⇒
       Severity → IOClass → (DoMock → τ) → α → IO α → DoMock → μ α
mkIO sv ioc lg mock_value io mock = do
  logIO sv (def & ioClass ⊢ ioc) (lg mock)
  case mock of
    NoMock → liftIO io
    DoMock → return mock_value

-- XXX simplify logging
-- XXX simple functions for severity

{- This exists here for testing only; real file reading/writing will be in a
   different package. -}
-- enhancements: toDoc(), set mock text
readFn ∷ ∀ ω μ . (MonadIO μ, MonadLog (Log ω) μ, Default ω, HasIOClass ω) ⇒
         String → DoMock → μ Text
readFn s = mkIO Informational IORead (const $ [fmtT|read %s|] s) "mock text" $
           readFile s

readFnTests ∷ TestTree
readFnTests =
  let src_loc     = SrcLoc "main" "MockIO" "src/MockIO.hs" 192 3 192 58
      call_list   = [("logIO"∷String,src_loc)]
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group") IORead

      log_ ∷ UTCTime → Log IOClass
      log_ t       = fromList [ log_entry t ]

      log_string ∷ UTCTime → Log IOClass → String
      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource2' (runPureLoggingT $ readFn @IOClass "/etc/group" NoMock)
                     (readFile "/etc/group") $ \ txtlog exptxt →
        testGroup "readFn"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← exptxt
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        t ← getCurrentTime
                                        assertBool (log_string t lg)
                                                   (log_ t ≃ lg)
                  ]

readFnMockTests ∷ TestTree
readFnMockTests =
  let src_loc     = SrcLoc "main" "MockIO" "src/MockIO.hs" 192 3 192 58
      call_list   = [("logIO"∷String,src_loc)]
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group") IORead

      log_ ∷ UTCTime → Log IOClass
      log_ t       = fromList [ log_entry t ]

      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource' (runPureLoggingT $ readFn @IOClass "/etc/group" DoMock) $
                    \ txtlog  →
        testGroup "readFn-Mock"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← return "mock text"
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        t ← getCurrentTime
                                        assertBool (log_string t lg)
                                                   (log_ t ≃ lg)
                  ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ readFnTests, readFnMockTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

_testm ∷ IO ()
_testm = do
  gr ← logToStderr FullCallStack $ readFn @IOClass "/etc/group" NoMock
  putStrLn "---- /etc/group"
  putStrLn gr
  putStrLn "---------------"

-- that's all, folks! ----------------------------------------------------------
