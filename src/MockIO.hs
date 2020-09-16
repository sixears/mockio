{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO
  ( DoMock(..), HasDoMock( doMock ), MockIOClass, mkIO, mkIO', tests )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($), (&), const )
import Data.Maybe     ( Maybe( Just ) )
import Data.String    ( String )
import GHC.Exts       ( fromList )
import GHC.Stack      ( SrcLoc( SrcLoc ) )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode  ( (∧) )
import Data.Eq.Unicode    ( (≡) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens.Lens  ( lens )

-- log-plus ----------------------------

import Log           ( CSOpt( FullCallStack ), Log, ToDoc_( toDoc_ )
                     , logIO, logIOT, logToStderr )
import Log.LogEntry  ( LogEntry, logEntry )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity( Informational )
                          , runPureLoggingT )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens     ( (⊢) )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( parens, pretty )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus         ( (≟), runTestsP, runTestsReplay
                         , runTestTree,withResource', withResource2' )
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

import MockIO.DoMock        ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )
import MockIO.IOClass       ( HasIOClass( ioClass ), IOClass( IORead ) )

--------------------------------------------------------------------------------

_li0 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li0 = logIOT Informational "li0" ⪼ return "Godzilla"

_li1 ∷ (MonadIO μ, MonadLog (Log ()) μ) ⇒ μ Text
_li1 = do
  _ ← _li0
  logIOT Informational "li1"
  return "MUTO"

{- | Create an IO action that may be mocked; and log it.

     given:
        -) some logging text (actually, a function from "was it a mock?" to
           logging text (Mocked → Text)
        -) a mock value (ω)
        -) an IO action (IO ω)
     return:
        -) A monad which, when told whether to mock, will (a) act (b) log (c)
           return a value.  The IOClass, and whether the value was actually
           mocked, are annotated in the log.
-}
mkIO' ∷ ∀ ω τ μ α .
         (MonadIO μ, ToDoc_ τ,
          MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         Severity     -- ^ log severity
       → IOClass      -- ^ log with this IOClass
       → (DoMock → τ) -- ^ log message, is given {Do,No}Mock so message can
                      -- ^ visually identify whether it really happened
       → IO α         -- ^ mock value; IO is available here so that, e.g., in
                      -- ^ case of mock a file open, /dev/null is opened instead
       → IO α         -- ^ the IO to perform when not mocked
       → DoMock       -- ^ whether to mock
       → μ α
mkIO' sv ioc lg mock_value io mck = do
  logIO sv (def & ioClass ⊢ ioc & doMock ⊢ mck) (lg mck)
  case mck of
    NoMock → liftIO io
    DoMock → liftIO mock_value

--------------------

{- | Mildly simplified `mkIO'`, specifically with a constant log message
     (that is surrounded in parens in case of DoMock); and a non-IO mock value.
 -}
mkIO ∷ ∀ ω τ μ α .
        (MonadIO μ, ToDoc_ τ, 
         MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
        Severity → IOClass → τ → α → IO α → DoMock → μ α
mkIO sv ioc lg mock_value io mck =
  let plog l DoMock = parens (toDoc_ l)
      plog l NoMock = toDoc_ l
   in mkIO' sv ioc (plog lg) (return mock_value) io mck

{- This exists here for testing only; real file reading/writing will be in a
   different package. -}
readFn ∷ ∀ ω μ . (MonadIO μ,
                  MonadLog (Log ω) μ, Default ω, HasIOClass ω, HasDoMock ω) ⇒
         String → DoMock → μ Text
readFn s = mkIO' Informational IORead (const $ [fmtT|read %s|] s)
                                      (return "mock text") $
           readFile s

{- | A unification of IOClass & DoMock for simple mockio logging. -}
data MockIOClass = MockIOClass IOClass DoMock
  deriving Show

instance Default MockIOClass where
  def = MockIOClass def NoMock

instance Equish MockIOClass where
  (MockIOClass i m) ≃ (MockIOClass i' m') = (i ≃ i') ∧ (m ≡ m')

instance HasIOClass MockIOClass where
  ioClass = lens (\ (MockIOClass i _) → i)
                 (\ (MockIOClass _ m) i → MockIOClass i m)

instance HasDoMock MockIOClass where
  doMock = lens (\ (MockIOClass _ m) → m)
                 (\ (MockIOClass i _) m → MockIOClass i m)

readFnTests ∷ TestTree
readFnTests =
  let src_loc     = SrcLoc "main" "MockIO" "src/MockIO.hs" 192 3 192 58
      call_list   = [("logIO"∷String,src_loc)]
      log_entry   ∷ UTCTime → LogEntry MockIOClass
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group")
                             (MockIOClass IORead NoMock)

      log_ ∷ UTCTime → Log MockIOClass
      log_ t       = fromList [ log_entry t ]

      log_string ∷ UTCTime → Log MockIOClass → String
      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource2' (runPureLoggingT $ readFn @MockIOClass "/etc/group" NoMock)
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
      log_entry   ∷ UTCTime → LogEntry MockIOClass
      log_entry t = logEntry call_list (Just t) Informational
                             (pretty @Text "read /etc/group")
                             (MockIOClass IORead NoMock)

      log_ ∷ UTCTime → Log MockIOClass
      log_ t       = fromList [ log_entry t ]

      log_string t lg = [fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (log_ t) lg

   in withResource' (runPureLoggingT $ readFn "/etc/group" DoMock) $
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
  gr ← logToStderr FullCallStack [] $ readFn @MockIOClass "/etc/group" NoMock
  putStrLn "---- /etc/group"
  putStrLn gr
  putStrLn "---------------"

-- that's all, folks! ----------------------------------------------------------
