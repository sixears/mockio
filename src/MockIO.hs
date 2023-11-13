{-# LANGUAGE UnicodeSyntax #-}
module MockIO
  ( DoMock(..)
  , HasDoMock(doMock)
  , mkIO
  , mkIO'
  , mkIO'ME
  , mkIOME
  , tests
  ) where

-- base --------------------------------

import Control.Monad ( join, return )
import Data.Function ( ($) )
import Data.String   ( String )
import System.Exit   ( ExitCode )
import System.IO     ( IO )

-- monaderror-io -----------------------

import MonadError ( ѥ )

-- monadio-plus ------------------------

import MonadIO ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad   ( (≫) )
import Data.MoreUnicode.Natural ( ℕ )

-- mtl -----------------------

import Control.Monad.Except ( ExceptT, MonadError )
import Control.Monad.Reader ( ReaderT )

-- tasty -------------------------------

import Test.Tasty ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit ( testCase )

-- tasty-plus --------------------------

import TastyPlus ( runTestTree, runTestsP, runTestsReplay, withResource',
                   withResource2', (≟) )

-- text --------------------------------

import Data.Text    ( Text )
import Data.Text.IO ( readFile )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.DoMock ( DoMock(DoMock, NoMock), HasDoMock(doMock) )

--------------------------------------------------------------------------------

{- | Create an IO action that may be mocked.

     given:
        -) an IO action (IO ω)
     return:
        -) A monad which, when told whether to mock, will (a) act (b) return a
           value.
-}
mkIO' ∷ ∀ μ α .
        MonadIO μ ⇒
        IO α         -- ^ mock value; IO is available here so that, e.g., in
                     -- ^ case of mock a file open, /dev/null is opened instead
      → IO α         -- ^ the IO to perform when not mocked
      → DoMock       -- ^ whether to mock
      → μ α
mkIO' _          io NoMock = liftIO io
mkIO' mock_value _  DoMock = liftIO mock_value

{- | Mildly simplified `mkIO'`, specifically taking a non-IO mock value.
 -}
mkIO ∷ ∀ μ α . MonadIO μ ⇒ α → IO α → DoMock → μ α
mkIO mock_value io mck = mkIO' (return mock_value) io mck

----------------------------------------


{- | mkIO', for `ExceptT` IO values. -}
mkIO'ME ∷ ∀ μ ε α .
          MonadError ε μ ⇒
          ExceptT ε μ α -- ^ mock value; IO is available here so that, e.g., in
                        -- ^ case of mock a file open, /dev/null is opened
                        -- ^ instead
        → ExceptT ε μ α -- ^ the IO to perform when not mocked
        → DoMock        -- ^ whether to mock
        → μ α

mkIO'ME _          io NoMock = join (ѥ io)
mkIO'ME mock_value _  DoMock = join (ѥ mock_value)

----------------------------------------

{- | Mildly simplified `mkIO'ME`, specifically taking a non-IO mock value. -}
mkIOME ∷ ∀ μ ε α . MonadError ε μ ⇒ α → ExceptT ε μ α → DoMock → μ α
mkIOME mock_value io mck = mkIO'ME (return mock_value) io mck

----------------------------------------

{-| run in a reader context with a constant NoMock value -}
noMock ∷ ∀ η α . ReaderT DoMock η α → η α
noMock = flip runReaderT NoMock

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

{- | This exists here for testing only; real file reading/writing will be in a
     different package. -}
readFn ∷ ∀ μ . MonadIO μ ⇒ String → DoMock → μ Text
readFn s = mkIO "mock text" $ readFile s

----------------------------------------

readFnTests ∷ TestTree
readFnTests =
  withResource2' (readFn "/etc/group" NoMock)
                 (readFile "/etc/group") $ \ txt exptxt →
    testGroup "readFn"
              [ testCase "/etc/group" $ do exp ← exptxt
                                           txt ≫ (exp ≟)
              ]

----------------------------------------

readFnMockTests ∷ TestTree
readFnMockTests =
  withResource' (readFn "/etc/group" DoMock) $ \ txt  →
    testGroup "readFn-Mock"
              [ testCase "/etc/group" $ txt ≫ ("mock text" ≟)
              ]

----------------------------------------

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

-- that's all, folks! ----------------------------------------------------------
