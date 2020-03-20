{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE UnicodeSyntax  #-}

module ProcLib.Types.ProcIO
  ( ProcIO, ProcIO', mkCmd, mkPIO, unCmd )
where

-- base --------------------------------

import Control.Applicative  ( Applicative, (<*>), pure )
import Control.Monad        ( Monad, (>>=), return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), const )
import Data.Functor         ( Functor, fmap )
import Data.String          ( String )
import System.Exit          ( ExitCode )
import System.IO            ( FilePath, IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- dlist -------------------------------

import Data.DList  ( DList, singleton )

-- lens --------------------------------

import Control.Lens  ( Lens', lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, logMessage, runLoggingT )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( ReaderT )
import Control.Monad.Trans   ( MonadTrans, lift )
import Control.Monad.Writer  ( MonadWriter, runWriterT, tell )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), assertFailure, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree, withResource' )

-- text --------------------------------

import Data.Text     ( Text )
import Data.Text.IO  ( readFile )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Types.ProcExecCtxt     ( ProcExecCtxt )
import ProcLib.Types.ProcIOAction     ( ProcIOAction )

--------------------------------------------------------------------------------

data ProcIO' ε η ω =
    Cmd { unCmd ∷ MonadError ε η ⇒
                       ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω }

type ProcIO ε η ω = MonadError ε η ⇒ ProcIO' ε η ω

instance (Monad η) ⇒ Functor (ProcIO' ε η) where
  fmap ∷ (α → β) → ProcIO' ε η α → ProcIO' ε η β
  fmap f (Cmd a) = Cmd (fmap f a)

instance (Monad η) ⇒ Applicative (ProcIO' ε η) where
  pure = Cmd ∘ pure
  (<*>) ∷ ProcIO' ε η (α → β) → ProcIO' ε η α → ProcIO' ε η β
  Cmd f <*> Cmd xs = Cmd (f <*> xs)


instance Monad η ⇒ Monad (ProcIO' ε η) where
  Cmd c >>=  f = Cmd (c >>=  unCmd ∘ f)

instance MonadTrans (ProcIO' ε) where
  lift ∷ Monad η ⇒ η α → ProcIO' ε η α
  lift = Cmd ∘ lift ∘ lift

mkCmd ∷ AsCreateProcError ε ⇒
        ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω → ProcIO' ε η ω
mkCmd = Cmd

mkPIO ∷ MonadError ε η ⇒
        ReaderT (ProcExecCtxt η) (Stream (Of ProcIOAction) η) ω → ProcIO' ε η ω
mkPIO = Cmd

-- given:
--    -) some logging text (actually, a function from "was it a mock?" to
--       logging text (Mocked → Text)
--    -) a mock value (ω)
--    -) an IO action (IO ω)
-- return:
--    -) A monad which, when told whether to mock, will (a) act (b) log (c)
--       return a value

data IOClass = IORead | IOWrite | IOCmdR | IOCmdW | IOExec
  deriving (Eq,Show)

class HasIOClass α where
  ioClass ∷ Lens' α IOClass

data Mock = DoMock | NoMock
  deriving (Eq,Show)

mkIO ∷ ∀ ω τ μ . (MonadIO μ, MonadLog τ μ, HasIOClass τ) ⇒ (Mock → τ) → ω → IO ω → Mock → μ ω
mkIO log mock_value io mock = do
  logMessage (log mock)
  case mock of
    NoMock → liftIO io
    DoMock → return mock_value

-- derive:
--    -) A general function to establish whether to mock
--    -) A general logger meta-fn which just surrounds its args with () or <>.
--    -) A fn which auto-mocks for suitable types (incl. Natural/Int), Lists, Seqs…
--    -) A fn which collects a list of actions

-- consider: a list, or class, of Things Wot IO Could Do, for the sake of logging
-- no, a typeclass; need an action (printable), and a list of arguments
-- (printable).  We can set up some standard actions, make them part of that
-- typeclass; and allow for other typeclasses that can also handle them.

newtype SimpleLogEntry = SimpleLogEntry (IOClass,Text)
  deriving (Eq,Show)

instance HasIOClass SimpleLogEntry where
  ioClass = lens (\ (SimpleLogEntry (c,_)) → c)
                 (\ (SimpleLogEntry (_,t)) c → SimpleLogEntry (c,t))

type SimpleLog = DList SimpleLogEntry

withResource2 ∷ IO α → (α → IO()) → IO β → (β → IO ()) → (IO α → IO β →TestTree)
              → TestTree
withResource2 gain lose gain' lose' tests =
  withResource gain lose (\ x → withResource gain' lose' (\ x' → tests x x'))

withResource2' ∷ IO α → IO β → (IO α → IO β → TestTree)
              → TestTree
withResource2' gain gain' tests =
  withResource' gain (\ x → withResource' gain' (\ x' → tests x x'))

myTests ∷ TestTree
myTests =
-- XXX add severity
-- XXX pureLoggingT
  let readFn ∷ (MonadIO μ, MonadWriter SimpleLog μ) ⇒ FilePath → μ Text
      readFn fn = runLoggingT (mkIO (const $ SimpleLogEntry (IORead, "Hello"))
                                    "mockety" (readFile fn) NoMock)
                              (tell ∘ singleton)
   in withResource2' (runWriterT $ readFn "/etc/subgid") (readFile "/etc/subgid") $ \ txtlog exptxt → 
        testGroup "my" [ testCase "txt" $ txtlog ≫ \ (txt,log) → exptxt ≫ \ exp → exp ≟ txt
                       , testCase "log" $ txtlog ≫ \ (txt,log) → [SimpleLogEntry (IORead,"Hello")] @=? log
                 ]


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "AbsPath" [ myTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
