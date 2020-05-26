{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO
  ( tests )
where

-- base --------------------------------

import Data.Bool            ( Bool( False, True ), not, otherwise )
import Control.Applicative  ( Applicative, (<*>), pure )
import Control.Monad        ( Monad, (>>=), return )
import Data.Eq              ( Eq )
import Data.Function        ( ($), (&), const, id )
import Data.Functor         ( Functor, fmap )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid )
import Data.String          ( String )
import GHC.Exts             ( fromList )
import GHC.Stack            ( SrcLoc( SrcLoc ) )
import System.Exit          ( ExitCode )
import System.IO            ( FilePath, IO )
import Text.Show            ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- dlist -------------------------------

import Data.DList  ( DList )

-- lens --------------------------------

import Control.Lens  ( Lens', lens )

-- log-plus ----------------------------

import Log           ( Log, fromList, logIO, logIO', logIOT )
import Log.Equish    ( Equish( (≃) ) )
import Log.LogEntry  ( logEntry )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog, Severity(..), WithSeverity( WithSeverity )
                          , logMessage, runLoggingT, runPureLoggingT )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Monoid   ( ю )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( ReaderT )
import Control.Monad.Trans   ( MonadTrans, lift )
import Control.Monad.Writer  ( MonadWriter, runWriterT, tell )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( SimpleDocStream(..)
                                  , annotate, defaultLayoutOptions, hsep
                                  , layoutPretty, line, pretty
                                  )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus   ( (≟), runTestsP, runTestsReplay, runTestTree,withResource' )
import TastyPlus2  ( withResource2' )

-- text --------------------------------

import Data.Text     ( Text, pack )
import Data.Text.IO  ( readFile )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime, getCurrentTime )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError )
import ProcLib.Types.ProcExecCtxt     ( ProcExecCtxt )
import ProcLib.Types.ProcIOAction     ( ProcIOAction )

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

data DoMock = DoMock | NoMock
  deriving (Eq,Show)

-- mkIO' ∷ ∀ ω τ μ . (MonadIO μ, MonadLog τ μ) ⇒ (DoMock → τ) → ω → IO ω → DoMock → μ ω
mkIO' ∷ (MonadIO μ, MonadLog τ μ) ⇒ (DoMock → τ) → ω → IO ω → DoMock → μ ω
mkIO' log mock_value io mock = do
  logMessage (log mock)
  case mock of
    NoMock → liftIO io
    DoMock → return mock_value

mkIORead ∷ (MonadIO μ, MonadLog (Log ω) μ, Default ω, HasIOClass ω) ⇒
           (DoMock → Text) → α → IO α → DoMock → μ α
mkIORead log mock_value io mock = do
  logIO Informational (def & ioClass ⊢ IORead) (log mock)
  case mock of
    NoMock → liftIO io
    DoMock → return mock_value

newtype SimpleLogEntry = SimpleLogEntry (IOClass,Text)
  deriving (Eq,Show)

instance HasIOClass SimpleLogEntry where
  ioClass = lens (\ (SimpleLogEntry (c,_)) → c)
                 (\ (SimpleLogEntry (_,t)) c → SimpleLogEntry (c,t))

type SimpleLog = DList (WithSeverity SimpleLogEntry)

{- | Fold a function across a stream; intended for use as a subclause of other
     folds. -}
foldDocStream ∷ (SimpleDocStream α → SimpleDocStream α) → SimpleDocStream α
              → SimpleDocStream α
foldDocStream f (SText l t rest)    = SText l t (f rest)
foldDocStream f (SLine i rest)      = SLine i (f rest)
foldDocStream f (SChar c rest)      = SChar c (f rest)
foldDocStream f (SAnnPush ann rest) = SAnnPush ann (f rest)
foldDocStream f (SAnnPop rest)      = SAnnPop (f rest)
foldDocStream _ SEmpty              = SEmpty
foldDocStream _ SFail               = SFail

dropEmptyAnnotations ∷ SimpleDocStream α → SimpleDocStream α
dropEmptyAnnotations (SAnnPush _ (SAnnPop rest)) = dropEmptyAnnotations rest
dropEmptyAnnotations ss = foldDocStream dropEmptyAnnotations ss

{- | Filter a doc stream on annotations; only annotations that match the given
     predicate may pass. -}
filterDocStream ∷ (α → 𝔹) → SimpleDocStream α → SimpleDocStream α
filterDocStream p = dropEmptyAnnotations ∘ go []
  where
    -- For Text,Char,Line; filter out things that match the predicate, else
    -- just keep calm & carry on.
    go (stack@(s:_)) (SText l t rest) | p s = SText l t (go stack rest)
                                      | otherwise = go stack rest
    go (stack@(s:_)) (SChar c rest)   | p s = SChar c (go stack rest)
                                      | otherwise = go stack rest
    go (stack@(s:_)) (SLine i rest)   | p s = SLine i (go stack rest)
                                      | otherwise = go stack rest

    -- Stack push & pop.
    go stack         (SAnnPush ann rest) = SAnnPush ann (go (ann : stack) rest)
    go (_:stack)     (SAnnPop rest)      = SAnnPop (go stack rest)

    go stack ss = foldDocStream (go stack) ss

filterDocTests ∷ TestTree
filterDocTests =
  let -- note that sdoc has SText ("begin"), SLine (line), and SChar (hsep)
      sdoc ∷ SimpleDocStream IOClass
      sdoc = layoutPretty defaultLayoutOptions
                          (ю̄ [ "begin"
                             , line
                             , hsep [ annotate IORead "ioread"
                                    , annotate IOCmdR "iocmdr"
                                    , annotate IOWrite "iowrite"
                                    ]
                             , line
                             , "end"
                             ])
      sdoc_none ∷ SimpleDocStream IOClass
      sdoc_none = layoutPretty defaultLayoutOptions
                               (ю̄ [ "begin", line, hsep [ "","", "" ], line
                                  , "end" ])

      sdoc_internal ∷ SimpleDocStream IOClass
      sdoc_internal = layoutPretty defaultLayoutOptions
                                   (ю̄ [ "begin"
                                      , line
                                      , hsep [ annotate IORead "ioread", ""
                                             , annotate IOWrite "iowrite"
                                             ]
                                      , line
                                      , "end"
                                      ])

   in testGroup "filterDoc"
                [ testCase "all may pass" $
                    sdoc @=? filterDocStream (const True) sdoc
                , testCase "none shall pass" $
                    sdoc_none @=? filterDocStream (const False) sdoc
                , testCase "isInternalIO" $
                    sdoc_internal @=? filterDocStream isInternalIO sdoc
                ]

{- | Note the awkward capitalization, to avoid clashing with
     `GHC.Stack.HasCallStack` -}
-- class HasCallstack α where
--   callStack' ∷ Lens' α CallStack

-- instance HasCallstack CallStack where
--   callStack' = id

-- class HasUTCTimeY α where
--   utcTimeY ∷ Lens' α (Maybe UTCTime)

-- instance HasUTCTimeY (Maybe UTCTime) where
--   utcTimeY = id

data LogRenderType = LRO_Plain
                   | LRO_Severity
                   | LRO_TimeStamp
                   | LRO_StackHead
                   | LRO_StackHeadTS
                   | LRO_Stack
                   | LRO_StackTS
  deriving Show

writerMonadTests ∷ TestTree
writerMonadTests =
  let helloEntry = fromList [ SimpleLogEntry(IORead,"Hello") ]
      readFn ∷ (MonadIO μ, MonadWriter (DList SimpleLogEntry) μ) ⇒ FilePath → DoMock → μ Text
      readFn fn mock = runLoggingT (mkIO' (const helloEntry) "mockety"
                                         (readFile fn) mock) tell
   in testGroup "writerMonad"
                [ withResource2' (runWriterT $ readFn "/etc/subgid" NoMock)
                                 (readFile "/etc/subgid") $ \ txtlog exptxt →
                    testGroup "NoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    exp ← exptxt
                                                    exp ≟ txt
                              , testCase "log" $ do (_,lg) ← txtlog
                                                    helloEntry @=? lg
                              ]
                , withResource' (runWriterT $ readFn "/etc/subgid" DoMock) $
                    \ txtlog →
                    testGroup "DoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    "mockety" ≟ txt
                              , testCase "log" $ do (_,lg) ← txtlog
                                                    helloEntry @=? lg
                              ]
                ]

pureLoggingTests ∷ TestTree
pureLoggingTests =
  let helloEntry = fromList [ SimpleLogEntry(IORead,"Hello") ]
      readFn' ∷ (MonadIO μ) ⇒ FilePath → DoMock → μ (Text, DList SimpleLogEntry)
      readFn' fn mock = runPureLoggingT (mkIO' (const helloEntry) "mockety"
                                        (readFile fn) mock)
   in testGroup "pureLogging"
                [ withResource2' (readFn' "/etc/subgid" NoMock)
                                 (readFile "/etc/subgid") $ \ txtlog exptxt →
                    testGroup "NoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    exp ← exptxt
                                                    exp ≟ txt
                              , testCase "log" $ do (_,lg) ← txtlog
                                                    helloEntry @=? lg
                              ]
                , withResource' (readFn' "/etc/subgid" DoMock) $ \ txtlog →
                    testGroup "DoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    "mockety" ≟ txt
                              , testCase "log" $ do (_,lg) ← txtlog
                                                    helloEntry @=? lg
                              ]
                ]

-- XXX simplify logging
-- XXX simple functions for severity

data IOClass = IORead  -- ^ An IO action that perceives but does not alter state
             | IOWrite -- ^ An IO action that may alter state
             | IOCmdR  -- ^ An external cmd (results in an execve or fork call)
                       --   that perceives but does not alter state
             | IOCmdW  -- ^ An external cmd (results in an execve or fork call)
                       --   that may alter state
             | IOExec  -- ^ An exec (replaces this executable)
             | NoIO    -- ^ no IO
  deriving (Eq,Show)

instance Default IOClass where
  def = NoIO

instance Equish IOClass where
  i ≃ i' = i ≡ i'

class HasIOClass α where
  ioClass ∷ Lens' α IOClass

instance HasIOClass IOClass where
  ioClass = id

{-| Predicate for IO that outside of this process; that is, exclude `IORead` &
    `IOWrite`; leaving `IOCmdR`, `IOCmdW`, `IOExec`. -}
isExternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isExternalIO a = case a ⊣ ioClass of
                   IORead  → False
                   IOWrite → False
                   IOCmdR  → True
                   IOCmdW  → True
                   IOExec  → True

{-| Logical inverse of `isExternalIO`. -}
isInternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isInternalIO = not ∘ isExternalIO

{- | "Log Message" the noun, rather than the verb; turn a simple message into a
     Log Message, with IOClass & Severity. -}
logMsg ∷ Severity → IOClass → Text → DList (WithSeverity SimpleLogEntry)
logMsg sv clss msg = fromList [WithSeverity sv (SimpleLogEntry(clss,msg))]

logInfo ∷ IOClass → Text → DList (WithSeverity SimpleLogEntry)
logInfo = logMsg Informational

{- This exists here for testing only; real file reading/writing will be in a
   different package. -}
-- enhancements: toDoc(), set mock text
readFn ∷ ∀ ω μ . (MonadIO μ, MonadLog (Log ω) μ, Default ω, HasIOClass ω) ⇒
         String → DoMock → μ Text
readFn s = mkIORead (const $ [fmt|read %s|] s) "mock text" (readFile s)

logMsgTests ∷ TestTree
logMsgTests =
  let -- helloEnt = fromList [ WithSeverity Informational $ SimpleLogEntry(IORead,"hello") ]
      readFn' ∷ (MonadIO μ) ⇒ FilePath → DoMock → μ (Text, SimpleLog)
      readFn' fn mock = runPureLoggingT (mkIO' (const $ logInfo IORead "hello")
                                               "mockety" (readFile fn) mock)
      my_log_entry t = logEntry [("logIO"∷String,SrcLoc "main" "MockIO" "src/MockIO.hs" 193 3 193 58)] (Just t) Informational (pretty @Text "read /etc/subgid") IORead
      my_log ∷ UTCTime → Log IOClass
      my_log t = fromList [ my_log_entry t ]
   in withResource2' (runPureLoggingT $ readFn @IOClass "/etc/subgid" NoMock)
                     (readFile "/etc/subgid") $ \ txtlog exptxt →
        testGroup "logMsg"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← exptxt
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        t ← getCurrentTime
                                        assertBool ([fmt|my_log:\n  exp: %w\nvs.\n  got: %w|] (my_log t) lg)
                                                   (my_log t ≃ lg)
                  ]

oldLogMsgTests ∷ TestTree
oldLogMsgTests =
  let helloEnt = fromList [ WithSeverity Informational $ SimpleLogEntry(IORead,"hello") ]
      readFn' ∷ (MonadIO μ) ⇒ FilePath → DoMock → μ (Text, SimpleLog)
      readFn' fn mock = runPureLoggingT (mkIO' (const $ logInfo IORead "hello")
                                               "mockety" (readFile fn) mock)
   in withResource2' (readFn' "/etc/subgid" NoMock)
                     (readFile "/etc/subgid") $ \ txtlog exptxt →
        testGroup "logMsg"
                  [ testCase "txt" $ do (txt,_) ← txtlog
                                        exp ← exptxt
                                        exp ≟ txt
                  , testCase "log" $ do (_,lg) ← txtlog
                                        helloEnt @=? lg
                  ]

ю̄ ∷ Monoid α ⇒ [α] → α
ю̄ = ю

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ filterDocTests, writerMonadTests, pureLoggingTests
                           , logMsgTests
                           ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
