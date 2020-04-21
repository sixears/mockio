-- {-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
-- {-# LANGUAGE ViewPatterns               #-}

module MockIO
  ( tests )
where

import Prelude  ( undefined )

-- base --------------------------------

import qualified  Control.Exception  as  E
import qualified  GHC.Stack

import Data.Bool            ( Bool( False, True ), not, otherwise )
import Control.Applicative  ( Applicative, (<*>), pure )
import Control.Monad        ( Monad, (>>=), mapM, return, unless )
import Control.Monad.Catch  ( MonadMask )
import Control.Monad.Identity  ( runIdentity )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, foldr, mapM_, toList )
import Data.Function        ( ($), (&), const, flip, id )
import Data.Functor         ( Functor, fmap )
import Data.List            ( zip )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid( mappend, mconcat, mempty ) )
import Data.Semigroup       ( Semigroup( (<>), sconcat, stimes ) )
import Data.String          ( String, lines )
import Data.Tuple           ( fst, snd )
import GHC.Exts             ( fromList )
import GHC.Stack            ( CallStack, HasCallStack, SrcLoc
                            , fromCallSiteList, getCallStack, popCallStack
                            , srcLocFile, srcLocStartLine
                            )
import System.Exit          ( ExitCode )
import System.IO            ( FilePath, IO, stderr )
import System.IO.Unsafe     ( unsafePerformIO )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText, toString )

-- dlist -------------------------------

import Data.DList  ( DList, singleton )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- lens --------------------------------

import Control.Lens  ( Lens', lens, view )

-- log-plus ----------------------------

import qualified  Log
import qualified  Log.LogEntry       as  LogEntry
import qualified  Log.LogRenderOpts

import Log                ( Log, logIO, logRender, _log0m, _log1m )
import Log.HasCallstack   ( HasCallstack( callstack ), stackHead )
import Log.HasSeverity    ( HasSeverity( severity ) )
import Log.HasUTCTime     ( HasUTCTimeY( utcTimeY ) )
import Log.LogEntry       ( LogEntry, logEntry, _le0, _le1, _le2, _le3 )
import Log.LogRenderOpts  ( LogRenderOpts )

-- logging-effect ----------------------

import qualified  Control.Monad.Log
import Control.Monad.Log  ( MonadLog, PureLoggingT, Severity(..)
                          , WithSeverity( WithSeverity )
                          , defaultBatchingOptions, logMessage, timestamp
                          , runLoggingT, runPureLoggingT
                          , withFDHandler
                          )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO, say )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFunctor( omap ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⊢) )
import Data.MoreUnicode.Monad    ( (⪼), (≫) )
import Data.MoreUnicode.Monoid   ( ф, ю )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )
import Control.Monad.Reader  ( ReaderT )
import Control.Monad.Trans   ( MonadTrans, lift )
import Control.Monad.Writer  ( MonadWriter, runWriterT, tell )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth( AvailablePerLine, Unbounded )
                                  , Pretty
                                  , SimpleDocStream(..)
                                  , (<+>)
                                  , align, annotate, brackets
                                  , defaultLayoutOptions, emptyDoc, enclose
                                  , hsep, indent, layoutPageWidth, layoutPretty
                                  , line, pretty
                                  , space, vsep
                                  )
import Data.Text.Prettyprint.Doc.Util  ( reflow )
import Data.Text.Prettyprint.Doc.Render.Util.Panic  ( panicInputNotFullyConsumed
                                                    , panicUncaughtFail
                                                    , panicUnpairedPop
                                                    )
import Data.Text.Prettyprint.Doc.Render.Text  ( renderStrict )

-- safe --------------------------------

import Safe  ( atMay, headMay )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, HUnitFailure( HUnitFailure )
                         , (@=?), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus   ( (≟), runTestsP, runTestsReplay, runTestTree,withResource' )
import TastyPlus2  ( assertListEq, withResource2' )

-- text --------------------------------

import qualified  Data.Text       as  T
import qualified  Data.Text.Lazy  as  LT

import Data.Text     ( Text, intercalate, pack, take, unlines, unpack )
import Data.Text.IO  ( readFile )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt2  ( fmt, formatUTCYDoW )

-- time --------------------------------

import Data.Time.Calendar  ( fromGregorian )
import Data.Time.Clock     ( UTCTime( UTCTime ), getCurrentTime
                           , secondsToDiffTime )
import Data.Time.Format    ( defaultTimeLocale, formatTime, rfc822DateFormat )

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




_li0 ∷ (MonadIO μ, MonadLog Log μ) ⇒ μ Text
_li0 = logIO Informational "li0" ⪼ return "Godzilla"

_li1 ∷ (MonadIO μ, MonadLog Log μ) ⇒ μ Text
_li1 = do
  _li0
  logIO Informational "li1"
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

data Mock = DoMock | NoMock
  deriving (Eq,Show)

mkIO' ∷ ∀ ω τ μ . (MonadIO μ, MonadLog τ μ) ⇒ (Mock → τ) → ω → IO ω → Mock → μ ω
mkIO' log mock_value io mock = do
  logMessage (log mock)
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

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)



{- | Render an instance of a `Pretty` type to text, with default options. -}
renderDoc ∷ Doc α → Text
renderDoc = renderStrict ∘ layoutPretty defaultLayoutOptions

data LogRenderType = LRO_Plain
                   | LRO_Severity
                   | LRO_TimeStamp
                   | LRO_StackHead
                   | LRO_StackHeadTS
                   | LRO_Stack
                   | LRO_StackTS
  deriving Show

     
{- | Render logs to stderr. -}
logsToStderr ∷ MonadIO μ ⇒ LogRenderOpts → PureLoggingT Log μ α → μ α
logsToStderr opts io = do
  (x,ts) ← logRender opts io
  mapM_ say ts
  return x

writerMonadTests ∷ TestTree
writerMonadTests =
  let helloEntry = fromList [ SimpleLogEntry(IORead,"Hello") ]
      readFn ∷ (MonadIO μ, MonadWriter (DList SimpleLogEntry) μ) ⇒ FilePath → Mock → μ Text
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
      readFn' ∷ (MonadIO μ) ⇒ FilePath → Mock → μ (Text, DList SimpleLogEntry)
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
  deriving (Eq,Show)

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

logMsgTests ∷ TestTree
logMsgTests =
  let helloEnt = fromList [ WithSeverity Informational $ SimpleLogEntry(IORead,"hello") ]
      readFn' ∷ (MonadIO μ) ⇒ FilePath → Mock → μ (Text, SimpleLog)
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

data WithAttr β α = WithAttr { attr ∷ β, datum ∷ α }
  deriving (Eq,Functor,Show)

ю̄ ∷ Monoid α ⇒ [α] → α
ю̄ = ю

_renderSimplyDecorated ∷ (Monoid α, HasIOClass δ, Show δ) ⇒
                        (Text → α) → (δ → α) → (δ → α) → SimpleDocStream δ → α
_renderSimplyDecorated text push pop = go []
  where
    go _           SFail               = panicUncaughtFail
    go []          SEmpty              = ф
    go (_:_)       SEmpty              = panicInputNotFullyConsumed
    go []          (SChar c rest)      = text (T.singleton c) ⊕ go []    rest
    go []          (SText _l t rest)   = text t ⊕ go []    rest
    go []          (SLine i rest)      = text (T.singleton '\n') ⊕ text (T.replicate i " ") ⊕ go [] rest
    go stack       (SChar c rest)      = text (T.singleton c) ⊕ go stack rest
    go stack@(s:_) (SText _l t rest) | s ⊣ ioClass ≡ IORead = go stack rest
                                     | otherwise            = text (pack $ "]>" ⊕ show stack ⊕ "<[") ⊕ text t ⊕ go stack rest
    go stack       (SLine i rest)      = text (T.singleton '\n') ⊕ text (T.replicate i " ") ⊕ go stack rest
    go stack       (SAnnPush ann rest) = push ann ⊕ go (ann : stack) rest
    go (ann:stack) (SAnnPop rest)      = pop ann ⊕ go stack rest
    go []          SAnnPop{}           = panicUnpairedPop

-- λ> let sdoc = layoutPretty defaultLayoutOptions ("hello" <+> annotate IORead "world" <+> annotate IOWrite "and mum" <> "!")
-- λ> Data.Text.IO.putStrLn (Main.renderSimplyDecorated id (\ _ -> ">>>") (\ _ -> "<<<") sdoc)


--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ filterDocTests, writerMonadTests, pureLoggingTests
                           , logMsgTests, Log.tests
                           , Log.LogRenderOpts.tests
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
