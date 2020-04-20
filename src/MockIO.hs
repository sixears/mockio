{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module MockIO
  ( Log, WithLog, log, logIO, logRender, logRender', lroRenderers
  , renderWithCallStack, renderWithSeverity, renderWithSeverityAndTimestamp
  , renderWithStackHead, renderWithTimestamp
  , assertListEq
  , tests
  )
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

import qualified  Log.LogEntry  as  LogEntry

import Log.LogEntry      ( LogEntry, logEntry )
import Log.HasCallstack  ( HasCallstack( callstack ), stackHead )
import Log.HasSeverity   ( HasSeverity( severity ) )
import Log.HasUTCTime    ( HasUTCTimeY( utcTimeY ) )

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
import TastyPlus2  ( withResource2' )

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

logIO' ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒
         Severity → Doc () → μ ()
logIO' sv doc = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv doc

{- | `WithLog` adds in the `CallStack` constraint, so that if you declare your
     function to use this constraint, your function will be included in the
     logged callstack.  If you do not include the `CallStack` constraint, then
     the callpoint from within the function lacking the constraint (and anything
     calling it) will not be shown in the callstack.
 -}
type WithLog   η = (MonadLog Log η, ?stack ∷ CallStack)
{- | `WithLog`, but with MonadIO, too. -}
type WithLogIO μ = (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack)

-- We redefine this, rather than simply calling logIO_, so we don't mess with
-- the callstack.
-- logIO ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒
logIO ∷ WithLogIO μ ⇒ Severity → Text → μ ()
logIO sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (pretty txt)

log ∷ WithLog μ ⇒ Severity → Text → μ ()
log sv txt = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (pretty txt)




-- test data
_li0 ∷ (MonadIO μ, MonadLog Log μ) ⇒ μ Text
_li0 = logIO Informational "li0" ⪼ return "Godzilla"

_li1 ∷ (MonadIO μ, MonadLog Log μ) ⇒ μ Text
_li1 = do
  _li0
  logIO Informational "li1"
  return "MUTO"

_cs0 ∷ CallStack
_cs0 = fromCallSiteList []

_cs1 ∷ CallStack
_cs1 = fromCallSiteList [ ("stack0",GHC.Stack.SrcLoc "z" "x" "y" 9 8 7 6) ]

_cs2 ∷ CallStack
_cs2 = fromCallSiteList [ ("stack0",GHC.Stack.SrcLoc "a" "b" "c" 1 2 3 4)
                        , ("stack1",GHC.Stack.SrcLoc "d" "e" "f" 5 6 7 8) ]

_tm ∷ UTCTime
_tm = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

_le0 ∷ LogEntry
_le0 = logEntry _cs2 (Just _tm) Informational (pretty ("log_entry 1" ∷ Text))

_le1 ∷ LogEntry
_le1 =
  logEntry _cs1 Nothing Critical (pretty ("multi-line\nlog\nmessage" ∷ Text))

_le2 ∷ LogEntry
_le2 =
  let valign = align ∘ vsep
   in logEntry _cs1 (Just _tm) Warning ("this is" ⊞ valign [ "a"
                                                           , "vertically"
                                                           ⊞ valign [ "aligned"
                                                                    , "message"
                                                                    ]
                                                           ])
_le3 ∷ LogEntry
_le3 = 
  logEntry _cs1 Nothing Emergency (pretty ("this is the last message" ∷ Text))

_log0 ∷ Log
_log0 = Log $ fromList [_le0]

_log0m ∷ MonadLog Log η ⇒ η ()
_log0m = logMessage _log0

_log1 ∷ Log
_log1 = Log $ fromList [ _le0, _le1, _le2, _le3 ]

_log1m ∷ MonadLog Log η ⇒ η ()
_log1m = logMessage _log1

renderTests ∷ TestTree
renderTests =
  let render  t = logRender' (LogRenderOpts [] Unbounded {- t -})
      render' o = runIdentity ∘ logRender' o
      line10 = AvailablePerLine 10 1.0
      renderLine10 t = logRender' (LogRenderOpts [] line10 {- t -})
      c = GHC.Stack.fromCallSiteList [("foo",GHC.Stack.SrcLoc "a" "b" "c" 1 2 3 4)]
      exp2 = [ intercalate "\n" [ "[Info] log_entry 1"
                                , "  stack0, called at c:1:2 in a:b"
                                , "    stack1, called at f:5:6 in d:e"
                                ]
             ]
      exp3 = [ "[1970-01-01Z00:00:00 Thu] [Info] «c#1» log_entry 1"
             , intercalate "\n" [   "[-----------------------] [CRIT] «y#9» "
                                  ⊕ "multi-line"
                                ,   "                                       "
                                  ⊕ "log"
                                ,   "                                       "
                                  ⊕ "message"
                                ]                   
             , intercalate "\n"
                           [ "[1970-01-01Z00:00:00 Thu] [Warn] «y#9» this is a"
                           ,   "                                               "
                             ⊕ "vertically aligned"
                           ,   "                                               "
                             ⊕ "           message"
                           ]                   
             , "[-----------------------] [EMRG] «y#9» this is the last message"
             ]
   in testGroup "render" $
                [ assertListEq "render2" exp2 (render' lroRenderSevCS _log0m)
                , assertListEqIO "render3"
                                 exp3 (logRender' lroRenderTSSevCSH _log1m)
                ]

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

prettyCallStack ∷ [(String,SrcLoc)] → Doc ann
prettyCallStack [] = "empty callstack"
prettyCallStack (root:rest) =
  prettyCallSite root ⊕ line ⊕ indent 2 (vsep (prettyCallSite ⊳ rest))
  where prettyCallSite (f,loc) =
          pretty (LT.pack f) ⊕ ", called at " ⊕
          pretty (LT.pack (GHC.Stack.prettySrcLoc loc))

renderWithCallStack ∷ HasCallstack δ ⇒ (δ -> Doc ρ) -> δ -> Doc ρ
renderWithCallStack f m =
  f m ⊕ line ⊕ indent 2 (prettyCallStack (getCallStack $ m ⊣ callstack))

renderWithStackHead ∷ HasCallstack δ ⇒ (δ -> Doc ρ) -> δ -> Doc ρ
renderWithStackHead f m =
  let renderStackHead = renderLocation ∘ fmap snd
   in renderStackHead (stackHead m) ⊞ align (f m)

locToString ∷ SrcLoc → String
-- locToString loc = "«" ⊕ srcLocFile loc ⊕ "#" ⊕ show (srcLocStartLine loc) ⊕ "»"
locToString loc = [fmt|«%s#%w»|] (srcLocFile loc) (srcLocStartLine loc)
                   
renderLocation ∷ Maybe SrcLoc → Doc α
renderLocation (Just loc) = pretty $ locToString loc
renderLocation Nothing    = emptyDoc

-- class HasUTCTimeY α where
--   utcTimeY ∷ Lens' α (Maybe UTCTime)

-- instance HasUTCTimeY (Maybe UTCTime) where
--   utcTimeY = id

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

renderWithSeverity_ ∷ HasSeverity τ ⇒ τ → Doc ρ
renderWithSeverity_ m =
  let pp ∷ HasSeverity α ⇒ α → Doc ann
      pp sv = pretty $ case sv ⊣ severity of
                         Emergency     → ("EMRG" ∷ Text)
                         Alert         → "ALRT"
                         Critical      → "CRIT"
                         Warning       → "Warn"
                         Notice        → "Note"
                         Informational → "Info"
                         Debug         → "Debg"
   in pp m

renderWithSeverity ∷ HasSeverity τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithSeverity f m =
  brackets (renderWithSeverity_ m) ⊞ align (f m)

renderWithSeverityAndTimestamp ∷ (HasSeverity τ, HasUTCTimeY τ) ⇒
                                 (τ → Doc ρ) → τ → Doc ρ
renderWithSeverityAndTimestamp f m =
  let pp ∷ HasSeverity α ⇒ α → Doc ann
      pp sv = pretty $ case sv ⊣ severity of
                         Emergency     → ("EMRG" ∷ Text)
                         Alert         → "ALRT"
                         Critical      → "CRIT"
                         Warning       → "Warn"
                         Notice        → "Note"
                         Informational → "Info"
                         Debug         → "Debg"
   in brackets (renderWithTimestamp_ m ⊕ "|" ⊕ renderWithSeverity_ m) ⊞ align (f m)


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

     
type LogRenderer = (LogEntry → Doc ()) → LogEntry → Doc ()

data LogRenderOpts =
  LogRenderOpts { {-| List of log renderers; applied in list order, tail of the
                      list first; hence, given that many renderers add something
                      to the LHS, the head of the list would be lefthand-most in
                      the resulting output.
                   -}
                  _lroRenderers  ∷ [LogRenderer]
                , _lroWidth      ∷ PageWidth
                }

{- | `LogRenderOpts` with no adornments.  Page width is `Unbounded`; you can
     override this with lens syntax, e.g.,

     > lroRenderPlain & lroWidth .~ AvailablePerLine 80 1.0
 -}
lroRenderPlain ∷ LogRenderOpts
lroRenderPlain = LogRenderOpts [] Unbounded

{- | `LogRenderOpts` with timestamp & severity.
 -}
lroRenderTSSev ∷ LogRenderOpts
lroRenderTSSev =
  LogRenderOpts [ renderWithTimestamp, renderWithSeverity ] Unbounded

{- | `LogRenderOpts` with severity & callstack.
 -}
lroRenderSevCS ∷ LogRenderOpts
lroRenderSevCS =
  LogRenderOpts [ renderWithCallStack, renderWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack.
 -}
lroRenderTSSevCS ∷ LogRenderOpts
lroRenderTSSevCS =
  LogRenderOpts [ renderWithCallStack,renderWithTimestamp, renderWithSeverity ]
                Unbounded

{- | `LogRenderOpts` with severity & callstack head.
 -}
lroRenderSevCSH ∷ LogRenderOpts
lroRenderSevCSH =
  LogRenderOpts [ renderWithSeverity, renderWithStackHead ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack head.
 -}
lroRenderTSSevCSH ∷ LogRenderOpts
lroRenderTSSevCSH =
  LogRenderOpts [ renderWithTimestamp, renderWithSeverity,renderWithStackHead ]
                Unbounded

lroRenderers ∷ Lens' LogRenderOpts [(LogEntry → Doc ())→ LogEntry → Doc ()]
lroRenderers = lens _lroRenderers (\ opts rs → opts { _lroRenderers = rs })

lroRenderer ∷ LogRenderOpts → LogEntry → Doc ()
lroRenderer opts = let foldf ∷ Foldable ψ ⇒ ψ (α → α) → α → α
                       foldf = flip (foldr ($))
                    in foldf (opts ⊣ lroRenderers) (view LogEntry.doc)

lroRendererTests ∷ TestTree
lroRendererTests =
  let check nme exp rs = let opts     = LogRenderOpts rs Unbounded
                             rendered = lroRenderer opts _le0
                          in testCase nme $ exp ≟ renderDoc rendered
      checks nme exp rs = let opts     = LogRenderOpts rs Unbounded
                              rendered = lroRenderer opts _le0
                           in assertListEq nme exp (T.lines $renderDoc rendered)
   in testGroup "lroRenderer"
                [ check "plain" "log_entry 1" []
                , check "sev" "[Info] log_entry 1" [renderWithSeverity]
                , check "ts" "[1970-01-01Z00:00:00 Thu] log_entry 1"
                             [renderWithTimestamp]
                , check "ts" "«c#1» log_entry 1" [renderWithStackHead]
                , checks "cs" [ "log_entry 1"
                              , "  stack0, called at c:1:2 in a:b"
                              , "    stack1, called at f:5:6 in d:e"
                              ]
                             [renderWithCallStack]
                , check "ts-sev"
                    "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                        [renderWithTimestamp,renderWithSeverity]
                , check "sev-ts"
                    "[Info] [1970-01-01Z00:00:00 Thu] log_entry 1"
                        [renderWithSeverity,renderWithTimestamp]
                , checks "ts-sev-cs"
                         [ "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                         ,   "                                 "
                           ⊕ "  stack0, called at c:1:2 in a:b"
                         ,   "                                 "
                           ⊕ "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithTimestamp, renderWithSeverity
                         , renderWithCallStack ]
                , checks "cs-ts-sev"
                         [ "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                         , "  stack0, called at c:1:2 in a:b"
                         , "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithCallStack
                         , renderWithTimestamp, renderWithSeverity ]
                , checks "sev-cs-ts"
                         [ "[Info] [1970-01-01Z00:00:00 Thu] log_entry 1"
                         , "         stack0, called at c:1:2 in a:b"
                         , "           stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithSeverity, renderWithCallStack
                         , renderWithTimestamp ]
                , checks "cs-sevts"
                         [ "[1970-01-01Z00:00:00 Thu|Info] log_entry 1"
                         , "  stack0, called at c:1:2 in a:b"
                         , "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithCallStack, renderWithSeverityAndTimestamp ]
                , checks "sh-sevts"
                  [ "[1970-01-01Z00:00:00 Thu|Info] «c#1» log_entry 1"
                         ]
                         [ renderWithSeverityAndTimestamp, renderWithStackHead ]
                ]

lroWidth ∷ Lens' LogRenderOpts PageWidth
lroWidth = lens _lroWidth (\ opts w → opts { _lroWidth = w })

lroOpts ∷ Lens' LogRenderOpts LayoutOptions
lroOpts = lens (LayoutOptions ∘ _lroWidth) (\ opts lo → opts { _lroWidth = layoutPageWidth lo })

instance Default LogRenderOpts where
  def = LogRenderOpts [] Unbounded

{- | Render logs to stderr. -}
logsToStderr ∷ MonadIO μ ⇒ LogRenderOpts → PureLoggingT Log μ α → μ α
logsToStderr opts io = do
  (x,ts) ← logRender opts io
  mapM_ say ts
  return x

logRender ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η α → η (α, DList Text)
logRender opts a = do
  let renderer = lroRenderer opts
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderStrict ∘ layoutPretty (opts ⊣ lroOpts) ∘ renderer ⊳ unLog ls

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η () → η (DList Text)
logRender' = fmap snd ⩺ logRender

{- | Render logs to text, including severity. -}
renderLogs ∷ Monad η ⇒ PureLoggingT Log η α → η (α, DList Text)
renderLogs a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity (renderWithStackHead (view LogEntry.doc)) ⊳ unLog ls

renderLogsSt ∷ Monad η ⇒ PureLoggingT Log η α → η (α, DList Text)
renderLogsSt a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity (renderWithCallStack (view LogEntry.doc)) ⊳ unLog ls

{- | Performing renderLogs, with IO returning () is sufficiently common to
     warrant a cheap alias. -}
renderLogs' ∷ Monad η ⇒ PureLoggingT Log η () → η (DList Text)
renderLogs' = snd ⩺ renderLogs

newtype Log = Log { unLog ∷ DList LogEntry }
  deriving (Monoid,Semigroup,Show)

instance Printable Log where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog

assertEq' ∷ (Eq t, HasCallStack) ⇒ (t → Text) → t → t → Assertion
assertEq' toT expected got =
  let toS = toString ∘ toT
   in -- equalize prefix lengths to make it easier to diff strings, etc.
       assertBool ("expected: " ⊕ toS expected ⊕ "\nbut got : " ⊕ toS got)
                  (got ≡ expected)

{- | Compare two lists for equality, with itemized testing.  We take the inputs
     as IO to allow for, well, IO.
 -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ, HasCallStack) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → TestTree
assertListEqIO' toT name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) =
        testCase (show i) (got ≫ \ g → assertEq' toT' (Just e) (atMay g i))
      toT' Nothing  = "Nothing"
      toT' (Just a) = "Just " ⊕ toT a

   in testGroup (toString name) $
          testCase "count" (got ≫ lengthCheck expect)
        : (assertItem ⊳ zip [0..] expect)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α, HasCallStack) ⇒
                Text → ψ α → IO (φ α) → TestTree
assertListEqIO = assertListEqIO' toText

-- | compare two lists for equality, with itemized testing
assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ, HasCallStack) ⇒
               Text → ψ α → φ α → TestTree
assertListEq name exp got = assertListEqIO name exp (return got)

renderWithTimestamp_  ∷ HasUTCTimeY τ ⇒ τ → Doc ρ
renderWithTimestamp_ m = pretty (formatUTCYDoW $ m ⊣ utcTimeY)

renderWithTimestamp ∷ HasUTCTimeY τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithTimestamp f m = brackets (renderWithTimestamp_ m) ⊞ align (f m)

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
                           , logMsgTests, renderTests, lroRendererTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
