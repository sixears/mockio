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

-- logging-effect ----------------------

import qualified  Control.Monad.Log
import Control.Monad.Log  ( MonadLog, PureLoggingT, Severity(..)
                          , WithSeverity( WithSeverity )
                          , defaultBatchingOptions, logMessage, timestamp
                          , renderWithSeverity, runLoggingT, runPureLoggingT
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

-- printer -----------------------------

import qualified  Text.Printer  as  P

-- safe --------------------------------

import Safe  ( atMay, headMay )

-- streaming ---------------------------

import Streaming.Prelude  ( Of, Stream )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, HUnitFailure( HUnitFailure ), (@=?), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree, withResource' )

-- text --------------------------------

import qualified  Data.Text       as  T
import qualified  Data.Text.Lazy  as  LT

import Data.Text     ( Text, intercalate, pack, take, unlines, unpack )
import Data.Text.IO  ( readFile )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

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

-- TODO: use logMessage in logIO; logIO'→logIO; printable Log{,Entry}
-- add logIO' to carry arbitrary data; same for log & log'; elide WithCallStack
-- add simple logging (without io); different brackets for severity & timestamp
-- tighten up naming; split out mocking & logging; terminal colouring
-- split out stacktrace tests; demo logging methods incl. stderr
-- clearer mock logging (e.g. (CMD) vs [CMD]); options handlers for logs
-- cmd logging using showcmdforuser

-- Placed at the top to reduce line movements messing up the tests
logIO ∷ (MonadIO μ, ?stack ∷ CallStack) ⇒ Severity → Text → μ LogEntry
logIO sv txt = liftIO getCurrentTime ≫ \ tm → return $ withCallStack (pretty txt,tm,sv)

logIO_ ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒
         Severity → Doc () → μ ()
logIO_ sv doc = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ withCallStack (doc,tm,sv)

-- We redefine this, rather than simply calling logIO_, so we don't mess with
-- the callstack.
logIO' ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒
         Severity → Text → μ ()
logIO' sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ withCallStack (pretty txt,tm,sv)

-- test data

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
_le0 = LogEntry _cs2 _tm Informational (pretty ("log_entry 1" ∷ Text))

_le1 ∷ LogEntry
_le1 =
  LogEntry _cs1 _tm Critical (pretty ("multi-line\nlog\nmessage" ∷ Text))

_le2 ∷ LogEntry
_le2 =
  let valign = align ∘ vsep
   in LogEntry _cs1 _tm Warning ("this is" ⊞ valign [ "a"
                                                    , "vertically"
                                                    ⊞ valign [ "aligned"
                                                             , "message"
                                                             ]
                                                    ])
_le3 ∷ LogEntry
_le3 = 
  LogEntry _cs1 _tm Emergency (pretty ("this is the last message" ∷ Text))

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
      exp3 = [ "[Thu 1970-01-01Z00:00:00] [Info] «c#1» log_entry 1"
             , intercalate "\n" [   "[Thu 1970-01-01Z00:00:00] [CRIT] «y#9» "
                                  ⊕ "multi-line"
                                ,   "                                       "
                                  ⊕ "log"
                                ,   "                                       "
                                  ⊕ "message"
                                ]                   
             , intercalate "\n"
                           [ "[Thu 1970-01-01Z00:00:00] [Warn] «y#9» this is a"
                           ,   "                                               "
                             ⊕ "vertically aligned"
                           ,   "                                               "
                             ⊕ "           message"
                           ]                   
             , "[Thu 1970-01-01Z00:00:00] [EMRG] «y#9» this is the last message"
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

-- instance Semigroup (WithSeverity (SimpleLogEntry)) where
-- instance Monoid (WithSeverity (SimpleLogEntry)) where

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

logit ∷ IO ()
logit = (\io -> withFDHandler defaultBatchingOptions stderr 0.01 5 (\lg -> runLoggingT io (lg ∘ renderWithSeverity id))) (Control.Monad.Log.logInfo "mary had a little lamb")

logit' ∷ (MonadIO μ, MonadMask μ) ⇒ μ ()
logit' = (\io->withFDHandler defaultBatchingOptions stderr 0.01 5 (\lg->runLoggingT io (lg ∘ renderLog))) (logIO Informational "log this" >>= logMessage )
logit'' ∷ (MonadIO μ, MonadMask μ) ⇒ μ ()
logit'' = (\io->withFDHandler defaultBatchingOptions stderr 0.01 5 (\lg->runLoggingT io (lg ∘ renderLog'))) (logIO Informational "log this" >>= logMessage )
-- [Sat 2020-04-04Z06:37:49] [Info ] log this
--                                    logIO, called at <interactive>:375:99 in interactive:Ghci10


-- logit' ∷ IO ()
-- logit' = (\io -> withFDHandler defaultBatchingOptions stderr 0.01 5 (\lg -> runLoggingT io (lg ∘ renderWithTimestamp (formatTime defaultTimeLocale rfc822DateFormat) ∘ renderWithSeverity id))) (timestamp $ Control.Monad.Log.logInfo "mary had a little lamb")

{-

import qualified Data.Text.Lazy as LT
import qualified Data.Text.Prettyprint.Doc as PP
import qualified Data.Text.Prettyprint.Doc.Render.Text as PP

-- | Given a way to render the underlying message @a@, render a message with its
-- severity.
--
-- >>> renderWithSeverity id (WithSeverity Informational "Flux capacitor is functional")
-- [Informational] Flux capacitor is functional
renderWithSeverity
  :: (a -> PP.Doc ann) -> (WithSeverity a -> PP.Doc ann)
renderWithSeverity k (WithSeverity u a) =
  PP.brackets (PP.pretty u) PP.<+> PP.align (k a)

-- >>> renderWithTimestamp (formatTime defaultTimeLocale rfc822DateFormat) id timestamppedLogMessage
-- [Tue, 19 Jan 2016 11:29:42 UTC] Setting target speed to plaid
renderWithTimestamp :: (UTCTime -> String)
                       -- ^ How to format the timestamp.
                    -> (a -> PP.Doc ann)
                       -- ^ How to render the rest of the message.
                    -> (WithTimestamp a -> PP.Doc ann)
renderWithTimestamp formatter k (WithTimestamp a t) =
  PP.brackets (PP.pretty (LT.pack (formatter t))) PP.<+> PP.align (k a)

-- | Given a way to render the underlying message @a@ render a message with a
-- callstack.
--
-- The callstack will be pretty-printed underneath the log message itself.
renderWithCallStack :: (a -> PP.Doc ann) -> WithCallStack a -> PP.Doc ann
renderWithCallStack k (WithCallStack stack msg) =
  k msg <> PP.line <> PP.indent 2 (prettyCallStack (getCallStack stack))


-- | Construct a 'WithCallStack' log message.
--
-- This should normally be preferred over just using 'WithCallStack' as it will
-- append a new entry to the stack - pointing to this exact log line. However,
-- if you are creating a combinator (such as a wrapper that logs and throws
-- an exception), you may be better manually capturing the 'CallStack' and
-- using 'WithCallStack'.
withCallStack :: (?stack :: CallStack) => a -> WithCallStack a
withCallStack = WithCallStack ?stack
-}

{- | Note the awkward capitalization, to avoid clashing with
     `GHC.Stack.HasCallStack` -}
class HasCallstack α where
  callStack' ∷ Lens' α CallStack

instance HasCallstack CallStack where
  callStack' = id

stackHead ∷ HasCallstack α ⇒ α → Maybe (String,SrcLoc)
stackHead = headMay ∘ getCallStack ∘ view callStack'

class WithCallStack ω where
  type CSElement ω
  withCallStack ∷ (?stack ∷ CallStack, HasCallStack) ⇒ CSElement ω → ω
  csDiscard ∷ ω -> CSElement ω
  _callStack_ ∷ ω → CallStack

instance WithCallStack CallStack where
  type CSElement CallStack = ()
  withCallStack () = ?stack
  csDiscard _ = ()
  _callStack_ w = w

instance WithCallStack (Control.Monad.Log.WithCallStack α) where
  type CSElement (Control.Monad.Log.WithCallStack α) = α
  withCallStack = Control.Monad.Log.withCallStack
  csDiscard w = Control.Monad.Log.discardCallStack w
  _callStack_ w = Control.Monad.Log.msgCallStack w

instance WithCallStack (CallStack, α) where
  type CSElement (CallStack, α) = α
  withCallStack = (?stack,)
  csDiscard = snd
  _callStack_ = fst

instance WithCallStack (CallStack, α, β) where
  type CSElement (CallStack,α,β) = (α,β)
  withCallStack (a,b) = (?stack,a,b)
  csDiscard (_,a,b)   = (a,b)
  _callStack_ (cs,_,_)  = cs

instance WithCallStack (CallStack, α, β, γ) where
  type CSElement (CallStack,α,β,γ) = (α,β,γ)
  withCallStack (a,b,c) = (?stack,a,b,c)
  csDiscard (_,a,b,c)   = (a,b,c)
  _callStack_ (cs,_,_,_)  = cs

prettyCallStack ∷ [(String,SrcLoc)] → Doc ann
prettyCallStack [] = "empty callstack"
prettyCallStack (root:rest) =
  prettyCallSite root ⊕ line ⊕ indent 2 (vsep (prettyCallSite ⊳ rest))
  where prettyCallSite (f,loc) =
          pretty (LT.pack f) ⊕ ", called at " ⊕
          pretty (LT.pack (GHC.Stack.prettySrcLoc loc))

renderWithCallStack ∷ HasCallstack δ ⇒ (δ -> Doc ρ) -> δ -> Doc ρ
renderWithCallStack f m =
  f m ⊕ line ⊕ indent 2 (prettyCallStack (getCallStack $ m ⊣ callStack'))

renderWithStackHead ∷ HasCallstack δ ⇒ (δ -> Doc ρ) -> δ -> Doc ρ
renderWithStackHead f m =
  let renderStackHead = renderLocation ∘ fmap snd
   in renderStackHead (stackHead m) ⊞ align (f m)

locToString ∷ SrcLoc → String
locToString loc = "«" ⊕ srcLocFile loc ⊕ "#" ⊕ show (srcLocStartLine loc) ⊕ "»"

renderLocation ∷ Maybe SrcLoc → Doc α
renderLocation (Just loc) = pretty $ locToString loc
renderLocation Nothing    = emptyDoc

stackHeadTxt ∷ HasCallstack α ⇒ α → Text
stackHeadTxt a = case locToString ⩺ fmap snd $ stackHead a of
                   Just s  → pack s 
                   Nothing → ""

{-
λ> :t renderWithSeverity' (Main.renderWithCallStack pretty)
renderWithSeverity' (Main.renderWithCallStack pretty)
  :: forall {a} {ann}.
     (HasSeverity a, Main.WithCallStack a, Pretty a) =>
     a -> Doc ann


λ> :t renderWithSeverity (Main.renderWithCallStack id)
renderWithSeverity (Main.renderWithCallStack id)
  :: forall {ann}.
     Main.WithCallStack (Doc ann) =>
     WithSeverity (Doc ann) -> Doc ann

-}

class HasUTCTime α where
  utcTime ∷ Lens' α UTCTime

instance HasUTCTime UTCTime where
  utcTime = id

class HasSeverity α where
  severity ∷ Lens' α Severity

instance HasSeverity Severity where
  severity = id

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

renderWithSeverity' ∷ HasSeverity τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithSeverity' f m =
  let pp ∷ HasSeverity α ⇒ α → Doc ann
      pp sv = pretty $ case sv ⊣ severity of
                         Emergency     → ("EMRG" ∷ Text)
                         Alert         → "ALRT"
                         Critical      → "CRIT"
                         Warning       → "Warn"
                         Notice        → "Note"
                         Informational → "Info"
                         Debug         → "Debg"
   in brackets (pp m) ⊞ align (f m)


{- | Log with timestamp, callstack, severity & IOClass -}
data LogEntry = LogEntry { _callstack ∷ CallStack
                         , _timestamp ∷ UTCTime
                         , _severity  ∷ Severity
                         , _logdoc    ∷ Doc ()
                         }
  deriving Show

logdoc ∷ Lens' LogEntry (Doc ())
logdoc = lens _logdoc (\ le txt → le { _logdoc = txt })

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
--                , _lroType       ∷ LogRenderType
                }

{- | `LogRenderOpts` with no adornments.  Page width is `Unbounded`; you can
     override this with lens syntax, e.g.,

     > lroRenderPlain & lroWidth .~ AvailablePerLine 80 1.0
 -}
lroRenderPlain ∷ LogRenderOpts
lroRenderPlain = LogRenderOpts [] Unbounded -- LRO_Plain

{- | `LogRenderOpts` with timestamp & severity.
 -}
lroRenderTSSev ∷ LogRenderOpts
lroRenderTSSev = LogRenderOpts [ renderWithTimestamp, renderWithSeverity' ] Unbounded -- LRO_Plain

{- | `LogRenderOpts` with severity & callstack.
 -}
lroRenderSevCS ∷ LogRenderOpts
lroRenderSevCS = LogRenderOpts [ renderWithCallStack, renderWithSeverity' ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack.
 -}
lroRenderTSSevCS ∷ LogRenderOpts
lroRenderTSSevCS = LogRenderOpts [ renderWithCallStack, renderWithTimestamp, renderWithSeverity' ] Unbounded -- LRO_Plain

{- | `LogRenderOpts` with severity & callstack head.
 -}
lroRenderSevCSH ∷ LogRenderOpts
lroRenderSevCSH = LogRenderOpts [ renderWithSeverity', renderWithStackHead ] Unbounded -- LRO_Plain

{- | `LogRenderOpts` with timestamp, severity & callstack head.
 -}
lroRenderTSSevCSH ∷ LogRenderOpts
lroRenderTSSevCSH = LogRenderOpts [ renderWithTimestamp, renderWithSeverity', renderWithStackHead ] Unbounded -- LRO_Plain

-- lroType ∷ Lens' LogRenderOpts LogRenderType
-- lroType = lens _lroType (\ opts typ → opts { _lroType = typ })
lroRenderers ∷ Lens' LogRenderOpts [(LogEntry → Doc ())→ LogEntry → Doc ()]
lroRenderers = lens _lroRenderers (\ opts rs → opts { _lroRenderers = rs })

lroRenderer ∷ LogRenderOpts → LogEntry → Doc ()
lroRenderer opts = let foldf ∷ Foldable ψ ⇒ ψ (α → α) → α → α
                       foldf = flip (foldr ($))
                    in foldf (opts ⊣ lroRenderers) (view logdoc)

lroRendererTests ∷ TestTree
lroRendererTests =
  let check nme exp rs = let opts     = LogRenderOpts rs Unbounded -- LRO_Stack
                             rendered = lroRenderer opts _le0
                          in testCase nme $ exp ≟ renderDoc rendered
      checks nme exp rs = let opts     = LogRenderOpts rs Unbounded -- LRO_Stack
                              rendered = lroRenderer opts _le0
                           in assertListEq nme exp (T.lines $renderDoc rendered)
   in testGroup "lroRenderer"
                [ check "plain" "log_entry 1" []
                , check "sev" "[Info] log_entry 1" [renderWithSeverity']
                , check "ts" "[Thu 1970-01-01Z00:00:00] log_entry 1"
                             [renderWithTimestamp]
                , check "ts" "«c#1» log_entry 1" [renderWithStackHead]
                , checks "cs" [ "log_entry 1"
                              , "  stack0, called at c:1:2 in a:b"
                              , "    stack1, called at f:5:6 in d:e"
                              ]
                             [renderWithCallStack]
                , check "ts-sev"
                    "[Thu 1970-01-01Z00:00:00] [Info] log_entry 1"
                        [renderWithTimestamp,renderWithSeverity']
                , check "sev-ts"
                    "[Info] [Thu 1970-01-01Z00:00:00] log_entry 1"
                        [renderWithSeverity',renderWithTimestamp]
                , checks "ts-sev-cs"
                         [ "[Thu 1970-01-01Z00:00:00] [Info] log_entry 1"
                         ,   "                                 "
                           ⊕ "  stack0, called at c:1:2 in a:b"
                         ,   "                                 "
                           ⊕ "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithTimestamp, renderWithSeverity'
                         , renderWithCallStack ]
                , checks "cs-ts-sev"
                         [ "[Thu 1970-01-01Z00:00:00] [Info] log_entry 1"
                         , "  stack0, called at c:1:2 in a:b"
                         , "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithCallStack
                         , renderWithTimestamp, renderWithSeverity' ]
                , checks "sev-cs-ts"
                         [ "[Info] [Thu 1970-01-01Z00:00:00] log_entry 1"
                         , "         stack0, called at c:1:2 in a:b"
                         , "           stack1, called at f:5:6 in d:e"
                         ]
                         [ renderWithSeverity', renderWithCallStack
                         , renderWithTimestamp ]
                ]

lroWidth ∷ Lens' LogRenderOpts PageWidth
lroWidth = lens _lroWidth (\ opts w → opts { _lroWidth = w })

lroOpts ∷ Lens' LogRenderOpts LayoutOptions
lroOpts = lens (LayoutOptions ∘ _lroWidth) (\ opts lo → opts { _lroWidth = layoutPageWidth lo })

instance Default LogRenderOpts where
  def = LogRenderOpts [] Unbounded -- LRO_TimeStamp

{- | Render logs to stderr. -}
logsToStderr ∷ MonadIO μ ⇒ LogRenderOpts → PureLoggingT Log μ α → μ α
logsToStderr opts io = do
  (x,ts) ← logRender opts io
  mapM_ say ts
  return x

logRender ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η α → η (α, DList Text)
logRender opts a = do
  let -- render ∷ [Doc () → Doc ()] → LogEntry → Doc ()
      -- render = foldf (view logdoc) (opts ⊣ lroRenderers)
      renderer = {- case opts ⊣ lroType of
                   LRO_Plain → view logdoc
                   _         → renderWithSeverity' (renderWithStackHead (view logdoc))
                 -}
                 lroRenderer opts
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderStrict ∘ layoutPretty (opts ⊣ lroOpts) ∘ renderer ⊳ unLog ls

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η () → η (DList Text)
logRender' = fmap snd ⩺ logRender

{- | Render logs to text, including severity. -}
renderLogs ∷ Monad η ⇒ PureLoggingT Log η α → η (α, DList Text)
renderLogs a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity' (renderWithStackHead (view logdoc)) ⊳ unLog ls

renderLogsSt ∷ Monad η ⇒ PureLoggingT Log η α → η (α, DList Text)
renderLogsSt a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity' (renderWithStackHead (view logdoc)) ⊳ unLog ls

{- | Performing renderLogs, with IO returning () is sufficiently common to
     warrant a cheap alias. -}
renderLogs' ∷ Monad η ⇒ PureLoggingT Log η () → η (DList Text)
renderLogs' = snd ⩺ renderLogs

{- | Render an instance of a `Pretty` type to text, with default options. -}
renderText ∷ Pretty α ⇒ α → Text
renderText = renderDoc ∘ pretty

instance Printable LogEntry where
  print le =
    P.text $ [fmt|[%t|%-4t] %t %t|] (formatUTCDoW $ le ⊣ utcTime) (take 4 ∘ pack ∘ show $ le ⊣ severity) (stackHeadTxt le) (renderDoc $ le ⊣ logdoc)

newtype Log = Log { unLog ∷ DList LogEntry }
  deriving (Monoid,Semigroup,Show)

-- instance Pretty Log where
--   pretty (Log logs) = vsep (pretty ⊳ toList logs)

instance Printable Log where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog

instance WithCallStack LogEntry where
  type CSElement LogEntry = (Doc(),UTCTime,Severity)
  withCallStack (txt,tm,sv) = LogEntry (popCallStack ?stack) tm sv txt
  {-# INLINE withCallStack #-}
  csDiscard (LogEntry _ tm sv txt)   = (txt,tm,sv)
  _callStack_ (LogEntry cs _ _ _ )  = cs

instance HasCallstack LogEntry where
  callStack' = lens _callstack (\ le cs → le { _callstack = cs })

instance HasSeverity LogEntry where
  severity = lens _severity (\ le sv → le { _severity = sv })

instance HasUTCTime LogEntry where
  utcTime = lens _timestamp (\ le tm → le { _timestamp = tm })

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

renderLogWithoutTimeStamp ∷ LogEntry → Doc ()
renderLogWithoutTimeStamp = renderWithSeverity' $ renderWithCallStack (view logdoc)

renderLog ∷ LogEntry → Doc ()
renderLog = renderWithTimestamp ∘ renderWithSeverity' $ renderWithCallStack (view logdoc)

renderLog' ∷ LogEntry → Doc ()
renderLog' = renderWithTimestamp ∘ renderWithSeverity' $ renderWithStackHead (view logdoc)

{-
renderWithTimestamp ∷ (UTCTime → String)
                       -- ^ How to format the timestamp.
                    → (a → PP.Doc ann)
                       -- ^ How to render the rest of the message.
                    → (WithTimestamp a → PP.Doc ann)
-}
-- Add this to tfmt?
{- | Format a UTCTime, in almost-ISO8601-without-fractional-seconds (always in Zulu). -}
formatUTC ∷ UTCTime → Text
formatUTC = pack ∘ formatTime defaultTimeLocale "%FZ%T"

{- | Format a UTCTime, in ISO8601-without-fractional-seconds (always in Zulu),
     with a leading 3-letter day-of-week -}
formatUTCDoW ∷ UTCTime → Text
formatUTCDoW = pack ∘ formatTime defaultTimeLocale "%a %FZ%T"


renderWithTimestamp f m =
  brackets (pretty (formatUTCDoW $ m ⊣ utcTime)) ⊞ align (f m)

withResource2 ∷ IO α → (α → IO()) → IO β → (β → IO ()) → (IO α → IO β →TestTree)
              → TestTree
withResource2 gain lose gain' lose' ts =
  withResource gain lose (\ x → withResource gain' lose' (\ x' → ts x x'))

withResource2' ∷ IO α → IO β → (IO α → IO β → TestTree)
              → TestTree
withResource2' gain gain' ts =
  withResource' gain (\ x → withResource' gain' (\ x' → ts x x'))

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
                              , testCase "log" $ do (_,log) ← txtlog
                                                    helloEntry @=? log
                              ]
                , withResource' (runWriterT $ readFn "/etc/subgid" DoMock) $
                    \ txtlog →
                    testGroup "DoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    "mockety" ≟ txt
                              , testCase "log" $ do (_,log) ← txtlog
                                                    helloEntry @=? log
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
                              , testCase "log" $ do (_,log) ← txtlog
                                                    helloEntry @=? log
                              ]
                , withResource' (readFn' "/etc/subgid" DoMock) $ \ txtlog →
                    testGroup "DoMock"
                              [ testCase "txt" $ do (txt,_) ← txtlog
                                                    "mockety" ≟ txt
                              , testCase "log" $ do (_,log) ← txtlog
                                                    helloEntry @=? log
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

{-
logMsg ∷ MonadLog (DList (WithSeverity SimpleLogEntry)) η ⇒
         Severity → IOClass → Text → η ()
logMsg sv clss msg = logMessage $ [WithSeverity sv (SimpleLogEntry(clss,msg))]

logInfo ∷ MonadLog (DList (WithSeverity SimpleLogEntry)) η ⇒
          IOClass → Text → η ()
logInfo = logMsg Informational
-}

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
                  , testCase "log" $ do (_,log) ← txtlog
                                        helloEnt @=? log
                  ]

data WithAttr β α = WithAttr { attr ∷ β, datum ∷ α }
  deriving (Eq,Functor,Show)

testApp ∷ MonadLog (WithSeverity (Doc ann)) m => m ()
testApp = do
  logMessage (WithSeverity Informational "Don't mind me")
  logMessage (WithSeverity Error "But do mind me!")

ю̄ ∷ Monoid α ⇒ [α] → α
ю̄ = ю

sdoc ∷ SimpleDocStream IOClass
sdoc = layoutPretty defaultLayoutOptions (ю̄ [ "begin"
                                            , line
                                            , hsep [ annotate IORead "ioread"
                                                   , annotate IOWrite "iowrite"
                                                   , annotate IOCmdR "iocmdr"
                                                   ]
                                            , line
                                            , "end"
                                            ])

sdoc' = ю̄ [ "begin"
          , line
          , hsep [ annotate IORead "ioread"
                 , annotate IOCmdR "iocmdr"
                 , annotate IOWrite "iowrite"
                 ]
          , line
          , "end"
          ]

sdoc_none ∷ SimpleDocStream IOClass
sdoc_none = layoutPretty defaultLayoutOptions
                         (ю̄ [ "begin"
                            , line
                            , hsep [ annotate IORead ф
                                   , annotate IOWrite ф
                                   , annotate IOCmdR ф
                                   ]
                            , line
                            , "end"
                            ])


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
