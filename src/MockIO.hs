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

import qualified  GHC.Stack

import Data.Bool            ( Bool( False, True ), not, otherwise )
import Control.Applicative  ( Applicative, (<*>), pure )
import Control.Monad        ( Monad, (>>=), mapM, return )
import Control.Monad.Catch  ( MonadMask )
import Control.Monad.Identity  ( runIdentity )
import Data.Eq              ( Eq )
import Data.Foldable        ( Foldable, toList )
import Data.Function        ( ($), (&), const, id )
import Data.Functor         ( Functor, fmap )
import Data.List            ( zip )
import Data.Maybe           ( Maybe( Just, Nothing ) )
import Data.Monoid          ( Monoid( mappend, mconcat, mempty ) )
import Data.Semigroup       ( Semigroup( (<>), sconcat, stimes ) )
import Data.String          ( String, lines )
import Data.Tuple           ( fst, snd )
import GHC.Exts             ( fromList )
import GHC.Stack            ( CallStack, HasCallStack, SrcLoc, getCallStack
                            , popCallStack, srcLocFile, srcLocStartLine )
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

import MonadIO  ( MonadIO, liftIO )

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

import Data.Text.Prettyprint.Doc  ( Doc, PageWidth( Unbounded ), Pretty
                                  , SimpleDocStream(..)
                                  , (<+>)
                                  , align, annotate, brackets
                                  , defaultLayoutOptions, emptyDoc, enclose
                                  , hsep, indent, layoutPretty, line, pretty
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

import Test.Tasty.HUnit  ( Assertion, (@=?), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree, withResource' )

-- text --------------------------------

import qualified  Data.Text       as  T
import qualified  Data.Text.Lazy  as  LT

import Data.Text     ( Text, intercalate, pack, take, unlines )
import Data.Text.IO  ( readFile )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- time --------------------------------

import Data.Time.Clock   ( UTCTime, getCurrentTime )
import Data.Time.Format  ( defaultTimeLocale, formatTime, rfc822DateFormat )

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
-- cmd logging using showcmdforuser; add example of mapped logs for payload

-- Placed at the top to reduce line movements messing up the tests
logIO ∷ (MonadIO μ, ?stack ∷ CallStack) ⇒ Severity → Text → μ LogEntry
logIO sv txt = liftIO getCurrentTime ≫ \ tm → return $ withCallStack (txt,tm,sv,Null)

bob ∷ (?stack ∷ CallStack) ⇒ IO LogEntry
bob = let -- stack = GHC.Stack.callStack
           -- add an additional callstack to test the formatting
           mybob ∷ (?stack ∷ CallStack) ⇒ IO LogEntry
--           mybob ∷ HasCallStack ⇒ IO LogEntry
           mybob = logIO Informational "bob"
        in mybob -- lg Informational "bob"

bob' ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒ μ ()
bob' = let -- stack = GHC.Stack.callStack
           -- add an additional callstack to test the formatting
--           mybob ∷ (?stack ∷ CallStack) ⇒ IO LogEntry
           mybob' ∷ (MonadIO μ, MonadLog Log μ, HasCallStack) ⇒ μ ()
           mybob' = do logIO' Informational "bob'"
                       logIO' Notice "jimmy"
        in mybob' -- lg Informational "bob"

renderTests ∷ TestTree
renderTests =
  testGroup "render" $
        ю [ assertListEqIO "render1" exp1 (lines ∘ show ∘ renderWithSeverity' (renderWithCallStack pretty) ⊳ bob)
          , let ?stack = c
             in assertListEqIO "render2" exp2 (lines ∘ show ∘ renderWithSeverity' (renderWithCallStack pretty) ⊳ bob)
          , assertListEqIO "renderLogs" exp4 (renderLogs' bob')
          ]
  where c = GHC.Stack.fromCallSiteList [("foo",GHC.Stack.SrcLoc "a" "b" "c" 1 2 3 4)]
        exp1 = [ "[Info] bob"
               , "         logIO, called at src/MockIO.hs:186:20 in main:Main"
               , "           mybob, called at src/MockIO.hs:187:12 in main:Main"
               , "           bob, called at src/MockIO.hs:201:110 in main:Main"
               ]
        exp2 = [ "[Info] bob"
               , "         logIO, called at src/MockIO.hs:186:20 in main:Main"
               , "           mybob, called at src/MockIO.hs:187:12 in main:Main"
               , "           bob, called at src/MockIO.hs:203:114 in main:Main"
               , "           foo, called at c:1:2 in a:b"
               ]
        exp4 = [ "[Info] «src/MockIO.hs#194» bob'"
               , "[Note] «src/MockIO.hs#195» jimmy"
               ]

logIO' ∷ (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack) ⇒
         Severity → Text → μ ()
logIO' sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log' ∘ singleton $ withCallStack (txt,tm,sv,Null)

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
data LogEntry' α = LogEntry' { _callstack ∷ CallStack
                             , _timestamp ∷ UTCTime
                             , _severity  ∷ Severity
                             , _logtxt    ∷ Text
                             , _payload   ∷ α
                             }
  deriving (Functor,Show)

data Null = Null

instance Semigroup Null where
  _ <> _     = Null
  sconcat _  = Null
  stimes _ _ = Null

instance Monoid Null where
  mempty      = Null
  mappend _ _ = Null
  mconcat _   = Null

instance Printable Null where
  print _ = P.text ""

type LogEntry = LogEntry' Null

logtxt ∷ Lens' (LogEntry' α) Text
logtxt = lens _logtxt (\ le txt → le { _logtxt = txt })

payload ∷ Lens' (LogEntry' α) α
payload = lens _payload (\ le p → le { _payload = p })

-- if you want to include the payload in the log message, use an fmap on the
-- logentries to format the payload with the text

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
data LogRenderOpts = LogRenderOpts { _lroType  ∷ LogRenderType
                                   , _lroWidth ∷ PageWidth
                                   }

instance Default LogRenderOpts where
  def = LogRenderOpts LRO_TimeStamp Unbounded

{- | Render logs to text, including severity. -}
renderLogs ∷ Monad η ⇒ PureLoggingT (Log' σ) η α → η (α, DList Text)
renderLogs a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity' (renderWithStackHead pretty) ⊳ unLog' ls

renderLogsSt ∷ Monad η ⇒ PureLoggingT (Log' σ) η α → η (α, DList Text)
renderLogsSt a = do
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderDoc ∘ renderWithSeverity' (renderWithStackHead pretty) ⊳ unLog' ls

{- | Performing renderLogs, with IO returning () is sufficiently common to
     warrant a cheap alias. -}
renderLogs' ∷ Monad η ⇒ PureLoggingT (Log' σ) η () → η (DList Text)
renderLogs' = snd ⩺ renderLogs

{- | Render an instance of a `Pretty` type to text, with default options. -}
renderText ∷ Pretty α ⇒ α → Text
renderText = renderDoc ∘ pretty

instance Pretty (LogEntry' α) where
  pretty (LogEntry' _ _ _ txt _) = pretty txt

instance Printable α ⇒ Printable (LogEntry' α) where
  print le =
    let pload = case (toText $ le ⊣ payload) of
                  "" → ""
                  t  → " <" ⊕ t ⊕ ">"
     in P.text $ [fmt|[%t|%-4t] %t %T%t|] (formatUTCDoW $ le ⊣ utcTime) (take 4 ∘ pack ∘ show $ le ⊣ severity) (stackHeadTxt le) (le ⊣ logtxt) pload

newtype Log' α = Log' { unLog' ∷ DList (LogEntry' α) }
  deriving (Functor,Monoid,Semigroup,Show)
type Log = Log' Null

instance Pretty (Log' α) where
  pretty (Log' logs) = vsep (pretty ⊳ toList logs)

instance Printable α ⇒ Printable (Log' α) where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog'

instance WithCallStack (LogEntry' β) where
  type CSElement (LogEntry' β) = (Text,UTCTime,Severity,β)
  withCallStack (txt,tm,sv,b) = LogEntry' (popCallStack ?stack) tm sv txt b
  {-# INLINE withCallStack #-}
  csDiscard (LogEntry' _ tm sv txt b)   = (txt,tm,sv,b)
  _callStack_ (LogEntry' cs _ _ _ _)  = cs

instance HasCallstack (LogEntry' β) where
  callStack' = lens _callstack (\ le cs → le { _callstack = cs })

instance HasSeverity (LogEntry' α) where
  severity = lens _severity (\ le sv → le { _severity = sv })

instance HasUTCTime (LogEntry' α) where
  utcTime = lens _timestamp (\ le tm → le { _timestamp = tm })

assertEq' ∷ (Eq t) ⇒ (t → Text) → t → t → Assertion
assertEq' toT expected got =
  let toS = toString ∘ toT
   in -- equalize prefix lengths to make it easier to diff strings, etc.
       assertBool ("expected: " ⊕ toS expected ⊕ "\nbut got : " ⊕ toS got)
                  (got ≡ expected)

{- | Compare two lists for equality, with itemized testing.  We take the inputs
     as IO to allow for, well, IO.
 -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → [TestTree]
assertListEqIO' toT name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) =
        testCase (toString name ⊕ ": "⊕ show i)
                 (got ≫ \ g → assertEq' toT' (Just e) (atMay g i))
      toT' Nothing  = "Nothing"
      toT' (Just a) = "Just " ⊕ toT a

   in testCase (toString name ⊕ ": count") (got ≫ lengthCheck expect)
    : (assertItem ⊳ zip [0..] expect)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α) ⇒
                Text → ψ α → IO (φ α) → [TestTree]
assertListEqIO = assertListEqIO' toText

renderLogWithoutTimeStamp ∷ LogEntry → Doc ()
renderLogWithoutTimeStamp = renderWithSeverity' $ renderWithCallStack pretty

renderLog ∷ LogEntry → Doc ()
renderLog = renderWithTimestamp ∘ renderWithSeverity' $ renderWithCallStack pretty

renderLog' ∷ LogEntry → Doc ()
renderLog' = renderWithTimestamp ∘ renderWithSeverity' $ renderWithStackHead pretty

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
                           , logMsgTests, renderTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
