{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
-- {-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module Log
  ( CSOpt(..), Log, ToDoc_( toDoc_ ), WithLog, WithLogIO

  , emergency, alert, critical, err, warn, notice, info, debug
  , emergency', alert', critical', err', warn', notice', info', debug'
  , emergencyT, alertT, criticalT, errT, warnT, noticeT, infoT, debugT

  , fromList
  , log, logMsg, log', logMsg', logT, logMsgT, logT', logMsgT'
  , logIO, logIO', logIOT
  , logRender, logRender'
  , logToFD', logToFD, logToFile, logToStderr
  -- test data
  , tests, _log0, _log0m, _log1, _log1m )
where

import Prelude ( toInteger )

-- base --------------------------------

import qualified  Data.Foldable  as  Foldable

import Control.Concurrent      ( threadDelay )
import Control.Monad           ( Monad, return )
import Control.Monad.Identity  ( Identity, runIdentity )
import Data.Bool               ( Bool( False, True ) )
import Data.Eq                 ( Eq )
import Data.Foldable           ( Foldable, all, foldl', foldl1
                               , foldMap, foldr, foldr1 )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.List               ( zip )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( Monoid )
import Data.Ord                ( (<) )
import Data.Semigroup          ( Semigroup )
import Data.String             ( String )
import Data.Tuple              ( snd )
import GHC.Exts                ( IsList( Item, fromList, toList ) )
import GHC.Stack               ( CallStack )
import System.Exit             ( ExitCode )
import System.IO               ( Handle, IO, hFlush, hIsTerminalDevice, stderr )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- dlist -------------------------------

import qualified  Data.DList  as  DList
import Data.DList  ( DList, singleton )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- logging-effect ----------------------

import Control.Monad.Log  ( BatchingOptions( BatchingOptions
                                           , blockWhenFull, flushMaxQueueSize )
                          , Handler, MonadLog, LoggingT, PureLoggingT
                          , Severity(..)
                          , flushMaxDelay, logMessage, runLoggingT
                          , runPureLoggingT, withBatchedHandler
                          )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element
                             , MonoFoldable( ofoldl', ofoldl1Ex', ofoldr
                                           , ofoldr1Ex , ofoldMap, olength
                                           , otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (⪼) )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import qualified  Data.Text.Prettyprint.Doc.Render.Terminal  as  RenderTerminal
import qualified  Data.Text.Prettyprint.Doc.Render.Text      as  RenderText

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth( AvailablePerLine, Unbounded )
                                  , SimpleDocStream(..)
                                  , layoutPretty, line', pretty, vsep
                                  )

-- single ------------------------------

import Single( MonoSingle( osingle ), ofilt', single )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus   ( runTestsP, runTestsReplay, runTestTree )
import TastyPlus2  ( assertListEq, assertListCmp, assertListEqIO )

-- terminal-size -----------------------

import qualified  System.Console.Terminal.Size  as  TerminalSize

-- text --------------------------------

import Data.Text  ( Text, intercalate, unlines )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- time --------------------------------

import Data.Time.Clock     ( getCurrentTime )

------------------------------------------------------------
--                     local imports                       -
------------------------------------------------------------

import Log.Equish         ( Equish( (≃) ) )
import Log.LogEntry       ( LogEntry, LogEntry
                          , attrs, logEntry
                          , _le0, _le1, _le2, _le3, _le4n, _le5n
                          )
import Log.LogRenderOpts  ( LogAnnotator, LogRenderOpts
                          , logRenderOpts', lroOpts, lroRenderer
                          , lroRendererAnsi
                          , lroRenderSevCS, lroRenderTSSevCSH, lroWidth
                          , renderLogWithCallStack, renderLogWithSeverity
                          , renderLogWithStackHead, renderLogWithTimestamp
                          )

--------------------------------------------------------------------------------

{- | A list of LogEntries. -}
newtype Log ω = Log { unLog ∷ DList (LogEntry ω) }
  deriving (Eq,Monoid,Semigroup,Show)

{- | `WithLog` adds in the `CallStack` constraint, so that if you declare your
     function to use this constraint, your function will be included in the
     logged callstack.  If you do not include the `CallStack` constraint, then
     the callpoint from within the function lacking the constraint (and anything
     calling it) will not be shown in the callstack.
 -}
type WithLog α η = (MonadLog (Log α) η, ?stack ∷ CallStack)
{- | `WithLog`, but with MonadIO, too. -}
type WithLogIO α μ = (MonadIO μ, MonadLog (Log α) μ, ?stack ∷ CallStack)

type instance Element (Log ω) = LogEntry ω

{- This Foldable instance would give rise to toList being a list of α, i.e., the
   payload; rather than of LogEntry α; which, therefore, would be a
   contradiction of IsList.toList -- that will lead to surprises, I don't think
   it's a good idea.

instance Foldable Log where
  foldr ∷ ∀ α β . (α → β → β) → β → Log α → β
  foldr f b (Log ls) = foldr (f ∘ view attrs) b ls
-}

instance MonoFoldable (Log ω) where
  otoList    (Log dl)     = toList dl
  ofoldl'    f x (Log dl) = foldl' f x dl
  ofoldr     f x (Log dl) = foldr  f x dl
  ofoldMap   f (Log dl)   = foldMap f dl
  ofoldr1Ex  f (Log dl)   = foldr1 f dl
  ofoldl1Ex' f (Log dl)   = foldl1 f dl

instance MonoFunctor (Log ω) where
  omap f (Log dl) = Log (f ⊳ dl)

instance Printable ω ⇒ Printable (Log ω) where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog

instance Equish ω ⇒ Equish (Log ω) where
  l ≃ l' = olength l ≡ olength l'
         ∧ all (\ (x,x') → x ≃ x') (zip (otoList l) (otoList l'))

instance MonoSingle (Log ω) where
  osingle w = Log (single w)

------------------------------------------------------------

{- | This is called `ToDoc_` with an underscore to distinguish from any `ToDoc`
     class that took a parameter for the annotation type. -}
class ToDoc_ α where
  toDoc_ ∷ α → Doc ()

instance ToDoc_ Text where
  toDoc_ = pretty

instance ToDoc_ (Doc()) where
  toDoc_ = id

------------------------------------------------------------

instance IsList (Log ω) where
  type Item (Log ω) = LogEntry ω
  fromList ∷ [LogEntry ω] → Log ω
  fromList ls = Log (DList.fromList ls)
  toList (Log ls) = DList.toList ls

----------------------------------------

{- | Log with a timestamp, thus causing IO. -}
logIO ∷ ∀ ρ ω μ . (WithLogIO ω μ, ToDoc_ ρ) ⇒ Severity → ω → ρ → μ ()
logIO sv p txt = do
  -- note that callstack starts here, *including* the call to logIO; this is
  -- deliberate, so that we see where in the code we made the log
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (toDoc_ txt) p

--------------------

-- We redefine this, rather than simply calling logIO, so as to not mess with
-- the callstack.
{- | Log with a timestamp, thus causing IO. -}
logIO' ∷ ∀ ρ ω μ . (WithLogIO ω μ, ToDoc_ ρ, Default ω) ⇒ Severity → ρ → μ ()
logIO' sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (toDoc_ txt) def

----------------------------------------

-- We redefine this, rather than simply calling logIO, so as to not mess with
-- the callstack.
{- | Log `Text` with a timestamp, thus causing IO. -}
logIOT ∷ ∀ ω μ . (WithLogIO ω μ, Default ω) ⇒ Severity → Text → μ ()
logIOT sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (toDoc_ txt) def

----------------------------------------

{- | Log with no IO, thus no timestamp. -}
log ∷ ∀ ω η ρ . (WithLog ω η, ToDoc_ ρ) ⇒ Severity → ω → ρ → η ()
log sv p txt =
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) p

{- | Alias for `log`, to avoid clashing with `Prelude.log`. -}
logMsg ∷ ∀ ω η ρ . (WithLog ω η, ToDoc_ ρ) ⇒ Severity → ω → ρ → η ()
logMsg = log

----------

{- | `log`, with a default value. -}
log' ∷ ∀ ω η ρ . (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ Severity → ρ → η ()
log' sv txt = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) def

----------

{- | Alias for `log'`, for consistency with `logMsg`. -}
logMsg' ∷ ∀ ω η ρ . (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ Severity → ρ → η ()
logMsg' = log'

----------

{- | `log`, with input type fixed to Text to avoid having to specify. -}
logT ∷ ∀ ω η . (WithLog ω η) ⇒ Severity → ω → Text → η ()
logT sv p txt =
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) p

----------

{- | Alias for `logT`, for consistency with `logMsg`. -}
logMsgT ∷ ∀ ω η . (WithLog ω η) ⇒ Severity → ω → Text → η ()
logMsgT sv p txt =
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) p

----------

{- | `log'`, with input type fixed to Text to avoid having to specify. -}
logT' ∷ ∀ ω η . (WithLog ω η, Default ω) ⇒ Severity → Text → η ()
logT' sv txt =
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) def

----------

{- | Alias for `logT'`, for consistency with `logMsg`. -}
logMsgT' ∷ ∀ ω η . (WithLog ω η, Default ω) ⇒ Severity → Text → η ()
logMsgT' sv txt =
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (toDoc_ txt) def

--------------------

emergency ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
emergency = log Emergency

----------

emergency' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
emergency' = log Emergency def

----------

emergencyT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
emergencyT = emergency'

----------

alert ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
alert = log Alert

----------

alert' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
alert' = log Alert def

----------

alertT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
alertT = alert'

----------

critical ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
critical = log Critical

----------

critical' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
critical' = log Critical def

----------

criticalT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
criticalT = critical'

----------

err ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
err = log Error

----------

err' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
err' = log Error def

----------

errT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
errT = err'

----------

warn ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
warn = log Warning

----------

warn' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
warn' = log Warning def

----------

warnT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
warnT = warn'

----------

notice ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
notice = log Notice

----------

notice' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
notice' = log Notice def

----------

noticeT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
noticeT = notice'

----------

info ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
info = log Informational

----------

info' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
info' = log Informational def

----------

infoT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
infoT = info'

----------

debug ∷ (WithLog ω η, ToDoc_ ρ) ⇒ ω → ρ → η ()
debug = log Debug

----------

debug' ∷ (WithLog ω η, ToDoc_ ρ, Default ω) ⇒ ρ → η ()
debug' = log Debug def

----------

debugT ∷ (WithLog ω η, Default ω) ⇒ Text → η ()
debugT = debug'

----------------------------------------

{- | Transform a monad ready to return (rather than effect) the logging. -}
logRender ∷ Monad η ⇒
            LogRenderOpts → PureLoggingT (Log ω) η α → η (α, DList Text)
logRender opts a = do
  let renderer = lroRenderer opts
  (a',ls) ← runPureLoggingT a
  let lpretty ∷ Doc ρ → SimpleDocStream ρ
      lpretty = layoutPretty (opts ⊣ lroOpts)
      txt = RenderText.renderStrict ∘ lpretty ∘ renderer ⊳ unLog ls
  return $ (a', txt)

--------------------

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒
             LogRenderOpts → PureLoggingT (Log ω) η () → η (DList Text)
logRender' = fmap snd ⩺ logRender

----------------------------------------

{- | How to write to an FD with given options, using `withBatchedHandler`.
     Each `Doc` is vertically separated.
 -}
withFDHandler ∷ (MonadIO μ, MonadMask μ) ⇒
                (Handle → SimpleDocStream ρ → IO ())
              → PageWidth
              → BatchingOptions
              → Handle
              → (Handler μ (Doc ρ) -> μ α)
              → μ α
withFDHandler r pw bopts fd handler =
  let layout ∷ Foldable ψ ⇒ ψ (Doc π) → SimpleDocStream π
      layout ms = layoutPretty (LayoutOptions pw)
                               (vsep (Foldable.toList ms) ⊕ line')
      flush messages = r fd (layout messages) ⪼ hFlush fd
   in withBatchedHandler bopts flush handler

----------------------------------------

{-| Options suitable for logging to a file; notably a 1s flush delay and keep
    messages rather than dropping if the queue fills.
 -}
fileBatchingOptions ∷ BatchingOptions
fileBatchingOptions = BatchingOptions { flushMaxDelay     = 1_000_000
                                      , blockWhenFull     = True
                                      , flushMaxQueueSize = 100
                                      }

{-| Options suitable for logging to a tty; notably a short flush delay (0.1s),
    and drop messages rather than blocking if the queue fills (which should
    be unlikely, with a length of 100 & 0.1s flush).
 -}
ttyBatchingOptions ∷ BatchingOptions
ttyBatchingOptions = BatchingOptions { flushMaxDelay     = 100_000
                                     , blockWhenFull     = False
                                     , flushMaxQueueSize = 100
                                     }

----------------------------------------

{- | Write a Log to a filehandle, with given rendering and options. -}
logToHandle ∷ (MonadIO μ, MonadMask μ) ⇒
              (Handle → SimpleDocStream ρ → IO()) -- ^ write an SDSρ to Handle
            → (LogEntry ω → Doc ρ)                -- ^ render a LogEntry
            → BatchingOptions
            → PageWidth
            → Handle
            → LoggingT (Log ω) μ α
            → μ α
logToHandle renderIO renderEntry bopts width fh io =
  let -- `vsep` returns an emptyDoc for an empty list; that results in a blank
      -- line.  We don't want that; the blank line appears whenever a log was
      -- filtered; which would really suck for heavily filtered logs (thus
      -- discouraging the use of logs for infrequently looked-at things - but
      -- then making it awkward to debug irritating edge-cases.
      -- So we define a `vsep` variant, `vsep'`, which declares `Nothing`
      -- for empty docs, thus we can completely ignore them (dont call the
      -- logger at all)
      vsep' [] = Nothing
      vsep' xs = Just $ vsep xs
      -- renderDoc   ∷ Log ω → Maybe (Doc ρ)
      renderDoc   = vsep' ∘ fmap renderEntry ∘ otoList
      -- handler     ∷ (Maybe (Doc ρ) → μ ()) → μ α
      handler h   =
        runLoggingT io ((\ case Just d → h d; Nothing → return ()) ∘ renderDoc)
   in withFDHandler renderIO width bopts fh handler

--------------------

{- | Write a Log to a filehandle, with given options but no adornments. -}
logToHandleNoAdornments ∷ (MonadIO μ, MonadMask μ) ⇒
                          BatchingOptions
                        → LogRenderOpts
                        → Handle
                        → LoggingT (Log ω) μ α
                        → μ α
logToHandleNoAdornments bopts lro =
  logToHandle RenderText.renderIO (lroRenderer lro) bopts (lro ⊣ lroWidth)

--------------------

{- | Write a Log to a filehandle, with given options and Ansi adornments. -}
logToHandleAnsi ∷ (MonadIO μ, MonadMask μ) ⇒
                  BatchingOptions
                → LogRenderOpts
                → Handle
                → LoggingT (Log ω) μ α
                → μ α
logToHandleAnsi bopts lro = logToHandle RenderTerminal.renderIO
                                        (lroRendererAnsi lro) bopts
                                        (lro ⊣ lroWidth)
----------------------------------------

{- | Log to a regular file, with unbounded width. -}
logToFile' ∷ (MonadIO μ, MonadMask μ) ⇒
             [LogAnnotator] → Handle → LoggingT (Log ω) μ α → μ α
logToFile' ls = let lro = logRenderOpts' ls Unbounded
                 in logToHandleNoAdornments fileBatchingOptions lro

--------------------

{- | Log to a tty, using current terminal width. -}
logToTTY' ∷ (MonadIO μ, MonadMask μ) ⇒
            [LogAnnotator] → Handle → LoggingT (Log ω) μ α → μ α
logToTTY' ls h io = do
  size ← liftIO $ TerminalSize.size
  let lro = case size of
              Just sz → let width = AvailablePerLine (TerminalSize.width sz) 1.0
                         in logRenderOpts' ls width
              Nothing → logRenderOpts' ls Unbounded
  logToHandleAnsi ttyBatchingOptions lro h io

--------------------

{- | Log to a file handle; if it looks like a terminal, use Ansi logging and low
     batch time; else go unadorned with higher batch time. -}
logToFD' ∷ (MonadIO μ, MonadMask μ) ⇒
           [LogAnnotator] → Handle → LoggingT (Log ω) μ α → μ α
logToFD' ls h io = do
  isatty ← liftIO $ hIsTerminalDevice h
  if isatty
  then logToTTY'  ls h io
  else logToFile' ls h io

----------------------------------------

data CSOpt = NoCallStack | CallStackHead | FullCallStack

{- | Log to a plain file with given callstack choice. -}
logToFile ∷ (MonadIO μ, MonadMask μ) ⇒
            CSOpt → Handle → LoggingT (Log ω) μ α → μ α
logToFile NoCallStack =
  logToFile' [ renderLogWithTimestamp, renderLogWithSeverity ]
logToFile CallStackHead =
  logToFile' [ renderLogWithTimestamp, renderLogWithSeverity
             , renderLogWithStackHead ]
logToFile FullCallStack =
  logToFile' [ renderLogWithCallStack, renderLogWithTimestamp
             , renderLogWithSeverity ]

--------------------

{- | Log to a terminal with given callstack choice. -}
logToTTY ∷ (MonadIO μ, MonadMask μ) ⇒
           CSOpt → Handle → LoggingT (Log ω) μ α → μ α
logToTTY NoCallStack =
  logToTTY' [ renderLogWithTimestamp, renderLogWithSeverity ]
logToTTY CallStackHead =
  logToTTY' [ renderLogWithTimestamp, renderLogWithSeverity
            , renderLogWithStackHead ]
logToTTY FullCallStack =
  logToTTY' [ renderLogWithCallStack, renderLogWithTimestamp
            , renderLogWithSeverity ]

--------------------

{- | Log to a file handle; if it looks like a terminal, use Ansi logging and
     current terminal width; else go unadorned with unbounded width. -}
logToFD ∷ (MonadIO μ, MonadMask μ) ⇒
          CSOpt → Handle → LoggingT (Log ω) μ α → μ α
logToFD cso h io = do
  isatty ← liftIO $ hIsTerminalDevice h
  if isatty
  then logToTTY  cso h io
  else logToFile cso h io

----------------------------------------

{- | Log to stderr, assuming it's a terminal, with given callstack choice &
     filter. -}
logToStderr ∷ (MonadIO μ, MonadMask μ) ⇒
              CSOpt → LoggingT (Log ω) μ α → μ α
logToStderr cso = logToTTY cso stderr

{- | Log to a handle, assuming it's a terminal, with no log decorations. -}
logToTTYPlain ∷ (MonadIO μ, MonadMask μ) ⇒ Handle → LoggingT (Log ω) μ α → μ α
logToTTYPlain = logToTTY' []

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

_log0 ∷ Log ()
_log0 = fromList [_le0]

_log0m ∷ MonadLog (Log ()) η ⇒ η ()
_log0m = logMessage _log0

_log1 ∷ Log ()
_log1 = fromList [ _le0, _le1, _le2, _le3 ]

_log1m ∷ MonadLog (Log ()) η ⇒ η ()
_log1m = logMessage _log1

_log2 ∷ MonadLog (Log ℕ) η ⇒ η ()
_log2 = do logT Warning       1 "start"
           logT Informational 3 "middle"
           logT Critical      2 "end"

_log0io ∷ (MonadIO μ, MonadLog (Log ℕ) μ) ⇒ μ ()
_log0io = do logIO @Text Warning 1 "start"
             liftIO $ threadDelay 1_000_000
             logIO @Text Informational 3 "middle"
             liftIO $ threadDelay 1_000_000
             logIO @Text Critical 2 "end"

_log1io ∷ (MonadIO μ, MonadLog (Log ℕ) μ) ⇒ μ ()
_log1io = do logIO @Text Warning 1 "start"
             liftIO $ threadDelay 1_000_000
             logIO @Text Informational 3 "you shouldn't see this"
             liftIO $ threadDelay 1_000_000
             logIO @Text Critical 2 "end"

-- tests -------------------------------

renderTests ∷ TestTree
renderTests =
  let render o = runIdentity ∘ logRender' o
      exp2 ∷ [Text]
      exp2 = [ intercalate "\n" [ "[Info] log_entry 1"
                                , "  stack0, called at c:1:2 in a:b"
                                , "    stack1, called at f:5:6 in d:e"
                                ]
             ]
      exp3 ∷ [Text]
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
                [ assertListEq "render2" exp2 (render lroRenderSevCS _log0m)
                , assertListEqIO "render3"
                                 exp3 (logRender' lroRenderTSSevCSH _log1m)
                ]


--------------------

runPureLoggingT' ∷ (Monad η, Monoid α) ⇒ PureLoggingT α η () → η α
runPureLoggingT' = snd ⩺ runPureLoggingT

-- instance Printable ℕ where
--   print n = P.string (show n)

{- | (F)Map the payload of a log. -}
mapLog ∷ MonadLog ω' η ⇒ (ω → ω') → LoggingT ω η σ → η σ
mapLog f m = runLoggingT m (logMessage ∘ f)

{- | Filter a log with some predicate; return a `Log` with only entries whose
     payload matches the given predicate. -}
filterLog ∷ MonadLog (Log ω) η ⇒ (ω → 𝔹) → LoggingT (Log ω) η σ → η σ
filterLog p = mapLog $ ofilt' (p ∘ view attrs)

filterTests ∷ TestTree
filterTests =
  let runLog ∷ Monoid α ⇒ PureLoggingT α Identity () → α
      runLog = runIdentity ∘ runPureLoggingT'
   in testGroup "filter"
            [ assertListCmp (toText ∘ fmap toInteger) (toText ∘ fmap toInteger)
                            (≃) ("<3" ∷ Text)
                            [ _le4n, _le5n ]
                            (otoList ∘ runLog $ filterLog ((<3)) _log2)
            ]

--------------------

tests ∷ TestTree
tests = testGroup "Log" [ renderTests, filterTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

{- | Manual tests - run these by hand, there is no automated testing option
     for these. -}
_testm ∷ IO ()
_testm = do
  logToStderr NoCallStack          _log0io
  logToTTYPlain             stderr _log0io
  logToTTY    NoCallStack   stderr _log0io
  logToTTY    CallStackHead stderr _log0io
  logToTTY    CallStackHead stderr _log0io
  logToTTY    FullCallStack stderr (filterLog (<3) _log1io)

_testm' ∷ IO ()
_testm' = do
  logToTTY    FullCallStack stderr (filterLog (<3) _log1io)

-- that's all, folks! ----------------------------------------------------------
