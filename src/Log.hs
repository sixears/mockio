{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Log
  ( Log, WithLog, WithLogIO
  , log, log', logIO, logIO', logRender, logRender'
  , logToFD', logToFD, logToFile
  -- test data
  , tests, _log0, _log0m, _log1, _log1m )
where

-- base --------------------------------

import Control.Concurrent      ( threadDelay )
import Control.Monad           ( Monad, return )
import Control.Monad.Identity  ( runIdentity )
import Data.Bool               ( Bool( False, True ) )
import Data.Foldable           ( Foldable
                               , foldl', foldl1, foldMap, foldr, foldr1,toList )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( Monoid )
import Data.Semigroup          ( Semigroup )
import Data.String             ( String )
import Data.Tuple              ( snd )
import GHC.Stack               ( CallStack )
import System.Exit             ( ExitCode )
import System.IO               ( Handle, IO, hFlush, hIsTerminalDevice, stderr )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- dlist -------------------------------

import Data.DList  ( DList, fromList, singleton )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask )

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
                                           , ofoldr1Ex , ofoldMap, otoList )
                             , MonoFunctor( omap )
                             )

-- more-unicode ------------------------

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

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus   ( runTestsP, runTestsReplay, runTestTree )
import TastyPlus2  ( assertListEq, assertListEqIO )

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

import Log.LogEntry       ( LogEntry, logEntry, _le0, _le1, _le2, _le3  )
import Log.LogRenderOpts  ( LogAnnotator, LogRenderOpts
                          , logRenderOpts', lroOpts, lroRenderer
                          , lroRendererAnsi
                          , lroRenderSevCS, lroRenderTSSevCSH, lroWidth
                          , renderLogWithCallStack, renderLogWithSeverity
                          , renderLogWithStackHead, renderLogWithTimestamp
                          )

--------------------------------------------------------------------------------

{- | A list of LogEntries. -}
newtype Log = Log { unLog ∷ DList LogEntry }
  deriving (Monoid,Semigroup,Show)

{- | `WithLog` adds in the `CallStack` constraint, so that if you declare your
     function to use this constraint, your function will be included in the
     logged callstack.  If you do not include the `CallStack` constraint, then
     the callpoint from within the function lacking the constraint (and anything
     calling it) will not be shown in the callstack.
 -}
type WithLog   η = (MonadLog Log η, ?stack ∷ CallStack)
{- | `WithLog`, but with MonadIO, too. -}
type WithLogIO μ = (MonadIO μ, MonadLog Log μ, ?stack ∷ CallStack)

type instance Element Log = LogEntry

instance MonoFoldable Log where
  otoList    (Log dl)     = toList dl
  ofoldl'    f x (Log dl) = foldl' f x dl
  ofoldr     f x (Log dl) = foldr  f x dl
  ofoldMap   f (Log dl)   = foldMap f dl
  ofoldr1Ex  f (Log dl)   = foldr1 f dl
  ofoldl1Ex' f (Log dl)   = foldl1 f dl

instance MonoFunctor Log where
  omap f (Log dl) = Log (f ⊳ dl)

instance Printable Log where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog

------------------------------------------------------------

{- | Log a `Doc()` with a timestamp, thus causing IO. -}
logIO' ∷ WithLogIO μ ⇒ Severity → Doc () → μ ()
logIO' sv doc = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv doc

--------------------

-- We redefine this, rather than simply calling logIO', so as to not mess with
-- the callstack.
{- | Log with a timestamp, thus causing IO. -}
logIO ∷ WithLogIO μ ⇒ Severity → Text → μ ()
logIO sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (pretty txt)

----------------------------------------

{- | Log with no IO, thus no timestamp. -}
log ∷ WithLog μ ⇒ Severity → Text → μ ()
log sv txt = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (pretty txt)

--------------------

{- | Log a `Doc()` with no IO, thus no timestamp. -}
log' ∷ WithLog μ ⇒ Severity → Doc() → μ ()
log' sv doc = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv doc

----------------------------------------

{- | Transform a monad ready to return (rather than effect) the logging. -}
logRender ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η α → η (α, DList Text)
logRender opts a = do
  let renderer = lroRenderer opts
  (a',ls) ← runPureLoggingT a
  let lpretty ∷ Doc ρ → SimpleDocStream ρ
      lpretty = layoutPretty (opts ⊣ lroOpts)
      txt = RenderText.renderStrict ∘ lpretty ∘ renderer ⊳ unLog ls
  return $ (a', txt)

--------------------

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η () → η (DList Text)
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
  let layout ∷ (Foldable ψ) ⇒ ψ (Doc π) → SimpleDocStream π
      layout ms = layoutPretty (LayoutOptions pw) (vsep (toList ms) ⊕ line')
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
            → (LogEntry → Doc ρ)                  -- ^ render a LogEntry
            → BatchingOptions
            → PageWidth
            → Handle
            → LoggingT Log μ α
            → μ α
logToHandle renderIO renderEntry bopts width fh io =
  let renderDoc   = vsep ∘ fmap renderEntry ∘ otoList
      handler h   = runLoggingT io (h ∘ renderDoc)
   in withFDHandler renderIO width bopts fh handler

--------------------

{- | Write a Log to a filehandle, with given options but no adornments. -}
logToHandleNoAdornments ∷ (MonadIO μ, MonadMask μ) ⇒
                          BatchingOptions
                        → LogRenderOpts
                        → Handle
                        → LoggingT Log μ α
                        → μ α
logToHandleNoAdornments bopts lro =
  logToHandle RenderText.renderIO (lroRenderer lro) bopts (lro ⊣ lroWidth)

--------------------

{- | Write a Log to a filehandle, with given options and Ansi adornments. -}
logToHandleAnsi ∷ (MonadIO μ, MonadMask μ) ⇒
                  BatchingOptions → LogRenderOpts → Handle → LoggingT Log μ α
                → μ α
logToHandleAnsi bopts lro = logToHandle RenderTerminal.renderIO
                                        (lroRendererAnsi lro) bopts
                                        (lro ⊣ lroWidth)
----------------------------------------

{- | Log to a regular file, with unbounded width. -}
logToFile' ∷ (MonadIO μ, MonadMask μ) ⇒
            [LogAnnotator] → Handle → LoggingT Log μ α → μ α
logToFile' ls = let lro = logRenderOpts' ls Unbounded
                 in logToHandleNoAdornments fileBatchingOptions lro

--------------------

{- | Log to a tty, using current terminal width. -}
logToTTY' ∷ (MonadIO μ, MonadMask μ) ⇒
            [LogAnnotator] → Handle → LoggingT Log μ α → μ α
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
           [LogAnnotator] → Handle → LoggingT Log μ α → μ α
logToFD' ls h io = do
  isatty ← liftIO $ hIsTerminalDevice h
  if isatty
  then logToTTY'  ls h io
  else logToFile' ls h io

----------------------------------------

data CSOpt = NoCallStack | CallStackHead | FullCallStack

{- | Log to a plain file with given callstack choice. -}
logToFile ∷ (MonadIO μ, MonadMask μ) ⇒ CSOpt → Handle → LoggingT Log μ α → μ α
logToFile NoCallStack   =
  logToFile' [ renderLogWithTimestamp, renderLogWithSeverity ]
logToFile CallStackHead = 
  logToFile' [ renderLogWithTimestamp, renderLogWithSeverity
           , renderLogWithStackHead ]
logToFile FullCallStack =
  logToFile' [ renderLogWithCallStack, renderLogWithTimestamp
           , renderLogWithSeverity ]

--------------------

{- | Log to a terminal with given callstack choice. -}
logToTTY ∷ (MonadIO μ, MonadMask μ) ⇒ CSOpt → Handle → LoggingT Log μ α → μ α
logToTTY NoCallStack   =
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
          CSOpt → Handle → LoggingT Log μ α → μ α
logToFD cso h io = do
  isatty ← liftIO $ hIsTerminalDevice h
  if isatty
  then logToTTY  cso h io
  else logToFile cso h io

----------------------------------------

{- | Log to stderr, assuming it's a terminal, with given callstack choice. -}
logToStderr ∷ (MonadIO μ, MonadMask μ) ⇒ CSOpt → LoggingT Log μ α → μ α
logToStderr cso = logToTTY cso stderr

{- | Log to a handle, assuming it's a terminal, with no log decorations. -}
logToTTYPlain ∷ (MonadIO μ, MonadMask μ) ⇒ Handle → LoggingT Log μ α → μ α
logToTTYPlain = logToTTY' []

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

_log0 ∷ Log
_log0 = Log $ fromList [_le0]

_log0m ∷ MonadLog Log η ⇒ η ()
_log0m = logMessage _log0

_log1 ∷ Log
_log1 = Log $ fromList [ _le0, _le1, _le2, _le3 ]

_log1m ∷ MonadLog Log η ⇒ η ()
_log1m = logMessage _log1

_log0io ∷ (MonadIO μ, MonadLog Log μ) ⇒ μ ()
_log0io = do logIO Warning "start"
             liftIO $ threadDelay 2_000_000
             logIO Critical "end"

-- tests -------------------------------

renderTests ∷ TestTree
renderTests =
  let render o = runIdentity ∘ logRender' o
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
                [ assertListEq "render2" exp2 (render lroRenderSevCS _log0m)
                , assertListEqIO "render3"
                                 exp3 (logRender' lroRenderTSSevCSH _log1m)
                ]


tests ∷ TestTree
tests = testGroup "Log" [ renderTests ]

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
  logToStderr NoCallStack _log0io
  logToTTYPlain           stderr _log0io
  logToTTY NoCallStack   stderr _log0io
  logToTTY CallStackHead stderr _log0io
  logToTTY FullCallStack stderr _log0io

-- that's all, folks! ----------------------------------------------------------
