{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Log
  ( Log, WithLog, WithLogIO, log, log', logIO, logIO', logRender, logRender'
  -- test data
  , tests, _log0, _log0m, _log1, _log1m )
where

-- base --------------------------------

import Control.Concurrent      ( threadDelay )
import Control.Monad           ( Monad, return )
import Control.Monad.Identity  ( runIdentity )
import Data.Bool               ( Bool( False ) )
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
import System.IO               ( Handle, IO, hFlush, stderr )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( def )

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
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth, SimpleDocStream(..)
                                  , layoutPretty, line', pretty, vsep
                                  )
import Data.Text.Prettyprint.Doc.Render.Text  ( renderIO, renderStrict )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-plus --------------------------

import TastyPlus   ( runTestsP, runTestsReplay, runTestTree )
import TastyPlus2  ( assertListEq, assertListEqIO )

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
import Log.LogRenderOpts  ( LogRenderOpts, lroOpts, lroRenderer, lroRenderSevCS
                          , lroRenderTSSevCSH, lroWidth )

--------------------------------------------------------------------------------

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

{- | Log a `Doc()` with a timestamp, thus causing IO. -}
logIO' ∷ WithLogIO μ ⇒ Severity → Doc () → μ ()
logIO' sv doc = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv doc

-- We redefine this, rather than simply calling logIO', so as to not mess with
-- the callstack.
{- | Log with a timestamp, thus causing IO. -}
logIO ∷ WithLogIO μ ⇒ Severity → Text → μ ()
logIO sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (pretty txt)

{- | Log with no IO, thus no timestamp. -}
log ∷ WithLog μ ⇒ Severity → Text → μ ()
log sv txt = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (pretty txt)

{- | Log a `Doc()` with no IO, thus no timestamp. -}
log' ∷ WithLog μ ⇒ Severity → Doc() → μ ()
log' sv doc = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv doc

logRender ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η α → η (α, DList Text)
logRender opts a = do
  let renderer = lroRenderer opts
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderStrict ∘ layoutPretty (opts ⊣ lroOpts) ∘ renderer ⊳ unLog ls

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η () → η (DList Text)
logRender' = fmap snd ⩺ logRender

{- | Create a new Handler that will append to a given `Handle`.  Note that this
     Handler requires log messages to be of type `PP.Doc`. This abstractly
     specifies a pretty-printing for log lines. The additional arguments to
     `withFDHandler` determine how this pretty-printing should be realised when
     outputting log lines.
  
     These Handlers asynchronously log messages to the given file descriptor,
     rather than blocking.
 -}
withFDHandler ∷ (MonadIO μ,MonadMask μ) ⇒ BatchingOptions → Handle → PageWidth
                                        → (Handler μ (Doc ρ) -> μ α) → μ α
withFDHandler options fd pw =
  let layout ∷ (Foldable ψ) ⇒ ψ (Doc π) → SimpleDocStream π
      layout ms = layoutPretty (LayoutOptions pw) (vsep (toList ms) ⊕ line')
      flush messages = do renderIO fd (layout messages)
                          hFlush fd
   in withBatchedHandler options flush

logToHandle ∷ (MonadIO μ, MonadMask μ) ⇒
               BatchingOptions → LogRenderOpts → Handle → LoggingT Log μ α → μ α
logToHandle bopts lro fh io =
  let renderEntry     = lroRenderer lroRenderTSSevCSH
      renderDoc       = vsep ∘ fmap renderEntry ∘ otoList
      handler stderrH = runLoggingT io (stderrH ∘ renderDoc)
   in withFDHandler bopts fh (lro ⊣ lroWidth) handler

{-| Options suitable for logging to a tty; notably a short flush delay (0.1s),
    and drop messages rather than blocking if the queue fills (which should
    be unlikely, with a length of 100 & 0.1s flush).
 -}
ttyBatchingOptions ∷ BatchingOptions
ttyBatchingOptions = BatchingOptions { flushMaxDelay     = 100_000
                                     , blockWhenFull     = False
                                     , flushMaxQueueSize = 100
                                     }

logToStderr ∷ (MonadIO μ, MonadMask μ) ⇒ LoggingT Log μ α → μ α
logToStderr = logToHandle ttyBatchingOptions def stderr

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
_log0io = do logIO Informational "start"
             liftIO $ threadDelay 2_000_000
             logIO Informational "end"

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
_testm = logToStderr _log0io

-- that's all, folks! ----------------------------------------------------------
