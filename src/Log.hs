{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Log
  ( Log, WithLog, WithLogIO, log, logIO, logIO', logRender, logRender'
  -- test data
  , tests, _log0, _log0m, _log1, _log1m )
where

-- base --------------------------------

import Control.Monad           ( Monad, return )
import Control.Monad.Identity  ( runIdentity )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.Monoid             ( Monoid )
import Data.Semigroup          ( Semigroup )
import Data.String             ( String )
import Data.Tuple              ( snd )
import GHC.Stack               ( CallStack, SrcLoc( SrcLoc ), fromCallSiteList )
import System.Exit             ( ExitCode )
import System.IO               ( IO )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- dlist -------------------------------

import Data.DList  ( DList, fromList, singleton, toList )

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

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Natural  ( ℕ )

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
import Data.Text.Prettyprint.Doc.Render.Text  ( renderStrict )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-plus --------------------------

import TastyPlus   ( (≟), runTestsP, runTestsReplay, runTestTree,withResource' )
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
import Log.LogRenderOpts  ( LogRenderOpts, lroOpts, lroRenderer, lroRenderPlain
                          , lroRenderSevCS, lroRenderTSSevCSH )

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

instance Printable Log where
  print = P.text ∘ unlines ∘ toList ∘ fmap toText ∘ unLog

logIO' ∷ WithLogIO μ ⇒ Severity → Doc () → μ ()
logIO' sv doc = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv doc

-- We redefine this, rather than simply calling logIO', so as to not mess with
-- the callstack.
logIO ∷ WithLogIO μ ⇒ Severity → Text → μ ()
logIO sv txt = do
  tm ← liftIO getCurrentTime
  logMessage ∘ Log ∘ singleton $ logEntry ?stack (Just tm) sv (pretty txt)

log ∷ WithLog μ ⇒ Severity → Text → μ ()
log sv txt = do
  logMessage ∘ Log ∘ singleton $ logEntry ?stack Nothing sv (pretty txt)

logRender ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η α → η (α, DList Text)
logRender opts a = do
  let renderer = lroRenderer opts
  (a',ls) ← runPureLoggingT a
  return ∘ (a',) $ renderStrict ∘ layoutPretty (opts ⊣ lroOpts) ∘ renderer ⊳ unLog ls

{- | `logRender` with `()` is sufficiently common to warrant a cheap alias. -}
logRender' ∷ Monad η ⇒ LogRenderOpts → PureLoggingT Log η () → η (DList Text)
logRender' = fmap snd ⩺ logRender


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

-- tests -------------------------------

renderTests ∷ TestTree
renderTests =
  let render o = runIdentity ∘ logRender' o
      line10 = AvailablePerLine 10 1.0
      c = fromCallSiteList [("foo", SrcLoc "a" "b" "c" 1 2 3 4)]
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

-- that's all, folks! ----------------------------------------------------------
