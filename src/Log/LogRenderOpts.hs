{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Log.LogRenderOpts
  ( LogR, LogRenderOpts

  , logRenderOpts'

  , lroOpts, lroRenderPlain, lroRenderSevCS, lroRenderSevCSH
  , lroRenderTSSev, lroRenderTSSevCS, lroRenderTSSevCSH
  , lroRenderer, lroWidth

  , renderWithCallStack, renderWithSeverity, renderWithStackHead
  , renderWithTimestamp

  , tests
  )
where

-- base --------------------------------

import Data.Foldable  ( Foldable( foldr ) )
import Data.Function  ( ($), flip, id )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Nothing ) )
import Data.String    ( String )
import GHC.Stack      ( SrcLoc )
import System.Exit    ( ExitCode )
import System.IO      ( Handle, IO, stdout )
import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( Lens', lens, view )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Alert, Debug, Critical, Emergency, Error
                                    , Informational, Notice, Warning ) )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element, MonoFoldable( otoList )
                             , MonoFunctor( omap ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monoid   ( ф )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth( Unbounded )
                                  , defaultLayoutOptions, layoutPageWidth
                                  , layoutPretty, line, pretty, reAnnotate, vsep
                                  )
import Data.Text.Prettyprint.Doc.Render.Text      ( renderStrict )

-- prettyprinter-ansi-terminal ---------


import qualified  Data.Text.Prettyprint.Doc.Render.Terminal  as  Terminal
import Data.Text.Prettyprint.Doc.Render.Terminal  ( AnsiStyle )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertListEq, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text  as  T

import Data.Text  ( Text )

------------------------------------------------------------
--                     Local Imports                      --
------------------------------------------------------------

import qualified  Log.LogEntry  as  LogEntry

import Log.LogEntry      ( LogEntry , logEntry, _le0 )
import Log.Render        ( renderWithCallStack, renderWithSeverity
                         , renderWithSeverityAndTimestamp
                         , renderWithSeverityAnsi, renderWithStackHead
                         , renderWithTimestamp
                         )

--------------------------------------------------------------------------------

type LogR ω = (LogEntry ω → Doc AnsiStyle) → LogEntry ω → Doc AnsiStyle

------------------------------------------------------------

newtype LogRenderer ω = LogRenderer { unLogRenderer ∷ [LogR ω] }

--------------------

type instance Element (LogRenderer ω) = (LogR ω)

--------------------

instance MonoFunctor (LogRenderer ω) where
  omap f (LogRenderer ls) = LogRenderer (f ⊳ ls)

----------------------------------------

class HasLogRenderer ω α where
  logRenderer ∷ Lens' α (LogRenderer ω)

--------------------

instance HasLogRenderer ω (LogRenderer ω) where
  logRenderer = id

------------------------------------------------------------

data LogRenderOpts ω =
  LogRenderOpts { {-| List of log annotators; applied in list order, tail of the
                      list first; hence, given that many renderers add something
                      to the LHS, the head of the list would be lefthand-most in
                      the resulting output.
                   -}
                  _lroRenderers    ∷ LogRenderer ω
                , _lroWidth        ∷ PageWidth
                }

--------------------

instance Default (LogRenderOpts ω) where
  def = LogRenderOpts (LogRenderer []) Unbounded

--------------------

instance HasLogRenderer ω (LogRenderOpts ω) where
  logRenderer ∷ Lens' (LogRenderOpts ω) (LogRenderer ω)
  logRenderer = lens _lroRenderers (\ opts rs → opts { _lroRenderers = rs })

------------------------------------------------------------

logRenderOpts' ∷ [LogR ω] → PageWidth → LogRenderOpts ω
logRenderOpts' as w = LogRenderOpts (LogRenderer as) w

{- | `LogRenderOpts` with no adornments.  Page width is `Unbounded`; you can
     override this with lens syntax, e.g.,

     > lroRenderPlain & lroWidth .~ AvailablePerLine 80 1.0
 -}
lroRenderPlain ∷ LogRenderOpts ω
lroRenderPlain = logRenderOpts' [] Unbounded

{- | `LogRenderOpts` with severity. -}
lroRenderSev ∷ LogRenderOpts ω
lroRenderSev = logRenderOpts' [ renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp & severity. -}
lroRenderTSSev ∷ LogRenderOpts ω
lroRenderTSSev =
  logRenderOpts' [ renderLogWithTimestamp, renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with severity & callstack. -}
lroRenderSevCS ∷ LogRenderOpts ω
lroRenderSevCS =
  logRenderOpts' [ renderLogWithCallStack, renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack. -}
lroRenderTSSevCS ∷ LogRenderOpts ω
lroRenderTSSevCS =
  logRenderOpts' [ renderLogWithCallStack,renderLogWithTimestamp
                 , renderLogWithSeverity ]
                Unbounded

{- | `LogRenderOpts` with severity & callstack head. -}
lroRenderSevCSH ∷ LogRenderOpts ω
lroRenderSevCSH =
  logRenderOpts' [ renderLogWithSeverity, renderLogWithStackHead ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack head.
 -}
lroRenderTSSevCSH ∷ LogRenderOpts ω
lroRenderTSSevCSH =
  logRenderOpts' [ renderLogWithTimestamp, renderLogWithSeverity
                 , renderLogWithStackHead ]
                Unbounded

{- | A single renderer for a LogEntry, using the renderers selected in
     LogRenderOpts. -}
lroRenderer ∷ LogRenderOpts ω → LogEntry ω → Doc AnsiStyle
lroRenderer opts =
  let foldf ∷ Foldable ψ ⇒ ψ (α → α) → α → α
      foldf = flip (foldr ($))
   in foldf (unLogRenderer (opts ⊣ logRenderer))
            (reAnnotate ф ∘ view LogEntry.logdoc)

lroRendererTests ∷ TestTree
lroRendererTests =
  let renderDoc ∷ Doc α → Text
      renderDoc = renderStrict ∘ layoutPretty defaultLayoutOptions

      check nme exp rs = let opts     = logRenderOpts' rs Unbounded
                             rendered = lroRenderer opts _le0
                          in testCase nme $ exp ≟ renderDoc rendered
      checks nme exp rs = let opts     = logRenderOpts' rs Unbounded
                              rendered = lroRenderer opts _le0
                           in assertListEq nme exp (T.lines $ renderDoc rendered)
   in testGroup "lroRenderer"
                [ check "plain" "log_entry 1" []
                , check "sev" "[Info] log_entry 1" [renderLogWithSeverity]
                , check "ts" "[1970-01-01Z00:00:00 Thu] log_entry 1"
                             [renderLogWithTimestamp]
                , check "ts" "«c#1» log_entry 1" [renderLogWithStackHead]
                , checks "cs" [ "log_entry 1"
                              , "  stack0, called at c:1:2 in a:b"
                              , "    stack1, called at f:5:6 in d:e"
                              ]
                             [renderLogWithCallStack]
                , check "ts-sev"
                    "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                        [renderLogWithTimestamp,renderLogWithSeverity]
                , check "sev-ts"
                    "[Info] [1970-01-01Z00:00:00 Thu] log_entry 1"
                        [renderLogWithSeverity,renderLogWithTimestamp]
                , checks "ts-sev-cs"
                         [ "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                         ,   "                                 "
                           ⊕ "  stack0, called at c:1:2 in a:b"
                         ,   "                                 "
                           ⊕ "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderLogWithTimestamp, renderLogWithSeverity
                         , renderLogWithCallStack ]
                , checks "cs-ts-sev"
                         [ "[1970-01-01Z00:00:00 Thu] [Info] log_entry 1"
                         , "  stack0, called at c:1:2 in a:b"
                         , "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderLogWithCallStack
                         , renderLogWithTimestamp, renderLogWithSeverity ]
                , checks "sev-cs-ts"
                         [ "[Info] [1970-01-01Z00:00:00 Thu] log_entry 1"
                         , "         stack0, called at c:1:2 in a:b"
                         , "           stack1, called at f:5:6 in d:e"
                         ]
                         [ renderLogWithSeverity, renderLogWithCallStack
                         , renderLogWithTimestamp ]
                , checks "cs-sevts"
                         [ "[1970-01-01Z00:00:00 Thu|Info] log_entry 1"
                         , "  stack0, called at c:1:2 in a:b"
                         , "    stack1, called at f:5:6 in d:e"
                         ]
                         [ renderLogWithCallStack
                         , renderLogWithSeverityAndTimestamp ]
                , checks "sh-sevts"
                         [ "[1970-01-01Z00:00:00 Thu|Info] «c#1» log_entry 1"]
                         [ renderLogWithSeverityAndTimestamp
                         , renderLogWithStackHead ]
                ]

lroWidth ∷ Lens' (LogRenderOpts ω) PageWidth
lroWidth = lens _lroWidth (\ opts w → opts { _lroWidth = w })

lroOpts ∷ Lens' (LogRenderOpts ω) LayoutOptions
lroOpts = lens (LayoutOptions ∘ _lroWidth)
               (\ opts lo → opts { _lroWidth = layoutPageWidth lo })

----------

renderLogWithSeverity ∷ LogR ω
renderLogWithSeverity = renderWithSeverityAnsi

----------

renderLogWithTimestamp ∷ LogR ω
renderLogWithTimestamp = renderWithTimestamp

----------

renderLogWithStackHead ∷ LogR ω
renderLogWithStackHead = renderWithStackHead

----------

renderLogWithCallStack ∷ LogR ω
renderLogWithCallStack = renderWithCallStack

----------

renderLogWithSeverityAndTimestamp ∷ LogR ω
renderLogWithSeverityAndTimestamp = renderWithSeverityAndTimestamp

--------------------

renderLogEntries ∷ (MonoFoldable χ, Element χ ~ LogEntry ω) ⇒
                       LogRenderOpts ω → χ → Doc AnsiStyle
renderLogEntries opts = (⊕ line) ∘ (vsep ∘ fmap (lroRenderer opts) ∘ otoList)

renderIO ∷ (MonadIO μ, MonoFoldable χ, Element χ ~ LogEntry ω) ⇒
           Handle → LogRenderOpts ω → χ → μ ()
renderIO h o =
  liftIO ∘ Terminal.renderIO h ∘ layoutPretty (o ⊣ lroOpts) ∘ renderLogEntries o

{-| Test ANSI rendering; designed to run just to stderr, rather than within
    a Tasty test harness -}
ansiTests ∷ IO ()
ansiTests = let mkle sev = logEntry @[(String,SrcLoc)] @()
                                    [] Nothing sev (pretty $ show sev) ()
                logs = mkle ⊳ [ Emergency, Alert, Critical, Error, Warning
                              , Notice, Informational, Debug ]
             in renderIO stdout lroRenderSev logs

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "MockIO" [ lroRendererTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

{- | Manual tests, designed to be run at the ghci prompt. -}
_testm ∷ IO()
_testm = do
  ansiTests

-- that's all, folks! ----------------------------------------------------------
