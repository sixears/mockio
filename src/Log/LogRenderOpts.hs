{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.LogRenderOpts
  ( LogRenderOpts

  , lroOpts, lroRenderPlain, lroRenderSevCS, lroRenderSevCSH
  , lroRenderTSSev, lroRenderTSSevCS, lroRenderTSSevCSH
  , lroRenderer, lroRenderers, lroWidth

  , renderWithCallStack, renderWithSeverity, renderWithStackHead
  , renderWithTimestamp
  , tests
  )
where

-- base --------------------------------

import Data.Foldable  ( Foldable( foldr ) )
import Data.Function  ( ($), flip )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Data.Tuple     ( snd )
import GHC.Stack      ( SrcLoc
                      , getCallStack, prettySrcLoc, srcLocFile,srcLocStartLine )
import System.Exit    ( ExitCode )
import System.IO      ( IO )

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

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth( Unbounded )
                                  , (<+>)
                                  , align, brackets, defaultLayoutOptions
                                  , emptyDoc, indent, layoutPageWidth
                                  , layoutPretty, line, pretty, vsep
                                  )
import Data.Text.Prettyprint.Doc.Render.Text  ( renderStrict )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( testCase )

-- tasty-plus --------------------------

import TastyPlus   ( (≟), runTestsP, runTestsReplay, runTestTree )
import TastyPlus2  ( assertListEq )

-- text --------------------------------

import qualified  Data.Text       as  T
import qualified  Data.Text.Lazy  as  LT

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt2  ( formatUTCYDoW, fmt )

------------------------------------------------------------
--                     Local Imports                      --
------------------------------------------------------------

import qualified  Log.LogEntry  as  LogEntry

import Log.HasCallstack  ( HasCallstack( callstack ), stackHead )
import Log.HasSeverity   ( HasSeverity( severity ) )
import Log.HasUTCTime    ( HasUTCTimeY( utcTimeY ), )
import Log.LogEntry      ( LogEntry, _le0 )

--------------------------------------------------------------------------------

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

{- | `LogRenderOpts` with timestamp & severity. -}
lroRenderTSSev ∷ LogRenderOpts
lroRenderTSSev =
  LogRenderOpts [ renderWithTimestamp, renderWithSeverity ] Unbounded

{- | `LogRenderOpts` with severity & callstack. -}
lroRenderSevCS ∷ LogRenderOpts
lroRenderSevCS =
  LogRenderOpts [ renderWithCallStack, renderWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack. -}
lroRenderTSSevCS ∷ LogRenderOpts
lroRenderTSSevCS =
  LogRenderOpts [ renderWithCallStack,renderWithTimestamp, renderWithSeverity ]
                Unbounded

{- | `LogRenderOpts` with severity & callstack head. -}
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
  let renderDoc ∷ Doc α → Text
      renderDoc = renderStrict ∘ layoutPretty defaultLayoutOptions

      check nme exp rs = let opts     = LogRenderOpts rs Unbounded
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
lroOpts = lens (LayoutOptions ∘ _lroWidth)
               (\ opts lo → opts { _lroWidth = layoutPageWidth lo })

instance Default LogRenderOpts where
  def = LogRenderOpts [] Unbounded

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

locToText ∷ SrcLoc → Text
locToText loc = [fmt|«%s#%w»|] (srcLocFile loc) (srcLocStartLine loc)
                   
renderLocation ∷ Maybe SrcLoc → Doc α
renderLocation (Just loc) = pretty $ locToText loc
renderLocation Nothing    = emptyDoc

renderWithSeverity_ ∷ HasSeverity τ ⇒ τ → Doc ρ
renderWithSeverity_ m =
  let pp ∷ HasSeverity α ⇒ α → Doc ann
      pp sv = pretty $ case sv ⊣ severity of
                         Emergency     → ("EMRG" ∷ Text)
                         Alert         → "ALRT"
                         Critical      → "CRIT"
                         Error         → "Erro"
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
  brackets (renderWithTimestamp_ m ⊕ "|" ⊕ renderWithSeverity_ m) ⊞ align (f m)

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

renderWithTimestamp_  ∷ HasUTCTimeY τ ⇒ τ → Doc ρ
renderWithTimestamp_ m = pretty (formatUTCYDoW $ m ⊣ utcTimeY)

renderWithTimestamp ∷ HasUTCTimeY τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithTimestamp f m = brackets (renderWithTimestamp_ m) ⊞ align (f m)

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

-- that's all, folks! ----------------------------------------------------------
