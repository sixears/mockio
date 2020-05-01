{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.LogRenderOpts
  ( LogAnnotator, LogRenderOpts

  , logRenderOpts'

  , lroOpts, lroRenderPlain, lroRenderSevCS, lroRenderSevCSH
  , lroRenderTSSev, lroRenderTSSevCS, lroRenderTSSevCSH
  , lroRenderer, lroRendererAnsi, lroWidth

  , renderLogWithCallStack, renderLogWithSeverity, renderLogWithStackHead
  , renderLogWithTimestamp

  , tests
  )
where

-- base --------------------------------

import Data.Foldable  ( Foldable( foldr ) )
import Data.Function  ( ($), flip, id )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import Data.Tuple     ( snd )
import GHC.Stack      ( SrcLoc
                      , getCallStack, prettySrcLoc, srcLocFile,srcLocStartLine )
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
import Data.MoreUnicode.Monoid   ( ф, ю )
import Data.MoreUnicode.Natural  ( ℕ )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, LayoutOptions( LayoutOptions )
                                  , PageWidth( Unbounded )
                                  , (<+>)
                                  , align, annotate, brackets
                                  , defaultLayoutOptions, emptyDoc, indent
                                  , layoutPageWidth, layoutPretty, line, pretty
                                  , reAnnotate, vsep
                                  )
import Data.Text.Prettyprint.Doc.Render.Terminal
                                  ( AnsiStyle, Color( Black, Green, Red, White
                                                    , Yellow )
                                  , bgColor, bgColorDull, bold, color
                                  , renderIO, underlined
                                  )
import Data.Text.Prettyprint.Doc.Render.Text
                                  ( renderStrict )

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
import Log.LogEntry      ( LogEntry, logEntry', _le0 )

--------------------------------------------------------------------------------

type LogR α = (LogEntry → Doc α) → LogEntry → Doc α
data LogAnnotator = LogAnnotator { _renderTTY   ∷ LogR AnsiStyle
                                 , _renderNoTTY ∷ LogR ()
                                 }

newtype LogRenderer = LogRenderer { unLogRenderer ∷ [LogAnnotator] }

type instance Element LogRenderer = LogAnnotator
instance MonoFunctor LogRenderer where
  omap f (LogRenderer ls) = LogRenderer (f ⊳ ls)


class HasLogRenderer α where
  logRenderer ∷ Lens' α LogRenderer

instance HasLogRenderer LogRenderer where
  logRenderer = id

data LogRenderOpts =
  LogRenderOpts { {-| List of log renderers; applied in list order, tail of the
                      list first; hence, given that many renderers add something
                      to the LHS, the head of the list would be lefthand-most in
                      the resulting output.
                   -}
                  _lroRenderers  ∷ LogRenderer
                , _lroWidth      ∷ PageWidth
                }

instance Default LogRenderOpts where
  def = LogRenderOpts (LogRenderer []) Unbounded

instance HasLogRenderer LogRenderOpts where
  logRenderer ∷ Lens' LogRenderOpts LogRenderer
  logRenderer = lens _lroRenderers (\ opts rs → opts { _lroRenderers = rs })

logRenderOpts' ∷ [LogAnnotator] → PageWidth → LogRenderOpts
logRenderOpts' as w = LogRenderOpts (LogRenderer as) w

{- | `LogRenderOpts` with no adornments.  Page width is `Unbounded`; you can
     override this with lens syntax, e.g.,

     > lroRenderPlain & lroWidth .~ AvailablePerLine 80 1.0
 -}
lroRenderPlain ∷ LogRenderOpts
lroRenderPlain = logRenderOpts' [] Unbounded

{- | `LogRenderOpts` with severity. -}
lroRenderSev ∷ LogRenderOpts
lroRenderSev = logRenderOpts' [ renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp & severity. -}
lroRenderTSSev ∷ LogRenderOpts
lroRenderTSSev =
  logRenderOpts' [ renderLogWithTimestamp, renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with severity & callstack. -}
lroRenderSevCS ∷ LogRenderOpts
lroRenderSevCS =
  logRenderOpts' [ renderLogWithCallStack, renderLogWithSeverity ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack. -}
lroRenderTSSevCS ∷ LogRenderOpts
lroRenderTSSevCS =
  logRenderOpts' [ renderLogWithCallStack,renderLogWithTimestamp
                 , renderLogWithSeverity ]
                Unbounded

{- | `LogRenderOpts` with severity & callstack head. -}
lroRenderSevCSH ∷ LogRenderOpts
lroRenderSevCSH =
  logRenderOpts' [ renderLogWithSeverity, renderLogWithStackHead ] Unbounded

{- | `LogRenderOpts` with timestamp, severity & callstack head.
 -}
lroRenderTSSevCSH ∷ LogRenderOpts
lroRenderTSSevCSH =
  logRenderOpts' [ renderLogWithTimestamp, renderLogWithSeverity
                 , renderLogWithStackHead ]
                Unbounded

{- | A single renderer for a LogEntry, using the renderers selected in
     LogRenderOpts. -}
lroRenderer ∷ LogRenderOpts → LogEntry → Doc ()
lroRenderer opts =
  let foldf ∷ Foldable ψ ⇒ ψ (α → α) → α → α
      foldf = flip (foldr ($))
   in foldf (_renderNoTTY ⊳ unLogRenderer (opts ⊣ logRenderer))
            (view LogEntry.doc)

{- | A single renderer for a LogEntry, using the renderers selected in
     LogRenderOpts; this one produces doc with AnsiStyle annotations for
     rendering to an ANSI-compatible terminal.
-}
lroRendererAnsi ∷ LogRenderOpts → LogEntry → Doc AnsiStyle
lroRendererAnsi opts =
  let foldf ∷ Foldable ψ ⇒ ψ (α → α) → α → α
      foldf = flip (foldr ($))
   in foldf (_renderTTY ⊳ unLogRenderer (opts ⊣ logRenderer))
            (reAnnotate ф ∘ view LogEntry.doc)

lroRendererTests ∷ TestTree
lroRendererTests =
  let renderDoc ∷ Doc α → Text
      renderDoc = renderStrict ∘ layoutPretty defaultLayoutOptions

      check nme exp rs = let opts     = logRenderOpts' rs Unbounded
                             rendered = lroRenderer opts _le0
                          in testCase nme $ exp ≟ renderDoc rendered
      checks nme exp rs = let opts     = logRenderOpts' rs Unbounded
                              rendered = lroRenderer opts _le0
                           in assertListEq nme exp (T.lines $renderDoc rendered)
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

lroWidth ∷ Lens' LogRenderOpts PageWidth
lroWidth = lens _lroWidth (\ opts w → opts { _lroWidth = w })

lroOpts ∷ Lens' LogRenderOpts LayoutOptions
lroOpts = lens (LayoutOptions ∘ _lroWidth)
               (\ opts lo → opts { _lroWidth = layoutPageWidth lo })

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

renderWithSeverityAnsi_ ∷ HasSeverity τ ⇒ τ → Doc AnsiStyle
renderWithSeverityAnsi_ m =
  let red = color Red
      yellow = color Yellow
      green  = color Green
      white  = color White
      black  = color Black
      b = bold
      u = underlined
      bgBlack = bgColorDull Black
      bgRed   = bgColor     Red
      anno ls = annotate (ю (bgBlack : ls))
      annoR ls = annotate (ю (bgRed : ls))
      pp ∷ HasSeverity α ⇒ α → Doc AnsiStyle
      pp sv = case sv ⊣ severity of
                         Emergency     → annoR [b,u,black] "EMRG"
                         Alert         → anno  [b,u,red] "ALRT"
                         Critical      → anno  [red] "CRIT"
                         Error         → anno  [red] "Erro"
                         Warning       → anno  [yellow] "Warn"
                         Notice        → anno  [u,green] "Note"
                         Informational → anno  [green] "Info"
                         Debug         → anno  [white] "Debg"
   in pp m

renderWithSeverityAnsi ∷ HasSeverity τ ⇒ (τ → Doc AnsiStyle) → τ → Doc AnsiStyle
renderWithSeverityAnsi f m = brackets (renderWithSeverityAnsi_ m) ⊞ align (f m)

renderWithSeverity ∷ HasSeverity τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithSeverity f m = brackets (renderWithSeverity_ m) ⊞ align (f m)

renderLogWithSeverity ∷ LogAnnotator
renderLogWithSeverity = LogAnnotator renderWithSeverityAnsi renderWithSeverity

renderWithSeverityAndTimestamp ∷ (HasSeverity τ, HasUTCTimeY τ) ⇒
                                 (τ → Doc ρ) → τ → Doc ρ
renderWithSeverityAndTimestamp f m =
  brackets (renderWithTimestamp_ m ⊕ "|" ⊕ renderWithSeverity_ m) ⊞ align (f m)

renderLogWithSeverityAndTimestamp ∷ LogAnnotator
renderLogWithSeverityAndTimestamp =
  LogAnnotator renderWithSeverityAndTimestamp renderWithSeverityAndTimestamp

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

renderLogWithCallStack ∷ LogAnnotator
renderLogWithCallStack = LogAnnotator renderWithCallStack renderWithCallStack

renderWithStackHead ∷ HasCallstack δ ⇒ (δ -> Doc ρ) -> δ -> Doc ρ
renderWithStackHead f m =
  let renderStackHead = renderLocation ∘ fmap snd
   in renderStackHead (stackHead m) ⊞ align (f m)

renderLogWithStackHead ∷ LogAnnotator
renderLogWithStackHead = LogAnnotator renderWithStackHead renderWithStackHead

renderWithTimestamp_  ∷ HasUTCTimeY τ ⇒ τ → Doc ρ
renderWithTimestamp_ m = pretty (formatUTCYDoW $ m ⊣ utcTimeY)

renderWithTimestamp ∷ HasUTCTimeY τ ⇒ (τ → Doc ρ) → τ → Doc ρ
renderWithTimestamp f m = brackets (renderWithTimestamp_ m) ⊞ align (f m)

renderLogWithTimestamp ∷ LogAnnotator
renderLogWithTimestamp = LogAnnotator renderWithTimestamp renderWithTimestamp

renderLogEntriesAnsi ∷ (MonoFoldable χ, Element χ ~ LogEntry) ⇒
                       LogRenderOpts → χ → Doc AnsiStyle
renderLogEntriesAnsi opts =
  (⊕ line) ∘ (vsep ∘ fmap (lroRendererAnsi opts) ∘ otoList)

renderIOAnsi ∷ (MonadIO μ, MonoFoldable χ, Element χ ~ LogEntry) ⇒
               Handle → LogRenderOpts → χ → μ ()
renderIOAnsi h o =
  liftIO ∘ renderIO h ∘ layoutPretty (o ⊣ lroOpts) ∘ renderLogEntriesAnsi o

{-| Test ANSI rendering; designed to run just to stderr, rather than within
    a Tasty test harness -}
ansiTests ∷ IO ()
ansiTests = let mkle sev = logEntry' [] Nothing sev (pretty $ show sev)
                logs = mkle ⊳ [ Emergency, Alert, Critical, Error, Warning
                              , Notice, Informational, Debug ]
             in renderIOAnsi stdout lroRenderSev logs

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
