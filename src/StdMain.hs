{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( stdMain, stdMain' )
where

-- base --------------------------------

import Control.Applicative     ( pure )
import Control.Exception       ( Exception )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Either             ( Either( Left, Right ) )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String, unwords, words )
import System.IO               ( IO )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≤) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- exited ------------------------------

import qualified  Exited2  as  Exited
import Exited2  ( ToExitCode )

-- log-plus ----------------------------

import Log              ( Log
                        , logToFile', logFilter, logToStderr', stdRenderers )
import Log.LogEntry     ( LogEntry, attrs, mapPrefixDoc )
import Log.HasSeverity  ( severity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT )

-- mockio ------------------------------

import MockIO               ( DoMock( DoMock, NoMock ), HasDoMock, MockIOClass )
import MockIO.IOClass       ( HasIOClass, (∈), ioClass )
import MockIO.RenderDoMock  ( renderWithDoMock )

-- monaderror-io -----------------------

import MonadError           ( ѥ )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.File2  ( IOMode( WriteMode ), fileWritable, withFileT )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣) )
import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, throwError )
import Control.Monad.Reader  ( ReaderT, runReaderT )

-- natural-plus ------------------------

import Natural  ( Natty, One, one, count )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, footerDoc, progDesc )
import Options.Applicative.Help.Pretty  ( Doc
                                        , (<+>)
                                        , align, fillBreak, fillSep
                                        , indent, string, text
                                        , vcat
                                        )

-- optparse-plus -----------------------

import OptParsePlus2  ( parseOpts )

-- prettyprinter -----------------------

import qualified Data.Text.Prettyprint.Doc  as  PPDoc

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions      ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                               , StdOptions
                               , ifDryRun, options, parseStdOptions
                               )
import StdMain.UsageError      ( AsUsageError, UsageIOError, throwUsage )
import StdMain.VerboseOptions  ( csopt, ioClassFilter, logFile, unLogFile
                               , verboseDesc, verboseOptions )

--------------------------------------------------------------------------------

{- | Like `stdMain`, but gives the incoming `io` full access to the `StdOptions`
     object. -}

stdMain_ ∷ ∀ ε α σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
            ToExitCode σ, HasIOClass ω, HasDoMock ω) ⇒
           Natty ν
         → Text
         → Parser α
         → (StdOptions ν α → LoggingT (Log ω) (ExceptT ε IO) σ)
         → μ ()
stdMain_ n desc p io = do
  let optionDesc ∷ String → [String] → Doc
      optionDesc name descn =
        let para = fillSep $ text ⊳ (words $ unwords descn)
         in indent 2 (fillBreak 14 (string name) <+> align para)
      optionDesc' ∷ String → Doc → Doc
      optionDesc' name para =
        indent 2 (fillBreak 14 (string name) <+> align para)
      footerDesc ∷ Doc
      footerDesc = vcat ([ string "Standard options:"
                         , optionDesc "-v" [ "Increase verbosity.  This may"
                                           , "be used up to 3 times (which is"
                                           , "equivalent to --debug); and is"
                                           , "exclusive with --quiet,"
                                           , "--debug, and --verbose."
                                           ]

                         , optionDesc "--quiet" [ "Decrease verbosity.  This "
                                                , "may be used up to 4 times;"
                                                , "and is exclusive with -v,"
                                                , "--debug, and --verbose."
                                                ]

                         , optionDesc "--debug" [ "Set verbosity to maximum"
                                                , "(debug level).  This option "
                                                , "is exclusive with -v,"
                                                , "--quiet, and --verbose."
                                                ]
                         ] ⊕ case count n of
                               0 → []
                               1 → [ optionDesc "--dry-run"
                                                [ "Do not make any changes; "
                                                , "just pretend."
                                                ]
                                   ]
                               _ → [ optionDesc "--dry-run"
                                                [ "Do not make any changes; "
                                                , "just pretend.  May be used"
                                                , "up to ", show (count n)
                                                , " times."
                                                ]
                                   ]

                         ⊕ [ optionDesc' "--verbose=OPTS" verboseDesc
                         ]
                        )
  o ← parseOpts Nothing (progDesc (toString desc) ⊕ footerDoc (Just footerDesc))
                        (parseStdOptions n p)
  let vopts      = o ⊣ verboseOptions
      ioClasses  = vopts ⊣ ioClassFilter
      sevOpt     = o ⊣ severity
      renderers  = renderWithDoMock : stdRenderers (vopts ⊣ csopt)

      prefixIOC ∷ ∀ α β . HasIOClass α ⇒ LogEntry α → PPDoc.Doc β
      prefixIOC le =
        PPDoc.braces (PPDoc.pretty ∘ toText $ le ⊣ attrs∘ioClass) ⊕ PPDoc.space
      filters    = [
        -- XXX ONLY IF SELECTED IN OPTIONS
                     pure ∘ mapPrefixDoc prefixIOC
                   , \ le → if le ⊣ severity ≤ sevOpt then [le] else []
                   , logFilter (\ le → (le ⊣ attrs ∘ ioClass) ∈ ioClasses)
                   ]
      logIOToFile i o' h = logToFile' renderers filters h (i o')


  Exited.doMain $
    case vopts ⊣ logFile of
      Nothing    → logToStderr' renderers filters (io o)
      Just logfn → ѥ (fileWritable (unLogFile logfn)) ≫ \ case 
                     Left e         → throwError e
                     Right (Just e) → throwUsage $ "bad log file: " ⊕ e
                     Right Nothing  → withFileT (unLogFile logfn) WriteMode $
                                        logIOToFile io o


----------

{- | Execute the 'main' of a standard program with standard options that returns
     a toExitCode, that may throw exceptions; logging as requested by cmdline
     options.

     The `LoggingT (Log ω) (LoggingT (Log ω) (ExceptT ε IO)) α` is satisfied by,
     e.g.,
     `MonadLog (Log IOClass) μ, MonadIO μ, MonadError ε μ, AsUsageError ε) ⇒ μ α`
     though quite honestly, I couldn't say why the double `Logging`.
 -}
stdMain ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           ToExitCode σ, HasIOClass ω, HasDoMock ω) ⇒
          Natty ν
        → Text
        → Parser α
        → (DryRunLevel ν → α → LoggingT (Log ω) (ExceptT ε IO) σ)
        → μ ()
stdMain n desc p io =
  stdMain_ n desc p (\ o → io (o ⊣ dryRunLevel) (o ⊣ options))

type LogTIO ω ε = (LoggingT (Log ω) (ExceptT ε IO))

stdMainx ∷ ∀ ε α σ ω ν μ .
          (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
           ToExitCode σ, HasIOClass ω, HasDoMock ω) ⇒
          Natty ν
        → Text
        → Parser α
        → (α → ReaderT (DryRunLevel ν) (LogTIO ω ε) σ)
        → μ ()
stdMainx n desc p io =
  stdMain_ n desc p (\ o → runReaderT (io (o ⊣ options)) (o ⊣ dryRunLevel))

----------

{- | More simpley-typed version of `stdMain`, where the error is specifically a
     `UsageIOError`, and there is a single dry-run level which is translated to
     DoMock/NoMock; intended for simple IO programs.

     Note that although the `io` arg. is typed to a `ReaderT`, much simpler
     types - e.g., `MonadIO ⇒ μ ()`, or `MonadIO ⇒ μ ExitCode` - will suffice.
 -}
stdMain' ∷ ∀ ρ σ μ . (MonadIO μ, ToExitCode σ) ⇒
           Text
         → Parser ρ
         → (DoMock → ρ → ReaderT (DryRunLevel One) (LogTIO MockIOClass UsageIOError) σ)
         → μ ()
stdMain' desc parser io =
  let go opts = do
        mock ← ifDryRun DoMock NoMock
        io mock opts
   in stdMainx one desc parser go

-- that's all, folks! ----------------------------------------------------------
