{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- Move/Factor StdOptions into own file
module StdMain
  ( stdMain, stdMain' )
where

-- base --------------------------------

import qualified  System.IO

import Control.Exception       ( Exception, throwIO )
import Control.Monad           ( return )
import Control.Monad.Fix       ( mfix )
import Control.Monad.IO.Class  ( MonadIO )
import Data.Either             ( either )
import Data.Foldable           ( and )
import Data.Function           ( ($), id )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String, unwords, words ) 
import System.IO               ( FilePath, Handle, IO )
import Text.Show               ( show )

-- base-unicode-symbols ----------------

import Data.Bool.Unicode      ( (∧) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≥), (≤) )

-- containers --------------------------

import Data.Set  ( fromList, member )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- exited ------------------------------

import qualified  Exited2  as  Exited
import Exited2  ( ToExitCode )

-- fpath -------------------------------

import FPath.AsFilePath2  ( filepath )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log              ( CSOpt( NoCallStack ), Log, filterLog, filterLog'
                        , filterMinSeverity, filterSeverity, logToFile
                        , logToStderr
                        )
import Log.LogEntry     ( LogEntry, attrs, mapPrefixDoc )
import Log.HasSeverity  ( severity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Debug )
                          , discardLogging, mapLogMessage, mapLogMessageM )

-- mockio ------------------------------

import MockIO          ( DoMock( DoMock, NoMock ) )
import MockIO.IOClass  ( HasIOClass, IOClass( IORead, IOWrite ), IOClassSet
                       , (∈), ioClass, member )

-- monad-control -----------------------

import Control.Monad.Trans.Control  ( MonadBaseControl, liftBaseWith, restoreM )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO.File2  ( IOMode( WriteMode ), withFile, withFileT )

-- mono-traversable --------------------

import Data.MonoTraversable  ( omap )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )
import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, runExceptT )
import Control.Monad.Reader  ( ReaderT, runReaderT )

-- natural-plus ------------------------

import Natural  ( Natty, One, one, count )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, footerDoc, progDesc )
import Options.Applicative.Help.Pretty  ( Doc
                                        , (<+>)
                                        , align, fillBreak, fillSep, hang
                                        , hardline, indent, space, string, text
                                        , vcat
                                        )

-- optparse-plus -----------------------

import OptParsePlus2  ( parseOpts )

-- prettyprinter -----------------------

import qualified Data.Text.Prettyprint.Doc  as  PPDoc

-- text --------------------------------

import Data.Text  ( Text, unpack )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.StdOptions      ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                               , StdOptions
                               , ifDryRun, options, parseStdOptions
                               )
import StdMain.UsageError      ( AsUsageError, UsageError, UsageIOError )
import StdMain.VerboseOptions  ( csopt, ioClassFilter, logFile, unLogFile
                               , verboseDesc, verboseOptions )

--------------------------------------------------------------------------------

{- | Like `stdMain`, but gives the incoming `io` full access to the `StdOptions`
     object. -}

stdMain_ ∷ ∀ ε α σ ω ν μ .
           (MonadIO μ, Exception ε, Printable ε, AsUsageError ε, AsIOError ε,
            ToExitCode σ, HasIOClass ω) ⇒
           Natty ν
         → Text
         → Parser α
--         → (StdOptions ν α → LoggingT (Log ω)(LoggingT (Log ω)(ExceptT ε IO)) σ)
         → (StdOptions ν α → LoggingT (Log ω)(ExceptT ε IO) σ)
         → μ ()
stdMain_ n desc p io = do
  let optionDesc ∷ String → [String] → Doc
      optionDesc name descn =
        let para = fillSep $ text ⊳ (words $ unwords descn)
         in indent 2 (fillBreak 14 (string name) <+> align para)
      -- assemble a list of words into a Doc
      mkDoc ∷ [Text] → Doc
      mkDoc = fillSep ∘ fmap (text ∘ unpack)
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

      filter     ∷ (MonadLog (Log ω) η, HasIOClass ω) ⇒ LoggingT (Log ω) η σ → η σ
      filter io  = filterLog' (\ w → and [ sevOpt ≥ w ⊣ severity
                                         , (w ⊣ attrs ∘ ioClass) ∈ ioClasses ])
                              io
      filters    = [ \le → [ mapPrefixDoc (\le → PPDoc.braces (PPDoc.pretty (show $ le ⊣ attrs ∘ ioClass)) ⊕ PPDoc.space) le ]
                   , \le → if le ⊣ severity ≤ sevOpt then [le] else []
                   ]

  
  Exited.doMain $
    case vopts ⊣ logFile of
      Nothing    → logToStderr (vopts ⊣ csopt) filters
                               ({- filter $ -} io o)
      Just logfn → withFileT (unLogFile logfn) WriteMode $ \ h → 
                     logToFile (vopts ⊣ csopt) filters h (io o)


xx ∷ HasIOClass ω ⇒ LogEntry ω → LogEntry ω
xx = mapPrefixDoc (\ e → PPDoc.pretty ∘ show $ e ⊣ (attrs ∘ ioClass))

yy ∷ HasIOClass ω ⇒ Log ω → Log ω
yy = omap xx

zz ∷ (MonadLog (Log ω) η, HasIOClass ω) ⇒ α → η α
zz io = mapLogMessage yy (return io)

ff     ∷ (MonadLog (Log ω) η, HasIOClass ω) ⇒
         Severity → IOClassSet → LoggingT (Log ω) η σ → η σ
ff sevOpt ioClasses io  = {- mapLogMessage id $ -} filterLog' (\ w → and [ sevOpt ≥ w ⊣ severity
                                         , (w ⊣ attrs ∘ ioClass) ∈ ioClasses ])
                              io

-- https://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html#v:liftBaseWith
withFileLifted :: MonadBaseControl IO m => FilePath -> IOMode -> (Handle -> m a) -> m a
withFileLifted file mode action = liftBaseWith (\runInBase -> System.IO.withFile file mode (runInBase ∘ action)) ≫ restoreM
                             -- = control $ \runInBase -> withFile file mode (runInBase . action)
                             -- = liftBaseOp (withFile file mode) action
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
           ToExitCode σ, HasIOClass ω) ⇒
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
           ToExitCode σ, HasIOClass ω) ⇒
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
stdMain' ∷ ∀ ω ρ σ μ . (MonadIO μ, ToExitCode σ, HasIOClass ω) ⇒
           Text
         → Parser ρ
         → (DoMock → ρ → ReaderT (DryRunLevel One) (LogTIO ω UsageIOError) σ)
         → μ ()
stdMain' desc parser io =
  let go opts = do
        mock ← ifDryRun DoMock NoMock
        io mock opts
   in stdMainx one desc parser go

-- that's all, folks! ----------------------------------------------------------
