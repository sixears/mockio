{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE RankNTypes                 #-}

module StdMain.StdOptions
  ( HasDryRun( dryRun ), HasVerbosity( verbosity ), StdOptions
  , filterVerbosity, options, parseBaseOptions, parseStdOptions, quietitude
  , verbosityLevel
  )
where

import Debug.Trace  ( trace, traceShow )

import Prelude  ( (-), enumFrom, maxBound, minBound, pred, succ, undefined )

-- base --------------------------------

import Control.Applicative  ( many, some, pure )
import Control.Monad        ( Monad, MonadFail, fail, guard, return, sequence )
import Data.Bifunctor       ( bimap, first )
import Data.Bool            ( Bool( True, False ), otherwise )
import Data.Char            ( Char, toLower )
import Data.Either          ( Either( Left, Right ), either )
import Data.Eq              ( Eq )
import Data.Function        ( ($), const, flip, id )
import Data.Functor         ( fmap )
import Data.List            ( filter, intercalate, isPrefixOf )
import Data.Maybe           ( Maybe( Just, Nothing ), fromMaybe, isJust, maybe )
import Data.Ord             ( Ord, (>) )
import Data.String          ( String )
import Data.Tuple           ( fst )
import System.Exit          ( ExitCode )
import System.IO            ( IO )
import Text.Read            ( Read, read, readMaybe )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≤) )

-- containers --------------------------

import qualified  Data.Set         as  Set
import qualified  Data.Map.Strict  as  Map

import Data.Map.Strict  ( (!) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toString )

-- fpath -------------------------------

import FPath.AbsFile    ( AbsFile, absfile )
import FPath.File       ( File( FileA, FileR ) )
import FPath.Parseable  ( parse' )
import FPath.RelFile    ( RelFile, relfile )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log  ( Log, filterSeverity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Alert, Emergency
                                                        , Notice ) )

-- mockio ------------------------------

import MockIO          ( DoMock( DoMock, NoMock ) )
import MockIO.IOClass  ( IOClass( IOCmdR, IOCmdW, IOWrite ), ioClasses )

-- monaderror-io -----------------------

import MonadError2  ( mErrFail )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∈), (∤), (≫), (⊣), (⊳), (⊵), (⋪), (⋫), (⩺)
                         , ю, 𝔹, ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- natural-plus ------------------------

import Natural  ( toEnum, length )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag, flag', help, long, short )

-- parsec -----------------------------

import Text.Parsec.Char        ( alphaNum, char, letter, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( option, optionMaybe, sepBy )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, (<?>), try )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable( parser, parsec' ) )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsLeft, assertRight, runTestsP
                  , runTestsReplay, runTestTree, withResource' )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( Text, pack, splitOn, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.UsageError      ( AsUsageError, UsageError
                               , readUsage, throwUsage, usageError )
import StdMain.VerboseOptions  ( VerboseOptions )

--------------------------------------------------------------------------------


data LogOptions = LogOptions { _verbosity     ∷ ℕ
                             , _quietitude    ∷ ℕ
                             -- lowest passing severity
                             , _verboseOptions ∷ Maybe VerboseOptions
                             }

data BaseOptions = BaseOptions { _verbosity_  ∷ ℕ
                               , _quietitude_ ∷ ℕ
--                               , _log_file   ∷ AbsFile
                               , _dryRun     ∷ DoMock
                               }

class HasBaseOptions α where
  stdOptions ∷ Lens' α BaseOptions

instance HasBaseOptions BaseOptions where
  stdOptions = id

class HasVerbosity α where
  verbosity ∷ Lens' α ℕ
  quietitude ∷ Lens' α ℕ

instance HasVerbosity BaseOptions where
  verbosity = lens _verbosity_ (\ s v → s { _verbosity_ = v })
  quietitude = lens _quietitude_ (\ s q → s { _quietitude_ = q })

class HasDryRun α where
  dryRun ∷ Lens' α DoMock

instance HasDryRun DoMock where
  dryRun = id

instance HasDryRun BaseOptions where
  dryRun = lens _dryRun (\ s d → s { _dryRun = d })

parseBaseOptions ∷ Parser BaseOptions
parseBaseOptions = BaseOptions ⊳ (length ⊳ many (flag' () (short 'v')))
                                ⊵ (length ⊳ many (flag' () (long "quiet")))
                                ⊵ (flag NoMock DoMock (ю [ short 'n'
                                                         , long "dry-run"
                                                         , help "dry run" ]))

data StdOptions α = StdOptions { _a ∷ α, _s ∷ BaseOptions }

instance HasBaseOptions (StdOptions α) where
  stdOptions = lens _s (\ sso s → sso { _s = s })

instance HasDryRun (StdOptions α) where
  dryRun = stdOptions ∘ dryRun

instance HasVerbosity (StdOptions α) where
  verbosity  = stdOptions ∘ verbosity
  quietitude = stdOptions ∘ quietitude

options ∷ Lens' (StdOptions α) α
options = lens _a (\ s a → s { _a = a })

severityNames = [(toLower ⊳ show s,s) | s ← enumFrom Emergency]

parseStdOptions ∷ Parser α → Parser (StdOptions α)
parseStdOptions p = StdOptions ⊳ p ⊵ parseBaseOptions

-- parsecVerboseOpts ∷ Stream σ η Char ⇒ ParsecT σ τ η (Severity, Maybe String)
-- parsecVerboseOpts = (,) ⊳ parsecSeverity ⊵ optionMaybe (string "::" ⋫ many letter)

----------------------------------------

verbosityLevel ∷ (HasVerbosity σ, AsUsageError ε, MonadError ε η) ⇒
                 σ → η Severity
verbosityLevel opts =
  let v = opts ⊣ verbosity
      q = opts ⊣ quietitude
      warnTooLow  x = [fmtT|warning: attempt to exceed min verbosity level %w|] x
      warnTooHigh x = [fmtT|warning: attempt to exceed max verbosity level %w|] x
      succs 0 x = return x
      succs n x | x ≡ maxBound = throwUsage (warnTooHigh x)
                | otherwise    = succs (n-1) (succ x)
      preds 0 x = return x
      preds n x | x ≡ minBound = throwUsage (warnTooLow x)
                | otherwise    = preds (n-1) (pred x)
      l = case v > q of
            True  → succs (v-q) Notice
            False → preds (q-v) Notice
   in l

----------------------------------------

filterVerbosity ∷ ∀ ε η υ ω α σ .
                  (AsUsageError ε, MonadError ε η, MonadLog (Log ω) υ,
                  HasVerbosity σ) ⇒
                  σ → η (LoggingT (Log ω) υ α → υ α)
filterVerbosity stdOpts =
  verbosityLevel stdOpts ≫ return ∘ filterSeverity ∘ flip (≤)

-- that's all, folks! ----------------------------------------------------------
