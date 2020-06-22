{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE RankNTypes        #-}

-- Move/Factor StdOptions into own file
module StdMain.StdOptions
  ( HasDryRun( dryRun ), HasStdOptions( stdOptions ), StdOptions, SuperStdOptions
  , filterVerbosity, options, parseStdOptions, parseSuperStdOptions, quietitude
  , verbosity, verbosityLevel )
where

import Prelude  ( (-), maxBound, minBound, pred, succ )

-- base --------------------------------

import Control.Applicative  ( many )
import Control.Monad        ( return )
import Data.Bool            ( Bool( True, False ), otherwise )
import Data.Function        ( flip, id )
import Data.Ord             ( (>) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Ord.Unicode       ( (≤) )
import Data.Monoid.Unicode    ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log  ( Log, filterSeverity )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Notice ) )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag, flag', help, long, short )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.UsageError  ( AsUsageError, throwUsage )

--------------------------------------------------------------------------------

data StdOptions = StdOptions { _verbosity  ∷ ℕ
                             , _quietitude ∷ ℕ
                             , _dryRun     ∷ DoMock
                             }

class HasStdOptions α where
  stdOptions ∷ Lens' α StdOptions

instance HasStdOptions StdOptions where
  stdOptions = id

class HasVerbosity α where
  verbosity ∷ Lens' α ℕ
  quietitude ∷ Lens' α ℕ

instance HasVerbosity StdOptions where
  verbosity = lens _verbosity (\ s v → s { _verbosity = v })
  quietitude = lens _quietitude (\ s q → s { _quietitude = q })

class HasDryRun α where
  dryRun ∷ Lens' α DoMock

instance HasDryRun DoMock where
  dryRun = id

instance HasDryRun StdOptions where
  dryRun = lens _dryRun (\ s d → s { _dryRun = d })

parseStdOptions ∷ Parser StdOptions
parseStdOptions = StdOptions ⊳ (length ⊳ many (flag' () (short 'v')))
                             ⊵ (length ⊳ many (flag' () (long "quiet")))
                             ⊵ (flag NoMock DoMock (short 'n' ⊕ long "dry-run"
                                                              ⊕ help "dry run"))

data SuperStdOptions α = SuperStdOptions { _a ∷ α, _s ∷ StdOptions }

instance HasStdOptions (SuperStdOptions α) where
  stdOptions = lens _s (\ sso s → sso { _s = s })

instance HasDryRun (SuperStdOptions α) where
  dryRun = stdOptions ∘ dryRun

options ∷ Lens' (SuperStdOptions α) α
options = lens _a (\ s a → s { _a = a })

parseSuperStdOptions ∷ Parser α → Parser (SuperStdOptions α)
parseSuperStdOptions p = SuperStdOptions ⊳ p ⊵ parseStdOptions

----------------------------------------

verbosityLevel ∷ (HasVerbosity σ, AsUsageError ε, MonadError ε η) ⇒
                 σ → η Severity
verbosityLevel opts =
  let v = opts ⊣ verbosity
      q = opts ⊣ quietitude
      warnTooLow  x = [fmt|warning: attempt to exceed min verbosity level %w|] x
      warnTooHigh x = [fmt|warning: attempt to exceed max verbosity level %w|] x
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
                  HasVerbosity σ, HasDryRun σ) ⇒
                  σ → η (LoggingT (Log ω) υ α → υ α)
filterVerbosity stdOpts =
  verbosityLevel stdOpts ≫ return ∘ filterSeverity ∘ flip (≤)

-- that's all, folks! ----------------------------------------------------------
