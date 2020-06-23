{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE RankNTypes        #-}

module StdMain.StdOptions
  ( HasDryRun( dryRun ), HasVerbosity( verbosity ), StdOptions
  , filterVerbosity, options, parseBaseOptions, parseStdOptions, quietitude
  , verbosityLevel
  )
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
import Data.MoreUnicode.Monoid       ( ю )
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

data BaseOptions = BaseOptions { _verbosity  ∷ ℕ
                               , _quietitude ∷ ℕ
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
  verbosity = lens _verbosity (\ s v → s { _verbosity = v })
  quietitude = lens _quietitude (\ s q → s { _quietitude = q })

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

parseStdOptions ∷ Parser α → Parser (StdOptions α)
parseStdOptions p = StdOptions ⊳ p ⊵ parseBaseOptions

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
                  HasVerbosity σ) ⇒
                  σ → η (LoggingT (Log ω) υ α → υ α)
filterVerbosity stdOpts =
  verbosityLevel stdOpts ≫ return ∘ filterSeverity ∘ flip (≤)

-- that's all, folks! ----------------------------------------------------------
