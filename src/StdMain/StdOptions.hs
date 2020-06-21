{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE ViewPatterns      #-}

-- Move/Factor StdOptions into own file
module StdMain.StdOptions
  ( HasDryRun( dryRun ), HasStdOptions( stdOptions ), StdOptions
  , filterVerbosity, parseStdOptions, quietitude, verbosity, verbosityLevel )
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

import Control.Lens.Getter  ( view )
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

verbosity ∷ Lens' StdOptions ℕ
verbosity = lens _verbosity (\ s v → s { _verbosity = v })

quietitude ∷ Lens' StdOptions ℕ
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

----------------------------------------

verbosityLevel ∷ (HasStdOptions σ, AsUsageError ε, MonadError ε η) ⇒
                 σ → η Severity
verbosityLevel (view stdOptions → opts) =
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

filterVerbosity ∷ ∀ ε η υ ω α .
                  (AsUsageError ε, MonadError ε η, MonadLog (Log ω) υ) ⇒
                  StdOptions → η (LoggingT (Log ω) υ α → υ α)
filterVerbosity stdOpts =
  verbosityLevel stdOpts ≫ return ∘ filterSeverity ∘ flip (≤)

-- that's all, folks! ----------------------------------------------------------
