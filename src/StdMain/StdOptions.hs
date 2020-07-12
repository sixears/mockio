{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain.StdOptions
  ( HasDryRun( dryRun ), StdOptions, options, parseStdOptions )
where

import Prelude  ( (-), error, maxBound, minBound, pred, succ )

-- base --------------------------------

import Control.Applicative  ( many )
import Control.Monad        ( return )
import Control.Monad.Fail   ( MonadFail, fail )
import Data.Bool            ( Bool( True, False ), otherwise )
import Data.Either          ( Either( Left, Right ) )
import Data.Function        ( ($), (&), flip, id )
import Data.Maybe           ( Maybe( Just, Nothing ), fromMaybe, maybe )
import Data.String          ( String )
import Data.Ord             ( (>) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Ord.Unicode       ( (≤) )

-- data-textual ------------------------

import Data.Textual  ( toString )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log              ( Log, filterSeverity )
import Log.HasSeverity  ( HasSeverity( severity ) )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog, Severity( Notice ) )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∤), (≫), (⊢), (⊣), (⊳), (⊵), ю, ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- natural-plus ------------------------

import Natural  ( length )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag, flag', help, long, optional, short )

-- optparse-plus -------------------------

import OptParsePlus2  ( parsecOption )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import StdMain.VerboseOptions  ( HasVerboseOptions( verboseOptions )
                               , VerboseOptions, defVOpts )

--------------------------------------------------------------------------------

{- | Default Severity level; start with Notice, -v goes to Informational. -}
defaultSev ∷ Severity
defaultSev = Notice

{-
data BaseOptions = BaseOptions { _verboseOptions  ∷ VerboseOptions
                               , _dryRun_         ∷ DoMock
                               }

class HasBaseOptions α where
  stdOptions ∷ Lens' α BaseOptions

instance HasBaseOptions BaseOptions where
  stdOptions = id

instance HasVerboseOptions BaseOptions where
  verboseOptions = lens _verboseOptions (\ bo vo → bo { _verboseOptions = vo })

instance HasSeverity BaseOptions where
  severity = verboseOptions ∘ severity

class HasDryRun α where
  dryRun ∷ Lens' α DoMock

instance HasDryRun DoMock where
  dryRun = id

instance HasDryRun BaseOptions where
  dryRun = lens _dryRun_ (\ s d → s { _dryRun_ = d })

parseBaseOptions ∷ Parser BaseOptions
parseBaseOptions =
  let count m = length ⊳ many (flag' () m)
      vs_qs   = verbosityLevel ⊳ (count (short 'v')) ⊵ (count (long "quiet"))
      verbose = parsecOption (long "verbose")
   in BaseOptions ⊳ (fromMaybe (defVOpts defaultSev) ⊳ optional ((defVOpts ⊳ vs_qs ∤ verbose)))
                  ⊵ (flag NoMock DoMock (ю [ short 'n', long "dry-run"
                                           , help "dry run" ]))

-}

----------------------------------------

-- data StdOptions' α = StdOptions' { _nonBaseOptions ∷ α, _s ∷ BaseOptions }

data StdOptions α = StdOptions { _nonBaseOptions ∷ α
                               , _verboseOptions ∷ VerboseOptions
                               , _dryRun         ∷ DoMock
                               }

class HasDryRun α where
  dryRun ∷ Lens' α DoMock

instance HasDryRun DoMock where
  dryRun = id

-- instance HasBaseOptions (StdOptions α) where
--   stdOptions = lens _s (\ sso s → sso { _s = s })

instance HasDryRun (StdOptions α) where
  dryRun = lens _dryRun (\ s d → s { _dryRun = d })
--  dryRun = stdOptions ∘ dryRun

instance HasVerboseOptions (StdOptions α) where
  verboseOptions = lens _verboseOptions (\ so vo → so { _verboseOptions = vo })

instance HasSeverity (StdOptions α) where
  severity = verboseOptions ∘ severity

options ∷ Lens' (StdOptions α) α
options = lens _nonBaseOptions
               (\ s nonBaseOptions → s { _nonBaseOptions = nonBaseOptions })

-- parseStdOptions ∷ Parser α → Parser (StdOptions α)
-- parseStdOptions p = StdOptions ⊳ p ⊵ parseBaseOptions

parseStdOptions ∷ Parser α → Parser (StdOptions α)
parseStdOptions p =
  let count m = length ⊳ many (flag' () m)
      vs_qs   = verbosityLevel ⊳ (count (short 'v')) ⊵ (count (long "quiet"))
      verbose = parsecOption (long "verbose")
   in StdOptions ⊳ p
                 ⊵ (fromMaybe (defVOpts defaultSev) ⊳ optional ((defVOpts ⊳ vs_qs ∤ verbose)))
                 ⊵ (flag NoMock DoMock (ю [ short 'n', long "dry-run"
                                           , help "dry run" ]))

----------------------------------------

verbosityLevel ∷ ℕ → ℕ → Severity
verbosityLevel v q =
  let warnTooLow  x = [fmt|warning: attempt to exceed min verbosity level %w|] x
      warnTooHigh x = [fmt|warning: attempt to exceed max verbosity level %w|] x
      succs 0 x = x
      succs n x | x ≡ maxBound = error (warnTooHigh x)
                | otherwise    = succs (n-1) (succ x)
      preds 0 x = x
      preds n x | x ≡ minBound = error (warnTooLow x)
                | otherwise    = preds (n-1) (pred x)
      l = case v > q of
            True  → succs (v-q) defaultSev
            False → preds (q-v) defaultSev
   in l

-- that's all, folks! ----------------------------------------------------------
