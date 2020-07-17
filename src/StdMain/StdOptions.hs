{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UnicodeSyntax          #-}

module StdMain.StdOptions
  ( DryRunLevel, HasDryRun, HasDryRunLevel( dryRunLevel, level ), StdOptions
  , dryRunOff, dryRunOn, dryRunP, dryRun1P, dryRun2P
  , ifDryRun, ifDryRunEq, ifDryRunGE, options, parseStdOptions, unlessDryRunGE
  )
where

import Prelude  ( (-), error, maxBound, minBound, pred, succ )

-- base --------------------------------

import Control.Applicative  ( many )
import Data.Bool            ( Bool( True, False ), otherwise )
import Data.Function        ( const, id )
import Data.Maybe           ( fromMaybe )
import Data.Ord             ( (>) )
import Text.Show            ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )
import Data.Ord.Unicode       ( (≥) )

-- lens --------------------------------

import Control.Lens.Getter  ( view )
import Control.Lens.Iso     ( iso )
import Control.Lens.Lens    ( Lens', lens )

-- log-plus ----------------------------

import Log.HasSeverity  ( HasSeverity( severity ) )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Notice ) )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∤), (⊳), (⊵), ℕ )

-- natural-plus ------------------------

import Natural  ( AtMost( Cons, Nil ), Countable( count ), Nat( S ), Natty
                , One, Two, atMost, atMostOne, atMostTwo, count, length )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, flag', help, long, optional, short )

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

{- | Curryable if-then-else; flippety-`flip` of `Data.Bool.bool`. -}
ifThenElse ∷ Bool → a → a → a
ifThenElse b t e = if b then t else e

----------------------------------------

data DryRunN = DryRunN
  deriving Show

newtype DryRunLevel n = DryRunLevel { _level ∷ AtMost n DryRunN }

class HasDryRunLevel n c | c → n where
  dryRunLevel ∷ Lens' c (DryRunLevel n)
  level       ∷ Lens' c (AtMost n DryRunN)
  {-# INLINE level #-}
  level       =  dryRunLevel ∘ level

instance HasDryRunLevel n (DryRunLevel n) where
  dryRunLevel = id
  level       = iso ( \ (DryRunLevel x) → x) DryRunLevel

instance HasDryRunLevel n (AtMost n DryRunN) where
  dryRunLevel = lens DryRunLevel (const ∘ view level)

instance Show (DryRunLevel n) where
  show (DryRunLevel d) = [fmt|DryRun: %d|] (count d)

type DryRun = DryRunLevel One
type HasDryRun = HasDryRunLevel One

instance Countable (DryRunLevel ν) where
  count (DryRunLevel n) = count n

----------------------------------------

dryRunLvl' ∷ DryRunLevel n → ℕ
dryRunLvl' (DryRunLevel d) = count d

dryRunLvl ∷ HasDryRunLevel n s ⇒ s → ℕ
dryRunLvl = dryRunLvl' ∘ view dryRunLevel

ifDryRunEq ∷ HasDryRunLevel n h ⇒ ℕ → h → a → a → a
ifDryRunEq i a = ifThenElse (dryRunLvl a ≡ i)

ifDryRunGE ∷ HasDryRunLevel n h ⇒ ℕ → h → a → a → a
ifDryRunGE i a = ifThenElse (dryRunLvl a ≥ i)

ifDryRun ∷ HasDryRunLevel n h ⇒ h → a → a → a
ifDryRun = ifDryRunGE 1

unlessDryRunGE ∷ HasDryRunLevel n h ⇒ ℕ → h → a → a → a
unlessDryRunGE i a d n = ifDryRunGE i a n d

----------------------------------------

flagDryRun ∷ Parser DryRunN
flagDryRun = flag' DryRunN (long "dry-run" ⊕ help "don't really run")

dryRunOff ∷ DryRunLevel ('S n)
dryRunOff = DryRunLevel Nil

dryRunOn ∷ DryRunLevel ('S n)
dryRunOn  = DryRunLevel (Cons DryRunN Nil)

----------------------------------------

dryRunP ∷ Natty ν → Parser (DryRunLevel ν)
dryRunP n = DryRunLevel ⊳ atMost n flagDryRun

----------

dryRun1P ∷ Parser DryRun
dryRun1P = DryRunLevel ⊳ atMostOne flagDryRun

----------

dryRun2P ∷ Parser (DryRunLevel Two)
dryRun2P = DryRunLevel ⊳ atMostTwo flagDryRun

------------------------------------------------------------

{- | Default Severity level; start with Notice, -v goes to Informational. -}
defaultSev ∷ Severity
defaultSev = Notice

----------------------------------------

data StdOptions ν α = StdOptions { _nonBaseOptions ∷ α
                                 , _verboseOptions ∷ VerboseOptions
                                 , _dryRunLevel    ∷ DryRunLevel ν
                                 }

-- class HasDryRun α where
--  dryRun ∷ Lens' α DoMock

-- instance HasDryRun DoMock where
--   dryRun = id

-- instance HasDryRun (StdOptions ν α) where
--   dryRun = lens _dryRun (\ s d → s { _dryRun = d })

instance HasDryRunLevel ν (StdOptions ν α) where
  dryRunLevel = lens _dryRunLevel (\ so drl → so { _dryRunLevel = drl })

instance HasVerboseOptions (StdOptions ν α) where
  verboseOptions = lens _verboseOptions (\ so vo → so { _verboseOptions = vo })

instance HasSeverity (StdOptions ν α) where
  severity = verboseOptions ∘ severity

options ∷ Lens' (StdOptions ν α) α
options = lens _nonBaseOptions
               (\ s nonBaseOptions → s { _nonBaseOptions = nonBaseOptions })

parseStdOptions ∷ Natty ν → Parser α → Parser (StdOptions ν α)
parseStdOptions n p =
  let countF m = length ⊳ many (flag' () m)
      vs_qs   = verbosityLevel ⊳ (countF (short 'v')) ⊵ (countF (long "quiet"))
      verbose = parsecOption (long "verbose")
   in StdOptions ⊳ p
                 ⊵ (fromMaybe (defVOpts defaultSev) ⊳ optional ((defVOpts ⊳ vs_qs ∤ verbose)))
                 ⊵ dryRunP n

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
