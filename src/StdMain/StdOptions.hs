{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE UnicodeSyntax          #-}

module StdMain.StdOptions
  ( DryRunLevel, HasDryRun, HasDryRunLevel( dryRunLevel, level )
  , ReadDryRunLevel, StdOptions
  , askDryRunL, dryRunOff, dryRunOn, dryRunP, dryRun1P, dryRun2P
  , ifDryRun, ifDryRunEq, ifDryRunGE, options, parseStdOptions, unlessDryRunGE
  )
where

import Prelude  ( pred, succ )

-- base --------------------------------

import Data.Bool      ( Bool )
import Data.Foldable  ( foldr )
import Data.Function  ( ($),  const, id )
import Text.Show      ( Show( show ) )

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

import Control.Monad.Log  ( Severity( Debug, Warning ) )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∤), (⊳), (⊵), 𝔹, ℕ )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ask )

-- natural-plus ------------------------

import Natural  ( AtMost( Cons, Nil ), Countable( count ), Nat( S ), Natty
                , One, Two, atMost, atMostOne, atMostTwo, count, four, replicate
                , three
                )

-- optparse-applicative ----------------

import Options.Applicative  ( FlagFields, Mod, Parser, flag', help, long,short )

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

------------------------------------------------------------

type ReadDryRunLevel ν η = MonadReader (DryRunLevel ν) η

askDryRunL ∷ ReadDryRunLevel ν η ⇒ η (DryRunLevel ν)
askDryRunL = ask

----------------------------------------

dryRunLvl' ∷ DryRunLevel n → ℕ
dryRunLvl' (DryRunLevel d) = count d

dryRunLvl ∷ HasDryRunLevel n s ⇒ s → ℕ
dryRunLvl = dryRunLvl' ∘ view dryRunLevel

ifDryRunP ∷ ReadDryRunLevel ν η ⇒ (ℕ → 𝔹) → α → α → η α
ifDryRunP f go nogo = (\ drl → ifThenElse (f (dryRunLvl drl)) go nogo) ⊳ ask
  
ifDryRunEq ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
ifDryRunEq i = ifDryRunP (≡ i)

ifDryRunGE ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
ifDryRunGE i = ifDryRunP (≥ i)

ifDryRun ∷ ReadDryRunLevel ν η ⇒ α → α → η α
ifDryRun = ifDryRunGE 1

unlessDryRunGE ∷ ReadDryRunLevel ν η ⇒ ℕ → α → α → η α
unlessDryRunGE i d n = ifDryRunGE i n d

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

{- | Default Severity level; start with Warning, -v goes to Notice then
     Informational. -}
defaultSev ∷ Severity
defaultSev = Warning

----------------------------------------

data StdOptions ν α = StdOptions { _nonBaseOptions ∷ α
                                 , _verboseOptions ∷ VerboseOptions
                                 , _dryRunLevel    ∷ DryRunLevel ν
                                 }

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
  let -- up to n flags, each invoking an instance of a function
      flagn ∷ Natty ν → Mod FlagFields () → (α → α) → α → Parser α
      flagn i m f a =
        (\ c → foldr ($) a (replicate (count c) f)) ⊳ atMost i (flag' () m)
      flagsev ∷ Natty ν → Mod FlagFields () → (Severity → Severity)
              → Parser VerboseOptions
      flagsev i m f = defVOpts ⊳ flagn i m f defaultSev
      flagv         = flagsev three (short 'v' ⊕ help "Increase verbosity.")
                              succ
      flagq         = flagsev four (long "quiet" ⊕ help "Decrease verbosity.")
                              pred
      debug_help    = "Set verbosity to maximum"
      flagd         = defVOpts ⊳ flag' Debug (long "debug" ⊕ help debug_help)
      verbose = parsecOption (long "verbose")
   in StdOptions ⊳ p
                 ⊵ (flagv ∤ flagq ∤ flagd ∤ verbose)
                 ⊵ dryRunP n

-- that's all, folks! ----------------------------------------------------------
