{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module StdMain
  ( HasDryRun( dryRun ), HasStdOptions( stdOptions ), StdOptions
  , parseStdOptions, quietitude, verbosity )
where

-- base --------------------------------

import Control.Applicative  ( many )
import Data.Function        ( id )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode    ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- lens --------------------------------

import Control.Lens.Lens    ( Lens', lens )

-- mockio ------------------------------

import MockIO  ( DoMock( DoMock, NoMock ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊴), (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Natural      ( ℕ )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, execParser, flag, flag', fullDesc, help
                            , helper, info, long, metavar, progDesc, short
                            , strArgument, strOption
                            )

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

-- that's all, folks! ----------------------------------------------------------
