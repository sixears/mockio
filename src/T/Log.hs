{-# LANGUAGE ImplicitParams    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

-- base --------------------------------

import Control.Monad.Identity  ( runIdentity )
import Data.Foldable           ( toList )
import Data.Function           ( ($) )
import Data.Maybe              ( Maybe( Just, Nothing ) )
import Data.String             ( String )
import Data.Tuple              ( snd )
import GHC.Stack               ( CallStack )
import System.Exit             ( ExitCode )
import System.IO               ( IO )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- logging-effect ----------------------

import qualified  Control.Monad.Log
import Control.Monad.Log  ( MonadLog, Severity( Informational ) )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor   ( (⊳) )
import Data.MoreUnicode.Natural   ( ℕ )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( assertFailure, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( Text, intercalate, replicate )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO  ( Log, WithLog, log, renderLogsSt
               , assertListEq
               )

--------------------------------------------------------------------------------

{- | Log some text (at Informational severity); should produce at least 3 stack
     frames -}
_sf_plus_3 ∷ WithLog μ ⇒ Text → μ ()
_sf_plus_3 t = let -- add an additional callstack to test the formatting
                   _sf_plus_2 ∷ WithLog μ ⇒ μ ()
                   _sf_plus_2 = log Informational t
                in _sf_plus_2

_3sf ∷ MonadLog Log μ ⇒ Maybe Text → μ ()
_3sf Nothing  = _sf_plus_3 "3 stack frames"
_3sf (Just t) = _sf_plus_3 t

_3sf' ∷ WithLog μ ⇒ μ ()
_3sf' = _3sf (Just "3 frames of stack")

_4sf ∷ WithLog μ ⇒ Maybe Text → μ ()
_4sf Nothing  = _sf_plus_3 "4 stack frames"
_4sf (Just t) = _sf_plus_3 t

_4sf' ∷ MonadLog Log μ ⇒ μ ()
_4sf' = _4sf (Just "4 stack frames")

_5sf ∷ WithLog μ ⇒ μ ()
_5sf = _4sf (Just "5+ stack frames")

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests =
  let join = intercalate "\n"
      indent n t = replicate n " " ⊕ t
      indents _ []     = ""
      indents n (t:ts) = join (t:(indent n ⊳ ts))
      exp3sf' =
        [ indents 9 [ "[Info] 3 frames of stack"
                    , "log, called at src/T/Log.hs:67:33 in main:Main"
                    , "  _sf_plus_2, called at src/T/Log.hs:68:20 in main:Main"
                    , "  _sf_plus_3, called at src/T/Log.hs:72:17 in main:Main"
                    ]
        ]
      exp4sf' =
        [ indents 9 [ "[Info] 4 stack frames"
                    , "log, called at src/T/Log.hs:67:33 in main:Main"
                    , "  _sf_plus_2, called at src/T/Log.hs:68:20 in main:Main"
                    , "  _sf_plus_3, called at src/T/Log.hs:79:17 in main:Main"
                    , "  _4sf, called at src/T/Log.hs:82:9 in main:Main"
                    ]
        ]
      exp5sf =
        [ indents 9 [ "[Info] 5+ stack frames"
                    , "log, called at src/T/Log.hs:67:33 in main:Main"
                    , "  _sf_plus_2, called at src/T/Log.hs:68:20 in main:Main"
                    , "  _sf_plus_3, called at src/T/Log.hs:79:17 in main:Main"
                    , "  _4sf, called at src/T/Log.hs:85:8 in main:Main"
                    , "  _5sf, called at src/T/Log.hs:127:62 in main:Main"
                    ]
        ]
   in testGroup "Log"
                [ assertListEq "_3sf'" exp3sf' $
                    toList (runIdentity $ snd ⊳ renderLogsSt _3sf')
                , assertListEq "_4sf'" exp4sf' $
                    toList (runIdentity $ snd ⊳ renderLogsSt _4sf')
                , assertListEq "_5sf" exp5sf $
                    toList (runIdentity $ snd ⊳ renderLogsSt _5sf)
                ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------