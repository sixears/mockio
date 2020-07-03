{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ViewPatterns               #-}

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

import Data.MoreUnicode.Applicative  ( (∤), (⊵), (⋪), (⋫) )
import Data.MoreUnicode.Bool         ( 𝔹 )
import Data.MoreUnicode.Functor      ( (⊳), (⩺) )
import Data.MoreUnicode.Lens         ( (⊣) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ю )
import Data.MoreUnicode.Natural      ( ℕ )

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

import ParsecPlus2( Parsecable( parser, parsec' ) )

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

import StdMain.UsageError  ( AsUsageError, UsageError, throwUsage, usageError )

--------------------------------------------------------------------------------

data VerboseOptions =
  VerboseOptions { _logSeverity   ∷ Severity -- ^ lowest passing severity
                 , _ioClassFilter ∷ Set.Set IOClass
                 , _config        ∷ Map.Map Text Text
                 , _logFile       ∷ Maybe LogFile
                 }
  deriving (Eq,Show)

instance Printable VerboseOptions where
  print (VerboseOptions sev ioclasses cfg Nothing) =
    P.text $ [fmt|%w-[%L]-[%L]|] sev ioclasses cfg
  print v@(VerboseOptions sev ioclasses cfg (Just logfile)) =
    P.text $ [fmt|%w-[%L]-[%w]-%T|] sev ioclasses cfg logfile

(∈) ∷ ∀ κ α . Ord κ ⇒ κ → Map.Map κ α → 𝔹
(∈) = Map.member

readUsage ∷ ∀ τ ε ω η . (AsUsageError ε, MonadError ε η, Read ω, Printable τ) ⇒
            τ → η ω
readUsage s = let errMsg = [fmtT|failed to parse: '%T'|] s
               in maybe (throwUsage $ errMsg) return (readMaybe $ toString s)

parseKV ∷ Stream σ η Char ⇒ ParsecT σ τ η (Text,Text)
parseKV = bimap pack pack ⊳ ((,) ⊳ some (alphaNum ∤ oneOf "_-") ⋪ char '=' ⊵ many (alphaNum ∤ oneOf "_-") )

parseCfgs  ∷ [(Text,Text)] → ParsecT σ τ η (Set.Set IOClass, Map.Map Text Text)
parseCfgs = mErrFail ∘ fmap (first $ fromMaybe ioClasses) ∘ parseCfgs_

type CfgsParse = (Maybe (Set.Set IOClass), Map.Map Text Text)

parseCfgs_ ∷ [(Text,Text)] → Either UsageError CfgsParse
parseCfgs_ = parseCfgs__ (Nothing,Map.empty)

-- recursively parse a set of config pairs
parseCfgs__ ∷ (AsUsageError ε, MonadError ε η) ⇒
              CfgsParse → [(Text,Text)] → η CfgsParse
parseCfgs__ (iocs,cfg) [] = return (iocs,cfg)
parseCfgs__ (iocs,cfg) ((Text.toLower → k,v) : more) =
  case k of
    -- duplicate of ioclasses
    "ioclass"   → case iocs of
                    Nothing    → do let class_txts = splitOn "," v
                                    classes ← sequence $ readUsage ⊳ class_txts
                                    let iocs' = Just $ Set.fromList classes
                                    parseCfgs__ (iocs', cfg) more
                    Just iocs_ →  throwUsage $ e_iocs_defined iocs_
    -- duplicate of ioclass
    "ioclasses" → case iocs of
                    Nothing    → do let class_txts = splitOn "," v
                                    classes ← sequence $ readUsage ⊳ class_txts
                                    let iocs' = Just $ Set.fromList classes
                                    parseCfgs__ (iocs', cfg) more
                    Just iocs_ →  throwUsage $ e_iocs_defined iocs_
    _           → if k ∈ cfg
                  then throwUsage $ e_config_defined cfg k v
                  else parseCfgs__ (iocs,Map.insert k v cfg) more
  where e_iocs_defined   iocs_   = [fmtT|IOClasses already defined: ⟨%L⟩|] iocs_
        e_config_defined cfg k v = [fmtT|config %t already defined: '%t' (%t)|]
                                   k (cfg ! k) v
  
parseCfgsTests ∷ TestTree
parseCfgsTests =
  let test name exp (input_texts, input_values) =
        testCase name $
          assertRight (exp @=?)(parseCfgs__ @UsageError input_values input_texts)
      testErr name (input_texts, input_values) =
        testCase name $
          assertIsLeft (parseCfgs__ @UsageError input_values input_texts)
      iocsText = pack $ intercalate "," (show ⊳ Set.toList ioClasses)
   in testGroup "parseCfgs"
            [ test "empty" (Just ioClasses,Map.empty)
                           ([], (Just ioClasses,Map.empty))
            , test "just one ioclass"
                   (Just $ Set.fromList [IOCmdW],Map.empty)
                   ([("ioClasses", "IOCmdW")],
                   (Nothing,Map.empty))
            , test "just ioclasses"
                   (Just ioClasses,Map.empty)
                   ([("ioClasses", iocsText)], (Nothing,Map.empty))
            , testErr "more ioclasses"
                      ([("ioClasses", iocsText)], (Just Set.empty,Map.empty))
            , test "foobar"
                   (Nothing,Map.fromList [("foo","bar")])
                   ([("foo","bar")], (Nothing,Map.empty))
            , testErr "foobar again"
                      ([("foo","bar")], (Nothing,Map.fromList [("foo","baz")]))
            , test "ioclasses + config"
                   (Just $ Set.fromList [IOCmdW,IOCmdR],
                    Map.fromList [("foo","bar"),("baz","quux")])
                   ([ ("foo","bar")
                    , ("ioclasses","IOCmdWrite,IOCmdR")
                    , ("baz","quux") ], (Nothing,Map.empty))
            ]

parseIOClassesCfg ∷ Stream σ η Char ⇒ ParsecT σ τ η (Set.Set IOClass,Map.Map Text Text)
-- parseCfgs = undefined
-- parseIOClassesCfg = option (ioClasses,Map.empty) $ parseCfgs ⊳ (char '{' ⋫ (parseKV `sepBy` (char '^')) ⋪ char '}')
parseIOClassesCfg = option (ioClasses,Map.empty) $ (char '{' ⋫ (parseKV `sepBy` (char '^')) ⋪ char '}') ≫ parseCfgs

instance Parsecable VerboseOptions where
  parser =
    let mkVO ∷ Severity → Maybe ((Set.Set IOClass,Map.Map Text Text),LogFile)
                        → VerboseOptions
        mkVO sev Nothing          = VerboseOptions sev ioClasses
                                                   Map.empty Nothing
        mkVO sev (Just ((iocs,cfgs),fn)) = VerboseOptions sev iocs
                                                   cfgs (Just fn)
     in mkVO ⊳ parsecSeverity ⊵ optionMaybe ((,) ⊳ (char ':' ⋫ (parseIOClassesCfg) ⋪ char ':') ⊵ parser)

parseVerboseOptionsTests ∷ TestTree
parseVerboseOptionsTests =
  let test exp txt =
        testCase (unpack txt) $ assertRight (exp ≟) (parsec' txt txt)
      testErr txt =
        testCase (unpack txt) $ assertIsLeft (parsec' @VerboseOptions txt txt)
      tmplog = LogFile (FileA [absfile|/tmp/log|])
      logtmp = LogFile (FileR [relfile|log:tmp|])
   in testGroup "parseVerboseOptions"
            [ test (VerboseOptions Alert ioClasses Map.empty Nothing) "1"
            , test (VerboseOptions Alert ioClasses Map.empty (Just tmplog))
                   "1::/tmp/log"
            , test (VerboseOptions Alert (Set.fromList [IOWrite])
                                   Map.empty (Just logtmp))
                   "1:{ioclasses=iowrite}:log:tmp"
            , testErr "1:deliberately!!bad:log:tmp"
            ]

newtype LogFile = LogFile File
  deriving (Eq,Printable,Show)

instance Parsecable LogFile where
  parser = LogFile ⊳ (some (noneOf "\0") ≫ mErrFail ∘ parse')

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

prefixesSeverity (fmap toLower → t) =
  filter ((t `isPrefixOf`) ∘ fst) severityNames

uniquelyPrefixesSeverity (prefixesSeverity → [(_,s)]) = Just s
uniquelyPrefixesSeverity _                            = Nothing

parsecSeverityN ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverityN = toEnum ∘ read ∘ pure ⊳ oneOf "01234567"

parsecSeverityS ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverityS = do
  s ← some letter
  let u = uniquelyPrefixesSeverity s
  case uniquelyPrefixesSeverity s of
    Just u  → return u
    Nothing → fail ("severity '" ⊕ s ⊕ "' not recognized")

parsecSeverity ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverity = try parsecSeverityN ∤ parsecSeverityS

parseStdOptions ∷ Parser α → Parser (StdOptions α)
parseStdOptions p = StdOptions ⊳ p ⊵ parseBaseOptions

parsecVerboseOpts ∷ Stream σ η Char ⇒ ParsecT σ τ η (Severity, Maybe String)
parsecVerboseOpts = (,) ⊳ parsecSeverity ⊵ optionMaybe (string "::" ⋫ many letter)

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

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "StdOptions" [ parseCfgsTests, parseVerboseOptionsTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
