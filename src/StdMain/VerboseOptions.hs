{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module StdMain.VerboseOptions
  ( LogFile, HasVerboseOptions( verboseOptions ), VerboseOptions, defVOpts )
where

import Prelude  ( enumFrom )

-- base --------------------------------

import Control.Applicative    ( many, some, pure )
import Control.Monad          ( return, sequence )
import Data.Bifunctor         ( bimap, first )
import Data.Char              ( Char, toLower )
import Data.Either            ( Either )
import Data.Eq                ( Eq )
import Data.Function          ( ($), id )
import Data.Functor           ( fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( intercalate )
import Data.Maybe             ( Maybe( Just, Nothing ), fromMaybe )
import Data.String            ( String )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.Read              ( read )
import Text.Show              ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- containers --------------------------

import qualified  Data.Set         as  Set
import qualified  Data.Map.Strict  as  Map

import Data.Map.Strict  ( (!) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath -------------------------------

import FPath.AbsFile    ( absfile )
import FPath.File       ( File( FileA, FileR ) )
import FPath.Parseable  ( parse' )
import FPath.RelFile    ( relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- log-plus ----------------------------

import Log.HasSeverity  ( HasSeverity( severity ) )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Alert, Emergency, Notice ) )

-- mockio ------------------------------

import MockIO.IOClass  ( IOClass( IOCmdR, IOCmdW, IORead, IOWrite ), ioClasses )

-- monaderror-io -----------------------

import MonadError2  ( mErrFail )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∈), (∤), (≫), (⊳), (⊵), (⋪), (⋫), ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- natural-plus ------------------------

import Natural  ( toEnum )

-- parsec -----------------------------

import Text.Parsec.Char        ( alphaNum, char, letter, noneOf, oneOf )
import Text.Parsec.Combinator  ( between, option, optionMaybe, sepBy )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, try )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable( parser, parsec' ), uniquePrefix )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), assertIsLeft, assertRight, runTestsP, runTestsReplay
                  , runTestTree )

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

import StdMain.UsageError   ( AsUsageError, UsageError, readUsage, throwUsage )

--------------------------------------------------------------------------------

newtype LogFile = LogFile File
  deriving (Eq,Printable,Show)

instance Parsecable LogFile where
  parser = LogFile ⊳ (some (noneOf "\0") ≫ mErrFail ∘ parse')

------------------------------------------------------------

type IOClassSet = Set.Set IOClass
type TextMap    = Map.Map Text Text

------------------------------------------------------------

data VerboseOptions =
  VerboseOptions { _logSeverity   ∷ Severity -- ^ lowest passing severity
                 , _ioClassFilter ∷ IOClassSet
                 , _config        ∷ TextMap
                 , _logFile       ∷ Maybe LogFile
                 }
  deriving (Eq,Show)

class HasVerboseOptions α where
  verboseOptions ∷ Lens' α VerboseOptions

instance HasVerboseOptions VerboseOptions where
  verboseOptions = id

----------------------------------------

instance Printable VerboseOptions where
  -- just for easier visibility during debugging
  print (VerboseOptions sev ioclasses cfg Nothing) =
    P.text $ [fmt|%w-[%L]-[%L]|] sev ioclasses cfg
  print (VerboseOptions sev ioclasses cfg (Just logfile)) =
    P.text $ [fmt|%w-[%L]-[%w]-%T|] sev ioclasses cfg logfile

----------------------------------------

instance HasSeverity VerboseOptions where
  severity = lens _logSeverity (\ vo s → vo { _logSeverity = s })

----------------------------------------

defVOpts ∷ Severity → VerboseOptions
defVOpts sev = VerboseOptions sev ioClasses Map.empty Nothing

----------------------------------------

type CfgsParse = (Maybe (IOClassSet), TextMap)

-- recursively parse a set of config pairs
parseCfgs__ ∷ (AsUsageError ε, MonadError ε η) ⇒
              CfgsParse → [(Text,Text)] → η CfgsParse
parseCfgs__ (iocs,cfg) [] = return (iocs,cfg)
parseCfgs__ (iocs,cfg) ((Text.toLower → k,v) : more) =
  let ioclasses = case iocs of
                    Nothing    → do let class_txts = splitOn "," v
                                    classes ← sequence $ readUsage ⊳ class_txts
                                    let iocs' = Just $ Set.fromList classes
                                    parseCfgs__ (iocs', cfg) more
                    Just iocs_ →  throwUsage $ e_iocs_defined iocs_
   in case k of
        "ioclass"   → ioclasses
        "ioclasses" → ioclasses
        _           → if k ∈ cfg
                      then throwUsage $ e_config_defined
                      else parseCfgs__ (iocs,Map.insert k v cfg) more
  where e_iocs_defined   iocs_ = [fmtT|IOClasses already defined: ⟨%L⟩|] iocs_
        e_config_defined       = [fmtT|config %t already defined: '%t' (%t)|]
                                 k (cfg ! k) v
----------

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

----------------------------------------

parseCfgs_ ∷ [(Text,Text)] → Either UsageError CfgsParse
parseCfgs_ = parseCfgs__ (Nothing,Map.empty)

----------------------------------------

parseCfgs  ∷ [(Text,Text)] → ParsecT σ τ η (IOClassSet, TextMap)
parseCfgs = mErrFail ∘ fmap (first $ fromMaybe ioClasses) ∘ parseCfgs_

----------------------------------------

parseKV ∷ Stream σ η Char ⇒ ParsecT σ τ η (Text,Text)
parseKV = bimap pack pack ⊳ ((,) ⊳ some (alphaNum ∤ oneOf "_-")
                                 ⋪ char '='
                                 ⊵ many (alphaNum ∤ oneOf "_-") )

----------------------------------------

parseIOClassesCfg ∷ Stream σ η Char ⇒ ParsecT σ τ η (IOClassSet,TextMap)
parseIOClassesCfg =
  let parens open close p = char open ⋫ p ⋪ char close
      braces = parens '{' '}'
   in option (ioClasses,Map.empty)
             (braces (parseKV `sepBy` (char '^')) ≫ parseCfgs)

----------------------------------------

parsecSeverityN ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverityN = toEnum ∘ read ∘ pure ⊳ oneOf "01234567"

----------------------------------------

parsecSeverityS ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverityS = let err s = "severity '" ⊕ s ⊕ "' not recognized"
                      sevNames = [(toLower ⊳ show s,s) | s ← enumFrom Emergency]
                   in uniquePrefix sevNames err ((fmap toLower) ⊳ some letter)

----------------------------------------

parsecSeverity ∷ Stream σ η Char ⇒ ParsecT σ τ η Severity
parsecSeverity = try parsecSeverityN ∤ parsecSeverityS

----------------------------------------

mkVerboseOptions ∷ Severity → Maybe((IOClassSet,TextMap),Maybe LogFile)
                 → VerboseOptions
mkVerboseOptions sev c =
  case c of
    Nothing                 → VerboseOptions sev ioClasses Map.empty Nothing
    (Just ((iocs,cfgs),fn)) → VerboseOptions sev iocs      cfgs      fn

----------------------------------------

parseVerboseOptions ∷ ∀ σ η . Stream σ Identity Char ⇒ Parsec σ η VerboseOptions
parseVerboseOptions =
  let colon = char ':'
      betwixt p = between p p
   in mkVerboseOptions ⊳ option Notice parsecSeverity
                       ⊵ optionMaybe ((,) ⊳ (betwixt colon (parseIOClassesCfg))
                                          ⊵ optionMaybe parser)

----------

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
                   -- check case-random prefix of 'alert'
                   "aL::/tmp/log"
            , test (VerboseOptions Alert (Set.fromList [IOWrite])
                                   Map.empty (Just logtmp))
                   "1:{ioclasses=iowrite}:log:tmp"
            , testErr "1:deliberately!!bad:log:tmp"
            , test (VerboseOptions Notice ioClasses Map.empty (Just tmplog))
                   "::/tmp/log"
            , test (VerboseOptions Notice (Set.fromList [IORead]) Map.empty
                                   (Just tmplog))
                   ":{IOCLASS=ioRead}:/tmp/log"
            , test (VerboseOptions Notice (Set.fromList [IOCmdW]) Map.empty
                                   Nothing)
                   ":{ioclass=IOCMDW}:"
            ]

----------------------------------------

instance Parsecable VerboseOptions where
  parser = parseVerboseOptions

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
