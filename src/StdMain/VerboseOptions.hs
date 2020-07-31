{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module StdMain.VerboseOptions
  ( LogFile, HasVerboseOptions( verboseOptions ), VerboseOptions, defVOpts )
where

import GHC.Exts  ( fromList )

import Prelude  ( enumFrom, undefined )

-- base --------------------------------

import Control.Applicative    ( many, some, pure )
import Control.Monad          ( foldM, return, sequence )
import Control.Monad.Fail     ( MonadFail( fail ) )
import Data.Bifunctor         ( bimap, first )
import Data.Char              ( Char, toLower )
import Data.Either            ( Either )
import Data.Eq                ( Eq )
import Data.Foldable          ( foldl )
import Data.Function          ( ($), id )
import Data.Functor           ( fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( intercalate )
import Data.List.NonEmpty     ( NonEmpty( (:|) ) )
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

import Log              ( CSOpt( CallStackHead, FullCallStack, NoCallStack ) )
import Log.HasSeverity  ( HasSeverity( severity ) )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Alert, Emergency, Notice ) )

-- mockio ------------------------------

import MockIO.IOClass  ( IOClass( IOCmdR, IOCmdW, IORead, IOWrite ), IOClassSet
                       , ioClasses )

-- monaderror-io -----------------------

import MonadError2  ( mErrFail )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∈), (∤), (≫), (⊳), (⊵), (⋪), (⋫), ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- natural-plus ------------------------

import Natural  ( toEnum )

-- parsec -----------------------------

import Text.Parsec.Char        ( alphaNum, char, letter, noneOf, oneOf, string )
import Text.Parsec.Combinator  ( between, option, optionMaybe, sepBy )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, try )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable( parser, parsec ), ParseError
                    , caseInsensitiveString, __parsecN__, uniquePrefix )

-- parser-plus -------------------------

import ParserPlus  ( tries )

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

type TextMap    = Map.Map Text Text

------------------------------------------------------------

data VerboseOptions =
  VerboseOptions { _logSeverity   ∷ Severity -- ^ lowest passing severity
                 , _ioClassFilter ∷ IOClassSet
--                 , _config        ∷ TextMap
                 , _callstack     ∷ CSOpt
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
  print (VerboseOptions sev ioclasses csopt Nothing) =
    P.text $ [fmt|%w-%T-<%w>-|] sev ioclasses csopt
  print (VerboseOptions sev ioclasses csopt (Just logfile)) =
    P.text $ [fmt|%w-%T-<%w>-%T|] sev ioclasses csopt logfile

----------------------------------------

instance HasSeverity VerboseOptions where
  severity = lens _logSeverity (\ vo s → vo { _logSeverity = s })

----------------------------------------

defVOpts ∷ Severity → VerboseOptions
defVOpts sev = VerboseOptions sev ioClasses NoCallStack Nothing

----------------------------------------

parseKVs ∷ Stream σ η Char ⇒ ParsecT σ τ η (Text,[Text])
parseKVs =
  let comma  = string ","
      identC = alphaNum ∤ oneOf "_-"
   in bimap pack (fmap pack) ⊳
        ((,) ⊳ some identC ⋪ char '=' ⊵ many identC `sepBy` comma)

----------------------------------------

data LogCfgElement = LogCfgIOClassSet IOClassSet | LogCfgCSOpt CSOpt
  deriving (Eq,Show)

instance Parsecable LogCfgElement where
  parser = let ciString = caseInsensitiveString
               ioc_tag = tries $ ciString "ioclasses" :| [ ciString "ioclass" ]
            in LogCfgCSOpt ⊳ parser
             ∤ LogCfgIOClassSet ⊳ (ioc_tag ⋫ char '=' ⋫ parser)



parseElements ∷ MonadFail η ⇒ [LogCfgElement] → η LogCfg
parseElements lces = do
  let -- f ∷ LogCfgY → LogCfgElement → η LogCfgY
      f (Nothing, csoY) (LogCfgIOClassSet iocs) = return (Just iocs, csoY)
      f (Just iocs,_ ) (LogCfgIOClassSet iocs') =
        fail $ [fmt|Cannot re-assign ioclasses '%w' (was '%w')|] iocs iocs'
      f (iocsY, Nothing) (LogCfgCSOpt cso) = return (iocsY, Just cso)
      f (_, Just cso) (LogCfgCSOpt cso') =
        fail $ [fmt|Cannot re-assign stack option '%w' (was '%w')|] cso cso'
      g ∷ LogCfgY → LogCfg
      g (iocsY, csoY) =
        LogCfg (fromMaybe ioClasses iocsY, fromMaybe NoCallStack csoY)
  (iocsY, csoY) ← foldM f (Nothing,Nothing) lces
  return $ g (iocsY,csoY)


newtype LogCfg = LogCfg { unLogCfg ∷ (IOClassSet,CSOpt) }
-- type LogCfg = (IOClassSet,CSOpt)
type LogCfgY = (Maybe IOClassSet,Maybe CSOpt)

-- instance Parsecable LogCfg where
instance Parsecable LogCfg where
  parser = let braces = between (char '{') (char '}')
            in option (LogCfg (ioClasses, NoCallStack)) $ braces $ parser `sepBy` char '^' ≫ parseElements

                 

{- Recursively parse a set of config pairs to a LogCfg. -}
               
{-
parseCfgs__ ∷ (AsUsageError ε, MonadError ε η) ⇒
              LogCfg → [(Text,Text)] → η (Maybe IOClassSet, Maybe CSOpt)
parseCfgs__ (iocs{-,cfg-}) [] = return (iocs{-,cfg-})
parseCfgs__ (iocs{-,cfg-}) ((Text.toLower → k,v) : more) =
  let ioclasses = case iocs of
                    Nothing    → do let class_txts = splitOn "," v
                                    classes ← sequence $ readUsage ⊳ class_txts
                                    let iocs' = Just $ Set.fromList classes
                                    parseCfgs__ (iocs'{-, cfg-}) more
                    Just iocs_ →  throwUsage $ e_iocs_defined iocs_
   in case k of
        "ioclass"   → ioclasses
        "ioclasses" → ioclasses
        _           → throwUsage $ [fmtT|unrecognized field %t|] k
  where e_iocs_defined   iocs_ = [fmtT|IOClasses already defined: ⟨%L⟩|] iocs_

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
            [ test "empty" (Just ioClasses{-,Map.empty-})
                           ([], (Just ioClasses{-,Map.empty-}))
            , test "just one ioclass"
                   (Just $ Set.fromList [IOCmdW]{-,Map.empty-})
                   ([("ioClasses", "IOCmdW")],
                   (Nothing{-,Map.empty-}))
            , test "just ioclasses"
                   (Just ioClasses{-,Map.empty-})
                   ([("ioClasses", iocsText)], (Nothing{-,Map.empty-}))
            , testErr "more ioclasses"
                      ([("ioClasses", iocsText)], (Just Set.empty{-,Map.empty-}))
            , test "foobar"
                   (Nothing{-,Map.fromList [("foo","bar")]-})
                   ([("foo","bar")], (Nothing{-,Map.empty-}))
            , testErr "foobar again"
                      ([("foo","bar")], (Nothing{-,Map.fromList [("foo","baz")]-}))
            , test "ioclasses + config"
                   (Just $ Set.fromList [IOCmdW,IOCmdR] {-,
                    Map.fromList [("foo","bar"),("baz","quux")]-})
                   ([ ("foo","bar")
                    , ("ioclasses","IOCmdWrite,IOCmdR")
                    , ("baz","quux") ], (Nothing{-,Map.empty-}))
            ]

----------------------------------------

parseCfgs_ ∷ [(Text,Text)] → Either UsageError LogCfg
parseCfgs_ = parseCfgs__ (Nothing{-,Map.empty-})

----------------------------------------

-- parseCfgs  ∷ [(Text,Text)] → ParsecT σ τ η (IOClassSet {- , TextMap -})
-- parseCfgs = mErrFail ∘ fmap ({- first $ -} fromMaybe ioClasses) ∘ parseCfgs_

----------------------------------------

go ∷ [(Text,Text)] → ParsecT σ τ η (IOClassSet, CSOpt)
go = mErrFail ∘ parseCfgs_

-}

parseLogCfg ∷ Stream σ η Char ⇒ ParsecT σ τ η (IOClassSet, CSOpt)
parseLogCfg = undefined
{-
  let parens open close p = char open ⋫ p ⋪ char close
      braces = parens '{' '}'
   in option (ioClasses, NoCallStack)
             (braces (parseKV `sepBy` (char '^')) ≫ go)
-}

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

mkVerboseOptions ∷ Severity → Maybe(LogCfg, Maybe LogFile)
                 → VerboseOptions
mkVerboseOptions sev c =
  case c of
    Nothing → VerboseOptions sev ioClasses NoCallStack Nothing
    (Just (unLogCfg → (iocs,csopt),fn)) →
      VerboseOptions sev iocs csopt fn

----------------------------------------

parseVerboseOptions ∷ ∀ σ η . Stream σ Identity Char ⇒ Parsec σ η VerboseOptions
parseVerboseOptions =
  let colon = char ':'
      betwixt p = between p p
   in mkVerboseOptions ⊳ option Notice parsecSeverity
                       ⊵ optionMaybe ((,) ⊳ (betwixt colon {- parseLogCfg -} parser)
                                          ⊵ optionMaybe parser)

----------

parseVerboseOptionsTests ∷ TestTree
parseVerboseOptionsTests =
  let test exp txt =
        testCase (unpack txt) $
          assertRight (exp ≟) (parsec @_ @ParseError txt txt)
      testErr txt =
        testCase (unpack txt) $
          assertIsLeft (parsec @VerboseOptions @ParseError txt txt)
      tmplog = LogFile (FileA [absfile|/tmp/log|])
      logtmp = LogFile (FileR [relfile|log:tmp|])
   in testGroup "parseVerboseOptions"
            [ test (VerboseOptions Alert ioClasses NoCallStack Nothing) "1"
            , test (VerboseOptions Alert ioClasses NoCallStack (Just tmplog))
                   -- check case-random prefix of 'alert'
                   "aL::/tmp/log"
            , test (VerboseOptions Alert (fromList [IOWrite]) NoCallStack
                                   (Just logtmp))
                   "1:{ioclasses=iowrite}:log:tmp"
            , testErr "1:deliberately!!bad:log:tmp"
            , test (VerboseOptions Notice ioClasses NoCallStack (Just tmplog))
                   "::/tmp/log"
            , test (VerboseOptions Notice (fromList [IORead]) NoCallStack
                                   (Just tmplog))
                   ":{IOCLASS=ioRead}:/tmp/log"
            , test (VerboseOptions Notice (fromList [IOCmdW]) NoCallStack
                                   Nothing)
                   ":{ioclass=IOCMDW}:"
            , test (VerboseOptions Notice (fromList [IOCmdW]) CallStackHead
                                   Nothing)
                   ":{cshead^ioclass=IOCMDW}:"
            , test (VerboseOptions Notice (fromList [IOCmdW]) FullCallStack
                                   Nothing)
                   ":{ioclass=IOCMDW^CallStack}:"
            , test (VerboseOptions Notice ioClasses CallStackHead Nothing)
                   ":{cshead}:"
            ]

----------------------------------------

instance Parsecable VerboseOptions where
  parser = parseVerboseOptions

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "VerboseOptions" [ {- parseCfgsTests, -} parseVerboseOptionsTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
