{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE UnicodeSyntax              #-}
{-# LANGUAGE ViewPatterns               #-}

module StdMain.VerboseOptions
  ( LogFile( unLogFile ), HasVerboseOptions( verboseOptions ), VerboseOptions
  , csopt, defVOpts, ioClassFilter, logFile, verboseDesc )
where

import GHC.Exts  ( fromList )

import Prelude  ( enumFrom )

-- base --------------------------------

import Control.Applicative    ( some, pure )
import Control.Monad          ( foldM, return )
import Control.Monad.Fail     ( MonadFail( fail ) )
import Data.Char              ( Char, toLower )
import Data.Eq                ( Eq )
import Data.Function          ( ($), id )
import Data.Functor           ( fmap )
import Data.Functor.Identity  ( Identity )
import Data.List              ( intersperse )
import Data.List.NonEmpty     ( NonEmpty( (:|) ), toList )
import Data.Maybe             ( Maybe( Just, Nothing ), fromMaybe )
import Data.String            ( String )
import Data.Tuple             ( fst )
import System.Exit            ( ExitCode )
import System.IO              ( IO )
import Text.Read              ( read )
import Text.Show              ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), toText )

-- fpath -------------------------------

import FPath.AbsFile    ( absfile )
import FPath.File2      ( File( FileA, FileR ) )
import FPath.Parseable  ( parse' )
import FPath.RelFile    ( relfile )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- log-plus ----------------------------

import Log              ( CSOpt( CallStackHead, FullCallStack, NoCallStack )
                        , stackOptions, stackParses )
import Log.HasSeverity  ( HasSeverity( severity ) )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Alert, Emergency, Warning, Notice ) )

-- mockio ------------------------------

import MockIO.IOClass  ( IOClass( IOCmdW, IORead, IOWrite ), IOClassSet
                       , ioClasses, ioClassParses )

-- monaderror-io -----------------------

import MonadError2  ( mErrFail )

-- more-unicode ------------------------

import Data.MoreUnicode  ( (∤), (≫), (⊳), (⊵), (⋫), ю, ℕ )

-- natural-plus ------------------------

import Natural  ( allEnum, toEnum )

-- optparse-applicative ----------------

import Options.Applicative.Help.Pretty  ( Doc
                                        , (<+>), (<$$>)
                                        , align, comma, dquotes, empty, fillSep
                                        , hang, hsep, indent, punctuate, sep
                                        , space, text, vcat
                                        )

-- optparse-plus --------------------------------

import OptParsePlus2  ( (⊞), finalFullStop, listDQOr, listDQSlash, listW, toDoc
                      , toDocT, toDocTs )

-- parsec -----------------------------

import Text.Parsec.Char        ( char, letter, noneOf, oneOf )
import Text.Parsec.Combinator  ( between, option, optionMaybe, sepBy )
import Text.Parsec.Prim        ( Parsec, ParsecT, Stream, try )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable( parsec, parser ), ParseError
                    , caseInsensitiveString, uniquePrefix )

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

import Data.Text  ( Text, intercalate, pack, unpack, words )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

newtype LogFile = LogFile { unLogFile ∷ File }
  deriving (Eq,Printable,Show)

instance Parsecable LogFile where
  parser = LogFile ⊳ do
    fn ← some (noneOf "\0")
    mErrFail $ parse' fn

------------------------------------------------------------

data VerboseOptions =
  VerboseOptions { _logSeverity   ∷ Severity -- ^ lowest passing severity
                 , _ioClassFilter ∷ IOClassSet
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

logFile ∷ Lens' VerboseOptions (Maybe LogFile)
logFile = lens _logFile (\ o l → o { _logFile = l })

----------------------------------------

csopt ∷ Lens' VerboseOptions CSOpt
csopt = lens _callstack (\ o c → o { _callstack = c })

----------------------------------------

ioClassFilter ∷ Lens' VerboseOptions IOClassSet
ioClassFilter = lens _ioClassFilter (\ o iocs → o { _ioClassFilter = iocs })

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
  deriving (Eq, Show)
type LogCfgY = (Maybe IOClassSet,Maybe CSOpt)

----------------------------------------

instance Parsecable LogCfg where
  -- '^' was selected as being a character less likely to be required in
  -- config values, but not requiring escaping with regular shells (e.g., bash)
  parser = let braces = between (char '{') (char '}')
            in option (LogCfg (ioClasses, NoCallStack)) ∘ braces $
                 parser `sepBy` char '^' ≫ parseElements


parseLogCfgTests ∷ TestTree
parseLogCfgTests =
  let test ∷ (IOClassSet,CSOpt) → Text → TestTree
      test exp txt = testCase (unpack txt) $
        assertRight (@=? LogCfg exp) (parsec @_ @ParseError txt txt)
   in testGroup "parseCfgs"
            [ test (ioClasses,NoCallStack) "{}"
            , test (fromList [IOCmdW],NoCallStack) "{ioclass=iocmdw}"
            , test (ioClasses,CallStackHead) "{csh}"
            , test (fromList [IOWrite,IORead],CallStackHead)
                   "{CSH^iOcLaSsEs=ioread,iow}"
            , test (fromList [IOWrite,IORead],CallStackHead)
                   "{CSH^iOcLaSsEs=iow,ioread}"
            ]

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

defSeverity ∷ Severity
defSeverity = Notice

parseVerboseOptions ∷ ∀ σ η . Stream σ Identity Char ⇒ Parsec σ η VerboseOptions
parseVerboseOptions =
  let colon = char ':'
      betwixt p = between p p
   in mkVerboseOptions ⊳ option defSeverity parsecSeverity
                       ⊵ optionMaybe ((,) ⊳ (betwixt colon parser)
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
            , test (VerboseOptions Warning ioClasses NoCallStack Nothing)
                   "warn"
            , test (VerboseOptions Alert (fromList [IOWrite]) NoCallStack
                                   (Just logtmp))
                   "1:{ioclasses=iowrite}:log:tmp"
            , testErr "1:deliberately!!bad:log:tmp"
            , test (VerboseOptions defSeverity ioClasses NoCallStack
                                   (Just tmplog))
                   "::/tmp/log"
            , test (VerboseOptions defSeverity (fromList [IORead]) NoCallStack
                                   (Just tmplog))
                   ":{IOCLASS=ioRead}:/tmp/log"
            , test (VerboseOptions defSeverity (fromList [IOCmdW]) NoCallStack
                                   Nothing)
                   ":{ioclass=IOCMDW}:"
            , test (VerboseOptions defSeverity (fromList [IOCmdW]) CallStackHead
                                   Nothing)
                   ":{cshead^ioclass=IOCMDW}:"
            , test (VerboseOptions defSeverity (fromList [IOCmdW]) FullCallStack
                                   Nothing)
                   ":{ioclass=IOCMDW^CallStack}:"
            , test (VerboseOptions defSeverity ioClasses CallStackHead Nothing)
                   ":{cshead}:"
            ]

----------------------------------------

instance Parsecable VerboseOptions where
  parser = parseVerboseOptions

----------------------------------------

{- | --verbose description, for user help. -}
verboseDesc ∷ Doc
verboseDesc =
  let stackControl = fillSep [ toDocTs [ "Choose stack output; by default, no"
                                       , "stack info is output, but any of " ]
                             , listDQOr (stackParses CallStackHead)
                             , toDocTs [ "may be used to get the top stack"
                                       , "frame included with the log; any of"
                                       ]
                             , listDQOr (stackParses FullCallStack)
                             , toDocTs [ "may be used to get the full call"
                                       , "stack; and any of"
                                       ]
                             , listDQOr (stackParses NoCallStack)
                             , toDocTs [ "may be used to elide the call stack"
                                       , "(which is the default).  All parsing"
                                       , "is case-insensitive."
                                       ]
                             ]
      ioclasses = fillSep $ ю [ [ toDocTs [ "Select which IO classes to output."
                                          , "The config should be written as"
                                          , "'ioclasses=class0,class1...'"
                                          , "The available IO classes are " ]
                                ]
                              , finalFullStop (punctuate comma $
                                 (listDQSlash ∘ ioClassParses) ⊳ allEnum)
                              , [ toDocTs [ "The default is to output all"
                                          , "IO classes." ]
                                ]
                              ]

      example = text "info:{cshead^ioclasses=iocmdw,iocmdr}:/tmp/log"

   in toDoc $ [ toDoc [ "Detailed setting of verbosity."
                      , "The general form of OPTS is LEVEL:CONFIG:FILE; where"
                      , "LEVEL is a verbosity level."
                      , "The default verbosity level is"
                      , pack (show defSeverity) ⊕ ", and logging is sent to"
                      , "stderr."
                      ]

               ,   toDocT "Available verbosity levels are"
                 ⊞ (listW $ allEnum @Severity) ⊕ "."
                 ⊞ toDocTs [ "Any unique prefix of a verbosity level,"
                           , "is accepted.  Alternatively, any digit from"
                           , "0 (Emergency) to 7 (Debug) is accepted.  Parsing"
                           , "is case-insensitive."
                           ]

               , toDocTs [ "The config, if provided, must be between two braces"
                         , "({}).  Configuration consists of a list of"
                         , "settings, separated by a circumflex or high caret"
                         , "(^) character.  Available settings are: "
                         ]
                 <$$> text "stack control " ⊞ align stackControl
                 <$$> text "io classes    " ⊞ align ioclasses

               , toDocTs [ "The file, if provided, will be opened for writing"
                         , "(rather than appending)." ]

               , toDoc   [ text "An example value to --verbose is: "
                         , indent 4 example ]

               , toDocTs [ "This option is exclusive with -v, --quiet, and"
                         , "--debug." ]


               ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "VerboseOptions" [ parseLogCfgTests,parseVerboseOptionsTests ]

----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
