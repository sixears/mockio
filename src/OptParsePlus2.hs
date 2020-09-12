{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module OptParsePlus2
  ( argS, argT, completePrintables, optT, parserPrefs, parseOpts, parsecArgument
  , parsecOption, parsecReader, readT, textualArgument, textualOption
  , usageFailure, usageFailureCode

  , ToDoc( toDoc ), (⊞)
  , finalFullStop, listDQOr, listSlash, listDQSlash, listW, toDocT, toDocTs
  )
where

import Prelude  ( Int, error, fromIntegral )

-- base --------------------------------

import Control.Monad       ( return )
import Data.Bifunctor      ( first )
import Data.Eq             ( Eq )
import Data.Foldable       ( Foldable, foldr, toList )
import Data.Function       ( ($), flip, id )
import Data.Functor        ( fmap )
import Data.List           ( intersperse )
import Data.Maybe          ( Maybe( Just, Nothing ), fromMaybe )
import Data.String         ( String )
import Data.Typeable       ( Typeable )
import Data.Word           ( Word8 )
import System.Environment  ( getArgs, getProgName )
import System.Exit         ( ExitCode( ExitSuccess ), exitSuccess, exitWith )
import System.IO           ( IO, hPutStrLn, putStr, putStrLn, stderr )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.List.Unicode      ( (∈) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, Textual, toString )

-- exited ------------------------------

import Exited  ( exitWith' )

-- extra -------------------------------

import Data.List.Extra  ( unsnoc )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor2  ( (⊳), (⊳⊳) )
import Data.MoreUnicode.Monad     ( (≫) )

-- optparse-applicative ----------------

import Options.Applicative.Builder
                              ( ArgumentFields, HasCompleter, InfoMod, Mod
                              , OptionFields, ReadM
                              , argument, columns, completeWith, eitherReader
                              , failureCode, fullDesc, info, option, prefs
                              )
import Options.Applicative.Extra
                              ( ParserPrefs, execParserPure, renderFailure )
import Options.Applicative.Help.Core
                              ( footerHelp, headerHelp, parserHelp, parserUsage )
import Options.Applicative.Help.Pretty
                              ( Doc, (<+>), comma, displayS, dquotes, empty, fillSep
                              , punctuate, renderPretty, space, text, vcat )


import Options.Applicative.Types
                              ( Parser, ParserInfo, ParserHelp, ParserResult( CompletionInvoked, Failure, Success ), execCompletion, infoFooter, infoHeader, infoParser )
import Options.Applicative.Help.Types  ( renderHelp )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Natural  ( ℕ )

-- parsec-plus -------------------------

import ParsecPlus2  ( ParseError, Parsecable, parsec )

-- terminal-size -----------------------

import qualified  System.Console.Terminal.Size  as  TerminalSize

-- textual-plus ------------------------

import TextualPlus  ( parseTextual )

-- text --------------------------------

import Data.Text  ( Text, intercalate, pack, unpack, words )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

readT ∷ (Textual α, Typeable α) ⇒ ReadM α
readT = eitherReader parseTextual

----------------------------------------

argT ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argT = argument readT

{-# DEPRECATED argS "use `argT` instead" #-}
argS ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
argS = argT

----------------------------------------

optT ∷ (Textual α, Typeable α) ⇒ Mod OptionFields α → Parser α
optT = option readT

----------------------------------------

-- | an option that produces a textual value

textualOption ∷ (Textual α, Typeable α) ⇒ Mod OptionFields α → Parser α
textualOption = option readT

----------------------------------------

-- | an argement that produces a textual value

textualArgument ∷ (Textual α, Typeable α) ⇒ Mod ArgumentFields α → Parser α
textualArgument = argument readT

----------------------------------------

-- | arg/option completer using Printables

completePrintables ∷ (Foldable φ, Printable α, HasCompleter χ) ⇒ φ α → Mod χ γ
completePrintables = completeWith ∘ fmap toString ∘ toList

----------------------------------------

{- | Standard parser preferences.  Input is terminal width. -}
parserPrefs ∷ ℕ → ParserPrefs
parserPrefs width = let -- width = (fromIntegral $ fromMaybe 80 w)
                     in prefs $ {- showHelpOnError ⊕ -} {- showHelpOnEmpty ⊕ -} columns (fromIntegral width)

----------------------------------------

{- | Common exit code for usage errors, including --help (so that scripts that
     call --help get errors)
 -}
usageFailureCode ∷ Word8
usageFailureCode = 2

{- | Common exit code for usage errors, digestable by `Options.Applicative` -}
usageFailure ∷ InfoMod α
usageFailure = failureCode (fromIntegral usageFailureCode)

----------------------------------------

data DoHelp = DoHelp | NoHelp
  deriving (Eq, Show)

--------------------

data HelpWith' α = DoHelp' | NoHelp' α

--------------------

myExecParser' ∷ ParserPrefs → ParserInfo α → IO (HelpWith' α)
myExecParser' pprefs pinfo = do
  args ← getArgs
  if "--help" ∈ args {- ∨ any ("--help=" `isPrefixOf`) args -}
  then return DoHelp'
  else NoHelp' ⊳ handleParseResult (execParserPure pprefs pinfo args)


-- | Handle `ParserResult`.
handleParseResult :: ParserResult a -> IO a
handleParseResult (Success a) = return a
handleParseResult (Failure failure) = do
      progn <- getProgName
      let (msg, exit) = renderFailure failure progn
      case exit of
        ExitSuccess -> putStrLn msg
        _           -> hPutStrLn stderr msg
      exitWith exit
handleParseResult (CompletionInvoked compl) = do
      progn <- getProgName
      msg <- execCompletion compl progn
      putStr msg
      exitSuccess

--------------------

{- | Parse options, with description, helper, shows help on error and missing
     parameters.  Also exits 2 if --help is called - this is because the exit
     code is most commonly used within scripts, where calling --help is almost
     certainly not what was intended.
-}
parseOpts ∷ MonadIO μ ⇒ Maybe Text -- ^ program name (or uses `getProgName`)
                      -- | base infomod for parser; typically `progDesc
                      --   "some description"`
--                      → InfoMod (HelpWith α)
                      → InfoMod α
                      → Parser α   -- ^ proggie opts parser
                      → μ α
parseOpts progn baseinfo prsr = liftIO $ do
  width ← fromMaybe 80 ⊳ (TerminalSize.width @Int ⊳⊳ TerminalSize.size)
  let infoMod = fullDesc ⊕ baseinfo ⊕ usageFailure
      pprefs  = parserPrefs (fromIntegral width)
      showHelp ∷ ParserHelp → IO()
      showHelp = hPutStrLn stderr ∘ renderHelp width
      showDoc  ∷ Doc → IO()
      showDoc  = hPutStrLn stderr ∘ (`displayS` "") ∘ renderPretty 1.0 width
  hopts  ← myExecParser' pprefs (info prsr infoMod)
  progn' ← flip fromMaybe progn ⊳ (pack ⊳ getProgName)
  case hopts of
    NoHelp' opts → return opts
    DoHelp' → do
      let usage = parserUsage pprefs prsr (unpack progn')
          i = info prsr infoMod
      showDoc usage
      showHelp $ headerHelp (infoHeader i)
      showHelp $ parserHelp pprefs (infoParser i)
      showHelp $ footerHelp (infoFooter i)
      _ ← exitWith' usageFailureCode
      error "unreachable code in parseOpts"


----------------------------------------

parsecReader ∷ Parsecable α ⇒ ReadM α
parsecReader = eitherReader (\ s → first show $ parsec @_ @ParseError s s)

----------------------------------------

parsecOption ∷ Parsecable α ⇒ Mod OptionFields α → Parser α
parsecOption = option parsecReader

----------------------------------------

parsecArgument ∷ Parsecable α ⇒ Mod ArgumentFields α → Parser α
parsecArgument = argument parsecReader

----------------------------------------

infixr 6 ⊞
(⊞) ∷ Doc → Doc → Doc
(⊞) = (<+>)

{- | Simple auto-conversions to Doc. -}
class ToDoc α where
  toDoc ∷ α → Doc

instance ToDoc Doc where
  toDoc = id

instance ToDoc Text where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (words ts)

instance ToDoc [Text] where
  {- | The text is broken (on spaces) into words; and then re-joined with
       breaking spaces (⊞) between them, to form a flowable paragraph. -}
  toDoc ts = fillSep $ fmap (text ∘ unpack) (ts ≫ words)

instance ToDoc [[Text]] where
  {- | Each text list is assembled into a flowing paragraph; each para is
       separated by a blank line. -}
  toDoc tss = vcat $ intersperse space (toDoc ⊳ tss)

instance ToDoc [Doc] where
  {- | Each doc is separated by a blank line. -}
  toDoc ds = vcat $ intersperse space ds

toDocT ∷ Text → Doc
toDocT = toDoc

toDocTs ∷ [Text] → Doc
toDocTs = toDoc

{- | Create a list by joining words (which are surrounded with double-quotes)
     with ", ", except for the last, which is joined with "or". -}
listDQOr ∷ [String] → Doc
listDQOr (unsnoc → Nothing)     = empty
listDQOr (unsnoc → Just (ws,w)) =
  fillSep (punctuate comma (dquotes ∘ text ⊳ ws)) ⊞ text "or" ⊞ dquotes (text w)
listDQOr x = error $ [fmt|this should never happen (listDQOr) %w|] x

{- | Create a list by joining words (showable things) with ", ". -}
listW ∷ Show α ⇒ [α] → Doc
listW xs = toDoc $ intercalate ", " (pack ∘ show ⊳ xs)

{- | Create a list by joining strings with "/". -}
listSlash ∷ [String] → Doc
listSlash xs = toDoc $ intercalate "/" (pack ⊳ xs)

{- | Create a list by joining double-quoted strings with "/". -}
listDQSlash ∷ [String] → Doc
listDQSlash []     = empty
listDQSlash (x:xs) =
  foldr (\ a b → a ⊕ text "/" ⊕ b) (dquotes $ text x) (dquotes ∘ text ⊳ xs)

{- | Add a full stop (period) to the final doc in a list. -}
finalFullStop ∷ [Doc] → [Doc]
finalFullStop (unsnoc → Just (ds,d)) = ds ⊕ [d ⊕ text "."]
finalFullStop (unsnoc → Nothing)     = []
finalFullStop x = error $ [fmt|this should never happen (finalFullStop) %w|] x

-- that's all, folks! ----------------------------------------------------------
