{-# LANGUAGE UnicodeSyntax #-}

module OptParsePlus2
  ( argS, argT, completePrintables, optT, parserPrefs, parseOpts, parsecArgument
  , parsecOption, parsecReader, readT, textualArgument, textualOption
  , usageFailure, usageFailureCode
  )
where

import Prelude  ( fromIntegral )

-- base --------------------------------

import Control.Monad       ( return, when )
import Data.Bifunctor      ( first )
import Data.Eq             ( Eq )
import Data.Foldable       ( Foldable, toList )
import Data.Function       ( ($), flip )
import Data.Functor        ( fmap )
import Data.Maybe          ( Maybe, fromMaybe )
import Data.Typeable       ( Typeable )
import Data.Word           ( Word8 )
import System.Environment  ( getProgName )
import Text.Show           ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, Textual, toString )

-- exited ------------------------------

import Exited  ( exitWith' )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )

-- optparse-applicative ----------------

import Options.Applicative.Builder
                              ( ArgumentFields, HasCompleter, InfoMod, Mod
                              , OptionFields, ReadM
                              , argument, completeWith, eitherReader
                              , failureCode, flag, fullDesc, info, long, option
                              , prefs, progDesc, showHelpOnEmpty
                              , showHelpOnError
                              )
import Options.Applicative.Help.Core
                              ( parserHelp, parserUsage )
import Options.Applicative.Extra
                              ( ParserPrefs, customExecParser )
import Options.Applicative.Types
                              ( Parser )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO, warn )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable, parsec' )

-- textual-plus ------------------------

import TextualPlus  ( parseTextual )

-- text --------------------------------

import Data.Text  ( Text, pack, unpack )

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

-- | standard parser preferences
parserPrefs ∷ ParserPrefs
parserPrefs = prefs $ showHelpOnError ⊕ showHelpOnEmpty

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

data HelpWith α = HelpWith { _alpha ∷ α, _doHelp ∷ DoHelp }

--------------------

parseHelpWith ∷ Parser α → Parser (HelpWith α)
parseHelpWith f = HelpWith ⊳ f ⊵ flag NoHelp DoHelp (long "help")

--------------------

{- | Parse options, with description, helper, shows help on error and missing
     parameters.  Also exits 2 if --help is called - this is because the exit
     code is most commonly used within scripts, where calling --help is almost
     certainly not what was intended.
-}
parseOpts ∷ MonadIO μ ⇒ Maybe Text -- ^ program name (or uses `getProgName`)
                      → Text       -- ^ brief program description
                      → Parser α   -- ^ proggie opts parser
                      → μ α
parseOpts progn descn prsr = liftIO $ do
  let infoMod = fullDesc ⊕ progDesc (toString descn) ⊕ usageFailure
      prsr'   = parseHelpWith prsr
  opts ← customExecParser parserPrefs (info prsr' infoMod)
  progn' ← flip fromMaybe progn ⊳ (pack ⊳ getProgName)
  when (DoHelp ≡ _doHelp opts) $ do
    let usage = parserUsage parserPrefs prsr (unpack progn')
        help  = parserHelp  parserPrefs prsr
    warn (show usage)
    warn (show help)
    -- Note that usage failures, including using --help in the 'wrong' place,
    -- will result in a showHelpOnError failure; so we use the same exit code.
    _ ← exitWith' usageFailureCode
    return ()
  return $ _alpha opts

----------------------------------------

parsecReader ∷ Parsecable α ⇒ ReadM α
parsecReader = eitherReader (\ s → first show $ parsec' s s)

----------------------------------------

parsecOption ∷ Parsecable α ⇒ Mod OptionFields α → Parser α
parsecOption = option parsecReader

----------------------------------------

parsecArgument ∷ Parsecable α ⇒ Mod ArgumentFields α → Parser α
parsecArgument = argument parsecReader

-- that's all, folks! ----------------------------------------------------------
