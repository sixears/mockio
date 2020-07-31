{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}
{-# LANGUAGE ViewPatterns        #-}

module MockIO.IOClass
  ( HasIOClass( ioClass ), IOClass(..), IOClassSet
  , ioClasses, isExternalIO, isInternalIO )
where

import GHC.Exts  ( IsList( Item, fromList, toList ) )

-- base --------------------------------

import Data.Char           ( toLower )
import Data.List.NonEmpty  ( NonEmpty( (:|) ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- containers --------------------------

import qualified Data.Set  as  Set

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens  ( Lens' )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⋪), (⋫), (∤) )
import Data.MoreUnicode.Bool         ( 𝔹 )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )

-- parsec ------------------------------

import Text.Parsec.Char        ( char, string )
import Text.Parsec.Combinator  ( sepBy )

-- parsec-plus -------------------------

import ParsecPlus2  ( Parsecable( parser ), caseInsensitiveString )

-- parser-plus -------------------------

import ParserPlus  ( tries )

-- tasty-plus --------------------------

import TastyPlus.Equish  ( Equish( (≃) ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

data IOClass = IORead  -- ^ An IO action that perceives but does not alter state
                       --   (e.g., read a file, or the system clock).
             | IOWrite -- ^ An IO action that may alter state
                       --   (e.g., write a file, or to the network).
             | IOCmdR  -- ^ An external cmd (results in an execve or fork call)
                       --   that perceives but does not alter state.
             | IOCmdW  -- ^ An external cmd (results in an execve or fork call)
                       --   that may alter state.
             | IOExec  -- ^ An exec (replaces this executable).
             | NoIO    -- ^ No IO.
  -- ordering is not relevant, we just derive it to support Set
  deriving (Eq,Ord,Show)

newtype IOClassSet = IOClassSet { unIOClassSet ∷ Set.Set IOClass }
  deriving (Eq, Show)

instance IsList IOClassSet where
  -- requirement is that fromList ∘ toList = id (not the other way round)
  type Item IOClassSet = IOClass
  fromList = IOClassSet ∘ Set.fromList
  toList   = Set.toList ∘ unIOClassSet

instance Printable IOClassSet where
  print (toList → iocs) = P.text $ [fmt|«%L»|] iocs

instance Parsecable IOClassSet where
  parser = fromList ⊳ (parser `sepBy` (char ','))

ioClasses ∷ IOClassSet
ioClasses =
  IOClassSet $ Set.fromList [ IORead, IOWrite, IOCmdR, IOCmdW, IOExec, NoIO ]

instance Parsecable IOClass where
  parser = let strs =    ("IORead"     , IORead)
                    :| [ ("IOWrite"    , IOWrite)
                       , ("IOCmdRead"  , IOCmdR)
                       , ("IOCmdR"     , IOCmdR)
                       , ("IOCmdWrite" , IOCmdW)
                       , ("IOCmdW"     , IOCmdW)
                       , ("IOExec"     , IOExec)
                       , ("NoIO"       , NoIO)
                       ]
            in tries [ caseInsensitiveString st ⋫ return ioc | (st,ioc) ← strs]

instance Default IOClass where
  def = NoIO

instance Printable IOClass where
  print = P.string ∘ show

instance Equish IOClass where
  i ≃ i' = i ≡ i'

class HasIOClass α where
  ioClass ∷ Lens' α IOClass

instance HasIOClass IOClass where
  ioClass = id

{-| Predicate for IO that outside of this process (utilizes exec*); that is,
    exclude `NoIO`, `IORead` & `IOWrite`; leaving `IOCmdR`, `IOCmdW`, `IOExec`.
 -}
isExternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isExternalIO a = case a ⊣ ioClass of
                   NoIO    → False
                   IORead  → False
                   IOWrite → False
                   IOCmdR  → True
                   IOCmdW  → True
                   IOExec  → True

{-| Logical inverse of `isExternalIO`. -}
isInternalIO ∷ HasIOClass α ⇒ α -> 𝔹
isInternalIO = not ∘ isExternalIO

-- that's all, folks! ----------------------------------------------------------
