{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE ViewPatterns  #-}

module MockIO.IOClass
  ( HasIOClass( ioClass ), IOClass(..), ioClasses, isExternalIO, isInternalIO )
where

-- base --------------------------------

import Data.Char  ( toLower )
import Text.Read  ( Read( readsPrec ) )

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

import Data.MoreUnicode.Bool  ( 𝔹 )
import Data.MoreUnicode.Lens  ( (⊣) )

-- tasty-plus --------------------------

import TastyPlus.Equish  ( Equish( (≃) ) )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

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

ioClasses ∷ Set.Set IOClass
ioClasses = Set.fromList [ IORead, IOWrite, IOCmdR, IOCmdW, IOExec, NoIO ]

instance Read IOClass where
  readsPrec _ (fmap toLower → "ioread")      = [(IORead  ,"")]
  readsPrec _ (fmap toLower → "iowrite")     = [(IOWrite ,"")]
  readsPrec _ (fmap toLower → "iocmdr")      = [(IOCmdR  ,"")]
  readsPrec _ (fmap toLower → "iocmdread")   = [(IOCmdR  ,"")]
  readsPrec _ (fmap toLower → "iocmdw")      = [(IOCmdW  ,"")]
  readsPrec _ (fmap toLower → "iocmdwrite")  = [(IOCmdW  ,"")]
  readsPrec _ (fmap toLower → "ioexec")      = [(IOExec  ,"")]
  readsPrec _ (fmap toLower → "noio")        = [(NoIO    ,"")]
  readsPrec _ _                              = []

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
