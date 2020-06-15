{-# LANGUAGE UnicodeSyntax #-}

module MockIO.IOClass
  ( HasIOClass( ioClass ), IOClass(..), isExternalIO, isInternalIO )
where

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- lens --------------------------------

import Control.Lens  ( Lens' )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )
import Data.MoreUnicode.Lens  ( (⊣) )

-- tasty-plus --------------------------

import TastyPlus.Equish  ( Equish( (≃) ) )

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
  deriving (Eq,Show)

instance Default IOClass where
  def = NoIO

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
