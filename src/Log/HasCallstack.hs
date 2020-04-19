{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.HasCallstack
  ( HasCallstack( callstack ), stackHead )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Function   ( id )
import Data.Functor    ( fmap )
import Data.Maybe      ( Maybe )
import GHC.Stack       ( CallStack, SrcLoc, getCallStack )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens  ( Lens', view )

-- safe --------------------------------

import Safe  ( headMay )

-- text --------------------------------

import Data.Text  ( Text, pack )

--------------------------------------------------------------------------------

class HasCallstack α where
  callstack ∷ Lens' α CallStack

instance HasCallstack CallStack where
  callstack = id

stackHead ∷ HasCallstack α ⇒ α → Maybe (Text,SrcLoc)
stackHead = fmap (first pack) ∘ headMay ∘ getCallStack ∘ view callstack

-- that's all, folks! ----------------------------------------------------------
