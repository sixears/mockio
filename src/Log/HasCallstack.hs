{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.HasCallstack
  ( HasCallstack( callstack ), stackHead, stackHeadTxt )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Function   ( ($), const, id )
import Data.Functor    ( fmap )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import Data.Tuple      ( snd )
import GHC.Stack       ( CallStack, SrcLoc, fromCallSiteList, getCallStack
                       , srcLocFile, srcLocStartLine )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- lens --------------------------------

import Control.Lens  ( Lens', lens, view )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⩺) )

-- safe --------------------------------

import Safe  ( headMay )

-- text --------------------------------

import Data.Text  ( Text, pack )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

--------------------------------------------------------------------------------

class HasCallstack α where
  callstack ∷ Lens' α CallStack

instance HasCallstack CallStack where
  callstack = id

instance HasCallstack [(String,SrcLoc)] where
  callstack = lens fromCallSiteList (const getCallStack)

stackHead ∷ HasCallstack α ⇒ α → Maybe (Text,SrcLoc)
stackHead = fmap (first pack) ∘ headMay ∘ getCallStack ∘ view callstack

stackHeadTxt ∷ HasCallstack α ⇒ α → Text
stackHeadTxt a =
  let locToString loc = [fmt|«%s#%w»|] (srcLocFile loc) (srcLocStartLine loc)
   in case locToString ⩺ fmap snd $ stackHead a of
        Just s  → pack s 
        Nothing → ""

-- that's all, folks! ----------------------------------------------------------
