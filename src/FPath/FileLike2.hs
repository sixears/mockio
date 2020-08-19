{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FileLike2
  ( FileLike( (⊙), (<.>)
            , addExt, dir, dirfile, ext, file, split, splitExt, updateExt )
  , IsFile
  )
where

-- base --------------------------------

import Data.Maybe  ( Maybe( Just, Nothing ) )
import Data.Tuple  ( snd )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- lens --------------------------------

import Control.Lens.Iso    ( Iso', iso )
import Control.Lens.Lens   ( Lens', lens )
import Control.Lens.Tuple  ( _1, _2 )

-- mono-traversable --------------------

import Data.MonoTraversable  ( Element )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens       ( (⊣), (⫣) )

-- non-empty-containers ----------------

import NonEmptyContainers.SeqConversions  ( IsMonoSeq )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified FPath.PathComponent  as  PathComponent

import FPath.AsFilePath     ( AsFilePath )
import FPath.DirLike        ( IsDir )
import FPath.DirType        ( DirTypeC( DirType ) )
import FPath.Parent         ( HasParent, HasParentMay )
import FPath.PathComponent  ( PathComponent )

--------------------------------------------------------------------------------

{- | A 'file-like' data-type, e.g., `RelFile` or `AbsFile` - or even
     `PathComponent`.  Importantly, a `FileLike` is isomorphic with a File
     (`PathComponent`) and some other opaque thing (its Directory).
 -}
class (Printable α, DirTypeC α) ⇒ FileLike α where
  {- | Split/Reform a `FileLike` into its directory & file components -}
  dirfile ∷ Iso' α (DirType α, PathComponent)

  file ∷ Lens' α PathComponent
  file = lens (⊣ (dirfile ∘ _2)) (\ a c → (a ⊣ (dirfile ∘ _1),c) ⫣ dirfile)

  dir ∷ Lens' α (DirType α)
  dir = lens (⊣ (dirfile ∘ _1)) (\ a d → (d, a ⊣ (dirfile ∘ _2)) ⫣ dirfile)

  {- | Add an "extension", that is, join two `PathComponent`s with a '.'
       character
   -}
  addExt ∷ α → PathComponent → α
  addExt a c = let (d,f) = a ⊣ dirfile
                in (d,f `addExt` c) ⫣ dirfile

  {- | operator alias for `addExt` -}
  infixr 6 <.> -- same as for ⊕
  (<.>) ∷ α → PathComponent → α
  (<.>) = addExt

  {- | operator alias for `addExt` -}
  infixr 6 ⊙ -- same as for ⊕
  (⊙) ∷ α → PathComponent → α
  (⊙) = (<.>)

  {- | Split an "extension" - the maximal non-empty sequence of characters,
       excluding '.' - from the end of a `PathComponent`; if there is one.
  -}
  splitExt ∷ α → (α, Maybe PathComponent)
  splitExt a = let (d,f) = a ⊣ dirfile
                in case splitExt f of
                     (b, Just e)  → ((d,b) ⫣ dirfile, Just e)
                     (_, Nothing) → (a, Nothing)

  ext ∷ α → Maybe PathComponent
  ext = snd ∘ splitExt
  {- | Update an existing extension; no-op on objects with no extension. -}
  updateExt ∷ (PathComponent → PathComponent) → α → α
  updateExt f x = case splitExt x of
                    (x', Just e) → x' ⊙ f e
                    (_, Nothing) → x

  split ∷ α → (DirType α, PathComponent, Maybe PathComponent)
  split f = let (d,b) = f ⊣ dirfile
                (s,e) = splitExt b
             in (d,s,e)
                     
instance FileLike PathComponent where
  dirfile ∷ Iso' PathComponent ((), PathComponent)
  dirfile = iso ((),) (\ ((),p) → p)

  addExt ∷ PathComponent → PathComponent → PathComponent
  addExt = PathComponent.addExt

  splitExt ∷ PathComponent → (PathComponent, Maybe PathComponent)
  splitExt = PathComponent.splitExt

  updateExt ∷ (PathComponent → PathComponent) → PathComponent → PathComponent
  updateExt = PathComponent.updateExt

{- | Just a marker class for types that represent a file, e.g., AbsFile,
     RelFile, File. -}
class (Printable α, AsFilePath α, AsFilePath (DirType α), IsDir (DirType α),
       HasParent α, HasParentMay α) ⇒ IsFile α

-- that's all, folks! ----------------------------------------------------------
