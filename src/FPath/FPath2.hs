{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FPath2
  ( FPathAs( _FPath ) )
where

-- base --------------------------------

import Data.Function  ( ($), id )
import Data.Maybe     ( Maybe( Just, Nothing ) )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism', prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.Abs         ( Abs, AsAbs( _Abs ) )
import FPath.AbsDir      ( AbsDir, AsAbsDir( _AbsDir ) )
import FPath.AbsFile     ( AbsFile, AsAbsFile( _AbsFile ) )
import FPath.AsFilePath  ( AsFilePath )
import FPath.Dir2        ( AsDir( _Dir ), Dir( DirA, DirR ) )
import FPath.FPath       ( FPath( FAbsD, FAbsF, FRelD, FRelF ) )
import FPath.File2       ( AsFile( _File ), File( FileA, FileR ) )
import FPath.Rel         ( Rel, AsRel( _Rel ) )
import FPath.RelDir      ( RelDir, AsRelDir( _RelDir ) )
import FPath.RelFile     ( RelFile, AsRelFile( _RelFile ) )

--------------------------------------------------------------------------------

instance AsDir FPath where
  _Dir = prism' (\ p → case p of DirA d → FAbsD d
                                 DirR f → FRelD f
                )
                (\ p → case p of FAbsD d → Just $ DirA d
                                 FRelD f → Just $ DirR f
                                 _       → Nothing
                )

instance AsFile FPath where
  _File = prism' (\ p → case p of FileA d → FAbsF d
                                  FileR f → FRelF f
                 )
                 (\ p → case p of FAbsF d → Just $ FileA d
                                  FRelF f → Just $ FileR f
                                  _       → Nothing
                 )

------------------------------------------------------------

{- | Class of things that are guaranteed convertable to an FPath (but that an
     FPath might or might not be able to convert to). -}
class (Printable α, AsFilePath α) ⇒ FPathAs α where
  _FPath ∷ Prism' FPath α

instance FPathAs FPath where
  _FPath = id

instance FPathAs AbsDir where
  _FPath = _AbsDir

instance FPathAs RelDir where
  _FPath = _RelDir

instance FPathAs AbsFile where
  _FPath = _AbsFile

instance FPathAs RelFile where
  _FPath = _RelFile

instance FPathAs Dir where
  _FPath = _Dir

instance FPathAs File where
  _FPath = _File

instance FPathAs Rel where
  _FPath = _Rel

instance FPathAs Abs where
  _FPath = _Abs

-- that's all, folks! ----------------------------------------------------------
