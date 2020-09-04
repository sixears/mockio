{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.AbsFile2
  ( AbsDir, AbsFile, AsAbsFile( _AbsFile )

  , absfile, absfileT

  , tests
  )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified FPath.FileLike

import FPath.AbsFile
import FPath.AsFilePath2  ( AsFilePath'( filepath' ), filepath )
import FPath.FileLike2    ( FileLike( dirfile ), IsFile )

-------------------------------------------------------------------------------

instance FileLike AbsFile where
  dirfile = FPath.FileLike.dirfile

instance IsFile AbsFile

instance AsFilePath' AbsFile where
  filepath' = filepath

-- that's all, folks! ----------------------------------------------------------
