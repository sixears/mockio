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

module FPath.RelFile2
  ( module FPath.RelFile )
where

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import qualified  FPath.FileLike
import FPath.RelFile
import FPath.AsFilePath2  ( AsFilePath'( filepath' ), filepath )
import FPath.FileLike2    ( FileLike( dirfile ), IsFile )

-------------------------------------------------------------------------------

instance FileLike RelFile where
  dirfile = FPath.FileLike.dirfile

instance IsFile RelFile

instance AsFilePath' RelFile where
  filepath' = filepath

-- that's all, folks! ----------------------------------------------------------
