{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.DirLike
  ( IsDir )
where

-- data-textual ------------------------

import Data.Textual  ( Printable )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir  ( AbsDir )
import FPath.RelDir  ( RelDir )

--------------------------------------------------------------------------------

{- | Just a marker class for types that represent a dir, e.g., AbsDir,
     RelDir, Dir. -}
class Printable α ⇒ IsDir α where

instance IsDir AbsDir

instance IsDir RelDir

-- that's all, folks! ----------------------------------------------------------
