{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.DoMock
  ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )
where

-- base --------------------------------

import Data.Eq          ( Eq )
import Data.Function    ( id )
import Text.Show        ( Show )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

--------------------------------------------------------------------------------

data DoMock = DoMock | NoMock
  deriving (Eq,Show)

class HasDoMock α where
  doMock ∷ Lens' α DoMock

instance HasDoMock DoMock where
  doMock = id

-- that's all, folks! ----------------------------------------------------------
