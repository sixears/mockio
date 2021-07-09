module MockIO.DoMock
  ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )
where

-- base --------------------------------

import Data.Eq        ( Eq )
import Data.Function  ( id )
import GHC.Generics   ( Generic )
import Text.Show      ( Show )

-- deepseq -----------------------------

import Control.DeepSeq  ( NFData )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens' )

--------------------------------------------------------------------------------

data DoMock = DoMock | NoMock
  deriving (Eq,Generic,NFData,Show)

class HasDoMock α where
  doMock ∷ Lens' α DoMock

instance HasDoMock DoMock where
  doMock = id

-- that's all, folks! ----------------------------------------------------------
