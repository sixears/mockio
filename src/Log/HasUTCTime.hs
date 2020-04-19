{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.HasUTCTime
  ( HasUTCTime( utcTime ), HasUTCTimeY( utcTimeY ) )
where

-- base --------------------------------

import Data.Function  ( id )
import Data.Maybe     ( Maybe )

-- lens --------------------------------

import Control.Lens  ( Lens' )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime )

--------------------------------------------------------------------------------

class HasUTCTime α where
  utcTime ∷ Lens' α UTCTime

instance HasUTCTime UTCTime where
  utcTime = id

------------------------------------------------------------

class HasUTCTimeY α where
  utcTimeY ∷ Lens' α (Maybe UTCTime)

instance HasUTCTimeY (Maybe UTCTime) where
  utcTimeY = id

-- that's all, folks! ----------------------------------------------------------
