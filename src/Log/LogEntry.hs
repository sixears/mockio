{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.LogEntry
  ( LogEntry, doc, logEntry )
where

-- base --------------------------------

import Data.Maybe  ( Maybe )
import GHC.Stack   ( CallStack )
import Text.Show   ( Show )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Log.HasCallstack  ( HasCallstack( callstack ) )
import Log.HasSeverity   ( HasSeverity( severity ) )
import Log.HasUTCTime    ( HasUTCTimeY( utcTimeY ) )

--------------------------------------------------------------------------------

{- | Log with timestamp, callstack, severity & IOClass -}
data LogEntry = LogEntry { _callstack ∷ CallStack
                         , _timestamp ∷ Maybe UTCTime
                         , _severity  ∷ Severity
                         , _logdoc    ∷ Doc ()
                         }
  deriving Show

instance HasCallstack LogEntry where
  callstack = lens _callstack (\ le cs → le { _callstack = cs })

instance HasSeverity LogEntry where
  severity = lens _severity (\ le sv → le { _severity = sv })

instance HasUTCTimeY LogEntry where
  utcTimeY = lens _timestamp (\ le tm → le { _timestamp = tm })

logEntry ∷ CallStack → Maybe UTCTime → Severity → Doc() → LogEntry
logEntry = LogEntry

doc ∷ Lens' LogEntry (Doc ())
doc = lens _logdoc (\ le txt → le { _logdoc = txt })

-- that's all, folks! ----------------------------------------------------------
