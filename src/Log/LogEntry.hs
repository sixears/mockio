{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.LogEntry
  ( LogEntry, doc, logEntry )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe )
import GHC.Stack      ( CallStack )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc              ( Doc, defaultLayoutOptions
                                              , layoutPretty )
import Data.Text.Prettyprint.Doc.Render.Text  ( renderStrict )

-- text --------------------------------

import Data.Text  ( pack, take )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt2  ( fmt )

-- time --------------------------------

import Data.Time.Clock  ( UTCTime )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Log.HasCallstack  ( HasCallstack( callstack ), stackHeadTxt )
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

instance Printable LogEntry where
  print le =
    let renderDoc = renderStrict ∘ layoutPretty defaultLayoutOptions
     in P.text $ [fmt|[%Z|%-4t] %t %t|] (le ⊣ utcTimeY)
                                        (take 4 ∘ pack ∘ show $ le ⊣ severity)
                                        (stackHeadTxt le)
                                        (renderDoc $ le ⊣ doc)

logEntry ∷ CallStack → Maybe UTCTime → Severity → Doc() → LogEntry
logEntry = LogEntry

doc ∷ Lens' LogEntry (Doc ())
doc = lens _logdoc (\ le txt → le { _logdoc = txt })

-- that's all, folks! ----------------------------------------------------------
