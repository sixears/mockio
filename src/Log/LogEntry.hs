{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.LogEntry
  ( LogEntry, doc, logEntry, logEntry'
  , _le0, _le1, _le2, _le3 )
where

-- base --------------------------------

import qualified  GHC.Stack

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import GHC.Stack      ( CallStack, SrcLoc, fromCallSiteList )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- logging-effect ----------------------

import Control.Monad.Log  ( Severity( Critical, Emergency, Informational
                                    , Warning ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc              ( Doc, (<+>), align, defaultLayoutOptions
                                              , layoutPretty, pretty, vsep )
import Data.Text.Prettyprint.Doc.Render.Text  ( renderStrict )

-- text --------------------------------

import Data.Text  ( Text, pack, take )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt2  ( fmt )

-- time --------------------------------

import Data.Time.Calendar  ( fromGregorian )
import Data.Time.Clock     ( UTCTime( UTCTime ), secondsToDiffTime )

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

logEntry ∷ HasCallstack α ⇒ α → Maybe UTCTime → Severity → Doc() → LogEntry
logEntry cs = LogEntry (cs ⊣ callstack)

logEntry' ∷ [(String,SrcLoc)] → Maybe UTCTime → Severity → Doc() → LogEntry
logEntry' = logEntry

doc ∷ Lens' LogEntry (Doc ())
doc = lens _logdoc (\ le txt → le { _logdoc = txt })

-- test data -------------------------------------------------------------------

_cs0 ∷ CallStack
_cs0 = fromCallSiteList []

_cs1 ∷ CallStack
_cs1 = fromCallSiteList [ ("stack0",GHC.Stack.SrcLoc "z" "x" "y" 9 8 7 6) ]

_cs2 ∷ CallStack
_cs2 = fromCallSiteList [ ("stack0",GHC.Stack.SrcLoc "a" "b" "c" 1 2 3 4)
                        , ("stack1",GHC.Stack.SrcLoc "d" "e" "f" 5 6 7 8) ]

_tm ∷ UTCTime
_tm = UTCTime (fromGregorian 1970 1 1) (secondsToDiffTime 0)

_le0 ∷ LogEntry
_le0 = logEntry _cs2 (Just _tm) Informational (pretty ("log_entry 1" ∷ Text))

_le1 ∷ LogEntry
_le1 =
  logEntry _cs1 Nothing Critical (pretty ("multi-line\nlog\nmessage" ∷ Text))

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

_le2 ∷ LogEntry
_le2 =
  let valign = align ∘ vsep
   in logEntry _cs1 (Just _tm) Warning ("this is" ⊞ valign [ "a"
                                                           , "vertically"
                                                           ⊞ valign [ "aligned"
                                                                    , "message"
                                                                    ]
                                                           ])
_le3 ∷ LogEntry
_le3 = 
  logEntry _cs1 Nothing Emergency (pretty ("this is the last message" ∷ Text))

-- that's all, folks! ----------------------------------------------------------
