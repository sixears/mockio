{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.RenderDoMock
  ( renderLogWithDoMock )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- log-plus ----------------------------

import Log.LogEntry       ( LogEntry, attrs )
import Log.LogRenderOpts  ( LogAnnotator( LogAnnotator ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊣) )

-- prettyprinter -----------------------

import Data.Text.Prettyprint.Doc                  ( Doc, annotate )
import Data.Text.Prettyprint.Doc.Render.Terminal  ( AnsiStyle, italicized )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MockIO.DoMock        ( DoMock( DoMock ), HasDoMock( doMock ) )

--------------------------------------------------------------------------------

renderWithDoMockAnsi ∷ HasDoMock τ ⇒
                       (LogEntry τ → Doc AnsiStyle) → LogEntry τ → Doc AnsiStyle
renderWithDoMockAnsi f m = if m ⊣ attrs ∘ doMock ≡ DoMock
                           then annotate italicized $ f m
                           else f m

renderWithDoMock ∷ (LogEntry τ → Doc ()) → LogEntry τ → Doc ()
renderWithDoMock f m = f m

----------

renderLogWithDoMock ∷ HasDoMock τ ⇒ LogAnnotator τ
renderLogWithDoMock = LogAnnotator renderWithDoMockAnsi renderWithDoMock

-- that's all, folks! ----------------------------------------------------------
