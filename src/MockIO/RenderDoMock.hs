{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MockIO.RenderDoMock
  ( renderWithDoMock )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- log-plus ----------------------------

import Log.LogEntry       ( LogEntry, attrs )
import Log.LogRenderOpts  ( LogR )

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

renderWithDoMock ∷ HasDoMock τ ⇒ LogR τ

renderWithDoMock f m = if m ⊣ attrs ∘ doMock ≡ DoMock
                           then annotate italicized $ f m
                           else f m

-- that's all, folks! ----------------------------------------------------------
