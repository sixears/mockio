{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Data.MoreUnicode.Doc
  ( (⊞) )
where
-- Prettyprinter -----------------------

import Data.Text.Prettyprint.Doc  ( Doc, (<+>) )

--------------------------------------------------------------------------------

infixr 5 ⊞
-- hsep
(⊞) ∷ Doc α → Doc α → Doc α
(⊞) = (<+>)

-- that's all, folks! ----------------------------------------------------------
