{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}

module Log.Equish
  ( Equish( (≃) ) )
where

-- base --------------------------------

import Data.Bool  ( Bool )

--------------------------------------------------------------------------------

{- | Approximately equal; FOR TESTING ONLY -}
infix 4 ≃
class Equish α where
  (≃) ∷ α → α → Bool

-- that's all, folks! ----------------------------------------------------------
