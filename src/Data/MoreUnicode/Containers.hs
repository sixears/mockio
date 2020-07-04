{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE UnicodeSyntax #-}

module Data.MoreUnicode.Containers
  ( (∈) )
where

-- containers --------------------------

import qualified  Data.Map  as  Map

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )

--------------------------------------------------------------------------------

(∈) ∷ ∀ κ α . Ord κ ⇒ κ → Map.Map κ α → 𝔹
(∈) = Map.member

-- that's all, folks! ----------------------------------------------------------
