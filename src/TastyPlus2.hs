{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module TastyPlus2
  ( assertListEq, assertListEqIO, withResource2, withResource2' )
where

import TastyPlus  ( withResource' )

-- base --------------------------------

import Control.Monad  ( return )
import Data.Eq        ( Eq )
import Data.Foldable  ( Foldable( toList ) )
import Data.Function  ( ($) )
import Data.Functor   ( fmap )
import Data.List      ( zip )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import GHC.Stack      ( HasCallStack )
import System.IO      ( IO )
import Text.Show      ( show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- fluffy ------------------------------

import Fluffy.Foldable  ( length )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫) )

-- safe --------------------------------

import Safe  ( atMay )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, testCase )

-- text --------------------------------

import Data.Text  ( Text )

--------------------------------------------------------------------------------

withResource2 ∷ IO α → (α → IO()) → IO β → (β → IO ()) → (IO α → IO β →TestTree)
              → TestTree
withResource2 gain lose gain' lose' ts =
  withResource gain lose (\ x → withResource gain' lose' (\ x' → ts x x'))

withResource2' ∷ IO α → IO β → (IO α → IO β → TestTree)
              → TestTree
withResource2' gain gain' ts =
  withResource' gain (\ x → withResource' gain' (\ x' → ts x x'))

assertEq' ∷ (Eq t, HasCallStack) ⇒ (t → Text) → t → t → Assertion
assertEq' toT expected got =
  let toS = toString ∘ toT
   in -- equalize prefix lengths to make it easier to diff strings, etc.
       assertBool ("expected: " ⊕ toS expected ⊕ "\nbut got : " ⊕ toS got)
                  (got ≡ expected)

{- | Compare two lists for equality, with itemized testing.  We take the inputs
     as IO to allow for, well, IO.
 -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ, HasCallStack) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → TestTree
assertListEqIO' toT name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) =
        testCase (show i) (got ≫ \ g → assertEq' toT' (Just e) (atMay g i))
      toT' Nothing  = "Nothing"
      toT' (Just a) = "Just " ⊕ toT a

   in testGroup (toString name) $
          testCase "count" (got ≫ lengthCheck expect)
        : (assertItem ⊳ zip [0..] expect)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α, HasCallStack) ⇒
                Text → ψ α → IO (φ α) → TestTree
assertListEqIO = assertListEqIO' toText

-- | compare two lists for equality, with itemized testing
assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ, HasCallStack) ⇒
               Text → ψ α → φ α → TestTree
assertListEq name exp got = assertListEqIO name exp (return got)

-- that's all, folks! ----------------------------------------------------------
