{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module TastyPlus2
  ( assertListEq, assertListCmp, assertListEqIO, assertListCmpIO
  , withResource2, withResource2' )
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

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Monad    ( (≫) )

-- natural-plus ------------------------

import Natural  ( length )

-- safe --------------------------------

import Safe  ( atMay )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, assertBool, assertFailure, testCase )

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

assertCmp' ∷ HasCallStack ⇒
             (α → Text) → (β → Text) → (α → β → 𝔹) → α → Maybe β → Assertion
assertCmp' toTa _ _ expected Nothing =
       assertFailure ("expected: " ⊕ toString (toTa expected)
                                   ⊕ "\nbut got Nothing")
assertCmp' toTa toTb cmp expected (Just got) =
  let toSa = toString ∘ toTa
      toSb = toString ∘ toTb
   in -- equalize prefix lengths to make it easier to diff strings, etc.
       assertBool ("expected: " ⊕ toSa expected ⊕ "\nbut got : " ⊕ toSb got)
                  (cmp expected got)

{- | Compare two lists for compatibility, with customized, itemized testing.
     We take the inputs as IO to allow for, well, IO.
 -}
assertListCmpIO ∷ (Foldable ψ, Foldable φ, Printable σ, HasCallStack) ⇒
                    (α → Text) → (β → Text) → (α → β → 𝔹) → σ → ψ α → IO (φ β)
                  → TestTree
assertListCmpIO toTa toTb cmp name (toList → expect) (fmap toList → got) =
  let lCheck e g =
        assertBool ("length " ⊕ show g ⊕ " did not match expected " ⊕ show e)
                   (e ≡ g)
      lengthCheck e g = lCheck (length e) (length g)
      assertItem (i,e) =
        testCase (show i)
                 (got ≫ \ g → assertCmp' toTa toTb cmp e (atMay g i))

   in testGroup (toString name) $
          testCase "count" (got ≫ lengthCheck expect)
        : (assertItem ⊳ zip [0..] expect)

{- | Compare two lists for equality, with itemized testing and IO. -}
assertListEqIO' ∷ (Foldable ψ, Foldable φ, Eq α, Printable σ, HasCallStack) ⇒
                  (α → Text) → σ → ψ α → IO (φ α) → TestTree
assertListEqIO' toT = assertListCmpIO toT toT (≡)

assertListEqIO ∷ (Foldable ψ, Foldable φ, Eq α, Printable α, HasCallStack) ⇒
                Text → ψ α → IO (φ α) → TestTree
assertListEqIO = assertListEqIO' toText


{- | Compare two lists for compatibility, with itemized testing. -}
assertListCmp ∷ (Foldable ψ, Foldable φ, Printable σ, HasCallStack) ⇒
                  (α → Text) → (β → Text) → (α → β → 𝔹) → σ → ψ α → φ β
                 → TestTree
assertListCmp toTa toTb cmp name exp got =
  assertListCmpIO toTa toTb cmp name exp (return got)

{- | Compare two lists for equality, with itemized testing. -}
assertListEq ∷ (Eq α, Printable α, Foldable ψ, Foldable φ, HasCallStack) ⇒
               Text → ψ α → φ α → TestTree
assertListEq name exp got = assertListEqIO name exp (return got)

-- that's all, folks! ----------------------------------------------------------
