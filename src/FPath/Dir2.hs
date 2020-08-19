{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.Dir2
  ( AsDir( _Dir ), Dir(..), DirAs( _Dir' )

  , tests
  )
where

-- base --------------------------------

import Data.Bool        ( Bool( False, True ) )
import Data.Either      ( Either( Right ) )
import Data.Eq          ( Eq )
import Data.Function    ( ($), (&), id )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.String      ( String )
import Data.Typeable    ( Proxy( Proxy ), TypeRep, typeRep )
import System.Exit      ( ExitCode )
import System.IO        ( IO )
import Text.Show        ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣), (⊢), (⊩), (⫥), (⩼), (##) )
import Data.MoreUnicode.Natural      ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- safe --------------------------------

import Safe  ( headDef )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( (≟), runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import Data.Text  ( head, null )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir            ( AbsDir, AsAbsDir( _AbsDir)
                               , AsNonRootAbsDir( _NonRootAbsDir ), NonRootAbsDir
                               , ToAbsDir( toAbsDir )

                               , absdir
                               )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath2       ( AsFilePath( filepath )
                               , AsFilePath'( filepath' ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.DirLike           ( IsDir )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.Parent            ( HasParentMay( parentMay, parents, parents' ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( toUpper )
import FPath.RelDir            ( AsRelDir( _RelDir ), RelDir, reldir )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( etc, pamd, a0, a1, a2, a3, r0, r1, r2, r3, root
                               , wgm )

-------------------------------------------------------------------------------

data Dir = DirA AbsDir | DirR RelDir
  deriving (Eq, Show)

--------------------

instance IsDir Dir

--------------------

class (Printable α, AsFilePath α) ⇒ DirAs α where
  _Dir' ∷ Prism' Dir α
instance DirAs Dir where
  _Dir' = id
instance DirAs AbsDir where
  _Dir' = _AbsDir
instance DirAs RelDir where
  _Dir' = _RelDir

--------------------

class AsDir α where
  _Dir ∷ Prism' α Dir

instance AsDir Dir where
  _Dir = id

--------------------

instance RelTypeC Dir where
  type RelType Dir = RelDir

--------------------

instance DirTypeC Dir where
  type DirType Dir = Dir

--------------------

instance AsAbsDir Dir where
  _AbsDir ∷ Prism' Dir AbsDir
  _AbsDir = prism' DirA (\ case (DirA d) → Just d; _ → Nothing)

--------------------

instance AsNonRootAbsDir Dir where
  _NonRootAbsDir ∷ Prism' Dir NonRootAbsDir
  _NonRootAbsDir = prism' (DirA ∘ toAbsDir)
                          (\ case (DirA d) → d ⩼ _NonRootAbsDir; _ → Nothing)

--------------------

instance AsRelDir Dir where
  _RelDir ∷ Prism' Dir RelDir
  _RelDir = prism' DirR (\ case (DirR d) → Just d; _ → Nothing)

----------------------------------------

instance Printable Dir where
  print (DirA f) = print f
  print (DirR f) = print f

instance Textual Dir where
  textual = DirA ⊳ textual ∤ DirR ⊳ textual

----------------------------------------

instance AsFilePath Dir where
  filepath = prism' toString fromString

--------------------

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe Dir
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "root"  $ "/"           ≟ DirA root    ## filepath
            , testCase "etc"   $ "/etc/"       ≟ DirA etc     ## filepath
            , testCase "pam.d" $ "/etc/pam.d/" ≟ DirA pamd    ## filepath
            , testCase "wgm"   $ "/w/g/M/"     ≟ DirA wgm     ## filepath

            , testCase "/etc/" $ Just etc      @=? "/etc/" ⩼ filepath

            , testCase "r0" $ "./"     ≟ DirR r0 ## filepath
            , testCase "r1" $ "r/"     ≟ DirR r1 ## filepath
            , testCase "r2" $ "r/p/"   ≟ DirR r2 ## filepath
            , testCase "r3" $ "p/q/r/" ≟ DirR r3 ## filepath

            , testCase "r0" $ Just (DirR r0) @=? "./"     ⩼ filepath
            , testCase "r1" $ Just (DirR r1) @=? "r/"     ⩼ filepath
            , testCase "r2" $ Just (DirR r2) @=? "r/p/"   ⩼ filepath
            , testCase "r3" $ Just (DirR r3) @=? "p/q/r/" ⩼ filepath

            , fail "/etc"
            , fail "foo/etc"
            , fail "etc"
            , fail "/etc/pam.d"
            , fail "etc/pam.d"
            , fail "/etc//pam.d/"
            , fail "e/c"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

--------------------

instance AsFilePath' Dir where
  filepath' = prism' (\ case DirA f → f ⫥ filepath'; DirR f → f ⫥ filepath')
                     (\ fp → case headDef '/' fp of
                               '/' → (DirA ⊳ fp ⩼ filepath')
                               _   → (DirR ⊳ fp ⩼ filepath')
                     )

----------

asFilePath'Tests ∷ TestTree
asFilePath'Tests =
  let testGet expect input =
        testCase (toString input) $ expect ≟ input ⫥ filepath'
      testSet expect input =
        testCase (toString input) $ Just expect @=? input ⩼ filepath'
   in testGroup "asFilePath'"
                [ testGroup "get"
                             [ testGet "/"      a0d
                             , testGet "/r"     a1d
                             , testGet "/r/p"   a2d
                             , testGet "/p/q/r" a3d
                             , testGet "."      r0d
                             , testGet "r"      r1d
                             , testGet "r/p"    r2d
                             , testGet "p/q/r"  r3d
                             ]
                , testGroup "set"
                             [ testSet a0d "/"
                             , testSet a1d "/r"
                             -- should still cope with trailing '/'
                             , testSet a2d "/r/p/"
                             , testSet a3d "/p/q/r"
                             , testSet r0d "."
                             , testSet r1d "r"
                             , testSet r2d "r/p"
                             -- should still cope with trailing '/'
                             , testSet r3d "p/q/r/"
                             ]

                ]

--------------------

instance Basename Dir where
  basename (DirA d) = basename d
  basename (DirR d) = basename d
  updateBasename u (DirA d) = DirA $ updateBasename u d
  updateBasename u (DirR d) = DirR $ updateBasename u d

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "a0d" $ r0           ≟ basename a0d
            , testCase "a1d" $ r1           ≟ basename a1d
            , testCase "a2d" $ [reldir|p/|] ≟ basename a2d
            , testCase "a3d" $ r1           ≟ basename a3d
            , testCase "r0d" $ r0           ≟ basename r0d
            , testCase "r1d" $ r1           ≟ basename r1d
            , testCase "r2d" $ [reldir|p/|] ≟ basename r2d
            , testCase "r3d" $ r1           ≟ basename r3d
            ]

----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let
      test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test a0d a0d
            , test a1d (DirA [absdir|/R/|])
            , test a2d (DirA [absdir|/r/P/|])
            , test a3d (DirA [absdir|/p/q/R/|])
            , test r0d (DirR [reldir|./|])
            , test r1d (DirR [reldir|R/|])
            , test r2d (DirR [reldir|r/P/|])
            , test r3d (DirR [reldir|p/q/R/|])
            ]

--------------------

instance HasParentMay Dir where
  parentMay = lens get set
              where get ∷ Dir → Maybe Dir
                    get (DirA d) = DirA ⊳ d ⊣ parentMay
                    get (DirR d) = DirR ⊳ d ⊣ parentMay
                    set ∷ Dir → Maybe Dir → Dir
                    set (DirA d) (Just (DirA p)) =
                      DirA $ d & parentMay ⊢ Just p
                    set (DirA d) (Just (DirR p)) =
                      DirR $ p ⫻ basename d
                    set (DirA d) Nothing =
                      DirA $ d & parentMay ⊢ Nothing
                    set (DirR d) (Just (DirR p)) =
                      DirR $ d & parentMay ⊢ Just p
                    set (DirR d) (Just (DirA p)) =
                      DirA $ p ⫻ basename d
                    set (DirR d) Nothing =
                      DirR $ d & parentMay ⊢ Nothing

----------

parentMayGetTests ∷ TestTree
parentMayGetTests =
  let getTest expect input =
        testCase (toString input) $ expect @=? input ⊣ parentMay
   in testGroup "get"
                [ getTest Nothing                       a0d
                , getTest (Just a0d)                    a1d
                , getTest (Just $ DirA [absdir|/r/|])   a2d
                , getTest (Just $ DirA [absdir|/p/q/|]) a3d
                , getTest Nothing                       r0d
                , getTest (Just $ DirR [reldir|./|])    r1d
                , getTest (Just $ DirR [reldir|r/|])    r2d
                , getTest (Just $ DirR [reldir|p/q/|])  r3d
                ]

----------

parentMaySetTests ∷ TestTree
parentMaySetTests =
  let -- (~~) ∷ α → α → α
      -- set d's parent to Just d'
      d ~~ d' = d & parentMay ⊩ d'
      -- set d's parent to d'
      d *~ d' = d & parentMay ⊢ d'
      setTest expect got = testCase (toString expect) $ expect @=? got
   in testGroup "set"
                [ testGroup "abs/abs"
                            [ -- setting root's parent to root → root
                              setTest a0d (a0d ~~ a0d)
                            , setTest a0d (a0d *~ Nothing)

                              -- setting root's parent to /r/ → /r/
                            , setTest a1d (a0d ~~ a1d)

                              -- setting /r/'s parent to root → /r/
                            , setTest a1d (a1d *~ Nothing)
                            , setTest a1d (a1d ~~ a0d)

                              -- setting /r/p/'s parent to root → /p/
                            , setTest (DirA [absdir|/p/|]) (a2d ~~ a0d)

                              -- setting /r/p/'s parent to /q/s/ → /q/s/p/
                            , setTest (DirA [absdir|/q/s/p/|])
                                      (a2d ~~ DirA [absdir|/q/s/|])

                              -- setting /p/q/r/'s parent to /r/ → /r/r/
                            , setTest (DirA [absdir|/r/r/|]) (a3d ~~ a1d)
                              -- setting /p/q/r/'s parent to /p/q/r/ → /p/q/r/r/
                            , setTest (DirA [absdir|/p/q/r/r/|]) (a3d ~~ a3d)
                            ]

                , testGroup "rel/rel"
                            [ setTest r0d (r0d ~~ r0d)
                            , setTest r0d (r0d *~ Nothing) 
                            , setTest r1d (r0d ~~ r1d)
                            , setTest r1d (r1d *~ Nothing)
                            , setTest r1d (r1d ~~ r0d)
                            , setTest (DirR [reldir|p/|]) (r2d ~~ r0d)
                            , setTest (DirR [reldir|q/s/p/|])
                                      (r2d ~~ DirR [reldir|q/s/|])
                            , setTest (DirR [reldir|r/r/|]) (r3d ~~ r1d)
                            , setTest (DirR [reldir|p/q/r/r/|]) (r3d ~~ r3d)
                            ]

                  -- cross type dirs (abs-rel)  ------------
                , testGroup "abs/rel"
                            [ setTest r0d (a0d ~~ r0d)
                            , setTest r1d (a0d ~~ r1d)
                            , setTest r1d (a1d ~~ r0d)
                            , setTest (DirR [reldir|p/|]) (a2d ~~ r0d)
                            , setTest (DirR [reldir|q/s/p/|])
                                      (a2d ~~ DirR [reldir|q/s/|])
                            , setTest (DirR [reldir|r/r/|]) (a3d ~~ r1d)
                            , setTest (DirR [reldir|p/q/r/r/|]) (a3d ~~ r3d)
                            ]

                  -- cross type dirs (abs-rel)  ------------
                , testGroup "rel/abs"
                            [ setTest a0d (r0d ~~ a0d)
                            , setTest a1d (r0d ~~ a1d)
                            , setTest a1d (r1d ~~ a0d)
                            , setTest (DirA [absdir|/p/|]) (r2d ~~ a0d)
                            , setTest (DirA [absdir|/q/s/p/|])
                                      (r2d ~~ DirA [absdir|/q/s/|])
                            , setTest (DirA [absdir|/r/r/|]) (r3d ~~ a1d)
                            , setTest (DirA [absdir|/p/q/r/r/|]) (r3d ~~ a3d)
                            ]
                ]

----------

parentMayTests ∷ TestTree
parentMayTests = testGroup "parentMay" [ parentMayGetTests, parentMaySetTests ]

----------

parentsTests ∷ TestTree
parentsTests =
  let parentsTest expect input =
        testCase (toString input) $ expect @=? parents input
   in testGroup "parents"
            [ parentsTest []                                                a0d
            , parentsTest [ a0d ]                                           a1d
            , parentsTest [ a0d, DirA [absdir|/r/|] ]                       a2d
            , parentsTest [ a0d, DirA [absdir|/p/|], DirA [absdir|/p/q/|] ] a3d
            , parentsTest []                                                r0d
            , parentsTest [ r0d ]                                           r1d
            , parentsTest [ r0d, DirR [reldir|r/|] ]                        r2d
            , parentsTest [ r0d, DirR [reldir|p/|], DirR [reldir|p/q/|] ]   r3d
            ]

----------

parents'Tests ∷ TestTree
parents'Tests =
  let parents'Test expect input =
        testCase (toString input) $ expect @=? parents' input
   in testGroup "parents'"
            [ parents'Test [ a0d ]                                           a0d
            , parents'Test [ a0d, a1d ]                                      a1d
            , parents'Test [ a0d, DirA [absdir|/r/|], a2d ]                  a2d
            , parents'Test [a0d,DirA [absdir|/p/|],DirA [absdir|/p/q/|],a3d] a3d
            , parents'Test [ r0d ]                                           r0d
            , parents'Test [ r0d, r1d ]                                      r1d
            , parents'Test [ r0d, DirR [reldir|r/|], r2d ]                   r2d
            , parents'Test [ r0d,DirR [reldir|p/|],DirR [reldir|p/q/|],r3d ] r3d
            ]

----------------------------------------

dirT ∷ TypeRep
dirT = typeRep (Proxy ∷ Proxy Dir)

instance Parseable Dir where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η Dir
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ dirT
      False → case head t of
                '/' → DirA ⊳ parse t
                _   → DirR ⊳ parse t

parseDirTests ∷ TestTree
parseDirTests =
  let success d f t = testCase t $ Right (d ## f) @=? parse @Dir @FPathError t
   in testGroup "parseDir"
                [ success [absdir|/|]     _AbsDir "/"
                , success [absdir|/etc/|] _AbsDir "/etc/"
                , success [reldir|./|]    _RelDir "./"
                , success [reldir|etc/|]  _RelDir "etc/"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

a0d ∷ Dir
a0d = DirA a0

a1d ∷ Dir
a1d = DirA a1

a2d ∷ Dir
a2d = DirA a2

a3d ∷ Dir
a3d = DirA a3

r0d ∷ Dir
r0d = DirR r0

r1d ∷ Dir
r1d = DirR r1

r2d ∷ Dir
r2d = DirR r2

r3d ∷ Dir
r3d = DirR r3

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.Dir" [ filepathTests, asFilePath'Tests, basenameTests
                              , updateBasenameTests, parentMayTests
                              , parentsTests, parents'Tests, parseDirTests
                              ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
