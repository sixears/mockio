{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module FPath.File2
  ( AsFile( _File ), File(..)

  , tests
  )
where

-- base --------------------------------

import Data.Bifunctor  ( first )
import Data.Bool       ( Bool( False, True ) )
import Data.Either     ( Either( Right ) )
import Data.Eq         ( Eq )
import Data.Function   ( ($), (&), const, id )
import Data.Maybe      ( Maybe( Just, Nothing ) )
import Data.String     ( String )
import Data.Typeable   ( Proxy( Proxy ), TypeRep, typeRep )
import System.Exit     ( ExitCode )
import System.IO       ( IO )
import Text.Show       ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ), Textual( textual )
                     , fromString, toString, toText )

-- lens --------------------------------

import Control.Lens.Iso     ( Iso', iso )
import Control.Lens.Lens    ( lens )
import Control.Lens.Prism   ( Prism', prism' )

-- more-unicode-symbols ----------------

import Data.MoreUnicode.Functor    ( (⊳) )
import Data.MoreUnicode.Lens       ( (⊣), (⫣), (⊢), (⊩), (##) )
import Data.MoreUnicode.Natural    ( ℕ )
import Data.MoreUnicode.Semigroup  ( (◇) )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (∤) )
import Data.MoreUnicode.Lens         ( (⫥), (⩼) )

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

import FPath.AbsDir            ( absdir, root )
import FPath.AbsFile2          ( AbsFile, AsAbsFile( _AbsFile ), absfile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.AsFilePath2       ( AsFilePath( filepath )
                               , AsFilePath'( filepath' ) )
import FPath.Basename          ( Basename( basename, updateBasename) )
import FPath.Dir2              ( Dir( DirA, DirR ) )
import FPath.DirType           ( DirTypeC( DirType ) )
import FPath.Error.FPathError  ( AsFPathError, FPathError, __FPathEmptyE__ )
import FPath.FileLike2         ( FileLike( (⊙), addExt, dir, dirfile, file, ext
                                         , splitExt, updateExt ), IsFile )
import FPath.Parent            ( HasParent( parent )
                               , HasParentMay( parentMay, parents ) )
import FPath.Parseable         ( Parseable( parse ) )
import FPath.PathComponent     ( PathComponent, pc, toUpper )
import FPath.RelDir            ( reldir )
import FPath.RelFile2          ( AsRelFile( _RelFile ), RelFile, relfile )
import FPath.RelType           ( RelTypeC( RelType ) )
import FPath.T.FPath.TestData  ( af1, af2, af3, af4, rf1, rf2, rf3, rf4 )

-------------------------------------------------------------------------------

data File = FileA AbsFile | FileR RelFile
  deriving (Eq, Show)

instance IsFile File

--------------------

class AsFile α where
  _File ∷ Prism' α File

instance AsFile File where
  _File = id

--------------------

instance AsFilePath' File where
  filepath' = prism' (\ case FileA f → f ⫥ filepath'; FileR f → f ⫥ filepath')
                     (\ fp → case headDef '/' fp of
                               '/' → (FileA ⊳ fp ⩼ filepath')
                               _   → (FileR ⊳ fp ⩼ filepath')
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
                             [ testGet "/r.e"       af1f
                             , testGet "/r/p.x"     af2f
                             , testGet "/p/q/r.mp3" af3f
                             , testGet "/.x"        af4f
                             , testGet "r.e"        rf1f
                             , testGet "r/p.x"      rf2f
                             , testGet "p/q/r.mp3"  rf3f
                             , testGet ".x"         rf4f
                             ]
                , testGroup "set"
                             [ testSet af1f "/r.e"
                             , testSet af2f "/r/p.x"
                             , testSet af3f "/p/q/r.mp3"
                             , testSet af4f "/.x"
                             , testSet rf1f "r.e"
                             , testSet rf2f "r/p.x"
                             , testSet rf3f "p/q/r.mp3"
                             , testSet rf4f ".x"
                             ]
                ]

--------------------

instance AsAbsFile File where
  _AbsFile ∷ Prism' File AbsFile
  _AbsFile = prism' FileA (\ case (FileA d) → Just d; _ → Nothing)

--------------------

instance AsRelFile File where
  _RelFile ∷ Prism' File RelFile
  _RelFile = prism' FileR (\ case (FileR d) → Just d; _ → Nothing)

--------------------

instance RelTypeC File where
  type RelType File = RelFile

--------------------

instance DirTypeC File where
  type DirType File = Dir

--------------------

instance Basename File where
  basename (FileA fn) = basename fn
  basename (FileR fn) = basename fn
  updateBasename u (FileA fn) = FileA $ updateBasename u fn
  updateBasename u (FileR fn) = FileR $ updateBasename u fn

----------

basenameTests ∷ TestTree
basenameTests =
  testGroup "basename"
            [ testCase "af1f" $ [relfile|r.e|]   ≟ basename af1f
            , testCase "af2f" $ [relfile|p.x|]   ≟ basename af2f
            , testCase "af3f" $ [relfile|r.mp3|] ≟ basename af3f
            , testCase "af4f" $ [relfile|.x|]    ≟ basename af4f
            , testCase "rf1f" $ [relfile|r.e|]   ≟ basename rf1f
            , testCase "rf2f" $ [relfile|p.x|]   ≟ basename rf2f
            , testCase "rf3f" $ [relfile|r.mp3|] ≟ basename rf3f
            , testCase "rf4f" $ [relfile|.x|]    ≟ basename rf4f
            ]

----------

updateBasenameTests ∷ TestTree
updateBasenameTests =
  let test input expect =
        testCase (toString input) $ expect ≟ updateBasename toUpper input
   in testGroup "updateBasename"
            [ test af1f (FileA [absfile|/R.E|])
            , test af2f (FileA [absfile|/r/P.X|])
            , test af3f (FileA [absfile|/p/q/R.MP3|])
            , test af4f (FileA [absfile|/.X|])
            , test rf1f (FileR [relfile|R.E|])
            , test rf2f (FileR [relfile|r/P.X|])
            , test rf3f (FileR [relfile|p/q/R.MP3|])
            , test rf4f (FileR [relfile|.X|])
            ]

--------------------

instance HasParent File where
  parent = lens (\ case FileA f → DirA (f ⊣ parent)
                        FileR f → DirR (f ⊣ parent))
                (\ f → (\ case (DirA d) → FileA (d ⫻ basename f)
                               (DirR d) → FileR (d ⫻ basename f)))

----------

parentTests ∷ TestTree
parentTests =
  let getTest expect input = testCase (toString input) $ expect ≟ input ⊣ parent
      d ~~ d' = d & parent ⊢ d'
      setTest expect got = testCase (toString expect) $ expect ≟ got
   in testGroup "parent"
                [ testGroup "get"
                            [ getTest (DirA root) af1f
                            , getTest (DirA [absdir|/r/|]) af2f
                            , getTest (DirA [absdir|/r/|]) af2f
                            , getTest (DirA [absdir|/p/q/|]) af3f
                            , getTest (DirA [absdir|/|]) af4f
                            , getTest (DirR [reldir|./|]) rf1f
                            , getTest (DirR [reldir|r/|]) rf2f
                            , getTest (DirR [reldir|p/q/|]) rf3f
                            , getTest (DirR [reldir|./|]) rf4f
                            ]

                , testGroup "set"
                            [ setTest (FileA [absfile|/r/r.e|])
                                      (af1f ~~ DirA [absdir|/r/|])
                            , setTest (FileA [absfile|/p.x|])
                                      (af2f ~~ DirA root)
                            , setTest (FileR [relfile|r.mp3|])
                                      (af3f ~~ DirR [reldir|./|])
                            , setTest (FileR [relfile|q/p/.x|])
                                      (af4f ~~ DirR [reldir|q/p/|])
                            , setTest (FileR [relfile|q/p/r.e|])
                                      (rf1f ~~ DirR [reldir|q/p/|])
                            , setTest (FileR [relfile|p.x|])
                                      (rf2f ~~ DirR [reldir|./|])
                            , setTest (FileA [absfile|/p/r.mp3|])
                                      (rf3f ~~ DirA [absdir|/p/|])
                            , setTest (FileA [absfile|/.x|]) (rf4f ~~ DirA root)
                            ]
            ]

--------------------

instance HasParentMay File where
  parentMay = lens get set
              where get ∷ File → Maybe Dir
                    get (FileA f) = Just (DirA $ f ⊣ parent)
                    get (FileR f) = Just (DirR $ f ⊣ parent)
                    set ∷ File → Maybe Dir → File
                    set f@(FileA _) Nothing  = f & parent ⊢ DirA root
                    set f@(FileA _) (Just d) = f & parent ⊢ d
                    set f@(FileR _) Nothing  = f & parent ⊢ DirR [reldir|./|]
                    set f@(FileR _) (Just d) = f & parent ⊢ d

----------

parentMayGetTests ∷ TestTree
parentMayGetTests =
  let getTest expect input =
        testCase (toString input) $ Just expect @=? input ⊣ parentMay
   in testGroup "get"
                [ getTest (DirA root) af1f
                , getTest (DirA [absdir|/r/|]) af2f
                , getTest (DirA [absdir|/p/q/|]) af3f
                , getTest (DirA [absdir|/|]) af4f
                , getTest (DirR [reldir|./|]) rf1f
                , getTest (DirR [reldir|r/|]) rf2f
                , getTest (DirR [reldir|p/q/|]) rf3f
                , getTest (DirR [reldir|./|]) rf4f
                ]

----------

parentMaySetTests ∷ TestTree
parentMaySetTests =
  let -- (~~) ∷ α → α → α
      -- set d's parent to Just d'
      d ~~ d' = d & parentMay ⊩ d'
      -- set d's parent to d'
      d *~ d' = d & parentMay ⊢ d'
      setTest expect got = testCase (toString expect ⊕ " ~~ " ⊕ toString got) $
                             expect @=? got
      ad0d  = DirA root
      adrd  = DirA [absdir|/r/|]
      adstd = DirA [absdir|/s/t/|]
      rd0d  = DirR [reldir|./|]
      rdrd  = DirR [reldir|r/|]
      rdstd = DirR [reldir|s/t/|]
   in testGroup "set"
                [ testGroup "abs/abs"
                            [ setTest af1f (af1f ~~ ad0d)
                            , setTest af1f (af1f *~ Nothing)
                            , setTest (FileA [absfile|/r/r.e|]) (af1f ~~ adrd)
                            , setTest (FileA [absfile|/p.x|]) (af2f *~ Nothing)
                            , setTest (FileA [absfile|/p.x|]) (af2f ~~ ad0d)
                            , setTest (FileA [absfile|/r.mp3|]) (af3f ~~ ad0d)
                            , setTest (FileA [absfile|/s/t/r.mp3|])
                                      (af3f ~~ adstd)
                            , setTest (FileA [absfile|/r/.x|])
                                      (af4f ~~ adrd)
                            , setTest (FileA [absfile|/s/t/.x|])
                                      (af4f ~~ adstd)
                            ]

                , testGroup "rel/rel"
                            [ setTest rf1f (rf1f ~~ rd0d)
                            , setTest rf1f (rf1f *~ Nothing)
                            , setTest (FileR [relfile|r/r.e|]) (rf1f ~~ rdrd)
                            , setTest (FileR [relfile|p.x|]) (rf2f *~ Nothing)
                            , setTest (FileR [relfile|p.x|]) (rf2f ~~ rd0d)
                            , setTest (FileR [relfile|r.mp3|]) (rf3f ~~ rd0d)
                            , setTest (FileR [relfile|s/t/r.mp3|])
                                      (rf3f ~~ rdstd)
                            , setTest (FileR [relfile|r/.x|]) (rf4f ~~ rdrd)
                            , setTest (FileR [relfile|s/t/.x|]) (rf4f ~~ rdstd)
                            ]

                , testGroup "abs/rel"
                            [ setTest rf1f (af1f ~~ rd0d)
                            , setTest af1f (af1f *~ Nothing)
                            , setTest (FileR [relfile|r/r.e|]) (af1f ~~ rdrd)
                            , setTest (FileA [absfile|/p.x|]) (af2f *~ Nothing)
                            , setTest (FileR [relfile|p.x|]) (af2f ~~ rd0d)
                            , setTest (FileR [relfile|r.mp3|]) (af3f ~~ rd0d)
                            , setTest (FileR [relfile|s/t/r.mp3|])
                                      (af3f ~~ rdstd)
                            , setTest (FileR [relfile|r/.x|]) (af4f ~~ rdrd)
                            , setTest (FileR [relfile|s/t/.x|]) (af4f ~~ rdstd)
                            ]

                , testGroup "rel/abs"
                            [ setTest af1f (rf1f ~~ ad0d)
                            , setTest rf1f (rf1f *~ Nothing)
                            , setTest (FileA [absfile|/r/r.e|]) (rf1f ~~ adrd)
                            , setTest (FileR [relfile|p.x|]) (rf2f *~ Nothing)
                            , setTest (FileA [absfile|/p.x|]) (rf2f ~~ ad0d)
                            , setTest (FileA [absfile|/r.mp3|]) (rf3f ~~ ad0d)
                            , setTest (FileA [absfile|/s/t/r.mp3|])
                                      (rf3f ~~ adstd)
                            , setTest (FileA [absfile|/r/.x|]) (rf4f ~~ adrd)
                            , setTest (FileA [absfile|/s/t/.x|]) (rf4f ~~ adstd)
                            ]
                ]

parentMayTests ∷ TestTree
parentMayTests = testGroup "parentMay" [ parentMayGetTests, parentMaySetTests ]

----------

parentsTests ∷ TestTree
parentsTests =
  let parentsTest expect input =
        testCase (toString input) $ expect @=? parents input
      a0d  = DirA root
      ard  = DirA [absdir|/r/|]
      apd  = DirA [absdir|/p/|]
      apqd = DirA [absdir|/p/q/|]
      r0d  = DirR [reldir|./|]
      rrd  = DirR [reldir|r/|]
      rpd  = DirR [reldir|p/|]
      rpqd = DirR [reldir|p/q/|]
   in testGroup "parents"
                [ parentsTest [ a0d ]            af1f
                , parentsTest [ a0d, ard ]       af2f
                , parentsTest [ a0d, apd, apqd ] af3f
                , parentsTest [ a0d ]            af4f
                , parentsTest [ r0d ]            rf1f
                , parentsTest [ r0d, rrd ]       rf2f
                , parentsTest [ r0d, rpd, rpqd ] rf3f
                , parentsTest [ r0d ]            rf4f
                ]

--------------------

instance Printable File where
  print (FileA f) = print f
  print (FileR f) = print f

----------

instance Textual File where
  textual = FileA ⊳ textual ∤ FileR ⊳ textual

--------------------

instance AsFilePath File where
  filepath = prism' toString fromString

--------------------

filepathTests ∷ TestTree
filepathTests =
  let nothin' = Nothing ∷ Maybe File
      fail s  = testCase s $ nothin' @=? s ⩼ filepath
   in testGroup "filepath"
            [ testCase "af1" $ "/r.e"       ≟ af1f ## filepath
            , testCase "af2" $ "/r/p.x"     ≟ af2f ## filepath
            , testCase "af3" $ "/p/q/r.mp3" ≟ af3f ## filepath
            , testCase "af4" $ "/.x"        ≟ af4f ## filepath

            , testCase "af1" $ Just af1f @=? "/r.e"       ⩼ filepath
            , testCase "af2" $ Just af2f @=? "/r/p.x"     ⩼ filepath
            , testCase "af3" $ Just af3f @=? "/p/q/r.mp3" ⩼ filepath
            , testCase "af4" $ Just af4f @=? "/.x"        ⩼ filepath

            , testCase "rf1" $ "r.e"       ≟ rf1f ## filepath
            , testCase "rf2" $ "r/p.x"     ≟ rf2f ## filepath
            , testCase "rf3" $ "p/q/r.mp3" ≟ rf3f ## filepath
            , testCase "rf4" $ ".x"        ≟ rf4f ## filepath

            , testCase "rf1" $ Just rf1f @=? "r.e"       ⩼ filepath
            , testCase "rf2" $ Just rf2f @=? "r/p.x"     ⩼ filepath
            , testCase "rf3" $ Just rf3f @=? "p/q/r.mp3" ⩼ filepath
            , testCase "rf4" $ Just rf4f @=? ".x"        ⩼ filepath

            , fail "/etc/"
            , fail "etc/"
            , fail "etc/pam.d/"
            , fail "/etc/pam.d/"
            , fail "etc//pam.d/"
            , fail "/\0etc"
            , fail "/etc\0"
            , fail "/e\0c"
            , fail "etc/pam.d/"
            , fail "/etc//pam.d/"
            , fail "\0etc"
            , fail "etc\0"
            , fail "e\0c"
            ]

----------------------------------------

instance FileLike File where
  dirfile ∷ Iso' File (Dir, PathComponent)
  dirfile = iso (\ case FileA a → first DirA (a ⊣ dirfile)
                        FileR r → first DirR (r ⊣ dirfile))
                (\ case (DirA a, c) → FileA ((a,c) ⫣ dirfile)
                        (DirR r, c) → FileR ((r,c) ⫣ dirfile))

fileLikeFileTests ∷ TestTree
fileLikeFileTests =
  let (~~) ∷ File → PathComponent → File
      f ~~ d' = f & file ⊢ d'
   in testGroup "file"
                [ testCase "rf3"       $ [pc|r.mp3|] ≟ rf3f ⊣ file
                , testCase "rf4"       $ [pc|.x|]    ≟ rf4f ⊣ file
                , testCase "af3"       $ [pc|r.mp3|] ≟ af3f ⊣ file
                , testCase "af4"       $ [pc|.x|]    ≟ af4f ⊣ file

                , testCase "af3 → a0"  $
                    FileA [absfile|/p/q/foo|] ≟ af3f ~~ [pc|foo|]
                , testCase "af2 → a1"  $
                    af2f ≟ af2f ~~ [pc|p.x|]
                , testCase "rf1 → a2"  $
                    FileR [relfile|.z|] ≟ rf1f ~~ [pc|.z|]
                ]

fileLikeDirTests ∷ TestTree
fileLikeDirTests =
  let (~~) ∷ File → Dir → File
      f ~~ d' = f & dir ⊢ d'
   in testGroup "dir"
                [ testCase "rf3"      $ DirR [reldir|p/q/|]  ≟ rf3f ⊣ dir
                , testCase "rf4"      $ DirR [reldir|./|]    ≟ rf4f ⊣ dir
                , testCase "af3"      $ DirA [absdir|/p/q/|] ≟ af3f ⊣ dir
                , testCase "af4"      $ DirA [absdir|/|]     ≟ af4f ⊣ dir

                , testCase "rf3 → a0" $
                    FileR [relfile|s/r.mp3|] ≟ af3f ~~ DirR [reldir|s/|]
                , testCase "af2 → a1" $
                    rf2f                ≟ rf2f ~~ DirR [reldir|r/|]
                , testCase "af1 → a2" $
                    FileA [absfile|/p.x|]    ≟ af2f ~~ DirA [absdir|/|]
                , testCase "af1 → a2" $
                    FileA [absfile|/q/p/.x|] ≟ rf4f ~~ DirA [absdir|/q/p/|]
                ]

fileLikeAddExtTests ∷ TestTree
fileLikeAddExtTests =
  testGroup "addExt"
    [ testCase "foo.bar" $
        FileA [absfile|/foo.bar|]     ≟ addExt (FileA [absfile|/foo|]) [pc|bar|]
    , testCase "r.e.bar" $
        FileA [absfile|/r.e.bar|]     ≟ af1f ⊙ [pc|bar|]
    , testCase "f.o.b.r" $
        FileR [relfile|p/q/r.mp3.b.r|]≟ rf3f ⊙ [pc|b.r|]
    ]

fileLikeSplitExtTests ∷ TestTree
fileLikeSplitExtTests =
  testGroup "splitExt"
    [ testCase "foo/bar" $
            (FileR [relfile|foo/bar|], Nothing)
        @=? splitExt (FileR [relfile|foo/bar|])
    , testCase "r/p.x"   $
        (FileR [relfile|r/p|],Just [pc|x|]) @=? splitExt rf2f
    , testCase "f.x/g.y" $
            (FileA [absfile|/f.x/g|], Just [pc|y|])
        @=? splitExt (FileA [absfile|/f.x/g.y|])
    , testCase "f.x/g"   $
            (FileA [absfile|/f.x/g|], Nothing)
        @=? splitExt (FileA [absfile|/f.x/g|])
    ]

fileLikeExtGetterTests ∷ TestTree
fileLikeExtGetterTests =
  testGroup "getter" [ testCase "foo.z/bar.x" $
                         Just [pc|x|] @=? ext(FileR [relfile|foo.z/bar.x|])
                     , testCase "foo/bar" $
                         Nothing @=? ext (FileA [absfile|/foo/bar|])
                     , testCase "g/f.b.x.baz"  $
                         Just [pc|baz|] @=? ext (FileA [absfile|/g/f.b.x.baz|])
                     ]

fileLikeExtSetterTests ∷ TestTree
fileLikeExtSetterTests =
  testGroup "setter"
    [ testCase "foo.bar -> foo.baz" $
          FileR [relfile|p/foo.baz|]
        ≟ FileR (updateExt (const [pc|baz|]) [relfile|p/foo.bar|])
    , testCase "/foo.x/bar -> /foo.x/bar" $
          FileA [absfile|/foo.x/bar|]
        ≟ FileA (updateExt (const [pc|baz|]) [absfile|/foo.x/bar|])
    , testCase "foo -> foo" $
        FileA [absfile|/foo|] ≟ FileA (updateExt (const [pc|baz|]) [absfile|/foo|])
    , testCase "g/foo. -> g/foo." $
          FileA [absfile|/g/foo.|]
        ≟ FileA (updateExt (const [pc|baz|]) [absfile|/g/foo.|])
    ]

fileLikeExtAdjusterTests ∷ TestTree
fileLikeExtAdjusterTests =
  testGroup "adjuster"
    [ testCase ".baz -> .BAR" $
        FileR [relfile|g/fo.BA|] ≟ FileR (updateExt toUpper [relfile|g/fo.ba|])
    , testCase ".x.b -> .x.B" $
        FileR [relfile|f.x.B|]   ≟ FileR (updateExt toUpper [relfile|f.x.b|])
    , testCase ".x -> .xy"    $
        FileA [absfile|/f.xy|] ≟ FileA (updateExt (◇ [pc|y|]) [absfile|/f.x|])
    , testCase ".    -> ."    $
        FileA [absfile|/fo.|]  ≟ FileA (updateExt (◇ [pc|y|]) [absfile|/fo.|])
    ]

fileLikeTests ∷ TestTree
fileLikeTests =
  testGroup "FileLike" [ fileLikeFileTests, fileLikeDirTests
                       , fileLikeAddExtTests, fileLikeSplitExtTests
                       , fileLikeExtGetterTests, fileLikeExtSetterTests
                       , fileLikeExtAdjusterTests
                       ]

------------------------------------------------------------

fileT ∷ TypeRep
fileT = typeRep (Proxy ∷ Proxy File)

instance Parseable File where
  parse ∷ (AsFPathError ε, MonadError ε η, Printable τ) ⇒ τ → η File
  parse (toText → t) =
    case null t of
      True → __FPathEmptyE__ fileT
      False → case head t of
                '/' → FileA ⊳ parse t
                _   → FileR ⊳ parse t

parseFileTests ∷ TestTree
parseFileTests =
  let success d f t = testCase t $ Right (d ## f) @=? parse @File @FPathError t
   in testGroup "parseFile"
                [ success [absfile|/etc|]  _AbsFile "/etc"
                , success [relfile|etc|]   _RelFile "etc"
                ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

-- test data ---------------------------

af1f ∷ File
af1f = FileA af1

af2f ∷ File
af2f = FileA af2

af3f ∷ File
af3f = FileA af3

af4f ∷ File
af4f = FileA af4

rf1f ∷ File
rf1f = FileR rf1

rf2f ∷ File
rf2f = FileR rf2

rf3f ∷ File
rf3f = FileR rf3

rf4f ∷ File
rf4f = FileR rf4

----------------------------------------

tests ∷ TestTree
tests = testGroup "FPath.File" [ basenameTests, asFilePath'Tests, parentTests
                               , parentMayTests, parentsTests
                               , parseFileTests, fileLikeTests, filepathTests
                               , updateBasenameTests
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
