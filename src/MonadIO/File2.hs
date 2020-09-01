{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

module MonadIO.File2
  ( hClose
  , withFile, withFileT, System.IO.IOMode(..)

  , access, stat, writable

  , fileWritable

  , readFileBinary, readHandleBinary, writeFileBinary

  , getContentsUTF8, hGetContentsUTF8, readFileUTF8, readFUTF8, writeFileUTF8

  , getContentsUTF8Lenient, hGetContentsUTF8Lenient, readFileUTF8Lenient
  , readFUTF8Lenient

  )
where

import Debug.Trace  ( trace, traceShow )

-- base --------------------------------

import qualified  System.IO

import Control.Monad           ( join, return )
import Control.Monad.IO.Class  ( MonadIO, liftIO )
import Data.Bool               ( Bool( False, True ), bool )
import Data.Either             ( Either( Right ) )
import Data.Eq                 ( Eq )
import Data.Function           ( ($) )
import Data.Functor            ( fmap )
import Data.Maybe              ( Maybe( Just, Nothing ), fromMaybe )
import Data.String             ( String )
import System.Exit             ( ExitCode )
import System.IO               ( FilePath, Handle, IO
                               , IOMode( ReadMode, WriteMode )
                               , hSetEncoding, stdin, utf8
                               )
import Text.Show               ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- bytestring --------------------------

import qualified Data.ByteString  as  BS

import Data.ByteString  ( ByteString )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- fpath -------------------------------

import FPath.AbsDir       ( absdir )
import FPath.AbsFile      ( absfile )
import FPath.AsFilePath2  ( AsFilePath( filepath ), exterminate, filepath' )
import FPath.DirLike      ( IsDir )
import FPath.File2        ( File )
import FPath.FileLike2    ( FileLike, IsFile )
import FPath.FPath2       ( FPathAs )
import FPath.Parent       ( parentMay )

-- lens --------------------------------

import Control.Lens.Review  ( review )

-- monadio-error -----------------------

import MonadError            ( ѥ )
import MonadError.IO2        ( asIOError, asIOErrorY )
import MonadError.IO.Error2  ( AsIOError, IOError, squashInappropriateTypeT )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor2  ( (⊳), (⊳⊳⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⊣), (⫥) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertRight, runTestsP, runTestsReplay, runTestTree )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text                 ( Text )
import Data.Text.Encoding        ( decodeUtf8With )
import Data.Text.Encoding.Error  ( lenientDecode )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import qualified  System.Posix.Files  as  Files
import System.Posix.Files  ( FileStatus, fileExist, getFileStatus, isDirectory )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `NoFExists`; but "does /etc exist?" where
     /etc exists but is a directory will return `FExists`.  See also `fexists'`.
 -}
fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ) ⇒ τ → μ FExists
-- fileExist throws an InappropriateType IOException if you ask about a file
-- in a non-existent directory.  I think that sucks, and should be a simple
-- False (NoFExists)
fexists f = fromMaybe NoFExists ⩺ squashInappropriateTypeT ∘ asIOError $
              bool NoFExists FExists ⊳ fileExist (f ⫥ filepath)

----------

fexistsTests =
  let testFExists expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists input)) ≫ assertRight (expect @=?)
   in testGroup "fexists"
                [ testFExists FExists   [absdir|/etc/|]
                , testFExists NoFExists [absdir|/nonsuch/|]
                , testFExists NoFExists [absdir|/etc/nonsuch/|]
                , testFExists FExists   [absfile|/etc/passwd|]
                , testFExists NoFExists [absdir|/etc/passwd/|]
                , testFExists NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

--------------------

{- | Does file exist.  Note that "does /etc/passwd/ exist?", where /etc/passwd
     exists but is a file, will return `FExists`.  See also `fexists`.  This is
     more symmetric, since "does /etc exist?" where /etc exists but is a
     directory will return `FExists`; but at a cost of being arguably less
     accurate.
 -}
fexists' ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath τ)⇒ τ → μ FExists
-- fileExist throws an InappropriateType IOException if you ask about a file
-- in a directory that is in reality a file.  I think that sucks, and should be
-- a simple False (NoFExists)
fexists' f = fromMaybe NoFExists ⩺ squashInappropriateTypeT ∘ asIOError $
               bool NoFExists FExists ⊳ fileExist (exterminate $ f ⫥ filepath)

----------

fexists'Tests =
  let testFExists' expect input =
        testCase (toString input) $
          (ѥ @IOError (fexists' input)) ≫ assertRight (expect @=?)
   in testGroup "fexists'"
                [ testFExists' FExists   [absdir|/etc/|]
                , testFExists' NoFExists [absdir|/nonsuch/|]
                , testFExists' NoFExists [absdir|/etc/nonsuch/|]
                , testFExists' FExists   [absfile|/etc/passwd|]
                , testFExists' FExists   [absdir|/etc/passwd/|]
                , testFExists' NoFExists [absfile|/etc/passwd/nonsuch|]
                , testFExists' NoFExists [absdir|/etc/passwd/nonsuch/|]
                ]

----------------------------------------

-- | file stat; returns Nothing if file does not exist
stat ∷ ∀ ε ρ μ . (MonadIO μ, AsFilePath ρ, AsIOError ε, MonadError ε μ) ⇒
       ρ → μ (Maybe FileStatus)
stat f = do
  -- The fexists' introduces a race-condition - bah - but without it, the
  -- stat may fail with an `InappropriateType` IOException when trying to stat
  -- a file in a "directory" that is in reality a file.  I think that sucks, and
  -- want to try that like any other non-existent file.
  fexists' f ≫ \ case
    NoFExists → return Nothing
    FExists   → asIOErrorY ∘ getFileStatus ∘ exterminate $ (f ⫥ filepath)
    
----------

statTests ∷ TestTree
statTests =
  let testStat expect input f =
        testCase (toString input) $
          f (ѥ @IOError (stat input)) ≫ assertRight (expect @=?)
   in testGroup "stat"
                [ testStat (Just True)  [absdir|/etc/|]        (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absfile|/etc/passwd|] (isDirectory ⊳⊳⊳)
                , testStat (Just False) [absdir|/etc/passwd/|] (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/nonsuch|]    (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absfile|/etc/passwd/nonsuch|]
                                                               (isDirectory ⊳⊳⊳)
                , testStat Nothing      [absdir|/nonsuch/|]    (isDirectory ⊳⊳⊳)
                ]

----------------------------------------

withFile ∷ (MonadIO μ, IsFile π, AsIOError ε, MonadError ε μ) ⇒
           π → IOMode → (Handle → IO ω) → μ ω
withFile fn mode io = asIOError $ System.IO.withFile (fn ⫥ filepath) mode io

withFileT ∷ (MonadIO μ, IsFile π, AsIOError ε, MonadError ε μ) ⇒
            π → IOMode → (Handle → ExceptT ε IO ω) → μ ω
withFileT fn mode io =
  join ∘ asIOError $ System.IO.withFile (fn ⫥ filepath) mode (\ h → ѥ (io h))

----------------------------------------

-- cribbed shamelessly from RIO.Prelude.IO

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Throw an exception on invalid character.
 -}
readFileUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ Text
readFileUTF8 fn =
  withFile fn ReadMode $ \ h → do
    hSetEncoding h utf8
    TextIO.hGetContents h

--------------------

{- | Read a file in UTF8 encoding using OS-specific line-ending handling.
     Replace any invalid input bytes with the Unicode replacement character
     U+FFFD.
-}
-- plagiarized from https://www.snoyman.com/blog/2016/12/beware-of-readfile
readFileUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ Text
readFileUTF8Lenient = decodeUtf8With lenientDecode ⩺ readFileBinary

----------------------------------------

{- | Read a filehandle of UTF8-encoded text. -}
hGetContentsUTF8 ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                   Handle → μ Text
hGetContentsUTF8 h = asIOError $ do
  hSetEncoding h utf8
  liftIO $ TextIO.hGetContents h

----------------------------------------

{- | Read a filehandle of UTF8-encoded text; be lenient as in
     `readFileUTF8Lenient`. -}
hGetContentsUTF8Lenient ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                          Handle → μ Text
hGetContentsUTF8Lenient = decodeUtf8With lenientDecode ⩺ readHandleBinary

----------------------------------------

{- | Read UTF8-encoded text from `stdin`. -}
getContentsUTF8  ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                   μ Text
getContentsUTF8 = hGetContentsUTF8 stdin

----------------------------------------

{- | Read UTF8-encoded text from `stdin`; be lenient as in
     `readFileUTF8Lenient`. -}
getContentsUTF8Lenient  ∷ ∀ ε μ . (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒
                          μ Text
getContentsUTF8Lenient = hGetContentsUTF8Lenient stdin

----------------------------------------

{- | Read a file, as for `readFileUTF8`; if no file is provided, read `stdin`.
 -}
readFUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Maybe File → μ Text
readFUTF8 Nothing   = getContentsUTF8
readFUTF8 (Just fn) = readFileUTF8 fn

----------------------------------------

{- | Read a file, as for `readFileUTF8Lenient`; if no file is provided,
     read `stdin`. -}
readFUTF8Lenient ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Maybe File → μ Text
readFUTF8Lenient Nothing   = getContentsUTF8Lenient
readFUTF8Lenient (Just fn) = readFileUTF8Lenient fn

----------------------------------------

-- | Same as 'BS.readFile', but generalized to 'MonadIO'
readFileBinary ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ File → μ ByteString
readFileBinary = asIOError ∘ liftIO ∘ BS.readFile ∘ review filepath

----------------------------------------

-- | Same as 'BS.hGetContents', but generalized to 'MonadIO'
readHandleBinary ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                   Handle → μ ByteString
readHandleBinary = asIOError ∘ liftIO ∘ BS.hGetContents

----------------------------------------

-- XXX SHOULD TAKE OVERWRITE OPTION, AND FILE MODE

{- | Write a file in UTF8 encoding using OS-specific line-ending handling. -}
writeFileUTF8 ∷ (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                File → Text → μ ()
writeFileUTF8 fn text =
  withFile fn WriteMode $ \h → do
    hSetEncoding h utf8
    TextIO.hPutStr h text

----------------------------------------

-- XXX SHOULD TAKE OVERWRITE OPTION, AND FILE MODE

-- | Same as 'BS.writeFile', but generalized to 'MonadIO'
writeFileBinary ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒
                  File → ByteString → μ ()
writeFileBinary fn = asIOError ∘ BS.writeFile (fn ⫥ filepath)

----------------------------------------

hClose ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ, MonadIO μ) ⇒ Handle → μ ()
hClose = asIOError ∘ System.IO.hClose

-- fileAccess ----------------------------------------------

data AccessMode = ACCESS_R | ACCESS_WX | ACCESS_RWX
                | ACCESS_W | ACCESS_RX
                | ACCESS_X | ACCESS_RW

access ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
         AccessMode → ρ → μ (Maybe 𝔹)
access mode ((⫥ filepath) → fp) = asIOErrorY $ go mode fp
  where go ∷ AccessMode → FilePath → IO 𝔹
        go ACCESS_R   p = Files.fileAccess (p ⫥ filepath) True  False False
        go ACCESS_W   p = Files.fileAccess (p ⫥ filepath) False True  False
        go ACCESS_X   p = Files.fileAccess (p ⫥ filepath) False False True
        go ACCESS_RW  p = Files.fileAccess (p ⫥ filepath) True  True  False
        go ACCESS_RX  p = Files.fileAccess (p ⫥ filepath) True  False True
        go ACCESS_WX  p = Files.fileAccess (p ⫥ filepath) False True  True
        go ACCESS_RWX p = Files.fileAccess (p ⫥ filepath) True  True  True

{- | Simple shortcut for file (or directory) is writable by this user; `Nothing`
     is returned if file does not exist. -}
writable ∷ ∀ ε ρ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, AsFilePath ρ) ⇒
            ρ → μ (Maybe 𝔹)
writable = access ACCESS_W

----------------------------------------

{- | Is `f` an extant writable file? -}
_isWritableFile ∷ (MonadIO μ, IsFile α,AsFilePath α,MonadError ε μ,AsIOError ε)⇒
                  α → Maybe FileStatus -> μ (Maybe Text)

_isWritableFile f st =
  let rJust = return ∘ Just
   in case st of
        Nothing  → rJust $ [fmt|%T does not exist|] f
        Just stp → if isDirectory stp
                   then rJust $ [fmt|%T is a directory|] f
                   else writable f ≫ \ case
                          Nothing    → rJust $ [fmt|no such file %T|] f
                          Just True  → return Nothing
                          Just False → rJust $ [fmt|cannot write to %T|] f

----------------------------------------

{- | Is `f` an extant writable file? -}
isWritableFile ∷ (MonadIO μ, IsFile α, AsFilePath α,MonadError ε μ,AsIOError ε)⇒
                 α -> μ (Maybe Text)

isWritableFile f = stat f ≫ _isWritableFile f

----------------------------------------

{- | Is `d` an extant writable directory? -}
isWritableDir ∷ ∀ α ε μ .
                (MonadIO μ, IsDir α, AsFilePath α, MonadError ε μ, AsIOError ε)⇒
                α -> μ (Maybe Text)

isWritableDir d =
  let rJust = return ∘ Just
   in stat d ≫ \ case
        Nothing  → rJust $ [fmt|%T does not exist|] d
        Just stp → if isDirectory stp
                   then writable d ≫ \ case
                          Nothing    → rJust $ [fmt|no such directory %T|] d
                          Just True  → return Nothing
                          Just False → rJust $ [fmt|cannot write to %T|] d
                   else -- remove trailing '/', since the point is that d is
                        -- not a directory
                        rJust $ [fmt|%s is not a directory|]
                                (exterminate (d ⫥ filepath))

----------

isWritableDirTests ∷ TestTree
isWritableDirTests =
  let testE f e = testCase (toString f) $
                    ѥ (isWritableDir @_ @IOError f) ≫ assertRight (Just e @=?)
      testN f   = testCase (toString f) $
                    ѥ (isWritableDir @_ @IOError f) ≫ assertRight (Nothing @=?)
   in testGroup "isWritableDir"
            [ testN [absdir|/tmp/|]
            , testE [absdir|/nonsuch/|]
                    "/nonsuch/ does not exist"
            , testE [absdir|/nonsuch/passwd/|]
                    "/nonsuch/passwd/ does not exist"
            , testE [absdir|/etc/|]
                    "cannot write to /etc/"
            , testE [absdir|/etc/passwd/|]
                    "/etc/passwd is not a directory"
            ]

----------------------------------------

{- | Test that the given path is a writable (by this user) *file*, or does not
     exist but is in a directory that is writable & executable by this user.
     In case of not writable, some error text is returned to say why.
 -}
fileWritable ∷ ∀ α ε μ .
               (MonadIO μ, IsFile α, AsFilePath α, AsIOError ε, MonadError ε μ)⇒
               α → μ (Maybe Text)
fileWritable fn = do
  stat fn ≫ \ x → case x of
    Just st → _isWritableFile fn (Just st)
    Nothing → -- fn does not exist; does it have a writeable dir parent?
              case fn ⊣ parentMay of
                Just p → do iwd ← isWritableDir p
                            case iwd of
                              Nothing → return Nothing
                              Just e  → return ∘ Just $ [fmt|%t (%T)|] e fn

----------

fileWritableTests ∷ TestTree
fileWritableTests =
  let testE f e = testCase (toString f) $
                    ѥ (fileWritable @_ @IOError f) ≫ assertRight (Just e @=?)
      testE' f e = testCase (toString f) $
                     ѥ (fileWritable @_ @IOError f) ≫ assertRight (e @=?)

   in testGroup "fileWritable"
            [ testE [absfile|/etc/passwd|]
                    "cannot write to /etc/passwd"
            , testE [absfile|/nonsuch/passwd|]
                    "/nonsuch/ does not exist (/nonsuch/passwd)"
            , testE [absfile|/etc/nonsuch|]
                    "cannot write to /etc/ (/etc/nonsuch)"
            , testE [absfile|/etc/passwd/nonsuch|]
                    "/etc/passwd is not a directory (/etc/passwd/nonsuch)"
            , testE [absfile|/etc/pam.d|]
                    "/etc/pam.d is a directory"

            , testE' [absfile|/dev/null|] Nothing
            ]

----------------------------------------

tests ∷ TestTree
tests = testGroup "MonadIO.File" [ fexistsTests, fexists'Tests, statTests
                                 , isWritableDirTests, fileWritableTests ]

--------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------

