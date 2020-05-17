{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE UnicodeSyntax     #-}

{-# OPTIONS_HADDOCK hide #-}

{- | tests for Text.Fmt -}

module Text.T.Fmt2
  ( _test, unitTests )
where

import Prelude ( Double, Float, Int, Integer, (^) )

-- base --------------------------------

import Data.Bifunctor   ( first )
import Data.Either      ( Either( Left, Right ) )
import Data.Eq          ( Eq )
import Data.Function    ( ($) )
import Data.List        ( isInfixOf )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import Data.Monoid      ( (<>) )
import Data.String      ( String, unlines )
import Data.Word        ( Word8 )
import GHC.Stack        ( SrcLoc( SrcLoc ), fromCallSiteList )
import Numeric.Natural  ( Natural )
import System.IO        ( IO )
import Text.Show        ( Show, show )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- parsec ------------------------------

import Text.Parsec.Prim   ( parse )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, defaultMain, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@?=), assertBool, testCase )

-- text --------------------------------

import qualified  Data.Text.Lazy  as  LazyText

import Data.Text  ( Text, intercalate, unpack )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- time --------------------------------

import Data.Time.Clock.POSIX  ( posixSecondsToUTCTime )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import Text.Fmt2  ( ByteFmtBase( B_1024, B_1000 ), Token( Conversion, Str )
                  , conversion, fill, fmt, fmtS, fmtL, fmtT, formatBytes
                  , sprintf, tokens )

-------------------------------------------------------------------------------

data TestToText = TestToText Text

instance Printable TestToText where
  print (TestToText t) = P.text $ "ttt: " <> t

ts :: [TestToText]
ts =  [ TestToText "c", TestToText "b", TestToText "a" ]

-- | run the tests
_test :: IO ()
_test = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [ unitTests ]

-- | unit tests
unitTests :: TestTree
unitTests = testGroup "unitTests" [ hunitGroup ]

hunitGroup :: TestTree
hunitGroup =
  testGroup "CmdLib.Fmt hunit" [ fillTest, convTest, formatBytesTest
                               , tokensTest, sprintfTest, fmtTest ]

convTest :: TestTree
convTest =
  testGroup "conversion" $
    let testConv t expect = testCase t $ parse conversion t t @?= Right expect
      in [ testConv "%t"      (Conversion Nothing Nothing Nothing 't')
         , testConv "%-03.2f" (Conversion (Just (-3,'0')) (Just 2) Nothing 'f')
         ]

formatBytesTest :: TestTree
formatBytesTest =
  let (^^) :: Int -> Int -> Int
      x ^^ y = x ^ y
      testBy :: Int -> Text -> TestTree
      testBy v ex = testCase (show v <> "b") $ formatBytes B_1000 v @?= ex
      testBi :: Int -> Text -> TestTree
      testBi v ex = testCase (show v <> "b") $ formatBytes B_1024 v @?= ex
   in testGroup "formatBytes" $
        [ testBi 0 "0"
        , testBy 0 "0"
        , testBy 500 "500B"
        , testBi 500 "500B"
        , testBy 1000 "1.00kB"
        , testBi 1000 "1000B"
        , testBy 1024 "1.02kB"
        , testBi 1024 "1.00KiB"
        , testBy 5000 "5.00kB"
        , testBi 5000 "4.88KiB"
        , testBy 1000000 "1.00MB"
        , testBi 1000000 "977KiB"
        , testBy 1048576 "1.05MB"
        , testBi 1048576 "1.00MiB"
        , testBy (1024^^3) "1.07GB"
        , testBi (1024^^3) "1.00GiB"
        , testBi (1024^^3) "1.00GiB"
        ]


fillTest :: TestTree
fillTest = testGroup "fill" $
  let testFill  s i = testCase s $ parse fill s s @?= Right i
      -- testFillE s e = testCase s $ first show (parseInt s) @?= Left e
   in [ testFill "-7" (-7, ' ')
      , testFill "7"  (7, ' ')
      ]

cmp :: (Eq a, Show a) => Either String a -> Either String a -> Assertion
cmp (Left l) (Left r) =
  assertBool (unlines ["expected: " <> r, "got: " <> l]) $ r `isInfixOf` l
cmp l r = l @?= r

tokensTest :: TestTree
tokensTest =
  let testTokens :: Text -> Either String [Token] -> TestTree
      testTokens s expect = testCase (unpack s) $
                              first show (tokens s) `cmp` expect
   in testGroup "tokens"
         [ testTokens "just a string"    (Right [ Str "just a string" ])
         , testTokens ""                 (Right [ ])
         , testTokens "percent %% here"  (Right [ Str "percent % here" ])
         , testTokens "percent after %%" (Right [ Str "percent after %" ])
         , testTokens "%%percent before" (Right [ Str "%percent before" ])
         , testTokens "%"                (Left "unexpected end of input")
         , testTokens "%%%"              (Left "unexpected end of input")
         , testTokens "%t%"              (Left "unexpected end of input")
         , testTokens "% %t"             (Left "unexpected \" \"")
         , testTokens "%\t%t"            (Left "unexpected \"\\t\"")
         , testTokens "%t%a"             (Left "unexpected \"a\"")
         , testTokens "my %ttoken"
             (Right [ Str "my ", Conversion Nothing Nothing Nothing 't'
                    , Str "token" ])
         , testTokens "%7t token"
             (Right [ Conversion (Just (7, ' ')) Nothing Nothing 't'
                    , Str " token" ])
         , testTokens "%-7t%%"
             (Right [ Conversion (Just (-7, ' ')) Nothing Nothing 't'
                    , Str "%" ])
         , testTokens "%07t token"
             (Right [ Conversion (Just (7, '0')) Nothing Nothing 't'
                    , Str " token" ])
         , testTokens "%-07t%%"
             (Right [ Conversion (Just (-7, '0')) Nothing Nothing 't'
                    , Str "%" ])
         ]

sprintfTest :: TestTree
sprintfTest =
  testGroup "sprintf"
    [ testCase "foo"     $ $( sprintf "foo"   )          @?= ("foo"    :: Text)
    , testCase "foo%t"   $ $( sprintf "foo%t" )    "bar" @?= ("foobar" :: Text)
    , testCase "%tfoo"   $ $( sprintf "%tfoo" )    "bar" @?= ("barfoo" :: Text)
    , testCase "fo%bzs"  $ $( sprintf "fo%tbz" )   "br"  @?= ("fobrbz" :: Text)

    , testCase "%d"      $ $( sprintf "%d"  ) (  7  :: Int) @?= ("7"   :: Text)
    , testCase "%d (-)"  $ $( sprintf "%d"  ) ((-7) :: Int) @?= ("-7"  :: Text)
    , testCase "%dC"     $ $( sprintf "%dC" ) (  7  :: Int) @?= ("7C"  :: Text)
    , testCase "F%d"     $ $( sprintf "F%d" ) (  7  :: Int) @?= ("F7"  :: Text)
    , testCase "-%d"     $ $( sprintf "-%d" ) (  7  :: Int) @?= ("-7"  :: Text)
    , testCase "-%d"     $ $( sprintf "-%d" ) ((-7) :: Int) @?= ("--7" :: Text)
    ]

fmtTest :: TestTree
fmtTest =
  let (^^) :: Int -> Int -> Int
      x ^^ y = x ^ y
      bar = "bar" ∷ String
      dayOne = posixSecondsToUTCTime 94755600
      cs = fromCallSiteList [("foo", SrcLoc "a" "b" "c" 8 13 21 34),
                             ("bar", SrcLoc "x" "y" "z" 55 89 144 233)]
   in testGroup "fmt"
    [ testCase "-empty-"  $ [fmt||]               @?= ("" :: Text)

    , testCase "foo%tbaz" $ [fmt|foo%tbaz|] "bar" @?= ("foobarbaz" :: Text)
    , testCase "a%3tc"    $ [fmt|a%3tc|]    "b"   @?= ("a  bc" :: Text)
    , testCase "a%-3tc"   $ [fmt|a%-3tc|]   "b"   @?= ("ab  c" :: Text)
    , testCase "a%-03tc"  $ [fmt|a%-03tc|]  "b"   @?= ("ab00c" :: Text)
    , testCase "a%03tc"   $ [fmt|a%03tc|]   "b"   @?= ("a00bc" :: Text)

    , testCase "a%-3sc"   $ [fmt|a%3sc|]    "b"   @?= ("a  bc" :: Text)
    , testCase "a%-2lc"   $ [fmt|a%-3lc|]   "b"   @?= ("ab  c" :: Text)
    , testCase "a%5Lc"   $ [fmt|a%5Lc|] (["b","d"] :: [LazyText.Text])
                                                  @?= ("a  b,dc" :: Text)

    , testCase "foo%Tbaz" $ [fmt|foo%Tbaz|] bar @?= ("foobarbaz" :: String)

    , testCase "a%-3sc"   $ [fmt|n|]                @?= ("n" :: Text)
    , testCase "a%-3sc"   $ [fmt|\n|]               @?= ("\n" :: Text)
    , testCase "a%-3sc"   $ [fmt|a%3s\nc|]    "b"   @?= ("a  b\nc" :: Text)
    , testCase "a%-3sc"   $ [fmt|a%3s\tc|]    "b"   @?= ("a  b\tc" :: Text)
    , testCase "a%-3sc"   $ [fmt|a%3s\\ntc|]    "b"   @?= ("a  b\\ntc" :: Text)
    , testCase "a%-3sc"   $ [fmt|a\t%3s\nc|]    "b"   @?= ("a\t  b\nc" :: Text)


    , testCase "a%-3wc" $ [fmt|a%-3wc|] ("b" :: String) @?= ("a\"b\"c" :: Text)
    , testCase "a%-5wc" $ [fmt|a%-5wc|] ("b" :: String) @?= ("a\"b\"  c"::Text)
    , testCase "a%5wc" $ [fmt|a%5wc|] ("b" :: String) @?= ("a  \"b\"c" :: Text)

    , testCase "a%3dc: 7" $ [fmt|a%3dc|]  (7 :: Int)      @?= ("a  7c" :: Text)
    , testCase "a%3dc:-7" $ [fmt|a%3dc|] (-7 :: Int)      @?= ("a -7c" :: Text)
    , testCase "a%03dc"   $ [fmt|a%03dc|] (7 :: Int)      @?= ("a007c" :: Text)
    , testCase "a%-3dc"   $ [fmt|a%-3dc|] (7 :: Int)      @?= ("a7  c" :: Text)
    , testCase "a%3dc: 7" $ [fmt|a%3nc|]  (7 :: Natural)  @?= ("a  7c" :: Text)
      -- I'm not sure what -03d should be - it would make sense to me to be
      -- '700', but bash & perl both say '7  '

    , testCase "a%3xc"  $ [fmt|a%3xc|]   (175 :: Int)     @?= ("a afc" :: Text)
    , testCase "a%-3xc" $ [fmt|a%-3xc|]  (175 :: Integer) @?= ("aaf c" :: Text)
    , testCase "a%03xc" $ [fmt|a%03xc|]  (175 :: Int)     @?= ("a0afc" :: Text)

    , testCase "a%3oc"  $ [fmt|a%3oc|]  (175 :: Int)     @?= ("a257c"  :: Text)
    , testCase "a%-4oc" $ [fmt|a%-4oc|] (175 :: Integer) @?= ("a257 c" :: Text)
    , testCase "a%04oc" $ [fmt|a%04oc|] (175 :: Int)     @?= ("a0257c" :: Text)

    , testCase "%04b"     $ [fmt|%04b|]      (6 :: Int)  @?= ("0110" :: Text)
    , testCase "%-4b"     $ [fmt|%-4b|]      (6 :: Int)  @?= ("110 " :: Text)
    , testCase "%2b"      $ [fmt|%2b|]       (6 :: Int)  @?= ( "110" :: Text)

    , testCase "%f"     $ [fmtT|%f|]      (6 :: Int)     @?=  "6"
    , testCase "%f"     $ [fmtT|%f|]    (6.5 :: Float)   @?=  "6.5"
    , testCase "%f"     $ [fmtT|%f|]    (6.2 :: Double)  @?=  "6.2"

    , testCase "%L"       $ [fmtT|(%L)|]     ts  @?=  "(ttt: c,ttt: b,ttt: a)"
    , testCase "%22L"     $ [fmtT|(%22L)|]   ts  @?=  "(  ttt: c,ttt: b,ttt: a)"

    , testCase "%3f"     $ [fmtT|%3f|]    (6 :: Int)       @?=  "  6"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.2 :: Float)   @?=  "6.20"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.005 :: Float) @?=  "6.01"
    , testCase "%3.2f"   $ [fmtT|%3.2f|]  (6.002 :: Float) @?=  "6.00"
    , testCase "%05.2f"  $ [fmtT|%05.2f|] (6.2 :: Double)  @?=  "06.20"
    , testCase "%-5.2f"  $ [fmtT|%-5.2f|] (6.2 :: Double)  @?=  "6.20 "
    , testCase "%.3f"    $ [fmtT|%.3f|]   (6.2 :: Double)  @?=  "6.200"

    , testCase "%3n"     $ [fmtT|%3n|]    (6 :: Int)       @?=  "  6"
    , testCase "%3n"     $ [fmtT|%3n|]    (6 :: Word8)     @?=  "  6"

    , testCase "%e"     $ [fmtT|%e|]       (1000000 :: Double)  @?=      "10e5"
    , testCase "%.2e"   $ [fmtT|%.2e|]     (1000000 :: Double)  @?=   "10.00e5"
    , testCase "%5.2e"  $ [fmtT|%5.2e|]    (9999999 :: Double)  @?=   "10.00e6"
    , testCase "%7.2e"  $ [fmtT|%9.2e|]    (1000000 :: Double)  @?= "  10.00e5"

    , testCase "fmtS"         $ [fmtS|a%03tc|] "b" @?= ("a00bc" :: String)
    , testCase "as string"    $ [fmtS|a%03tc|] "b" @?=  "a00bc"
    , testCase "fmtL"       $ [fmtL|a%03tc|] "b" @?= ("a00bc" ::LazyText.Text)
    , testCase "as lazy text" $ [fmtL|a%03tc|] "b" @?= "a00bc"
    , testCase "fmtT"        $ [fmtT|a%03tc|] "b" @?= ("a00bc" :: Text)
    , testCase "as strict text" $ [fmtT|a%03tc|] "b" @?= "a00bc"

    , testCase "%d"      $ [fmtT|%%%d|] (  7  :: Int) @?= ("%7"   :: Text)
    , testCase "%d (-)"  $ [fmtT|%d|]   ((-7) :: Int) @?= ("-7"  :: Text)
    , testCase "%dC"     $ [fmtT|%dC|]  (  7  :: Int) @?= ("7C"  :: Text)
    , testCase "F%d"     $ [fmtT|F%d|]  (  7  :: Int) @?= ("F7"  :: Text)
    , testCase "-%d"     $ [fmtT|-%d|]  (  7  :: Int) @?= ("-7"  :: Text)
    , testCase "-%d"     $ [fmtT|-%d|]  ((-7) :: Int) @?= ("--7" :: Text)

    , testCase "0 b" $ [fmt|%Y|] (0 :: Int) @?= ("0" :: Text)
    , testCase "0 B" $ [fmt|%y|] (0 :: Int) @?= ("0" :: Text)
    , testCase "500 b" $ [fmt|%y|] (500 :: Int) @?= ("500B" :: Text)
    , testCase "500 B" $ [fmt|%Y|] (500 :: Int) @?= ("500B" :: Text)
    , testCase "1000 b" $ [fmt|%y|] (1000 :: Int) @?= ("1.00kB" :: Text)
    , testCase "1000 B" $ [fmt|%Y|] (1000 :: Int) @?= ("1000B" :: Text)
    , testCase "1024 b" $ [fmt|%y|] (1024 :: Int) @?= ("1.02kB" :: Text)
    , testCase "1024 B" $ [fmt|%Y|] (1024 :: Int) @?= ("1.00KiB" :: Text)
    , testCase "5000 b" $ [fmt|%y|] (5000 :: Int) @?= ("5.00kB" :: Text)
    , testCase "5000 B" $ [fmt|%Y|] (5000 :: Int) @?= ("4.88KiB" :: Text)
    , testCase "1000000 b" $ [fmt|%y|] (1000000 :: Int) @?= ("1.00MB" :: Text)
    , testCase "1000000 B" $ [fmt|%Y|] (1000000 :: Int) @?= ("977KiB" :: Text)
    , testCase "1048576 b" $ [fmt|%y|] (1048576 :: Int) @?= ("1.05MB" :: Text)
    , testCase "1048576 B" $ [fmt|%Y|] (1048576 :: Int) @?= ("1.00MiB" :: Text)
    , testCase "1073741824 b" $ [fmt|%7y|] (1024^^3) @?= (" 1.07GB" :: Text)
    , testCase "1073741824 B" $ [fmt|%Y|] (1024^^3) @?= ("1.00GiB" :: Text)
    , testCase "1073741824 B" $ [fmt|%Y|] (1024^^3) @?= ("1.00GiB" :: Text)

    , testCase "1973-01-01-Z17:00:00" $
          [fmtT|%z|] dayOne @?= "1973-01-01Z17:00:00"
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%Z|] dayOne @?= "1973-01-01Z17:00:00 Mon"

    , testCase "1973-01-01-Z17:00:00" $
          [fmtT|%k|] cs @?= "«c#8»"
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%K|] cs @?= intercalate "\n" [ "foo (a:b:c 8:13-21:34)"
                                             , "bar (x:y:z 55:89-144:233)" ]
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%26K|] cs @?= intercalate "\n" [ "    foo (a:b:c 8:13-21:34)"
                                               , " bar (x:y:z 55:89-144:233)" ]
    , testCase "1973-01-01-Z17:00:00 Mon" $
          [fmtT|%-26K|] cs @?= intercalate "\n" [ "foo (a:b:c 8:13-21:34)    "
                                                , "bar (x:y:z 55:89-144:233) " ]
    ]

-- that's all, folks! ---------------------------------------------------------
