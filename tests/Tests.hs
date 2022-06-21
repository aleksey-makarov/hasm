{-# LANGUAGE BinaryLiterals #-}

module Main (main) where

import Prelude as P

import Control.Monad.State as MS
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Elf
import Data.Int
import Data.Word
-- import Numeric
import System.FilePath
import System.Posix.Files
import System.Process
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck
import Test.QuickCheck.Monadic as QC.Monadic

import Asm.Asm
import Asm.AArch64
import Asm.Data
import Asm.DummyLd

import Code.AArch64.HelloWorld
import Code.AArch64.ForwardLabel
import Code.AArch64.TestBss
import Code.AArch64.DontRun

testsOutDir :: FilePath
testsOutDir = "tests" </> "out"

makeFileExecutable :: String -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

-- | Read the file strictly but return lazy bytestring
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = BSL.fromStrict <$> BS.readFile path

runExe :: String -> String -> IO String
runExe name stdin = readProcess "qemu-aarch64" [f] stdin
    where
        f = testsOutDir </> name

mkObj :: String -> StateT (CodeState AArch64) IO () -> IO ()
mkObj name code = assemble code >>= serializeElf >>= BSL.writeFile n
    where
        n = testsOutDir </> name <.> "o"

ldDummy :: String -> IO ()
ldDummy name = do
    readFileStrict i >>= parseElf >>= dummyLd >>= serializeElf >>= BSL.writeFile o
    makeFileExecutable o
    where
        i = testsOutDir </> name <.> "o"
        o = testsOutDir </> name <.> "dummy"

ldGcc :: String -> IO ()
ldGcc name = callProcess "aarch64-unknown-linux-musl-gcc" [i, "-nostdlib", "-o", o]
    where
        i = testsOutDir </> name <.> "o"
        o = testsOutDir </> name <.> "gcc"

testExe :: String -> StateT (CodeState AArch64) IO () -> Maybe String -> [ TestTree ]
testExe name code maybeExpectedString =
    [ testCase mkObjTestName $ mkObj name code
    , after AllSucceed mkObjTestName $ testGroup checkObjTestName $
        [ goldenVsFile dumpObjTestName dumpGoldenName dumpOutName mkDump
        ] ++
        case maybeExpectedString of
            Just expectedString ->
                [ testCase mkGccLdTestName $ ldGcc name
                , after AllSucceed mkGccLdTestName $ localOption (mkTimeout 500000) $ testCase runGccLdTestName $ do
                    out <- runExe (name <.> "gcc") ""
                    out @?= expectedString
                , testCase mkDummyLdTestName $ ldDummy name
                , after AllSucceed mkDummyLdTestName $ localOption (mkTimeout 500000) $ testCase runDummyLdTestName $ do
                    out <- runExe (name <.> "dummy") ""
                    out @?= expectedString
                ]
            Nothing -> []
    ]
    where
        mkObjTestName      = name ++ "_mkobj"
        checkObjTestName   = name ++ "_checkobj"
        dumpObjTestName    = name ++ "_dump"
        mkGccLdTestName    = name ++ "_mkgcc"
        runGccLdTestName   = name ++ "_rungcc"
        mkDummyLdTestName  = name ++ "_mkdummy"
        runDummyLdTestName = name ++ "_rundummy"

        objName            = testsOutDir </> name <.> "o"
        dumpOutName        = testsOutDir </> name <.> "o" <.> "dump"
        dumpGoldenName     = testsOutDir </> name <.> "o" <.> "dump" <.> "golden"

        mkDump             = callCommand ("hobjdump -f " ++ objName ++ " > " ++ dumpOutName)

--------------------------------------------------
--

prop_sum :: (Word32, Word32) -> Property
prop_sum (aw, bw) = monadicIO $ do
    retString <- run $ runExe "testBss.gcc" (printWord32 aw ++ " " ++ printWord32 bw)
    QC.Monadic.assert (aw + bw == readWord32 retString)

testPropSum :: TestTree
testPropSum = testProperty "bssExample_testProperty" prop_sum

testExeBss :: [ TestTree ]
testExeBss = [ testCase mkObjTestName $ mkObj name testBss
             , after AllSucceed mkObjTestName $ testGroup checkObjTestName
                [ goldenVsFile dumpObjTestName dumpGoldenName dumpOutName mkDump
                , testCase mkGccLdTestName $ ldGcc name
                , after AllSucceed mkGccLdTestName testPropSum
                ]
             ]
    where
        name            = "testBss"
        mkObjTestName   = name ++ "_mkobj"
        checkObjTestName = name ++ "_checkobj"
        dumpObjTestName = name ++ "_dump"
        mkGccLdTestName = name ++ "_mkgcc"
        objName         = testsOutDir </> name <.> "o"
        dumpOutName     = testsOutDir </> name <.> "o" <.> "dump"
        dumpGoldenName  = testsOutDir </> name <.> "o" <.> "dump" <.> "golden"
        mkDump          = callCommand ("hobjdump -f " ++ objName ++ " > " ++ dumpOutName)

fitNWidth :: Int
fitNWidth = 4

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

testFitNTestCase :: Int64 -> Maybe Word32 -> TestTree
testFitNTestCase w mn = testCase (show w) $ (rightToMaybe $ fitN fitNWidth w) @?= mn

testFitN :: TestTree
testFitN = testGroup "testFitN"
    [ testFitNTestCase   8  $ Nothing
    , testFitNTestCase   7  $ Just 0b00111
    , testFitNTestCase   1  $ Just 0b00001
    , testFitNTestCase   0  $ Just 0b00000
    , testFitNTestCase (-1) $ Just 0b01111
    , testFitNTestCase (-2) $ Just 0b01110
    , testFitNTestCase (-7) $ Just 0b01001
    , testFitNTestCase (-8) $ Just 0b01000
    , testFitNTestCase (-9) $ Nothing
    ]

--
--------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "tests" $ testFitN :
    (  testExe "helloWorld"   helloWorld   (Just "Hello World!\n")
    ++ testExe "forwardLabel" forwardLabel (Just "ok\n")
    ++ testExe "dontRun"      dontRun      Nothing
    ++ testExeBss
    )
