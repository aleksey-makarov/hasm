module Main (main) where

import Control.Monad.State as MS
import Data.Bits
import Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Elf
import System.FilePath
import System.Posix.Files
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import Asm.AsmAArch64
import Asm.DummyLd

import Code.HelloWorld
import Code.ForwardLabel

testsOutDir :: FilePath
testsOutDir = "tests" </> "out"

makeFileExecutable :: String -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

-- | Read the file strictly but return lazy bytestring
readFileStrict :: FilePath -> IO BSL.ByteString
readFileStrict path = BSL.fromStrict <$> BS.readFile path

runExe :: String -> IO String
runExe name = readProcess "qemu-aarch64" [f] []
    where
        f = testsOutDir </> name

mkObj :: String -> StateT CodeState IO () -> IO ()
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
ldGcc name = callProcess "aarch64-unknown-linux-gnu-gcc" [i, "-nostdlib", "-o", o]
    where
        i = testsOutDir </> name <.> "o"
        o = testsOutDir </> name <.> "gcc"

testExe :: String -> StateT CodeState IO () -> String -> [ TestTree ]
testExe name code expectedString =
    [ testCase mkObjTestName $ mkObj name code
    , after AllSucceed mkObjTestName $ testGroup checkObjTestName
        [ testCase mkGccLdTestName $ ldGcc name
        , after AllSucceed mkGccLdTestName $ testCase runGccLdTestName $ do
            out <- runExe $ name <.> "gcc"
            out @?= expectedString
        , testCase mkDummyLdTestName $ ldDummy name
        , after AllSucceed mkDummyLdTestName $ testCase runDummyLdTestName $ do
            out <- runExe $ name <.> "dummy"
            out @?= expectedString
        ]
    ]
    where
        mkObjTestName      = name ++ "_mkobj"
        checkObjTestName   = name ++ "_checkobj"
        mkGccLdTestName    = name ++ "_mkgcc"
        runGccLdTestName   = name ++ "_rungcc"
        mkDummyLdTestName  = name ++ "_mkdummy"
        runDummyLdTestName = name ++ "_rundummy"

main :: IO ()
main = defaultMain $ testGroup "tests"
    (  testExe "helloWorld"   helloWorld   "Hello World!\n"
    ++ testExe "forwardLabel" forwardLabel "ok\n"
    )
