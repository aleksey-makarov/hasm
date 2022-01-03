module Main (main) where

import Control.Monad.State as MS
import Data.ByteString.Lazy as BSL
import Data.Elf
import System.FilePath
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import Paths_hasm

import Asm.AsmAArch64

import Code.HelloWorld
import Code.ForwardLabel

testsOutDir :: FilePath
testsOutDir = "tests" </> "out"

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
    bindir <- getBinDir
    callProcess (bindir </> "hld") [i, o]
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
