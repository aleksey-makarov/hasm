module Main (main) where

import Control.Monad.State as MS
import Data.Bits
import Data.ByteString.Lazy as BSL
import System.FilePath
import System.Posix.Files
import System.Process
import Test.Tasty
import Test.Tasty.HUnit

import Data.Elf

import Asm.AsmAArch64
import Asm.DummyLd

import Code.HelloWorld
import Code.ForwardLabel

makeFileExecutable :: String -> IO ()
makeFileExecutable path = do
    m <- fileMode <$> getFileStatus path
    setFileMode path $ m .|. ownerExecuteMode

fixTargetName :: String -> String
fixTargetName = fmap f
    where
        f '.' = '_'
        f x   = x

writeElf :: FilePath -> Elf -> IO ()
writeElf path elf = do
    e <- serializeElf elf
    BSL.writeFile path e
    makeFileExecutable path

testsOutDir :: FilePath
testsOutDir = "tests" </> "out"

runExe :: String -> IO String
runExe elfFileName = readProcess "qemu-aarch64" [f] []
    where
        f = testsOutDir </> elfFileName

testExe :: String -> StateT CodeState IO () -> String -> [ TestTree ]
testExe elfFileName code expectedString =
    [ testCase makeTargetName (assemble code >>= dummyLd >>= writeElf f)
    , after AllSucceed makeTargetName $ testCase checkTargetName $ do
        out <- runExe elfFileName
        out @?= expectedString
    ]
    where
        makeTargetName  = "make_"  ++ fixTargetName elfFileName
        checkTargetName = "check_" ++ fixTargetName elfFileName
        f = testsOutDir </> elfFileName

main :: IO ()
main = defaultMain $ testGroup "examples"
    (  testExe "helloWorld"   helloWorld   "Hello World!\n"
    ++ testExe "forwardLabel" forwardLabel "ok\n"
    )
