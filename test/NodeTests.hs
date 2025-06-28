module Main ( main ) where
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import Data.Maybe ( fromMaybe ) -- base
import System.Directory ( createDirectoryIfMissing, getTemporaryDirectory, removeFile ) -- directory
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath
import System.Environment ( lookupEnv ) -- base
import System.IO ( hClose, openBinaryTempFile ) -- base
import System.Process ( readProcess ) -- process

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import Text.IPG.Export.JS ( Context(..), defaultContext, toJSWithContext )

nODE_EXE :: String
nODE_EXE = "node"

type Env = (FilePath, String)

main :: IO ()
main = do
    tmpDir <- getTemporaryDirectory
    nodeExe <- fromMaybe nODE_EXE <$> lookupEnv "NODE_EXE"
    createDirectoryIfMissing True tmpDir
    tests <- goldenTests (tmpDir, nodeExe)
    defaultMain tests

run :: Env -> Bool -> FilePath -> IO LBS.ByteString
run (tmpDir, nodeExe) doDebugging f = do
    parseResult <- parseFile True f
    case parseResult of
        Left err -> return $ LBS.pack (show err)
        Right (preamble, g, _, postamble) -> do
            (tmpFile, h) <- openBinaryTempFile tmpDir "test.js"
            LBS.hPutStrLn h preamble
            LBS.hPutStrLn h (toJSWithContext (defaultContext { debugMode = doDebugging }) g)
            LBS.hPutStrLn h postamble
            hClose h

            -- TODO: There's surely a ByteString version of this.
            result <- LBS.pack <$> readProcess nodeExe [tmpFile] ""

            removeFile tmpFile

            return result

-- TODO: Have tests that compare interpreter and node output.
goldenTests :: Env -> IO TestTree
goldenTests env = do
    ipgFiles <- findByExtension [".ipg"] "test/node/"
    return $ testGroup "Node tests"
        [ testGroup "Non-Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (run env False ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".release.json"
              ]
        , testGroup "Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (run env True ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".debug.json"
              ]
        ]
