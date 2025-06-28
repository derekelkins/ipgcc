module Main ( main ) where
import Control.Exception ( catch, throwIO ) -- base
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import System.Directory ( createDirectoryIfMissing, removeDirectoryRecursive ) -- directory
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath
import System.Exit ( ExitCode ) -- base
import System.IO ( hClose, openBinaryTempFile ) -- base
import System.Process ( readProcess ) -- process

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import Text.IPG.Export.JS ( Context(..), defaultContext, toJSWithContext )

-- TODO: Make these configurable
tMP_DIR :: FilePath
tMP_DIR = "./tmp/"

nODE_EXE :: String
nODE_EXE = "node"

main :: IO ()
main = do
    createDirectoryIfMissing True tMP_DIR
    tests <- goldenTests
    defaultMain tests
     `catch` (\e -> do
        removeDirectoryRecursive tMP_DIR
        throwIO (e :: ExitCode))

run :: Bool -> FilePath -> IO LBS.ByteString
run doDebugging f = do
    results <- parseFile False f
    case results of
        Left err -> return $ LBS.pack (show err)
        Right (preamble, g, _, postamble) -> do
            (tmpFile, h) <- openBinaryTempFile tMP_DIR "test.js"
            LBS.hPutStrLn h preamble
            LBS.hPutStrLn h (toJSWithContext (defaultContext { debugMode = doDebugging }) g)
            LBS.hPutStrLn h postamble
            hClose h

            -- TODO: There's surely a ByteString version of this.
            LBS.pack <$> readProcess nODE_EXE [tmpFile] ""

goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/node/"
    return $ testGroup "Node tests"
        [ testGroup "Non-Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (run False ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".release.json"
              ]
        , testGroup "Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (run True ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".debug.json"
              ]
        ]
