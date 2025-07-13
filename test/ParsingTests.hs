{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import Data.ByteString.Builder ( byteString, toLazyByteString ) -- bytestring
import Data.List ( intersperse ) -- base
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import Text.IPG.PPrint ( pprint )
import qualified Text.IPG.TypeCheck as TC

main :: IO ()
main = defaultMain =<< goldenTests

parse :: Bool -> FilePath -> IO LBS.ByteString
parse doValidation f = do
    results <- parseFile doValidation f
    case results of
        Left err -> return $ LBS.concat (intersperse "\n" (map LBS.pack err))
        Right (_, g, _, _, _) -> return $ toLazyByteString (pprint g)

tc :: FilePath -> IO LBS.ByteString
tc f = do
    results <- parseFile True f
    case results of
        Left err -> return $ LBS.concat (intersperse "\n" (map LBS.pack err))
        Right (_, g, _, _, _) -> do
            let ctxt = TC.Context {
                           TC.currentRule = "",
                           TC.values = "values",
                           TC.out = byteString,
                           TC.tOut = byteString,
                           TC.ntOut = byteString
                       }
            case TC.typeCheck ctxt g of
                Right envs -> return (toLazyByteString (pprint (TC.annotate envs g)))
                Left errs -> return $ LBS.concat (intersperse "\n" (map toLazyByteString errs))

goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/parsing/"
    ipgFiles' <- findByExtension [".ipg"] "test/validation/"
    tcFiles <- findByExtension [".ipg"] "test/typecheck/"
    return $ testGroup "Parser tests"
      [ testGroup "Non-Validating" 
        [
        goldenVsString (takeBaseName ipgFile) goldFile (parse False ipgFile)
        | ipgFile <- ipgFiles
        , let goldFile = replaceExtension ipgFile ".ipg.golden"
        ]
      , testGroup "Validating" 
        [
        goldenVsString (takeBaseName ipgFile) goldFile (parse True ipgFile)
        | ipgFile <- ipgFiles'
        , let goldFile = replaceExtension ipgFile ".ipg.golden"
        ]
      , testGroup "Type Checking" 
        [
        goldenVsString (takeBaseName ipgFile) goldFile (tc ipgFile)
        | ipgFile <- tcFiles
        , let goldFile = replaceExtension ipgFile ".ipg.golden"
        ]
      ]
