{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as CBS -- bytestring
import Data.ByteString.Builder ( toLazyByteString ) -- bytestring
import Data.Char ( isSpace ) -- base
import qualified Data.Map as Map -- containers
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Interpreter ( Value(BINDINGS, INT), asJSON', interpret )
import Text.IPG.Simple ( parseFile )

main :: IO ()
main = defaultMain =<< goldenTests

interpretFile :: FilePath -> IO LBS.ByteString
interpretFile f = do
    Right (_, g, _, input) <- parseFile True f
    let input' = CBS.dropWhileEnd isSpace input
    return (toLazyByteString
            (asJSON' (postProcess <$> interpret g Map.empty [] (LBS.toStrict input'))))
  where postProcess (bs, start, end) = BINDINGS
            (Map.insert "_ipg_start" (INT (fromIntegral start))
                (Map.insert "_ipg_end" (INT (fromIntegral end)) bs))

goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/interpret/"
    return $ testGroup "Interpreter tests"
      [ goldenVsString (takeBaseName ipgFile) goldFile (interpretFile ipgFile)
      | ipgFile <- ipgFiles
      , let goldFile = replaceExtension ipgFile ".ipg.json"
      ]

