{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as CBS -- bytestring
import Data.ByteString.Builder ( toLazyByteString ) -- bytestring
import Data.Char ( isSpace ) -- base
import qualified Data.Map as Map -- containers
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Interpreter ( Value(..), asJSON', interpret )
import Text.IPG.Simple ( parseFile )

main :: IO ()
main = defaultMain =<< goldenTests

efs :: Map.Map BS.ByteString ([Value a] -> Value a)
efs = Map.fromList [
    ("nil", \[] -> SEQUENCE []),
    ("cons", \[x, SEQUENCE xs] -> SEQUENCE (x:xs)),
    ("null", \[SEQUENCE xs] -> BOOL (null xs)),
    ("head", \[SEQUENCE (x:_)] -> x),
    ("tail", \[SEQUENCE (_:xs)] -> SEQUENCE xs),
    ("empty", \[] -> BINDINGS Map.empty),
    ("insert", \[STRING k, v, BINDINGS bs] -> BINDINGS (Map.insert k v bs)),
    ("contains", \[STRING k, BINDINGS bs] -> BOOL (Map.member k bs)),
    ("lookup", \[STRING k, BINDINGS bs] -> bs Map.! k)
  ]

interpretFile :: FilePath -> IO LBS.ByteString
interpretFile f = do
    Right (_, g, _, input) <- parseFile True f
    let input' = CBS.dropWhileEnd isSpace input
    return (toLazyByteString
            (asJSON' (postProcess <$> interpret g efs [] (LBS.toStrict input'))))
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

