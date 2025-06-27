module Main ( main ) where
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import Data.ByteString.Builder ( toLazyByteString ) -- bytestring
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import Text.IPG.PPrint ( pprint )

main :: IO ()
main = defaultMain =<< goldenTests

parse :: FilePath -> IO LBS.ByteString
parse f = do
    results <- parseFile False f
    case results of
        Left err -> return $ LBS.pack (show err)
        Right (_, g, _, _) -> return $ toLazyByteString (pprint g)

goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/parsing/"
    return $ testGroup "Parser tests"
      [ goldenVsString (takeBaseName ipgFile) goldFile (parse ipgFile)
      | ipgFile <- ipgFiles
      , let goldFile = replaceExtension ipgFile ".ipg.golden"
      ]
