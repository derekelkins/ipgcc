module Main ( main ) where
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import Text.IPG.Export.JS ( Context(..), defaultContext, toJSWithContext )

main :: IO ()
main = defaultMain =<< goldenTests

export :: Bool -> Bool -> FilePath -> IO LBS.ByteString
export doDebugging useAsync f = do
    results <- parseFile True f
    let ctxt = defaultContext {
        debugMode = doDebugging,
        asyncMode = useAsync }
    case results of
        Left err -> return $ LBS.pack (show err)
        Right (preamble, g, _, postamble) -> return $
            preamble  <> toJSWithContext ctxt g <> postamble

-- TODO: Not sure how much I want these as the export could easily change in
-- minor ways that don't affect the behavior.
goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/export/"
    return $ testGroup "Export tests"
        [ testGroup "Non-Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (export False False ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".release.js"
              ]
        , testGroup "Debugging"
              [ goldenVsString (takeBaseName ipgFile) goldFile (export True False ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".debug.js"
              ]
        , testGroup "Async"
              [ goldenVsString (takeBaseName ipgFile) goldFile (export False True ipgFile)
              | ipgFile <- ipgFiles
              , let goldFile = replaceExtension ipgFile ".async.js"
              ]
        ]
