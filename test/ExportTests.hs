module Main ( main ) where
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import System.FilePath ( replaceExtension, takeBaseName ) -- filepath

import Test.Tasty ( TestTree, defaultMain, testGroup ) -- tasty
import Test.Tasty.Golden ( findByExtension, goldenVsString ) -- tasty-golden

import Text.IPG.Simple ( parseFile )
import qualified Text.IPG.Export.JS as JS
import qualified Text.IPG.Export.Rust as RS

main :: IO ()
main = defaultMain =<< goldenTests

export :: Bool -> Bool -> FilePath -> IO LBS.ByteString
export doDebugging useAsync f = do
    results <- parseFile True f
    let ctxt = JS.defaultContext {
        JS.debugMode = doDebugging,
        JS.asyncMode = useAsync }
    case results of
        Left err -> return $ LBS.pack (show err)
        Right (preamble, g, _, _, postamble) -> return $
            preamble <> JS.toJSWithContext ctxt g <> postamble

exportRs :: FilePath -> IO LBS.ByteString
exportRs f = do
    results <- parseFile True f
    let ctxt = RS.defaultContext
    case results of
        Left err -> return $ LBS.pack (show err)
        Right (preamble, g, _, _, postamble) ->
            case RS.toRustWithContext ctxt g of
                Left err -> return $ Builder.toLazyByteString err
                Right rs -> return $ preamble <> rs <> postamble

-- TODO: Not sure how much I want these as the export could easily change in
-- minor ways that don't affect the behavior.
goldenTests :: IO TestTree
goldenTests = do
    ipgFiles <- findByExtension [".ipg"] "test/export/"
    rsIpgFiles <- findByExtension [".ipg"] "test/export-rs/"
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
        , testGroup "Rust"
              [ goldenVsString (takeBaseName ipgFile) goldFile (exportRs ipgFile)
              | ipgFile <- rsIpgFiles
              , let goldFile = replaceExtension ipgFile ".rs"
              ]
        ]
