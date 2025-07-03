{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.Map as Map -- containers
import System.IO ( IOMode(..), hClose, hPutStrLn, openFile, stderr, stdout ) -- base

import qualified Options.Applicative as Opt -- optparse-applicative

import Text.IPG.Interpreter ( NT, Value(..), asJSON, interpret )
import Text.IPG.Export.JS ( Context(..), defaultContext, toJSWithContext )
import Text.IPG.PPrint ( pprint )
import Text.IPG.Simple ( parse )

data ExportType = JS | CORE deriving ( Eq, Ord, Show, Read )

data Options = Options {
    inFile :: Maybe String,
    outFile :: Maybe String,
    exportType :: !ExportType,
    noValidation :: !Bool,
    debugModeFlag :: !Bool,
    interpretFlag :: !Bool
  }

options :: Opt.ParserInfo Options
options = Opt.info (Options
    <$> Opt.optional (Opt.strOption (
            Opt.long "in-file"
         <> Opt.short 'i'
         <> Opt.metavar "FILE"
         <> Opt.help "Source grammar file. stdin if omitted."))
    <*> Opt.optional (Opt.strOption (
            Opt.long "out-file"
         <> Opt.short 'o'
         <> Opt.metavar "FILE"
         <> Opt.help "Output file. stdout if omitted."))
    <*> Opt.option Opt.auto (
            Opt.long "export-type"
         <> Opt.short 't'
         <> Opt.help "Export type. JS or CORE. Default JS."
         <> Opt.value JS)
    <*> Opt.switch (
            Opt.long "no-validation"
         <> Opt.help "Disable validating the IPG input.")
    <*> Opt.switch (
            Opt.long "debug-mode"
         <> Opt.help "Enable debug mode in output.")
    <*> Opt.switch (
            Opt.long "interpret"
         <> Opt.short 'I'
         <> Opt.help "Interpret grammar instead. --in-file is required for the .ipg and stdin will be the parser's input.")
    Opt.<**> Opt.helper) (
        Opt.fullDesc
     <> Opt.progDesc "Interval Parsing Grammar parser generator"
     <> Opt.header "IPGcc")

main :: IO ()
main = do
    opts <- Opt.execParser options
    ipgInput <- case inFile opts of Nothing -> LBS.getContents; Just f -> LBS.readFile f
    h <- case outFile opts of Nothing -> return stdout; Just f -> openFile f WriteMode

    case parse (not (noValidation opts)) ipgInput of
        Left errs -> mapM_ (hPutStrLn stderr) errs
        Right (preamble, core, _, postamble) -> do
            case exportType opts of
                CORE -> LBS.hPutStrLn h (Builder.toLazyByteString (pprint core))
                JS -> do
                    if interpretFlag opts then do
                        buf <- CBS.getContents
                        case interpret core externalFuncs [] buf of
                            Nothing -> hPutStrLn h "null"
                            Just (bs, _, _) ->
                                LBS.hPutStrLn h
                                    (Builder.toLazyByteString (asJSON (BINDINGS bs)))
                      else do
                        LBS.hPutStrLn h preamble
                        LBS.hPutStrLn h (toJSWithContext
                            (defaultContext { debugMode = debugModeFlag opts }) core)
                        LBS.hPutStr h postamble
    hClose h

externalFuncs :: Map.Map NT ([Value a] -> Value a)
externalFuncs = Map.fromList [
    ("empty", \[] -> BINDINGS Map.empty),
    ("decodeAscii", \[SEQUENCE cs] ->
        STRING (BS.pack $ map (\(INT c) -> fromIntegral c) cs)),
    ("makeEntry", \[name, descr, typ] ->
        BINDINGS (Map.fromList [("name", name), ("descriptor", descr), ("type", typ)])),
    ("projectSections", \[SEQUENCE sections] ->
        SEQUENCE (map (\(BINDINGS b) -> b Map.! "section") sections))
  ]
