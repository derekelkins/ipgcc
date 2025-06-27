{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers
import System.IO ( IOMode(..), hPutStrLn, openFile, stderr, stdin, stdout ) -- base

import qualified Options.Applicative as Opt -- optparse-applicative

import Text.IPG.CheckIPG ( validate )
import Text.IPG.FullIPG ( ExpHelpers(..), toCore )
import qualified Text.IPG.GenericExp as E
import Text.IPG.Interpreter ( Bindings, NT, Value(..), asJSON, interpret )
import Text.IPG.IPGParser ( IdType, Exp', parseWithStartPos )
import Text.IPG.Export.JSExport ( Context(..), defaultContext, toJS, toJSWithContext )
import Text.IPG.PPrint ( hexyString, pprint )
import Text.IPG.TopLevel.FileSplit ( splitFile )

data ExportType = JS | PPRINT deriving ( Eq, Ord, Show, Read )

data Options = Options {
    inFile :: Maybe String,
    outFile :: Maybe String,
    exportType :: !ExportType,
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
         <> Opt.help "Export type. JS or PPRINT. Default JS."
         <> Opt.value JS)
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
    ipgInput' <- case inFile opts of Nothing -> LBS.getContents; Just f -> LBS.readFile f
    h <- case outFile opts of Nothing -> return stdout; Just f -> openFile f WriteMode

    let (preamble, ipgInput, postamble) = splitFile ipgInput'

    let byteOffset = fromIntegral (LBS.length preamble) -- This isn't 100% correct but it doesn't really matter
    let startLine = computeStartLine preamble
    let startCol = 1

    case parseWithStartPos byteOffset startLine startCol ipgInput of
        Left err -> hPutStrLn stderr err
        Right (g, decls) -> do
            let core = E.simplify (toCore helper g)
            case exportType opts of
                PPRINT -> LBS.hPutStrLn h (Builder.toLazyByteString (pprint core))
                JS -> do
                    case validate (Set.fromList decls) core of
                        Just errs -> mapM_ (CBS.hPutStrLn stderr) errs
                        Nothing -> do
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

helper :: ExpHelpers IdType IdType IdType Exp'
helper = ExpHelpers {
    len = E.Int . fromIntegral . BS.length,
    add = E.Add,
    num = E.Int . fromIntegral,
    ref = E.Ref
  }  

computeStartLine :: LBS.ByteString -> Int
computeStartLine "" = 1
computeStartLine s = 3 + fromIntegral (LBS.count '\n' s)

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
