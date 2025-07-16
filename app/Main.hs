{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Main ( main ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import qualified Data.Map as Map -- containers
import System.IO ( IOMode(..), hClose, hPutStrLn, openFile, stderr, stdout ) -- base

import qualified Options.Applicative as Opt -- optparse-applicative

import Text.IPG.Interpreter ( NT, Value(..), asJSON, interpret )
import qualified Text.IPG.Export.JS as JS
import qualified Text.IPG.Export.Rust as RS
import Text.IPG.PPrint ( pprint )
import Text.IPG.Simple ( parse )
import qualified Text.IPG.TypeCheck as TC

data Options = Options {
    inFile :: Maybe String,
    outFile :: Maybe String,
    noValidation :: !Bool,
    commandOpts :: CommandOptions
  }

data CommandOptions
    = JS JsOptions
    | RUST RustOptions
    | INTERPRET InterpretOptions
    | CORE CoreOptions

data JsOptions = JsOptions {
    jsDebugModeFlag :: !Bool,
    leaveExtraFieldsFlag :: !Bool,
    jsTypeCheckFlag :: !Bool,
    asyncModeFlag :: !Bool
  }

data RustOptions = RustOptions {
    rsDebugModeFlag :: !Bool,
    mutableFieldsFlag :: !Bool,
    dumpCoreFlag :: !Bool
  }

data InterpretOptions = InterpretOptions {
    interpreterTypeCheckFlag :: !Bool
  }

data CoreOptions = CoreOptions {
    coreTypeCheckFlag :: !Bool
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
    <*> Opt.switch (
            Opt.long "no-validation"
         <> Opt.help "Disable validating the IPG input.")
    <*> Opt.hsubparser (
            Opt.command "pprint" (Opt.info (fmap CORE $ CoreOptions
            <$> Opt.switch (
                    Opt.long "type-check"
                 <> Opt.short 'T'
                 <> Opt.help "Print out the grammar with type information."))
            (Opt.progDesc "Pretty print the pre-processed grammar."))
         <> Opt.command "interpret" (Opt.info (fmap INTERPRET $ InterpretOptions
            <$> Opt.switch (
                    Opt.long "type-check"
                 <> Opt.short 'T'
                 <> Opt.help "Type check before performing interpretation."))
            (Opt.progDesc "Interpret grammar instead. --in-file is required for the .ipg and stdin will be the parser's input."))
         <> Opt.command "js" (Opt.info (fmap JS $ JsOptions
            <$> Opt.switch (
                    Opt.long "debug-mode"
                 <> Opt.help "Enable debug mode in output.")
            <*> Opt.switch (
                    Opt.long "leave-extra-fields"
                 <> Opt.help "Don't strip internal fields in JS export.")
            <*> Opt.switch (
                    Opt.long "type-check"
                 <> Opt.short 'T'
                 <> Opt.help "Type check before exporting JavaScript. This doesn't change the result.")
            <*> Opt.switch (
                    Opt.long "async-mode"
                 <> Opt.help "Export using asynchronous interface."))
            (Opt.progDesc "JavaScript export"))
         <> Opt.command "rust" (Opt.info (fmap RUST $ RustOptions
            <$> Opt.switch (
                    Opt.long "debug-mode"
                 <> Opt.help "Enable debug mode in output.")
            <*> Opt.switch (
                    Opt.long "mutable-fields"
                 <> Opt.help "Declare variables holding fields with `let mut`.")
            <*> Opt.switch (
                    Opt.long "dump-core"
                 <> Opt.help "Dump the core upon failures."))
            (Opt.progDesc "Rust export")))
    Opt.<**> Opt.helper) (
        Opt.fullDesc
     <> Opt.progDesc "Interval Parsing Grammar parser generator"
     <> Opt.header "IPGcc")

typeCheckCore False core k = k core
typeCheckCore True core' k =
    case fmap (\envs -> TC.annotate envs core') (TC.typeCheck ctxt core') of
        Left err -> LBS.hPutStrLn stderr (Builder.toLazyByteString err)
        Right core -> k core
  where ctxt = TC.Context {
                   TC.currentRule = "",
                   TC.values = "values",
                   TC.out = Builder.byteString,
                   TC.tOut = Builder.byteString,
                   TC.ntOut = Builder.byteString
               }

main :: IO ()
main = do
    opts <- Opt.execParser options
    ipgInput <- case inFile opts of Nothing -> LBS.getContents; Just f -> LBS.readFile f
    h <- case outFile opts of Nothing -> return stdout; Just f -> openFile f WriteMode
    case parse (not (noValidation opts)) ipgInput of
        Left errs -> mapM_ (hPutStrLn stderr) errs
        Right (preamble, core', _, _, postamble) ->
            case commandOpts opts of
                CORE coreOpts ->
                    typeCheckCore (coreTypeCheckFlag coreOpts) core' $ \core -> do
                        LBS.hPutStrLn h (Builder.toLazyByteString (pprint core))
                INTERPRET interpretOpts -> do
                    typeCheckCore (interpreterTypeCheckFlag interpretOpts) core' $ \core -> do
                        buf <- CBS.getContents
                        case interpret core externalFuncs [] buf of
                            Nothing -> hPutStrLn h "null"
                            Just (bs, _, _) ->
                                LBS.hPutStrLn h
                                    (Builder.toLazyByteString (asJSON (BINDINGS bs)))
                JS jsOpts -> do
                    typeCheckCore (jsTypeCheckFlag jsOpts) core' $ \core -> do
                        LBS.hPutStrLn h preamble
                        LBS.hPutStrLn h (JS.toJSWithContext
                            (JS.defaultContext {
                                JS.debugMode = jsDebugModeFlag jsOpts,
                                JS.asyncMode = asyncModeFlag jsOpts,
                                JS.leaveExtraFields = leaveExtraFieldsFlag jsOpts
                            }) core)
                        LBS.hPutStr h postamble
                RUST rustOpts -> do
                    let ctxt = RS.defaultContext {
                        RS.debugMode = rsDebugModeFlag rustOpts,
                        RS.dumpCore = dumpCoreFlag rustOpts,
                        RS.mutableFields = mutableFieldsFlag rustOpts
                      }
                    case RS.toRustWithContext ctxt core' of
                        Left err -> LBS.hPutStrLn stderr (Builder.toLazyByteString err)
                        Right rs -> do
                            LBS.hPutStrLn h preamble
                            LBS.hPutStrLn h rs
                            LBS.hPutStr h postamble
    hClose h

externalFuncs :: Map.Map NT ([Value a] -> Value a)
externalFuncs = Map.fromList [
    ("decodeAscii", \[SEQUENCE cs] ->
        STRING (BS.pack $ map (\(INT c) -> fromIntegral c) cs)),
    ("makeEntry", \[name, descr, typ] ->
        BINDINGS (Map.fromList [("name", name), ("descriptor", descr), ("type", typ)])),
    ("projectSections", \[SEQUENCE sections] ->
        SEQUENCE (map (\(BINDINGS b) -> b Map.! "section") sections)),
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
