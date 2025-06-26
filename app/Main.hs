{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as LBS -- bytestring
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers
import System.Environment ( getArgs ) -- base
import System.IO ( stderr ) -- base

import Data.ByteString.Lazy.Search ( breakOn ) -- stringsearch

import CheckIPG ( validate )
import FullIPG ( ExpHelpers(..), toCore )
import qualified GenericExp as E
import Interpreter ( Bindings, NT, Value(..), interpret )
import IPGParser ( IdType, Exp', parse )
-- import JSExport ( toJS )

helper :: ExpHelpers IdType IdType IdType Exp'
helper = ExpHelpers {
    len = E.Int . fromIntegral . BS.length,
    add = E.Add,
    num = E.Int . fromIntegral,
    ref = E.Ref
  }  

isSpace :: Char -> Bool
isSpace c = c `elem` (" \n\r\t" :: String)

splitAround :: BS.ByteString -> LBS.ByteString -> Maybe (LBS.ByteString, LBS.ByteString)
splitAround pattern s
    = if LBS.null after then Nothing
                        else Just (before, LBS.drop (fromIntegral $ BS.length pattern) after)
  where (before, after) = breakOn pattern s

externalFuncs :: Map.Map NT ([Value ()] -> Value ())
externalFuncs = Map.fromList [
    ("empty", \[] -> BINDINGS Map.empty),
    ("decodeAscii", \[SEQUENCE cs] -> STRING (BS.pack $ map (\(INT c) -> fromIntegral c) cs)),
    ("makeEntry", \[name, descr, typ] ->
        BINDINGS (Map.fromList [("name", name), ("descriptor", descr), ("type", typ)])),
    ("projectSections", \[SEQUENCE sections] ->
        SEQUENCE (map (\(BINDINGS b) -> b Map.! "section") sections))
  ]

main :: IO ()
main = do
    (interpreterInput:_) <- getArgs
    input' <- LBS.getContents 
    let (preamble, rest) = case splitAround "\n%preamble_end" input' of
            Nothing -> ("", input')
            Just (x, p) -> (x, LBS.dropWhile isSpace p)
    let (input, postamble) = case splitAround "\n%postamble_begin" rest of
            Nothing -> (rest, "")
            Just (x, p) -> (x, LBS.dropWhile isSpace p)
    let (g, decls) = parse input
    let core = E.simplify (toCore helper g)
    case validate (Set.fromList decls) core of
        Just errs -> mapM_ (CBS.hPutStrLn stderr) errs
        Nothing -> do
            buf <- CBS.readFile interpreterInput
            case interpret core externalFuncs [] buf of
                Nothing -> putStrLn "null"
                Just results -> print results
        -- Nothing -> do
        --     LBS.putStrLn preamble
        --     LBS.putStrLn (toJS core)
        --     LBS.putStr postamble
