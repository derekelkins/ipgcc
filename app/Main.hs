module Main ( main ) where
import Data.Char ( isSpace ) -- base
import Data.List ( isPrefixOf ) -- base
import FullIPG ( ExpHelpers(..), toCore )
import qualified GenericExp as E
import IPGParser ( IdType, Exp', parse )
import JSExport ( toJS )

helper :: ExpHelpers IdType IdType IdType Exp'
helper = ExpHelpers {
    len = E.Int . fromIntegral . length,
    add = E.Add,
    num = E.Int . fromIntegral,
    ref = E.Ref
  }  

-- Replace with Data.Text.breakOn when I switch to Text.
breakOn :: String -> String -> Maybe (String, String)
breakOn _ [] = Nothing
breakOn needle s@(c:cs)
    | needle `isPrefixOf` s = Just ("", s)
    | otherwise = fmap (\(x, y) -> (c:x, y)) (breakOn needle cs)

main :: IO ()
main = do
    input' <- getContents 
    let (preamble, rest) = case breakOn "\n%preamble_end" input' of
            Nothing -> ("", input')
            Just (x, p) -> (x, dropWhile isSpace (drop 14 p))
    let (input, postamble) = case breakOn "\n%postamble_begin" rest of
            Nothing -> (rest, "")
            Just (x, p) -> (x, dropWhile isSpace (drop 17 p))
    let (g, _decls) = parse input
    let core = E.simplify (toCore helper g)
    -- TODO: validate decls core
    putStrLn preamble
    putStrLn (toJS core)
    putStr postamble
