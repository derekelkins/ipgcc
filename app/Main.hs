module Main where
import JSExport
import IPGParser ( parse )

main :: IO ()
main = getContents >>= putStrLn . toJS . parse
-- main = getContents >>= print . parse

{-
const example = new Uint8Array([2,0,1,0,3,0,4,0]);
console.log(S(example));
-}
