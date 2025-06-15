module Main ( main ) where
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

main :: IO ()
main = getContents >>= putStrLn . toJS . E.simplify . toCore helper . parse
-- main = getContents >>= print . parse

{-
const example = new Uint8Array([2,0,1,0,3,0,4,0]);
console.log(S(example));
-}
