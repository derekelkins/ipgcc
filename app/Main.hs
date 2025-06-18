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

{-
const fs = require("node:fs");
console.log(JSON.stringify(QOI(fs.readFileSync("./qoi_test_images/dice.qoi"))));
-}
