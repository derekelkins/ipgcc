{-# LANGUAGE LambdaCase #-}
module Main where
import CoreIPG

main :: IO ()
main = putStrLn (toJS exampleGrammar1)

constExpr :: T -> Expr
constExpr t = Expr (CrudeExpr undefined [Right t])

ref :: Ref T T Expr -> Expr
ref r = Expr (CrudeExpr (\case 0 -> r) [Left 0])

exampleGrammar1 = Grammar [
    Rule "S" [Alternative [
        NonTerminal "H" (constExpr "0") (constExpr "2"),
        Array "i" (constExpr "0") (ref (Attr "H" "num")) "A"
            (Expr (CrudeExpr (\case 0 -> Id "i") [Right "2 + 2*", Left 0]))
            (Expr (CrudeExpr (\case 0 -> Id "i") [Right "2 + 2*(", Left 0, Right " + 1)"])),
        "a_0" := ref (Index "A" (constExpr "0") "val"),
        Guard (Expr (CrudeExpr (\case 0 -> Id "a_0") [Left 0, Right " > 0 && 10 > ", Left 0]))
    ]],
    Rule "H" [Alternative [
        NonTerminal "Int" (constExpr "0") (constExpr "2"),
        "num" := ref (Attr "Int" "val")
    ]],
    Rule "A" [Alternative [
        NonTerminal "Int" (constExpr "0") (constExpr "2"),
        "val" := ref (Attr "Int" "val")
    ]],
    Rule "Int" [Alternative [
       Any "lo" (constExpr "0"),
       Any "hi" (constExpr "1"),
       "val" := Expr (CrudeExpr (\case 0 -> Id "lo"; 1 -> Id "hi")
                    [Left 0, Right " + 256*", Left 1])
    ]]
  ]
{-
const example = new Uint8Array([2,0,1,0,3,0,4,0]);

function process(input) {
    const result = S(input, 0, input.length);
    console.log(result);
}

process(example);
-}
