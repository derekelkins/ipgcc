{
module IPGParser (
    IdType, Exp', Grammar', Rule', Alternative', Term', Ref',
    parseIPG, parse, parseWithStartPos,
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as CLBS -- bytestring
import Data.List ( intersperse ) -- base

import CoreIPG ( Ref(..), MetaTag(..) )
import FullIPG ( Grammar(..), Rule(..), Alternative(..), Term(..) )
import GenericExp ( Exp(..) )
import IPGLexer ( alexError, alexGetInput, alexMonadScan, alexSetInput, getCurrentLine, runAlex,
                  Alex, AlexInput, AlexPosn(..), Token(..) )

-- Decent intro: https://serokell.io/blog/parsing-with-happy
}

%name parseIPG Top
%tokentype { Token }
%error { parseError }
%error.expected
%monad { Alex }
%lexer { lexer } { TokenEOF }

%expect 1

%token
    '%declare' { TokenDeclare }
    '%instrument' { TokenInstrument }
    '%end'  { TokenEndDeclare }
    EOI     { TokenEOI }
    repeat  { TokenRepeat }
    until   { TokenUntil }
    START   { TokenStart }
    END     { TokenEnd }
    for     { TokenFor }
    to      { TokenTo }
    do      { TokenDo }
    int     { TokenInt $$ }
    double  { TokenDouble $$ }
    string  { TokenString $$ }
    name    { TokenName $$ }
    '?['    { TokenGuard }
    '?'     { TokenQuestion }
    ':'     { TokenColon }
    ';'     { TokenSemicolon }
    '.'     { TokenDot }
    ','     { TokenComma }
    '('     { TokenLParen }
    ')'     { TokenRParen }
    '{'     { TokenLCurly }
    '}'     { TokenRCurly }
    '['     { TokenLBracket }
    ']'     { TokenRBracket }
    '<<'    { TokenLShift }
    '>>'    { TokenRShift }
    '<'     { TokenLAngle }
    '>'     { TokenRAngle }
    '->'    { TokenArrow }
    '='     { TokenEq }
    '=='    { TokenEqual }
    '!='    { TokenNotEqual }
    '!'     { TokenNot }
    '<='    { TokenLTE }
    '>='    { TokenGTE }
    '&'     { TokenAmpersand }
    '|'     { TokenPipe }
    '^'     { TokenCaret }
    '%'     { TokenPercent }
    '~'     { TokenTilde }
    '&&'    { TokenAnd }
    '||'    { TokenOr }
    '+'     { TokenAdd }
    '-'     { TokenSub }
    '**'    { TokenExp }
    '*'     { TokenMul }
    '/'     { TokenDiv }

-- Javascript Operator Precedence table according to:
-- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Operator_precedence

-- 18 Grouping: N/A: [ "(x)" ]
-- 17 Access and Call: Left [ "x.y", "x?.y" ], N/A: [ "x[y]", "new x(y)", "x(y)", "import(x)" ]
-- 16 New: N/A: [ "new x" ]
-- 15 Postfix: N/A: [ "x++", "x--" ]
-- 14 Prefix: N/A: [ "++x", "--x", "!x", "~x", "+x", "-x", "typeof x", "void x", "delete x", "await x" ]
-- 13 Exponentiation: Right: [ "x ** y" ]
-- 12 Multiplicative: Left: [ "x * y", "x / y", "x % y" ]
-- 11 Additive: Left: [ "x + y", "x - y" ]
-- 10 Bitwise Shift: Left: [ "x << y", "x >> y", "x >>> y" ]
-- 9 Relational: Left: [ "x < y", "x <= y", "x > y", "x >= y", "x in y", "x instanceof y" ]
-- 8 Equality: Left: [ "x == y", "x != y", "x === y", "x !== y" ]
-- 7 Bitwise And: Left: [ "x & y" ]
-- 6 Bitwise Xor: Left: [ "x ^ y" ]
-- 5 Bitwise Or: Left: [ "x | y" ]
-- 4 Logical And: Left: [ "x && y" ]
-- 3 Logical Or: Left: [ "x || y", "x ?? y" ]
-- 2 Assignment: Right: [ "x = y", "x += y", "x -= y", "x **= y", "x *= y", "x /= y", "x %= y",
--                        "x <<= y", "x >>= y", "x >>>= y", "x &= y", "x ^= y", "x |= y",
--                        "x &&= y", "x ||= y", "x ??= y", "x ? y : z", "x => y"]
--                 N/A: [ "yield x", "yield* x", "...x" ]
-- 1 Comma: Left: [ "x, y" ]

%right ':'
%right '=' '?'              -- 2
%left '||'                  -- 3
%left '&&'                  -- 4
%left '|'                   -- 5
%left '^'                   -- 6
%left '&'                   -- 7
%left '==' '!='             -- 8
%left '<' '>' '<=' '>='     -- 9
%left '<<' '>>'             -- 10
%left '+' '-'               -- 11
%left '*' '/' '%'           -- 12
%right '**'                 -- 13
%nonassoc NEG PLUS '~' '!'  -- 14
%nonassoc '['               -- 17
%left '.'                   -- 17

%%

Top :: { (Grammar', [IdType]) }
    : MaybeDeclarations Grammar { ($2, $1) }

MaybeDeclarations :: { [IdType] }
    : '%declare' Declarations '%end' { reverse $2 }
    | {- empty -} { [] }

Declarations :: { [IdType] }
    : Declarations name { $2 : $1 }
    | name { [$1] }

Grammar :: { Grammar' }
    : Rules { Grammar (reverse $1) }

Rules :: { [Rule'] }
    : Rules ';' Rule { $3 : $1 }
    | Rules ';' { $1 }
    | Rule { [$1] }
    | {- empty -} { [] }

Rule :: { Rule' }
    : MaybeInstrument name ParamList '->' Alternatives { Rule $1 $2 $3 (reverse $5) }

MaybeInstrument :: { [MetaTag] }
    : '%instrument' { [INSTRUMENT] }
    | {- empty -} { [] }

ParamList :: { [IdType] }
    : '(' Params ')' { reverse $2 }
    | {- empty -} { [] }

Params :: { [IdType] }
    : {- empty -} { [] }
    | name { [$1] }
    | Params ',' name { $3 : $1 }

Alternatives :: { [Alternative'] }
    : Alternative { [$1] }
    | Alternatives '/' Alternative { $3 : $1 }

Alternative :: { Alternative' }
    : Terms { Alternative (reverse $1) }

Terms :: { [Term'] }
    : Term { [$1] }
    | Terms Term { $2 : $1 }

Term :: { Term' }
    : name ArgList { NonTerminal0 $1 $2 }
    | name ArgList '[' Exp ']' { NonTerminal1 $1 $2 $4 }
    | name ArgList '[' Exp ',' Exp ']' { NonTerminal2 $1 $2 $4 $6 }
    | string { Terminal0 $1 }
    | string '[' Exp ']' { Terminal1 $1 $3 }
    | string '[' Exp ',' Exp ']' { Terminal2 $1 $3 $5 }
    | '{' name '=' AssignTail '}' { makeAssign $2 $4 }
    | '?[' Exp ']' { Guard $2 }
    | for name '=' Exp to Exp do name ArgList '[' Exp ',' Exp ']' { Array $2 $4 $6 $8 $9 $11 $13 }
    | repeat name ArgList '.' name UntilTail { case $6 of Just (n,es) -> RepeatUntil $2 $3 $5 n es; _ -> Repeat $2 $3 $5 }
    -- TODO: Liberalize repeat-until syntax.

UntilTail :: { Maybe (IdType, [Exp']) }
    : until name ArgList { Just ($2, $3) }
    | {- empty -} { Nothing }

ArgList :: { [Exp'] }
    : '(' Args ')' { reverse $2 }
    | {- empty -} { [] }

AssignTail :: { AssignTail }
    : '.' { Any0' }
    | '.' '[' Exp ']' { Any1' $3 }
    | '*' { Slice0' }
    | '*' '[' Exp ']' { Slice1' $3 }
    | '*' '[' Exp ',' Exp ']' { Slice2' $3 $5 }
    | Exp  { Assign' $1 }

Exp :: { Exp' }
    : int { Int $1 }
    | double { Float $1 }
    | string { String $1 }
    | Exp '+' Exp { Add $1 $3 }
    | Exp '-' Exp { Sub $1 $3 }
    | '-' Exp %prec NEG { Neg $2 }
    | '+' Exp %prec PLUS { $2 }
    | Exp '*' Exp { Mul $1 $3 }
    | Exp '/' Exp { Div $1 $3 }
    | Exp '%' Exp { Mod $1 $3 }
    | Exp '**' Exp { Exp $1 $3 }
    | Exp '&&' Exp { And $1 $3 }
    | Exp '||' Exp { Or $1 $3 }
    | Exp '&' Exp { BitwiseAnd $1 $3 }
    | Exp '^' Exp { BitwiseXor $1 $3 }
    | Exp '|' Exp { BitwiseOr $1 $3 }
    | Exp '<<' Exp { LSh $1 $3 }
    | Exp '>>' Exp { RSh $1 $3 }
    | Exp '<' Exp { LessThan $1 $3 }
    | Exp '<=' Exp { LTE $1 $3 }
    | Exp '>' Exp { GreaterThan $1 $3 }
    | Exp '>=' Exp { GTE $1 $3 }
    | Exp '==' Exp { Equal $1 $3 }
    | Exp '!=' Exp { NotEqual $1 $3 }
    | '!' Exp { Not $2 }
    | '~' Exp { BitwiseNeg $2 }
    | Exp '?' Exp ':' Exp { If $1 $3 $5 }
    | Exp '[' Exp ']' { At $1 $3 }
    | '(' Exp ')' { $2 }
    | EOI { Ref EOI }
    | NameExp { $1 }

NameExp :: { Exp' }
    : name NameExpTail { makeExp $1 $2 }

NameExpTail :: { NameExpTail }
    : {- empty -} { Id' }
    | '.' START { Start' }
    | '.' END { End' }
    | '.' name { Attr' $2 }
    | '(' Exp ')' MaybeIndex { case $4 of Just i -> Index' $2 i;_ -> Call' [$2] }
    | '(' Args ')' { Call' (reverse $2) }

MaybeIndex :: { Maybe IdType }
    : '.' name { Just $2 }
    | {- empty -} { Nothing }

Args :: { [Exp'] }
    : {- empty -} { [] }
    | Exp { [$1] }
    | Args ',' Exp { $3 : $1 }

{
type IdType = BS.ByteString
type Exp' = Exp IdType IdType IdType
type Grammar' = Grammar IdType IdType IdType Exp'
type Rule' = Rule IdType IdType IdType Exp'
type Alternative' = Alternative IdType IdType IdType Exp'
type Term' = Term IdType IdType IdType Exp'
type Ref' = Ref IdType IdType Exp'

data NameExpTail
    = Start'                -- name '.' START               { Ref (Start $1) }
    | End'                  -- name '.' END                 { Ref (End $1) }
    | Attr' IdType          -- name '.' name                { Ref (Attr $1 $3) } 
    | Index' Exp' IdType    -- name '(' Exp ')' '.' name    { Ref (Index $1 $3 $6) }
    | Call' [Exp']          -- name '(' Args ')'            { Call $1 (reverse $3) }
    | Id'                   -- name                         { Ref (Id $1) }

makeExp :: IdType -> NameExpTail -> Exp'
makeExp t Start' = Ref (Start t)
makeExp t End' = Ref (End t)
makeExp t (Attr' i) = Ref (Attr t i)
makeExp t (Index' e i) = Ref (Index t e i)
makeExp t (Call' es) = Call t es
makeExp t Id' = Ref (Id t)

data AssignTail
    = Any0'
    | Any1' Exp'
    | Slice0'
    | Slice1' Exp'
    | Slice2' Exp' Exp'
    | Assign' Exp'

makeAssign :: IdType -> AssignTail -> Term'
makeAssign n Any0' = Any0 n 
makeAssign n (Any1' e) = Any1 n e
makeAssign n Slice0' = Slice0 n
makeAssign n (Slice1' l) = Slice1 n l
makeAssign n (Slice2' l r) = Slice2 n l r
makeAssign n (Assign' e) = n := e

parse :: LBS.ByteString -> Either String (Grammar', [IdType])
parse input = runAlex input parseIPG

parseWithStartPos :: Int -> Int -> Int -> LBS.ByteString -> Either String (Grammar', [IdType])
parseWithStartPos n l col input = runAlex input $ do
    (_, c, bs, bpos) <- alexGetInput
    alexSetInput (AlexPn n l col, c, bs, bpos)
    parseIPG

lexer :: (Token -> Alex a) -> Alex a
lexer action = alexMonadScan >>= action

parseError :: Token -> [String] -> Alex a
parseError _ followers = do
    (AlexPn _ line col, _, _, _) <- alexGetInput
    errLine <- getCurrentLine
    alexError (
        errLine <> "\n"
     <> replicate (col - 2) ' ' <> "^\n"
     <> "Parse error at line " <> show line <> ", column " <> show (col - 1) <> ".\n"
     <> "Possible following tokens are: " <> unwords (intersperse "," followers))
}
