{
module IPGParser (
    IdType, Exp', Grammar', Rule', Alternative', Term', Ref',
    parseIPG, parse,
) where
import CoreIPG ( Ref(..) )
import FullIPG ( Grammar(..), Rule(..), Alternative(..), Term(..) )
import GenericExp ( Exp(..) )
import IPGLexer ( alexScanTokens, Token(..) )
}

%name parseIPG
%tokentype { Token }
%error { parseError }

%token
    EOI     { TokenEOI }
    start   { TokenStart }
    end     { TokenEnd }
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
    '&&'    { TokenAnd }
    '||'    { TokenOr }
    '+'     { TokenAdd }
    '-'     { TokenSub }
    '*'     { TokenMul }
    '/'     { TokenDiv }

-- TODO: Look up the actual precedence table.
%nonassoc '='
%nonassoc '?'
%left '&&' '||'
%nonassoc '<' '>' '<=' '>=' '==' '!='
%nonassoc '<<' '>>'
%left '+' '-'
%left '*' '/'
%left NEG
%left '!'
%nonassoc '['

%%

Grammar :: { Grammar' }
    : Rules { Grammar (reverse $1) }

Rules :: { [Rule'] }
    : Rules ';' Rule { $3 : $1 }
    | Rules ';' { $1 }
    | Rule { [$1] }
    | {- empty -} { [] }

Rule :: { Rule' }
    : name '->' Alternatives { Rule $1 (reverse $3) }

Alternatives :: { [Alternative'] }
    : Alternative { [$1] }
    | Alternatives '/' Alternative { $3 : $1 }

Alternative :: { Alternative' }
    : Terms { Alternative (reverse $1) }

Terms :: { [Term'] }
    : Term { [$1] }
    | Terms Term { $2 : $1 }

Term :: { Term' }
    : name { NonTerminal0 $1 }
    | name '[' Exp ']' { NonTerminal1 $1 $3 }
    | name '[' Exp ',' Exp ']' { NonTerminal2 $1 $3 $5 }
    | string { Terminal0 $1 }
    | string '[' Exp ']' { Terminal1 $1 $3 }
    | string '[' Exp ',' Exp ']' { Terminal2 $1 $3 $5 }
    | '{' name '=' AssignTail '}' { makeAssign $2 $4 }
    | '?[' Exp ']' { Guard $2 }
    | for name '=' Exp to Exp do name '[' Exp ',' Exp ']' { Array $2 $4 $6 $8 $10 $12 }

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
    | Exp '*' Exp { Mul $1 $3 }
    | Exp '/' Exp { Div $1 $3 }
    | Exp '&&' Exp { And $1 $3 }
    | Exp '||' Exp { Or $1 $3 }
    | Exp '<<' Exp { LSh $1 $3 }
    | Exp '>>' Exp { RSh $1 $3 }
    | Exp '<' Exp { LessThan $1 $3 }
    | Exp '<=' Exp { LTE $1 $3 }
    | Exp '>' Exp { GreaterThan $1 $3 }
    | Exp '>=' Exp { GTE $1 $3 }
    | Exp '==' Exp { Equal $1 $3 }
    | Exp '!=' Exp { NotEqual $1 $3 }
    | '!' Exp { Not $2 }
    | Exp '?' Exp ':' Exp { If $1 $3 $5 }
    | Exp '[' Exp ']' { At $1 $3 }
    | '(' Exp ')' { $2 }
    | EOI { Ref EOI }
    | NameExp { $1 }

NameExp :: { Exp' }
    : name NameExpTail { makeExp $1 $2 }

NameExpTail :: { NameExpTail }
    : {- empty -} { Id' }
    | '.' start { Start' }
    | '.' end { End' }
    | '.' name { Attr' $2 }
    | '(' Exp ')' '.' name { Index' $2 $5 } -- TODO: Deal with this conflict.
    | '(' Args ')' { Call' (reverse $2) }

Args :: { [Exp'] }
    : Exp { [$1] }
    | Args ',' Exp { $3 : $1 }

{
type IdType = String
type Exp' = Exp IdType
type Grammar' = Grammar IdType IdType IdType Exp'
type Rule' = Rule IdType IdType IdType Exp'
type Alternative' = Alternative IdType IdType IdType Exp'
type Term' = Term IdType IdType IdType Exp'
type Ref' = Ref IdType IdType Exp'

data NameExpTail
    = Start'                -- name '.' start               { Ref (Start $1) }
    | End'                  -- name '.' end                 { Ref (End $1) }
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

parse :: String -> Grammar'
parse = parseIPG . alexScanTokens

parseError :: [Token] -> a
parseError ts = error ("Parse error: " ++ show ts) -- TODO: Do something better.
}
