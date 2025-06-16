{
module IPGLexer( alexScanTokens, Token(..) ) where
}

%wrapper "basic" -- TODO: Replace with "text" or "strict-text".

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
    $white+ ;
    "//".*  ;
    $digit+ \. $digit* { TokenDouble . read }
    $digit+ { TokenInt . read }
    EOI     { \_ -> TokenEOI }
    start   { \_ -> TokenStart }
    end     { \_ -> TokenEnd }
    for     { \_ -> TokenFor }
    to      { \_ -> TokenTo }
    do      { \_ -> TokenDo }
    "?["    { \_ -> TokenGuard }
    "?"     { \_ -> TokenQuestion }
    ":"     { \_ -> TokenColon }
    ";"     { \_ -> TokenSemicolon }
    "."     { \_ -> TokenDot }
    ","     { \_ -> TokenComma }
    "("     { \_ -> TokenLParen }
    ")"     { \_ -> TokenRParen }
    "{"     { \_ -> TokenLCurly }
    "}"     { \_ -> TokenRCurly }
    "["     { \_ -> TokenLBracket }
    "]"     { \_ -> TokenRBracket }
    "**"    { \_ -> TokenExp }
    "<<"    { \_ -> TokenLShift }
    ">>"    { \_ -> TokenRShift }
    "<="    { \_ -> TokenLTE }
    ">="    { \_ -> TokenGTE }
    "<"     { \_ -> TokenLAngle }
    ">"     { \_ -> TokenRAngle }
    "->"    { \_ -> TokenArrow }
    "=="    { \_ -> TokenEqual }
    "!="    { \_ -> TokenNotEqual }
    "!"     { \_ -> TokenNot }
    "="     { \_ -> TokenEq }
    "&&"    { \_ -> TokenAnd }
    "||"    { \_ -> TokenOr }
    "&"     { \_ -> TokenAmpersand }
    "|"     { \_ -> TokenPipe }
    "^"     { \_ -> TokenCaret }
    "%"     { \_ -> TokenPercent }
    "~"     { \_ -> TokenTilde }
    "+"     { \_ -> TokenAdd }
    "-"     { \_ -> TokenSub }
    "*"     { \_ -> TokenMul }
    "/"     { \_ -> TokenDiv }
    [_ $alpha] [_ $alpha $digit]* { TokenName }
    \" [^\"]* \" { \(_:s) -> TokenString (init s) } -- TODO: Handle properly escaped strings.

{
data Token
    = TokenEOI
    | TokenStart
    | TokenEnd
    | TokenFor
    | TokenTo
    | TokenDo
    | TokenInt !Integer
    | TokenDouble !Double
    | TokenString String
    | TokenName String
    | TokenGuard
    | TokenQuestion
    | TokenColon
    | TokenSemicolon
    | TokenDot
    | TokenComma
    | TokenLParen
    | TokenRParen
    | TokenLCurly
    | TokenRCurly
    | TokenLBracket
    | TokenRBracket
    | TokenLShift
    | TokenRShift
    | TokenLAngle
    | TokenRAngle
    | TokenArrow
    | TokenEqual
    | TokenNotEqual
    | TokenNot
    | TokenEq
    | TokenLTE
    | TokenGTE
    | TokenAmpersand
    | TokenPipe
    | TokenCaret
    | TokenPercent
    | TokenExp
    | TokenTilde
    | TokenAnd
    | TokenOr
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
  deriving ( Eq, Show )
}
