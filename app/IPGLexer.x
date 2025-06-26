{
module IPGLexer( alexScanTokens, Token(..) ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lex.Integral as I -- bytestring-lexing
import qualified Data.ByteString.Lex.Fractional as F -- bytestring-lexing
}

%wrapper "basic-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]
$ascii = \x00-\xFF
$print = $printable # [^$ascii]
$stringchar = $print # [\n\r\f\v]

tokens :-
    $white+ ;
    "//".*  ;
    "0x" $hex $hex? { TokenInt . readHex }
    $digit+ \. $digit* { TokenDouble . readDouble }
    $digit+ { TokenInt . readInteger }
    ^"%declare" { \_ -> TokenDeclare }
    "%instrument" { \_ -> TokenInstrument }
    "%end" { \_ -> TokenEndDeclare }
    EOI     { \_ -> TokenEOI }
    repeat  { \_ -> TokenRepeat }
    until   { \_ -> TokenUntil }
    START   { \_ -> TokenStart }
    END     { \_ -> TokenEnd }
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
    [_ $alpha] [_ $alpha $digit]* { TokenName . LBS.toStrict }
    \" ($stringchar#[\"\\]|\\[0abfnrtv\\\"']|\\x$hex$hex)* \" { TokenString . readString }

{
data Token
    = TokenEOI
    | TokenDeclare
    | TokenInstrument
    | TokenEndDeclare
    | TokenRepeat
    | TokenUntil
    | TokenStart
    | TokenEnd
    | TokenFor
    | TokenTo
    | TokenDo
    | TokenInt !Integer
    | TokenDouble !Double
    | TokenString BS.ByteString
    | TokenName BS.ByteString
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

readInteger :: LBS.ByteString -> Integer
readInteger bs = case I.readDecimal (LBS.toStrict bs) of Just (n, _) -> n

readHex :: LBS.ByteString -> Integer
readHex bs = case I.readHexadecimal (LBS.toStrict (LBS.drop 2 bs)) of Just (n, _) -> n

readDouble :: LBS.ByteString -> Double
readDouble bs = case F.readDecimal (LBS.toStrict bs) of Just (d, _) -> d

readString :: LBS.ByteString -> BS.ByteString
readString = BS.concat . go . BS.tail . BS.init . LBS.toStrict
    where go bs = case BS.span (\c -> c /= 0x5C) bs of
                    (before, after) ->
                        case BS.uncons after of
                            Nothing -> [before]
                            Just (_, rest) -> before : handleEscapes (BS.head rest) (BS.tail rest)
          handleEscapes 0x30 rest = BS.singleton 0x00 : go rest
          handleEscapes 0x61 rest = BS.singleton 0x07 : go rest
          handleEscapes 0x62 rest = BS.singleton 0x08 : go rest
          handleEscapes 0x66 rest = BS.singleton 0x0C : go rest
          handleEscapes 0x6E rest = BS.singleton 0x0A : go rest
          handleEscapes 0x72 rest = BS.singleton 0x0D : go rest
          handleEscapes 0x74 rest = BS.singleton 0x09 : go rest
          handleEscapes 0x76 rest = BS.singleton 0x0B : go rest
          handleEscapes 0x5C rest = BS.singleton 0x5C : go rest
          handleEscapes 0x22 rest = BS.singleton 0x22 : go rest
          handleEscapes 0x27 rest = BS.singleton 0x27 : go rest
          handleEscapes 0x78 rest = BS.singleton hex : go rest'
            where (hexString, rest') = BS.splitAt 2 rest
                  hex = case I.readHexadecimal hexString of Just (n, _) -> n
}
