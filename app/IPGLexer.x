{
module IPGLexer(
    alexError, alexGetInput, alexMonadScan, runAlex,
    Alex, AlexInput(..), AlexPosn(..), Token(..)
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lex.Integral as I -- bytestring-lexing
import qualified Data.ByteString.Lex.Fractional as F -- bytestring-lexing
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]
$ascii = \x00-\xFF
$print = \x20-\x7E
$stringchar = $print # [\n\r\f\v]

tokens :-
<0>    $white+ ;
<0>    "//".*  ;
<0>    "/*"    { nestComment `andBegin` cmt }
<0>    "*/"    { \inp len -> alexError "Unexpected closing comment" }
<cmt>  "/*"    { nestComment }
<cmt>  "*/"    { unnestComment }
<cmt>  "."     ;
<cmt>  "\n"    ;
<0>    "0x" $hex $hex?
               { token $ \inp len -> TokenInt (readHex (current inp len)) }
<0>    $digit+ \. $digit*
               { token $ \inp len -> TokenDouble (readDouble (current inp len)) }
<0>    $digit+ { token $ \inp len -> TokenInt (readInteger (current inp len)) }
<0>    ^"%declare"
               { token $ \inp len -> TokenDeclare }
<0>    "%instrument"
               { token $ \inp len -> TokenInstrument }
<0>    "%end"  { token $ \inp len -> TokenEndDeclare }
<0>    EOI     { token $ \inp len -> TokenEOI }
<0>    repeat  { token $ \inp len -> TokenRepeat }
<0>    until   { token $ \inp len -> TokenUntil }
<0>    START   { token $ \inp len -> TokenStart }
<0>    END     { token $ \inp len -> TokenEnd }
<0>    for     { token $ \inp len -> TokenFor }
<0>    to      { token $ \inp len -> TokenTo }
<0>    do      { token $ \inp len -> TokenDo }
<0>    "?["    { token $ \inp len -> TokenGuard }
<0>    "?"     { token $ \inp len -> TokenQuestion }
<0>    ":"     { token $ \inp len -> TokenColon }
<0>    ";"     { token $ \inp len -> TokenSemicolon }
<0>    "."     { token $ \inp len -> TokenDot }
<0>    ","     { token $ \inp len -> TokenComma }
<0>    "("     { token $ \inp len -> TokenLParen }
<0>    ")"     { token $ \inp len -> TokenRParen }
<0>    "{"     { token $ \inp len -> TokenLCurly }
<0>    "}"     { token $ \inp len -> TokenRCurly }
<0>    "["     { token $ \inp len -> TokenLBracket }
<0>    "]"     { token $ \inp len -> TokenRBracket }
<0>    "**"    { token $ \inp len -> TokenExp }
<0>    "<<"    { token $ \inp len -> TokenLShift }
<0>    ">>"    { token $ \inp len -> TokenRShift }
<0>    "<="    { token $ \inp len -> TokenLTE }
<0>    ">="    { token $ \inp len -> TokenGTE }
<0>    "<"     { token $ \inp len -> TokenLAngle }
<0>    ">"     { token $ \inp len -> TokenRAngle }
<0>    "->"    { token $ \inp len -> TokenArrow }
<0>    "=="    { token $ \inp len -> TokenEqual }
<0>    "!="    { token $ \inp len -> TokenNotEqual }
<0>    "!"     { token $ \inp len -> TokenNot }
<0>    "="     { token $ \inp len -> TokenEq }
<0>    "&&"    { token $ \inp len -> TokenAnd }
<0>    "||"    { token $ \inp len -> TokenOr }
<0>    "&"     { token $ \inp len -> TokenAmpersand }
<0>    "|"     { token $ \inp len -> TokenPipe }
<0>    "^"     { token $ \inp len -> TokenCaret }
<0>    "%"     { token $ \inp len -> TokenPercent }
<0>    "~"     { token $ \inp len -> TokenTilde }
<0>    "+"     { token $ \inp len -> TokenAdd }
<0>    "-"     { token $ \inp len -> TokenSub }
<0>    "*"     { token $ \inp len -> TokenMul }
<0>    "/"     { token $ \inp len -> TokenDiv }
<0>    [_ $alpha] [_ $alpha $digit]*
               { token $ \inp len -> TokenName (LBS.toStrict (current inp len)) }
<0>    \" ($stringchar#[\"\\]|\\[0abfnrtv\\\"']|\\x$hex$hex)* \"
               { token $ \inp len -> TokenString (readString (current inp len)) }

{
data Token
    = TokenEOF
    | TokenEOI
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

type AlexUserState = Int

alexInitUserState = 0 :: Int

current :: AlexInput -> Int64 -> LBS.ByteString 
current (_, _, s, _) len = LBS.take len s

alexEOF :: Alex Token
alexEOF = do
    c <- alexGetStartCode
    if c == cmt then
        alexError "Error: unclosed comment"
      else
        return TokenEOF

nestComment :: AlexAction Token
nestComment input len = do
    l <- alexGetUserState
    alexSetUserState (l + 1)
    skip input len

unnestComment :: AlexAction Token
unnestComment input len = do
    l <- alexGetUserState
    alexSetUserState (l - 1)
    if l == 1 then
        alexSetStartCode 0
      else
        return ()
    skip input len
}
