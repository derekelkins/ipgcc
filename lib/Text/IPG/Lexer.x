{
module Text.IPG.Lexer(
    alexError, alexGetInput, alexMonadScan, alexSetInput, getCurrentLine, runAlex,
    Alex, AlexInput, AlexPosn(..), Token(..)
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Char8 as CBS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Lazy.Char8 as CLBS -- bytestring
import qualified Data.ByteString.Lex.Integral as I -- bytestring-lexing
import qualified Data.ByteString.Lex.Fractional as F -- bytestring-lexing

-- Decent intro: https://serokell.io/blog/lexing-with-alex
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]
$hex = [0-9a-fA-F]
$ascii = \x00-\xFF
$print = [\t \x20-\x7E]
$stringchar = $print # [\n\r\f\v]
$nnl = $white # [\n]

tokens :-
<0>    \n      { saveLine }
<0>    $nnl+   ;
<0>    "//".*  ;
<0>    "/*"    { nestComment `andBegin` cmt }
<0>    "*/"    { unexpectedCloseComment  }
<cmt>  "/*"    { nestComment }
<cmt>  "*/"    { unnestComment }
<cmt>  .       ;
<cmt>  \n      ;
<0>    "0x" $hex $hex?
               { token $ \inp len -> TokenInt (readHex (current inp len)) }
<0>    $digit+ \. $digit*
               { token $ \inp len -> TokenDouble (readDouble (current inp len)) }
<0>    $digit+ { token $ \inp len -> TokenInt (readInteger (current inp len)) }
<0>    ^"%declare"
               { token $ \_ _ -> TokenDeclare }
<0>    "%instrument"
               { token $ \_ _ -> TokenInstrument }
<0>    "%end"  { token $ \_ _ -> TokenEndDeclare }
<0>    EOI     { token $ \_ _ -> TokenEOI }
<0>    repeat  { token $ \_ _ -> TokenRepeat }
<0>    starting
               { token $ \_ _ -> TokenStarting }
<0>    on      { token $ \_ _ -> TokenOn }
<0>    until   { token $ \_ _ -> TokenUntil }
<0>    START   { token $ \_ _ -> TokenStart }
<0>    END     { token $ \_ _ -> TokenEnd }
<0>    for     { token $ \_ _ -> TokenFor }
<0>    to      { token $ \_ _ -> TokenTo }
<0>    do      { token $ \_ _ -> TokenDo }
<0>    "?["    { token $ \_ _ -> TokenGuard }
<0>    "?"     { token $ \_ _ -> TokenQuestion }
<0>    ":"     { token $ \_ _ -> TokenColon }
<0>    ";"     { token $ \_ _ -> TokenSemicolon }
<0>    "."     { token $ \_ _ -> TokenDot }
<0>    ","     { token $ \_ _ -> TokenComma }
<0>    "("     { token $ \_ _ -> TokenLParen }
<0>    ")"     { token $ \_ _ -> TokenRParen }
<0>    "{"     { token $ \_ _ -> TokenLCurly }
<0>    "}"     { token $ \_ _ -> TokenRCurly }
<0>    "["     { token $ \_ _ -> TokenLBracket }
<0>    "]"     { token $ \_ _ -> TokenRBracket }
<0>    "**"    { token $ \_ _ -> TokenExp }
<0>    "<<"    { token $ \_ _ -> TokenLShift }
<0>    ">>"    { token $ \_ _ -> TokenRShift }
<0>    "<="    { token $ \_ _ -> TokenLTE }
<0>    ">="    { token $ \_ _ -> TokenGTE }
<0>    "<"     { token $ \_ _ -> TokenLAngle }
<0>    ">"     { token $ \_ _ -> TokenRAngle }
<0>    "->"    { token $ \_ _ -> TokenArrow }
<0>    "=="    { token $ \_ _ -> TokenEqual }
<0>    "!="    { token $ \_ _ -> TokenNotEqual }
<0>    "!"     { token $ \_ _ -> TokenNot }
<0>    "="     { token $ \_ _ -> TokenEq }
<0>    "&&"    { token $ \_ _ -> TokenAnd }
<0>    "||"    { token $ \_ _ -> TokenOr }
<0>    "&"     { token $ \_ _ -> TokenAmpersand }
<0>    "|"     { token $ \_ _ -> TokenPipe }
<0>    "^"     { token $ \_ _ -> TokenCaret }
<0>    "%"     { token $ \_ _ -> TokenPercent }
<0>    "~"     { token $ \_ _ -> TokenTilde }
<0>    "+"     { token $ \_ _ -> TokenAdd }
<0>    "-"     { token $ \_ _ -> TokenSub }
<0>    "*"     { token $ \_ _ -> TokenMul }
<0>    "/"     { token $ \_ _ -> TokenDiv }
<0>    [_ $alpha] [_ $alpha $digit]* "@" $digit+
               { token $ \inp len -> tokenNonTerminal (LBS.toStrict (current inp len)) }
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
    | TokenStarting
    | TokenOn
    | TokenUntil
    | TokenStart
    | TokenEnd
    | TokenFor
    | TokenTo
    | TokenDo
    | TokenInt !Integer
    | TokenDouble !Double
    | TokenString BS.ByteString
    | TokenNonTerminal (BS.ByteString, Int)
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

tokenNonTerminal :: BS.ByteString -> Token
tokenNonTerminal s = TokenNonTerminal (name, idx)
    where (name, idxString) = CBS.break ('@' ==) s
          idx = case I.readDecimal (BS.tail idxString) of Just (n, _) -> fromIntegral n

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

data AlexUserState = AlexUserState {
    nestingLevel :: !Int,
    commentStart :: Maybe AlexPosn,
    currentLine :: !BS.ByteString
  }

alexInitUserState = AlexUserState {
    nestingLevel = 0,
    commentStart = Nothing,
    currentLine = BS.empty
  }

current :: AlexInput -> Int64 -> LBS.ByteString
current (_, _, s, _) len = LBS.take len s

saveLine :: AlexAction Token
saveLine input@(_, _, s, _) len = do
    let line = LBS.toStrict (CLBS.takeWhile ('\n' /=) (LBS.tail s))
    state <- alexGetUserState
    alexSetUserState (state { currentLine = line })
    skip input len

getCurrentLine :: Alex String
getCurrentLine = do
    state <- alexGetUserState
    return (CBS.unpack (currentLine state))

alexEOF :: Alex Token
alexEOF = do
    c <- alexGetStartCode
    if c == cmt then do
        state <- alexGetUserState
        case commentStart state of
            Just (AlexPn _ line col) ->
                alexError $
                 "Unclosed comment starting at line " <> show line <> ", column " <> show col <> "."
            Nothing -> error "Unset commentStart. This should never happen."
      else
        return TokenEOF

nestComment :: AlexAction Token
nestComment input@(pos, _, _, _) len = do
    state <- alexGetUserState
    let level = nestingLevel state
    if level == 0 then
        alexSetUserState (state { nestingLevel = 1, commentStart = Just pos })
      else
        alexSetUserState (state { nestingLevel = level + 1 })
    skip input len

unnestComment :: AlexAction Token
unnestComment input len = do
    state <- alexGetUserState
    let level = nestingLevel state
    if level == 1 then do
        alexSetStartCode 0
        alexSetUserState (state { nestingLevel = level - 1, commentStart = Nothing })
      else
        alexSetUserState (state { nestingLevel = level - 1 })
    skip input len

unexpectedCloseComment :: AlexAction Token
unexpectedCloseComment (AlexPn _ line col, _, _, _) _ = do
    errLine <- getCurrentLine
    alexError (
        errLine <> "\n"
     <> replicate (col - 1) ' ' <> "^\n"
     <> "Unexpected closing comment at line " <> show line <> ", column " <> show col <> ".")
}
