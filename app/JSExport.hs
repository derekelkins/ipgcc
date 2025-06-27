{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module JSExport ( hexyString, toJS, T ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.Char ( ord ) -- base
import Data.List ( intersperse ) -- base
import qualified Data.Set as Set -- containers

import Data.String.Interpolate ( i, __i ) -- string-interpolate

import CoreIPG
import GenericExp ( Exp(..) )

-- It's worth noting that the way this export works already supports blackbox parsers.
-- ANY function that takes a slice and returns an object with the _ipg_start/_ipg_end fields
-- suitably set can just immediately be referenced. If we assume a blackbox parser will always
-- consume its full input, we can just return _ipg_start = 0, _ipg_end = input.length.

type T = BS.ByteString
type Out = Builder.Builder
type Expr = Exp T T T
type Env = Set.Set T

refToJS :: Env -> Ref T T Expr -> Out
refToJS env (Id f) | f `Set.member` env = [i|a_#{f}|]
                   | otherwise          = [i|self.#{f}|]
refToJS _   (Attr nt "this") = [i|(({_ipg_start,_ipg_end,...o}) => o)(nt_#{nt})|]
refToJS _   (Attr nt "these") = [i|seq_#{nt}.map(({_ipg_start,_ipg_end,...o}) => o)|]
refToJS _   (Attr nt f) = [i|nt_#{nt}.#{f}|]
refToJS env (Index nt e "this") =
    [i|(({_ipg_start,_ipg_end,...o}) => o)(seq_#{nt}[#{exprToJS env e} - seq_#{nt}_start])|]
refToJS env (Index nt e f) = [i|seq_#{nt}[#{exprToJS env e} - seq_#{nt}_start].#{f}|]
refToJS _   EOI = "EOI";
refToJS _   (Start nt) = [i|nt_#{nt}._ipg_start|]
refToJS _   (End nt) = [i|nt_#{nt}._ipg_end|]

exprToJS :: Env -> Expr -> Out
exprToJS env e = exprToJS' env 0 e

outParen :: Bool -> Out -> Out
outParen True x = "(" <> x <> ")"
outParen False x = x

exprToJS' :: Env -> Int -> Expr -> Out
exprToJS' _ _ (Int n) = Builder.integerDec n
exprToJS' _ _ (Float n) = floatToOut n
    where floatToOut = mconcat . map (Builder.word8 . fromIntegral . ord) . show -- TODO: Crude
exprToJS' _ _ (String s) = hexyString s
exprToJS' env p (Add l r) =
    outParen (p > 11) (exprToJS' env 11 l <> " + " <> exprToJS' env 12 r)
exprToJS' env p (Sub l r) =
    outParen (p > 11) (exprToJS' env 11 l <> " - " <> exprToJS' env 12 r)
exprToJS' env p (Mul l r) =
    outParen (p > 12) (exprToJS' env 12 l <> " * " <> exprToJS' env 13 r)
exprToJS' env p (Div l r) =
    outParen (p > 12) (exprToJS' env 12 l <> " / " <> exprToJS' env 13 r)
exprToJS' env p (Mod l r) =
    outParen (p > 12) (exprToJS' env 12 l <> " % " <> exprToJS' env 13 r)
exprToJS' env p (Exp l r) =
    outParen (p > 13) (exprToJS' env 14 l <> " ** " <> exprToJS' env 13 r)
exprToJS' env p (Neg e) =
    outParen (p > 14) ("-" <> exprToJS' env 15 e)
exprToJS' env p (BitwiseNeg e) =
    outParen (p > 14) ("~" <> exprToJS' env 15 e)
exprToJS' env p (And l r) =
    outParen (p > 4) (exprToJS' env 4 l <> " && " <> exprToJS' env 5 r)
exprToJS' env p (Or l r) =
    outParen (p > 3)  (exprToJS' env 3 l <> " || " <> exprToJS' env 4 r)
exprToJS' env p (BitwiseAnd l r) =
    outParen (p > 7) (exprToJS' env 7 l <> " & " <> exprToJS' env 8 r)
exprToJS' env p (BitwiseXor l r) =
    outParen (p > 6)  (exprToJS' env 6 l <> " ^ " <> exprToJS' env 7 r)
exprToJS' env p (BitwiseOr l r) =
    outParen (p > 5)  (exprToJS' env 5 l <> " | " <> exprToJS' env 6 r)
exprToJS' env p (LSh l r) =
    outParen (p > 10) (exprToJS' env 10 l <> " << " <> exprToJS' env 11 r)
exprToJS' env p (RSh l r) =
    outParen (p > 10) (exprToJS' env 10 l <> " >> " <> exprToJS' env 11 r)
exprToJS' env p (LessThan l r) =
    outParen (p > 9) (exprToJS' env 9 l <> " < " <> exprToJS' env 10 r)
exprToJS' env p (LTE l r) =
    outParen (p > 9) (exprToJS' env 9 l <> " <= " <> exprToJS' env 10 r)
exprToJS' env p (GreaterThan l r) =
    outParen (p > 9) (exprToJS' env 9 l <> " > " <> exprToJS' env 10 r)
exprToJS' env p (GTE l r) =
    outParen (p > 9) (exprToJS' env 9 l <> " >= " <> exprToJS' env 10 r)
exprToJS' env p (Equal l r) =
    outParen (p > 8) (exprToJS' env 8 l <> " == " <> exprToJS' env 9 r)
exprToJS' env p (NotEqual l r) =
    outParen (p > 8) (exprToJS' env 8 l <> " != " <> exprToJS' env 9 r)
exprToJS' env p (Not l) =
    outParen (p > 14) ("!" <> exprToJS' env 15 l)
exprToJS' env p (If b t e) =
    outParen (p > 2)
        (exprToJS' env 2 b <> " ? " <> exprToJS' env 3 t <> " : " <> exprToJS' env 3 e)
exprToJS' env _ (Call t es) =
    Builder.byteString t <> "(" <> mconcat (intersperse ", " $ map (exprToJS' env 0) es) <> ")"
exprToJS' env p (At l r) =
    outParen (p > 17) (exprToJS' env 17 l <> "[" <> exprToJS' env 0 r <> "]")
exprToJS' env _ (Ref r) = refToJS env r

paramList :: [T] -> T
paramList = BS.concat . map (", a_"<>)

argList :: [Out] -> Out
argList = mconcat . map ((", "<>))

call :: [Out] -> Out
call [] = ""
call [e] = "(" <> e <> ")"
call (e:es) = "(" <> e <> mconcat (map (", "<>) es) <> ")"

hexyString :: T -> Out
hexyString s = "\"" <> mconcat (map go (BS.unpack s)) <> "\""
    where go 0x5C = "\\\\"
          go 0x22 = "\\\""
          go c | isPrintable c = Builder.word8 c
               | otherwise = "\\x" <> paddedHex c
          paddedHex c = if c >= 16 then Builder.word8Hex c else "0" <> Builder.word8Hex c
          isPrintable c = c >= 0x20 && c <= 0x7E

-- left and right will be the interval *actually* consumed by the previous term if
-- it is a consuming term, otherwise it will be unchanged from earlier terms.
-- For Array, currently, we treat the "previous term" as the last iteration.
termToJS :: Out -> Env -> Term T T T Expr -> Out
termToJS indent env (NonTerminal nt args l r)
    = indent <> [i|// #{nt}#{call es}[#{lExp}, #{rExp}]\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|nt_#{nt} = #{nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|if (nt_#{nt} === null) break _ipg_alt;\n|]
   <> indent <> [i|if (nt_#{nt}._ipg_end !== 0) {\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, left + nt_#{nt}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, left + nt_#{nt}._ipg_end);\n|]
   <> indent <>   "}\n"
   <> indent <> [i|nt_#{nt}._ipg_end += left;\n|]
   <> indent <> [i|nt_#{nt}._ipg_start += left;\n|]
   <> indent <> [i|left = nt_#{nt}._ipg_start;\n|]
   <> indent <> [i|right = nt_#{nt}._ipg_end;\n\n|]
  where lExp = exprToJS env l; rExp = exprToJS env r; es = map (exprToJS env) args
termToJS indent env (Terminal "" l r) 
    = indent <> [i|// ""[#{lExp}, #{rExp}]\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS indent env (Terminal t l r)
    = indent <> [i|// #{terminal}[#{lExp}, #{rExp}]\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|if (!_ipg_startsWith(input, begin + left, begin + right, #{terminal})) break _ipg_alt;\n|]
   <> indent <>   "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <> [i|right = left + #{BS.length t};\n\n|]
  where lExp = exprToJS env l; rExp = exprToJS env r; terminal = hexyString t
termToJS indent env (x := e)
    = indent <> [i|// {#{x} = #{eExp}}\n|]
   <> indent <> [i|self.#{x} = #{eExp};\n\n|]
  where eExp = exprToJS env e
termToJS indent env (Guard e)
    = indent <> [i|// ?[#{eExp}]\n|]
   <> indent <> [i|if (!#{eExp}) break _ipg_alt;\n\n|]
  where eExp = exprToJS' env 15 e
termToJS indent env (Array x start end nt args l r)
    = indent <> [i|// for #{x} = #{startExp} to #{endExp} do #{nt}#{call es}[#{lExp}, #{rExp}]\n|]
   <> indent <> [i|nt_#{nt} = { _ipg_end: right, _ipg_start: left };\n|] -- Special case
   <> indent <> [i|seq_#{nt}_start = #{startExp};\n|]
   <> indent <> [i|loopEnd = #{endExp};\n|]
   <> indent <> [i|seq_#{nt} = new Array(loopEnd - seq_#{nt}_start);\n|]
   <> indent <> [i|for (self.#{x} = seq_#{nt}_start; self.#{x} < loopEnd; self.#{x}++) {\n|]
   <> indent <> [i|  const left = #{lExp};\n|]
   <> indent <> [i|  const right = #{rExp};\n|]
   <> indent <>   "  if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|  const tmp = #{nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <>   "  if (tmp === null) break _ipg_alt;\n"
   <> indent <>   "  if (tmp._ipg_end !== 0) {\n"
   <> indent <>   "    self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);\n"
   <> indent <>   "    self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);\n"
   <> indent <>   "  }\n"
   <> indent <>   "  tmp._ipg_end += left;\n"
   <> indent <>   "  tmp._ipg_start += left;\n"
   <> indent <> [i|  nt_#{nt}._ipg_end = tmp._ipg_end;\n|] -- Special case
   <> indent <> [i|  nt_#{nt}._ipg_start = tmp._ipg_start;\n|] -- Special case
   <> indent <> [i|  seq_#{nt}[self.#{x} - seq_#{nt}_start] = tmp;\n|]
   <> indent <>   "}\n"
   <> indent <> [i|delete self.#{x};\n|]
   <> indent <> [i|left = nt_#{nt}._ipg_start;\n|]
   <> indent <> [i|right = nt_#{nt}._ipg_end;\n\n|]
  where startExp = exprToJS env start; endExp = exprToJS' env 10 end;
        lExp = exprToJS env l; rExp = exprToJS env r; es = map (exprToJS env) args
termToJS indent env (Any x l)
    = indent <> [i|// {#{x} = .[#{lExp}]}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <>   "right = left + 1;\n"
   <> indent <>   "if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|self.#{x} = input[begin + left];\n|]
   <> indent <>   "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS env l
termToJS indent env (Slice x l r)
    = indent <> [i|// {#{x} = *[#{lExp}, #{rExp}]}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|self.#{x} = input.slice(begin + left, begin + right);\n|]
   <> indent <>   "if (left !== right) {\n"
   <> indent <>   "  self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "  self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <>   "}\n\n"
  where lExp = exprToJS env l; rExp = exprToJS env r
termToJS indent env (Repeat nt args x)
    = indent <> [i|// repeat #{nt}#{call es}.#{x}\n|]
   <> indent <>   "self.values = [];\n"
   <> indent <> [i|nt_#{nt} = #{nt}(input, begin + right, begin + EOI#{argList es});\n|]
   <> indent <> [i|if (nt_#{nt} !== null) {\n|]
   <> indent <> [i|  if (nt_#{nt}._ipg_end === 0) throw 'repeat of non-consuming rule: #{nt}';\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, right + nt_#{nt}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, right + nt_#{nt}._ipg_end);\n|]
   <> indent <> [i|  nt_#{nt}._ipg_end += right;\n|]
   <> indent <> [i|  nt_#{nt}._ipg_start += right;\n|]
   <> indent <> [i|  left = nt_#{nt}._ipg_start;\n|]
   <> indent <> [i|  right = nt_#{nt}._ipg_end;\n|]
   <> indent <> [i|  self.values.push(nt_#{nt}.#{x});\n\n|]

   <> indent <>   "  while (right <= EOI) {\n"
   <> indent <> [i|    nt_#{nt} = #{nt}(input, begin + right, begin + EOI#{argList es});\n|]
   <> indent <> [i|    if (nt_#{nt} === null) break;\n|]
   <> indent <> [i|    if (nt_#{nt}._ipg_end === 0) throw 'repeat of non-consuming rule: #{nt}';\n|]
   <> indent <> [i|    self._ipg_start = Math.min(self._ipg_start, right + nt_#{nt}._ipg_start);\n|]
   <> indent <> [i|    self._ipg_end = Math.max(self._ipg_end, right + nt_#{nt}._ipg_end);\n|]
   <> indent <> [i|    nt_#{nt}._ipg_end += right;\n|]
   <> indent <> [i|    nt_#{nt}._ipg_start += right;\n|]
   <> indent <> [i|    self.values.push(nt_#{nt}.#{x});\n|]
   <> indent <> [i|    right = nt_#{nt}._ipg_end;\n|]
   <> indent <>   "  }\n"
   <> indent <>   "}\n\n"
  where es = map (exprToJS env) args
termToJS indent env (RepeatUntil nt1 args1 x nt2 args2)
    = indent <> [i|// repeat #{nt1}#{call es1}.#{x} until #{nt2}#{call es2}\n|]
   <> indent <>   "left = right;\n"
   <> indent <>   "self.values = [];\n"
   <> indent <>   "while (true) {\n"
   <> indent <>   "  if (EOI < right) break _ipg_alt;\n"
   <> indent <> [i|  nt_#{nt2} = #{nt2}(input, begin + right, begin + EOI#{argList es2});\n|]
   <> indent <> [i|  if (nt_#{nt2} !== null) {\n|]
   <> indent <> [i|    if (nt_#{nt2}._ipg_end !== 0) {\n|]
   <> indent <> [i|      self._ipg_start = Math.min(self._ipg_start, right + nt_#{nt2}._ipg_start);\n|]
   <> indent <> [i|      self._ipg_end = Math.max(self._ipg_end, right + nt_#{nt2}._ipg_end);\n|]
   <> indent <>   "    }\n"
   <> indent <> [i|    nt_#{nt2}._ipg_end += right;\n|]
   <> indent <> [i|    nt_#{nt2}._ipg_start += right;\n|]
   <> indent <> [i|    right = nt_#{nt2}._ipg_end;\n|]
   <> indent <>   "    break;\n"
   <> indent <>   "  }\n"
   <> indent <> [i|  nt_#{nt1} = #{nt1}(input, begin + right, begin + EOI#{argList es1});\n|]
   <> indent <> [i|  if (nt_#{nt1} === null) break _ipg_alt;\n|]
   <> indent <> [i|  if (nt_#{nt1}._ipg_end === 0) throw 'repeat of non-consuming rule: #{nt1}';\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, right + nt_#{nt1}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, right + nt_#{nt1}._ipg_end);\n|]
   <> indent <> [i|  nt_#{nt1}._ipg_end += right;\n|]
   <> indent <> [i|  nt_#{nt1}._ipg_start += right;\n|]
   <> indent <> [i|  self.values.push(nt_#{nt1}.#{x});\n|]
   <> indent <> [i|  right = nt_#{nt1}._ipg_end;\n|]
   <> indent <>   "}\n\n"
  where es1 = map (exprToJS env) args1; es2 = map (exprToJS env) args2

alternativeToJS :: Maybe (T, [T]) -> Out -> Env -> Alternative T T T Expr -> Out
alternativeToJS instrument indent env (Alternative ts)
    = indent <> "_ipg_alt: {\n"
   <> indent <> "  let left = EOI; let right = 0; let loopEnd = 0;\n"
   <>              mconcat (map declare nts)
   <>              mconcat (map declareSeqs seqs)
   <> indent <> "  self = { _ipg_start: EOI, _ipg_end: 0 };\n\n"
   <>              mconcat (map (termToJS ("  " <> indent) env) ts)
   <>              instrumentation
   <> indent <> "  return self;\n"
   <> indent <> "}\n"
  where nts = nonTerminals ts
        seqs = arrayNonTerminals ts
        declare nt = indent <> [i|  let nt_#{nt :: T};\n|]
        declareSeqs nt = indent <> [i|  let seq_#{nt :: T}; let seq_#{nt}_start = 0;\n|]
        instrumentation = case instrument of
                            Nothing -> ""
                            -- Purposely over-indented.
                            Just (nt, args) -> indent <>
                                [i|    console.error({#{nt}: self#{paramList args}});\n|]
    
ruleToJS :: Rule T T T Expr -> Out
ruleToJS (Rule mt nt args alts) =
    [__i|
      function #{nt}(input, begin = 0, end = input.length#{paramList args}) {
        const EOI = end - begin; let self;
      #{mconcat (map (alternativeToJS instrument "  " env) alts)}
        return null;
      }\n\n
    |]
  where env = Set.fromList args
        instrument = if INSTRUMENT `elem` mt then Just (nt, args) else Nothing

toJS :: Grammar T T T Expr -> LBS.ByteString
toJS (Grammar rules) = Builder.toLazyByteString $
    [__i| 
      function _ipg_startsWith(s, l, r, prefix) {
        if (r - l < prefix.length) return false;
        if (typeof s === 'string') return s.startsWith(prefix, l);
        for (let i = 0; i < prefix.length; ++i) {
          if (s[l + i] !== prefix.charCodeAt(i)) return false;
        }
        return true;
      }\n
    |]
   <> mconcat (map ruleToJS rules)
