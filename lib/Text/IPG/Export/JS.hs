{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Text.IPG.Export.JS ( Context(..), T, defaultContext, toJS, toJSWithContext ) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.List ( intersperse ) -- base
import qualified Data.Set as Set -- containers

import Data.String.Interpolate ( i, __i ) -- string-interpolate

import Text.IPG.Core (
    Grammar(..), Rule(..), Alternative(..), Term(..), Ref(..), MetaTag(..),
    arrayNonTerminals, nonTerminals, )
import Text.IPG.GenericExp ( UnOp(..), BinOp(..), Exp(..) )
import Text.IPG.PPrint ( floatToOut, hexyString, outParen, pprintTerm )

type T = BS.ByteString
type Out = Builder.Builder
type Expr = Exp T T T
type Env = Set.Set T

data Context = Context {
    debugMode :: !Bool,
    asyncMode :: !Bool,
    leaveExtraFields :: !Bool,
    constants :: Set.Set T,
    iterationVar :: T
  }

defaultContext :: Context
defaultContext = Context {
    debugMode = False,
    asyncMode = False,
    leaveExtraFields = False,
    constants = Set.empty,
    iterationVar = ""
  }

whenDebug :: Context -> Out -> Out
whenDebug (Context { debugMode = True }) o = o
whenDebug (Context { debugMode = False }) _ = ""

u :: (T, Int) -> Out
u (nt, n) = Builder.byteString nt <> "_" <> Builder.intDec n

refToJS :: Context -> Env -> Ref T T Expr -> Out
refToJS c env (Id x)
    | x == iterationVar c = [i|i_#{x}|]
    | x `Set.member` env  = [i|a_#{x}|]
    | not (x `Set.member` constants c) = [i|self.#{x}|]
    | otherwise = [i|#{x}|]
refToJS c _   (Attr nt "this")
    | leaveExtraFields c = [i|nt_#{u nt}|] -- Probably still need to clone
    | otherwise = [i|(({_ipg_start,_ipg_end,...o}) => o)(nt_#{u nt})|]
refToJS c _   (Attr nt "these")
    | leaveExtraFields c = [i|seq_#{u nt}|] -- Probably still need to clone
    | otherwise = [i|seq_#{u nt}.map(({_ipg_start,_ipg_end,...o}) => o)|]
refToJS _ _   (Attr nt f) = [i|nt_#{u nt}.#{f}|]
refToJS c env (Index nt e "this")
    | leaveExtraFields c = -- Probably still need to clone
        [i|(({_ipg_start,_ipg_end,...o}) => o)(seq_#{u nt}[#{exprToJS c env e} - seq_#{u nt}_start])|]
    | otherwise = [i|seq_#{u nt}[#{exprToJS c env e} - seq_#{u nt}_start]|]
refToJS c env (Index nt e f) = [i|seq_#{u nt}[#{exprToJS c env e} - seq_#{u nt}_start].#{f}|]
refToJS _ _   EOI = "EOI";
refToJS _ _   (Start nt) = [i|nt_#{u nt}._ipg_start|]
refToJS _ _   (End nt) = [i|nt_#{u nt}._ipg_end|]

exprToJS :: Context -> Env -> Expr -> Out
exprToJS ctxt env e = exprToJS' ctxt env 0 e

exprToJS' :: Context -> Env -> Int -> Expr -> Out
exprToJS' _ _ _ T = "true"
exprToJS' _ _ _ F = "false"
exprToJS' _ _ _ (Int n) = Builder.int64Dec n
exprToJS' _ _ _ (Float n) = floatToOut n
exprToJS' _ _ _ (String s) = hexyString s
exprToJS' c env p (Bin Add l r) =
    outParen (p > 11) (exprToJS' c env 11 l <> " + " <> exprToJS' c env 12 r)
exprToJS' c env p (Bin Sub l r) =
    outParen (p > 11) (exprToJS' c env 11 l <> " - " <> exprToJS' c env 12 r)
exprToJS' c env p (Bin Mul l r) =
    outParen (p > 12) (exprToJS' c env 12 l <> " * " <> exprToJS' c env 13 r)
exprToJS' c env p (Bin Div l r) =
    outParen (p > 12) (exprToJS' c env 12 l <> " / " <> exprToJS' c env 13 r)
exprToJS' c env p (Bin Mod l r) =
    outParen (p > 12) (exprToJS' c env 12 l <> " % " <> exprToJS' c env 13 r)
exprToJS' c env p (Bin Exp l r) =
    outParen (p > 13) (exprToJS' c env 14 l <> " ** " <> exprToJS' c env 13 r)
exprToJS' c env p (Un Neg e) =
    outParen (p > 14) ("-" <> exprToJS' c env 15 e)
exprToJS' c env p (Un BitwiseNeg e) =
    outParen (p > 14) ("~" <> exprToJS' c env 15 e)
exprToJS' c env p (Bin And l r) =
    outParen (p > 4) (exprToJS' c env 4 l <> " && " <> exprToJS' c env 5 r)
exprToJS' c env p (Bin Or l r) =
    outParen (p > 3)  (exprToJS' c env 3 l <> " || " <> exprToJS' c env 4 r)
exprToJS' c env p (Bin BitwiseAnd l r) =
    outParen (p > 7) (exprToJS' c env 7 l <> " & " <> exprToJS' c env 8 r)
exprToJS' c env p (Bin BitwiseXor l r) =
    outParen (p > 6)  (exprToJS' c env 6 l <> " ^ " <> exprToJS' c env 7 r)
exprToJS' c env p (Bin BitwiseOr l r) =
    outParen (p > 5)  (exprToJS' c env 5 l <> " | " <> exprToJS' c env 6 r)
exprToJS' c env p (Bin LSh l r) =
    outParen (p > 10) (exprToJS' c env 10 l <> " << " <> exprToJS' c env 11 r)
exprToJS' c env p (Bin RSh l r) =
    outParen (p > 10) (exprToJS' c env 10 l <> " >> " <> exprToJS' c env 11 r)
exprToJS' c env p (Bin LessThan l r) =
    outParen (p > 9) (exprToJS' c env 9 l <> " < " <> exprToJS' c env 10 r)
exprToJS' c env p (Bin LTE l r) =
    outParen (p > 9) (exprToJS' c env 9 l <> " <= " <> exprToJS' c env 10 r)
exprToJS' c env p (Bin GreaterThan l r) =
    outParen (p > 9) (exprToJS' c env 9 l <> " > " <> exprToJS' c env 10 r)
exprToJS' c env p (Bin GTE l r) =
    outParen (p > 9) (exprToJS' c env 9 l <> " >= " <> exprToJS' c env 10 r)
exprToJS' c env p (Bin Equal l r) =
    outParen (p > 8) (exprToJS' c env 8 l <> " == " <> exprToJS' c env 9 r)
exprToJS' c env p (Bin NotEqual l r) =
    outParen (p > 8) (exprToJS' c env 8 l <> " != " <> exprToJS' c env 9 r)
exprToJS' c env p (Un Not l) =
    outParen (p > 14) ("!" <> exprToJS' c env 15 l)
exprToJS' c env p (If b t e) =
    outParen (p > 2)
        (exprToJS' c env 2 b <> " ? " <> exprToJS' c env 3 t <> " : " <> exprToJS' c env 3 e)
exprToJS' c env _ (Call t es) =
    Builder.byteString t <> "(" <> mconcat (intersperse ", " $ map (exprToJS' c env 0) es) <> ")"
exprToJS' c env p (Bin At l r) =
    outParen (p > 17) (exprToJS' c env 17 l <> "[" <> exprToJS' c env 0 r <> "]")
exprToJS' c env _ (Ref r) = refToJS c env r

paramList :: [T] -> T
paramList = BS.concat . map (", a_"<>)

argList :: [Out] -> Out
argList = foldMap ((", "<>))

-- left and right will be the interval *actually* consumed by the previous term if
-- it is a consuming term, otherwise it will be unchanged from earlier terms.
-- For Array, currently, we treat the "previous term" as the last iteration.
termToJS :: Out -> Context -> Env -> Term T T T Expr -> Out
termToJS indent c env z@(NonTerminal nt args l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)}, left, right };\n|])
   <> indent <> [i|right = #{rExp};\n|]
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|nt_#{u nt} = #{await}#{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|if (nt_#{u nt} === null) break _ipg_alt;\n|]
   <> indent <> [i|if (nt_#{u nt}._ipg_end !== 0) {\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, left + nt_#{u nt}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, left + nt_#{u nt}._ipg_end);\n|]
   <> indent <>   "}\n"
   <> indent <> [i|nt_#{u nt}._ipg_end += left;\n|]
   <> indent <> [i|nt_#{u nt}._ipg_start += left;\n|]
   <> indent <> [i|left = nt_#{u nt}._ipg_start;\n|]
   <> indent <> [i|right = nt_#{u nt}._ipg_end;\n\n|]
  where lExp = exprToJS c env l; rExp = exprToJS c env r; es = map (exprToJS c env) args
        await = if asyncMode c then "await " else "" :: T
termToJS indent c env z@(Terminal "" l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)}, left, right };\n|])
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n\n"
  where lExp = exprToJS c env l; rExp = exprToJS c env r
termToJS indent c env z@(Terminal t l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)}, left, right };\n|])
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|if (!#{await}_ipg_startsWith(input, begin + left, begin + right, #{terminal})) break _ipg_alt;\n|]
   <> indent <>   "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <> [i|right = left + #{BS.length t};\n\n|]
  where lExp = exprToJS c env l; rExp = exprToJS c env r; terminal = hexyString t
        await = if asyncMode c then "await " else "" :: T
termToJS indent c env z@(x := e)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|self.#{x} = #{eExp};\n\n|]
  where eExp = exprToJS c env e
termToJS indent c env z@(Guard e)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)} };\n|])
   <> indent <> [i|if (!#{eExp}) break _ipg_alt;\n\n|]
  where eExp = exprToJS' c env 15 e
termToJS indent c env z@(Array x start end nt args l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)} };\n|])
   <> indent <> [i|nt_#{u nt} = { _ipg_end: right, _ipg_start: left };\n|] -- Special case
   <> indent <> [i|seq_#{u nt}_start = #{startExp};\n|]
   <> indent <> [i|loopEnd = #{endExp};\n|]
   <> indent <> [i|seq_#{u nt} = new Array(Math.max(0, loopEnd - seq_#{u nt}_start));\n|]
   <> indent <> [i|for (let i_#{x} = seq_#{u nt}_start; i_#{x} < loopEnd; i_#{x}++) {\n|]
   <> indent <> [i|  const left = #{lExp};\n|]
   <> indent <> [i|  const right = #{rExp};\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm.left = left; _ipg_failedTerm.right = right;\n|])
   <> indent <>   "  if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|  const tmp = #{await}#{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <>   "  if (tmp === null) break _ipg_alt;\n"
   <> indent <>   "  if (tmp._ipg_end !== 0) {\n"
   <> indent <>   "    self._ipg_start = Math.min(self._ipg_start, left + tmp._ipg_start);\n"
   <> indent <>   "    self._ipg_end = Math.max(self._ipg_end, left + tmp._ipg_end);\n"
   <> indent <>   "  }\n"
   <> indent <>   "  tmp._ipg_end += left;\n"
   <> indent <>   "  tmp._ipg_start += left;\n"
   <> indent <> [i|  nt_#{u nt}._ipg_end = tmp._ipg_end;\n|] -- Special case
   <> indent <> [i|  nt_#{u nt}._ipg_start = tmp._ipg_start;\n|] -- Special case
   <> indent <> [i|  seq_#{u nt}[i_#{x} - seq_#{u nt}_start] = tmp;\n|]
   <> indent <>   "}\n"
   <> indent <> [i|left = nt_#{u nt}._ipg_start;\n|]
   <> indent <> [i|right = nt_#{u nt}._ipg_end;\n\n|]
  where startExp = exprToJS c env start; endExp = exprToJS' c env 10 end;
        lExp = exprToJS c' env l; rExp = exprToJS c' env r; es = map (exprToJS c' env) args
        await = if asyncMode c then "await " else "" :: T
        c' = c { iterationVar = x }
termToJS indent c env z@(Any x l)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <>   "right = left + 1;\n"
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)}, left, right };\n|])
   <> indent <>   "if (left < 0 || right > EOI) break _ipg_alt;\n"
   <> indent <>   asyncAt
   <> indent <>   "self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "self._ipg_end = Math.max(self._ipg_end, right);\n\n"
  where lExp = exprToJS c env l
        asyncAt | asyncMode c =  [i|self.#{x} = await input.at(begin + left);\n|]
                | otherwise = [i|self.#{x} = input[begin + left];\n|]
termToJS indent c env z@(Slice x l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp};\n|]
   <> indent <> [i|right = #{rExp};\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)}, left, right };\n|])
   <> indent <>   "if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|self.#{x} = #{await}input.slice(begin + left, begin + right);\n|]
   <> indent <>   "if (left !== right) {\n"
   <> indent <>   "  self._ipg_start = Math.min(self._ipg_start, left);\n"
   <> indent <>   "  self._ipg_end = Math.max(self._ipg_end, right);\n"
   <> indent <>   "}\n\n"
  where lExp = exprToJS c env l; rExp = exprToJS c env r
        await = if asyncMode c then "await " else "" :: T
termToJS indent c env z@(Repeat nt args l r x l0 r0)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <>   "self.values = [];\n"
   <> indent <> [i|left = #{l0Exp};\n|]
   <> indent <> [i|right = #{r0Exp};\n|]
   <> indent <> [i|nt_#{u nt} = #{await}#{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|if (nt_#{u nt} !== null) {\n|]
   <> indent <> [i|  if (nt_#{u nt}._ipg_end === 0) throw 'repeat of non-consuming rule: #{fst nt}';\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, left + nt_#{u nt}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, left + nt_#{u nt}._ipg_end);\n|]
   <> indent <> [i|  nt_#{u nt}._ipg_end += left;\n|]
   <> indent <> [i|  nt_#{u nt}._ipg_start += left;\n|]
   <> indent <> [i|  left = #{lExp};\n|]
   <> indent <> [i|  right = #{rExp};\n|]
   <> indent <> [i|  self.values.push(#{xAttr});\n\n|]

   <> indent <>   "  while (left >= 0 && left <= right && right <= EOI) {\n"
   <> indent <> [i|    nt_#{u nt} = #{await}#{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|    if (nt_#{u nt} === null) break;\n|]
   <> indent <> [i|    if (nt_#{u nt}._ipg_end === 0) throw 'repeat of non-consuming rule: #{fst nt}';\n|]
   <> indent <> [i|    self._ipg_start = Math.min(self._ipg_start, left + nt_#{u nt}._ipg_start);\n|]
   <> indent <> [i|    self._ipg_end = Math.max(self._ipg_end, left + nt_#{u nt}._ipg_end);\n|]
   <> indent <> [i|    nt_#{u nt}._ipg_end += left;\n|]
   <> indent <> [i|    nt_#{u nt}._ipg_start += left;\n|]
   <> indent <> [i|    self.values.push(#{xAttr});\n|]
   <> indent <> [i|    left = #{lExp};\n|]
   <> indent <> [i|    right = #{rExp};\n|]
   <> indent <>   "  }\n"
   <> indent <>   "}\n\n"
  where es = map (exprToJS c env) args
        lExp = exprToJS c env l; rExp = exprToJS c env r
        l0Exp = exprToJS c env l0; r0Exp = exprToJS c env r0
        await = if asyncMode c then "await " else "" :: T
        xAttr = refToJS c env (Attr nt x)
termToJS indent c env z@(RepeatUntil nt1 args1 l r x l0 r0 nt2 args2)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> whenDebug c (indent <> [i|_ipg_failedTerm = { term: #{show (pprintTerm z)} };\n|])
   <> indent <> [i|left = #{l0Exp};\n|]
   <> indent <> [i|right = #{r0Exp};\n|]
   <> indent <>   "self.values = [];\n"
   <> indent <>   "while (true) {\n"
   <> indent <>   "  if (left < 0 || right < left || right > EOI) break _ipg_alt;\n"
   <> indent <> [i|  nt_#{u nt2} = #{await}#{fst nt2}(input, begin + left, begin + right#{argList es2});\n|]
   <> indent <> [i|  if (nt_#{u nt2} !== null) {\n|]
   <> indent <> [i|    if (nt_#{u nt2}._ipg_end !== 0) {\n|]
   <> indent <> [i|      self._ipg_start = Math.min(self._ipg_start, left + nt_#{u nt2}._ipg_start);\n|]
   <> indent <> [i|      self._ipg_end = Math.max(self._ipg_end, left + nt_#{u nt2}._ipg_end);\n|]
   <> indent <>   "    }\n"
   <> indent <> [i|    nt_#{u nt2}._ipg_end += left;\n|]
   <> indent <> [i|    nt_#{u nt2}._ipg_start += left;\n|]
   <> indent <> [i|    right = nt_#{u nt2}._ipg_end;\n|]
   <> indent <>   "    break;\n"
   <> indent <>   "  }\n"
   <> indent <> [i|  nt_#{u nt1} = #{await}#{fst nt1}(input, begin + left, begin + right#{argList es1});\n|]
   <> indent <> [i|  if (nt_#{u nt1} === null) break _ipg_alt;\n|]
   <> indent <> [i|  if (nt_#{u nt1}._ipg_end === 0) throw 'repeat of non-consuming rule: #{fst nt1}';\n|]
   <> indent <> [i|  self._ipg_start = Math.min(self._ipg_start, left + nt_#{u nt1}._ipg_start);\n|]
   <> indent <> [i|  self._ipg_end = Math.max(self._ipg_end, left + nt_#{u nt1}._ipg_end);\n|]
   <> indent <> [i|  nt_#{u nt1}._ipg_end += left;\n|]
   <> indent <> [i|  nt_#{u nt1}._ipg_start += left;\n|]
   <> indent <> [i|  self.values.push(#{xAttr});\n|]
   <> indent <> [i|  left = #{lExp};\n|]
   <> indent <> [i|  right = #{rExp};\n|]
   <> indent <>   "}\n\n"
  where es1 = map (exprToJS c env) args1; es2 = map (exprToJS c env) args2
        lExp = exprToJS c env l; rExp = exprToJS c env r
        l0Exp = exprToJS c env l0; r0Exp = exprToJS c env r0
        await = if asyncMode c then "await " else "" :: T
        xAttr = refToJS c env (Attr nt1 x)

alternativeToJS :: Maybe (T, [T]) -> Out -> Context -> Env -> Alternative T T T Expr -> Out
alternativeToJS instrument indent c env (Alternative ts)
    = indent <> "_ipg_alt: {\n"
   <> indent <> "  let left = EOI; let right = 0; let loopEnd = 0;\n"
   <>              foldMap declare nts
   <>              foldMap declareSeqs seqs
   <> indent <> "  self = { _ipg_start: EOI, _ipg_end: 0 };\n\n"
   <>              foldMap (termToJS ("  " <> indent) c env) ts
   <>              instrumentation
   <>              debuggingPostamble
   <> indent <> "  return self;\n"
   <> indent <> "}\n"
  where nts = nonTerminals ts
        seqs = arrayNonTerminals ts
        declare nt = indent <> [i|  let nt_#{u nt};\n|]
        declareSeqs nt = indent <> [i|  let seq_#{u nt}; let seq_#{u nt}_start = 0;\n|]
        instrumentation = case instrument of
                            Nothing -> ""
                            -- Purposely over-indented.
                            Just (nt, args) -> indent <>
                                [i|    console.error({#{nt}: self#{paramList args}});\n|]
        debuggingPostamble = whenDebug c (indent <> "_ipg_failTreeStack.pop();\n")

constToJS :: Context -> (T, Expr) -> Out
constToJS c (n, e) = [i|const #{n} = #{exprToJS c Set.empty e};\n|]

ruleToJS :: Context -> Rule T T T Expr -> Out
ruleToJS c (Rule mt nt args alts) =
    [__i|
      #{export}#{async}function #{nt}(input, begin = 0, end = input.length#{paramList args}) {
        const EOI = end - begin; let self;
        #{debuggingPreamble}
      #{foldMap (alternativeToJS instrument "  " c env) alts}
        #{debuggingPostamble}
        return null;
      }\n\n
    |]
  where env = Set.fromList args
        export = if EXPORT `elem` mt then "export " else "" :: T
        instrument = if INSTRUMENT `elem` mt then Just (nt, args) else Nothing
        async = if asyncMode c then "async " else "" :: T
        debuggingPreamble = whenDebug c [__i|
            const _ipg_currentFailTree = {
                rule: "#{nt}",
                args: [#{mconcat (intersperse ", " (map ("a_"<>) args))}],
                begin,
                end,
                children: []
            };
            let _ipg_failedTerm = null;
            _ipg_failTreeStack.push(_ipg_currentFailTree);
          |]
        debuggingPostamble = whenDebug c [__i|
            _ipg_failTreeStack.pop();
            _ipg_currentFailTree.failedTerm = _ipg_failedTerm;
            _ipg_failTreeStack[_ipg_failTreeStack.length - 1].children.push(_ipg_currentFailTree);
          |]

toJSWithContext :: Context -> Grammar T T T Expr -> LBS.ByteString
toJSWithContext c (Grammar ruleOrConsts) = Builder.toLazyByteString $
      startsWith
   <> whenDebug c [__i|
      const _ipg_failTreeRoot = { children: [] };
      const _ipg_failTreeStack = [_ipg_failTreeRoot];\n
    |]
   <> foldMap (either (ruleToJS c') (constToJS c')) ruleOrConsts
  where c' = c {
                constants = foldMap (either (const Set.empty) (Set.singleton . fst)) ruleOrConsts
             }
        startsWith
            | asyncMode c = [__i|
                async function _ipg_startsWith(input, l, r, prefix) {
                  if (r - l < prefix.length) return false;
                  const s = await input.slice(l, r);
                  for (let i = 0; i < prefix.length; ++i) {
                    if (s[i] !== prefix.charCodeAt(i)) return false;
                  }
                  return true;
                }\n|]
            | otherwise = [__i|
                function _ipg_startsWith(s, l, r, prefix) {
                  if (r - l < prefix.length) return false;
                  if (typeof s === 'string') return s.startsWith(prefix, l);
                  for (let i = 0; i < prefix.length; ++i) {
                    if (s[l + i] !== prefix.charCodeAt(i)) return false;
                  }
                  return true;
                }\n|]

toJS :: Grammar T T T Expr -> LBS.ByteString
toJS = toJSWithContext defaultContext
