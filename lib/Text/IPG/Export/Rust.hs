{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Text.IPG.Export.Rust (
    Context(..), T,
    defaultContext, toRust, toRustWithContext,
) where
import qualified Data.ByteString as BS -- bytestring
import qualified Data.ByteString.Lazy as LBS -- bytestring
import qualified Data.ByteString.Builder as Builder -- bytestring
import Data.List ( intersperse ) -- base
import qualified Data.Map as Map -- containers
import qualified Data.Set as Set -- containers

import Data.String.Interpolate ( i, __i ) -- string-interpolate

import Text.IPG.Core (
    Ty, Ty'(..), Grammar(..), Declaration(..), Rule(..), Alternative(..), Term(..),
    Ref(..), MetaTag(..),
    partitionDeclarations, )
import Text.IPG.GenericExp ( UnOp(..), BinOp(..), Exp(..) )
import Text.IPG.PPrint ( floatToOut, outParen, pprintTerm, pprint )
import qualified Text.IPG.TypeCheck as TC

type T = BS.ByteString
type Out = Builder.Builder
type Expr = Exp T T T T
type Env = Set.Set T

-- TODO
-- data RefType = MOVE | REF | REFMUT

-- TODO: Use type Ref(a) = a; and type RefMut(a) = a; to indicate parameters that should
-- expect a &/&mut. Use function ref(x: 'a): 'a; and function ref_mut(x: 'a): 'a to indicate
-- expressions to which &/&mut should be applied.
-- Ref/RefMut in a rule parameter list indicates that parameter should be a &/&mut.
-- Ref/RefMut in a function parameter list indicates that argument should always be wrapped
-- with &/&mut when the function is called.
-- ref/ref_mut give more precision.

data Context = Context {
    debugMode :: !Bool,
    mutableFields :: !Bool,
    dumpCore :: !Bool,
    constants :: Set.Set T,
    ruleTypes :: TC.RuleTypes T T T,
    ruleRows :: Map.Map T (T, [T]), -- (struct name, field names)
    -- funArgRefs :: Map.Map T [RefType],
    typeDefs :: TC.TypeDefs T T,
    currentRule :: T,
    iterationVar :: T
  }

defaultContext :: Context
defaultContext = Context {
    debugMode = False,
    mutableFields = False,
    dumpCore = False,
    constants = Set.empty,
    ruleTypes = Map.empty,
    ruleRows = Map.empty,
    typeDefs = Map.empty,
    currentRule = "",
    iterationVar = ""
  }

-- whenDebug :: Context -> Out -> Out
-- whenDebug (Context { debugMode = True }) o = o
-- whenDebug (Context { debugMode = False }) _ = ""

u :: (T, Int) -> Out
u (nt, n) = Builder.byteString nt <> "_" <> Builder.intDec n

-- TODO: Add more primitive types (e.g. u8/u16/u32/u64/i8/i16/i32/i64/f32) to IPG itself?

-- TODO: Evaluate the need for the clones. Worst-case scenario, the user can use a clone
-- function when necessary.
refToRust :: Context -> Env -> Ref T T Expr -> Out
refToRust c env (Id x)
    | x == iterationVar c = [i|i_#{x}|]
    | x `Set.member` env  = [i|a_#{x}|]
    | not (x `Set.member` constants c) = [i|self_#{x}|]
    | otherwise = [i|#{x}|]
refToRust _ _   (Attr nt "this") = [i|nt_#{u nt}|] -- TODO: .clone()|]
refToRust _ _   (Attr nt "these") = [i|seq_#{u nt}|] -- TODO: .clone()|]
refToRust _ _   (Attr nt f) = [i|nt_#{u nt}.#{f}|]
refToRust c env (Index nt e "this") =
    [i|seq_#{u nt}[(#{exprToRust c env e} - seq_#{u nt}_start) as usize].clone()|]
refToRust c env (Index nt e f) =
    [i|seq_#{u nt}[(#{exprToRust c env e} - seq_#{u nt}_start) as usize].#{f}|]
refToRust _ _   EOI = "EOI";
refToRust _ _   (Start nt) = [i|nt_#{u nt}_ipg_start|]
refToRust _ _   (End nt) = [i|nt_#{u nt}_ipg_end|]

exprToRust :: Context -> Env -> Expr -> Out
exprToRust ctxt env e = exprToRust' ctxt env 0 e

explode :: T -> Out
explode = mconcat . intersperse ", " . map Builder.word8Dec . BS.unpack

-- TODO: Compare the precedences to the Rust precedences.
-- See https://doc.rust-lang.org/reference/expressions.html
exprToRust' :: Context -> Env -> Int -> Expr -> Out
exprToRust' _ _ _ T = "true"
exprToRust' _ _ _ F = "false"
exprToRust' _ _ _ (U8 n) = Builder.word8Dec n
exprToRust' _ _ _ (Int n) = Builder.int64Dec n
exprToRust' _ _ _ (Float n) = floatToOut n
exprToRust' _ _ _ (String s) = "vec![" <> explode s <> "]"
exprToRust' c env p (Bin Add l r) = -- TODO: Handle string case somehow.
    outParen (p > 11) (exprToRust' c env 11 l <> " + " <> exprToRust' c env 12 r)
exprToRust' c env p (Bin Sub l r) =
    outParen (p > 11) (exprToRust' c env 11 l <> " - " <> exprToRust' c env 12 r)
exprToRust' c env p (Bin Mul l r) =
    outParen (p > 12) (exprToRust' c env 12 l <> " * " <> exprToRust' c env 13 r)
exprToRust' c env p (Bin Div l r) =
    outParen (p > 12) (exprToRust' c env 12 l <> " / " <> exprToRust' c env 13 r)
exprToRust' c env p (Bin Mod l r) =
    outParen (p > 12) (exprToRust' c env 12 l <> " % " <> exprToRust' c env 13 r)
exprToRust' c env _ (Bin Exp l r) =
    exprToRust' c env 100 l <> ".pow(" <> exprToRust' c env 0 r <> ")"
exprToRust' c env p (Un Neg e) =
    outParen (p > 14) ("-" <> exprToRust' c env 15 e)
exprToRust' c env p (Un BitwiseNeg e) =
    outParen (p > 14) ("!" <> exprToRust' c env 15 e)
exprToRust' c env p (Bin And l r) =
    outParen (p > 4) (exprToRust' c env 4 l <> " && " <> exprToRust' c env 5 r)
exprToRust' c env p (Bin Or l r) =
    outParen (p > 3)  (exprToRust' c env 3 l <> " || " <> exprToRust' c env 4 r)
exprToRust' c env p (Bin BitwiseAnd l r) =
    outParen (p > 7) (exprToRust' c env 7 l <> " & " <> exprToRust' c env 8 r)
exprToRust' c env p (Bin BitwiseXor l r) =
    outParen (p > 6)  (exprToRust' c env 6 l <> " ^ " <> exprToRust' c env 7 r)
exprToRust' c env p (Bin BitwiseOr l r) =
    outParen (p > 5)  (exprToRust' c env 5 l <> " | " <> exprToRust' c env 6 r)
exprToRust' c env p (Bin LSh l r) =
    outParen (p > 10) (exprToRust' c env 10 l <> " << " <> exprToRust' c env 11 r)
exprToRust' c env p (Bin RSh l r) =
    outParen (p > 10) (exprToRust' c env 10 l <> " >> " <> exprToRust' c env 11 r)
exprToRust' c env p (Bin LessThan l r) =
    outParen (p > 9) (exprToRust' c env 9 l <> " < " <> exprToRust' c env 10 r)
exprToRust' c env p (Bin LTE l r) =
    outParen (p > 9) (exprToRust' c env 9 l <> " <= " <> exprToRust' c env 10 r)
exprToRust' c env p (Bin GreaterThan l r) =
    outParen (p > 9) (exprToRust' c env 9 l <> " > " <> exprToRust' c env 10 r)
exprToRust' c env p (Bin GTE l r) =
    outParen (p > 9) (exprToRust' c env 9 l <> " >= " <> exprToRust' c env 10 r)
exprToRust' c env p (Bin Equal l r) =
    outParen (p > 8) (exprToRust' c env 8 l <> " == " <> exprToRust' c env 9 r)
exprToRust' c env p (Bin NotEqual l r) =
    outParen (p > 8) (exprToRust' c env 8 l <> " != " <> exprToRust' c env 9 r)
exprToRust' c env p (Un Not l) =
    outParen (p > 14) ("!" <> exprToRust' c env 15 l)
exprToRust' c env p (If b t e) =
    outParen (p > 2)
        ("if " <> exprToRust' c env 2 b <> " { " <>
            exprToRust' c env 3 t <>
        " } else { " <>
            exprToRust' c env 3 e <>
        " }")
exprToRust' c env _ (Call "ref" [e]) = "&(" <> exprToRust c env e <> ")"
exprToRust' c env _ (Call "ref_mut" [e]) = "&mut (" <> exprToRust c env e <> ")"
exprToRust' c env _ (Call t es) =
    Builder.byteString t <> "(" <> mconcat (intersperse ", " $ map (exprToRust' c env 0) es) <> ")"
exprToRust' c env p (Bin At l r) =
    outParen (p > 17) (exprToRust' c env 17 l <> "[" <> exprToRust' c env 0 r <> "]")
exprToRust' c env p (Annotate e t) = -- TODO: Is this what I want?
    outParen (p > 0) (exprToRust' c env 14 e <> " as " <> typeToRust c t)
exprToRust' c env _ (Ref r) = refToRust c env r

paramList :: Context -> [(T, Ty T T)] -> Out
paramList c = mconcat . map (\(x, ty) -> ", a_" <> Builder.byteString x <> ": " <> typeToRust c ty)

argList :: [Out] -> Out
argList = foldMap ((", "<>))

mut :: Context -> Out
mut c | mutableFields c = " mut"
      | otherwise = ""

-- left and right will be the interval *actually* consumed by the previous term if
-- it is a consuming term, otherwise it will be unchanged from earlier terms.
-- For Array, currently, we treat the "previous term" as the last iteration.
termToRust :: Out -> Context -> Env -> Term T T T Expr -> Out
termToRust indent c env z@(NonTerminal nt args l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp} as usize;\n|]
   <> indent <> [i|right = #{rExp} as usize;\n|]
   <> indent <>   "if right < left || right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|let nt_#{u nt}_m = #{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|let (mut nt_#{u nt}_ipg_start, mut nt_#{u nt}_ipg_end, nt_#{u nt}) = match nt_#{u nt}_m {\n|]
   <> indent <> [i|  None => { break '_ipg_alt; }\n|]
   <> indent <> [i|  Some(p) => p,\n|]
   <> indent <> [i|};\n|]
   <> indent <> [i|if nt_#{u nt}_ipg_end != 0 {\n|]
   <> indent <> [i|  self_ipg_start = self_ipg_start.min(left + nt_#{u nt}_ipg_start);\n|]
   <> indent <> [i|  self_ipg_end = self_ipg_end.max(left + nt_#{u nt}_ipg_end);\n|]
   <> indent <>   "}\n"
   <> indent <> [i|nt_#{u nt}_ipg_end += left;\n|]
   <> indent <> [i|nt_#{u nt}_ipg_start += left;\n|]
   <> indent <> [i|left = nt_#{u nt}_ipg_start;\n|]
   <> indent <> [i|right = nt_#{u nt}_ipg_end;\n\n|]
  where lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r
        es = map (exprToRust c env) args
termToRust indent c env z@(Terminal "" l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp} as usize;\n|]
   <> indent <> [i|right = #{rExp} as usize;\n|]
   <> indent <>   "if right < left || right > EOI { break '_ipg_alt; }\n\n"
  where lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r
termToRust indent c env z@(Terminal t l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp} as usize;\n|]
   <> indent <> [i|right = #{rExp} as usize;\n|]
   <> indent <>   "if right < left || right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|if !&input[begin + left .. begin + right].starts_with(&[#{terminal}]) { break '_ipg_alt; }\n|]
   <> indent <>   "self_ipg_start = self_ipg_start.min(left);\n"
   <> indent <> [i|right = left + #{BS.length t};\n|]
   <> indent <>   "self_ipg_end = self_ipg_end.max(right);\n\n"
  where lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r; terminal = explode t
termToRust indent c env z@(x := e)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|let#{mut c} self_#{x} = #{eExp};\n\n|]
  where eExp = exprToRust c env e
termToRust indent c env z@(Guard e)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|if !#{eExp} { break '_ipg_alt; }\n\n|]
  where eExp = exprToRust' c env 15 e
termToRust indent c env z@(Array x start end nt args l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|let mut nt_#{u nt}_ipg_start = left;\n|] -- Special case
   <> indent <> [i|let mut nt_#{u nt}_ipg_end = right;\n|] -- Special case
   <> indent <> [i|let seq_#{u nt}_start = #{startExp} as usize;\n|]
   <> indent <> [i|let loopEnd = #{endExp} as usize;\n|]
   <> indent <> [i|let mut seq_#{u nt} = Vec::with_capacity(loopEnd.saturating_sub(seq_#{u nt}_start));\n|]
   <> indent <> [i|for i_#{x} in seq_#{u nt}_start..loopEnd {\n|]
   <> indent <> [i|  let left = #{lExp} as usize;\n|]
   <> indent <> [i|  let right = #{rExp} as usize;\n|]
   <> indent <>   "  if right < left || right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|  let tmp_m = #{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|  let (mut tmp_ipg_start, mut tmp_ipg_end, tmp) = match tmp_m {\n|]
   <> indent <> [i|    None => { break '_ipg_alt; }\n|]
   <> indent <> [i|    Some(p) => p,\n|]
   <> indent <> [i|  };\n|]
   <> indent <>   "  if tmp_ipg_end != 0 {\n"
   <> indent <>   "    self_ipg_start = self_ipg_start.min(left + tmp_ipg_start);\n"
   <> indent <>   "    self_ipg_end = self_ipg_end.max(left + tmp_ipg_end);\n"
   <> indent <>   "  }\n"
   <> indent <>   "  tmp_ipg_end += left;\n"
   <> indent <>   "  tmp_ipg_start += left;\n"
   <> indent <> [i|  nt_#{u nt}_ipg_end = tmp_ipg_end;\n|] -- Special case
   <> indent <> [i|  nt_#{u nt}_ipg_start = tmp_ipg_start;\n|] -- Special case
   <> indent <> [i|  seq_#{u nt}.push(tmp);\n|]
   <> indent <>   "}\n"
   <> indent <> [i|left = nt_#{u nt}_ipg_start;\n|]
   <> indent <> [i|right = nt_#{u nt}_ipg_end;\n\n|]
  where startExp = exprToRust' c env 14 start; endExp = exprToRust' c env 14 end
        lExp = exprToRust' c' env 14 l; rExp = exprToRust' c' env 14 r
        es = map (exprToRust c' env) args
        c' = c { iterationVar = x }
termToRust indent c env z@(Any x l)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp} as usize;\n|]
   <> indent <>   "right = left + 1;\n"
   <> indent <>   "if right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|let#{mut c} self_#{x} = input[begin + left];\n|]
   <> indent <>   "self_ipg_start = self_ipg_start.min(left);\n"
   <> indent <>   "self_ipg_end = self_ipg_end.max(right);\n\n"
  where lExp = exprToRust' c env 14 l
termToRust indent c env z@(Slice x l r)
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{lExp} as usize;\n|]
   <> indent <> [i|right = #{rExp} as usize;\n|]
   <> indent <>   "if right < left || right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|let#{mut c} self_#{x} = (&input[begin + left .. begin + right]).to_vec();\n|]
   <> indent <>   "if left != right {\n"
   <> indent <>   "  self_ipg_start = self_ipg_start.min(left);\n"
   <> indent <>   "  self_ipg_end = self_ipg_end.max(right);\n"
   <> indent <>   "}\n\n"
  where lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r
termToRust indent c env z@(Repeat nt args l r x l0 r0) -- TODO: Need to expand the scope of nt_#{u nt}*
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <>   "let mut self_values = Vec::new();\n"
   <> indent <> [i|left = #{l0Exp} as usize;\n|]
   <> indent <> [i|right = #{r0Exp} as usize;\n|]
   <> indent <> [i|let nt_#{u nt}_m = #{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|match nt_#{u nt}_m {\n|]
   <> indent <> [i|  None => {}\n|]
   <> indent <> [i|  Some((mut nt_#{u nt}_ipg_start, mut nt_#{u nt}_ipg_end, nt_#{u nt})) => {\n|]
   <> indent <> [i|    if nt_#{u nt}_ipg_end == 0 { panic!("repeat of non-consuming rule: #{fst nt}"); }\n|]
   <> indent <> [i|    self_ipg_start = self_ipg_start.min(left + nt_#{u nt}_ipg_start);\n|]
   <> indent <> [i|    self_ipg_end = self_ipg_end.max(left + nt_#{u nt}_ipg_end);\n|]
   <> indent <> [i|    nt_#{u nt}_ipg_end += left;\n|]
   <> indent <> [i|    nt_#{u nt}_ipg_start += left;\n|]
   <> indent <> [i|    left = #{lExp} as usize;\n|]
   <> indent <> [i|    right = #{rExp} as usize;\n|]
   <> indent <> [i|    self_values.push(#{xAttr});\n\n|]

   <> indent <>   "    while left <= right && right <= EOI {\n"
   <> indent <> [i|      let nt_#{u nt}_m = #{fst nt}(input, begin + left, begin + right#{argList es});\n|]
   <> indent <> [i|      let (mut nt_#{u nt}_ipg_start, mut nt_#{u nt}_ipg_end, nt_#{u nt}) = match nt_#{u nt}_m {\n|]
   <> indent <> [i|        None => { break; }\n|]
   <> indent <> [i|        Some(p) => p,\n|]
   <> indent <> [i|      };\n|]
   <> indent <> [i|      if nt_#{u nt}_ipg_end == 0 { panic!("repeat of non-consuming rule: #{fst nt}"); }\n|]
   <> indent <> [i|      self_ipg_start = self_ipg_start.min(left + nt_#{u nt}_ipg_start);\n|]
   <> indent <> [i|      self_ipg_end = self_ipg_end.max(left + nt_#{u nt}_ipg_end);\n|]
   <> indent <> [i|      nt_#{u nt}_ipg_end += left;\n|]
   <> indent <> [i|      nt_#{u nt}_ipg_start += left;\n|]
   <> indent <> [i|      self_values.push(#{xAttr});\n|]
   <> indent <> [i|      left = #{lExp} as usize;\n|]
   <> indent <> [i|      right = #{rExp} as usize;\n|]
   <> indent <>   "    }\n"
   <> indent <>   "  }\n"
   <> indent <>   "};\n\n"
  where es = map (exprToRust c env) args
        lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r
        l0Exp = exprToRust' c env 14 l0; r0Exp = exprToRust' c env 14 r0
        xAttr = refToRust c env (Attr nt x)
termToRust indent c env z@(RepeatUntil nt1 args1 l r x l0 r0 nt2 args2) -- TODO: Need to expand the scope of nt_#{u nt1}*/nt_#{u nt2}*
    = indent <> [i|// #{pprintTerm z}\n|]
   <> indent <> [i|left = #{l0Exp} as usize;\n|]
   <> indent <> [i|right = #{r0Exp} as usize;\n|]
   <> indent <>   "let mut self_values = Vec::new();\n"
   <> indent <>   "loop {\n"
   <> indent <>   "  if right < left || right > EOI { break '_ipg_alt; }\n"
   <> indent <> [i|  let nt_#{u nt2}_m = #{fst nt2}(input, begin + left, begin + right#{argList es2});\n|]
   <> indent <> [i|  match nt_#{u nt2}_m {\n|]
   <> indent <> [i|    None => {}\n|]
   <> indent <> [i|    Some((mut nt_#{u nt2}_ipg_start, mut nt_#{u nt2}_ipg_end, nt_#{u nt2})) => {\n|]
   <> indent <> [i|      if nt_#{u nt2}_ipg_end != 0 {\n|]
   <> indent <> [i|        self_ipg_start = self_ipg_start.min(left + nt_#{u nt2}_ipg_start);\n|]
   <> indent <> [i|        self_ipg_end = self_ipg_end.max(left + nt_#{u nt2}_ipg_end);\n|]
   <> indent <>   "      }\n"
   <> indent <> [i|      nt_#{u nt2}_ipg_end += left;\n|]
   <> indent <> [i|      nt_#{u nt2}_ipg_start += left;\n|]
   <> indent <> [i|      right = nt_#{u nt2}_ipg_end;\n|]
   <> indent <>   "      break;\n"
   <> indent <>   "    }\n"
   <> indent <>   "  };\n"
   <> indent <> [i|  let nt_#{u nt1}_m = #{fst nt1}(input, begin + left, begin + right#{argList es1});\n|]
   <> indent <> [i|  let (mut nt_#{u nt1}_ipg_start, mut nt_#{u nt1}_ipg_end, nt_#{u nt1}) = match nt_#{u nt1}_m {\n|]
   <> indent <> [i|    None => { break '_ipg_alt; }\n|]
   <> indent <> [i|    Some(p) => p,\n|]
   <> indent <> [i|  };\n|]
   <> indent <> [i|  if nt_#{u nt1}_ipg_end == 0 { panic!("repeat of non-consuming rule: #{fst nt1}"); }\n|]
   <> indent <> [i|  self_ipg_start = self_ipg_start.min(left + nt_#{u nt1}_ipg_start);\n|]
   <> indent <> [i|  self_ipg_end = self_ipg_end.max(left + nt_#{u nt1}_ipg_end);\n|]
   <> indent <> [i|  nt_#{u nt1}_ipg_end += left;\n|]
   <> indent <> [i|  nt_#{u nt1}_ipg_start += left;\n|]
   <> indent <> [i|  self_values.push(#{xAttr});\n|]
   <> indent <> [i|  left = #{lExp} as usize;\n|]
   <> indent <> [i|  right = #{rExp} as usize;\n|]
   <> indent <>   "}\n\n"
  where es1 = map (exprToRust c env) args1; es2 = map (exprToRust c env) args2
        lExp = exprToRust' c env 14 l; rExp = exprToRust' c env 14 r
        l0Exp = exprToRust' c env 14 l0; r0Exp = exprToRust' c env 14 r0
        xAttr = refToRust c env (Attr nt1 x)

alternativeToRust :: Out -> Context -> Env -> Alternative T T T Expr -> Out
alternativeToRust indent c env (Alternative ts)
    = indent <>   "'_ipg_alt: {\n"
   <> indent <>   "  let mut left: usize = EOI; let mut right: usize = 0;\n"
   <> indent <>   "  let mut self_ipg_start: usize = EOI; let mut self_ipg_end: usize = 0;\n\n"
   <>                foldMap (termToRust ("  " <> indent) c env) ts
   <> indent <> [i|  return Some((self_ipg_start, self_ipg_end, #{structName} {\n|]
   <>                  foldMap setField fields
   <> indent <>   "  }));\n"
   <> indent <>   "}\n"
  where (structName, fields) = ruleRows c Map.! currentRule c
        setField f = indent <> [i|    #{f :: T}: self_#{f},\n|]

typeDeclToRust :: Context -> (T, [T], Ty T T) -> Out
typeDeclToRust _ ("Ref", [_], _) = ""
typeDeclToRust _ ("RefMut", [_], _) = ""
typeDeclToRust c (t, [], RowTy fs) = struct c t fs <> "\n"
typeDeclToRust c (t, xs, RowTy fs) = struct c [i|#{t}<#{xs'}>|] fs <> "\n"
    where xs' = mconcat (intersperse ", " (map (Builder.byteString . BS.drop 1) xs))
typeDeclToRust c (t, [], ty) = [i|type #{t} = #{typeToRust c ty};\n\n|]
typeDeclToRust c (t, xs, ty) = [i|type #{t}<#{xs'}> = #{typeToRust c ty};\n\n|]
    where xs' = mconcat (intersperse ", " (map (Builder.byteString . BS.drop 1) xs))

constToRust :: Context -> (T, Maybe (Ty T T), Expr) -> Out
constToRust c (n, Just ty, e) = [i|let #{n}: #{typeToRust c ty} = #{exprToRust c Set.empty e};\n|]
constToRust c (n, Nothing, e) = [i|let #{n} = #{exprToRust c Set.empty e};\n|]

typeToRust :: Context -> Ty T T -> Out
typeToRust _ BoolTy = "bool"
typeToRust _ U8Ty = "u8"
typeToRust _ IntTy = "i64"
typeToRust _ FloatTy = "f64"
typeToRust _ StringTy = "Vec<u8>" -- "String" -- TODO: Change based on whether this is a parameter or not?
typeToRust _ (RowTy fs) = error [i|Row types need to be named...\n#{show fs}|] -- TODO
typeToRust c (ArrayTy ty) = "Vec<" <> typeToRust c ty <> ">" -- TODO
typeToRust c (TyApp "Ref" [ty]) = "&" <> typeToRust c ty
typeToRust c (TyApp "RefMut" [ty]) = "&mut " <> typeToRust c ty
typeToRust _ (TyApp t []) = Builder.byteString t
typeToRust c (TyApp t tys) = -- TODO
    Builder.byteString t <> "<" <> mconcat (intersperse ", " $ map (typeToRust c) tys) <> ">"
typeToRust _ (ExternalTy t) = Builder.byteString t
typeToRust _ (TyVar v) = Builder.byteString (BS.drop 1 v)
typeToRust _ (Note n (RowTy _)) = Builder.byteString n -- TODO: This doesn't handle rule return types that have type variables.
typeToRust c (Note _ ty) = typeToRust c ty

ruleToRust :: Context -> Rule T T T Expr -> Out
ruleToRust c (Rule mt nt args alts) =
    [__i|
      #{export}fn #{nt}(input: &[u8], begin: usize, end: usize#{paramList c' tyArgs}) -> Option<(usize, usize, #{rt})> {
        let EOI: usize = end - begin;
      #{foldMap (alternativeToRust "  " c' env) alts}
        return None;
      }\n\n
    |]
  where env = Set.fromList args
        export = if EXPORT `elem` mt then "pub " else "" :: T
        (argTys, _) = ruleTypes c Map.! nt
        (rt, _) = ruleRows c Map.! nt
        tyArgs = zip args argTys
        c' = c { currentRule = nt }

-- Turn rules with explicit row type results into structs with those rows as fields.
ruleDeclToRust :: Context -> (T, [(T, Ty T T)], Maybe (Ty T T)) -> Out
ruleDeclToRust c (nt, _, ty) = toStruct ty
    where toStruct (Just (RowTy fs)) = struct c nt fs <> "\n"
          toStruct (Just _) = ""
          toStruct Nothing = error "Shouldn't happen if the code is annotated first"

struct :: Context -> T -> Map.Map T (Ty T T) -> Out
struct c t fs = [i|\#[derive(Debug)]\nstruct #{t} {\n#{foldMap toField (Map.toList fs)}}\n|]
  where toField :: (T, Ty T T) -> Out
        toField (fieldName, ty') = [i|  #{fieldName}: #{typeToRust c ty'},\n|]

toRustWithContext :: Context -> Grammar T T T T Expr -> Either Out LBS.ByteString
toRustWithContext c (Grammar decls) =
    let core = Grammar (foldMap rewrite decls)
    in case TC.typeCheck ctxt core of
        Right envs ->
            let g = TC.annotate envs core
            in Right (toRustWithContext' c envs g)
        Left err -> Left (err <> if dumpCore c then "\n\n" <> pprint core else "")
  where ctxt =
          TC.Context {
              TC.currentRule = "",
              TC.values = "values",
              TC.out = Builder.byteString,
              TC.tOut = Builder.byteString,
              TC.ntOut = Builder.byteString
          }
        -- rewrite (RuleDeclaration nt _ Nothing) =
        --     error [i|Rule declaration for #{nt :: T} without return type|]
        rewrite (RuleDeclaration nt args (Just ty@(RowTy _))) =
            [TypeDeclaration nt [] ty, RuleDeclaration nt args (Just (TyApp nt []))]
        rewrite d = [d]

toRustWithContext' :: Context -> TC.Environments T T T -> Grammar T T T T Expr -> LBS.ByteString
toRustWithContext' c envs (Grammar decls) = Builder.toLazyByteString $
    foldMap (typeDeclToRust c') typeDecls
    <> foldMap (constToRust c') constDecls
    <> foldMap (ruleDeclToRust c') ruleDecls -- If the return type is a row type, make a struct with the name of the rule with some prefix.
    <> foldMap (ruleToRust c') ruleDefs
  where c' = c {
                constants = Set.fromList (map  (\(n, _, _) -> n) constDecls),
                ruleTypes = TC.ruleTypes envs,
                typeDefs = TC.typeDefs envs,
                ruleRows =
                    Map.fromList
                        (map (\(nt, (_, ty)) -> getRows' nt ty) (Map.toList (TC.ruleTypes envs)))
                -- funArgRefs =
             }
        (ruleDefs, constDecls, typeDecls, ruleDecls, _funDecls) = partitionDeclarations decls
        getRows' nt ty =
            case TC.getRows (TC.typeDefs envs) ty of
                Just rows -> (nt, (n, Map.keys rows))
                Nothing -> error [i|Rule #{nt :: T} has a non-row-type result|]
                -- TODO: Allow this by letting a user specify what the rows are.
          where n = case ty of TyApp t _ -> t; _ -> nt

toRust :: Grammar T T T T Expr -> Either Out LBS.ByteString
toRust = toRustWithContext defaultContext
