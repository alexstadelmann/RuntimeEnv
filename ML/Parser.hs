{- |
Module : Parser

The parser takes L5 tokens as input and creates an L5 syntax tree out of these.
-}
module Parser
(
  parse
)
  where


import qualified Data.Set as Set
  
import Declarations


-- | Creates a syntax tree out of tokens created by the tokenizer.
parse :: [(Symbol, Int)] -- ^ List of tokens and their corresponding line in the source code.
  -> SyntaxTree -- The syntax tree created by the Parser.
parse s =
  case program s [] of
       ([], p) -> p
       ([(NewLine, _)], p) -> p
       ((sym, line) : _, _) ->
         error $ "Parse error in line " ++ show line
         ++ ".\nParsing of program already finished, but found: " ++ show sym


-- | Parses a complete L5 program if possible.
program :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> [PClause] -- ^ Program clauses already parsed.
  -> ([(Symbol, Int)], SyntaxTree) -- ^ Remaining tokens, syntax tree as actual result.
program s@((Name _, _ ) : _) pcs =
  let result = pclause s
  in program (fst result) $ (snd result) : pcs

program s@((If , _) : _) pcs =
  let (rest, g) = goal s
      pcs' = reverse pcs
  in (rest, SyntaxTree (zip (map varseqpc pcs') pcs') (varseqgoal g, g))

program ((sym, line) : _) _ =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected start of program clause or goal, but found: " ++ show sym


-- | Parses a program clause if possible.
pclause :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> ([(Symbol, Int)], PClause) -- ^ Remaining tokens, program clause as actual result.
pclause s@((Name _, _) : _) = uncurry pclause' $ nvlterm s where

  pclause' :: [(Symbol, Int)] -> NVLTerm -> ([(Symbol, Int)], PClause)
  pclause' ((Point, _) : t) nvlt = pclause'' t $ PClause nvlt []
  pclause' s@((If, _) : _) nvlt =
    let g = goal s
    in pclause'' (fst g) $ PClause nvlt $ snd g
  pclause' ((sym, line) : _) _ =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected end of program clause or start of goal, but found: " ++ show sym

  -- Every clause has to be in a new line:
  pclause'' :: [(Symbol, Int)] -> PClause -> ([(Symbol, Int)], PClause)
  pclause'' ((NewLine, _) : t) pc = (t, pc) 
  pclause'' ((sym, line) : _) _ = 
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected new line, but found: " ++ show sym

pclause ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected start of program clause, but found: " ++ show sym


-- | Parses a goal if possible.
goal :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> ([(Symbol, Int)], Goal) -- ^ Remaining tokens, goal as actual result.
goal ((If, _) : t) = goal' t [] where

  goal' :: [(Symbol, Int)] -> [Literal] -> ([(Symbol, Int)], Goal)
  goal' s@((sym, line) : _) ls
    | fstOfLit sym =
      let l = literal s
      in goal'' (fst l) $ (snd l) : ls
    | otherwise =
      error $ "Parse error in line " ++ show line
      ++ ".\nExpected start of literal, but found: " ++ show sym

  goal'' :: [(Symbol, Int)] -> [Literal] -> ([(Symbol, Int)], Goal)
  goal'' ((And, _) : t) ls = goal' t ls
  goal'' ((Point, _) : t) ls = (t, reverse ls)
  goal'' ((sym, line) : _) _ =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected end of literal or goal, but found: " ++ show sym

goal ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected start of goal, but found: " ++ show sym


-- | Parses a literal if possible.
literal :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> ([(Symbol, Int)], Literal) -- ^ Remaining tokens, literal as actual result.
literal ((Not, _) : t) =
  let lt = lterm t
  in (fst lt, Literal True $ snd lt)

literal s@((sym,line):_)
  | fstOfLTerm sym =
    let lt = lterm s
    in (fst lt, Literal False $ snd lt)
  | otherwise =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected \"not\" or name or variable, but found: " ++ show sym


-- | Parses an atom if possible.
nvlterm :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> ([(Symbol, Int)], NVLTerm) -- ^ Remaining tokens, atom as actual result.
nvlterm ((Name n, _) : t) = nvlterm' t n where

  nvlterm' :: [(Symbol, Int)] -> String -> ([(Symbol, Int)], NVLTerm)
  nvlterm' ((LBracket, _) : t) n = nvlterm'' t n []
  nvlterm' s n = (s, NVLTerm n [])              
  -- Eventueller Fehler wird von pclause oder lterm abgefangen

  nvlterm'' :: [(Symbol, Int)] -> String -> [LTerm] -> ([(Symbol, Int)], NVLTerm)
  nvlterm'' s@((sym, line) : _) n lts
    | fstOfLTerm sym =
      let l = lterm s
      in nvlterm''' (fst l) n $ (snd l):lts
    | otherwise =
      error $ "Parse error in line " ++ show line
      ++ ".\nExpected name or variable, but found:" ++ show sym

  nvlterm''' :: [(Symbol, Int)] -> String -> [LTerm] -> ([(Symbol, Int)], NVLTerm)
  nvlterm''' ((And,_):t) n lts = nvlterm'' t n lts
  nvlterm''' ((RBracket, _) : t) n lts = (t, NVLTerm n $ reverse lts)
  nvlterm''' ((sym,line):_) _ _ =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected end of L-Term, but found: " ++ show sym

nvlterm ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected name, but found: " ++ show sym


-- | Parses a term if possible.
lterm :: [(Symbol, Int)] -- ^ List of remaining tokens and their lines in the source code.
  -> ([(Symbol, Int)], LTerm) -- ^ Remaining tokens, term as actual result.
lterm ((Variable v, _) : t) = (t, Var v)

lterm s@((Name _, _) : _) =
  let nt = nvlterm s
  in (fst nt, NVar $ snd nt)

lterm ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected name or variable, but found: " ++ show sym


-- | Checks whether a symbol may be the first symbol of a term.
fstOfLTerm :: Symbol -- ^ The symbol.
  -> Bool -- ^ True, if symbol may be the first symbol of a term.
fstOfLTerm (Name _) = True
fstOfLTerm (Variable _) = True
fstOfLTerm _ = False


-- | Checks whether a symbol may be the first symbol of a literal.
fstOfLit :: Symbol -- ^ The symbol.
  -> Bool -- ^ True, if symbol may be the first symbol of a literal.
fstOfLit Not = True
fstOfLit s = fstOfLTerm s


-- | Creates the variable sequence of a program clause.
varseqpc :: PClause -- ^ The program clause.
  -> VarSeq -- ^ The variable sequence.
varseqpc (PClause nvlt g) = Set.union (varseqnvlt nvlt) $ varseqgoal g


-- | Creates the variable sequence of an atom.
varseqnvlt :: NVLTerm -- ^ The atom.
  -> VarSeq -- ^ The variable sequence.
varseqnvlt (NVLTerm _ lts) = Set.unions $ map varseqlt lts


-- | Creates the variable sequence of a term.
varseqlt :: LTerm -- ^ The term.
  -> VarSeq -- ^ The variable sequence.
varseqlt (Var str) = Set.singleton str
varseqlt (NVar nvlt) = varseqnvlt nvlt


-- | Creates the variable sequence of a goal.
varseqgoal :: Goal -- ^ The goal.
  -> VarSeq -- ^ The variable sequence.
varseqgoal lits = Set.unions $ map varseqlit lits


-- | Creates the variable sequence of a literal.
varseqlit :: Literal -- ^ The literal.
  -> VarSeq -- ^ The variable sequence.
varseqlit (Literal _ lt) = varseqlt lt
