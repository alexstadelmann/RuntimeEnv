module Parser
(
  parse
)
  where


import qualified Data.Set as Set
  
import Declarations


parse :: [(Symbol, Int)] -> SyntaxTree
parse s =
  case program s [] of
       ([], p) -> p
       ([(NewLine, _)], p) -> p
       ((sym, line) : _, _) ->
         error $ "Parse error in line " ++ show line
         ++ ".\nParsing of program already finished, but found: " ++ show sym


program :: [(Symbol, Int)] -> [PClause] -> ([(Symbol, Int)], SyntaxTree)
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


pclause :: [(Symbol, Int)] -> ([(Symbol, Int)], PClause)
pclause s@((Name _, _) : _) = uncurry pclause' $ nvlterm s where

  pclause' :: [(Symbol, Int)] -> NVLTerm -> ([(Symbol, Int)], PClause)
  pclause' ((Point, _) : t) nvlt = pclause'' t $ PClause nvlt Nothing
  pclause' s@((If, _) : _) nvlt =
    let g = goal s
    in pclause'' (fst g) $ PClause nvlt $ Just $ snd g
  pclause' ((sym, line) : _) _ =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected end of program clause or start of goal, but found: " ++ show sym

  -- Jede Klausel muss in eigener, neuer Zeile stehen:
  pclause'' :: [(Symbol, Int)] -> PClause -> ([(Symbol, Int)], PClause)
  pclause'' ((NewLine, _) : t) pc = (t, pc) 
  pclause'' ((sym, line) : _) _ = 
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected new line, but found: " ++ show sym

pclause ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected start of program clause, but found: " ++ show sym


goal :: [(Symbol, Int)] -> ([(Symbol, Int)], Goal)
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
  goal'' ((Point, _) : t) ls = (t, Goal $ reverse ls)
  goal'' ((sym, line) : _) _ =
    error $ "Parse error in line " ++ show line
    ++ ".\nExpected end of literal or goal, but found: " ++ show sym

goal ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected start of goal, but found: " ++ show sym


literal :: [(Symbol, Int)] -> ([(Symbol, Int)], Literal)
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


nvlterm :: [(Symbol, Int)] -> ([(Symbol, Int)], NVLTerm)
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


lterm :: [(Symbol, Int)] -> ([(Symbol, Int)], LTerm)
lterm ((Variable v, _) : t) = (t, Var v)

lterm s@((Name _, _) : _) =
  let nt = nvlterm s
  in (fst nt, NVar $ snd nt)

lterm ((sym, line) : _) =
  error $ "Parse error in line " ++ show line
  ++ ".\nExpected name or variable, but found: " ++ show sym


fstOfLTerm :: Symbol -> Bool
fstOfLTerm (Name _) = True
fstOfLTerm (Variable _) = True
fstOfLTerm _ = False


fstOfLit :: Symbol -> Bool
fstOfLit Not = True
fstOfLit s = fstOfLTerm s


varseqpc :: PClause -> VarSeq
varseqpc (PClause nvlt Nothing) = varseqnvlt nvlt
varseqpc (PClause nvlt (Just g)) = Set.union (varseqgoal g) $ varseqnvlt nvlt


varseqnvlt :: NVLTerm -> VarSeq
varseqnvlt (NVLTerm _ lts) = Set.unions $ map varseqlt lts


varseqlt :: LTerm -> VarSeq
varseqlt (Var str) = Set.singleton str
varseqlt (NVar nvlt) = varseqnvlt nvlt


varseqgoal :: Goal -> VarSeq
varseqgoal (Goal lits) = Set.unions $ map varseqlit lits


varseqlit :: Literal -> VarSeq
varseqlit (Literal _ lt) = varseqlt lt
