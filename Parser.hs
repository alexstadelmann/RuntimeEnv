module Parser
(
  parse
)
  where

import Declarations


parse :: [Symbol] -> SyntaxTree
parse s = case program s [] of
               ([], p) -> p
               _ -> error "Parsing went wrong."


program :: [Symbol] -> [PClause] -> ([Symbol], SyntaxTree)
program s@((Name _):_) pcs =
  let result = pclause s
  in program (fst result) $ (snd result):pcs

program s@(If:_) pcs =
  let g = goal s
  in (fst g, SyntaxTree (reverse pcs) $ snd g)

program _ _ = error "Parsing went wrong"


pclause :: [Symbol] -> ([Symbol], PClause)
pclause s@((Name _):_) =
  let nvlt = nvlterm s
  in uncurry pclause' nvlt where

  pclause' :: [Symbol] -> NVLTerm -> ([Symbol], PClause)
  pclause' (Point:t) nvlt = (t, PClause nvlt Nothing)
  pclause' s@(If:_) nvlt =
    let g = goal s
    in (fst g, PClause nvlt $ Just $ snd g)
  pclause' _ _ = error "Parsing went wrong."

pclause _ = error "Parsing went wrong."


goal :: [Symbol] -> ([Symbol], Goal)
goal (If:t) = goal' t [] where

  goal' :: [Symbol] -> [Literal] -> ([Symbol], Goal)
  goal' s ls =
    let l = literal s
    in goal'' (fst l) $ (snd l):ls

  goal'' :: [Symbol] -> [Literal] -> ([Symbol], Goal)
  goal'' (And:t) ls = goal' t ls
  goal'' (Point:t) ls = (t, Goal $ reverse ls)
  goal'' _ _ = error "Parsing went wrong."

goal _ = error "Parsing went wrong."


literal :: [Symbol] -> ([Symbol], Literal)
literal (Not:t) =
  let lt = lterm t
  in (fst lt, Literal True $ snd lt)

literal s =
  let lt = lterm s
  in (fst lt, Literal False $ snd lt)


nvlterm :: [Symbol] -> ([Symbol], NVLTerm)
nvlterm ((Name n):t) = nvlterm' t n where

  nvlterm' :: [Symbol] -> String -> ([Symbol], NVLTerm)
  nvlterm' (LBracket:t) n = nvlterm'' t n []
  nvlterm' s n = (s, NVLTerm n [])

  nvlterm'' :: [Symbol] -> String -> [LTerm] -> ([Symbol], NVLTerm)
  nvlterm'' s n lts =
    let l = lterm s
    in nvlterm''' (fst l) n $ (snd l):lts

  nvlterm''' :: [Symbol] -> String -> [LTerm] -> ([Symbol], NVLTerm)
  nvlterm''' (And:t) n lts = nvlterm'' t n lts
  nvlterm''' (RBracket:t) n lts = (t, NVLTerm n $ reverse lts)
  nvlterm''' _ _ _ = error "Parsing went wrong"

nvlterm _ = error "Parsing went wrong"


lterm :: [Symbol] -> ([Symbol], LTerm)
lterm ((Variable v):t) = (t, Var v)

lterm s@((Name _):_) =
  let nt = nvlterm s
  in (fst nt, NVar $ snd nt)

lterm _ = error "Parsing went wrong"
