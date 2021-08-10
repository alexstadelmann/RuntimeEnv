module ML
(
  evaluate,
  numAt
)
  where


-- import Debug.Trace

import Declarations
import Translator


evaluate :: Storage -> Storage
evaluate stor@(_, cod, _, reg, _, _) =
  case cod !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- use this version of evaluate for debugging purposes:

-- evaluate :: Storage -> Storage
-- evaluate stor@(st, cod, _, reg, _, _)
--   | trace ((show st) ++ "   " ++ (show reg) ++ "   "
--     ++ (show $ cod !! (p reg)) ++ "\n") False = undefined
--   | otherwise =
--     case cod !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


execute :: Command -> Storage -> Storage
execute (Push arg) = push arg
execute (Unify arg) = unify arg
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: Arg -> Storage -> Storage
push CHP (st, cod, env, reg, tr, us) =
  let st' = NUM (l reg)
             : NUM retAdd
             : NUM (c reg)
             : NUM (cFirst env)
             : st
      retAdd = getRetAdd cod $ p reg + 3
      reg' = reg {c = length st,
                  r = length st + 1,
                  p = p reg + 1,
                  up = length st + 4}
  in (st', cod, env, reg', tr, us) where

  getRetAdd :: Code -> Int -> Int
  getRetAdd cod i =
    case cod !! i of
         Return -> i
         Prompt -> i
         Push CHP -> i
         _ -> getRetAdd cod $ i + 1

push (STR' s i) (st, cod, env, reg, tr, us) =
  let st' = STR s i : st
      reg' = reg {p = p reg + 1}
  in (st', cod, env, reg', tr, us)


unify :: Arg -> Storage -> Storage
unify a (st, cod, env, reg, tr, us)
  | not $ b reg =
    let reg' = reg {p = p reg + 1,
                    l = numAt st (c reg + 3) + 1,
                    up = up reg + 1}
    in (st, cod, env, reg', tr, us)
  | otherwise =
    let reg' = reg {p = p reg + 1}
    in (st, cod, env, reg', tr, us)


call :: Storage -> Storage
call stor@(st, cod, env, reg, tr, us)
  | numAt st (c reg) < 0 =
    let reg' = reg {b = True,
                    p = p reg + 1}
    in (st, cod, env, reg', tr, us)
  | otherwise =
    let st' = setCNext stor
        reg' = reg {p = numAt st $ c reg}
    in (st', cod, env, reg', tr, us)


returnL :: Storage -> Storage
returnL (st, cod, env, reg, tr, us)
  | numAt st (r reg + 2) /= l reg - 1 =
    let reg' = reg {r = numAt st (r reg) + 1}
    in returnL (st, cod, env, reg', tr, us)
  | otherwise =
    let reg' = reg {p = numAt st $ r reg + 1,
                    l = l reg - 1}
    in (st, cod, env, reg', tr, us)


backtrack :: Storage -> Storage
backtrack stor@(st, cod, env, reg, tr, us)
  | b reg =
    case (numAt st $ c reg, numAt st $ r reg) of
         (-1, -1) -> let reg' = reg {p = cLast env}
                     in (st, cod, env, reg', tr, us)
         (-1, _) -> let newC = numAt st $ r reg
                        reg' = reg {c = newC,
                                    r = newC + 1}
                        st' = drop (length st - c reg) st
                    in backtrack (st', cod, env, reg', tr, us)
         _ -> let st' = setCNext stor
                  reg' = reg {p = numAt st $ c reg,
                              b = False,
                              up = c reg + 4}
              in (st', cod, env, reg', tr, us)
  | otherwise = let reg' = reg {p = p reg + 1}
                in (st, cod, env, reg', tr, us)


elemAt :: Stack -> Int -> StackElem
elemAt st i = st !! (length st - i - 1)


numAt :: Stack -> Int -> Int
numAt st i =
  case elemAt st i of
       NUM n -> n
       _ -> error "expected NUM constructor"


setCNext :: Storage -> Stack
setCNext (st, _, env, reg, _, _) =
  let cNew = NUM $ cNext env $ numAt st $ c reg
      pos = length st - (c reg) - 1
  in take pos st ++ cNew : drop (pos + 1) st
