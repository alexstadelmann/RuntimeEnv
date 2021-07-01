module MiniL (
  push,
  unify,
  call,
  returnL,
  backtrackQ,
  prompt,
  evaluate,
  miniL,
  -- test
) where

import Declarations
import Translator


-- replace Element at a given stack position
replace :: StackElem -> Stack -> Int -> Stack
replace a xs n
  | n < 0 || n >= length xs = error "index out of scope"
  | otherwise = (take n xs) ++ (a : drop (n+1) xs)


getNumAt :: Stack -> Int -> Int
getNumAt s i =
  case s !! i of
       Zahl a -> a
       _ -> error "expected a Zahl constructor, but got an Atom constructor"


push :: StackElem -> Storage -> Storage
push a (xs, ys, env, reg, result) =
  let xs' = xs ++ [Zahl 0, Zahl $ c reg, Zahl ((p reg) + 3), a]
      reg' = reg {c = (t reg) + 1, r = (t reg) + 2, t = (t reg) + 4, p = (p reg) + 1}
  in (xs', ys, env, reg', result)


unify :: StackElem -> Storage -> Storage
unify a (xs, ys, env, reg, result) =
  let reg' = reg {b = a /= (xs !! ((c reg) + 3)), p = (p reg) + 1}
  in (xs, ys, env, reg', result)


call :: Storage -> Storage
call (xs, ys, env, reg, result) =
  case getNumAt xs (c reg) of
       (-1) -> let reg' = reg {b = True, p = (p reg) + 1}
               in (xs, ys, env, reg', result)
       _ -> let xs' = replace (Zahl $ c_next env $ getNumAt xs $ c reg) xs $ c reg
                reg' = reg {p = getNumAt xs $ c reg}
            in (xs', ys, env, reg', result)


returnL :: Storage -> Storage
returnL (xs, ys, env, reg, result) =
  let a = getNumAt xs $ r reg
      b = getNumAt xs $ (r reg) + 1
  in if a /= -1
        then let reg' = reg {r = a + 1, p = b}
             in (xs, ys, env, reg', result)
        else let reg'' = reg {p = b}
             in (xs, ys, env, reg'', result)


backtrackQ :: Storage -> Storage
backtrackQ (xs, ys, env, reg, result) =
  case b reg of
       False -> let reg' = reg {p = (p reg) + 1}
                in (xs, ys, env, reg', result)
       _ -> case (getNumAt xs $ c reg, getNumAt xs $ r reg) of
                 (-1, -1) -> let reg' = reg {p = c_last env}
                             in (xs, ys, env, reg', result)
                 (-1, _) -> let a = getNumAt xs $ r reg
                            in let reg' = reg {c = a, r = a + 1, t = a + 3}
                                   xs' = take (a + 4) xs
                               in backtrackQ (xs', ys, env, reg', result)
                 _ -> let xs' = replace (Zahl $ c_next env $ getNumAt xs $ c reg) xs $ c reg
                          reg' = reg {p = getNumAt xs $ c reg, b = False}
                      in (xs', ys, env, reg', result)


prompt :: Storage -> Storage
prompt (xs, ys, env, reg, result) =
  case b reg of
       True -> let reg' = reg{p = -1}
               in (xs, ys, env, reg', result)
       _ -> let reg' = reg{b = True, p = (p reg) - 1}
                result' = xs:result
            in (xs, ys, env, reg', result')


evaluate :: Storage -> Maybe Result
evaluate all@(_, ys, _, reg, result) =
  case p reg of
       -1 -> Just result
       _ -> evaluate $ (execute $ ys !! (p reg)) all


execute :: Command -> Storage -> Storage
execute (Push (Atom a)) = push (Atom a)
execute (Unify (Atom a)) = unify (Atom a)
execute (Call) = call
execute (Return) = returnL
execute (Backtrack) = backtrackQ
execute (Prompt) = prompt


miniL :: Programm -> Maybe Result
miniL x =
  let pcode = translate x
      stack = []
      result = []
  in let env = createEnv pcode
     in let reg = Register{i = 0, b = False, t = -1, c = -1, r = -1, p = c_goal env}
        in evaluate (stack, pcode, env, reg, result)


test :: Programm -> Storage
test x =
  let pcode = translate x
      stack = []
      result = []
  in let env = createEnv pcode
     in let reg = Register{i = 0, b = False, t = -1, c = -1, r = -1, p = c_goal env}
        in (stack, pcode, env, reg, result)
