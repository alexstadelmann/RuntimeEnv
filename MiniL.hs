module MiniL
(
  evaluate
)
  where

import Declarations
import MiniTranslator


evaluate :: Storage -> Storage
evaluate s@(_, pcode, _, reg) =
  case pcode !! (p reg) of
       Prompt -> s
       c -> evaluate $ execute c s


execute :: Command -> Storage -> Storage
execute (Push (Atom a)) = push $ Atom a
execute (Unify (Atom a)) = unify $ Atom a
execute Call = call
execute Return = returnL
execute Backtrack = backtrackQ


push :: StackElem -> Storage -> Storage
push a (xs, ys, env, reg) =
  let xs' = xs ++ [Zahl 0, Zahl $ c reg, Zahl $ (p reg) + 3, a]
      reg' = reg {c = (t reg) + 1,
                  r = (t reg) + 2,
                  t = (t reg) + 4,
                  p = (p reg) + 1}
  in (xs', ys, env, reg')


unify :: StackElem -> Storage -> Storage
unify a (xs, ys, env, reg) =
  let reg' = reg {b = a /= xs !! ((c reg) + 3),
                  p = (p reg) + 1}
  in (xs, ys, env, reg')


call :: Storage -> Storage
call (xs, ys, env, reg) =
  case getNumAt xs (c reg) of
       (-1) -> let reg' = reg {b = True,
                               p = (p reg) + 1}
               in (xs, ys, env, reg')
       _ -> let xs' = replace (Zahl $ c_next env $ getNumAt xs $ c reg) xs $ c reg
                reg' = reg {p = getNumAt xs $ c reg}
            in (xs', ys, env, reg')


returnL :: Storage -> Storage
returnL (xs, ys, env, reg) =
  let tmp1 = getNumAt xs $ r reg
      tmp2 = getNumAt xs $ (r reg) + 1
      reg' = if tmp1 /= -1
                then reg {r = tmp1 + 1,
                          p = tmp2}
                else reg {p = tmp2}
  in (xs, ys, env, reg')


backtrackQ :: Storage -> Storage
backtrackQ (xs, ys, env, reg) =
  case b reg of
       False -> let reg' = reg {p = (p reg) + 1}
                in (xs, ys, env, reg')
       _ -> case (getNumAt xs $ c reg, getNumAt xs $ r reg) of
                 (-1, -1) -> let reg' = reg {p = c_last env}
                             in (xs, ys, env, reg')
                 (-1, _) -> let tmp = getNumAt xs $ r reg
                                reg' = reg {c = tmp,
                                            r = tmp + 1,
                                            t = tmp + 3}
                                xs' = take (tmp + 4) xs
                            in backtrackQ (xs', ys, env, reg')
                 _ -> let xs' = replace (Zahl $ c_next env $ getNumAt xs $ c reg) xs $ c reg
                          reg' = reg {p = getNumAt xs $ c reg,
                                      b = False}
                      in (xs', ys, env, reg')


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



-- test :: SyntaxTree -> Storage
-- test x =
--   let pcode = translate x
--       stack = []
--       result = []
--   in let env = createEnv pcode
--      in let reg = Register{i = 0,
--                            b = False,
--                            t = -1,
--                            c = -1,
--                            r = -1,
--                            p = c_goal env}
--         in (stack, pcode, env, reg, result)
