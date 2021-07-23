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
execute (Push (STR a)) = push $ STR a
execute (Unify (STR a)) = unify $ STR a
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: StackElem -> Storage -> Storage
push a (stack, pcode, env, reg) =
  let stack' = a : (Number $ (p reg) + 3)
                 : (Number $ c reg)
                 : (Number 0)
                 : stack
      reg' = reg {c = length stack,
                  r = (length stack) + 1,
                  p = (p reg) + 1}
  in (stack', pcode, env, reg')


unify :: StackElem -> Storage -> Storage
unify a (stack, pcode, env, reg) =
  let reg' = reg {b = a /= (elemAt stack $ (c reg) + 3),
                  p = (p reg) + 1}
  in (stack, pcode, env, reg')


call :: Storage -> Storage
call stor@(stack, pcode, env, reg)
  | numAt stack (c reg) < 0 =
    let reg' = reg {b = True,
                    p = (p reg) + 1}
    in (stack, pcode, env, reg')
  | otherwise =
    let stack' = setCNext stor
        reg' = reg {p = numAt stack $ c reg}
    in (stack', pcode, env, reg')


returnL :: Storage -> Storage
returnL (stack, pcode, env, reg) =
  let tmp1 = numAt stack $ r reg
      tmp2 = numAt stack $ (r reg) + 1
      reg' = if tmp1 /= -1
                then reg {r = tmp1 + 1,
                          p = tmp2}
                else reg {p = tmp2}
  in (stack, pcode, env, reg')


backtrack :: Storage -> Storage
backtrack stor@(stack, pcode, env, reg)
  | b reg =
    case (numAt stack $ c reg, numAt stack $ r reg) of
         (-1, -1) -> let reg' = reg {p = cLast env}
                     in (stack, pcode, env, reg')
         (-1, _) -> let tmp = numAt stack $ r reg
                        reg' = reg {c = tmp,
                                    r = tmp + 1}
                        stack' = take (tmp + 4) stack
                    in backtrack (stack', pcode, env, reg')
         _ -> let stack' = setCNext stor
                  reg' = reg {p = numAt stack $ c reg,
                              b = False}
              in (stack', pcode, env, reg')
  | otherwise = let reg' = reg {p = (p reg) + 1}
                in (stack, pcode, env, reg')


-- replace element at a given stack position
replace :: StackElem -> Stack -> Int -> Stack
replace x stack k
  | k < 0 || k >= length stack = error "index out of range"
  | otherwise =
    let l = length stack
    in take (l - k - 1) stack ++ x : drop (l - k) stack


elemAt :: Stack -> Int -> StackElem
elemAt stack k = stack !! ((length stack) - k - 1)


numAt :: Stack -> Int -> Int
numAt stack k =
  case elemAt stack k of
       Number n -> n
       _ -> error "expected Number, but got STR"


setCNext :: Storage -> Stack
setCNext (stack, _, env, reg) =
  replace (Number $ cNext env $ numAt stack $ c reg) stack $ c reg
