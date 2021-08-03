module MiniL
(
  evaluate,
  numAt
)
  where

import Declarations
import MiniTranslator
--import Debug.Trace


evaluate :: Storage -> Storage
evaluate stor@(_, pcode, _, reg) =
  case pcode !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- use this version of evaluate for debugging purposes:

-- evaluate :: Storage -> Storage
-- evaluate stor@(stack, pcode, env, reg)
--   | trace ((show stack) ++ "   " ++ (show reg) ++ "   " 
--     ++ (show $ pcode !! (p reg)) ++ "\n") False = undefined
--   | otherwise =
--     case pcode !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


execute :: Command -> Storage -> Storage
execute (Push b (STR a)) = push b (STR a)
execute (Unify (STR a)) = unify $ STR a
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: Int -> StackElem -> Storage -> Storage
push b a (stack, pcode, env, reg) =
  let stack' = a : (NUM b)
                 : (NUM $ l reg)
                 : (NUM retAdd)
                 : (NUM $ c reg)
                 : (NUM $ cFirst env)
                 : stack
      retAdd = if pcode !! (p reg + 2) == Backtrack
                  then p reg + 3
                  else p reg + 2
      reg' = reg {c = length stack,
                  r = length stack + 1,
                  p = p reg + 1}
  in (stack', pcode, env, reg')


unify :: StackElem -> Storage -> Storage
unify a (stack, pcode, env, reg) =
  let reg' = reg {b = a /= (elemAt stack $ c reg + 5),
                  p = p reg + 1,
                  l = numAt stack (c reg + 3) + 1}
  in (stack, pcode, env, reg')


call :: Storage -> Storage
call stor@(stack, pcode, env, reg)
  | numAt stack (c reg) == (-1) && numAt stack (r reg + 3)==0 = -- case: there are no clauses and atom is positive.
    let reg' = reg {b = True,
                    p = cLast env}
    in (stack, pcode, env, reg')
  | numAt stack (c reg) == (-1) = 
    let reg' = reg {p = p reg + 1} --case: there are no clauses and atom is negative.
    in (stack, pcode, env, reg')
  | otherwise =
    let stack' = setCNext stor
        reg' = reg {p = numAt stack $ c reg}
    in (stack', pcode, env, reg')


returnL :: Storage -> Storage
returnL stor@(stack, pcode, env, reg)
  | numAt stack (r reg + 3) == 1 = -- uppermost atom in stack is negative and still a resolution succeeded, thus the next command(=backtrack) is called with the b-flag on.
    let reg' = reg {p = p reg + 1,
                    b = True}
        stack' = setCNil stor
    in (stack', pcode, env, reg')
  | numAt stack (r reg + 2) /= l reg - 1 = --stay on same proof tree level
            let reg' = reg {r = (numAt stack (r reg)) + 1}
            in returnL (stack, pcode, env, reg')
  | otherwise = -- move up one level
            let reg' = reg {p = numAt stack $ r reg + 1,
                           -- r = (numAt stack (r reg)) + 1,
                            l = l reg - 1}
            in (stack, pcode, env, reg')
  
          
    


backtrack :: Storage -> Storage
backtrack stor@(stack, pcode, env, reg) =
  case (b reg, numAt stack (r reg + 3), numAt stack (c reg),  numAt stack (r reg), pcode !! (p reg - 1)) of
    (True, 1, -1, _, Unify s) ->  let reg' = reg{p = numAt stack (r reg + 1), -- negative atom "successfully" failed, that is the resolution (of its negation) with all clause-heads failed.
                                                 b = False,
                                                 l = l reg - 1}
                                      stack' = setNProven stor
                                  in (stack', pcode, env, reg')

    (True, _, -1, -1, _) -> let reg' = reg {p = cLast env}
                            in (stack, pcode, env, reg')

    (True, _, -1, _, _) ->  let newC = numAt stack $ r reg
                                reg' = reg {c = newC,
                                          r = newC + 1}
                                stack' = drop (length stack - newC - 6) stack
                            in backtrack (stack', pcode, env, reg')

    (True, _, _, _, _) -> let stack' = setCNext stor
                              reg' = reg {p = numAt stack $ c reg,
                                          b = False}
                          in (stack', pcode, env, reg') 

    _ ->  let reg' = reg {p = p reg + 1}
          in (stack, pcode, env, reg')


elemAt :: Stack -> Int -> StackElem
elemAt stack i = stack !! (length stack - i - 1)


numAt :: Stack -> Int -> Int
numAt stack i =
  case elemAt stack i of
       NUM n -> n
       _ -> error "expected NUM constructor"


setCNext :: Storage -> Stack
setCNext (stack, _, env, reg) =
  let cNew = NUM $ cNext env $ numAt stack $ c reg
      pos = length stack - (c reg) - 1
  in take pos stack ++ cNew : drop (pos + 1) stack

setCNil :: Storage -> Stack
setCNil (stack, _, _, reg) =
  let pos = length stack - (c reg) - 1
  in take pos stack ++ (NUM (-1)): drop (pos + 1) stack

setNProven :: Storage -> Stack
setNProven (stack, _, _, reg) =
  let pos = length stack - (r reg + 3) - 1
  in take pos stack ++ (NUM (-1)):drop (pos + 1) stack
