module MiniL
(
  evaluate
)
  where

import Declarations
import MiniTranslator
-- import Debug.Trace


evaluate :: Storage -> Storage
evaluate stor@(_, pcode, _, reg) =
  case pcode !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- use this version of evaluate for debugging:

-- evaluate :: Storage -> Storage
-- evaluate stor@(stack, pcode, _, reg)
--   | trace ((show stack) ++ "   " ++ (show reg) ++ "\n") False = undefined
--   | otherwise =
--     case pcode !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


execute :: Command -> Storage -> Storage
execute (Push (STR a)) = push $ STR a
execute (Unify (STR a)) = unify $ STR a
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: StackElem -> Storage -> Storage
push a (stack, pcode, env, reg) =
  let stack' = a : (RET (p reg + 2) $ -1)
                 : (NUM $ c reg)
                 : (NUM 0)
                 : stack
      reg' = reg {c = length stack,
                  r = length stack + 1,
                  p = p reg + 1}
  in (stack', pcode, env, reg')


unify :: StackElem -> Storage -> Storage
unify a (stack, pcode, env, reg) =
  let reg' = reg {b = a /= (elemAt stack $ c reg + 3),
                  p = p reg + 1}
  in (stack, pcode, env, reg')


call :: Storage -> Storage
call stor@(stack, pcode, env, reg)
  | numAt stack (c reg) < 0 =
    let reg' = reg {b = True,
                    p = p reg + 1}
    in (stack, pcode, env, reg')
  | otherwise =
    let stack' = setCNext stor
        reg' = reg {p = numAt stack $ c reg}
    in (stack', pcode, env, reg')


returnL :: Storage -> Storage
returnL (stack, pcode, env, reg) =
  let lastCHP = numAt stack $ r reg
      (rOld, rNew) = retAt stack $ r reg + 1
      retAdd = if rNew < 0 then rOld else rNew
      reg' = reg {p = retAdd}
      reg'' = if lastCHP >= 0
                 then reg' {r = lastCHP + 1}
                 else reg'
      stack' = if lastCHP >= 0
                  then replace retAdd' stack $ r reg + 1
                  else stack
      retAdd' = newRetAdd pcode rOld rNew
  in (stack', pcode, env, reg'') where
    
    newRetAdd :: PCode -> Int -> Int -> StackElem
    newRetAdd pcode i j
      | j < 0 = newRetAdd pcode i i
      | pcode !! j == Return = RET i j
      | otherwise = newRetAdd pcode i $ j + 1


backtrack :: Storage -> Storage
backtrack stor@(stack, pcode, env, reg)
  | b reg =
    case (numAt stack $ c reg, numAt stack $ r reg) of
         (-1, -1) -> let reg' = reg {p = cLast env}
                     in (stack, pcode, env, reg')
         (-1, _) -> let newC = numAt stack $ r reg
                        reg' = reg {c = newC,
                                    r = newC + 1}
                        stack' = drop (length stack - newC - 4) stack
                    in backtrack (stack', pcode, env, reg')
         _ -> let stack' = setCNext stor
                  reg' = reg {p = numAt stack $ c reg,
                              b = False}
              in (stack', pcode, env, reg')
  | otherwise = let reg' = reg {p = p reg + 1}
                in (stack, pcode, env, reg')


-- replace element at a given stack position
replace :: StackElem -> Stack -> Int -> Stack
replace x stack k
  | k < 0 || k >= length stack = error "index out of range"
  | otherwise =
    let l = length stack
    in take (l - k - 1) stack ++ x : drop (l - k) stack


elemAt :: Stack -> Int -> StackElem
elemAt stack k = stack !! (length stack - k - 1)


numAt :: Stack -> Int -> Int
numAt stack i =
  case elemAt stack i of
       NUM n -> n
       _ -> error "expected NUM constructor"


retAt :: Stack -> Int -> (Int, Int)
retAt stack i =
  case elemAt stack i of
       RET old new -> (old, new)
       _ -> error "expected RET constructor"


setCNext :: Storage -> Stack
setCNext (stack, _, env, reg) =
  replace (NUM $ cNext env $ numAt stack $ c reg) stack $ c reg
