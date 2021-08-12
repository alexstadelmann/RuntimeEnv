{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module GroundL
(
  evaluate,
  numAt
)
  where


--import Debug.Trace

import Declarations
import Translator



evaluate :: Storage -> Storage
evaluate stor@(_, pcode, _, reg) =
  case pcode !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- use this version of evaluate for debugging purposes:

-- evaluate :: Storage -> Storage
-- evaluate stor@(stack, pcode, _, reg)
--   | trace ((show stack) ++ "   " ++ (show reg) ++ "   "
--     ++ (show $ pcode !! (p reg)) ++ "\n") False = undefined
--   | otherwise =
--     case pcode !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


execute :: Command -> Storage -> Storage
execute (Push arg) = push arg
execute (Unify arg) = unify arg
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: PushArg -> Storage -> Storage
push (CHP a) (stack, pcode, env, reg) =
  let stack' = NUM a
             : NUM (l reg)
             : NUM retAdd
             : NUM (c reg)
             : NUM (cFirst env)
             : stack
      retAdd = getRetAdd pcode $ p reg + 3
      reg' = reg {c = length stack,
                  r = length stack + 1,
                  p = p reg + 1,
                  up = length stack + 5}
  in (stack', pcode, env, reg') where

  getRetAdd :: PCode -> Int -> Int
  getRetAdd pcode i =
    case pcode !! i of
         Return -> i
         Prompt -> i
         Push (CHP _) -> i
         _ -> getRetAdd pcode $ i + 1

push (Atom s i) (stack, pcode, env, reg) =
  let stack' = STR s i : stack
      reg' = reg {p = p reg + 1}
  in (stack', pcode, env, reg')


unify :: StackElem -> Storage -> Storage
unify a (stack, pcode, env, reg)
  | not $ b reg =
    let reg' = reg {b = a /= elemAt stack (up reg),
                    p = p reg + 1,
                    l = numAt stack (c reg + 3) + 1,
                    up = up reg + 1}
    in (stack, pcode, env, reg')
  | otherwise =
    let reg' = reg {p = p reg + 1}
    in (stack, pcode, env, reg')


call :: Storage -> Storage
call stor@(stack, pcode, env, reg)
  | numAt stack (c reg) == (-1) && numAt stack (c reg + 4)==0 = -- case: there are no clauses and atom is positive.
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
                            l = l reg - 1}
            in (stack, pcode, env, reg')


backtrack :: Storage -> Storage
backtrack stor@(stack, pcode, env, reg) 
  | b reg = 
    case (numAt stack (c reg + 4), numAt stack (c reg),  numAt stack (r reg), pcode !! (p reg - 1)) of
      (1, -1, _, Unify s) ->  let reg' = reg{p = numAt stack (r reg + 1), -- negative atom "successfully" failed, that is the resolution (of its negation) with all clause-heads failed.
                                             b = False,
                                             l = l reg - 1}
                                  stack' = setNProven stor
                              in (stack', pcode, env, reg')

      (_, -1, -1, _) -> let reg' = reg {p = cLast env}
                        in (stack, pcode, env, reg')

      (_, -1, _, _) ->  let newC = numAt stack $ r reg
                            reg' = reg {c = newC,
                                        r = newC + 1}
                            stack' = drop (length stack - c reg) stack
                        in backtrack (stack', pcode, env, reg')

      _ -> let stack' = setCNext stor
               reg' = reg {p = numAt stack $ c reg,
                           b = False,
                           up = c reg + 5}
           in (stack', pcode, env, reg') 
  | otherwise = let reg' = reg {p = p reg + 1}
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
      pos = length stack - c reg - 1
  in take pos stack ++ cNew : drop (pos + 1) stack

setCNil :: Storage -> Stack
setCNil (stack, _, _, reg) =
  let pos = length stack - c reg - 1
  in take pos stack ++ NUM (-1): drop (pos + 1) stack

setNProven :: Storage -> Stack
setNProven (stack, _, _, reg) =
  let pos = length stack - (r reg + 3) - 1
  in take pos stack ++ NUM (-1):drop (pos + 1) stack

