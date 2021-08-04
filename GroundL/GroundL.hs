module GroundL
(
  evaluate,
  numAt
)
  where

import Declarations
import Translator
-- import Debug.Trace


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
push CHP (stack, pcode, env, reg) =
  let stack' = NUM (l reg)
             : NUM retAdd
             : NUM (c reg)
             : NUM (cFirst env)
             : stack
      retAdd = getRetAdd pcode $ p reg + 3
      reg' = reg {c = length stack,
                  r = length stack + 1,
                  p = p reg + 1,
                  up = length stack + 4}
  in (stack', pcode, env, reg') where

  getRetAdd :: PCode -> Int -> Int
  getRetAdd pcode i =
    case pcode !! i of
         Return -> i
         Prompt -> i
         Push CHP -> i
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
  | numAt stack (c reg) < 0 =
    let reg' = reg {b = True,
                    p = p reg + 1}
    in (stack, pcode, env, reg')
  | otherwise =
    let stack' = setCNext stor
        reg' = reg {p = numAt stack $ c reg}
    in (stack', pcode, env, reg')


returnL :: Storage -> Storage
returnL (stack, pcode, env, reg)
  | numAt stack (r reg + 2) /= l reg - 1 =
    let reg' = reg {r = numAt stack (r reg) + 1}
    in returnL (stack, pcode, env, reg')
  | otherwise =
    let reg' = reg {p = numAt stack $ r reg + 1,
                    l = l reg - 1}
    in (stack, pcode, env, reg')


backtrack :: Storage -> Storage
backtrack stor@(stack, pcode, env, reg)
  | b reg =
    case (numAt stack $ c reg, numAt stack $ r reg) of
         (-1, -1) -> let reg' = reg {p = cLast env}
                     in (stack, pcode, env, reg')
         (-1, _) -> let newC = numAt stack $ r reg
                        reg' = reg {c = newC,
                                    r = newC + 1}
                        stack' = drop (length stack - c reg) stack
                    in backtrack (stack', pcode, env, reg')
         _ -> let stack' = setCNext stor
                  reg' = reg {p = numAt stack $ c reg,
                              b = False,
                              up = c reg + 4}
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
      pos = length stack - (c reg) - 1
  in take pos stack ++ cNew : drop (pos + 1) stack
