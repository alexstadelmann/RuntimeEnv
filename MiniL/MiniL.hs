module MiniL (
    push,
    unify,
    call,
    returnL,
    backtrackQ,
    prompt,
    evaluate,
    miniL,
    test
) where

import Declarations
import Translator


type Storage = (Stack, PCode, Env, Register, Result)


-- replace Element at a given stack position
replace :: StackElem -> Stack -> Int -> Stack
replace a xs n
    | n < 0 || n >= length xs = error "index out of scope"
    | otherwise = (take n xs) ++ (a : drop (n+1) xs)


getNumAt :: Stack -> Int -> Int
getNumAt s i = case s !! i of
                    Zahl a -> a
                    _ -> error "expected a Zahl constructor, but got an Atom constructor"


push :: StackElem -> Storage -> Storage
push a (xs, ys, env, reg@(Register _ _ t c _ p), result) =
    let xs' = xs ++ [Zahl 0, Zahl c, Zahl (p+3), a]
        reg' = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
    in (xs', ys, env, reg', result)


unify :: StackElem -> Storage -> Storage
unify a (xs, ys, env, reg@(Register _ _ _ c _ p), result) =
    let reg' = reg { backQ = a /= (xs !! (c+3)), pcounter = p+1}
    in (xs, ys, env, reg', result)


call :: Storage -> Storage
call (xs, ys, env, reg@(Register _ _ _ c _ p), result) = case getNumAt xs c of
        (-1) -> let reg' = reg {backQ=True, pcounter = p + 1}
                in (xs, ys, env, reg', result)
        _ -> let xs' = replace (Zahl (c_next env $ getNumAt xs c)) xs c
                 reg' = reg {pcounter = getNumAt xs c}
             in (xs', ys, env, reg', result)


returnL :: Storage -> Storage
returnL (xs, ys, env, reg@(Register _ _ _ _ r _), result) =
    let a = getNumAt xs r
        b = getNumAt xs (r+1)
    in if a /= (-1)
       then let reg' = reg {ret = a+1, pcounter = b}
            in (xs, ys, env, reg', result)
       else let reg'' = reg {pcounter = b}
            in (xs, ys, env, reg'', result)


backtrackQ :: Storage -> Storage
backtrackQ (xs, ys, env, reg@(Register _ True _ c r _), result) =
    case (getNumAt xs c, getNumAt xs r) of
         (-1, -1) -> let reg' = reg {pcounter = c_last env}
                     in (xs, ys, env, reg', result)
         (-1, _) -> let a = getNumAt xs r
                    in let reg' = reg {choice = a, ret = a + 1, top = a + 3}
                           xs' = take (a + 4) xs
                       in backtrackQ (xs', ys, env, reg', result)
         _ -> let xs' = replace (Zahl $ c_next env $ getNumAt xs c) xs c
                  reg' = reg {pcounter = getNumAt xs c, backQ = False}
              in (xs', ys, env, reg', result)
backtrackQ (xs, ys, env, reg@(Register _ _ _ _ _ p), result) =
    let reg' = reg {pcounter = p + 1}
    in (xs, ys, env, reg', result)


prompt :: Storage -> Storage
prompt (xs, ys, env, reg@(Register _ True _ _ _ _), result) =
    let reg' = reg {pcounter = -1}
    in (xs, ys, env, reg', result)
prompt (xs, ys, env, reg@(Register _ _ _ _ _ p), result) =
    let reg' = reg {backQ = True, pcounter = p - 1}
        result' = xs:result
    in (xs, ys, env, reg', result')


evaluate ::  Storage -> Maybe Result
evaluate all@(_, ys, _, Register _ _ _ _ _ p, result) = case p of
            -1 -> Just result
            _ -> evaluate $ (execute (ys !! p)) all


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
       in let reg = Register {inst = 0, backQ = False, top = -1, choice = -1, ret = -1, pcounter = c_goal env}
          in evaluate (stack, pcode, env, reg, result)


test :: Programm -> Storage
test x = let pcode = translate x
             stack = []
             result = []
         in let env = createEnv pcode
            in let reg = Register {inst = 0, backQ = False, top = -1, choice = -1, ret = -1, pcounter = c_goal env}
               in (stack, pcode, env, reg, result)
