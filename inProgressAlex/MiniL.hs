
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

insert :: StackElem -> Stack -> Int -> Stack
insert a xs n 
    | n < 0 || n >= length xs = error "index out of scope"
    | True =  (take n xs)++ (a:drop (n+1) xs)



exZahl :: StackElem -> Int
exZahl (Zahl a) = a 

push :: StackElem -> (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
push a (xs, ys, env, reg@(Register _ _ t c _ p), result) = 
    let xs' = xs ++ [Zahl 0, Zahl c, Zahl (p+3), a] 
        reg' = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
        in
            (xs', ys, env, reg', result)

unify :: StackElem -> (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
unify a (xs, ys, env, reg@(Register _ _ _ c _ p), result) = 
    let reg' = reg { backQ = a /= (xs !! (c+3)), pcounter = p+1}
        in
            (xs, ys, env, reg', result)


call:: (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
call (xs, ys, env, reg@(Register i b t c r p), result) = case exZahl (xs !! c) of
        (-1) -> let reg' = reg {backQ=True, pcounter = p+1}
                    in
                        (xs, ys, env, reg', result)
        _ -> let xs' = insert (Zahl (c_next env $ exZahl (xs !! c))) xs c
                 reg' = reg {pcounter = exZahl (xs !!c)}
                 in
                    (xs', ys, env, reg', result)
        

returnL :: (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
returnL (xs, ys, env, reg@(Register i b t c r p), result) =
    let a = exZahl (xs !! r)
        b = exZahl (xs !! (r+1))
    in  
        if a /= (-1) 
            then 
                let reg' = reg {ret = a+1, pcounter = b} 
                    in
                        (xs, ys, env, reg', result)
        else
            let reg'' = reg {pcounter = b}
                in
                    (xs, ys, env, reg'', result)


backtrackQ :: (Stack, PCode, Env, Register, Result) ->(Stack, PCode, Env, Register, Result)
backtrackQ (xs, ys, env, reg@(Register i True t c r p), result) = case (exZahl (xs !! c), exZahl (xs !! r)) of
            ((-1), (-1)) -> let reg' = reg {pcounter = c_last env} 
                                in  
                                    (xs, ys, env, reg', result)

            ((-1), _) -> let a = exZahl (xs !! r)
                            in 
                                let reg' = reg {choice = a, ret = a+1, top = a+3}
                                    xs' = take (a + 4) xs
                                    in 
                                        backtrackQ (xs', ys, env, reg', result)

            (_,_) -> let xs' = insert (Zahl (c_next env $ exZahl (xs !! c))) xs c
                         reg' = reg {pcounter = exZahl (xs !! c), backQ = False}
                            in
                                (xs',ys,env,reg', result)

backtrackQ (xs, ys, env, reg@(Register i b t c r p), result) = let reg' = reg {pcounter = p+1}
    in
        (xs, ys, env, reg', result)

prompt :: (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
prompt (xs, ys, env, reg@(Register i True t c r p), result) = 
    let reg' = reg {pcounter = (-1)}
        in
            (xs, ys, env, reg', result)
prompt (xs, ys, env, reg@(Register i b t c r p), result) = 
    let reg' = reg {backQ = True, pcounter = p-1}
        result' = xs:result
            in
                (xs, ys, env, reg',result')

evaluate ::  (Stack, PCode, Env, Register, Result) -> Maybe Result
evaluate all@(xs, ys, env, reg@(Register i b t c r p), result) = case p of
            (-1) -> Just result
            _ -> evaluate $ (doIt (ys !! p)) all
    


doIt :: Command -> (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
doIt (Push (Atom a)) = push (Atom a)
doIt (Unify (Atom a)) = unify (Atom a)
doIt (Call) = call
doIt (Return) = returnL
doIt (Backtrack) = backtrackQ
doIt (Prompt) = prompt

exJust :: Maybe (Stack, PCode, Env, Register, Result) -> (Stack, PCode, Env, Register, Result)
exJust (Just x) = x


miniL :: Programm -> Maybe Result
miniL x = 
    let pcode = translate x
        stack = []
        result = []
        in
            let env = createEnv pcode
                in
                    let reg = Register {inst = 0, backQ = False, top = (-1), choice = (-1), ret = (-1), pcounter = c_goal env}
                        in
                            evaluate (stack, pcode, env, reg, result)

test :: Programm -> (Stack, PCode, Env, Register, Result)
test x = let pcode = translate x
             stack = []
             result = []
                in
                    let env = createEnv pcode
                        in
                            let reg = Register {inst = 0, backQ = False, top = (-1), choice = (-1), ret = (-1), pcounter = c_goal env}
                                in
                                    (stack, pcode, env, reg, result)




