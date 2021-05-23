
module A3 (
push,
unify,
call,
returnL,
backtrackQ,
prompt,
evaluate 
) where

import Declarations
import Translator




insert :: StackElem -> Stack -> Int -> Stack
insert a xs n 
    | n < 0 || n >= length xs = error "index out of scope"
    | True =  (take n xs)++ (a:drop (n+1) xs)



exZahl :: StackElem -> Int
exZahl (Zahl a) = a 

push :: StackElem -> (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
push a (xs, ys, env, reg@(Register _ _ t c _ p)) = 
    let xs' = xs ++ [Zahl 0, Zahl c, Zahl (p+3), a] 
        reg' = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
        in
            (xs', ys, env, reg')

unify :: StackElem -> (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
unify a (xs, ys, env, reg@(Register _ _ _ c _ p)) = 
    let reg' = reg { backQ = a /= (xs !! (c+3)), pcounter = p+1}
        in
            (xs, ys, env, reg')


call:: (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
call (xs, ys, env, reg@(Register i b t c r p)) = case exZahl (xs !! c) of
        (-1) -> let reg' = reg {backQ=True, pcounter = p+1}
                    in
                        (xs, ys, env, reg')
        _ -> let xs' = insert (Zahl (c_next env c)) xs c
                 reg' = reg {pcounter = c}
                 in
                    (xs', ys, env, reg')
        

returnL :: (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
returnL (xs, ys, env, reg@(Register i b t c r p)) =
    let a = exZahl (xs !! r)
        b = exZahl (xs !! (r+1))
    in  
        if a == (-1) 
            then 
                let reg' = reg {ret = a+1, pcounter = b} 
                    in
                        (xs, ys, env, reg')
        else
            let reg' = reg {pcounter = b}
                in
                    (xs,ys,env, reg')


backtrackQ :: (Stack, PCode, Env, Register) ->(Stack, PCode, Env, Register)
backtrackQ (xs, ys, env, reg@(Register i True t c r p)) = case (exZahl (xs !! c), exZahl (xs !! r)) of
                ((-1), (-1)) -> let reg' = reg {pcounter = c_last env} 
                                    in  
                                        (xs, ys, env, reg')

                ((-1), _) -> let a = exZahl (xs !! r)
                                in 
                                    let reg' = reg {choice = a, ret = a+1, top = a+3}
                                        in 
                                            backtrackQ (xs, ys, env, reg')

                (_,_) -> let xs' = insert (Zahl (c_next env c)) xs c
                             reg' = reg {pcounter = exZahl (xs !! c), backQ = False}
                                in
                                    (xs',ys,env,reg')

backtrackQ (xs, ys, env, reg@(Register i b t c r p)) = let reg' = reg {pcounter = p+1}
                                                in
                                                    (xs, ys, env, reg')

prompt :: (Stack, PCode, Env, Register) -> IO (Stack, PCode, Env, Register)
prompt (xs, ys, env, reg@(Register i True t c r p)) = do
                                    putStrLn "no (more) solutions"
                                    let reg' = reg {pcounter = (-1)}
                                        in
                                            return (xs, ys, env, reg')
prompt (xs, ys, env, reg@(Register i b t c r p)) = do
                                putStrLn "yes. more?"
                                response <- getLine
                                if response == ";" 
                                    then 
                                        let reg' = reg {backQ = False, pcounter = p-1}
                                            in
                                                return (xs, ys, env, reg')
                                else
                                    let reg' = reg {pcounter = (-1)} 
                                        in 
                                            return (xs, ys, env, reg')

evaluate :: (Stack, PCode, Env, Register) -> Maybe (Stack, PCode, Env, Register)
evaluate  all@(xs, ys, env, reg@(Register i b t c r p)) = case p of
            (-1) -> Nothing
            _ -> if (ys !! p) == Prompt
                    then 
                        evaluate $ doIt' (ys !! p) $ all
                    else
                        evaluate $doIt (ys !! p) all
    


doIt :: Command -> (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
doIt (Push a) = push (Atom a)
doIt (Unify a) = unify (Atom a)
doIt (Call) = call
doIt (Return) = returnL
doIt (Backtrack) = backtrackQ

doIt' :: Command -> (Stack, PCode, Env, Register) -> IO (Stack, PCode, Env, Register)
doIt' (Prompt) = prompt






