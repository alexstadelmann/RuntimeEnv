push :: StackElem' -> (Stack, PCode, Env, Register) -> (Stack, PCode, Env, Register)
push a (xs, ys, env, reg@(Register _ _ t c _ p)) = 
    let xs' = xs ++ [Zahl 0, Zahl c, Zahl (p+3), a] 
        reg' = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
        in
            (xs', ys, env, reg')