module A3 (
push,
unify
) where

import DeklarationenTest


stack :: [Mix]
stack = take 20 $ repeat Empty

insert :: Stack -> Int -> Mix -> Stack
insert xs i a = (take i xs)++(a:(drop(i+1) xs))


push :: LTerm -> Stack -> Register -> (Stack, Register)
push a xs reg@(Register i b t c r p) = 
  let temp1 = (take t xs) ++ [String "c_f"] ++ [Int c] ++ [Int (p+3)] ++ [LTerm' a] ++ (drop (t+4) xs)
      temp2 = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
  in
      (temp1, temp2)

exLTerm :: Mix -> LTerm
exLTerm (LTerm' a) = a


unify :: LTerm -> Stack -> Register -> Register
unify a xs register@(Register i b t c r p)
  | a /= exLTerm (xs !! (c+3)) = register {choice = t+1, ret = t+2, top = t+4, pcounter = c+1}

