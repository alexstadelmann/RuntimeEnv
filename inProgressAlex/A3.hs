module A3 (
push,
unify
) where

import Declarations
import Translator


insert :: StackElem -> Stack -> Int -> Stack
insert a xs n = (take n xs) ++ [a] ++ (drop (n+1) xs)



push :: String -> Stack -> Register -> (Stack, Register)
push a xs reg@(Register i b t c r p) = 
  let temp1 = (take t xs) ++ [Zahl 0, Zahl c, Zahl (p+3), Atom a] ++ (drop (t+4) xs)
      temp2 = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
  in
      (temp1, temp2)

exAtom :: StackElem -> String
exAtom (Atom a) = a


unify :: String -> Stack -> Register -> Register
unify a xs reg@(Register i b t c r p) = 
  reg { back = (a /= exAtom (xs !! (c+3))), pcounter = p+1}

exZahl :: StackElem -> Int
exZahl (Zahl a) = a 


call :: Stack -> Register -> Env-> (Stack, Register)
call xs reg@(Register i b t c r p) env 
  | 0 > exZahl (xs !! c) = let temp = reg {back=True, pcounter = p+1} in (xs, temp)
  | otherwise = 
    let u = exZahl (xs !! c) in
        (insert (Zahl (c_next env c)) xs c, reg {pcounter = c})
        

return :: Stack -> Register -> Register
return xs reg@(Register i b t c r p) =
    let a =exZahl (xs !! r)
        b =exZahl (xs !! (r+1))
    in  if 0 > a then reg {ret= a+1, pcounter=b} 
        else
            reg {pcounter=b}


backtrack :: Stack -> Register -> Env -> (Stack, Register)
backtrack xs reg@(Register i b t c r p) env
    | b == True = backtrack' xs reg
    | otherwise = (xs, reg {pcounter = p+1}) where
        
        backtrack' :: Stack -> Register  -> (Stack, Register)
        backtrack' xs reg@(Register i b t c r p) 
            | 0 > exZahl (xs !! c) && 0 < exZahl (xs !! r) = backtrack' xs reg{choice=exZahl (xs !! r), ret=c+1, top=c+3}
            | 0 > exZahl (xs !! c) = (xs, reg{pcounter = c_last env})
            | otherwise = ( insert (Zahl (c_next env (exZahl (xs !! c)))) xs c, reg{pcounter = exZahl (xs !! c), back=False})


