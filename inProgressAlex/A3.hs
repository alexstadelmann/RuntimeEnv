module A3 (
push,
unify,
call,
returnL,
backtrackQ
) where

import Declarations
import Translator



insert :: StackElem -> Stack -> Int -> Stack
insert a xs n 
    | n <= 0 || n > length xs = error "index out of scope"
    | True =  (take n xs)++ (a:drop (n+1) xs)



exZahl :: StackElem -> Int
exZahl (Zahl a) = a 

push :: StackElem -> Stack -> Register -> (Stack, Register)
push a xs reg@(Register i b t c r p) = 
  let temp1 = xs ++ [Zahl 0, Zahl c, Zahl (p+3), a] 
      temp2 = reg {choice = t+1, ret = t+2, top = t+4, pcounter = p+1}
  in
      (temp1, temp2)


unify :: StackElem -> Stack -> Register -> Register
unify a xs reg@(Register i b t c r p) = 
  reg { backQ = a /= (xs !! (c+3)), pcounter = p+1}



call :: Stack -> Register -> Env-> (Stack, Register)
call xs reg@(Register i b t c r p) env = case exZahl (xs !! c) of
        (-1) -> (xs, reg {backQ=True, pcounter = p+1})
        _ -> (insert (Zahl (c_next env c)) xs c, reg {pcounter = c})
        

returnL :: Stack -> Register -> Register
returnL xs reg@(Register i b t c r p) =
    let a = exZahl (xs !! r)
        b = exZahl (xs !! (r+1))
    in  
        if a == (-1) 
            then reg {ret = a+1, pcounter = b} 
        else
            reg {pcounter = b}


backtrackQ :: Stack -> Env -> Register ->  (Stack, Register)
backtrackQ xs env reg@(Register i True t c r p) = case (exZahl (xs !! c), exZahl (xs !! r)) of
                ((-1), (-1)) -> (xs, reg {pcounter = c_last env})

                ((-1), _) -> let a = exZahl (xs !! r)
                                in backtrackQ xs env reg {choice = a, ret = a+1, top = a+3}

                (_,_) -> (insert (Zahl (c_next env c)) xs c, reg {pcounter = exZahl (xs !! c), backQ = False})

backtrackQ xs _ reg@(Register i b t c r p) = (xs, reg {pcounter = p+1})
    

