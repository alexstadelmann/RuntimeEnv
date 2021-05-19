import DeklarationenTest

type Stack = [Mix]
stack :: [Mix]
stack = take 20 $ repeat Empty

insert :: Stack -> Int -> Mix -> Stack
insert xs i a = (take i xs)++(a:(drop(i+1) xs))


push :: LTerm -> Stack -> Register -> Stack
push a xs r =
  (take (t r) xs) ++ [String "c_first()"]++[Int $ c r]++[Int $ (p r)+3]++[LTerm' a]++(drop(t r+4) xs)

updateBacktracking :: Register -> Bool -> Register
updateBacktracking x a= let b = p x in x {b = a, p = b+1}
exLTerm :: Mix -> LTerm
exLTerm (LTerm' a) = a


unify :: LTerm -> Stack -> Register -> Register
unify a xs register@(Register i b t c r p)
  | a /= exLTerm (xs !! (c+3)) = register {c = t+1, r = t+2, t = t+4, p = c+1}

