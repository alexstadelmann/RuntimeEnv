module ML
(
  evaluate,
  numAt
)
  where


-- import Debug.Trace

import Declarations
import Translator


evaluate :: Storage -> Storage
evaluate stor@(_, cod, _, reg, _, _) =
  case cod !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- use this version of evaluate for debugging purposes:

-- evaluate :: Storage -> Storage
-- evaluate stor@(st, cod, _, reg, _, _)
--   | trace ((show st) ++ "   " ++ (show reg) ++ "   "
--     ++ (show $ cod !! (p reg)) ++ "\n") False = undefined
--   | otherwise =
--     case cod !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


execute :: Command -> Storage -> Storage
execute (Push arg) = push arg
execute (Unify arg) = unify arg
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


push :: Arg -> Storage -> Storage
push CHP (st, cod, env, reg, tr, us) =
  let st' = NUM (l reg)
          : NUM (length tr - 1)
          : NUM (e reg)
          : NUM retAdd
          : NUM (c reg)
          : NUM (cFirst env)
          : st
      retAdd = getRetAdd cod $ p reg + 3
      reg' = reg {c = length st,
                  r = length st + 1,
                  p = p reg + 1,
                  up = length st + 6,
                  pc = 0,
                  ac = -1}
  in (st', cod, env, reg', tr, []) where

  getRetAdd :: Code -> Int -> Int
  getRetAdd cod i =
    case cod !! i of
         Return -> i
         Prompt -> i
         Push CHP -> i
         _ -> getRetAdd cod $ i + 1

push (STR' s i) (st, cod, env, reg, tr, us) =
  let st' = STR s i : st
      reg' = reg {p = p reg + 1}
  in (st', cod, env, reg', tr, us)

push (VAR' s) (st, cod, env, reg, tr, us) =
  let st' = VAR s (sAdd s True st reg) : st
      reg' = reg {p = p reg + 1}
  in (st', cod, env, reg', tr, us)

push (EndEnv' n) (st, cod, env, reg, tr, us) =
  let st' = EndEnv : st
      reg' = reg {e = length st - 1 - n,
                  p = p reg + 1}
  in (st', cod, env, reg', tr, us)


unify :: Arg -> Storage -> Storage
unify a (st, cod, env, reg, tr, us)
  | not $ b reg =
    let reg' = reg {p = p reg + 1,
                    l = numAt st (c reg + 3) + 1,
                    up = up reg + 1}
    in (st, cod, env, reg', tr, us)
  | otherwise =
    let reg' = reg {p = p reg + 1}
    in (st, cod, env, reg', tr, us)

deref :: Stack -> Int -> Int
deref st i =
  case elemAt st i of
    STR _ _ -> i
    VAR _ i2 -> if i2 == -1
                then i
                else deref st i2

arity :: StackElem -> Int
arity (VAR _ _) = 0
arity (STR _ ar) = ar

add_AC :: Register -> Int -> Int
add_AC reg n
  | ac reg /= -1 = ac reg + n
  | otherwise = ac reg

restore_ac_up :: US -> Register -> (US, Register)
restore_ac_up us@(h1 : h2 : t) reg
  | ac reg == 0 =
    let reg' = reg {ac = h2,
                    up = h1}
    in (t, reg')
  | otherwise = (us, reg)

save_ac_up :: US -> Register -> Stack -> (US, Register)
save_ac_up us reg st
  | up reg <= numAt st (c reg + 5) && deref st (up reg) /= up reg && arity (elemAt st (deref st (up reg))) /= 0 =
    let reg' = reg {up = deref st (up reg), ac = 1 }
        us' = up reg : ac reg : us
    in (us', reg')
  | otherwise = (us, reg)


call :: Storage -> Storage
call stor@(st, cod, env, reg, tr, us)
  | numAt st (c reg) < 0 =
    let reg' = reg {b = True,
                    p = p reg + 1}
    in (st, cod, env, reg', tr, us)
  | otherwise =
    let st' = setCNext stor
        reg' = reg {p = numAt st $ c reg}
    in (st', cod, env, reg', tr, us)


returnL :: Storage -> Storage
returnL (st, cod, env, reg, tr, us)
  | numAt st (r reg + 2) /= l reg - 1 =
    let reg' = reg {r = numAt st (r reg) + 1}
    in returnL (st, cod, env, reg', tr, us)
  | otherwise =
    let reg' = reg {p = numAt st $ r reg + 1,
                    l = l reg - 1,
                    e = numAt st $ r reg + 2}
    in (st, cod, env, reg', tr, us)


backtrack :: Storage -> Storage
backtrack stor@(st, cod, env, reg, tr, us)
  | b reg =
    case (numAt st $ c reg, numAt st $ r reg) of
         (-1, -1) -> let reg' = reg {p = cLast env}
                     in (st, cod, env, reg', tr, us)
         (-1, _) -> let newC = numAt st $ r reg
                        reg' = reg {c = newC,
                                    r = newC + 1}
                        st' = drop (length st - c reg) st
                    in backtrack (st', cod, env, reg', tr, us)
         _ -> let st' = setCNext stor
                  reg' = reg {p = numAt st $ c reg,
                              b = False,
                              e = length st,
                              up = c reg + 6,
                              pc = 0,
                              ac = -1}
                  (st'', tr') = unbind st' reg tr
              in (st'', cod, env, reg', tr, [])
  | otherwise = let reg' = reg {p = p reg + 1}
                in (st, cod, env, reg', tr, us)


unbind :: Stack -> Register -> Trail -> (Stack, Trail)
unbind st reg = unbind' (numAt st (c reg + 4) + 1) st where

  unbind' :: Int -> Stack -> Trail -> (Stack, Trail)
  unbind' i st tr@(h : t)
    | i > length t = (st, tr)
    | otherwise =
      if h < length st
         then let VAR s _ = elemAt st h
                  st' = replace st h $ VAR s $ -1
              in unbind' (i + 1) st' t
         else unbind' (i + 1) st t


replace :: [a] -> Int -> a -> [a]
replace xs pos x =
  let i = length xs - pos - 1
  in take i xs ++ x : drop (i + 1) xs


elemAt :: Stack -> Int -> StackElem
elemAt st i = st !! (length st - i - 1)


numAt :: Stack -> Int -> Int
numAt st i =
  case elemAt st i of
       NUM n -> n
       _ -> error "expected NUM constructor"


setCNext :: Storage -> Stack
setCNext (st, _, env, reg, _, _) =
  let cNew = NUM $ cNext env $ numAt st $ c reg
  in replace st (c reg) cNew

--      varname   push?   stack    register    pos in env
sAdd :: String -> Bool -> Stack -> Register -> Int
sAdd sym pushmode st reg
  | pushmode =
    if c reg < 0
       then -1
       else sAdd' sym st reg $ numAt st $ c reg + 3
  | otherwise = sAdd' sym st reg $ e reg where

  sAdd' :: String -> Stack -> Register -> Int -> Int
  sAdd' sym st reg i =
    let VAR sym' _ = elemAt st i
    in if sym == sym'
          then i
          else sAdd' sym st reg $ i + 1
