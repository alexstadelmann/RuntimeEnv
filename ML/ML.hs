module ML
(
  evaluate,
  numAt,
  elemAt,
  deref
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
-- evaluate stor@(st, cod, _, reg, tr, us)
--   | trace (show st ++ "   " ++ show reg ++ "   " ++ show tr ++ "   "
--     ++ show us ++ "   " ++ show (cod !! (p reg)) ++ "\n")
--     False = undefined
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
  let retAdd = getRetAdd cod $ p reg + 3
      next = cNext env
      first = head $ clauses env
      cFirst =
        if clauses env == []
           then -1
           else if p reg >= cGoal env
                   then 0
                   else if next (p reg) == next first
                           then next first
                           else first
      st' = NUM (l reg)
          : NUM (length tr)
          : NUM (e reg)
          : NUM retAdd
          : NUM (c reg)
          : NUM cFirst
          : st
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

push (VAR' s inEnv) (st, cod, env, reg, tr, us) =
  let ref = if inEnv
               then -1
               else sAdd s True st reg
      st' = VAR s ref : st
      reg' = reg {p = p reg + 1}
  in (st', cod, env, reg', tr, us)

push (EndEnv' n) (st, cod, env, reg, tr, us) =
  let st' = EndEnv : st
      reg' = reg {e = length st - n,
                  p = p reg + 1}
  in (st', cod, env, reg', tr, us)


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
  | numAt st (r reg + 4) /= l reg - 1 =
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
              in (st'', cod, env, reg', tr', [])
  | otherwise = let reg' = reg {p = p reg + 1}
                in (st, cod, env, reg', tr, us) where

  unbind :: Stack -> Register -> Trail -> (Stack, Trail)
  unbind st reg = unbind' (numAt st (c reg + 4) + 1) st

  unbind' :: Int -> Stack -> Trail -> (Stack, Trail)
  unbind' i st tr@(h : t)
    | i > length tr = (st, tr)
    | otherwise =
      if h < length st
         then let VAR s _ = elemAt st h
                  st' = replace st h $ VAR s $ -1
              in unbind' (i + 1) st' t
         else unbind' (i + 1) st t
  unbind' _ st tr = (st, tr)


unify :: Arg -> Storage -> Storage
unify (STR' sym ar) (st, cod, env, reg, tr, us)
  | pc reg >= 1 =
    let reg' = reg {pc = pc reg - 1 + ar,
                    l = numAt st (c reg + 5) + 1,
                    p = p reg + 1}
        st' = STR sym ar : st
    in (st', cod, env, reg', tr, us)
  | otherwise =
    case elemAt st $ deref st $ up reg of
         VAR sym' _ ->
           let ref = deref st $ up reg
               reg' = reg {pc = ar}
               tr' = ref : tr
               st' = replace st ref $ VAR sym' $ length st
               st'' = STR sym ar : st'
           in updateReg (st'', cod, env, reg', tr', us) 0
         STR sym' ar' ->
           if sym /= sym' || ar /= ar'
              then let reg' = reg {b = True,
                                   p = p reg + 1}
                   in (st, cod, env, reg', tr, us)
              else let (us', reg') = save_AC_UP us reg st
                   in updateReg (st, cod, env, reg', tr, us') ar

unify (VAR' sym _) sto@(st, cod, env, reg, tr, us)
  | pc reg >= 1 =
    let reg' = reg {pc = pc reg - 1,
                    l = numAt st (c reg + 5) + 1,
                    p = p reg + 1}
        st' = VAR sym (sAdd sym False st reg) : st
    in (st', cod, env, reg', tr, us)
  | otherwise =
    let ref = deref st $ sAdd sym False st reg
    in case elemAt st ref of
            VAR sym' _ ->
              let st' = replace st ref $ VAR sym' $ up reg
                  tr' = sAdd sym False st' reg : tr
                  reg' = reg {sc = arity $ elemAt st' $ up reg}
              in skip (st', cod, env, reg', tr', us)
            STR sym' ar -> unify' sto [ref, up reg]


unify' :: Storage -> [Int] -> Storage
unify' (st, cod, env, reg, tr, us) [] =
  let reg' = reg {sc = arity $ elemAt st $ up reg}
  in skip (st, cod, env, reg', tr, us)

unify' sto@(st, cod, env, reg, tr, us) (add1 : add2 : t) =
  let d1 = deref st add1
      d2 = deref st add2
  in if d1 /= d2
        then
          case (elemAt st d1, elemAt st d2) of
            (VAR v _, _) ->
              let st' = replace st d1 $ VAR v d2
                  tr' = d1 : tr
              in unify' (st', cod, env, reg, tr', us) t
            (_, VAR v _) ->
              let st' = replace st d2 $ VAR v d1
                  tr' = d2 : tr
              in unify' (st', cod, env, reg, tr', us) t
            (STR sym1 ar1, STR sym2 ar2) ->
              if sym1 /= sym2 || ar1 /= ar2
                 then let reg' = reg {b = True,
                                      sc = arity $ elemAt st $ up reg}
                      in updateReg (st, cod, env, reg', tr, us) 0
                 else let adds = pushAdds t d1 d2 1 ar1
                      in unify' (st, cod, env, reg, tr, us) adds
        else unify' sto t where

  pushAdds :: [Int] -> Int -> Int -> Int -> Int -> [Int]
  pushAdds adds d1 d2 i ar
    | i <= ar = pushAdds (d1 + i : d2 + i : adds) d1 d2 (i + 1) ar
    | otherwise = adds


skip :: Storage -> Storage
skip sto@(st, cod, env, reg, tr, us)
  | sc reg >= 1 =
    let ar = arity $ elemAt st $ up reg + 1
        reg' = reg {up = up reg + 1,
                    sc = sc reg - 1 + ar}
    in skip (st, cod, env, reg', tr, us)
  | otherwise = updateReg sto 0


--      varname   push?   stack    register    pos in env
sAdd :: String -> Bool -> Stack -> Register -> Int
sAdd sym pushmode st reg
  | pushmode =
    if c reg < 0
       then -1
       else sAdd' sym st $ numAt st $ c reg + 3
  | otherwise = sAdd' sym st $ e reg where

  sAdd' :: String -> Stack -> Int -> Int
  sAdd' sym st i =
    let VAR sym' _ = elemAt st i
    in if sym == sym'
          then i
          else sAdd' sym st $ i + 1


deref :: Stack -> Int -> Int
deref st add =
  case elemAt st add of
    STR _ _ -> add
    VAR _ add' -> if add' == -1
                then add
                else deref st add'


arity :: StackElem -> Int
arity (VAR _ _) = 0
arity (STR _ ar) = ar


updateReg :: Storage -> Int -> Storage
updateReg (st, cod, env, reg, tr, us) i =
  let (us', reg') = restore_AC_UP st us $ add_AC reg $ i - 1
      reg'' = reg' {up = up reg' + 1,
                    l = numAt st (c reg + 5) + 1,
                    p = p reg' + 1}
  in (st, cod, env, reg'', tr, us')


add_AC :: Register -> Int -> Register
add_AC reg n
  | ac reg /= -1 = reg {ac = ac reg + n}
  | otherwise = reg


restore_AC_UP :: Stack -> US -> Register -> (US, Register)
restore_AC_UP st us@(newUP : newAC : t) reg
  | ac reg == 0 =
    if newAC == 0
       then restore_AC_UP st t reg
       else let reg' = reg {ac = newAC,
                            up = newUP}
            in (t, reg')
  | otherwise = (us, reg)
restore_AC_UP _ us reg = (us, reg)


save_AC_UP :: US -> Register -> Stack -> (US, Register)
save_AC_UP us reg st
  | deref st (up reg) /= up reg
  && arity (elemAt st (deref st (up reg))) /= 0 =
    let reg' = reg {up = deref st (up reg), ac = 1}
--         acAppend = if ac reg > 1
--                       then ac reg - 1
--                       else ac reg
        us' = up reg : ac reg - 1 : us
    in (us', reg')
  | otherwise = (us, reg)


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
  let next = cNext env
      cCur = numAt st $ c reg
      cNew = if next cCur == -1 || p reg >= cGoal env
                then next cCur
                else if next (p reg) == next (next cCur)
                        then next $ next cCur
                        else next cCur
  in replace st (c reg) $ NUM cNew
