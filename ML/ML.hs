{- |
Module      : ML
Description : The core of the L5 compiler that tries to proof the goal.

ML is the core of the L5 compiler that tries to proof the goal.
Therefore it uses the resolution technique and
backtracks if a resolvent turns out to be unprovable.
-}
module ML
(
  evaluate,
  numAt,
  elemAt,
  deref,
  cNext
)
  where


-- import Debug.Trace

import Declarations


-- | Tries to proof the goal of the L5 program.
evaluate :: Storage -- ^ The storage initialized by the Main module.
  -> Storage -- ^ The storage when reaching the prompt command.
evaluate stor@(_, cod, _, reg, _, _) =
  case cod !! (p reg) of
       Prompt -> stor
       command -> evaluate $ execute command stor


-- Debugging version of evaluate:

-- evaluate :: Storage -> Storage
-- evaluate stor@(st, cod, _, reg, tr, us)
--   | trace (show st ++ "   " ++ show reg ++ "   " ++ show tr ++ "   "
--     ++ show us ++ "   " ++ show (cod !! (p reg)) ++ "\n")
--     False = undefined
--   | otherwise =
--     case cod !! (p reg) of
--          Prompt -> stor
--          command -> evaluate $ execute command stor


-- | Executes the next ML command.
execute :: Command -- ^ The next command of the ML code to be executed.
  -> Storage -- ^ The storage before execution of the command.
  -> Storage -- ^ The storage after execution of the command.
execute (Push arg) = push arg
execute (Unify arg) = unify arg
execute Call = call
execute Return = returnL
execute Backtrack = backtrack


-- | Something is pushed to the stack.
push :: Arg -- ^ What to push to the stack.
  -> Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
push CHP (st, cod, env, reg, tr, us) =
  let retAdd = getRetAdd cod $ p reg + 3
      next = cNext env
      first = head $ clauses env
      cFirst
        | null $ clauses env = -1
        | p reg >= cGoal env = 0
        | next (p reg) /= next first = first
        | otherwise = next first
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


-- | Try to unify the current CHP with a clause.
call :: Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
call stor@(st, cod, env, reg, tr, us)
  | numAt st (c reg) < 0 =
    let reg' = reg {b = True,
                    p = p reg + 1}
    in (st, cod, env, reg', tr, us)
  | otherwise =
    let st' = setCNext stor
        reg' = reg {p = numAt st $ c reg}
    in (st', cod, env, reg', tr, us)


-- | Proof of a clause is finished.
returnL :: Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
returnL (st, cod, env, reg, tr, us)
  | numAt st (r reg + 4) /= l reg - 1 =
    let reg' = reg {r = numAt st (r reg) + 1}
    in returnL (st, cod, env, reg', tr, us)
  | otherwise =
    let reg' = reg {p = numAt st $ r reg + 1,
                    l = l reg - 1,
                    e = numAt st $ r reg + 2}
    in (st, cod, env, reg', tr, us)


-- | Undo earlier unifications, if necessary.
backtrack :: Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
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
              in unbind' i st' t
         else unbind' i st t
  unbind' _ st tr = (st, tr)


-- | Unification of Arg and stack[UP] is tried.
unify :: Arg -- ^ What to unify with stack[UP].
  -> Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
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


-- | Unification two terms on the stack is tried.
unify' :: Storage -- ^ The storage before execution.
  -> [Int] -- ^ Term elements that still need to be unified.
  -> Storage -- ^ The storage after execution.
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


-- | Skip some unification steps as a variable was just 
skip :: Storage -- ^ The storage before execution.
  -> Storage -- ^ The storage after execution.
skip sto@(st, cod, env, reg, tr, us)
  | sc reg >= 1 =
    let ar = arity $ elemAt st $ up reg + 1
        reg' = reg {up = up reg + 1,
                    sc = sc reg - 1 + ar}
    in skip (st, cod, env, reg', tr, us)
  | otherwise = updateReg sto 0


-- | For a variable, calculates the stack address in the local environment.
sAdd :: String -- ^ Name of the variable.
  -> Bool -- ^ True, if in push mode; False, if in unify mode.
  -> Stack -- ^ The current stack.
  -> Register -- ^ The current registers.
  -> Int -- ^ The stack address in the local environment.
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


-- | On the stack, dereferences from term element to where this element is bound to.
deref :: Stack -- ^ The current stack.
  -> Int -- ^ Where to derefence from.
  -> Int -- ^ The dereferencing.
deref st add =
  case elemAt st add of
    STR _ _ -> add
    VAR _ add' -> if add' == -1
                then add
                else deref st add'


-- | Calculates the arity of a variable or predicate.
arity :: StackElem -- ^ The variable or predicate.
  -> Int -- ^ The arity.
arity (VAR _ _) = 0
arity (STR _ ar) = ar


-- | Updates some registers after unify was executed.
updateReg :: Storage -- ^ The storage before execution.
  -> Int -- ^ How much to add to the argument counter.
  -> Storage -- ^ The storage after execution.
updateReg (st, cod, env, reg, tr, us) i =
  let (us', reg') = restore_AC_UP st us $ add_AC reg $ i - 1
      reg'' = reg' {up = up reg' + 1,
                    l = numAt st (c reg + 5) + 1,
                    p = p reg' + 1}
  in (st, cod, env, reg'', tr, us')


-- | Adds a fixed value to the argument counter.
add_AC :: Register -- ^ Registers before execution.
  -> Int -- ^ How much to add.
  -> Register -- ^ Registers after execution.
add_AC reg n
  | ac reg /= -1 = reg {ac = ac reg + n}
  | otherwise = reg


-- | Restore old argument counter and unification pointer if necessary.
restore_AC_UP :: Stack -- ^ The current stack.
  -> US -- ^ Unification stack before execution.
  -> Register -- ^ Registers before execution.
  -> (US, Register) -- ^ Unification stack and registers after execution.
restore_AC_UP st us@(newUP : newAC : t) reg
  | ac reg == 0 =
    if newAC == 0
       then restore_AC_UP st t reg
       else let reg' = reg {ac = newAC,
                            up = newUP}
            in (t, reg')
  | otherwise = (us, reg)
restore_AC_UP _ us reg = (us, reg)


-- | Save current argument counter and unification pointer if necessary.
save_AC_UP :: US -- ^ Unification stack before execution.
  -> Register -- ^ Registers before execution.
  -> Stack -- ^ The current stack.
  -> (US, Register) -- ^ Unification stack and registers after execution.
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


{- |
  Replace an element in a list at a given index with a new one.
  Note that the index is reversed in comparance to common Haskell lists.
-}
replace :: [a] -- ^ The list before execution.
  -> Int -- ^ The index.
  -> a -- ^ The new element.
  -> [a] -- ^ The list after an element was replaced.
replace xs pos x =
  let i = length xs - pos - 1
  in take i xs ++ x : drop (i + 1) xs


-- | Returns the stack element at a given index
elemAt :: Stack -- ^ The stack.
  -> Int -- ^ The index.
  -> StackElem -- ^ The stack element at the given index.
elemAt st i = st !! (length st - i - 1)


-- | Returns a number save in the stack at a given index.
numAt :: Stack -- ^ The stack.
  -> Int -- ^ The index.
  -> Int -- ^ The integer value saved in the stack.
numAt st i =
  case elemAt st i of
       NUM n -> n
       _ -> error "expected NUM constructor"


-- | For the physically topmost CHP, set the next clause to try unification with.
setCNext :: Storage -- ^ The storage before execution.
  -> Stack -- ^ The stack after execution.
setCNext (st, _, env, reg, _, _) =
  let next = cNext env
      cCur = numAt st $ c reg
      cNew
        | next cCur == -1 = next cCur
        | p reg >= cGoal env = next cCur
        | next (p reg) /= next (next cCur) = next cCur
        | otherwise = next $ next cCur
  in replace st (c reg) $ NUM cNew


-- | Finds the next clause starting from a clause in the code environent
cNext :: Env -- ^ The code environemt.
  -> Int -- ^ The current command.
  -> Int -- ^ The next clause.
cNext env = cNext' (clauses env) where

  cNext' :: [Int] -> Int -> Int
  cNext' [] _ = -1
  cNext' (h : t) i
    | h <= i = cNext' t i
    | otherwise = h
