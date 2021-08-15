{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-|
Module      : Main
Description : Module that manages the user-interaction with the logical maschine.

Just like all roads lead to Rome, all modules lead to Main, or spoken plainly,
Main imports the five other modules. The user is asked through an input-output action
to specify the filepath of the L5 program and subsequently the tokenizer, parser and translator 
lay the groundwork for ML to do it's job. Every time a proof is found (and prompt actuated) 
a proof tree is generated and displayed in the terminal. The clause head used in each unification is
presented to the right of an arrow next to the goal literal. Additionally, a list of the relevant
variable substitutions is printed to the screen.
-}

module Main where


import Data.Char (toUpper)
import Data.List
import System.Directory (doesFileExist)
import System.IO

import Declarations
import Tokenizer
import Parser
import Translator
import ML

-- | Central function of the module, read module description for more.
main :: IO ()
main = do
  -- Change settings so that putStr is executed immediatly.
  hSetBuffering stdout NoBuffering
  putStrLn "Please, specify the file path to an L5 program."
  putStr "> "
  -- Check whether user input is admissible.
  inputFilePath <- getLine
  fileExists <- doesFileExist inputFilePath
  let action
        | not $ isSuffixOf ".l5" inputFilePath = do
          putStrLn "Only L5 programs (.l5) may be compiled."
          main
        | not fileExists = do
          putStrLn "This file does not exist."
          main
        | otherwise = do
          inputProgram <- readFile inputFilePath
          putStrLn "Source code:"
          putStrLn inputProgram
          -- Compilation of source code begins.
          let tokens = tokenize inputProgram
              syntaxTree = parse tokens
              cod = translate syntaxTree
              env = createEnv cod
              reg = Reg {b = False,
                         c = -1,
                         r = -1,
                         p = cGoal env,
                         e = -1,
                         l = 0,
                         up = 0,
                         pc = 0,
                         sc = 0,
                         ac = -1}
          nextSolution ([], cod, env, reg, [], [])
  action

-- | Function that seeks to find the next proof and display it if it exists.
nextSolution :: Storage -- ^ Memory of ML that determines its state.
  -> IO () -- ^ Prints the results to the terminal 
nextSolution s = do
  let result = evaluate s
      (st, cod, env, reg, tr, us) = result
  if b reg
     then do putStrLn "\nNo (more) solutions\n"
             main
     else do putStrLn "Proof tree:"
             putStrLn $ showProofTree cod env $ reverse st
             putStrLn "Substitutions:"
             putStrLn $ showVars st $ reverse st
             wantMore result

-- | Display of the sequence of resolution steps that lead to a successful proof. 
showProofTree :: Code -- ^ List of commands
  -> Env -- ^ Helpful data about Code
  -> Stack -- ^ Data structure used to keep track of goals prooven and to be prooven.
  -> String -- ^ Result to be printed to terminal
showProofTree cod env (NUM cN : _ : NUM ret : _ : _ : NUM lvl : t) =
  let acc = display "" t
      acc' = cHead cN ret cod env
  in spaces lvl ++ fst acc ++ " -> " ++ acc' ++ "\n" ++ showProofTree cod env (snd acc)

showProofTree _ _ [] = ""

showProofTree cod env (_ : t) = showProofTree cod env t

-- | Shows the clause head of the clause used in the unification of literal.
cHead :: Int -- ^ Code address of next clause saved in current choice point.
  -> Int -- ^ Return address of current choice point.
  -> Code -- ^ ML-Code.
  -> Env -- ^ Code environment.
  -> String -- ^ Result.
cHead cN ret cod env =
  let cs = clauses env
      cThis
        | ret >= cGoal env = -1
        | cNext env ret < 0 = last cs
        | otherwise =
          cs !! (getPos (cNext env ret) cs - 1)
      clause
        | cN < 0 = last cs
        | otherwise =
          cs !! (getPos cN cs - 1)
      realC
        | cThis /= clause = clause
        | otherwise = cs !! (getPos clause cs - 1)
      st = codeToStack [] $ drop realC cod
  in fst (display "" st) where
  
  -- | Generates stack elements on the basis of Unify commands of current clause.
  codeToStack :: Stack -- ^ Accumulator.
    -> Code -- ^ ML-Code starting with current clause.
    -> Stack -- ^ Resulting stack.
  codeToStack acc (Return : t) = reverse acc
  codeToStack acc (Unify (STR' s a) : t) =
    codeToStack (STR s a : acc) t  
  codeToStack acc (Unify (VAR' s _) : t) =
    codeToStack (VAR s (-1) : acc) t
  codeToStack acc (_ : t) = codeToStack acc t
  codeToStack acc [] = reverse acc

-- | Creates the output string for one literal and returns the remaining stack.  
display :: String -- ^ Accumulator for recursion.
  -> Stack -- ^ Part of the result stack.
  -> (String, Stack) -- ^ String for one Literal and the remaining stack.
display acc (STR s i : t)
  | i > 0 = display' (acc ++ s ++ "(") t $ i - 1
  | otherwise = (acc ++ s, t)

display acc (VAR s _ : t) = (acc ++ s, t)

display acc _ = (acc, [])

-- | Helper function.
display' :: String -- ^ Accumulator for recursion.
  -> Stack -- ^ Part of the result stack.
  -> Int -- ^ Counter to establish the end of a linearisation of a term.
  -> (String, Stack) -- ^ Returns result to function display.
display' acc st i
  | i > 0 = let result = display acc st
            in display' (fst result ++ ", ") (snd result) $ i - 1
  | otherwise = let result = display acc st
                in (fst result ++ ")", snd result)


-- | Creates the list of variable bindings.
showVars :: Stack -- ^ Unreversed stack. 
  -> Stack -- ^ Reversed stack.
  -> String -- ^ List of variable bindings.
showVars all (VAR s (-1) : t) =
  s ++ " / " ++ s ++ "\n" ++ showVars all t 

showVars all (VAR s a : t) =
  displayTerm (deref all a) all ++ " / " ++ s ++ "\n" ++ showVars all t

showVars all (EndEnv : t) = ""

showVars all (_ : t) = showVars all t

-- | Helper function.
displayTerm :: Int -- ^ Stack address.
  -> Stack -- ^ Unreversed full Stack.
  -> String -- ^ Result to be returned to showVars.
displayTerm i all = 
  case elemAt all i of
    (VAR s (-1)) -> s
    (STR s 0) -> s
    (STR s a) -> fst (displayVars "" all $ drop i $ reverse all) 

-- | Helper function.
displayVars :: String -- ^ Accumulator for recursion.
  -> Stack -- ^ Unreversed full stack.
  -> Stack -- ^ Part of reversed stack.
  -> (String, Stack) -- ^ Result to be returned to displayTerm.
displayVars acc all (STR s i : t)
  | i > 0 = displayVars' (acc ++ s ++ "(") all t $ i - 1
  | otherwise = (acc ++ s, t)

displayVars acc all (VAR s (-1) : t) = (acc ++ s, t)

displayVars acc all (VAR s a : t) = (acc ++ displayTerm (deref all a) all, t)

-- | Helper function.
displayVars' :: String -- ^ Accumulator for recursion.
  -> Stack -- ^ Unreversed full stack.
  -> Stack -- ^ Part of reversed stack.
  -> Int -- ^ Counter to establish the end of a linearisation of a term.
  -> (String, Stack) -- ^ Result to be delivered to displayVars.
displayVars' acc all st i
  | i > 0 = let result = displayVars acc all st
            in displayVars' (fst result ++ ", ") all (snd result) $ i - 1
  | otherwise = let result = displayVars acc all st
                in (fst result ++ ")", snd result)

-- | Function that intercalates bars between spaces.
spaces :: Int -- ^ Level in proof tree.
  -> String -- ^ Spaces with bars in between.
spaces i
  | i > 0 = ' ' : '|' : ' ' : spaces (i - 1)
  | otherwise = ""

-- | Function that allows user to ask for another proof or reject it.
wantMore :: Storage -- ^ Memory/State of ML.
  -> IO () -- ^ Prints result to terminal.
wantMore sto = do
  putStrLn "Want More? (Yes [Y], No [N])"
  putStr "> "
  more <- getLine
  case map toUpper more of
       "N" -> wantMore' sto False
       "NO" -> wantMore' sto False
       "Y" -> wantMore' sto True
       "YE" -> wantMore' sto True
       "YES" -> wantMore' sto True
       ";" -> wantMore' sto True
       _ -> do putStrLn "Not a valid input"
               wantMore sto

-- | Helper Function
wantMore' :: Storage -- ^ Memory of ML.
  -> Bool -- ^ True if user requests another round.
  -> IO () -- ^ Result to be delivered to wantMore.
wantMore' sto@(st, cod, env, reg, tr, us) more
  | more =
    let reg' = reg {b = True,
                    p = p reg - 1,
                    r = c reg + 1,
                    l = numAt st $ c reg + 3}
    in nextSolution (st, cod, env, reg', tr, us)
  | otherwise = do putStrLn ""
                   main

-- | Finds the first occurence of an element in a list and returns its index.
getPos :: Eq a => a -- ^ Element seeked.
  -> [a] -- ^ List to seek in.
  -> Int -- ^ Index.
getPos = getPos' 0 where
  
  getPos' :: Eq a => Int -> a -> [a] -> Int
  getPos' i x (h : t)
    | x == h = i
    | otherwise = getPos' (i + 1) x t
  getPos' _ _ _ = error "Element could not be found."
