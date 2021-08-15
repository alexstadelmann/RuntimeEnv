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


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Please, specify the file path to an L5 program."
  putStr "> "
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
          let tokens = tokenize inputProgram
              syntaxTree = parse tokens
              cod = translate syntaxTree
              st = []
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
              tr = []
              us = []
          nextSolution (st, cod, env, reg, tr, us)
  action


nextSolution :: Storage -> IO ()
nextSolution s = do
  let result = evaluate s
      (st, cod, env, reg, tr, us) = result
  if b reg
     then do putStrLn "\nNo (more) solutions\n"
             main
     else do putStrLn "Prooftree:"
             putStrLn $ showSolution $ reverse st
             putStrLn "Substitutions:"
             putStrLn $ showVars st $ reverse st
             wantMore result


showSolution :: Stack -> String
showSolution (NUM n : t@(STR _ _ : _)) =
  let result = display "" t
  in spaces n ++ fst result ++ "\n" ++ showSolution (snd result)
showSolution(NUM n : t@(VAR _ _:_)) =
  let result = display "" t
  in spaces n ++ fst result ++ "\n" ++ showSolution (snd result)
showSolution [] = ""
showSolution (_ : t) = showSolution t

display :: String -> Stack -> (String, Stack)
display acc (STR s i : t)
  | i > 0 = display' (acc ++ s ++ "(") t $ i - 1
  | otherwise = (acc ++ s, t)
display acc (VAR s _ : t) = (acc ++ s, t)

display' :: String -> Stack -> Int -> (String, Stack)
display' acc st i
  | i > 0 = let result = display acc st
            in display' (fst result ++ ", ") (snd result) $ i - 1
  | otherwise = let result = display acc st
                in (fst result ++ ")", snd result)


-- the result stack, as it is when prompt is actuated, is called "all" in the subsequent functions.
showVars :: Stack -> Stack -> String
showVars all ((VAR s (-1):t)) = s ++ "/" ++ s ++ "\n" ++ showVars all t 
showVars all (VAR s a : t) = displayTerm (deref all a) all ++ "/" ++ s ++ "\n" ++ showVars all t
showVars all (EndEnv : t) = ""
showVars all (_: t) = showVars all t 


displayTerm :: Int -> Stack -> String
displayTerm i all = 
  case elemAt all i of
    (VAR s (-1)) -> s
    (STR s 0) -> s
    (STR s a) -> fst (displayVars "" all $ drop i $ reverse all) 


displayVars :: String -> Stack -> Stack -> (String, Stack)
displayVars acc all (STR s i : t)
  | i > 0 = displayVars' (acc ++ s ++ "(") all t $ i - 1
  | otherwise = (acc ++ s, t)
displayVars acc all (VAR s (-1) : t) = (acc ++ s, t)
displayVars acc all (VAR s a : t) = (acc ++ displayTerm (deref all a) all, t)


displayVars' :: String -> Stack -> Stack -> Int -> (String, Stack)
displayVars' acc all st i
  | i > 0 = let result = displayVars acc all st
            in displayVars' (fst result ++ ", ") all (snd result) $ i - 1
  | otherwise = let result = displayVars acc all st
                in (fst result ++ ")", snd result)


spaces :: Int -> String
spaces i
  | i > 0 = ' ' : '|' : ' ' : spaces (i - 1)
  | otherwise = ""


wantMore :: Storage -> IO ()
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

wantMore' :: Storage -> Bool -> IO ()
wantMore' sto@(st, cod, env, reg, tr, us) more
  | more =
    let reg' = reg {b = True,
                    p = p reg - 1,
                    r = c reg + 1,
                    l = numAt st $ c reg + 3}
    in nextSolution (st, cod, env, reg', tr, us)
  | otherwise = do putStrLn ""
                   main
