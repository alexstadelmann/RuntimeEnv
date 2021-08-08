module Main where


import Declarations
import Tokenizer
import Parser
import Translator
import GroundL


main :: IO ()
main = do
  putStrLn "Please, specify a file to be compiled."
  putStr "> "
  inputFilePath <- getLine
  inputProgram <- readFile inputFilePath
  let tokens = tokenize inputProgram
      syntaxTree = parse tokens
      pcode = translate syntaxTree
      stack = []
      env = createEnv pcode
      reg = Register {c = -1,
                      r = -1,
                      b = False,
                      p = cGoal env,
                      l = 0,
                      up = 0}
  nextSolution (stack, pcode, env, reg)


nextSolution :: Storage -> IO ()
nextSolution s = do
  let result = evaluate s
      (stack, pcode, env, reg) = result
  if b reg
     then putStrLn "No (more) solutions"
     else do putStrLn $ showSolution $ reverse stack
             wantMore result


showSolution :: Stack -> String
showSolution (NUM n : t@(STR _ _ : _)) =
  let result = display "" t
  in spaces n ++ fst result ++ "\n" ++ showSolution (snd result)
showSolution [] = ""
showSolution (_ : t) = showSolution t


display :: String -> Stack -> (String, Stack)
display acc (STR s i : t)
  | i > 0 = display' (acc ++ s ++ "(") t $ i - 1
  | otherwise = (acc ++ s, t) where
  
  display' :: String -> Stack -> Int -> (String, Stack)
  display' acc st i
    | i > 0 = let result = display acc st
              in display' (fst result ++ ", ") (snd result) $ i - 1
    | otherwise = let result = display acc st
                  in (fst result ++ ")", snd result)


spaces :: Int -> String
spaces i
  | i > 0 = ' ' : '|' : ' ' : spaces (i - 1)
  | otherwise = ""


wantMore :: Storage -> IO ()
wantMore s@(stack, pcode, env, reg) = do
  putStrLn "Want More? (Yes [Y], No [N])"
  putStr "> "
  more <- getLine
  case more of
       "N" -> putStrLn "Good Bye!"
       "Y" -> let reg' = reg {b = True,
                              p = p reg - 1,
                              r = c reg + 1,
                              l = numAt stack $ c reg + 3}
              in nextSolution (stack, pcode, env, reg')
       _ -> do putStrLn "Not a valid input"
               wantMore s
