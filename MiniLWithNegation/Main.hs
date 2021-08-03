module Main where


import Declarations
import Tokenizer
import Parser
import MiniTranslator
import MiniL


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
                      l = 0}
  nextSolution (stack, pcode, env, reg)


nextSolution :: Storage -> IO ()
nextSolution s = do
  let result = evaluate s
      (stack, pcode, env, reg) = result
  if b reg
     then putStrLn "No (more) solutions"
     else do putStrLn $ showSolution stack
             wantMore result


showSolution :: Stack -> String
showSolution = showSolution' "" where
  
  showSolution' :: String -> Stack -> String
  showSolution' acc [] = acc
  showSolution' acc ((STR s):(NUM 1):t) =showSolution' (spaces t ++ "not " ++ s ++ "\n" ++ spaces t ++"  proof of " ++ s ++ " fails." ++ ('\n':acc)) t
  showSolution' acc ((STR s):(NUM (-1)):t) =showSolution' (spaces t ++ "not " ++ s ++ "\n" ++ spaces t ++"  proof of " ++ s ++ " fails." ++ ('\n':acc)) t
  showSolution' acc ((STR s):(NUM 0):t) =showSolution' (spaces t ++ s ++ ('\n':acc)) t
  showSolution' acc (_:t) = showSolution' acc t
  
  spaces :: Stack -> String
  spaces ((NUM n):t) = spaces' "" n
  
  spaces' :: String -> Int -> String
  spaces' acc i
    | i <= 0 = acc
    | otherwise = spaces' (' ':' ':acc) $ i - 1


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
