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
      pCode = translate syntaxTree
      stack = []
      env = createEnv pCode
      reg = Register {c = -1,
                      r = -1,
                      b = False,
                      p = cGoal env}
--   putStrLn $ show pCode
--   putStrLn $ show env
--   putStrLn $ show reg
  nextSolution (stack, pCode, env, reg)


nextSolution :: Storage -> IO ()
nextSolution s = do
  let result = evaluate s
      (stack, pCode, env, reg) = result
  if b reg
     then putStrLn "No (more) solutions"
     else do putStrLn $ show stack
--              putStrLn $ show pCode
--              putStrLn $ show env
--              putStrLn $ show reg
             wantMore result


wantMore :: Storage -> IO ()
wantMore s@(stack, pcode, env, reg) = do
  putStrLn "Want More? (Yes [Y], No [N])"
  putStr "> "
  more <- getLine
  case more of
       "N" -> putStrLn "Good Bye!"
       "Y" -> let stack' = delNewRets stack
                  reg' = reg {b = True,
                              p = (p reg) - 1,
                              r = (c reg) + 1}
              in nextSolution (stack', pcode, env, reg')
       _ -> do putStrLn "Not a valid input"
               wantMore s


delNewRets :: Stack -> Stack
delNewRets s = delNewRets' s [] where
  
  delNewRets' :: Stack -> Stack -> Stack
  delNewRets' [] acc = reverse acc
  delNewRets' ((RET o n):t) acc = delNewRets' t $ (RET o $ -1):acc
  delNewRets' (h:t) acc = delNewRets' t $ h:acc
