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
                      p = cGoal env}
--   putStrLn $ show pcode
--   putStrLn $ show env
--   putStrLn $ show reg
  nextSolution (stack, pcode, env, reg)


nextSolution :: Storage -> IO ()
nextSolution s = do
  let result = evaluate s
      (stack, pcode, env, reg) = result
  if b reg
     then putStrLn "No (more) solutions"
     else do putStrLn $ show $ solution stack
--              putStrLn $ show stack
--              putStrLn $ show pcode
--              putStrLn $ show env
--              putStrLn $ show reg
             wantMore result


solution :: Stack -> [String]
solution = solution' [] where
  
  solution' :: [String] -> Stack -> [String]
  solution' acc [] = acc
  solution' acc ((STR s):t) = solution' (s:acc) t
  solution' acc (_:t) = solution' acc t


wantMore :: Storage -> IO ()
wantMore s@(stack, pcode, env, reg) = do
  putStrLn "Want More? (Yes [Y], No [N])"
  putStr "> "
  more <- getLine
  case more of
       "N" -> putStrLn "Good Bye!"
       "Y" -> let stack' = delNewRets stack
                  reg' = reg {b = True,
                              p = p reg - 1,
                              r = c reg + 1}
              in nextSolution (stack', pcode, env, reg')
       _ -> do putStrLn "Not a valid input"
               wantMore s


delNewRets :: Stack -> Stack
delNewRets = delNewRets' [] where
  
  delNewRets' :: Stack -> Stack -> Stack
  delNewRets' acc [] = reverse acc
  delNewRets' acc ((RET x _):t) = delNewRets' ((RET x $ -1):acc) t
  delNewRets' acc (h:t) = delNewRets' (h:acc) t
