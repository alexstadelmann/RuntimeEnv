module CodeMaker(
code
) where

import DeklarationenTest

code :: Programm -> PCode
code (Programm xs (Ziel s)) = 
  (code' xs []) ++ (codeBody s []) ++ [Promt]

code' :: [PKlausel] -> PCode -> PCode
code' [] ys = ys 
code' ((PKlausel a Nothing):xs) ys =
  ys ++ codeHead a ++ [Return] ++ code' xs ys
code' ((PKlausel a (Just (Ziel b))):xs) ys =
  ys ++ codeHead a ++ codeBody b [] ++ [Return] ++ code' xs ys

--LTerme sind in MiniL Atome (hier einzelne Kleinbuchstaben) und es kommen keine negierten Atome vor.
codeBody :: [Literal] -> PCode -> PCode
codeBody [] ys = ys
codeBody ((Literal false (NVar (NVLTerm x _))):xs) ys =
  ys ++ [Push x, Call, Backtrack] ++ codeBody xs []

--in MiniL nur nullstellige NVLTerme
codeHead :: NVLTerm -> PCode
codeHead (NVLTerm x _) =
  [Unify x, Backtrack]
