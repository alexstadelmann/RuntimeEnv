module A1(
rumpf,
subProgramm,
ident
) where


import Deklarationen

--Hilfsdatentyp: Klauseln ohne Ziel
data Pklausel' = PKlausel' NVLTerm

--gibt den Rumpf einer Pklausel aus
rumpf :: PKlausel -> [Literal]
rumpf (PKlausel _ Nothing) = []
rumpf (PKlausel _ (Just (Ziel xs))) = xs


vars :: [Symbol] -> [Symbol] -> [Symbol]
vars [] ys= ys
vars ((Variable x):xs) ys =
  vars xs ((Variable x):ys)
vars (_:xs) ys = vars xs ys

ident:: [Symbol] -> Substitution
ident xs = ident' (vars xs []) []

ident' :: [Symbol] -> Substitution -> Substitution
ident' [] ys = ys
ident' ((Variable x):xs) ys = ident' xs $ (x, x):ys


--substituiert in einem LTerm
subLTerm :: LTerm -> Substitution -> LTerm
subLTerm (Var x) ys =
  Var $ foldr(\(a,b) acc -> if x == a then b else acc) x ys
subLTerm (NVar xs) ys =
  NVar $ subNVLTerm xs ys

--substitutiert in einem NVLTerm
subNVLTerm :: NVLTerm -> Substitution -> NVLTerm
subNVLTerm (NVLTerm x xs) ys =
  NVLTerm (foldr(\(a,b) acc -> if x == a then b else acc) x ys) $ map (flip subLTerm $ ys) xs

subLiteral :: Literal -> Substitution -> Literal
subLiteral (Literal n x) ys= Literal n $ subLTerm x ys

subZiel :: Ziel -> Substitution -> Ziel
subZiel (Ziel xs) ys = Ziel $ map (flip subLiteral $ ys) xs

subPKlausel :: PKlausel -> Substitution -> PKlausel
subPKlausel (PKlausel x Nothing) ys =
  PKlausel (subNVLTerm x ys) Nothing
subPKlausel (PKlausel x (Just y)) ys =
  PKlausel (subNVLTerm x ys) (Just $ subZiel y ys)

subProgramm :: Programm -> Substitution -> Programm
subProgramm (Programm xs y) ys =
  Programm (map (flip subPKlausel $ ys) xs) (subZiel y ys)
