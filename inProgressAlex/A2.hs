module A2 (
eval
) where

import Deklarationen
import A1

--geht vorerst davon aus, dass alle Variablen schon umbenannt worden sind und keine Unifikation benötigt wird.
--und die Klauseln sind entweder in "optimaler Reinfolge" oder fail
eval :: (Programm, Substitution) -> Maybe Substitution
eval ((Programm xs (Ziel ys)), zs) =
  eval' xs ys zs


eval' :: [PKlausel] -> [Literal] -> Substitution -> Maybe Substitution
eval' xs [] zs = Just zs
eval' [] ys zs = Nothing
eval' (u@(PKlausel (NVLTerm a _) _):xs) ((Literal _ (NVar (NVLTerm b _))):ys) zs
  | a == b = eval' xs ((rumpf u) ++ ys) zs
  | otherwise = Nothing
