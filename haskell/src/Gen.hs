module Gen where

import CoC.Term (Const (..), ITypeDecl (..), Sort (..), Term (..), lamArgs, apps, piArgs, subs)
import CoC.Checker (TypeChecker, idecls)
import CoC.Base (base)
import Control.Monad.State.Lazy (State , put, get)

import Control.Monad (foldM)

-- Given fuel and a term representing a type
-- generate all possible instances of that type within
-- the fuel budget (return the term + remaining fuel)
gen :: TypeChecker -> Int -> Term ->  [(Int, Term)]
gen _ 0 = const []
gen tc fuel' = \case
    C I x -> genIT tc fuel $ idecls tc x
    _ -> error "invalid type"
  where fuel = fuel' - 1

genIT :: TypeChecker -> Int -> ITypeDecl ->  [(Int, Term)]
genIT tc fuel (ITypeDecl _ _ _ cs)= concatMap (f . fmap (map snd . fst . lamArgs)) cs
  where f (con, conArgs) = map (\(i, x) -> (i, apps $ C Ic con:x)) (gens tc fuel conArgs)

-- Given a certain amount of fuel and a list of types
-- Get all combinations of terms of those types within
-- the fuel budget
gens :: TypeChecker -> Int -> [Term] -> [(Int, [Term])]
gens tc fuel = foldr (genF tc) [(fuel, [])]

genF :: TypeChecker -> Term -> [(Int, [Term])] -> [(Int, [Term])]
genF _ _ [] = []
genF tc nextType partialSolns = concatMap (\(fuel, partialSoln) -> map (fmap (:partialSoln)) $ gen tc fuel nextType) partialSolns

main :: IO ()
main = do
  print $ gen base 100 $ C I "Bool"
  print $ gen base 3 $ C I "Nat"