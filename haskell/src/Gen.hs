module Gen where

import CoC.Base (base)
import CoC.Checker (TypeChecker, idecls)
import CoC.Term (Const (..), ITypeDecl (..), Sort (..), Term (..), apps, lamArgs, piArgs, sub, subs)
import Control.Monad (foldM)
import Control.Monad.State.Lazy (State, get, put)

-- Given fuel and a term representing a type
-- generate all possible instances of that type within
-- the fuel budget (return the term + remaining fuel)
gen :: TypeChecker -> Int -> Term -> [(Int, Term)]
gen _ 0 = const []
gen tc fuel' = \case
  C I x -> genIT tc fuel $ idecls tc x
  Pi v t x -> concat [gen tc fue (sub v arg x) | (fue, arg) <- gen tc fuel' t]
  x -> error $ "invalid type: " ++ show x
  where
    fuel = fuel' - 1

genIT :: TypeChecker -> Int -> ITypeDecl -> [(Int, Term)]
genIT tc fuel (ITypeDecl _ _ _ cs) = concatMap (f . fmap (map snd . fst . piArgs)) cs
  where
    f (con, conArgs) = map (\(i, x) -> (i, apps $ C Ic con : x)) (gens tc fuel conArgs)

-- Given a certain amount of fuel and a list of types
-- Get all combinations of terms of those types within
-- the fuel budget
gens :: TypeChecker -> Int -> [Term] -> [(Int, [Term])]
gens tc fuel = foldr (genF tc) [(fuel, [])]

genF :: TypeChecker -> Term -> [(Int, [Term])] -> [(Int, [Term])]
genF _ _ [] = []
genF tc nextType partialSolns = concatMap (\(fuel, partialSoln) -> map (fmap (: partialSoln)) $ gen tc fuel nextType) partialSolns

main :: IO ()
main = do
  print $ gen base 100 $ C I "Bool"
  print $ gen base 3 $ C I "Nat"