module Lib
  ( someFunc,
    Tab (..),
    toTab,
  )
where

import CoC.Base (base)
import CoC.Checker (TypeChecker (..), idecls, judge)
import CoC.Term (Const (..), ITypeDecl (..), Sort (..), Term (..), appArgs, apps, piArgs, subs)
import DB (conn)
import Data.Bifunctor (bimap, first)
import qualified Data.List as L (intercalate)
import qualified Data.Map as M
import Data.Text (Text, intercalate, unpack)
import Database.PostgreSQL.Simple (Only, query_)

{-
Cannot make tables for open terms
Cannot make tables for Prop or Type
CAN make tables for Set (if we only consider itypes currently defined)
Can only make tables for itypes that have all (TYPE) etc. params substituted
Logic might need to be converted to SQL logic

\x:Bool-> x :: Pi x: Bool -> Bool
\x:Bool -> bnot x :: Pi x: Bool -> Bool

\ x: Nat -> vector bool x :: Nat -> Set
-}
data Tab = IT {itd :: ITypeDecl, itParams :: [Term]} | PiT Text Term Term

----
size :: Term -> Int
size = \case
  S Set -> 1
  S Prop -> 1
  S (Type i) -> i + 1
  Var _ -> 1
  F -> 1
  C _ _ -> 1
  Pi _ x y -> sizes x y
  Lam _ x y -> sizes x y
  App x y -> sizes x y
  Fix _ t cs -> size t + sizeCases cs + 1
  Match t _ r cs -> sizes t r + sizeCases cs
  where
    sizes x y = size x + size y + 1
    sizeCases = sum . map (uncurry (+) . bimap size size)

-- SQL to insert a term if possible
insert :: TypeChecker -> Term -> String
insert tc t = insertTemplate tn ("size" : cols) (show (size t) : vals)
  where
    tab = toTab tc (judge tc t)
    tn = tabName tc tab
    (cols, vals) = case (appArgs t, tab) of
      ((C Ic n) : _, it@IT {}) -> ([tabName tc it ++ "_type"], [unpack n])
      _ -> ([], [])

insertTemplate :: String -> [String] -> [String] -> String
insertTemplate tName cols vals = concat ["INSERT INTO ", tName, " (", L.intercalate "," cols, ") VALUES (\n\t", L.intercalate "," vals, ")"]

----

toTab :: TypeChecker -> Term -> Tab
toTab tc = \case
  S (Type _) -> error "Cannot make table for non-set sorts"
  S Prop -> error "Cannot make table for non-set sorts"
  Var _ -> error "Cannot make table for variables"
  F -> error "Cannot make table for fixpoint reference"
  C Ic _ -> error "Cannot make table for type constructor"
  C D _ -> error "Cannot make table for definition"
  C WC _ -> error "Cannot make table for wildcard"
  Lam {} -> error "Cannot make table for Lam"
  Match {} -> error "Cannot make table for unbound Match"
  Fix {} -> error "Cannot make table for unbound Fix"
  S Set -> error "Cannot make table for Set"
  Pi x y z -> PiT x y z
  t -> case appArgs t of
    ((C I n) : ts) -> IT (idecls tc n) ts
    x -> error $ show x

--------------------------------------
tabName :: TypeChecker -> Tab -> String
tabName _ (IT itdcl ps) = show $ apps $ C I (itName itdcl) : ps
tabName _ (PiT v t x) = show $ Lam v t x

-- CREATE TABLE statement
createTab :: TypeChecker -> Tab -> String
createTab tc tab@(IT it' ps) =
  concat
    [tabType, "create table ", tname, "(\n\t", tname, "_id SERIAL primary key,\n\tsize INT not null,\n\t", tname, "_type ", tname, "_type not null", concatMap (f . bimap unpack (subDecl ps it')) (itConstructors it'), "\n\t);"]
  where
    tname = tabName tc tab
    f (conName, conType) = let (args, _) = piArgs conType in concatMap (g conName . first unpack) args
    g cn (colName, colType) = let c = concat [cn, "_", colName] in concat [",\n\t", c, " SERIAL ", "\n\t\tcheck ((", tname, "_type = '", cn, "') = (", c, " is not null)),\n\t", c, " REFERENENCES ", tabName tc $ toTab tc colType]
    tabType = concat ["\ncreate type ", tname, "_type as enum ('", unpack $ intercalate "','" (map fst (itConstructors it')), "');\n"]
createTab tc (PiT v t r) =
  concat
    ["create table ", tname, "(\n\t", tname, "_id SERIAL primary key,\n\tsize INT not null,\n\t", tname, "_type ", tname, "_type not null", concatMap (f . bimap unpack (subDecl ps it')) (itConstructors it'), "\n\t);"]
  where
    tname = show $ Pi v t r

------
-- Substitute an ITypeDecl with n parameters with a list of n terms
subDecl :: [Term] -> ITypeDecl -> Term -> Term
subDecl xs (ITypeDecl _ _ ps _)
  | length xs /= length ps = error "subDecl bad # of args"
  | otherwise = flip subs map'
  where
    map' = M.fromList $ zipWith (curry (\(a, (n, _)) -> (n, a))) xs ps

--------------------------------------
someFunc :: IO ()
someFunc = do
  putStrLn $ createTab base $ toTab base $ C I "Bool"
  putStrLn $ createTab base $ toTab base $ C I "Nat"
  putStrLn $ insert base (C Ic "ff")
  cxn <- conn
  xs <- query_ cxn "SELECT bool_type::text from bool" :: IO [Only String]
  print xs

-- putStrLn $ createTab $ undefined
