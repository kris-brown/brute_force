module Basic where

import CoC.Base (base)
import CoC.Checker (TypeChecker (..), judge)
import CoC.Term (Const (..), Sort (..), Term (..))
import Control.Monad (void, when)
import DB (conn)
import Data.Bifunctor (bimap)
import qualified Data.ByteString.UTF8 as BLU
import Data.Hashable (Hashable (..))
import qualified Data.Map as M
import Data.Text (unpack)
import Database.PostgreSQL.Simple (Connection, Only (..), execute_, query)
import Database.PostgreSQL.Simple.Types (Query (..))

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

getParentT1T2 :: TypeChecker -> Term -> Connection -> IO (Maybe Int, Maybe Int, Maybe Int)
getParentT1T2 tc t c = do
  parent <- ins $ if t == S (Type 2) then Nothing else Just (judge tc t)
  [trm1, trm2] <- mapM ins $ t1t2 t
  return (parent, trm1, trm2)
  where
    ins = mbInsertGetHash tc c
    nut = [Nothing, Nothing]
    t1t2 S {} = nut
    t1t2 C {} = nut
    t1t2 Var {} = nut
    t1t2 F = nut
    t1t2 (Pi _ x y) = [Just x, Just y]
    t1t2 (Lam _ x y) = [Just x, Just y]
    t1t2 (App x y) = [Just x, Just y]
    t1t2 (Fix _ x _) = [Just x, Nothing]
    t1t2 (Match x _ y _) = [Just x, Just y]

mbInsertGetHash :: TypeChecker -> Connection -> Maybe Term -> IO (Maybe Int)
mbInsertGetHash _ _ Nothing = pure Nothing
mbInsertGetHash tc c (Just t) = Just <$> insertGetHash tc t c

insertGetHash :: TypeChecker -> Term -> Connection -> IO Int
insertGetHash tc t c = do
  res <- query c "SELECT true FROM term WHERE id = ?" $ Only h :: IO [[Bool]]
  when (null res) (void $ insert tc t c)
  return h
  where
    h = hash t

-- SQL to insert a term if possible
insert :: TypeChecker -> Term -> Connection -> IO Int
insert tc t c = do
  (parent, trm1, trm2) <- getParentT1T2 tc t c
  [[i]] <-
    query
      c
      "INSERT INTO term (id, size, term_type, type_of, name, t1, t2) VALUES (?, ?, ?, ?, ?, ?, ?) RETURNING id"
      (hash t, size t, tt, parent, n, trm1, trm2)
  return i
  where
    (tt, n) = insertData tc t

insertData :: TypeChecker -> Term -> (String, Maybe String)
insertData _ (Var v) = ("var", Just $ unpack v)
insertData _ s@S {} = ("sort", Just $ show s)
insertData _ F = ("fixref", Nothing)
insertData _ (C D _) = error "should subtitute definitions prior to DB insert"
insertData _ (C I n) = ("itype", Just $ unpack n)
insertData _ (C Ic n) = ("icon", Just $ unpack n)
insertData _ (C WC n) = ("wildcard", Just $ unpack n)
insertData _ (Pi x _ _) = ("pi", Just $ unpack x)
insertData _ (Lam x _ _) = ("lam", Just $ unpack x)
insertData _ (App _ _) = ("app", Nothing)
insertData _ (Fix n _ _) = ("fix", Just $ unpack n)
insertData _ (Match _ n _ _) = ("match", Just $ unpack n)

insert' :: TypeChecker -> Term -> IO Int
insert' tc t = conn >>= insert tc t

setup :: IO ()
setup = do
  c <- conn
  f <- readFile "src/create_tables.sql"
  _ <- execute_ c $ Query $ BLU.fromString f
  return ()

main :: IO ()
main = do
  setup
  _ <- insert' base $ S (Type 1) --(defs base M.! "one")
  _ <- insert' base $ defs base M.! "one"
  return ()