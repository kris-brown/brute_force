module Attempt where

-- syntax for iota-lambda-P2
data Term = Var Int | App Term Term | Lam Term
data Type = TVar Int | ForAll Type | Pi Type | TLam Term Type | TApp Type Term | Eq Term Term | I Term Type
data Kind = Type | PiKind Term Type

shift :: Int -> Int -> Term -> Term
shift i c (Var n) = if n < c then Var n else Var (n+i)
shift i c (Lam e) = Lam (shift i (c+1) e)
shift i c (App e1 e2) = App (shift i c e1) (shift i c e2)

tShift :: Int -> Int -> Type -> Type
tShift i c (TVar n) = if n < c then TVar n else TVar (n+i)
tShift i c (TLam e t) = TLam e (tShift i (c+1) t)
tShift i c (TApp t1 t2) = TApp (tShift i c t1) t2
tShift i c (ForAll t) = ForAll (tShift i c t)
tShift _ _ (Pi t1) = Pi t1
tShift _ _ (Eq t1 t2) = Eq t1 t2
tShift _ _ (I t t2) = I t t2
{-CREATE TABLE term (
	id BIGINT PRIMARY KEY,
	type_of BIGINT REFERENCES term(id),
	term_type term_type NOT NULL,
    size int4 NOT NULL,
	name VARCHAR CHECK (term_type in('sort', 'var', 'lam', 'pi', 'fix', 'match', 'def', 'itype', 'icon', 'wildcard') = (term_type IS NOT NULL)),
	t1 BIGINT REFERENCES term(id)
	 CHECK (term_type in ('lam', 'pi', 'app', 'fix', 'match') = (t1 IS NOT NULL)),
	t2 BIGINT REFERENCES term(id)
	 CHECK (term_type in ('lam', 'pi', 'app', 'match') = (t2 IS NOT NULL))
);-}