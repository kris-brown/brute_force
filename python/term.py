from abc import ABCMeta, abstractmethod
from dataclasses import dataclass, field
from itertools import chain
from collections import defaultdict
from typing import (Optional as O, List as L, Set as S, Tuple as T, Dict as D,
                    Any, Iterator as I)
# from db import Table


class Term(metaclass=ABCMeta):
    '''
    Abstract syntax - actually a 'pseudo-term' b/c it represents
    terms, types, kinds, ...

    Terms can be:
    - Sort
    - Var
    - Abstraction
        - Lambda
        - Pi
    - Application
    '''

    @abstractmethod
    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        raise NotImplementedError

    def __eq__(self, other: object) -> bool:
        assert isinstance(other, Term)
        return self._eq(other)

    @abstractmethod
    def freevars(self, fv: S[str]) -> S[str]:
        raise NotImplementedError

    @abstractmethod
    def beta(self) -> 'Term':
        '''Convert to beta-normal form'''
        raise NotImplementedError

    @abstractmethod
    def sub(self, var: str, term: 'Term') -> 'Term':
        '''Substitute variable for value'''
        raise NotImplementedError

    @abstractmethod
    def __iter__(self) -> I['Term']:
        raise NotImplementedError


@dataclass(order=True, frozen=True)
class Sort(Term):
    sym: str

    def __post_init__(self) -> None:
        assert self.sym in [
            'Prop', 'Set'] or self.sym.isdigit() and int(self.sym) > 0

    def _eq(self, other: Term, _: Any = None) -> bool:
        return isinstance(other, Sort) and self.sym == other.sym

    def __str__(self) -> str:
        if self == Type:
            return 'Type'
        elif self in [Set, Prop]:
            return self.sym
        else:
            return 'Type_' + self.sym

    def freevars(self, fv: S[str]) -> S[str]:
        return fv

    def beta(self) -> Term:
        return self

    def sub(self, var: str, term: Term) -> Term:
        return self

    def __iter__(self) -> I[Term]:
        return iter([self])


Set = Sort('Set')
Prop = Sort('Prop')  # Impredicative
Type = Sort('1')


def lookup(key: str, dic: L[T[str, str]]) -> O[str]:
    for k, v in dic:
        if key == k:
            return v
    return None


@dataclass(order=True, frozen=True)
class Var(Term):
    sym: str

    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        vp = varpairs or []
        if not isinstance(other, Var):
            return False
        else:
            maybe = lookup(self.sym, vp)
            if isinstance(maybe, str):
                return maybe == other.sym
            else:
                maybe = lookup(other.sym, vp)
                if isinstance(maybe, str):
                    return False
                else:
                    return self.sym == other.sym

    def __str__(self) -> str:
        return self.sym

    def freevars(self, fv: S[str]) -> S[str]:
        return set([] if self.sym in fv else [self.sym])

    def beta(self) -> 'Term':
        return self

    def sub(self, var: str, term: Term) -> Term:
        return term if self == var else self

    def __iter__(self) -> I[Term]:
        return iter([self])


class Abs(Term, metaclass=ABCMeta):
    '''Abstraction - common structure of lambda and Pi'''
    sym: str
    t1: Term
    t2: Term

    def _eq(self, other: 'Term', varpairs: L[T[str, str]] = None) -> bool:
        vp = varpairs or []
        if isinstance(other, type(self)):
            if self.t1._eq(other.t1, vp):
                vp_ = [] if self.sym == other.sym else [(self.sym, other.sym)]
                return self.t2._eq(other.t2, vp_ + vp)
        return False

    def freevars(self, fv: S[str]) -> S[str]:
        return set.union(self.t1.freevars(fv), self.t2.freevars(fv))

    def sub(self, var: str, term: Term) -> Term:
        return self if self.sym == var else type(self)(  # type: ignore
            self.sym, self.t1.sub(var, term), self.t1.sub(var, term))

    def beta(self) -> 'Term':
        return self

    def __iter__(self) -> I[Term]:
        return chain(iter([self]), iter(self.t1), iter(self.t2))


@dataclass(order=True, frozen=True)
class Lam(Abs):
    sym: str
    t1: Term
    t2: Term

    def __str__(self) -> str:
        return 'λ{}: ({}), ({})'.format(self.sym, self.t1, self.t2)


@dataclass(order=True, frozen=True)
class Pi(Abs):
    sym: str
    t1: Term
    t2: Term

    def __str__(self) -> str:
        if self.sym in self.t2.freevars(set()):
            return 'Π{}: ({}), ({})'.format(
                self.sym, self.t1, self.t2)
        else:
            return '({}) → ({})'.format(self.t1, self.t2)

        raise NotImplementedError


@dataclass(order=True, frozen=True)
class App(Term):
    t1: Term
    t2: Term

    def _eq(self, other: Term, varpairs: L[T[str, str]] = None) -> bool:
        return (isinstance(other, App) and self.t1._eq(
            other.t1, varpairs) and self.t2._eq(other.t2, varpairs))

    def __str__(self) -> str:
        r, args = self.root()
        return '({} {})'.format(r, ' '.join(map(str, args)))

    def __iter__(self) -> I[Term]:
        return chain(iter([self]), iter(self.t1), iter(self.t2))

    def freevars(self, fv: S[str]) -> S[str]:
        return set.union(self.t1.freevars(fv), self.t2.freevars(fv))

    def beta(self) -> Term:
        assert isinstance(self.t1, Lam)
        return self.t1.t2.sub(self.t1.sym, self.t2)

    def sub(self, var: str, term: Term) -> Term:
        return App(self.t1.sub(var, term), self.t1.sub(var, term))

    def root(self) -> T[Term, L[Term]]:
        '''
        For a term with multiple args applied to it,
        get the original term being applied to,
        e.g. `(f(g))(h)` -> `(f, [g, h])`
        '''
        if isinstance(self.t1, App):
            r, args = self.t1.root()
        else:
            r, args = self.t1, []
        return (r, args + [self.t2])


def Apps(root: Term, args: L[Term]) -> Term:
    '''From `f` and `[g,h,i]` get `((f(g))(h))(i)`'''
    res = root
    for arg in args:
        res = App(res, arg)
    return res


@dataclass(order=True, frozen=True)
class IType(Term):
    name: str
    arity: Term
    pars: T[T[str, Term], ...] = field(default_factory=tuple)

    def __str__(self) -> str:
        return self.name

    def __iter__(self) -> I[Term]:
        return chain(iter([self, self.arity]), *[
            iter(arg) for _, arg in self.pars])

    @classmethod
    def mk(cls, name: str, arity: Term, pars: D[str, Term]) -> 'IType':
        return cls(name, arity, tuple(pars.items()))

    def _eq(self, other: Term, _: L[T[str, str]] = None) -> bool:
        return isinstance(other, IType) and self.name == other.name

    def beta(self) -> Term:
        return self

    def sub(self, _: str, __: Term) -> Term:
        return self

    def freevars(self, _: S[str]) -> S[str]:
        return set()


@dataclass(order=True, frozen=True)
class ICon(Term):
    itype: str
    name: str
    # types of terms required to instantiate this constructor
    args: D[str, Term]
    # terms applied to the index of the IType when this constructor is used
    indices: L[Term]

    def __str__(self) -> str:
        return '%s.%s' % (self.itype, self.name)

    def show(self, itype: IType) -> str:
        iargs = ' '.join(['({0}: {1})'.format(*kv)
                          for kv in self.args.items()])
        pars: L[Term] = [Var(v) for v, _ in itype.pars]
        ret_type = Apps(itype, pars + self.indices)
        return '\n    | {} {} : {}'.format(self.name, iargs, ret_type)

    def _eq(self, other: 'Term', _: L[T[str, str]] = None) -> bool:
        if isinstance(other, ICon):
            return self.itype == other.itype and self.name == other.name
        return False

    def beta(self) -> Term:
        return self

    def sub(self, _: str, __: Term) -> Term:
        return self

    def freevars(self, _: S[str]) -> S[str]:
        return set()

    def __iter__(self) -> I[Term]:
        return chain(iter([self]), *[iter(arg) for arg in self.args.values()],
                     *[iter(arg) for arg in self.indices])


@dataclass(order=True, frozen=True)
class IConDecl:
    name: str = 'mk'
    # types of terms required to instantiate this constructor
    args: D[str, Term] = field(default_factory=dict)
    # terms applied to the index of the IType when this constructor is used
    indices: L[Term] = field(default_factory=list)


@dataclass(order=True, frozen=True)
class Match(Term):
    arg: Term
    cases: D[str, T[L[Term], Term]]

    def _eq(self, other: 'Term', _: L[T[str, str]] = None) -> bool:
        if isinstance(other, Match):
            return self.arg == other.arg and self.cases == other.cases
        return False

    def beta(self) -> Term:
        return self

    def sub(self, _: str, __: Term) -> Term:
        return self

    def freevars(self, _: S[str]) -> S[str]:
        return set()

    def __iter__(self) -> I[Term]:
        return chain(iter([self, self.arg]), *[
            chain(*[iter(p) for p in patterns], iter(return_))
            for patterns, return_ in self.cases.values()])


@dataclass(order=True)
class TypeChecker:
    itypes: D[IType, D[str, ICon]] = field(
        default_factory=lambda: defaultdict(dict))

    def add(self, itype: IType, icon: IConDecl) -> None:
        self.itypes[itype].update(
            {icon.name: ICon(itype.name, icon.name, icon.args, icon.indices)})

    def positive_check(self) -> None:
        '''
        Check every argument to every inductive type constructor
        '''
        def _positive_check(it: Term, t: Term) -> None:
            '''
            Recurse through the term structure. If 'neg' is true, then we have
            entered a negative region and any observation of the inductive
            type is forbidden
            '''
            if isinstance(t, Pi):
                assert it not in t.t1
                _positive_check(it, t.t2)
            elif isinstance(t, App):
                _positive_check(it, t.t1)
                _positive_check(it, t.t2)
                root, args = t.root()
                for arg in args:
                    assert it not in arg
            elif isinstance(t, Lam):
                _positive_check(it, t.t1)
                _positive_check(it, t.t2)

        for it, cons in self.itypes.items():
            for con in cons.values():
                for t in con.args.values():
                    _positive_check(it, t)

    def show(self, t: Term) -> str:
        if isinstance(t, IType):
            cons = self.itypes[t]

            parstr = ' '.join(['(%s: %s)' % x for x in t.pars])
            topstr = 'Inductive {} {} : {} := '.format(t.name, parstr, t.arity)
            cons_strs = [icon.show(t) for icon in cons.values()]
            return topstr + ''.join(cons_strs)
        elif isinstance(t, Match):
            cases = ['{} {} ➡ {}'.format(con, ' '.join(map(str, args)), res)
                     for con, (args, res) in t.cases.items()]
            return 'case ({}) of {}'.format(t.arg, '\n\t'.join(cases))
        else:
            return str(t)

    def judge(self, t: Term, ctx: D[str, Term]) -> Term:
        if isinstance(t, Sort):
            if t.sym in ['Prop', 'Set']:
                return Sort('0')
            else:
                return Sort(str(int(t.sym) + 1))
        elif isinstance(t, Var):
            return ctx[t.sym]
        elif isinstance(t, Lam):
            raise NotImplementedError
        elif isinstance(t, Pi):
            raise NotImplementedError
        elif isinstance(t, App):
            raise NotImplementedError
        elif isinstance(t, IType):
            return t.arity
        else:
            raise TypeError
