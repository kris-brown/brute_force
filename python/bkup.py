from typing import FrozenSet as FS, Iterator as I, Dict as D
from dataclasses import dataclass
from frozendict import frozendict


@dataclass(order=True, frozen=True)
class Judgment:
    """
    Something that can be given as a reason and something
    for which reasons can be demanded.

    The type corresponds to a proposition
    The ctx corresponds to assumptions
    The term corresponds to the proof
    """
    ctx: FS['Judgment']
    term: PseudoTerm
    typ: PseudoTerm


@dataclass(order=True, frozen=True)
class Ctx:
    '''
    A set of DECLARATIONS, mappings of pseudoterms to pseudoterms

    Why formal definitions of this describe it as an ordered list of pairs
    (with unique first elements) is beyond me.
    '''
    ctx: frozendict

    @classmethod
    def fromdict(cls, ctx: D[Var, PseudoTerm]) -> 'Ctx':
        return cls(frozendict(ctx))

    def __iter__(self) -> I[PseudoTerm]:
        return iter(self.ctx)

    def __getitem__(self, key: PseudoTerm) -> PseudoTerm:
        return self.ctx[key]  # type: ignore

    def add(self, key: PseudoTerm, val: PseudoTerm) -> 'Ctx':
        assert key not in self
        res = dict(self.ctx.copy())
        res[key] = val
        return Ctx.fromdict(res)

    def start(self, v: Var, kindVar: Var) -> 'Ctx':
        '''Given a sort, declare a variable of that sort'''
        assert v not in self
        assert self[kindVar] == isKind
        return Ctx.fromdict(dict(v=kindVar, **self.ctx))

    def weaken(self, v: Var) -> 'Ctx':
        '''Remove an assumption'''
        assert v in self
        return Ctx.fromdict({k: v for k, v in self.ctx.items() if k != v})

    def app(self, F: Var, a: Var) -> 'Ctx':
        fterm = self[F]
        aterm = self[a]
        assert isinstance(fterm, Pi)
        assert fterm.varType == aterm
        return self.add(App(fterm, aterm), fterm.resType.sub(fterm.var, a))

    def beta(self, A: Var, B: Var, B_: Var) -> 'Ctx':
        assert self[B_] in sorts
        Bterm = self[B]
        assert isinstance(Bterm, App)
        assert Bterm.beta() == B_
        return self.add(A, B_)
        # class Constraint:
        #     pass
        # @dataclass(frozen=True)
        # class Type:
        #     name: str
        #     data: FS['Type']
        #     constraints: FS['Constraint']


def main() -> None:
    x, y = Var('x'), Var('y')
    A, C = Const('A'), Const('c')
    B = App(x, y)
    xab = Lam(x, A, B)
    xabc = App(xab, C)
    bxc = App(C, y)

    assert App(C, y) == B.sub(x, C)
    assert bxc == xabc.beta()

    ctx = Ctx.fromdict({x: A, y: A})
    ctx.add(Var('z'), A)
    print(ctx)


if __name__ == '__main__':
    main()
