from term import (TypeChecker, IType, Prop, Pi, Var, IConDecl, Type, App, Set,
                  Lam, Apps, Match)
'''
Define common terms (e.g. Not) and inductive types
and add the latter to a base type checker
'''

base = TypeChecker()
vA, vN, vX = map(Var, 'Anx')

# Inductive types
False_ = IType('False', Prop)
True_ = IType('True', Prop)
OR = IType.mk('OR', Prop, dict(A=Prop, B=Prop))
AND = IType.mk('AND', Prop, dict(A=Prop, B=Prop),)
Eq = IType.mk("Eq", Pi('a', vA, Prop), dict(A=Type, x=vA))
Nat = IType('Nat', Set)
List = IType.mk('List', Type, dict(A=Type))
Le = IType.mk('le', Pi('_', Nat, Prop), dict(n=Nat))
Vect = IType.mk('vector', Pi('_', Nat, Type), dict(A=Type))

# Terms
Not = Lam('A', Prop, Pi('A', Prop, False_))

# Add to checker
for itype, icons in {
    True_: [IConDecl()],
    OR: [IConDecl('inl', dict(a=vA)),
         IConDecl('inr', dict(b=Var('b')))],
    AND: [IConDecl(args=dict(fst=vA, snd=Var('B')))],
    Eq: [IConDecl('refl', indices=[vX, vX])],
    Nat: [IConDecl('zero'), IConDecl('succ', dict(i=Nat))],
    List: [IConDecl('nil'),
           IConDecl('cons', dict(head=vA,
                                 tail=App(List, vA)))]}.items():
    for icon in icons:
        base.add(itype, icon)

Zero, Succ = base.itypes[Nat].values()
base.add(Vect, IConDecl('Vnil', indices=[Zero]))
base.add(Vect, IConDecl('Vcons', dict(a=vA, n=Nat, v=Apps(
    Vect, [vA, vN])), [App(Succ, vN)]))

true, = base.itypes[True_].values()
isZero = Lam('x', Nat, Match(vX, dict(zero=([], True_), succ=([vN], False_))))


if __name__ == '__main__':
    print(*[base.show(x)
            for x in [False_, True_, OR, AND, Eq, Nat, List, Vect, true,
                      isZero]],
          sep='\n\n')
