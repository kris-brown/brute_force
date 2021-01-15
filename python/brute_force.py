# from db import create_int, ConnectInfo

import sys
from lark import Lark
from lark.reconstruct import Reconstructor


grammar = r"""
    ?start: defs

    defs : def*

    def : "Inductive" CNAME typed* ":" term ":=" icon* "."

    icon : "|" CNAME ("("term")" | typed)* ":" term

    ?term:
     | sort
     | lam
     | pi
     | arr
     | app
     | match
     | "(" term ")"
     | var

    sort :
     | "Set" -> set
     | "Prop" -> prop
     | ("Type" [INT]) -> type

    var : CNAME
    lam : ("λ"|"fun") typed* "=>" term
    arr:  term (("->" | "→") term)*
    pi : ("∀"|"Π") typed* "," term
    typed : "(" var ":" term ")"
    app : term*
    match : "match" term "with" matchcase* "."
    matchcase : "|" term "=>" term
    %import common.WS
    %import common.INT
    %import common.CNAME

    %ignore WS
"""
parser = Lark(grammar)

s = '''Inductive vector(A: Type): (Nat) → (Type) :=
    | Vnil: (vector A zero)
    | Vcons(a: A)(n: Nat) (vector A n): (vector A(succ n)) .
'''
tree = parser.parse(s)
rec = Reconstructor(parser).reconstruct(tree)
print(tree, rec, sep='\n')

sys.exit()

for s in ["Set", " Prop", "Type ", " Type 2 ", "Type30", "x", "_y1",
          "λ (x:Set) (y:Prop) => Type", "fun (_x: A) => _x",
          "∀ (x:Set) (y:Prop), Type", "Π (_x: A), _x", "x y",
          "x Set", "(fun (_x: A) => _x) Prop",
          """match F x y with
           | Zero => F x Zero
           | S z => G z z.""", "A -> B", "A -> B -> C -> D"]:
    tree = parser.parse(s)
    rec = Reconstructor(parser).reconstruct(tree)
    print(tree)
    # if '30' in s:
    #     breakpoint()
    assert rec == Reconstructor(parser).reconstruct(parser.parse(rec))

# def main() -> None:
#     ci = ConnectInfo()
#     conn = ci.connect()
#     create_int(conn)


# if __name__ == '__main__':
#     main()
