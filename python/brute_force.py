from typing import List as L, Tuple as T
from term import Term
from base import base
from abc import ABCMeta, abstractmethod, abstractproperty, abstractstaticmethod, abstractclassmethod
from dataclasses import dataclass


# def to_tables(t: Term) -> T[str, Table]:
#     keys = ','.join(["'%s'" % d for d in t.decls.keys()])
#     create = f"CREATE TYPE {t.name}_con AS ENUM ({keys});"
#     breakpoint()
#     raise NotImplementedError
#     return create, Table(t.name, '', fks)


class DBT(metaclass=ABCMeta):
    """
    Any CLOSED term must have a corresponding table

    Except for kinds (Set/Type/Prop) which we don't want tables for.

    Lam(x: )
    """

    t: Term

    def __post_init__(self) -> None:
        assert self.t.freevars() == set()  # closed

    @abstractproperty
    def name(self) -> str:
        raise NotImplementedError


@dataclass(order=True, frozen=True)
class DBItype(DBT):
    t: Term

    @property
    def name(self) -> str:
        x = self.t.__str__()
        reveal_type(x)
        return x


def main() -> None:
    pass
#     ci = ConnectInfo()
#     conn = ci.connect()
#     create_int(conn)


if __name__ == '__main__':
    main()
