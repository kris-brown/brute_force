# from term import Var  # , Const, Int, IType, Pi


from term import IConDecl, Pi, Prop
import copy
from base import base, True_
import pytest


def test_pos() -> None:
    '''
    No inductive defs in base fail positivity check, and we can force a failure
    '''
    base.positive_check()
    base_ = copy.deepcopy(base)
    base_.add(True_, IConDecl('x', dict(y=Pi('_', True_, Prop))))
    with pytest.raises(AssertionError):
        base_.positive_check()
