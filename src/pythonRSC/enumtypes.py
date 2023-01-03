from enum import Enum

class Instruction(Enum):
    HALT = 0
    LDAC = 1
    STAC = 2
    MVAC = 3
    MOVR = 4
    JMP = 5
    JMPZ = 6
    OUT = 7
    SUB = 8
    ADD = 9
    INC = 10
    CLAC = 11
    AND = 12
    OR = 13
    ASHR = 14
    NOT = 15


class Register(Enum):
    S = 0
    Z = 1
    IR = 2
    AR = 3
    DR = 4
    PC = 5
    OUTR = 6
    ACC = 7
    R = 8

def toReg(source: str) -> Register | None:
    for reg in Register:
        if source == reg.name:
            return reg
    return None