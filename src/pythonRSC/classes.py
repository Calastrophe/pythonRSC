from enum import Enum
from typing import Optional

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

class Block:
    def __init__(self, pos: int):
        # The instructions inside this block, with an optional operand.
        self._block: dict[int, (Instruction, Optional[int])] = {}
        # The starting program counter, used to identify if a loop is occuring.
        self._pc: int = pos

    def __str__(self):
        ret_string = ""
        for key in self._block:
            operand = hex(self._block[key][1]) if self._block[key][1] else None
            match self._block[key][0]:
                case Instruction.JMP | Instruction.JMPZ | Instruction.LDAC | Instruction.STAC:
                    ret_string += f"{convert_addr(key) : <12} {self._block[key][0].name :>8}\n{convert_addr(key+1) : <12} {operand:>8}\n"
                case _:
                    ret_string += f"{convert_addr(key) : <12} {self._block[key][0].name :>8}\n"
        return ret_string

    def add_instruction(self, addr: int, instr: Instruction, operand: Optional[int]):
        self._block.setdefault(addr, (instr, operand))


def convert_addr(addr) -> str: ## UTIL FUNC
        return "0x"+hex(addr)[2:].zfill(8)