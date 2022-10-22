from enum import Enum
from typing import List
from BitVector import BitVector # Absolutely horrible library, horrible, horrible, horrible.

class InstructionSet(Enum):
    HALT = '0x0'
    LDAC = '0x1'
    STAC = '0x2'
    MVAC = '0x3'
    MOVR = '0x4'
    JMP = '0x5'
    JMPZ = '0x6'
    OUT = '0x7'
    SUB = '0x8'
    ADD = '0x9'
    INC = '0xa'
    CLAC = '0xb'
    AND = '0xc'
    OR = '0xd'
    ASHR = '0xe'
    NOT = '0xf'

class Registers():
    def __init__(self):
        self.reg_map = {
            "s" : BitVector(intVal= 0x0, size=1),
            "z" : BitVector(intVal= 0x0, size=1),
            "ir" : BitVector(intVal= 0x0, size=32),
            "ar" : BitVector(intVal= 0x0, size=32),
            "dr" : BitVector(intVal= 0x0, size=32),
            "pc" : BitVector(intVal = 0x0, size=32),
            "outr" : BitVector(intVal = 0x0, size=32),
            "acc" : BitVector(intVal = 0x0, size=32),
            "r" : BitVector(intVal = 0x0, size=32)
        }
    
    def write_reg(self, reg:str, num:int):
        if reg in self.reg_map:
            self.reg_map[reg].set_value(intVal = num, size=self.reg_map[reg].size)

    def read_reg(self, reg:str) -> BitVector:
        if reg in self.reg_map:
            return self.reg_map[reg]

class Memory():
    def __init__(self):
        self.memory_map = {}

    def write(self, addr:int, num: BitVector):
        self.memory_map.update({addr:num})
    
    def read(self, addr:int) -> BitVector:
        return self.memory_map[addr]

class InstructionDef():
    def __init__(self, regs: Registers, mem: Memory):
        self.regs = regs
        self.mem = mem

    def instr_not(self):
        self.regs.reg_map["acc"] = ~self.regs.reg_map["acc"]
    
    def instr_shr(self):
        self.regs.reg_map["acc"].shift_right_by_one()
    
    def instr_or(self):
        self.regs.reg_map["acc"] = self.regs.reg_map["acc"] | self.regs.reg_map["r"]
    
    def instr_and(self):
        self.regs.reg_map["acc"] = self.regs.reg_map["acc"] & self.regs.reg_map["r"]

    def instr_clac(self):
        self.regs.reg_map["acc"].set_value(intVal=0x0, size=32)
    
    def instr_inc(self):
        self.regs.reg_map["acc"].set_value(intVal=self.regs.reg_map["acc"].int_val()+1, size=32)

    def instr_add(self):
        self.regs.reg_map["acc"] += self.regs.reg_map["r"]

    def instr_sub(self):
        self.regs.reg_map["acc"] -= self.regs.reg_map["r"]
    
    def instr_out(self):
        self.regs.reg_map["outr"] = self.regs.reg_map["acc"].deep_copy()
    
    def instr_jmpz(self, operand):
        if int(self.regs.read_reg("z")):
            self.regs.reg_map["pc"].set_value(intVal=operand, size=32)

    def instr_jmp(self, operand):
        self.regs.reg_map["pc"].set_value(intVal=operand, size=32)
    
    def instr_movr(self):
        self.regs.reg_map["acc"] = self.regs.reg_map["r"].deep_copy()
    
    def instr_mvac(self):
        self.regs.reg_map["r"] = self.regs.reg_map["acc"].deep_copy()
    
    def instr_stac(self, operand):
        self.mem.write(operand, self.regs.reg_map["acc"].deep_copy())
    
    def instr_ldac(self, operand):
        self.regs.reg_map["acc"] = self.mem.read(operand).deep_copy()

    def instr_halt(self):
        self.regs.reg_map["s"].set_value(intVal=0x1, size=1)

    def increment_pc(self):
        self.regs.reg_map["pc"].set_value(intVal=self.regs.reg_map["pc"].int_val()+1, size=32)