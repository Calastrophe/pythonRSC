from enum import Enum
from typing import List
from BitVector import BitVector

class InstructionSet(Enum):
    HALT = '0x0' ## This is being used as a 'breakpoint' in the emulator.
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
            "pc" : BitVector(intVal = 0x1, size=32),
            "outr" : BitVector(intVal = 0x0, size=32),
            "acc" : BitVector(intVal = 0x0, size=32),
            "r" : BitVector(intVal = 0x0, size=32)
        }
    
    def __getitem__(self, index) -> BitVector:
        if index in self.reg_map:
            return self.reg_map[index]

    def write_reg(self, reg:str, num:int):
        if reg in self.reg_map:
            self.reg_map[reg].set_value(intVal = num, size=self.reg_map[reg].size)

    def read_reg(self, reg:str) -> int:
        if reg in self.reg_map:
            return self.reg_map[reg].int_val()
        
    def read_all_regs(self):
        for reg in self.reg_map:
            yield (reg, self.reg_map[reg].int_val())


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
        self.regs["acc"] = ~self.regs.reg_map["acc"]
        self.next_ir()
        self.increment_pc()
    
    def instr_shr(self):
        self.regs["acc"].shift_right_by_one()
        self.next_ir()
        self.increment_pc()
    
    def instr_or(self):
        self.regs["acc"] = self.regs["acc"] | self.regs["r"]
        self.next_ir()
        self.increment_pc()
    
    def instr_and(self):
        self.regs["acc"] = self.regs["acc"] & self.regs["r"]
        self.next_ir()
        self.increment_pc()

    def instr_clac(self):
        self.regs["acc"].set_value(intVal=0x0, size=32)
        self.next_ir()
        self.increment_pc()
    
    def instr_inc(self):
        self.regs["acc"].set_value(intVal=self.regs["acc"].int_val()+1, size=32)
        self.next_ir()
        self.increment_pc()

    def instr_add(self):
        self.regs["acc"] += self.regs["r"]
        self.next_ir()
        self.increment_pc()

    def instr_sub(self):
        self.regs["acc"] -= self.regs["r"]
        self.next_ir()
        self.increment_pc()
    
    def instr_out(self):
        self.regs["outr"] = self.regs["acc"].deep_copy()
        self.next_ir()
        self.increment_pc()
    
    def instr_jmpz(self, operand):
        if self.regs.read_reg("z"):
            self.set_pc(operand)
        self.next_ir()
        self.increment_pc()

    def instr_jmp(self, operand):
        self.set_pc(operand)
        self.next_ir()
        self.increment_pc()
    
    def instr_movr(self):
        self.regs["acc"] = self.regs["r"].deep_copy()
        self.next_ir()
        self.increment_pc()
    
    def instr_mvac(self):
        self.regs["r"] = self.regs["acc"].deep_copy()
        self.next_ir()
        self.increment_pc()
    
    def instr_stac(self, operand):
        self.mem.write(operand, self.regs["acc"].deep_copy())
        self.next_ir()
        self.increment_pc()
    
    def instr_ldac(self, operand):
        self.regs["acc"] = self.mem.read(operand).deep_copy()
        self.next_ir()
        self.increment_pc()

    def instr_halt(self):
        self.regs["s"].set_value(intVal=0x1, size=1)

    def increment_pc(self):
        self.regs["pc"].set_value(intVal=self.regs["pc"].int_val()+1, size=32)

    def next_ir(self):
        self.regs["ir"].set_value(intVal=self.regs.read_reg("pc"), size=32)

    def set_pc(self, addr:int):
        self.regs["pc"].set_value(intVal=addr, size=32)

    def set_ir(self, addr:int):
        self.regs["ir"].set_value(intVal=addr, size=32)

    