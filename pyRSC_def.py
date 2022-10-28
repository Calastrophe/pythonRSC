from enum import Enum
from typing import List, DefaultDict
from BitVector import BitVector



# This is the instruction set of the RSC.
class InstructionSet(Enum):
    HALT = '0x0'
    LDAC = '0x1'
    STAC = '0x2'
    MOVAC = '0x3'
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

# The 'memory' of the RSC, this is a dictionary that uses key's inplace of addresses and values are not restricted to a certain size.
# There is some handling here for debugger output and etc.
class Memory():
    def __init__(self, memory_layout):
        self.mem_map = memory_layout
        self._lastindex = 0
        self._lastopcode = ""

    def __getitem__(self, index) -> int:
        if index in self.mem_map:
            self._lastindex = index
            return int(self.mem_map[index], base=16)

    def __setitem__(self, index, value):
        if index in self.mem_map:
            self.mem_map[index] = hex(value)
            self._lastindex = index

    ## Display a range of instructions, used in debugger.
    def disasm(self, begin : int, end : int):
        for addr in range(begin, end+1):
            try:
                if self.mem_map[addr]:
                    self._lastopcode = self.match_opcode(self.mem_map[addr])
                    if self._lastindex == addr:
                        print("IR--> ", self.convert_addr(addr), "| ", self._lastopcode)
                    else:
                        print("      ", self.convert_addr(addr), "| ", self._lastopcode)
            except KeyError:
                print("      ", self.convert_addr(addr), "|  NOP")

    def convert_addr(self, addr) -> str: ## UTIL FUNC
        return "0x"+hex(addr)[2:].zfill(8)

    def match_opcode(self, value):
        match self._lastopcode:
            case InstructionSet.JMP.name:
                return value
            case InstructionSet.JMPZ.name:
                return value
            case InstructionSet.LDAC.name:
                return value
            case InstructionSet.STAC.name:
                return value
            case _:
                pass
        match value:
            case InstructionSet.NOT.value:
                return InstructionSet.NOT.name
            case InstructionSet.ADD.value:
                return InstructionSet.ADD.name
            case InstructionSet.SUB.value:
                return InstructionSet.SUB.name
            case InstructionSet.LDAC.value:
                return InstructionSet.LDAC.name
            case InstructionSet.STAC.value:
                return InstructionSet.STAC.name
            case InstructionSet.INC.value:
                return InstructionSet.INC.name
            case InstructionSet.JMP.value:
                return InstructionSet.JMP.name
            case InstructionSet.JMPZ.value:
                return InstructionSet.JMPZ.name
            case InstructionSet.OUT.value:
                return InstructionSet.OUT.name
            case InstructionSet.AND.value:
                return InstructionSet.AND.name
            case InstructionSet.OR.value:
                return InstructionSet.OR.name
            case InstructionSet.ASHR.value:
                return InstructionSet.ASHR.name
            case InstructionSet.CLAC.value:
                return InstructionSet.CLAC.name
            case InstructionSet.MOVAC.value:
                return InstructionSet.MOVAC.name
            case InstructionSet.MOVR.value:
                return InstructionSet.MOVR.name
            case InstructionSet.HALT.value:
                return InstructionSet.HALT.name
            case _:
                return value        


# These are the registers of the RSC and their associated functions.
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


# Instruction declaration and definition
class InstructionDef():
    def __init__(self, regs: Registers, mem: DefaultDict):
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
        self.regs.write_reg("acc", 0)

    
    def instr_inc(self):
        self.regs.write_reg("acc", self.regs.read_reg("acc")+1)


    def instr_add(self):
        sum = self.regs.read_reg("acc") + self.regs.read_reg("r")
        self.regs.write_reg("acc", sum)


    def instr_sub(self):
        sum = self.regs.read_reg("acc") - self.regs.read_reg("r")
        self.regs.write_reg("acc", sum)
    

    def instr_out(self):
        self.regs.write_reg("outr", self.regs.read_reg("acc"))


    def instr_jmpz(self):
        if self.regs.read_reg("z"):
            self.regs.write_reg("dr", self.mem[self.regs.read_reg("ar")])
            self.regs.write_reg("pc", self.regs.read_reg("dr"))
        else:
            self.increment_pc()


    def instr_jmp(self):
        self.regs.write_reg("dr", self.mem[self.regs.read_reg("ar")])
        self.regs.write_reg("pc", self.regs.read_reg("dr"))

    
    def instr_movr(self):
        self.regs.write_reg("acc", self.regs.read_reg("r"))

    
    def instr_movac(self):
        self.regs.write_reg("r", self.regs.read_reg("acc"))
    

    def instr_stac(self):
        self.regs.write_reg("dr", self.mem[self.regs.read_reg("ar")])
        self.increment_pc()
        self.regs.write_reg("ar", self.regs.read_reg("dr"))
        self.regs.write_reg("dr", self.regs.read_reg("acc"))
        self.mem.update({self.regs.read_reg("ar"): hex(self.regs.read_reg("dr"))})
    
    def instr_ldac(self):
        self.regs.write_reg("dr", self.mem[self.regs.read_reg("ar")])
        self.increment_pc()
        self.regs.write_reg("ar", self.regs.read_reg("dr"))
        self.regs.write_reg("dr", self.mem[self.regs.read_reg("ar")])
        self.regs.write_reg("acc", self.regs.read_reg("dr"))

    def instr_halt(self):
        self.regs["s"].set_value(intVal=0x1, size=1)

    def increment_pc(self):
        self.regs.write_reg("pc", self.regs.read_reg("pc")+1)

    def check_z(self):
        if self.regs.read_reg("acc") == 0:
            self.regs.reg_map["z"].set_value(intVal=1, size=1)
        else:
            self.regs.reg_map["z"].set_value(intVal=0, size=1)

    