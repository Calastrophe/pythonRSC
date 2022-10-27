from typing import List
from pyRSC_def import InstructionSet, Registers, InstructionDef
from pyRSC_assembler import Assembler

class RSC():
    def __init__(self, fn:str):
        self.file = fn
        self._assembler = Assembler(fn)
        self.regs = Registers()
        self.instructions = self._assembler.memory_layout # These instructions are just a memory layout of the program.
        self.instr = InstructionDef(regs=self.regs, mem=self.instructions)
        self._running = True

    def run(self):
        while(self._running):
            if self.halted():
                break
            self.fetch()
            self.instr.check_z() ## We need this explicitly because we don't have a wired connection from ACC to Z.
            self.execute(hex(self.regs.read_reg("ir")))
        self.state()
        return

    def halted(self):
        return self.regs.read_reg("s")

    def fetch(self):
        self.regs.write_reg("ar", self.regs.read_reg("pc"))
        self.regs.write_reg("dr", int(self.instructions[self.regs.read_reg("ar")], base=16))
        self.instr.increment_pc()
        self.regs.write_reg("ir", self.regs.read_reg("dr"))
        self.regs.write_reg("ar", self.regs.read_reg("pc"))

    def state(self):
        for reg_tuple in self.regs.read_all_regs():
            print(reg_tuple)

    def execute(self, instr):
        print(f"The instruction {instr} was executed.")
        match instr:
            case InstructionSet.HALT.value:
                self.instr.instr_halt()
                return
            case InstructionSet.LDAC.value:
                self.instr.instr_ldac()
                return
            case InstructionSet.STAC.value:
                self.instr.instr_stac()
                return
            case InstructionSet.MVAC.value:
                self.instr.instr_mvac()
                return
            case InstructionSet.MOVR.value:
                self.instr.instr_movr()
                return
            case InstructionSet.JMP.value:
                self.instr.instr_jmp()
                return
            case InstructionSet.JMPZ.value:
                self.instr.instr_jmp()
                return
            case InstructionSet.OUT.value:
                self.instr.instr_out()
                return
            case InstructionSet.SUB.value:
                self.instr.instr_sub()
                return
            case InstructionSet.ADD.value:
                self.instr.instr_add()
                return
            case InstructionSet.INC.value:
                self.instr.instr_inc()
                return
            case InstructionSet.CLAC.value:
                self.instr.instr_clac()
                return
            case InstructionSet.AND.value:
                self.instr.instr_and()
                return
            case InstructionSet.OR.value:
                self.instr.instr_or()
                return
            case InstructionSet.ASHR.value:
                self.instr.instr_shr()
                return
            case InstructionSet.NOT.value:
                self.instr.instr_not()
                return 
            case _:
                print(f"There was an attempt to match {instr} but it failed...")
                self.state()
                raise Exception
