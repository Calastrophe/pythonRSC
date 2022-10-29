from typing import List
import sys
from pyRSC_def import InstructionSet, Registers, InstructionDef
from pyRSC_mem import Memory, Debugger
from pyRSC_assembler import Assembler

class RSC():
    def __init__(self, fn:str, debug=False):
        self.file = fn
        self._assembler = Assembler(fn)
        self.regs = Registers()
        self.mem = Memory(self._assembler.memory_layout) # Instructions are stored in pseudo-memory.
        self.instr = InstructionDef(regs=self.regs, mem=self.mem)
        self.debugger = Debugger(self.regs, self.mem, self.instr, self._assembler._symbol_table)
        self._debug = debug
        self._running = True

    def run(self):
        while(not self.halted()):
            if self._debug:
                raise NotImplementedError
                self.debugger.check()
                self.tick()
            else:
                self.tick()
        self.state()
        return

    def tick(self):
        self.instr.fetch()
        self.instr.check_z() ## We need this explicitly because we don't have a wired connection from ACC to Z.
        self.execute(hex(self.regs.read_reg("ir")))


    def halted(self):
        return self.regs.read_reg("s")

    def state(self):
        for reg_tuple in self.regs.read_all_regs():
            print(reg_tuple)

    def execute(self, instr):
        print(f"The instruction {self.mem.match_opcode(instr)} was executed.") # may be fudged by self._lastopcode in memory
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
            case InstructionSet.MOVAC.value:
                self.instr.instr_movac()
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


if __name__ == "__main__":
    args = sys.argv[1:]
    if args:
        match args[0]:
            case "assembler":
                asm = Assembler(args[1])
                asm.logisim_format(args[2])
            case "run":
                pyRSC = RSC(args[1])
                pyRSC.run()
            case "debug":
                pyRSC = RSC(args[1], debug=True)
                pyRSC.run()
            case "help":
                print("usage: pyRSC [run|assembler] [in] [out]\nThe assembler is used for producing logisim-formatted binaries of your microcode.\nThe run command is to emulate your given microcode file, output is not needed.")
            case _:
                print("usage: pyRSC [run|assembler] [in] [out]")
    else:
        print("usage: pyRSC [run|assembler] [in] [out]\nThe assembler is used for producing just logisim input.\nThe run command is to emulate your given microcode file, output is not needed.")
