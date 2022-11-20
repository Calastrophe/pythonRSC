from typing import List
if __name__ == "__main__":
    from pyRSC_def import InstructionSet, Registers, InstructionDef
    from pyRSC_mem import Memory, Debugger
    from pyRSC_assembler import Assembler
else:
    from .pyRSC_def import InstructionSet, Registers, InstructionDef
    from .pyRSC_mem import Memory, Debugger
    from .pyRSC_assembler import Assembler


class RSC():
    def __init__(self, fn:str, debug=False):
        self.file = fn
        self.assembler = Assembler(fn)
        self.regs = Registers()
        self.mem = Memory(self.assembler.memory_layout) # Instructions are stored in pseudo-memory.
        self.instr = InstructionDef(regs=self.regs, mem=self.mem)
        self.debugger = Debugger(self.regs, self.mem, self.instr, self, self.assembler)
        self._debug = debug

    def run(self):
        while(not self.halted()):
            if self._debug:
                self.debugger.check()
                self.tick()
            else:
                self.tick()
        self.state()
        return

    def tick(self):
        self.instr.check_z() ## We need this explicitly because we don't have a wired connection from ACC to Z.
        self.instr.fetch()
        self.execute(self.regs.read_reg("ir"))


    def halted(self):
        return self.regs.read_reg("s")

    def state(self):
        for reg_tuple in self.regs.read_all_regs():
            print(f" {reg_tuple[0].upper() : <4} : {reg_tuple[1]}")

    def execute(self, instr):
        print(f"The instruction {InstructionSet(instr).name} was executed.")
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
                self.instr.instr_jmpz()
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
