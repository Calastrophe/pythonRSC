from typing import List
from pyRSC_def import InstructionSet, Registers, InstructionDef
from pyRSC_assembler import Assembler

class RSC():
    def __init__(self, fn:str, decoded:bool=True):
        self.file = fn
        self.decoded = decoded
        self.regs = Registers()
        self._assembler = Assembler(fn)
        self._instructions : List[str] = self._assembler.instructions
        self._memory_layout = self._assembler.memory_layout
        self.instr = InstructionDef(regs=self.regs, mem=self._memory_layout, instructions=self._instructions)
        self._running = True

    def run(self):
        #Starts the emulation
        while (self._running):
            if self.halted():
                break  
            instr = self.fetch()
            match instr:
                case InstructionSet.JMPZ.value | InstructionSet.JMP.value:
                    self.instr.next_ir()
                    self.instr.increment_pc()
                    operand = int(self.fetch(), base=16)
                    self.regs.write_reg("dr", operand)
                case InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    self.instr.next_ir()
                    self.instr.increment_pc()
                    operand = int(self.fetch(), base=16)
                    self.regs.write_reg("dr", operand)
                    self.instr.next_ir()
                    self.instr.increment_pc()
                case _:
                    self.instr.next_ir()
                    self.instr.increment_pc()
            self.instr.check_z()
            self.execute(instr)
        self.state() # It will print the resultant state of the emulator.
        return

    def halted(self):
        return self.regs.read_reg("s")

    # TODO: Implement a debugger with breakpoints, with a nice command line interface.
    # def set_breakpoint(self, index):
    #     if len(self._instructions-1 >= index > 0):
    #         self._instructions[index] = InstructionSet.BREAKPOINT.value
    #     else:
    #         print("You attempted to insert a breakpoint on an invalid instruction location.")
    #     return

    def fetch(self):
        return self._instructions[self.regs.read_reg("ir")]

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


# You can uncomment this to directly test without having to use the library.
# if __name__ == "__main__":
#     rsc = RSC("clear&out_test.txt", True)
#     rsc.parse()
#     rsc.run()