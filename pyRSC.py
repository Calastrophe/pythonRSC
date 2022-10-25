from typing import List
from pyRSC_def import InstructionSet, Registers, InstructionDef, Memory

class RSC():
    def __init__(self, fn:str, decoded:bool=True):
        self.file = fn
        self.decoded = decoded
        self.regs = Registers()
        self.memory = Memory()
        self.instr = InstructionDef(regs=self.regs, mem=self.memory)
        self._instructions : List[str] = []
        self._running = True
    
    def parse(self):
        if self.decoded:
            with open(self.file, "r") as file:
                line = file.read().strip().replace("\n", "")
                self._instructions = [hex(int(line[i:i+32], 2)) for i in range(0, len(line), 32)]
            return
        else:
            # TODO: Create an assembler to parse microcode and construct binary for us.
            return

    def run(self):
        #Starts the emulation
        while (self._running):
            if self.halted():
                break  
            instr = self.fetch()
            operand = None
            match instr:
                case InstructionSet.JMPZ.value | InstructionSet.JMP.value:
                    self.instr.next_ir()
                    self.instr.increment_pc()
                    operand = int(self.fetch(), base=16)
                case InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    self.instr.next_ir()
                    self.instr.increment_pc()
                    operand = int(self.fetch(), base=16)
                    self.instr.next_ir()
                    self.instr.increment_pc()
                case _:
                    self.instr.next_ir()
                    self.instr.increment_pc()
            self.instr.check_z()
            self.execute(instr, operand)
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

    # This will decode the instruction, execute it, and tick the IR and PC.
    def execute(self, instr, operand=None):
        if operand is not None:
            print(f"The instruction {instr} was executed with address {operand} as target.")
        else:
            print(f"The instruction {instr} was executed.")
        match instr:
            case InstructionSet.HALT.value:
                self.instr.instr_halt()
                return
            case InstructionSet.LDAC.value:
                self.instr.instr_ldac(operand)
                return
            case InstructionSet.STAC.value:
                self.instr.instr_stac(operand)
                return
            case InstructionSet.MVAC.value:
                self.instr.instr_mvac()
                return
            case InstructionSet.MOVR.value:
                self.instr.instr_movr()
                return
            case InstructionSet.JMP.value:
                self.instr.instr_jmp(operand)
                return
            case InstructionSet.JMPZ.value:
                self.instr.instr_jmp(operand)
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