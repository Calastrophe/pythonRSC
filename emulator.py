from typing import List
from emulator_def import InstructionSet, Registers, InstructionDef, Memory

class RSC():
    def __init__(self, fn:str, decoded:bool):
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
            #Parse the file from given source, create instructions
            return

    def run(self):
        #Starts the emulation
        while (self._running):
            if self.halted():
                break  
            instr = self.fetch()
            match instr:
                case InstructionSet.JMPZ.value | InstructionSet.JMP.value | InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    self.instr.increment_pc()
                    operand = self.fetch()
                case _:
                    pass
            self.decode_execute_tick(instr)
        return

    def halted(self):
        return self.regs.read_reg("s").int_val()

    # TODO: Implement a debugger with breakpoints, with a nice command line interface.
    # def set_breakpoint(self, index):
    #     if len(self._instructions-1 >= index > 0):
    #         self._instructions[index] = InstructionSet.BREAKPOINT.value
    #     else:
    #         print("You attempted to insert a breakpoint on an invalid instruction location.")
    #     return

    def fetch(self):
        return self._instructions[self.regs.read_reg("pc").int_val()]
    
    def decode_execute_tick(self, instr, operand=None):
        print(instr)
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
                raise Exception

if __name__ == "__main__":
    rsc = RSC("instr.txt", True)
    rsc.parse()
    rsc.run()