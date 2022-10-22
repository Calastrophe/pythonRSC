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
            if self.check():
                break
            instr = self.fetch()
            self.decode_execute_tick(instr)
        return

    def check(self):
        return self.regs.read_reg("s").int_val()

    def breakpoint(self):
        #Insert a breakpoint at a given "address"
        return

    def fetch(self):
        return self._instructions[self.regs.read_reg("pc").int_val()]
    
    def decode_execute_tick(self, instr):
        print(instr)
        match instr:
            case InstructionSet.HALT.value:
                self.instr.instr_halt()
                return
            case InstructionSet.LDAC.value: # These instr's have not been tested
                self.instr.instr_ldac()
                return
            case InstructionSet.STAC.value:
                self.instr.instr_stac()
                return
            case InstructionSet.MVAC.value:
                self.instr.instr_mvac()
                return
            case InstructionSet.MOVR.value:
                return
            case InstructionSet.JMP.value:
                return
            case InstructionSet.JMPZ.value:
                return
            case InstructionSet.OUT.value:
                return
            case InstructionSet.SUB.value:
                return
            case InstructionSet.ADD.value:
                self.instr.instr_add()
                self.instr.increment_pc()
                return
            case InstructionSet.INC.value:
                self.instr.instr_inc()
                self.instr.increment_pc()
                return
            case InstructionSet.CLAC.value:
                return
            case InstructionSet.AND.value:
                return
            case InstructionSet.OR.value:
                return
            case InstructionSet.ASHR.value:
                return
            case InstructionSet.NOT.value:
               return 
            case _:
                raise Exception

if __name__ == "__main__":
    rsc = RSC("instr.txt", True)
    rsc.parse()
    rsc.run()