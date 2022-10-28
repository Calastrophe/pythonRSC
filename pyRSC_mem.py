from pyRSC_def import InstructionSet, Registers, InstructionDef

## THIS FILE IS FOR MEMORY AND DEBUGGER

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

    # Why all the match cases?! These are actually extremely fast, but horrible to look at...
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

# This is taken after GDB debugger
class Debugger():
    def __init__(self, regs: Registers, mem: Memory, instr, sym_table=None):
        self.regs = regs
        self.mem = mem
        self.instr = instr
        self.symbol_table = sym_table ## These are so if say you input
    
    def bp(self, addr):
        return

    def disable(self, index:int):
        return

    def enable(self, index:int):
        return

    def disas(self, begin:int, end:int):
        return

    def stepi(numOfSteps:int=1):
        return

    def print(type: str, reg: str):
        return

    def info(arg:str):
        return
