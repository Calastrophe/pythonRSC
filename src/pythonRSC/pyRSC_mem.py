if __name__ == "__main__":
    from pyRSC_def import InstructionSet, Registers, InstructionDef
else:
    from .pyRSC_def import InstructionSet, Registers, InstructionDef

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

class Debugger():
    def __init__(self, regs:Registers, mem:Memory, instr:InstructionDef, symbol_table, rsc_object):
        self.regs = regs
        self.mem = mem
        self.instr = instr
        self.symbol_table = symbol_table ## These are for if you want to breakpoint at certain labels.
        self._breakpoints = {0:True}
        self._command = None
        self._parent = rsc_object

    def bp(self, addr):
        if addr not in self._breakpoints:
            if type(addr) is str and addr in self.symbol_table:
                self._breakpoints.update({self.symbol_table[addr]: True})
            elif type(addr) is int:
                self._breakpoints.update({addr: True})
            else:
                print(f" {addr} is not a label.")
        else:
            print(f" {addr} is already a breakpoint.")
        return

    def disable(self, addr):
        if addr in self._breakpoints:
            self._breakpoints[addr] = False
        else:
            print(f" {addr} is not a breakpoint.")
        return

    def enable(self, addr):
        if addr in self._breakpoints:
            self._breakpoints[addr] = True
        else:
            print(f" {addr} is not a breakpoint.")
        return

    def disas(self, begin:int, end:int):
        self.mem.disasm(begin, end)
        return

    def stepi(self, numOfSteps:int=1):
        for i in range(0, numOfSteps):
            self.instr.fetch()
            self.instr.check_z()
            self._parent.execute(hex(self.regs.read_reg("ir")))
        return

    def print(self, type: str, reg: str):
        if reg in self.regs.reg_map:
            match type:
                case "/d":
                    print(self.regs.read_reg(reg))
                    return
                case "/x":
                    print(hex(self.regs.read_reg(reg)))
                    return
                case "/t":
                    print(bin(self.regs.read_reg(reg))[2:])
                    return
        else:
            print(f" {reg} is not a register.")
        return

    def state(self):
        self._parent.state()
        return

    def check(self):
        for breakpoint in self._breakpoints:
            if breakpoint == self.regs.read_reg("pc") and self._breakpoints[breakpoint]:
                self.debug_handler()
                break
        return

    def debug_handler(self):
        self._command = input(">> ")
        while (self._command != "run"):
            command = self._command.split(" ")
            arguments = command[1:]
            match command[0]:
                case "stepi":
                    try:
                        self.stepi(int(arguments[0]))
                    except:
                        try:
                            self.stepi(int(arguments[0], base=16))
                        except IndexError:
                            self.stepi()
                        except:
                            print(" Invalid arguments.")
                    pass
                case "bp":
                    if arguments:
                        for argument in arguments:
                            try:
                                self.bp(int(argument))
                            except:
                                try:
                                    self.bp(int(argument, base=16))
                                except:
                                    self.bp(argument)
                    else:
                        print(" Invalid arguments.")
                    pass
                case "enable":
                    if arguments:
                        for argument in arguments:
                            try:
                                self.enable(int(argument, base=16))
                            except:
                                self.enable(argument)
                    else:
                        print(" Invalid arguments.")
                    pass
                case "disable":
                    if arguments:
                        for argument in arguments:
                            try:
                                self.disable(int(argument, base=16))
                            except:
                                self.disable(argument)
                    else:
                        print(" Invalid arguments.")
                    pass
                case "disas":
                    try:
                        self.disas(int(arguments[0]), int(arguments[1]))
                    except:
                        try:
                            self.disas(int(arguments[0], base=16), int(arguments[1], base=16))
                        except:
                            print(" Invalid arguments.")
                    pass
                case "print":
                    try:
                        self.print(arguments[0], arguments[1])
                    except:
                        print(" Invalid arguments.")
                    pass
                case "info":
                    self.state()
                    pass
                case "help":
                    print(" Potential commands: [stepi|bp|enable|disable|disas|print|info]\n Please refer to documentation for arguments.")
                    pass
                case _:
                    print(f"{self._command} is not a command.")
                    pass
            self._command = input(">> ")
        return