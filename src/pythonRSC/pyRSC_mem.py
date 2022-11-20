if __name__ == "__main__":
    from pyRSC_def import InstructionSet, Registers, InstructionDef
    from pyRSC_assembler import Assembler
else:
    from .pyRSC_def import InstructionSet, Registers, InstructionDef
    from .pyRSC_assembler import Assembler

## THIS FILE IS FOR MEMORY AND DEBUGGER

# The 'memory' of the RSC, this is a dictionary that uses key's inplace of addresses and values are not restricted to a certain size.
# There is some handling here for debugger output and etc.
class Memory():
    def __init__(self, memory_layout):
        self.mem_map = memory_layout
        self._lastindex = 0

    def __getitem__(self, index) -> int:
        if index in self.mem_map:
            self._lastindex = index
            return self.mem_map[index]

    def __setitem__(self, index, value):
        if index in self.mem_map:
            self.mem_map[index] = value
            self._lastindex = index
  
class Debugger():
    def __init__(self, regs:Registers, mem:Memory, instr:InstructionDef, rsc_object, assembler: Assembler):
        self.regs = regs
        self.mem = mem
        self.instr = instr
        self.symbol_table = assembler.symbol_table
        self.replaced_instructions = assembler.replaced_instructions
        self.label_table = assembler.label_table
        self._breakpoints = {0:True}
        self._command = None
        self._assembler = assembler
        self._parent = rsc_object

    def bp(self, addr):
        if addr not in self._breakpoints:
            if type(addr) is str and addr in self.symbol_table:
                self._breakpoints.update({self.symbol_table[addr]: True})
                print(f" There is now a breakpoint at label {addr}.")
            elif type(addr) is int:
                self._breakpoints.update({addr: True})
                print(f" There is now a breakpoint at {hex(addr)}.")
            else:
                print(f" {addr} is not a label.")
        else:
            print(f" {addr} is already a breakpoint.")
        return

    def disable(self, addr):
        if addr in self._breakpoints:
            self._breakpoints[addr] = False
            print(f" The breakpoint at {addr} is now disabled.")
        else:
            print(f" {addr} is not a breakpoint.")
        return

    def enable(self, addr):
        if addr in self._breakpoints:
            self._breakpoints[addr] = True
            print(f" The breakpoint at {addr} is now enabled.")
        else:
            print(f" {addr} is not a breakpoint.")
        return

    def stepi(self, numOfSteps:int=1):
        for i in range(0, numOfSteps):
            if (not self._parent.halted()):
                self.instr.fetch()
                self.instr.check_z()
                self._parent.execute(self.regs.read_reg("ir"))
            else:
                print("The last instruction that was executed was HALT. Debugger exitted.")
                exit()
        return

    # Internally calls disas_rang, but does figuring for what range is needed for the current label
    def disas_curr(self):
        position = self.regs.read_reg("pc")
        func_label = self.determine_label(position)
        if func_label is None: ## The position is not in a label, so just print the next ten instructions.
            self.disas_rang(position, position+10)
            print(" You are not inside a label, printed next ten instructions instead.")
            return
        label_length = self.determine_length(func_label)
        if label_length is None: ## There is no next-label, but you are inside a label.
            dist_to_halt = len(self._assembler.instructions[self.label_table[func_label]:])
            end = self.label_table[func_label] + dist_to_halt
            print(f"\n {func_label}:")
            self.disas_rang(self.label_table[func_label], end)
            return
        print(f"\n {func_label}:")
        self.disas_rang(self.label_table[func_label], (self.label_table[func_label] + label_length)-1)
        return
    
    # This is a VERY lazy way to figure out what label to use, needs reworking.
    def determine_label(self, pos):
        potential_labels = []
        for label in self.label_table:
            if pos >= self.label_table[label]:
                potential_labels.append(label)
        if potential_labels:
            return potential_labels[-1]
        return None

    def determine_length(self, label):
        label_list = list(self.label_table.items())
        cut_index = label_list.index((label, self.label_table[label]))
        label_list = label_list[cut_index+1:] ## We want to cut out even our own label, and just determine the next_label and see if anything exists.
        if label_list:
            next_label = label_list[0][0]
            return self.label_table[next_label] - self.label_table[label]
        else:
            return None
            
    def disas_rang(self, begin:int, end:int):
        for addr in range(begin, end+1):
            try:
                if self.mem.mem_map[addr]:
                    if self.regs.read_reg('pc') == addr:
                        print("PC--> ", self.convert_addr(addr), "| ", self.match_opcode(addr))
                    else:
                        print("      ", self.convert_addr(addr), "| ", self.match_opcode(addr))
            except KeyError:
                print("      ", self.convert_addr(addr), "|  NOP")
        return

    ## TODO: POTENTIAL REFACTOR WITH SUBSUMING INSTRUCTIONSET() WRAPPING
    def match_opcode(self, addr):
        if addr in self.replaced_instructions:
            return hex(self.mem.mem_map[addr])
        try:
            return InstructionSet(self.mem.mem_map[addr]).name
        except:
            return hex(self.mem.mem_map[addr])

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
                case _:
                    print(f" {type} is not a valid type to be displayed.")
        else:
            print(f" {reg} is not a register.")
        return

    def check(self):
        for breakpoint in self._breakpoints:
            if breakpoint == self.regs.read_reg("pc") and self._breakpoints[breakpoint]:
                self.debug_handler()
                break
        return

    def convert_addr(self, addr) -> str: ## UTIL FUNC
        return "0x"+hex(addr)[2:].zfill(8)

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
                case "enable":
                    if arguments:
                        for argument in arguments:
                            try:
                                self.enable(int(argument, base=16))
                            except:
                                self.enable(argument)
                    else:
                        print(" Invalid arguments.")
                case "disable":
                    if arguments:
                        for argument in arguments:
                            try:
                                self.disable(int(argument, base=16))
                            except:
                                self.disable(argument)
                    else:
                        print(" Invalid arguments.")
                case "disas":
                    if len(arguments) == 2:
                        try:
                            self.disas_rang(int(arguments[0]), int(arguments[1]))
                        except:
                            try:
                                self.disas_rang(int(arguments[0], base=16), int(arguments[1], base=16))
                            except:
                                print(" Invalid arguments.")
                    elif len(arguments) == 0:
                        self.disas_curr()
                    else:
                        print("Invalid arguments.")
                case "print":
                    try:
                        self.print(arguments[0], arguments[1])
                    except:
                        print(" Invalid arguments.")
                case "info":
                    self._parent.state()
                case "help":
                    print(" Potential commands: [stepi|bp|enable|disable|disas|print|info]\n Please refer to documentation for arguments.")
                case _:
                    print(f"{self._command} is not a command.")
            self._command = input(">> ")
        return