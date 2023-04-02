from typing import Dict, Optional, List
import numpy as np
import pyCFG
from .assembler import Assembler
from .classes import Instruction, Register, toReg


class Emulator:
    """ The emulator constructor expects a list of instructions """
    def __init__(self, assembler_obj: Assembler, debug_mode: bool, cfg: str):
        self.memory = Memory(assembler_obj.memory_layout)
        self.regs = Registers()
        self.assembler = assembler_obj
        self.debugger: Optional[Debugger] = Debugger(self) if debug_mode else None
        self.engine = TimelessEngine(self) if debug_mode else None
        self.graph: pyCFG.pyCFG = pyCFG.pyCFG(0) if cfg else None
        self.out_type = cfg if cfg else None


    """ The start function which is executed when 'run' is used """
    def start(self):
        while (not self.halted()):
            if self.debugger:
                self.debugger.query()
            self.cycle()
        self.print_state()
        if self.graph:
            match self.out_type:
                case "png":
                    self.graph.png("control_flow")
                case "pdf":
                    self.graph.pdf("control_flow")

    """ A wrapper function around the fetch, decode, execute cycle. """
    def cycle(self):
        instr : Instruction = self.fetch()
        self.check_z()
        if self.graph:
            self.graph.execute(self.regs[Register.PC]-1, self.match_instruction(instr))
        self.execute(instr)
        if self.engine:
            self.engine.update_state()


    """ Utility function to check if halted or not """
    def halted(self) -> bool:
        return True if self.regs[Register.S] else False

    """ Utility function to check Z or update it """
    def check_z(self) -> bool:
        if self.regs[Register.ACC] == 0:
            self.regs[Register.Z] = 1
            return True
        else:
            self.regs[Register.Z] = 0
            return False

    """ Utility function to print the current state """
    def print_state(self):
        for reg in Register:
            print(f"{reg.name : <4} : {hex(self.regs[reg])}")

    """ Utility function to increment PC register """
    def inc_pc(self):
        self.regs[Register.PC] = self.regs[Register.PC] + 1

    """ The fetch cycle of the emulator """
    def fetch(self) -> Instruction:
        self.regs[Register.AR] = self.regs[Register.PC]
        self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
        self.inc_pc()
        self.regs[Register.IR] = self.regs[Register.DR]
        self.regs[Register.AR] = self.regs[Register.PC]
        return Instruction(self.regs[Register.IR]) # If you are reading this error message, you are somehow reading a non-instruction!
   

    """ The execution cycle of the emulator, matches each given instruction from fetch() """
    def execute(self, instruction: Instruction):
        print(f"The instruction {instruction.name} has been executed.")
        match instruction:
            case Instruction.HALT:
                self._halt()
            case Instruction.LDAC:
                self._ldac()
            case Instruction.STAC:
                self._stac()
            case Instruction.MVAC:
                self._mvac()
            case Instruction.MOVR:
                self._movr()
            case Instruction.JMP:
                self._jmp()
            case Instruction.JMPZ:
                self._jmpz()
            case Instruction.OUT:
                self._out()
            case Instruction.SUB:
                self._sub()
            case Instruction.ADD:
                self._add()
            case Instruction.INC:
                self._inc()
            case Instruction.CLAC:
                self._clac()
            case Instruction.AND:
                self._and()
            case Instruction.OR:
                self._or()
            case Instruction.ASHR:
                self._ashr()
            case Instruction.NOT:
                self._not()

    def _not(self):
        self.regs[Register.ACC] = ~self.regs[Register.ACC]

    def _ashr(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] >> 1

    def _or(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] | self.regs[Register.R]

    def _and(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] & self.regs[Register.R]
    
    def _clac(self):
        self.regs[Register.ACC] = 0

    def _inc(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] + 1

    def _add(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] + self.regs[Register.R]
    
    def _sub(self):
        self.regs[Register.ACC] = self.regs[Register.ACC] - self.regs[Register.R]

    def _out(self):
        self.regs[Register.OUTR] = self.regs[Register.ACC]

    def _jmpz(self):
        if self.check_z():
            self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
            self.regs[Register.PC] = self.regs[Register.DR]
        else:
            self.inc_pc()

    def _jmp(self):
        self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
        self.regs[Register.PC] = self.regs[Register.DR]

    def _movr(self):
        self.regs[Register.ACC] = self.regs[Register.R]
    
    def _mvac(self):
        self.regs[Register.R] = self.regs[Register.ACC]

    def _stac(self):
        self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
        self.inc_pc()
        self.regs[Register.AR] = self.regs[Register.DR]
        self.regs[Register.DR] = self.regs[Register.ACC]
        self.memory[self.regs[Register.AR]] = self.regs[Register.DR]

    def _ldac(self):
        self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
        self.inc_pc()
        self.regs[Register.AR] = self.regs[Register.DR]
        self.regs[Register.DR] = self.memory[self.regs[Register.AR]]
        self.regs[Register.ACC] = self.regs[Register.DR]

    def _halt(self):
        self.regs[Register.S] = 1

    def match_instruction(self, instruction: Instruction) -> pyCFG.Instruction | pyCFG.Jump:
        match instruction:
            case Instruction.JMP:
                return pyCFG.Jump(instruction.name, self.memory[self.regs[Register.AR]], pyCFG.JumpType.JMP)
            case Instruction.JMPZ:
                if self.check_z():
                    return pyCFG.Jump(instruction.name, self.memory[self.regs[Register.AR]], pyCFG.JumpType.JCC_TAKEN, int(self.regs[Register.PC]))
                else:
                    return pyCFG.Jump(instruction.name, self.memory[self.regs[Register.AR]], pyCFG.JumpType.JCC_NOT_TAKEN, int(self.regs[Register.PC]))
            case Instruction.LDAC | Instruction.STAC:
                operand = self.assembler.replaced_instructions[self.regs[Register.PC]] if self.regs[Register.PC] in self.assembler.replaced_instructions.keys() else hex(self.memory[self.regs[Register.AR]])
                return pyCFG.Instruction(instruction.name, operand)
            case _:
                return pyCFG.Instruction(instruction.name)





""" A simple dictionary wrapper to give some sort of illusion of memory. """
class Memory:
    def __init__(self, memory_layout: Dict[int, int]):
        self.memory = memory_layout

    def __getitem__(self, key) -> int:
        return self.memory.setdefault(key, 0)

    def __setitem__(self, key, value):
        self.memory[key] = value



""" A wrapper around an np.array of int32, allows for quick access and modification of contents """
class Registers:
    def __init__(self):
        self.regs = np.zeros(9, dtype=np.uint32)

    def __getitem__(self, register: Register) -> np.uint32:
        return self.regs[register.value]

    def __setitem__(self, register: Register, value: int): # May produce errors
        if register in [Register.S, Register.Z]:
            self.regs[register.value] = value % 2 ** 1
        else:
            self.regs[register.value] = value % 2 ** 32


""" A rather expensive way of tracing each modification to memory and registers. """
""" Another tactic would be to just reverse each instruction's side effect. """
class TimelessEngine:
    def __init__(self, emulator: Emulator):
        self.emulator = emulator
        self.changes: dict[int, tuple[np.ndarray, Optional[dict[int, int]] ]] = {}
        self.previous_reg = emulator.regs.regs.copy()
        self.previous_memory = emulator.memory.memory.copy()
        self.trace: int = 0

    def update_state(self):
        if self.trace not in self.changes:
            potential_difference = dict(set(self.previous_memory.items()) - set(self.emulator.memory.memory.items()))
            difference = potential_difference if potential_difference else None
            self.changes.update( {self.trace : (self.previous_reg, difference)} )
            self.previous_memory = self.emulator.memory.memory.copy()
            self.previous_reg = self.emulator.regs.regs.copy()
        self.trace += 1

    def go_back(self, numOfSteps=1):
        for i in range(0, numOfSteps):
            if (self.trace != 0):
                self.trace -= 1
                self.emulator.regs.regs = self.changes[self.trace][0].copy()
                mem_changes = self.changes[self.trace][1]
                if mem_changes:
                    self.emulator.memory.memory.update(mem_changes)


class Debugger:
    def __init__(self, emulator: Emulator):
        self.emulator = emulator
        self.assembler = self.emulator.assembler
        self._bps: Dict[int | str, bool] = {0: True}
        self._cmd = None


    """ Initalizes a breakpoint from a label or address, if already instantiated or not a label, tell the user and return execution. """
    def bp(self, addr: int | str):
        if isinstance(addr, str) and addr in self.assembler.symbol_table and self.assembler.symbol_table[addr] not in self._bps.keys():
            self._bps[self.assembler.symbol_table[addr]] = True
            print(f" There is now a breakpoint at label {addr}.")
        elif isinstance(addr, int) and addr not in self._bps.keys():
            self._bps[addr] = True
            print(f" There is now a breakpoint at {hex(addr)}.")
        else:
            try:
                print(f" The provided argument {hex(addr)} is already a breakpoint or isn't a valid label.")
            except:
                print(f" The provided argument {addr} is already a breakpoint or isn't a valid label.")

    """ Disables a present breakpoint or tells the user that it is not a present breakpoint. """
    def disable(self, addr):
        if addr in self._bps:
            self._bps[addr] = False
            print(f" The breakpoint at {hex(addr)} is now disabled.")
        elif self.assembler.symbol_table[addr] in self._bps:
            self._bps[self.assembler.symbol_table[addr]] = False
            print(f" The breakpoint at {addr} is now disabled.")
        else:
            try:
                print(f" {hex(addr)} is not a breakpoint.")
            except:
                print(f" {addr} is not a breakpoint.")

    """ Enables a present breakpoint or tells the user that it is not a present breakpoint. """
    def enable(self, addr):
        if addr in self._bps:
            self._bps[addr] = True
            print(f" The breakpoint at {hex(addr)} is now enabled.")
        elif self.assembler.symbol_table[addr] in self._bps:
            self._bps[self.assembler.symbol_table[addr]] = True
            print(f" The breakpoint at {addr} is now enabled.")
        else:
            try:
                print(f" {hex(addr)} is not a breakpoint.")
            except:
                print(f" {addr} is not a breakpoint.")


    """ Steps forward through execution numOfSteps times, if not given the default is one step. """
    def stepi(self, numOfSteps:int=1):
        for i in range(0, numOfSteps):
            if (not self.emulator.halted()):
                self.emulator.cycle()
            else:
                print("The last instruction that was executed was HALT. Debugger exitted.")
                exit()

    """ 
        A wrapper function around disas_rang to allow for dynamic disassembly of given instructions.
        If there is no label, ten instructions will be printed.
        If there is a label behind current PC, but no next label, all instructions will be printed past start label.
        If there is a label behind current PC and a next label, those instructions in start label will be printed till next label.
    """
    def disas_curr(self):
        position = self.emulator.regs[Register.PC]
        start_label: tuple[str, int] = self.determine_label(position)
        # If there is no label behind, print the next ten instructions
        if not start_label:
            self.disas_rang(position, position+10)
            print(" You were not inside a label, the next ten instructions were printed instead.")
            return
        # If there is a label behind, but no next label print till end of instructions
        label_length = self.determine_length(start_label)
        print(f"\n {start_label[0]}:")
        if not label_length:
            length = len(self.assembler.instructions[start_label[1]:])
            end = start_label[1] + length
            self.disas_rang(start_label[1], end)
            return
        # Otherwise print instructions from start label to the end of the label.
        self.disas_rang(start_label[1], start_label[1] + label_length)
    
    """ Determines the start label, returns a tuple of the name of the label and its position in self.instructions """
    def determine_label(self, pos) -> tuple[str, int]:
        potential_labels = [label for label in self.assembler.label_table.keys() if pos >= self.assembler.label_table[label]]
        return (potential_labels[-1], self.assembler.label_table[potential_labels[-1]]) if potential_labels else None

    """ This function finds the next label using the start_label's position """
    def determine_length(self, start_tuple: tuple[str, int]) -> Optional[int]:
        next_labels = [key for (key, value) in self.assembler.label_table.items() if value > start_tuple[1]]
        return self.assembler.label_table[next_labels[0]] - self.assembler.label_table[start_tuple[0]] if next_labels else None
            
    """ Disassembles a range of instructions, if the address is not instantiated yet, we will just output a NOP. ( No operation ) """
    def disas_rang(self, begin:int, end:int):
        for addr in range(begin, end+1):
            if addr in self.emulator.memory.memory.keys():
                if self.emulator.regs[Register.PC] == addr:
                    print("PC--> ", self.convert_addr(addr), "| ", self.match_opcode(addr))
                else:
                    print("      ", self.convert_addr(addr), "| ", self.match_opcode(addr))
            else:
                print("      ", self.convert_addr(addr), "|  NOP")
        return

    """ 
        Matches an address to an instruction or an actual value.
        Replaced instructions are operands or stored variables. ( which could be the same value as an instruction )
    """
    def match_opcode(self, addr):
        if addr in self.assembler.replaced_instructions.keys():
            return hex(self.emulator.memory[addr])
        try:
            return Instruction(self.emulator.memory[addr]).name
        except:
            return hex(self.emulator.memory[addr])

    """ Displays the current state of a given register in a given type. """
    def print(self, type: str, reg: str):
        targetReg: Register | None = toReg(reg.upper())
        if targetReg:
            match type:
                case "/d":
                    print(self.emulator.regs[targetReg])
                    return
                case "/x":
                    print(hex(self.emulator.regs[targetReg]))
                    return
                case "/t":
                    print(bin(self.emulator.regs[targetReg])[2:])
                    return
                case _:
                    print(f" {type} is not a valid type to be displayed.")
        else:
            print(f" {reg} is not a register.")
        return

    """ The large and bulky debug_handler which handles command inputs from the user. """
    def debug_handler(self):
        self._cmd = input(">> ")
        while (self._cmd != "run"):
            command = self._cmd.split(" ")
            arguments = command[1:]
            try:
                match command[0]:
                    case "stepi":
                        arguments.insert(len(arguments), 1) # Nifty way to get around having to handle error
                        self.stepi(int(arguments[0]))
                    case "backi":
                        arguments.insert(len(arguments), 1)
                        self.emulator.engine.go_back(int(arguments[0]))
                    case "bp":
                        if arguments:
                            for argument in arguments:
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
                            self.disas_rang(int(arguments[0], base=16), int(arguments[1], base=16))
                        elif len(arguments) == 0:
                            self.disas_curr()
                        else:
                            print(" Invalid arguments.")
                    case "print":
                        self.print(arguments[0], arguments[1])
                    case "info":
                        self.emulator.print_state()
                    case "help":
                        print(" Potential commands: [stepi|bp|enable|disable|disas|print|info]\n Please refer to documentation for arguments.")
                    case _:
                        print(f"{self._cmd} is not a command.")
            except:
                print(" Invalid arguments.")
            self._cmd = input(">> ")
        return

    """ Queries the breakpoints and checks if debug_handler should be called yet """
    def query(self):
        for breakpoint in self._bps.keys():
            if breakpoint == self.emulator.regs[Register.PC] and self._bps[breakpoint]:
                self.debug_handler()
                break
        return

    """ A pretty-print function to display 32-bit addresses """
    def convert_addr(self, addr) -> str: ## UTIL FUNC
        return "0x"+hex(addr)[2:].zfill(8)