from pyRSC_def import InstructionSet
from typing import List
import re

## IMP
class Assembler():
    def __init__(self, fn, output=None):
        self.fn = fn
        self.output = output if output is not None else fn
        self.memory_layout = {}
        self._instructions = []
        self._symbol_table = {}
        self._matches = []
        self._regex_dict = { ## These are not very good regex expressions, but they get the job done. Feel free to change.
            InstructionSet.HALT.value : "HALT",
            InstructionSet.LDAC.value : "LDAC ([^\s]+)",
            InstructionSet.STAC.value: "STAC ([^\s]+)",
            InstructionSet.MVAC.value: "MOVAC",
            InstructionSet.MOVR.value: "MOVR",
            InstructionSet.JMP.value: "JMP ([^\s]+)",
            InstructionSet.JMPZ.value: "JMPZ ([^\s]+)",
            InstructionSet.OUT.value: "OUT",
            InstructionSet.SUB.value: "SUB",
            InstructionSet.ADD.value: "ADD",
            InstructionSet.INC.value: "INC",
            InstructionSet.CLAC.value: "CLAC",
            InstructionSet.AND.value: "AND",
            InstructionSet.OR.value: "OR",
            InstructionSet.ASHR.value: "ASHR",
            InstructionSet.NOT.value: "NOT",
            "VARIABLE_ASSIGNMENT": "([^\s]+): ([^\s]+)",
            "LABEL": "([^\s]+):"
        }
        self.obtain_matches()
        self.parse_symbols()
        self.construct_instructions()
        self.replace_symbols()
        self.construct_mem()

    def obtain_matches(self):
        with open(self.fn, "r") as file:
            for line in file.readlines():
                if line != '\n':
                    match = self.match_line(line.strip())
                    if match is not None:
                        self._matches.append(match[0])

    ## Parse the symbols and construct indices for those symbols that will end up in the instructions.
    ## JMP,JMPZ,LDAC,STAC are all "8-byte wide", essentially meaning they take up two instructions to perform, so +2 index.
    def parse_symbols(self):
        index = 0
        for match in self._matches:
            match match[0]:
                case "VARIABLE_ASSIGNMENT":
                    split_match = match[1].split(":")
                    self._symbol_table.update({index: split_match[1].strip()})
                    self._symbol_table.update({split_match[0]: index})
                    index += 1
                    pass
                case "LABEL":
                    split_match = match[1].split(":")
                    self._symbol_table.update({split_match[0]: index})
                    pass
                case InstructionSet.JMP.value | InstructionSet.JMPZ.value | InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    index += 2
                    pass
                case _:
                    index += 1
                    pass

    # Scaffold out the instructions with labels and variables still not changed.
    def construct_instructions(self):
        for match in self._matches:
            match match[0]:
                case "LABEL":
                    pass
                case InstructionSet.JMP.value | InstructionSet.JMPZ.value | InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    self._instructions.extend([match[0], match[1].split(" ")[1]]) ## We need to add the operand as the next instruction
                case _:
                    self._instructions.append(match[0])

    # Replace the stored indices with their values and replace symbols with their respective indices
    def replace_symbols(self):
        for count, instruction in enumerate(self._instructions): # Attach a number to each instruction
            if count in self._symbol_table: # If that number is stored in our symbol table, replace that instruction with the stored one.
                self._instructions[count] = hex(int(self._symbol_table[count], base=16))
            elif instruction in self._symbol_table:
                self._instructions[count] = hex(self._symbol_table[instruction])


    # Take the instructions and lay them out in memory form to be modified by ldac, stac opcodes.
    def construct_mem(self):
        for count, instruction in enumerate(self._instructions):
            self.memory_layout.update({count: instruction})

    # Dirty function to output in the binary format for Logisim
    def logisim_format(self, fn):
        with open(fn, "w") as file:
            for instruction in self._instructions:
                file.write((bin(int(instruction, base=16))[2:].zfill(32)) + "\n")

    def match_line(self, line) -> List[tuple]:
        matches : List[re.Match] = []
        for item in self._regex_dict.items():
            regex_str = item[1]
            out = re.match(regex_str, line)
            if out is not None:
                matches.append((item[0],out.group(0)))
        return matches if len(matches) > 0 else None
    
    