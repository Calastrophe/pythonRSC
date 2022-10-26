from pyRSC_def import InstructionSet
from typing import List
import re

## IMP
class Assembler():
    def __init__(self, fn, output=None):
        self.fn = fn
        self.output = output if output is not None else fn
        self.instructions = []
        self._index = 0
        self.symbol_table = {}
        self._match_list = []
        self.memory_layout = {}
        self.regex_dict = { ## These are not very good regex expressions, but they get the job done. Feel free to change.
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
        self.scaffold()
        self.replace_symbols()
        self.create_memory()

    def obtain_matches(self):
        with open(self.fn, "r") as file:
            for line in file.readlines():
                if line != '\n':
                    match = self.match_line(line.strip())
                    if match is not None:
                        self._match_list.append(match[0])

    def scaffold(self):
        for count, _match in enumerate(self._match_list):
            match _match[0]:
                case "VARIABLE_ASSIGNMENT":
                    split_match = _match[1].split(":")
                    self.instructions.append(split_match[1].strip())
                    self.symbol_table.update({split_match[0].strip(): count+1})
                    pass
                case "LABEL":
                    split_match = _match[1].split(":")
                    self.symbol_table.update({split_match[0].strip(): count+1})
                    pass
                case InstructionSet.JMP.value | InstructionSet.JMPZ.value | InstructionSet.LDAC.value | InstructionSet.STAC.value:
                    split_match = _match[1].split(" ")
                    self.instructions.append(_match[0])
                    self.instructions.append(split_match[1].strip())
                    pass
                case _:
                    self.instructions.append(_match[0])
                    pass
    
    def replace_symbols(self):
        for count, instr in enumerate(self.instructions):
            if instr in self.symbol_table.keys():
                self.instructions[count] = hex(self.symbol_table[instr])
        return
    
    def create_memory(self):
        for count, instr in enumerate(self.instructions):
            self.memory_layout.update({count: instr})



    def match_line(self, line) -> List[tuple]:
        # re.match( "|".join( self.dict_items() ), line)    This is extremely useful but not exactly easy to use and we don't call .match() a bunch of times.
        matches : List[re.Match] = []
        for item in self.regex_dict.items():
            regex_str = item[1]
            out = re.match(regex_str, line)
            if out is not None:
                matches.append((item[0],out.group(0)))
        return matches if len(matches) > 0 else None



if __name__ == "__main__":
    tester_obj = Assembler("tests\\microcode\\tester.txt")
    print(tester_obj.instructions)
    print(tester_obj.memory_layout)
    
    