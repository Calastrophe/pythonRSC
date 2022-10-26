from pyRSC_def import InstructionSet
from typing import List
import re

class Assembler():
    def __init__(self, fn, output=None):
        self.fn = fn
        self.output = output if output is not None else fn
        self.instructions = []
        self.symbol_table = {}
        self.regex_dict = {
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

    def parse(self):
        with open(self.fn, "r") as file:
            for line in file.readlines():
                if line != '\n':
                    match = self.match_line(line.strip())
                    if match is not None:
                        #Produce symbols
                        print(match)
                        

    def match_line(self, line) -> List[tuple]:
        # re.match( "|".join( self.dict_items() ), line)    This is extremely useful but not exactly easy to use and we don't call .match() a bunch of times.
        matches : List[re.Match] = []
        for item in self.regex_dict.items():
            regex_str = item[1]
            out = re.match(regex_str, line)
            if out is not None:
                matches.append((item[0],out))
        return matches if matches else None



if __name__ == "__main__":
    tester_obj = Assembler("tests\\microcode\\tester.rsc")
    tester_obj.parse()
    print(tester_obj.instructions)