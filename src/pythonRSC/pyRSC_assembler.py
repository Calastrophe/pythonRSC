if __name__ == "__main__":
    from pyRSC_def import InstructionSet
else:
    from .pyRSC_def import InstructionSet
from typing import List
from enum import Enum

class Assembler():
    def __init__(self, fn):
        self.fn = fn
        self.ln = 0
        self.instructions = []
        self.symbol_table = {}
        self.label_table = {}
        self.initalizer()
        self.replaced_instructions = [count for count, instr in enumerate(self.instructions) if isinstance(instr, str)]
        self.instructions = [self.symbol_table[instruction] if instruction in self.symbol_table.keys() else instruction for instruction in self.instructions]
        self.memory_layout = {count:instruction for count, instruction in enumerate(self.instructions)}


    def initalizer(self):
        try:
            with open(self.fn, "r") as file:
                for line in file.readlines():
                    self.ln += 1
                    stripped_line = line.strip()
                    if stripped_line:
                        tokens : List[str] = stripped_line.replace("\t", ' ').split(' ')
                        try:
                            tokens = tokens[:tokens.index(";")]
                        except:
                            pass
                        if tokens:
                            self.parse_tokens(tokens)
        except FileNotFoundError:
            print(f"You do not have a file with the name {self.fn} in scope.")
            exit()

    def converter(self, token) -> int:
        for tType in InstructionSet:
            if tType.name == token:
                return tType.value

    def checker(self, token) -> bool:
        for tType in InstructionSet:
            if tType.name == token:
                return True
        return False

    ## TODO: REFACTOR???
    def parse_tokens(self, tokens : List[str]):
        first_token = tokens[0]
        if (self.checker(first_token)):
            if first_token in ["LDAC", "STAC", "JMP", "JMPZ"]:
                try:
                    self.instructions.extend([self.converter(first_token), tokens[1]])
                except IndexError:
                    print("Invalid operand for", first_token,"at line", self.ln)
                    exit()
            else:
                self.instructions.append(self.converter(first_token))
        elif ':' in tokens[0]:
            if len(tokens) > 1 and tokens[1] != '':
                try:
                    self.symbol_table.update({tokens[0][:-1] : len(self.instructions)})
                    self.instructions.append(int(tokens[1], base=16))
                except ValueError:
                    print("Expected a hexadecimal number after declaration", tokens[0][:-1], "at line", self.ln)
                    exit()
            else:
                self.symbol_table.update({tokens[0][:-1] : len(self.instructions)})
                self.label_table.update({tokens[0][:-1] : len(self.instructions)})
        else:
            print("Unknown keyword", tokens[0] ,"used at line", self.ln)
            exit()

    # Dirty function to output in the binary format for Logisim
    def logisim_format(self, fn):
        with open(fn, "w") as file:
            file.write("v2.0 raw\n")
            for instruction in self.instructions:
                file.write(hex(instruction)[2:].zfill(8)+"\n")
