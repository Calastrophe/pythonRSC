from typing import List, Dict
import struct
from .classes import Instruction

class Assembler():
    def __init__(self, fn):
        self.ln = 0
        self.opcodes: List[int | str] = []
        self.symbol_table: Dict[str, int] = {}
        self.label_table: Dict[str, int] = {}
        self.tokenizer(fn)
        self.replaced_instructions = {count : instr for count, instr in enumerate(self.opcodes) if isinstance(instr, str)}
        self.instructions = [self.symbol_table[instruction] if instruction in self.symbol_table.keys() else instruction for instruction in self.opcodes]
        self.memory_layout = {count:instruction for count, instruction in enumerate(self.instructions)}


    """ Each line of the file is delimited by a space and checked if it is empty, then passed to the token parser. """
    def tokenizer(self, fn):
        try:
            with open(fn, 'r') as file:
                for line in file.readlines():
                    self.ln += 1
                    tokens : List[str] = line.strip().replace("\t", ' ').split(' ')
                    if tokens and tokens != ['']:
                        self.parse_tokens(tokens)
        except FileNotFoundError:
            print(f"The file {fn} is not in scope.")
            exit()


    """ Converts a token into an InstructionSet """
    def converter(self, token) -> int:
        for tType in Instruction:
            if tType.name == token:
                return tType.value


    """ Checks if a token is part of InstructionSet or not """
    def checker(self, token) -> bool:
        for tType in Instruction:
            if tType.name == token:
                return True
        return False    


    """ 
        Parses the given tokens from each line and generates a scaffolding of instructions stored in self.opcodes 
        Additionally, the function fills symbol table and label table for later use and replacement.
    """
    def parse_tokens(self, tokens: List[str]):
        t1 : str = tokens[0]
        if self.checker(t1):
            if t1 in ["LDAC", "STAC", "JMP", "JMPZ"]:
                try:
                    self.opcodes.extend([self.converter(t1), tokens[1]]) # Named addresses
                except IndexError:
                    print("Expected an operand for", t1,"at line", self.ln)
                    exit()
            else:
                self.opcodes.append(self.converter(t1))
        elif ':' in t1:
            if len(tokens) > 1 and tokens[1] != '':
                try:
                    self.symbol_table.update({tokens[0][:-1] : len(self.opcodes)})
                    righthand_side: int = int(tokens[1], base=16)
                    self.opcodes.append(righthand_side)
                    if righthand_side < 0:
                        print("Unexpected negative sign before hexadecimal number", tokens[1][1:], "at line", self.ln)
                        exit()
                except ValueError:
                    print("Expected a hexadecimal number after declaration", tokens[0][:-1], "at line", self.ln)
                    exit()
            else:
                self.symbol_table.update({tokens[0][:-1] : len(self.opcodes)})
                self.label_table.update({tokens[0][:-1] : len(self.opcodes)})
        elif ';' in t1:
            pass
        else:
            print("Unknown keyword", t1 ,"used at line", self.ln)
            exit()


    """ Dirty function to output in the binary format for Logisim """
    def logisim_format(self, fn):
        with open(fn, "w") as file:
            file.write("v2.0 raw\n")
            for instruction in self.instructions:
                file.write(hex(instruction)[2:].zfill(8)+"\n")

    """ Binary ninja formatted output """
    " The most amazing refactor happened here, you gotta check the commit history to believe it. "
    def bn_format(self, fn):
        bn_format = [self.symbol_table[instruction]*4 if instruction in self.symbol_table.keys() else instruction for instruction in self.opcodes]
        with open(fn, "wb") as file:
            for instruction in bn_format:
                assert(type(instruction) == int)
                file.write(struct.pack("i", instruction))