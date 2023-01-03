from typing import List, Dict
if __name__ == "__main__":
    from enumtypes import Instruction
else:
    from .enumtypes import Instruction

class Assembler():
    def __init__(self, fn):
        self.ln = 0
        self.instructions: List[int | str] = []
        self.symbol_table: Dict[str, int] = {}
        self.label_table: Dict[str, int] = {}
        self.tokenizer(fn)
        self.replaced_instructions = [count for count, instr in enumerate(self.instructions) if isinstance(instr, str)]
        self.instructions = [self.symbol_table[instruction] if instruction in self.symbol_table.keys() else instruction for instruction in self.instructions]
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
        Parses the given tokens from each line and generates a scaffolding of instructions stored in self.instructions 
        Additionally, the function fills symbol table and label table for later use and replacement.
    """
    def parse_tokens(self, tokens: List[str]):
        t1 : str = tokens[0]
        if self.checker(t1):
            if t1 in ["LDAC", "STAC", "JMP", "JMPZ"]:
                try:
                    self.instructions.extend([self.converter(t1), int(tokens[1], 16)]) # Allows for operands to just be addresses
                except ValueError:
                    try:
                        self.instructions.extend([self.converter(t1), tokens[1]]) # Named addresses
                    except IndexError:
                        print("Expected an operand for", t1,"at line", self.ln)
                        exit()
            else:
                self.instructions.append(self.converter(t1))
        elif ':' in t1:
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