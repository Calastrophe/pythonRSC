from pyRSC import RSC
from pyRSC_assembler import Assembler

if __name__ == "__main__":
    pyRSC = RSC("tests\\testing.txt")
    pyRSC.run() # Runs the given instructions and gives you an output!
    pyRSC._assembler.logisim_format("output.txt") ## You can reference the assembler like this.

    pyAssembler = Assembler("tests\\testing.txt") # Or you can import and reference like this
    pyAssembler.logisim_format("output2.txt")