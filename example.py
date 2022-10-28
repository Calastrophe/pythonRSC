from pyRSC import RSC
from pyRSC_assembler import Assembler


pyRSC = RSC("tests\\testing.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
# pyRSC.mem.disasm(0x0, 0xF)
