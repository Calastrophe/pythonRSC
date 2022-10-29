from pyRSC import RSC
from pyRSC_assembler import Assembler


pyRSC = RSC("tests\\avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC._assembler.logisim_format("output.txt")
pyRSC.mem.disasm(0x0, 0x1F)