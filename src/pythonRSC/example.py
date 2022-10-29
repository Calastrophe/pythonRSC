from pythonRSC import pyRSC


pyRSC = pyRSC.RSC("..\\tests\\avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC._assembler.logisim_format("output.txt") # Logisim-formatted binary output
pyRSC.mem.disasm(0x0, 0x1F) # Disassembly of the given instructions
