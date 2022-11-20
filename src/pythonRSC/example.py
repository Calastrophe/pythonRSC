from pyRSC import RSC


pyRSC = RSC("avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC.assembler.logisim_format("output.txt") # Logisim-formatted binary output
pyRSC.debugger.disas_rang(0x0, 0x17) # Disassemble a range of instructions
