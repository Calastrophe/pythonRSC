# pythonRSC ( Relatively Simple Computer in Python )

pythonRSC is an emulator for the RSC architecture written in Python.

# Installing pythonRSC
To download the package, you can simply use pip.

``pip install pythonRSC``

# Emulating your microcode using pythonRSC
After downloading the package, you should have access to a command called 'pythonRSC'.
To use this command, you will need a microcode file to emulate. There are some test files provided.

``pythonRSC run microcode.txt``

This will parse the given microcode and output the state at the end of emulation.

# Generating bytecode using pythonRSC
If you desire to use the in-built assembler to parse the microcode into logisim bytecode, there is a command for that. You will need to provide a microcode input file and it **requires** an output file to function.

``pythonRSC assembler microcode.txt output.txt``

# Using pythonRSC as a library
To use this package as a library, you can simply import the classes that you wish to use in your own python code. To learn more about those classes, you will need to read the source code. 

This example shows some in-built functions inside of pythonRSC that you could use.
```py
from pythonRSC import RSC


pyRSC = RSC("tests\\avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC._assembler.logisim_format("output.txt") # Logisim-formatted binary output
pyRSC.mem.disasm(0x0, 0x1F) # Disassembly of the given instructions
```

# Emulating and debugging your microcode using pythonRSC

The debugger is still currently in later stages of development.

Breakpoints, instruction disassembly, and control flow will all be at your fingertips soon enough.