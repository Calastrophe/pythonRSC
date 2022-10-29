# pyRSC ( Relatively Simple Computer in Python )

pyRSC is an emulator for the RSC architecture written in Python.

To download the package, just use pip by ```pip install pythonRSC```

After downloading the package, you can quickly test it by running through the command line.

```pyRSC run tests\avg.txt```

This will emulate the given microcode and expects no other inputs.
As for the assembler, if you wish to create logisim-formatted binaries you can use the assembler command.

```pyRSC assembler tests\avg.txt output.txt```

It is **required** that you give an output filename along with the given input file.
If you wish to use pythonRSC and its libraries, you can easily include them like so.

```py
from pyRSC import RSC


pyRSC = RSC("tests\\avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC._assembler.logisim_format("output.txt") # Logisim-formatted binary output
pyRSC.mem.disasm(0x0, 0x1F) # Disassembly of the given instructions
```

The debug command is currently in development and will be documented in later versions.