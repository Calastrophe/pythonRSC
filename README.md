# pythonRSC ( Relatively Simple Computer in Python )

pythonRSC is an emulator for the RSC architecture written in Python.


With this program, you can generate bytecode from microcode files for Logisim or just emulate microcode files out right.
Soon, you will be able to debug and follow control flow within the emulator and efficiently find bugs.

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


pyRSC = pyRSC.RSC("tests\\avg.txt")
pyRSC.run() # Runs the given instructions and gives you an output!
pyRSC._assembler.logisim_format("output.txt") # Logisim-formatted binary output
pyRSC.mem.disasm(0x0, 0x1F) # Disassembly of the given instructions
```

# Emulating and debugging your microcode using pythonRSC
If you want to debug your program, pythonRSC provides an easy-to-use debugger similar to GDB Debugger.
To start the emulator with the debugger, use this command.

``pythonRSC debug microcode.txt``

After executing, you will be met with a blinking >> awaiting your next command.
The list of commands accepted by the debugger are listed below.
``py
stepi [stepsize] # This will 'step' forward once if stepsize is not provided, otherwise it will step as many times as provided in stepsize.
bp [addr|label] # This will set a breakpoint at the given address in hex or decimal or at a given label. Breakpoints are enabled on initialization.
enable [addr|label] # This will turn on a breakpoint if it was disabled.
disable [addr|label] # This will turn off a breakpoint.
disas [start] [end] # This will take a range of addresses and disassemble the instructions. Acceptable formats are hex or decimal, e.g. disas 0 50
print [type] [reg] # This will print a register in your desired format (type). The types are /d (decimal) /t (binary) /x (hexadecimal)
info # This will print the current state of the emulator, in other words print all registers.
help # This will just list the possible commands.
``


