# pythonRSC ( Relatively Simple Computer in Python )

pythonRSC is an emulator for the RSC architecture written in Python.


With this program, you can generate bytecode from microcode files for Logisim or just emulate microcode files out right.
Additionally, you can debug your program and see its every step.

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

``pythonRSC assembler microcode.txt -o output.txt``

# Emulating and debugging your microcode using pythonRSC
If you want to debug your program, pythonRSC provides an easy-to-use debugger similar to GDB Debugger.
To start the emulator with the debugger, use the following command.

``pythonRSC debug microcode.txt``

After executing, you will be met with a blinking ``>>`` awaiting your next command.
The list of commands accepted by the debugger are listed below.


``stepi [stepsize]`` This will 'step' forward once if stepsize is not provided, otherwise it will step as many times as provided in stepsize

``bp [addr|label] ...`` This will set a breakpoint at the given address(es) in hex or decimal or at given label(s). Breakpoints are enabled on initialization.

``enable [addr|label] ...`` This will turn on a breakpoint if it was disabled, it can take a variadic amount of breakpoints to be enabled.

``disable [addr|label] ...`` This will turn off a breakpoint, it can take a variadic amount of breakpoints to be disabled.

``disas [start] [end]`` This will take a range of addresses and disassemble the instructions. Hexadecimal or decimal.

``disas`` This variation of the disassemble command will try to identify if you are inside a label and disassemble that label for you.

``print [type] [reg]`` This will print a register in your desired format (type). The types are /d (decimal) /t (binary) /x (hexadecimal).

``run`` Resumes emulation unless a breakpoint is hit or HALT is met.

``info`` This will print the current state of the emulator, in other words print all registers.

``help`` This will just list the possible commands.


