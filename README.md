# pythonRSC ( Relatively Simple Computer in Python )

pythonRSC is an emulator for the RSC architecture written in Python.


With this program, you can generate bytecode of your program code for use in Logisim or just emulate program code outright.
Additionally, you can debug your program and see its every step or even generate a control flow graph of your emulated program code.

There is an [introduction video](https://youtu.be/cA685cUVUHM) which goes over the basics of how to download and use the emulator.


## Architecture layout and instruction effects
There are [nine registers](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/classes.py#L23-L32) in this architecture, each are named and explained below.

<br />

`S`: The `stop` register, if this register's value is 1, emulation is halted.

`Z`: The `zero` register, if this register's value is 1, `ACC` is currently 0.

`IR`: The `instruction` register, the emulator reads the value of this register to know what instruction to execute, can be seen [here](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L74).

`AR`: The `address` register, any memory addresses that are going to be accessed are placed into this register, can be seen [here](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L70).

`DR`: The `data` register, any values returned from a memory access will be put into this register, can be seen [here](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L70).

`PC`: The `program counter` register, this register holds the value of the next instruction to be executed, can be seen [here](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L69).

`OUTR`: The `out` register, the value in this register will be displayed on your Logisim board through a hexadecimal display.

`ACC`: The `accumulator` register, most instructions in this emulator operate on this register, used for arithmetic operations mainly.

`R`: No unique name, just a register, holds values to perform operations on `ACC` (i.e `ADD`, `SUB`, etc).

<br />

There are [sixteen instructions](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/classes.py#L4-L20) in this architecture, each are named and explained below, along with a link to their implementation.

<br />

[`HALT`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L172-173): Sets the value of `S` to 1, halting emulation.

[`LDAC`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L165-L170): Loads the value into `ACC` from the provided memory address operand.

[`STAC`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L158-L163): Stores the current value of `ACC` into the provided memory address operand.

[`MVAC`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L155-L156): Moves the current value from `ACC` into `R`, overwriting the current value.

[`MOVR`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L152-L153): Moves the current value from `R` into `ACC`, overwriting the current value.

[`JMP`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L148-L150): Sets `PC` to the provided memory address operand.

[`JMPZ`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L141-L146): If `Z` is 1, sets `PC` to the provided memory address operand, otherwise increments `PC` by 1.

[`OUT`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L138-L139): Moves the current value from `ACC` into `OUTR`.

[`SUB`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L135-L136): Subtracts the current value in `R` from `ACC`, storing it in `ACC`.

[`ADD`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L132-L133): Adds the current value in `R` from `ACC`, storing it in `ACC`.

[`INC`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L129-L130): Increments the current value in `ACC` by 1.

[`CLAC`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L126-L127): Sets the value of `ACC` to 0.

[`AND`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L123-L124): Performs a bitwise AND of `ACC` and `R`, storing the result in `ACC`.

[`OR`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L120-L121): Performs a bitwise OR of `ACC` and `R`, storing the result in `ACC`.

[`ASHR`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L117-L118): Performs an [arithmetic left shift](https://open4tech.com/logical-vs-arithmetic-shift/) by 1 on `ACC`.

[`NOT`](https://github.com/Calastrophe/pythonRSC/blob/d76b2f449544e0a7d158a170583592e87b93894a/src/pythonRSC/emulator.py#L114-L115): Performs a bitwise NOT on `ACC`.



# Installing pythonRSC
To download the package, you can simply use pip.

``pip install pythonRSC``

# Emulating your program using pythonRSC
After downloading the package, you should have access to a command called 'pythonRSC'.

To use this command, you will need a program file to emulate. There are some test files provided.

``pythonRSC run program_code.txt``

This will parse the given program file, emulate it and output the state at the end of emulation.

# Generating bytecode using pythonRSC
If you desire to use the in-built assembler to parse the program file into logisim bytecode, there is a command for that.

You will need to provide a program file and specify a **required** output file name.

``pythonRSC assemble program_code.txt -o bytecode.txt``

# Emulating and debugging your program (timelessly) using pythonRSC
If you want to debug your program, pythonRSC provides an easy-to-use timeless debugger similar to [GDB Debugger](https://en.wikipedia.org/wiki/GNU_Debugger).

The timeless part means that you can go forwards and backwards in execution, thereby relieving you of having to restart the emulator to chase an issue with your program.

To start the emulator with the debugger, use the following command.

``pythonRSC debug program_code.txt``

After executing, you will be met with a ``>>`` awaiting your next command.
The list of commands accepted by the debugger are listed below.


``stepi [stepsize]`` This will 'step' forward once if stepsize is not provided, otherwise it will step as many times as provided in stepsize.

``backi [stepsize]`` This will 'step' backwards once if stepsize is not provided, otherwise it will step as many times as provided in stepsize.

``bp [addr|label] ...`` This will set a breakpoint at the given address(es) in hex or decimal or at given label(s). Breakpoints are enabled on initialization.

``enable [addr|label] ...`` This will turn on a breakpoint if it was disabled, it can take a variadic amount of breakpoints to be enabled.

``disable [addr|label] ...`` This will turn off a breakpoint, it can take a variadic amount of breakpoints to be disabled.

``disas [start] [end]`` This will take a range of addresses and disassemble the instructions. Hexadecimal or decimal.

``disas`` This variation of the disassemble command will try to identify if you are inside a label and disassemble that label for you.

``print [type] [reg]`` This will print a register in your desired format (type). The types are /d (decimal) /t (binary) /x (hexadecimal).

``run`` Resumes emulation unless a breakpoint is hit or HALT is met.

``info`` This will print the current state of the emulator, in other words print all registers.

``help`` This will just list the possible commands.

# Generating a control flow graph for your program
If you wish to generate a control flow graph from your program, you simply pass a flag to the CLI.

``pythonRSC run program_code.txt -cfg pdf`` or ``pythonRSC debug program_code.txt -cfg png``

**GraphViz is required to be downloaded and on your PATH for this to work properly.**

The control flow graph will appear at the end of execution. The graph is rendered using GraphViz and the library pythonCFG.

An example graph:

![image](https://user-images.githubusercontent.com/74928681/215967794-9511d4eb-d0f4-4e98-9bf0-9d4fd14ebd15.png)

