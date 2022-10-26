from pyRSC import RSC

if __name__ == "__main__":
    pyRSC = RSC("tests\\microcode\\testing.txt")
    print(pyRSC._instructions)
    pyRSC.run() # Runs the given instructions and gives you an output!
    print(pyRSC._memory_layout)