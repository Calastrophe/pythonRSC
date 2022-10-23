from pyRSC import RSC

if __name__ == "__main__":
    pyRSC = RSC("tests\\not_test.txt")
    pyRSC.parse() # Parses the given binary into instructions
    pyRSC.run() # Runs the given instructions and gives you an output!