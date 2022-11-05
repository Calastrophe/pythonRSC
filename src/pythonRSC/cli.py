import sys
if __name__ == "__main__":
    from pyRSC import RSC, Assembler
else:
    from .pyRSC import RSC, Assembler

def main():
    args = sys.argv[1:]
    if args:
        match args[0]:
            case "assembler":
                if len(args) >= 3:
                    asm = Assembler(args[1])
                    asm.logisim_format(args[2])
                else:
                    print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]")
            case "run":
                if len(args) >= 2:
                    pyRSC = RSC(args[1])
                    pyRSC.run()
                else:
                    print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]")
            case "debug":
                if len(args) >= 2:
                    pyRSC = RSC(args[1], debug=True)
                    pyRSC.run()
                else:
                    print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]")
            case "help":
                print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]\nThe assembler is used for producing logisim-formatted binaries of your microcode.\nThe run command is to emulate your given microcode file, output is not needed.")
            case _:
                print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]")
    else:
        print("usage: pythonRSC [run|assembler|debug] [in] [out] [in] [out]\nThe assembler is used for producing just logisim input.\nThe run command is to emulate your given microcode file, output is not needed.")

if __name__ == "__main__":
    main()