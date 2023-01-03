import sys
if __name__ == "__main__":
    from emulator import Emulator
    from assembler import Assembler
else:
    from .emulator import Emulator
    from .assembler import Assembler

def main():
    args = sys.argv[1:]
    if args:
        match args[0]:
            case "assembler":
                if len(args) >= 3:
                    asm = Assembler(args[1])
                    asm.logisim_format(args[2])
                else:
                    print("usage: pythonRSC assembler [filein] [fileout] ")
            case "run":
                if len(args) >= 2:
                    asm = Assembler(args[1])
                    pyRSC = Emulator(asm, debug_mode=False)
                    pyRSC.start()
                else:
                    print("usage: pythonRSC run [file]")
            case "debug":
                if len(args) >= 2:
                    asm = Assembler(args[1])
                    pyRSC = Emulator(asm, debug_mode=True)
                    pyRSC.start()
                else:
                    print("usage: pythonRSC debug [file]")
            case "help":
                print("usage: pythonRSC [run|assembler|debug] [in] [out] \nThe assembler is used for producing logisim-formatted binaries of your microcode.\nThe run command is to emulate your given microcode file, output is not needed.")
            case _:
                print("usage: pythonRSC [run|assembler|debug] [in] [out] \nThe assembler is used for producing just logisim input.\nThe run command is to emulate your given microcode file, output is not needed.")
    else:
        print("usage: pythonRSC [run|assembler|debug] [in] [out] \nThe assembler is used for producing just logisim input.\nThe run command is to emulate your given microcode file, output is not needed.")

if __name__ == "__main__":
    main()