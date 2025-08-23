Simple C-like scripting engine written in Free Pascal. While it is primarily designed for `satania-buddy`, it can also be utilized as a general-purpose, embeddable scripting engine.

It has been tested and works on the following platforms: DOS (go32v2), Windows (x86 & x64), Linux (x64), although theoretically it should work on every platforms except 8/16-bit systems.

Enable `SE_LIBFFI` if you need to call external functions from DLLs/SOs.

#### Features
- https://github.com/Kagamma/satania-buddy/wiki/Scripting-Reference

#### Building
- `fpc -O4 evil.pas`

#### Running
- `evil examples/hello.evil`

#### How to embedded into applications
- See `Test.pas` and `evil.pas` source code

#### About compiler
- The compiler itself is a one-pass compiler. It follows Niklaus Wirth’s design, completely skips AST generation and generates binary directly.
- Due to the lack of AST, only constant folding and peephole optimizations are implemented.
- The virtual machine is stack-based, with super instructions to speed up certain operations.

<img width="598" height="366" alt="image" src="https://github.com/user-attachments/assets/6d5a21af-57f0-4a34-9fa3-38998beb5e31" />

poca: https://github.com/BeRo1985/poca
PascalScript: https://wiki.freepascal.org/Pascal_Script
Sheet: https://docs.google.com/spreadsheets/d/17FyT62GQ2gRQACV-nbVJaZml1xg9hsEx-rmCGu6OzNs/edit?usp=sharing


