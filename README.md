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

![image](https://github.com/user-attachments/assets/2b9c85fa-4234-4fac-b10b-405b0852b147)

poca: https://github.com/BeRo1985/poca


