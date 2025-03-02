Simple C-like scripting engine written in Free Pascal. While it is primarily designed for `satania-buddy`, it can also be utilized as a general-purpose, embeddable scripting engine.

It has been tested and works on the following platforms: DOS (go32v2), Windows (x86 & x64), Linux (x64), although theoretically it should work on every platforms except 8/16-bit systems.

By default `import` feature (allows to import external functions from DLLs directly) only works on x64. Enable `SE_LIBFFI`, which in turn uses `libffi` instead, will allow you to use `import` in other archs.

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

![image](https://github.com/user-attachments/assets/57163232-2beb-4508-a60d-17ab4762cee1)

Well there's no super instruction for modulo so `primes` is slightly slower than python (local vars) is kinda expected :)

poca: https://github.com/BeRo1985/poca


