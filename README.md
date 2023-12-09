Simple C-like scripting engine written in Free Pascal. While it is primarily designed for `satania-buddy`, it can also be utilized as a general-purpose, embeddable scripting engine.

It has been tested and works on the following platforms: DOS (go32v2), Windows (x86 & x64), Linux (x64), although theoretically it should work on every platforms except 8/16-bit systems and wasm, due to the lack of `goto`

By default `import` feature (allows to import external functions from DLLs directly) only works on x64 platforms by default. Enable `SE_LIBFFI`, which in turn uses `libffi` instead, will allow you to use `import` in other archs.

In case of `libffi`, because evil script does not allow to change calling convention (for now!), on x86 it is default to `stdcall` on Windows, and `cdecl` on Unix.

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
- The performance of its virtual machine should be better than CPython.
