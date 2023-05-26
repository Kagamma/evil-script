Simple C-like scripting engine written in Free Pascal. While it is primarily designed for `satania-buddy`, it can also be utilized as a general-purpose, embeddable scripting engine.

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
