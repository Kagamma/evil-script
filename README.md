Simple C-like scripting engine written in Free Pascal

#### Features
- https://github.com/Kagamma/satania-buddy/wiki/Scripting-Reference

#### How to use
- See Test.pas

#### About compiler
- The compiler itself is a one-pass compiler. It follows Niklaus Wirth’s design, completely skips AST generation and generates binary directly.
- Due to the lack of AST, only constant folding and peephole optimizations are implemented.
- The performance of its virtual machine should be better than CPython.
