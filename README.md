Simple C-like scripting engine written in Free Pascal. Originally designed as a scripting language for `satania-buddy`, it is now a standalone project intended as a general-purpose, embeddable scripting engine.

It has been tested and works on the following platforms: DOS (go32v2), Windows (x86 & x64), Linux (x64, AArch64), although theoretically it should work on every platforms except 8/16-bit systems.

Enable `SE_LIBFFI` if you need to call external functions from DLLs/SOs.

#### Documentations
- https://github.com/Kagamma/evil-script/tree/main/docs

#### Building
- `fpc -O4 evil.pas`

#### Running
- `evil examples/hello.evil`

#### How to embedded into applications
- See `Test.pas` and `evil.pas` source code
- Also see https://github.com/Kagamma/evil-script/tree/main/docs/how-to-use.md

#### Performance
- The interpreter uses indirect threading. It compares well to CPython in most cases.
- On x86-64 CPUs, the interpreter supports JIT compilation. This makes it much faster than pure interpreter mode for number crunching.

<img width="883" height="749" alt="image" src="https://github.com/user-attachments/assets/f3e18b70-4e90-4d13-83b6-9aa6e7e1d378" />

poca: https://github.com/BeRo1985/poca

PascalScript: https://wiki.freepascal.org/Pascal_Script

Sheet: https://docs.google.com/spreadsheets/d/17FyT62GQ2gRQACV-nbVJaZml1xg9hsEx-rmCGu6OzNs/edit?usp=sharing


```
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣆⠹⣿⣤⣤⠂⣾⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⠙⣾⡉⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠘⣷⣄⠹⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠈⠘⢧⡘⢿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⠛⢠⣦⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⠑⢄⠑⡈⢿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠋⢠⡆⣿⣿⣆⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣟⢻⣿⣿⣿⣆⢈⠣⡈⡄⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⣶⣿⢠⣿⣿⣿⡆⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡘⣿⣿⣿⣿⣿⡇  ⠃⢽⣿⣿⣿⣿
⣿⣿⣿⣿⣧⡉⠛⠿⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢸⣿⣿⣿⣿⣆⠻⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⢿⣿⣿⣿⣿⠇  ⢠⠸⣿⣿⣿⣿
⣿⣿⣿⣿⣿⣷⣤⣤⣾⣿⣿⣿⣿⣿⣿⣿  ⣿⣿⣿⢸⣿⠿⠛⠙⠛⠣⠙⣿⣿⣿⣿⣿⣿⣿⣿⣷⢸⣿⣿⣿⣿    ⣾⡇⣿⣿⣿⣿
⣿⣿⣿⣿⣿⡏⢹⣿⣿⣿⣿⣿⣿⣿⣿⡿  ⢻⣿⡇⡼⢁⡴⢂⣴⡆⢶⡂  ⠉⢛⠿⣿⣿⣿⣿⣿⡘⣿⣿⣿⣿⡀⣾⣿⣇⢹⣿⣿⣿
⣿⣿⣿⣿⣿⢰⡆⠿⠿⣿⣿⠿⠿⢛⣛⣃⣿⣤⣠⣤⣇⣾⢁⣾⣿⣷⣶⣶⡆⢱⣤⣿⣶⣭⢻⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣿⠸⣿⣿⣿
⣿⣿⣿⡿⠇⢀⠴⠒⠂⣄  ⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢸⣿⣿⣿⣿⣿⡇⣸⣿⣿⣿⣿⢸⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣿⡇⣿⣿⣿
⣿⡿⠛  ⣴⡏⣰⣿⣌⣁⣆⢹⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣦⡙⠻⠿⠛⣉⣴⣿⣿⣿⣿⣿⢸⣿⣿⣿⣿⣿⣿⣿⡇⢻⣿⢙⠏⣿⣿⣿
⣤⣶⠇⣸⣿⡇⣿⣿⣿⣿⣿⢸⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⢸⣿⣿⣿⣿⣿⣿⣿⡷⢸⣿⢸⣿⣿⣿⣿
⣿⣿⣦⣿⣿⣷⡘⢿⣿⡿⢃⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡏⣾⣿⣿⣿⣿⣿⡿⠛⣡⣿⡏⢸⣿⣿⣿⣿
⢻⣿⣿⣿⣿⣿⣿⣶⣶⣶⣿⣿⣿⣿⣿⣿⡿⠿⠿⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡟⢱⣿⣿⣿⣿⣿⠏  ⢙⣛⣩⣥⣜⡛⠻⠿⢿
⡆⢻⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣷⣶⣦⣍⣛⠿⣿⣿⣿⣿⣿⣿⡿⠇⢿⣿⠿⢛⣡⣴⣾⣿⣿⣿⣿⠿⠿⠿⠿⢷⣶
⣿⡌⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣿⣿⣿⣿⣿⣷⡘⣿⣿⣿⣿⠿⠇⣶⠞⣡⣶⣿⣿⣿⣿⣿⣿⠋⣴⣾⣿⣿⣿⣶⡙
⣿⣿⡈⠿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣿⣿⣿⣿⣿⣿⣷⡘⡿⢛⣡⣾  ⢁⣾⣿⣿⣿⣿⣿⣿⣿⠃⣾⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣷⣶⣤⣭⣉⣙⣛⠛⣛⠻⠿⠿⣿⣇⠿⣿⣿⣿⣿⣿⣿⣿⣿⡅⢲⢻⣿⡟⣰⣿⣿⣿⣿⣿⣿⣿⣿⡏⣸⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣿⣿⠟⣉⣉⣛⡉⢰⢻⣿⣿⣶⣶⣶⣭⠈⣝⢿⣿⣿⣿⣿⣿⡇⡼⢸⡟⣰⣿⣿⣿⣿⣿⣿⣿⣿⣿⡇⣿⣿⣿⣿⣿⣿⣿⣿⣿
⡘⢿⣿⠟⢡  ⣿⣿⣿⣿⡘⣧⡻⣿⣿⣿⣿⣿⡇⣿⣷⣝⡛⠟⠛⣉⣴⡇⡿⢃⣿⣿⣿⣿⣿⣿⣿⣿⡿⠿⠃⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣦⣈⢴⣿⢰⣿⣿⣿⣿⠗⣿⣿⣎⠻⣿⣿⣿⣷⡘⣿⡟⣡⣾⣷⠌⡙⢸⢡⣿⣿⣿⣿⣿⣿⡿⠋⣡⣶⣿⣿⣿⣿⠟⣛⣿⣿⣿⣿⣿
⣿⣿⣿⡀⣿⣌⣛⣛⣛⣋⣀⡀⢸⣿⣷⣦⣍⡛⠿⢷⣈⣄⢹⣿⠏⢼⣿⡇⣾⣿⣿⣿⣿⣿⢠⣶⣿⣿⣿⣿⣿⣿⣷⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⡇⠘⣿⣿⣿⣿⣿⡟⣡⣾⣿⣿⣿⣿⣿⣿⣷⣾⣌⠢⡙⣼⣦⠙⢣⣿⣿⣿⣿⣿⣿⡌⢿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
⣿⣿⣿⣷  ⠘⣿⣿⣿⠏⣴⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧⡀⣿⣿⣿⠈⣿⣿⣿⣿⣿⣿⣿⡘⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
```


