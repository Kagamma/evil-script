Simple C-like scripting engine written in Free Pascal

#### Features
- Dynamic typing, with 3 types of data (Number, String, Array)
- \+ - * / % & | ! < <= > >= != operators
- Array manipulation
- String manipulation (only for assign, need to enable SE_STRING flag for concat and compare strings)
- while, if, else, break, continue, yield, pause
- Call free pascal native functions by registering them to script engine

#### How to use
- See Test.pas
  
#### Notes
- By default SE_STRING is disabled for performance reason. Enable it in ScriptEngine.pas if you need to perform string manipulations (concat, compare)
- Number type by default is Single type. Enable SE_PRECISION flag in ScriptEngine.pas if you want more precision (Double type)
- No support for defining functions inside script (easy to add if needed)
- The output bytecode is not portable (need to include source code of the script for recompiling when needed), due to native functions being stored in bytecode as pointers (which can be changed) for better performance. It is possible to achieve portable bytecode by storing native functions by name and use a lookup table to search for its pointers instead.
