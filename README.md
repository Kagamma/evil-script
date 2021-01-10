#### Features
- Simple C-like scripting engine written in Free Pascal
- Dynamic typing, with 3 types of data (Single, String, Array)
- \+ - * / % & | ! < <= > >= != operators
- Array manipulation
- String manipulation (only for assign, need to disable SE_FAST flag for concat and compare strings)
- while, if, break, continue, yield, pause
- Call free pascal native functions by registering them to script engine

#### How to use
- See Test.pas
  
#### Notes
- By default SE_FAST is enabled for performance reason. Disable it in ScriptEngine.pas if you need to perform string manipulations (concat, compare)
- No support for defining functions inside script (easy to add if needed)

