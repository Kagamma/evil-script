Simple C-like scripting engine written in Free Pascal

#### Features
- https://github.com/Kagamma/satania-buddy/wiki/Scripting-References-&-APIs

#### How to use
- See Test.pas
  
#### Notes
- By default SE_STRING is disabled for performance reason. Enable it in ScriptEngine.pas if you need to perform string manipulations (concat, compare)
- Number type by default is Single type. Enable SE_PRECISION flag in ScriptEngine.pas if you want more precision (Double type)
- No support for defining functions inside script (easy to add if needed)
- The output bytecode is not portable, due to native functions being stored in bytecode as pointers (which can be changed) instead of by name
