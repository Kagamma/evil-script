#### Features
- Simple C-like scripting engine written in Free Pascal
- Dynamic typing, with 2 types of data (Single, String)
- \+ - * / % & | ! < <= > >= != operators
- Array manipulation
- String manipulation (need to disable SE_FAST flag)
- while, if, break, continue, yield, pause

#### How to use
- See Test.pas
  
#### Notes
- By default SE_FAST is enabled for performance reason. Disable it in ScriptEngine.pas if you need to perform string manipulations (concat, compare)

