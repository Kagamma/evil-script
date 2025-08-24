Below is an overview document that briefly explains how to integrate Evil script to your program.

- [TScriptEngine](#tscriptengine)
  + [A quick tour](#a-quick-tour)
  + [Execute a function](#execute-a-function)
  + [Register new functions](#register-new-functions)
  + [Register new functions with the self variable](#register-new-functions-with-the-self-variable)
  + [Exec, ExecFunc, ExecFuncOnly](#exec-execfunc-execfunconly)
  + [Change a global variable](change-a-global-variable)
- [TSEValue](#tsevalue)
  + [Overview](#overview)

## TScriptEngine

### A quick tour
Declare an evil script instance:
```
var
  SE: TScriptEngine;
...
  SE := TScriptEngine.Create;
```
Reset the script engine to default state:
```
  SE.Reset;
```
Pass the source code to the script engine. Note that `TScriptEngine.Reset` will automatically be called:
```
  SE.Source := 'writeln("hello, world!")';
```
Execute the script:
```
  SE.Exec;
```

### Execute a function
Given the following script:
```
a = 5

fn add(b) {
  result = a + b
}
```
Execute the script once to initialize global variables:
```
  SE.Exec;
```
Execute a named function inside the script, then shows the result on screen:
```
  Writeln(SE.ExecFuncOnly('add', [7]).VarNumber); // Should be 12
```

### Register new functions
Declares a new function. The function below will accept 2 parameters,  add them together and return the result:
```
type
  TCustomFunctions = class
  public
    class function Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  end;
  ...
class function TCustomFunctions.Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Args[0] + Args[1];
end;
```
Register the function to the script engine:
```
  SE.RegisterFunc('add', @TCustomFunctions.Add, 2);
  SE.Source := 'a = add(2, 3)';
```

### Register new functions with the `self` variable
Declares a new function. The function below will accept 2 parameters,  add them together and assign the result to `self.value`:
```
type
  TCustomFunctions = class
  public
    class function Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: TSEValue): TSEValue;
  end;
  ...
class function TCustomFunctions.Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: TSEValue): TSEValue;
begin
  This.SetValue('value', Args[0] + Args[1]);
end;
```
Register the function to the script engine:
```
  SE.RegisterFuncWithSelf('add', @TCustomFunctions.Add, 2);
  SE.Source :=
'obj = [ value: 0, add: add ]' + #10 +
'obj.add(2, 3)';
```

### Exec, ExecFunc, ExecFuncOnly
- `Exec` is used when you want to execute the script until it's done (check the `TScriptEngine.IsDone` flag). `yield` can be used to quit the script and return later.
- `ExecFunc` executes the script once to initialize global values, then executes the function. `yield` can be used to quit the script and return later.
- `ExecFuncOnly` executes the function only. `yield` CANNOT be used.

### Change a global variable
Useful if we want to modify a global variable after intialized them via `TScriptEngine.Exec()`
```
  SE.VM.ModifyGlobalVariable('a', NewValue);
```

## TSEValue
### Overview
A 16-bit data structure. `TSEValue.Kind` stores the type of variable, which can be one of the following values: sevkNumber, sevkBoolean, sevkString, sevkMap, sevkBuffer, sevkFunction, sevkPascalObject, sevkNull.

- Declares a new TSEValue:
```
  var V: TSEValue;
```
- Assign null value:
```
  V := SENull;
```
- Assign number:
```
  V := 5;
```
- Assign boolean value:
```
  V := True;
```
- Assign string:
```
  V := 'This is a string';
```
- Assign a map:
```
  GC.AllocMap(@V);
  // We can read / write the map via V.GetValue() / V.SetValue() helpers.
```
- Assign a Pascal object:
```
  // If IsManaged is true, then the script engine's garbage collector will automatically free AnObjectInstance when the variable is unreachable.
  GC.AllocPascalObject(@V, AnObjectInstance, IsManaged);

  // Access the object
  Obj := V.VarPascalObject^.Value;
```
- Assign a function:
```
  V.Kind := sevkFunction;

  // VarFuncKind stores the type of function:
  // - sefkNative = Pascal function registered via TScriptEngine.RegisterFunc() / TScriptEngine.RegisterFuncWithSelf().
  // - sefkScript = Evil script function.
  V.VarFuncKind := sefkScript;

  // VarFuncIndx is either:
  // - Function index, in case VarFuncKind = sefkScript
  // - Function pointer, in case VarFuncKind = sefkNative
  // We can look for function index via TScriptEngine.FindFuncScript()
  // or function pointer via TScriptEngine.FindFuncNative()
  V.VarFuncIndx := 2;

```
