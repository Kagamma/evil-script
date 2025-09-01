Below is an overview document that briefly explains how to integrate Evil script to your program.

- [TScriptEngine](#tscriptengine)
  + [A quick tour](#a-quick-tour)
  + [Execute a function](#execute-a-function)
  + [Register new functions](#register-new-functions)
  + [Register new functions with the self variable](#register-new-functions-with-the-self-variable)
  + [Yield](#yield)
  + [Exec, ExecFunc, ExecFuncOnly](#exec-execfunc-execfunconly)
  + [Change a global variable](#change-a-global-variable)
- [TSEValue](#tsevalue)
  + [Overview](#overview)
- [Performance tips](#performance-tips)

## TScriptEngine

### A quick tour
Declares an evil script instance:
```
var
  SE: TScriptEngine;
...
  SE := TScriptEngine.Create;
```
Resets the script engine to default state:
```
  SE.Reset;
```
Pass the source code to the script engine. Note that `TScriptEngine.Reset` will automatically be called:
```
  SE.Source := 'writeln("hello, world!")';
```
Executes the script:
```
  SE.Exec;
```
Forces script recompilation, without reset the state:
```
  SE.Lex;
  SE.Parse;
```

### Execute a function
Given the following script:
```
a = 5

fn add(b) {
  result = a + b
}
```
Executes the script once to initialize global variables:
```
  SE.Exec;
```
Executes a named function inside the script, then shows the result on screen:
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
  SE.RegisterFunc('add', @TCustomFunctions(nil).Add, 2);
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
  SE.RegisterFuncWithSelf('add', @TCustomFunctions(nil).Add, 2);
  SE.Source :=
'obj = [ value: 0, add: add ]' + #10 +
'obj.add(2, 3)';
```

#### Yield
`yield` is useful when you need to temporarily exit the script, perform tasks on the Pascal side, and then resume the script where it left off.

Consider the following script:
```
  i = 0
  while i < 3 {
    // Exit the script. The next time it will start from where it left off
    yield
    // Do something
    i += 1
  }
```

On Pascal side, we call `TScriptEngine.Exec` in a loop until `IsDone` flag is set to `true`:
```
  while not SE.IsDone do
  begin
    SE.Exec;
    // Do something else
  end;
```

### Exec, ExecFunc, ExecFuncOnly
- `Exec` is used when you want to execute the script until it's done (check the `TScriptEngine.IsDone` flag). `yield` can be used to quit the script and return later.
- `ExecFunc` executes a named function. If you want to initialize global variables, call `Exec` before `ExecFunc`. `yield` can be used to quit the script and return later.
- `ExecFuncOnly` similar to `ExecFunc` except it's one time only and because of that `yield` CANNOT be used.

### Change a global variable
Useful if we want to modify a global variable after intialized them via `TScriptEngine.Exec()`
```
  SE.VM.SetGlobalVariable('a', NewValue);
```

## TSEValue
### Overview
A 16-byte data structure. `TSEValue.Kind` stores the type of variable, which can be one of the following values: sevkNumber, sevkBoolean, sevkString, sevkMap, sevkBuffer, sevkFunction, sevkPascalObject, sevkNull.

- Declares a new TSEValue:
    ```
      var V: TSEValue;
    ```
- Assigns null value:
    ```
      V := SENull;
    ```
- Assigns number:
    ```
      V := 5;
      // Equivalent to:
      // V.Kind := sevkNumber;
      // V.VarNumber := 5;
    ```
- Assigns boolean value:
    ```
      V := True;
      // Equivalent to:
      // V.Kind := sevkBoolean;
      // V.VarBoolean := True;
    ```
- Assigns string:
    ```
      V := 'This is a string';
      // Equivalent to GC.AllocString(@V, 'This is a string');
      // You can access the string directly via V.VarString^
    ```
- Creates a new map:
    ```
      GC.AllocMap(@V);
      // You can read / write the map via V.GetValue() / V.SetValue() helpers.
      // You can access map instance directly via V.VarMap
    ```
- Creates a new buffer:
    ```
      // 1024 bytes
      GC.AllocBuffer(@V, 1024);
      // Access to buffer via V.VarBuffer^.Ptr pointer. DO NOT touch V.VarBuffer^.Base pointer.
    ```
- Assigns a Pascal object:
    ```
      // If IsManaged is true, then the script engine's garbage collector will automatically free AnObjectInstance when the variable is unreachable.
      GC.AllocPascalObject(@V, AnObjectInstance, IsManaged);

      // Access the object
      Obj := V.VarPascalObject^.Value;
    ```
- Assigns a function:
    ```
      V.Kind := sevkFunction;

      // VarFuncKind stores the type of function:
      // - sefkNative = Pascal function registered via TScriptEngine.RegisterFunc() / TScriptEngine.RegisterFuncWithSelf().
      // - sefkScript = Evil script function.
      V.VarFuncKind := sefkScript;

      // VarFuncIndx is function index.
      // We can look for function index via TScriptEngine.FindFuncScript() or TScriptEngine.FindFuncNative()
      V.VarFuncIndx := 2;
    ```

## Performance tips
- Install `https://github.com/avk959/LGenerics` and enable `SE_MAP_AVK959` flag for a significant map-related performance boost (approximately 200% more for scripts heavily reliant on maps).
- Set `GC.EnableParallel` to `True` to reduce stuttering on tight loops. This is especially useful for games. Note that so far I only test this feature with `SE_MAP_AVK959` on.