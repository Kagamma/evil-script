program Test;

{$mode objfpc}
{$H+}

uses
  SysUtils, ScriptEngine;

const
  HelloWorld = 'writeln(''Hello, World!'')';
  IfTest = 'i = 5.1 if i = 5.1 writeln(''True'') else writeln(''Something is wrong!'')';
  StringTest = 's = ''This is a string!'' writeln(s)';
  PerformanceWhileTest = 'k = 0 i = 0 while i < 999 { i = i + 1 j = 0 while j < 9999 { j = j + 1 k = i * j } } writeln(''k = '', k)';
  PerformanceForTest = 'k = 0 for i = 0 to 999 { for j = 0 to 9999 { k = i * j } } writeln(''k = '', k)';
  ArrayTest = 'a = [] i = 0 while i < 2 { a[i] = 1 + i * 2 i = i + 1 } a[2] = ''text'' writeln(a[0], '' '', a[1], '' '', a[2])';
  CustomFunctionTest = 'writeln(hello(''Satania''))';
  CustomFunctionWithSelfTest = 'a = [ func: add, value: 1 ] writeln(a.func(3).value)';
  ReturnAnotherNativeFunction = 'f = return_another_native_function() writeln(f())';
  CallFunction = 'a = 0 fn test(b) { a += b writeln(a) }';
  YieldTest = 'i = 0 while i < 3 { i = i + 1 yield }';
  FibTest = 'fn fib(n) { if n < 2 result = n else result = fib(n-1) + fib(n-2) } writeln(fib(35))';
  AssertTest = 'assert(false, "Assert triggered")';
  RttiTest = 'o = obj writeln(o.name) o.name = "REPLACED" writeln(o.name)';
  ResultTest = 'result = 5';

type
  TCustomFunctions = class
  public
    class function Hello(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: TSEValue): TSEValue;
    class function ReturnAnotherNativeFunction(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function ReturnMe(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  end;

  TRttiTest = class
  private
    FName: String;
  published
    property Name: String read FName write FName;
  end;

class function TCustomFunctions.Hello(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit('Hello, ' + Args[0]);
end;

class function TCustomFunctions.Add(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: TSEValue): TSEValue;
var
  V: TSEValue;
begin
  V := This.GetValue('value'); // Get data from "value" property
  V := V + Args[0];
  This.SetValue('value', V); // Set new data to "value" property
  Exit(This);
end;

class function TCustomFunctions.ReturnAnotherNativeFunction(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Ind: Integer;
begin
  if VM.Parent.FindFuncNative('return_me', Ind) <> nil then
  begin
    Result.Kind := sevkFunction;
    Result.VarFuncKind := sefkNative;
    Result.VarFuncIndx := Ind;
  end else
  begin
    Result := SENull;
  end;
end;

class function TCustomFunctions.ReturnMe(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit('You called ReturnMe()!');
end;

var
  SE: TScriptEngine;

procedure HelloWorldRun;
begin
  Writeln('--- HelloWorldRun ---');
  SE.Source := HelloWorld;
  SE.Exec;
end;

procedure IfTestRun;
begin
  Writeln('--- IfTestRun ---');
  SE.Source := IfTest;
  SE.Exec;
end;

procedure StringTestRun;
begin
  Writeln('--- StringTestRun ---');
  SE.Source := StringTest;
  SE.Exec;
end;

procedure PerformanceWhileTestRun;
var
  S: Integer;
begin
  Writeln('--- PerformanceWhileTestRun ---');
  SE.Source := PerformanceWhileTest;
  S := GetTickCount;
  SE.Exec;
  Writeln(GetTickCount - S, 'ms');
end;

procedure PerformanceForTestRun;
var
  S: Integer;
begin
  Writeln('--- PerformanceForTestRun ---');
  SE.Source := PerformanceForTest;
  S := GetTickCount;
  SE.Exec;
  Writeln(GetTickCount - S, 'ms');
end;

procedure ArrayTestRun;
begin
  Writeln('--- ArrayTestRun ---');
  SE.Source := ArrayTest;
  SE.Exec;
end;

procedure CustomFunctionTestRun;
begin
  Writeln('--- CustomFunctionTestRun ---');
  SE.RegisterFunc('hello', @TCustomFunctions(nil).Hello, 1);
  SE.Source := CustomFunctionTest;
  SE.Exec;
end;

procedure CustomFunctionWithSelfTestRun;
begin
  Writeln('--- CustomFunctionWithSelfTestRun ---');
  SE.RegisterFuncWithSelf('add', @TCustomFunctions(nil).Add, 1);
  SE.Source := CustomFunctionWithSelfTest;
  SE.Exec;
end;

procedure ReturnAnotherNativeFunctionTestRun;
begin
  Writeln('--- ReturnAnotherNativeFunction ---');
  SE.RegisterFunc('return_another_native_function', @TCustomFunctions(nil).ReturnAnotherNativeFunction, 0);
  SE.RegisterFunc('return_me', @TCustomFunctions(nil).ReturnMe, 0);
  SE.Source := ReturnAnotherNativeFunction;
  SE.Exec;
end;

procedure CallFunctionTestRun;
begin
  Writeln('--- CallFunctionTestRun ---');
  SE.Source := CallFunction;
  SE.Exec; // Initialize global variables
  SE.ExecFuncOnly('test', [2]); // 2
  SE.ExecFuncOnly('test', [2]); // 4
  SE.ExecFuncOnly('test', [1]); // 5
end;

procedure YieldTestRun;
begin
  Writeln('--- YieldTestRun ---');
  SE.Source := YieldTest;
  repeat
    SE.Exec;
    if SE.IsYielded then
      Writeln('Yield!');
  until SE.IsDone;
end;

procedure FibTestRun;
var
  S: Integer;
begin
  Writeln('--- FibTestRun ---');
  SE.Source := FibTest;
  S := GetTickCount;
  SE.Exec;
  Writeln(GetTickCount - S, 'ms');
end;

procedure AssertTestRun;
begin
  try
    Writeln('--- AssertTestRun ---');
    SE.OptimizeAsserts := False;
    SE.Source := AssertTest;
    SE.Exec;
  except
    on E: Exception do
    begin
      Writeln(E.Message);
    end;
  end;
end;

procedure RttiTestRun;
var
  O: TRttiTest;
  V: TSEValue;
begin
  Writeln('--- RttiTestRun ---');
  O := TRttiTest.Create;
  O.Name := 'Original Text';
  GC.AllocPascalObject(@V, O, True);
  SE.SetConst('obj', V);
  SE.Source := RttiTest;
  SE.Exec;
end;

procedure ResultTestRun;
begin
  Writeln('--- ResultTestRun ---');
  SE.Source := ResultTest;
  SE.Exec;
  Writeln(SE.VM.Global.Value^.Data[0].VarNumber); // The first global variable is always "result"
end;

begin
  SE := TScriptEngine.Create;
  HelloWorldRun;
  IfTestRun;
  StringTestRun;
  PerformanceWhileTestRun;
  PerformanceForTestRun;
  ArrayTestRun;
  CustomFunctionTestRun;
  CustomFunctionWithSelfTestRun;
  ReturnAnotherNativeFunctionTestRun;
  CallFunctionTestRun;
  YieldTestRun;
  FibTestRun;
  AssertTestRun;
  RttiTestRun;
  ResultTestRun;
  SE.Free;
end.
