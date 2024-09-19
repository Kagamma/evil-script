program Test;

{$mode objfpc}
{$H+}

uses
  SysUtils, ScriptEngine;

const
  HelloWorld = 'writeln(''Hello, World!'')';
  IfTest = 'i = 5.1 if i = 5.1 writeln(''True'') else writeln(''Something is wrong!'')';
  StringTest = 's = ''This is a string!'' writeln(s)';
  PerformanceTest = 'i = 0 while i < 999 { i = i + 1 j = 0 while j < 999 { j = j + 1 k = i * j } }';
  ArrayTest = 'a = [] i = 0 while i < 2 { a[i] = 1 + i * 2 i = i + 1 } a[2] = ''text'' writeln(a[0], '' '', a[1], '' '', a[2])';
  CustomFunctionTest = 'writeln(hello(''Satania''))';
  CustomFunctionWithSelfTest = 'a = [ func: add, value: 1 ] writeln(a.func(3).value)';
  YieldTest = 'i = 0 while i < 3 { i = i + 1 yield }';
  FibTest = 'fn fib(n) { if n < 2 result = n else result = fib(n-1) + fib(n-2) } writeln(fib(36))';
  AssertTest = 'assert(false, "Assert triggered")';
  ResultTest = 'result = 5';

type
  TCustomFunctions = class
  public
    class function Hello(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
    class function Add(const VM: TSEVM; const Args: array of TSEValue; const This: TSEValue): TSEValue;
  end;

class function TCustomFunctions.Hello(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit('Hello, ' + Args[0]);
end;

class function TCustomFunctions.Add(const VM: TSEVM; const Args: array of TSEValue; const This: TSEValue): TSEValue;
var
  V: TSEValue;
begin
  V := SEMapGet(This, 'value'); // Get data from "value" property
  V := V + Args[0];
  SEMapSet(This, 'value', V); // Set new data to "value" property
  Exit(This);
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

procedure PerformanceTestRun;
var
  S: Integer;
begin
  Writeln('--- PerformanceTestRun ---');
  SE.Source := PerformanceTest;
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

procedure ResultTestRun;
begin
  Writeln('--- ResultTestRun ---');
  SE.Source := ResultTest;
  SE.Exec;
  Writeln(SE.VM.Global[0].VarNumber); // The first global variable is always "result"
end;

begin
  SE := TScriptEngine.Create;
  HelloWorldRun;
  IfTestRun;
  StringTestRun;
  PerformanceTestRun;
  ArrayTestRun;
  CustomFunctionTestRun;
  CustomFunctionWithSelfTestRun;
  YieldTestRun;
  FibTestRun;
  AssertTestRun;
  ResultTestRun;
  SE.Free;
end.
