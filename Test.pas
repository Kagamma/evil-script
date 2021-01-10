program Test;

{$mode objfpc}

uses
  SysUtils, ScriptEngine;

const
  HelloWorld = 'writeln(''Hello, World!'')';
  IfTest = 'i = 5.1 if i = 5.1 writeln(''True'') else writeln(''Something is wrong!'')';
  StringTest = 's = ''This is a string!'' writeln(s)';
  PerformanceTest = 'i = 0 while i < 999 { i = i + 1 j = 0 while j < 999 { j = j + 1 k = i * j } }';
  ArrayTest = 'a = array(3) i = 0 while i < 3 { a[i] = 1 + i * 2 i = i + 1 } writeln(a[0], '' '', a[1], '' '', a[2])';
  CustomFunctionTest = 'writeln(''5.2 + 2.8 = '', add(5.2, 2.8))';
  YieldTest = 'i = 0 while i < 3 { i = i + 1 yield }';
  PauseTest = 'i = 0 while i < 3 { writeln(''Only run 1 time'') pause i = i + 1 }';
  ResultTest = 'result = 5';

type
  TCustomFunctions = class
  public
    class function Add(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
  end;

class function TCustomFunctions.Add(const VM: TSEVM; const Args: array of TSEValue): TSEValue;
begin
  Exit(Args[0].VarNumber + Args[1].VarNumber);
end;

var
  SE: TScriptEngine;

procedure HelloWorldRun;
begin
  Writeln('--- HelloWorldRun ---');
  SE.Reset;
  SE.Source := HelloWorld;
  SE.Exec;
end;

procedure IfTestRun;
begin
  Writeln('--- IfTestRun ---');
  SE.Reset;
  SE.Source := IfTest;
  SE.Exec;
end;

procedure StringTestRun;
begin
  Writeln('--- StringTestRun ---');
  SE.Reset;
  SE.Source := StringTest;
  SE.Exec;
end;

procedure PerformanceTestRun;
var
  S: Integer;
begin
  Writeln('--- PerformanceTestRun ---');
  SE.Reset;
  SE.Source := PerformanceTest;
  S := GetTickCount;
  SE.Exec;
  Writeln(GetTickCount - S, 'ms');
end;

procedure ArrayTestRun;
begin
  Writeln('--- ArrayTestRun ---');
  SE.Reset;
  SE.Source := ArrayTest;
  SE.Exec;
end;

procedure CustomFunctionTestRun;
begin
  Writeln('--- CustomFunctionTestRun ---');
  SE.Reset;
  SE.RegisterFunc('add', @TCustomFunctions(nil).Add, 2);
  SE.Source := CustomFunctionTest;
  SE.Exec;
end;

procedure YieldTestRun;
begin
  Writeln('--- YieldTestRun ---');
  SE.Reset;
  SE.Source := YieldTest;
  repeat
    SE.Exec;
    if SE.IsYielded then
      Writeln('Yield!');
  until SE.IsDone;
end;

procedure PauseTestRun;
begin
  Writeln('--- PauseTestRun ---');
  SE.Reset;
  SE.Source := PauseTest;
  SE.Exec;
end;

procedure ResultTestRun;
begin
  Writeln('--- ResultTestRun ---');
  SE.Reset;
  SE.Source := ResultTest;
  SE.Exec;
  Writeln(SE.VM.Stack[0].VarNumber);
end;

begin
  SE := TScriptEngine.Create;
  HelloWorldRun;
  IfTestRun;
  StringTestRun;
  PerformanceTestRun;
  ArrayTestRun;
  CustomFunctionTestRun;
  YieldTestRun;
  PauseTestRun;
  ResultTestRun;
  SE.Free;
end.
