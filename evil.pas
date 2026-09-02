program evil;

{$mode objfpc}
{$H+}

uses
  {$ifdef unix}cthreads, {$endif}SysUtils, Classes, ScriptEngine;

type
  // Helper class to print stack trace, including global variables
  TSEStackTraceHandler = class
    procedure PrintVariables(Message: String; StackTraceArray: TSEStackTraceSymbolArray);
  end;

procedure TSEStackTraceHandler.PrintVariables(Message: String; StackTraceArray: TSEStackTraceSymbolArray);

  procedure PrintNode(const Root: Boolean; const StackNode: PSEStackTraceSymbol; const Spacing: String);
  var
    I: Integer;
    S: String;
  begin
    // Do not show compiler's hidden variables
    if (not Root) and (StackNode^.Name.IndexOf('___') = 0) then
      Exit;
    S := StackNode^.Value;
    if Length(S) > 40 then
    begin
      SetLength(S, 40);
      S := S + '...';
    end;
    if Root then
      Writeln('--- ', StackNode^.Name, ' ---')
    else
      Writeln(Spacing, StackNode^.Name + ' (' + ValueKindNames[StackNode^.Kind] + '): ' + S);
    for I := 0 to Length(StackNode^.Childs) - 1 do
      PrintNode(False, @StackNode^.Childs[I], Spacing + '  ');
  end;

var
  I: Integer;
begin
  Exit;
  for I := 0 to Length(StackTraceArray) - 1 do
  begin
    PrintNode(True, @StackTraceArray[I], '  ');
  end;
end;

var
  SE: TScriptEngine;
  SL: TStrings;
  IsD: Boolean = False;
  IsO: Boolean = True;
  IsA: Boolean = False;
  IsP: Boolean = True;
  IsJ: Boolean = True;
  I: Integer;
  AsmStr, S: String;
  {$ifdef SE_PROFILER}
  Item: TSEProfilerReportItem;
  {$endif}

begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: evil <options> [script file]');
    Writeln('Options: ');
    Writeln(' -d  : Disassembly');
    Writeln(' -do : Disable optimizations');
    Writeln(' -dj : Disable JIT');
    Writeln(' -da : Disable assertions');
    Writeln(' -dp : Disable parallel garbage collector');
    Halt;
  end;
  if ParamCount > 1 then
  begin
    for I := 1 to ParamCount - 1 do
      case ParamStr(I) of
        '-d':
          IsD := True;
        '-dj':
          IsJ := False;
        '-do':
          IsO := False;
        '-da':
          IsA := True;
        '-dp':
          IsP := False;
      end;
  end;
  Randomize;
  GC.EnableParallel := IsP;
  SE := TScriptEngine.Create;
  SE.OptimizePeephole := IsO;
  SE.OptimizeConstantFolding := IsO;
  SE.OptimizeJIT := IsJ;
  SE.OptimizeAsserts := IsA;
  SE.StackTraceHandler := @TSEStackTraceHandler(nil).PrintVariables;
  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(ParamStr(ParamCount));
      SE.Source := SL.Text;
      if IsD then
      begin
        SE.Lex;
        SE.Parse;
        SEDisAsm(SE.VM, AsmStr);
        Writeln(AsmStr);
      end else
      begin
        SE.Exec;
        {$ifdef SE_PROFILER}
        Writeln('+------------------------------+-------------+-------------+-------------+-------------+');
        Writeln('|                        Name  |      Count  |    Min (ms) |    Max (ms) |    Avg (ms) |');
        Writeln('+------------------------------+-------------+-------------+-------------+-------------+');
        for S in SEProfiler.Report.Keys do
        begin
          Item := SEProfiler.Report[S];
          Writeln(Format('| %-28s | %11d | %11.5f | %11.5f | %11.5f |',
               [S, Item.CallCount, Item.LowestTimeInNSec / 1000000, Item.HighestTimeInNSec / 1000000, Item.TotalTimeInNSec / 1000000 / Item.CallCount]));
        end;
        Writeln('+------------------------------+-------------+-------------+-------------+-------------+');
        {$endif}
      end;
    except
     on E: Exception do
       Writeln(E.Message);
    end;
  finally
    SE.Free;
    SL.Free;
  end;
end.
