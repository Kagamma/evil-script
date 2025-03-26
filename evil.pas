program evil;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, ScriptEngine;

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
  I: Integer;
  AsmStr: String;

begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: evil <options> [script file]');
    Writeln('Options: ');
    Writeln(' -d  : Disassembly');
    Writeln(' -do : Disable optimizations');
    Writeln(' -da : Disable assertions');
    Halt;
  end;
  if ParamCount > 1 then
  begin
    for I := 1 to ParamCount - 1 do
      case ParamStr(I) of
        '-d':
          IsD := True;
        '-do':
          IsO := False;
        '-da':
          IsA := True;
      end;
  end;
  SE := TScriptEngine.Create;
  SE.OptimizePeephole := IsO;
  SE.OptimizeConstantFolding := IsO;
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
        SE.Exec;
    except
     on E: Exception do
       Writeln(E.Message);
    end;
  finally
    SE.Free;
    SL.Free;
  end;
end.
