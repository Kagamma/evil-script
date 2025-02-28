program evil;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, ScriptEngine;

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
