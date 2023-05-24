program evil;

{$mode objfpc}
{$H+}

uses
  SysUtils, Classes, ScriptEngine;

var
  SE: TScriptEngine;
  SL: TStrings;

begin
  if ParamCount < 1 then
  begin
    Writeln('Usage: evil [script file]');
    Halt;
  end;
  SE := TScriptEngine.Create;
  SL := TStringList.Create;
  try
    try
      SL.LoadFromFile(ParamStr(1));
      SE.Source := SL.Text;
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
