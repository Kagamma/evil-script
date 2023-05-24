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
    Writeln('Usage: Evil [script file]');
    Halt;
  end;
  SE := TScriptEngine.Create;
  SL := TStringList.Create;
  try
    SL.LoadFromFile(ParamStr(1));
    SE.Source := SL.Text;
    SE.Exec;
  finally
    SE.Free;
    SL.Free;
  end;
end.
