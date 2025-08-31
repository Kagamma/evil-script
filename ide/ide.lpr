program ide;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, GameCodeEditor, ScriptEngine
  { you can add units after this };

{$R *.res}

begin
  GC.EnableParallel := True;
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.{%H-}MainFormOnTaskbar:=True;
  Application.Initialize;
  Application.CreateForm(TCodeEditorFrm, CodeEditorFrm);
  Application.Run;
end.

