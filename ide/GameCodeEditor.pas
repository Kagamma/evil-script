unit GameCodeEditor;

interface

uses
  // RTL / FCL
  Classes, TypInfo, SysUtils, StrUtils,
  // LCL
  LCLType, LCLIntf, LCLProc, Forms, Controls, Buttons, Graphics, StdCtrls,
  Menus, ExtCtrls, ComCtrls, Dialogs, PropEdits, TextTools, ObjInspStrConsts,
  IDEWindowIntf, LazUTF8,
  SynEdit, SynEditTypes, SynEditMarkupSpecialLine, SynCompletion, SynFacilHighlighter, SynFacilBasic,
  ScriptEngine;

type
  { TCodeEditorFrm }

  TCodeEditorFrm = class(TForm)
    ImageList: TImageList;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    StatusBar: TStatusBar;
    SynCompletion: TSynCompletion;
    Editor: TSynEdit;
    ToolBar: TToolBar;
    ButtonSave: TToolButton;
    ButtonCompile: TToolButton;
    ToolButton1: TToolButton;
    ButtonUndo: TToolButton;
    ButtonRedo: TToolButton;
    ButtonOpen: TToolButton;
    ButtonRun: TToolButton;
    ButtonNew: TToolButton;
    ButtonSaveAs: TToolButton;
    ToolButton4: TToolButton;
    ButtonSearch: TToolButton;
    ButtonReplace: TToolButton;
    ToolButton7: TToolButton;
    procedure ButtonCompileClick(Sender: TObject);
    procedure ButtonNewClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure ButtonRedoClick(Sender: TObject);
    procedure ButtonReplaceClick(Sender: TObject);
    procedure ButtonRunClick(Sender: TObject);
    procedure ButtonSaveAsClick(Sender: TObject);
    procedure ButtonSearchClick(Sender: TObject);
    procedure ButtonUndoClick(Sender: TObject);
    procedure EditorChange(Sender: TObject);
    procedure EditorKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure EditorSpecialLineColors(Sender: TObject; Line: integer;
      var Special: boolean; var FG, BG: TColor);
    procedure SynCompletionBeforeExecute(ASender: TSynBaseCompletion;
      var ACurrentString: String; var APosition: Integer; var AnX,
      AnY: Integer; var AnResult: TOnBeforeExeucteFlags);
    function SynCompletionPaintItem(const AKey: string; ACanvas: TCanvas; X,
      Y: integer; Selected: boolean; Index: integer): boolean;
  public
    SearchText: String;
    Highlighter: TSynFacilSyn;
    Script: TScriptEngine;
    ErrorPos: TPoint;
    Path: String;
    procedure LoadHighligher;
    procedure GenerateAutoComplete;
    procedure FindNext;
  end;

var
  CodeEditorFrm: TCodeEditorFrm;

implementation

uses
  Math;

{$R *.lfm}

{ TCodeEditorFrm }

procedure TCodeEditorFrm.LoadHighligher;
var
  SS: TStringStream;
begin
  SS := TStringStream.Create('<?xml version="1.0"?>' +
'<Language name="evil script" CaseSensitive="true">' +
'  <Attribute Name="Keyword" ForeCol="#1080B0" Style="b"></Attribute>' +
'  <Attribute Name="Special" ForeCol="#D080D0"></Attribute>' +
'  <Attribute Name="String" ForeCol="#B08060"></Attribute>' +
'  <Attribute Name="Number" ForeCol="#B0B090"></Attribute>' +
'  <Attribute Name="Comment" ForeCol="#308000" Style=""></Attribute>' +
'  <Attribute Name="Directive" ForeCol="#60B0B0" Style=""></Attribute>' +

' <Comment Start="//" ></Comment>' +
'  <Comment Start="/*" End="*/" Multiline="true" Folding="true"></Comment>' +

'  <Token CharsStart="0..9" Content="0..9." Attribute="Number"></Token>' +
'  <Token CharsStart="0x" Content="0..9a..fA..Fx" Attribute="Number"></Token>' +
'  <Token CharsStart="#" Content="0..9A..Za..z_" Attribute="Directive"></Token>' +
'  <!--<Token CharsStart="A..Z" Content="0..9A..Z_" Attribute="Constant"></Token>-->' +
'  <Token CharsStart=''"'' End=''"'' Attribute="String" Escape="\" Multiline="true" ></Token>' +
'  <Token CharsStart="''" End="''" Attribute="String" Escape="\" Multiline="true" ></Token>' +
'  <Identifiers CharsStart="_a..z" Content="0..9a..z">' +
'    <Keyword>step local const using if else for while do yield break continue return in to downto fn import switch case default try catch throw stdcall cdecl</Keyword>' +
'    <Special>null true false result self</Special>' +
'  </Identifiers>' +

'  <Block Name="block" Start="{" End="}"></Block>' +
'  <Block Name="map" Start="[" End="]"></Block>' +
'</Language>');
  try
    if Highlighter <> nil then
    begin
      Editor.Highlighter := nil;
      FreeAndNil(Highlighter);
    end;
    Highlighter := TSynFacilSyn.Create(Self);
    Highlighter.LoadFromStream(SS);
    Self.Editor.Highlighter := Highlighter;
  finally
    SS.Free;
  end;
end;

procedure TCodeEditorFrm.GenerateAutoComplete;
var
  SL: TStringList;
  I, P: Integer;
  S: String;
  Token: TSEToken;
begin
  Self.SynCompletion.ItemList.Clear;
  Self.SynCompletion.ItemList.BeginUpdate;
  SL := TStringList.Create;
  try
    SL.Sorted := True;
    SL.Duplicates := dupIgnore;
    try
      Self.Script.Source := Editor.Lines.Text;
      Self.Script.OptimizeConstantFolding := False;
      Self.Script.OptimizePeephole := False;
      Self.Script.OptimizeAsserts := False;
      Self.Script.Lex;
      Self.Script.Parse;
    except
      // There will be errors, obviously. We just need to ignore it
    end;
    // Add keywords
    SL.AddStrings([
      'using',
      'if',
      'for',
      'while',
      'do',
      'switch',
      'case',
      'default',
      'yield',
      'break',
      'continue',
      'return',
      'self',
      'in',
      'to',
      'fn',
      'step',
      'import',
      'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f64', 'buffer', 'wbuffer'
    ]);
    // Transfer function names and constant names to completion
    for I := 0 to Self.Script.FuncNativeList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncNativeList[I].Name + '()');
    end;
    for I := 0 to Self.Script.FuncScriptList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncScriptList[I].Name + '()');
    end;
    for I := 0 to Self.Script.FuncImportList.Count - 1 do
    begin
      SL.Add(Self.Script.FuncImportList[I].Name + '()');
    end;
    for I := 0 to Self.Script.VarList.Count - 1 do
    begin
      SL.Add(Self.Script.VarList[I].Name);
    end;
    for S in Self.Script.ConstLookup.Keys do
    begin
      SL.Add(S);
    end;
    for S in SL do
    begin
      Self.SynCompletion.ItemList.Add(S);
    end;
  finally
    SL.Free;
    Self.SynCompletion.ItemList.EndUpdate;
  end;
end;

procedure TCodeEditorFrm.FindNext;
begin
  if Self.SearchText <> '' then
  begin
    Self.Editor.SearchReplace(Self.SearchText, '', [ssoFindContinue]);
  end;
end;

procedure TCodeEditorFrm.FormCreate(Sender: TObject);
begin
  Self.Script := TScriptEngine.Create;
  LoadHighligher;
end;

procedure TCodeEditorFrm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  // IDEDialogLayoutList.SaveLayout(Self);
  CloseAction := caFree;
end;

procedure TCodeEditorFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := True;
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want to close?', mtInformation, [mbYes, mbNo], 0) = mrYes then
      Editor.Modified := False
    else
      CanClose := False;
  end;
end;

procedure TCodeEditorFrm.ButtonCompileClick(Sender: TObject);
begin
  Self.Script.Source := Editor.Lines.Text;
  StatusBar.Panels[1].Text := '';
  try
    Self.Script.Lex;
    Self.Script.Parse;
    StatusBar.Panels[1].Text := 'Compile OK!';
  except
    on E: Exception do
    begin
      ErrorPos := Point(Self.Script.ErrorCol, Min(Self.Script.ErrorLn, Editor.Lines.Count));
      Editor.CaretXY := ErrorPos;
      Editor.SetFocus;
      Editor.Invalidate;
      StatusBar.Panels[1].Text := E.Message;
    end;
  end;
end;

procedure TCodeEditorFrm.ButtonNewClick(Sender: TObject);
begin
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want create new script?', mtInformation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  end;
  Self.Path := '';
  Editor.Lines.Text := '';
  Self.Caption := 'New script'; 
  Editor.Modified := False;
end;

procedure TCodeEditorFrm.ButtonOpenClick(Sender: TObject);
begin
  if Editor.Modified then
  begin
    if MessageDlg('', 'You have unsaved changes. Do you still want to open another file?', mtInformation, [mbYes, mbNo], 0) = mrNo then
      Exit;
  end;
  if OpenDialog.Execute then
  begin
    Self.Path := OpenDialog.FileName;
    Editor.Lines.LoadFromFile(OpenDialog.FileName);
    Self.Caption := ExtractFileName(OpenDialog.FileName);
    Editor.Modified := False;
  end;
end;

procedure TCodeEditorFrm.ButtonRedoClick(Sender: TObject);
begin
  Editor.Redo;
end;

procedure TCodeEditorFrm.ButtonReplaceClick(Sender: TObject);
var
  ATexts: array of String;
begin
  SearchText := '';
  SetLength(ATexts, 2);
  if InputQuery('Replace All', ['Search string', 'Replace with'], ATexts) then
  begin
    SearchText := ATexts[0];
    Editor.SearchReplace(ATexts[0], ATexts[1], [ssoEntireScope, ssoReplaceAll]);
  end;
end;

procedure TCodeEditorFrm.ButtonRunClick(Sender: TObject);
begin  
  Self.Script.Source := Editor.Lines.Text;
  StatusBar.Panels[1].Text := '';
  try
    Self.Script.OptimizeConstantFolding := True;
    Self.Script.OptimizePeephole := True;
    Self.Script.OptimizeAsserts := True;
    Self.Script.Exec;
  except
    on E: Exception do
    begin
      ErrorPos := Point(Self.Script.ErrorCol, Min(Self.Script.ErrorLn, Editor.Lines.Count));
      Editor.CaretXY := ErrorPos;
      Editor.SetFocus;
      Editor.Invalidate;
      StatusBar.Panels[1].Text := E.Message;
    end;
  end;
end;

procedure TCodeEditorFrm.ButtonSaveAsClick(Sender: TObject);
var
  I: Integer;
begin
  if SaveDialog.Execute then
  begin
    Self.Path := SaveDialog.FileName;
    Self.Caption := ExtractFileName(SaveDialog.FileName);
    Editor.Modified := False;
    for I := 0 to Self.Editor.Lines.Count - 1 do
    begin
      Self.Editor.Lines[I] := TrimRight(Self.Editor.Lines[I]);
    end;
    Self.Editor.Lines.SaveToFile(Self.Path);
  end;
end;

procedure TCodeEditorFrm.ButtonSearchClick(Sender: TObject);
begin
  SearchText := '';
  if InputQuery('Find', 'Search string', False, Self.SearchText) then
  begin
    Editor.SearchReplace(Self.SearchText, '', []);
  end;
end;

procedure TCodeEditorFrm.ButtonUndoClick(Sender: TObject);
begin
  Editor.Undo;
end;

procedure TCodeEditorFrm.EditorChange(Sender: TObject);
begin
  ErrorPos.Y := -1;
  if Caption[1] <> '*' then
    Caption := '*' + Caption;
end;

procedure TCodeEditorFrm.EditorKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  S: String;
begin
  if Key = $78 then
  begin
    Self.ButtonCompileClick(Self);
  end else
  if (Key = VK_S) and (Shift = [ssCtrl]) then
  begin
    Editor.Modified := False;
    Self.Editor.Lines.SaveToFile(Self.Path);
    if Caption[1] = '*' then
    begin
      S := Caption;
      Delete(S, 1, 1);
      Caption := S;
    end;
  end else
  if (Key = VK_F) and (Shift = [ssCtrl]) then
  begin
    Self.ButtonSearchClick(Self);
  end else
  if (Key = VK_R) and (Shift = [ssCtrl]) then
  begin
    Self.ButtonReplaceClick(Self);
  end else
  if Key = VK_F3 then
  begin
    Self.FindNext;
  end;
end;

procedure TCodeEditorFrm.ButtonSaveClick(Sender: TObject);
var
  I: Integer;
begin
  if Self.Path = '' then
    Self.ButtonSaveAsClick(Sender)
  else
  begin
    Editor.Modified := False;
    for I := 0 to Self.Editor.Lines.Count - 1 do
    begin
      Self.Editor.Lines[I] := TrimRight(Self.Editor.Lines[I]);
    end;
    Self.Editor.Lines.SaveToFile(Self.Path);
  end;
end;

procedure TCodeEditorFrm.FormDestroy(Sender: TObject);
begin
  Self.Script.Free;
end;

procedure TCodeEditorFrm.EditorSpecialLineColors(Sender: TObject;
  Line: integer; var Special: boolean; var FG, BG: TColor);
begin
  if Line = ErrorPos.Y then
  begin
    Special := True;
    BG := $00A5FF;
    Fg := clBlack;
  end;
end;

procedure TCodeEditorFrm.SynCompletionBeforeExecute(
  ASender: TSynBaseCompletion; var ACurrentString: String;
  var APosition: Integer; var AnX, AnY: Integer;
  var AnResult: TOnBeforeExeucteFlags);
begin
  Self.GenerateAutoComplete;
end;

function TCodeEditorFrm.SynCompletionPaintItem(const AKey: string;
  ACanvas: TCanvas; X, Y: integer; Selected: boolean; Index: integer): boolean;
begin
  if Pos('()', AKey) <> 0 then
    Self.ImageList.Draw(ACanvas, X, Y, 39);
  ACanvas.TextOut(X + 18, Y, AKey);
end;

end.
