program evil;

{$mode objfpc}
{$H+}

uses
  {$ifdef windows}windows, {$endif}
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
  for I := 0 to Length(StackTraceArray) - 1 do
  begin
    PrintNode(True, @StackTraceArray[I], '  ');
  end;
end;

{$ifdef windows}
type
  PVectoredExceptionHandler = function(pExceptionInfo: PEXCEPTION_POINTERS): Longint; stdcall;

function AddVectoredExceptionHandler(
  FirstHandler: ULONG;
  VectoredHandler: PVectoredExceptionHandler
): PVOID; stdcall; external kernel32 name 'AddVectoredExceptionHandler';

function RemoveVectoredExceptionHandler(
  Handler: PVOID
): ULONG; stdcall; external kernel32 name 'RemoveVectoredExceptionHandler';

function MyVectoredHandler(pInfo: PEXCEPTION_POINTERS): Longint; stdcall;
var
  rec: PExceptionRecord;
  ctx: PContext;
  I: Integer;
begin
  rec := pInfo^.ExceptionRecord;
  ctx := pInfo^.ContextRecord;

  Writeln('=== EXCEPTION CAUGHT BY VEH ===');
  Writeln(Format('Exception Code: %.8x at Address: %p', [rec^.ExceptionCode, rec^.ExceptionAddress]));

  {$ifdef CPUX86_64}
  // 64-bit General Purpose Registers
  Writeln('--- General Purpose Registers (x64) ---');
  Writeln(Format('RAX=%.16x  RBX=%.16x  RCX=%.16x', [ctx^.Rax, ctx^.Rbx, ctx^.Rcx]));
  Writeln(Format('RDX=%.16x  RSI=%.16x  RDI=%.16x', [ctx^.Rdx, ctx^.Rsi, ctx^.Rdi]));
  Writeln(Format('RBP=%.16x  RSP=%.16x  RIP=%.16x', [ctx^.Rbp, ctx^.Rsp, ctx^.Rip]));
  Writeln(Format('R8 =%.16x  R9 =%.16x  R10=%.16x', [ctx^.R8, ctx^.R9, ctx^.R10]));
  Writeln(Format('R11=%.16x  R12=%.16x  R13=%.16x', [ctx^.R11, ctx^.R12, ctx^.R13]));
  Writeln(Format('R14=%.16x  R15=%.16x', [ctx^.R14, ctx^.R15]));

  // 64-bit XMM Registers (XMM0 - XMM5 shown as example low/high QWORDs)
  Writeln('--- XMM Registers (Vector Unit) ---');
  for I := 0 to 15 do
    Writeln(Format('XMM%d=Low:%.16x High:%.16x', [I, ctx^.VectorRegister[I].Low, ctx^.VectorRegister[I].High]));
  {$else}
  // 32-bit General Purpose Registers
  Writeln('--- General Purpose Registers (x86) ---');
  Writeln(Format('EAX=%.8x  EBX=%.8x  ECX=%.8x', [ctx^.Eax, ctx^.Ebx, ctx^.Ecx]));
  Writeln(Format('EDX=%.8x  ESI=%.8x  EDI=%.8x', [ctx^.Edx, ctx^.Esi, ctx^.Edi]));
  Writeln(Format('EBP=%.8x  ESP=%.8x  EIP=%.8x', [ctx^.Ebp, ctx^.Esp, ctx^.Eip]));
  {$endif}

  // Tell Windows we handled the exception (or use EXCEPTION_CONTINUE_SEARCH)
  Result := EXCEPTION_EXECUTE_HANDLER;
end;
{$endif}

var
  SE: TScriptEngine;
  SL: TStrings;
  IsD: Boolean = False;
  IsO: Boolean = True;
  IsA: Boolean = False;
  IsP: Boolean = True;
  I: Integer;
  AsmStr: String;
  {$ifdef windows}
  HandlerHandle: PVOID;
  {$endif}

begin
  {$ifdef windows}
  HandlerHandle := AddVectoredExceptionHandler(1, @MyVectoredHandler);
  if HandlerHandle = nil then
  begin
    Writeln('Failed to register vectored exception handler.');
    Exit;
  end;
  {$endif}
  if ParamCount < 1 then
  begin
    Writeln('Usage: evil <options> [script file]');
    Writeln('Options: ');
    Writeln(' -d  : Disassembly');
    Writeln(' -do : Disable optimizations');
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
    {$ifdef windows}
    RemoveVectoredExceptionHandler(@HandlerHandle);
    {$endif}
  end;
end.
