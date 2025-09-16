unit ScriptEngine;

{$mode objfpc}
{$ifdef CPUX86_64}
  {$asmmode intel}
{$endif}
{$H+}
{$macro on}
{$modeswitch nestedprocvars}
{$modeswitch advancedrecords}
// enable this if you want to handle UTF-8 strings (requires LCL)
{.$define SE_STRING_UTF8}
// use computed goto instead of case of
// try-catch will not work without computed goto!
{$ifndef AARCH64}
  {$ifndef WASI}
    {$define SE_COMPUTED_GOTO}
  {$endif}
{$endif}
// enable this if you want to use libffi to handle dynamic function calls
{.$define SE_LIBFFI}
{$if defined(CPU32) or defined(CPU64) or defined(SE_LIBFFI)}
  {$ifndef WASI}
    {$define SE_DYNLIBS}
  {$endif}
{$endif}
// enable this if you have access to LCL's FileUtil
{.$define SE_HAS_FILEUTIL}
// enable this if you want to print logs to terminal
{.$define SE_LOG}
// enable this if you need json support
{$define SE_HAS_JSON}
// enable this if you want to include this in castle game engine's profiler report
{.$define SE_PROFILER}
// enable this if you dont need to store map's keys as (utf8)strings. It will be stored as shortstrings instead, which speed up map operations.
{$define SE_MAP_SHORTSTRING}
// enable this to replace FP's TDirectory with avk959's TGChainHashMap. It is a lot faster than TDirectory.
// requires https://github.com/avk959/LGenerics
// note: enable this will undef SE_MAP_SHORTSTRING, because this optimization is not necessary for TGChainHashMap
{.$define SE_MAP_AVK959}
{$ifdef SE_MAP_AVK959}
  {$undef SE_MAP_SHORTSTRING}
  {$define TSEDictionary := TGChainHashMap}
{$else}
  {$define TSEDictionary := TDictionary}
{$endif}
// Enable this if you want multi-threading support
{$ifndef GO32v2}
  {$define SE_THREADS}
{$endif}
{$align 16}
{$packenum 4}

interface

uses
  SysUtils, Classes, Generics.Collections, StrUtils, Types, DateUtils, RegExpr, {$ifdef SE_THREADS}syncobjs,{$endif}
  contnrs,
  {$ifdef SE_PROFILER}
  CastleTimeUtils,
  {$endif}
  {$ifdef SE_MAP_AVK959}
  lghashmap,
  {$endif}
  base64,
  fpjson, jsonparser
  {$ifdef SE_HAS_FILEUTIL}, FileUtil{$endif}
  {$ifdef SE_LIBFFI}, ffi{$endif}
  {$ifdef SE_STRING_UTF8},LazUTF8{$endif}{$ifdef SE_DYNLIBS}, dynlibs{$endif};

const
  SE_STACK_RESERVED = 2;

type
  TSENumber = Double;

  TSEOpcode = (
    opPushConst,
    opPushConstString,
    opPushGlobalVar,
    opPushLocalVar,
    opPushVar2,
    opPushArrayPop,
    opPushArrayPopString,
    opPopConst,
    opPopFrame,
    opAssignGlobalVar,
    opAssignGlobalArray,
    opAssignLocalVar,
    opAssignLocalArray,
    opAssignArrayFast,
    opAssignMapFast,
    opJumpEqual,
    opJumpEqual1,
    opJumpUnconditional,
    opJumpEqualOrGreater2,
    opJumpEqualOrLesser2,

    opJumpEqual1Rel,
    opJumpUnconditionalRel,

    opOperatorInc,

    opOperatorAdd0,
    opOperatorMul0,
    opOperatorDiv0,

    opOperatorAdd1,
    opOperatorSub1,
    opOperatorMul1,
    opOperatorDiv1,

    opOperatorAdd2,
    opOperatorSub2,
    opOperatorMul2,
    opOperatorDiv2,

    opOperatorAdd,
    opOperatorSub,
    opOperatorMul,
    opOperatorDiv,
    opOperatorMod,
    opOperatorPow,
    opOperatorNegative,
    opOperatorLesser,
    opOperatorLesserOrEqual,
    opOperatorGreater,
    opOperatorGreaterOrEqual,
    opOperatorEqual,
    opOperatorNotEqual,
    opOperatorAnd,
    opOperatorOr,
    opOperatorXor,
    opOperatorNot,
    opOperatorShiftLeft,
    opOperatorShiftRight,

    opCallRef,
    opCallNative,
    opCallScript,
    opCallImport,
    opYield,
    opHlt,

    {$ifdef UNIX}
    opBlockCleanup,
    {$endif}
    opPushTrap,
    opPopTrap,
    opThrow
  );
  TSEOpcodes = set of TSEOpcode;
  TSEOpcodeInfo = record
    Op: TSEOpcode;
    Pos: Integer;
    Binary: Pointer;
    Size: Integer;
  end;
  PSEOpcodeInfo = ^TSEOpcodeInfo;
  TSEOpcodeInfoListAncestor = specialize TList<TSEOpcodeInfo>;
  TSEOpcodeInfoList = class(TSEOpcodeInfoListAncestor)
  public
    function Ptr(const P: Cardinal): PSEOpcodeInfo;
  end;

  TSENestedProc = procedure is nested;

  TSEValueKind = (
    sevkNull,
    sevkNumber,
    sevkString,
    sevkMap,
    sevkBuffer,
    sevkPointer,
    sevkBoolean,
    sevkFunction,
    sevkPascalObject,
    sevkPackedString
  );
  PSECommonString = ^String;
  TSEBuffer = record
    Base: Pointer;
    Ptr: Pointer;
  end;
  PSEBuffer = ^TSEBuffer;
  TSEPascalObject = record
    Value: TObject;
    IsManaged: Boolean;
  end;
  PSEPascalObject = ^TSEPascalObject;

  TSEListStack = specialize TStack<TList>;
  TSEScopeStack = specialize TStack<Integer>;
  TSEIntegerList = specialize TList<Integer>;
  TSECardinalList = specialize TList<Cardinal>;
  TSEVM = class;
  TSEVMList = specialize TList<TSEVM>;

  TSEFuncKind = (sefkNative, sefkScript, sefkImport);

  PSEValue = ^TSEValue;
  TSEValue = record
    Ref: Cardinal;
    case Kind: TSEValueKind of
      sevkNumber:
        (
          VarNumber: TSENumber;
        );
      sevkString:
        (
          VarString: PSECommonString;
        );
      sevkMap:
        (
          VarMap: TObject;
        );
      sevkBuffer:
        (
          VarBuffer: PSEBuffer;
        );
      sevkPointer:
        (
          VarPointer: Pointer;
        );
      sevkNull:
        (
          VarNull: Pointer;
        );
      sevkBoolean:
        (
          VarBoolean: Boolean;
        );
      sevkFunction:
        (
          VarFuncKind: TSEFuncKind;
          VarFuncIndx: Cardinal;
        );
      sevkPascalObject:
        (
          VarPascalObject: PSEPascalObject;
        );
      sevkPackedString:
        (
          VarPackedString: array[0..7] of Char;
        );
  end;

  PSEStackTraceSymbol = ^TSEStackTraceSymbol;
  TSEStackTraceSymbol = record
    Name,
    Value: String;
    Kind: TSEValueKind;
    Ref: PSEValue;
    Childs: array of TSEStackTraceSymbol;
  end;
  TSEStackTraceSymbolArray = array of TSEStackTraceSymbol;
  TSEStackTraceSymbolProc = procedure(Message: String; Nodes: TSEStackTraceSymbolArray) of object;

  TSEValueHelper = record helper for TSEValue
    procedure AllocBuffer(constref Size: Integer); inline;
    procedure AllocMap; inline;
    procedure AllocString(const S: String); inline;
    procedure AllocPascalObject(const Obj: TObject; const IsManaged: Boolean); inline;
    function GetValue(constref I: Integer): TSEValue; inline; overload;
    function GetValue(constref S: String): TSEValue; inline; overload;
    function GetValue(constref I: TSEValue): TSEValue; inline; overload;
    procedure SetValue(constref I: Integer; const A: TSEValue); inline; overload;
    procedure SetValue(constref S: String; const A: TSEValue); inline; overload;
    procedure SetValue(I: TSEValue; const A: TSEValue); inline; overload;
    function ContainsKey(constref S: String): Boolean; inline; overload;
    procedure UnManaged; inline;
    procedure Managed; inline;
    function Clone: TSEValue; inline;
    function IsValidArray: Boolean; inline;
    procedure FromJSON(constref S: String);
    function ToJSON: String;
    function ToString: String;
    function Size: Integer;
  end;

  TSEValueDict = specialize TSEDictionary<{$ifdef SE_MAP_SHORTSTRING}ShortString{$else}String{$endif}, TSEValue>;
  TSEValueMap = class(specialize TList<TSEValue>)
  private
    FIsValidArray: Boolean;
    FMap: TSEValueDict;
    {$ifdef SE_THREADS}
    FLock: TRTLCriticalSection;
    {$endif}
  public
    constructor Create;
    destructor Destroy; override;
    procedure Lock; inline;
    procedure Unlock; inline;
    function TryLock: Boolean; inline;
    procedure ToMap;
    procedure Set2(const Key: String; const AValue: TSEValue); overload; inline;
    procedure Set2(const Index: Int64; const AValue: TSEValue); overload; inline;
    function Get2(const Key: String): TSEValue; overload; inline;
    function Get2(const Index: Int64): TSEValue; overload; inline;
    procedure Del2(const Key: String); overload; inline;
    procedure Del2(const Index: Int64); overload; inline;
    function Ptr(const I: Integer): PSEValue;
    property Map: TSEValueDict read FMap;
    property IsValidArray: Boolean read FIsValidArray;
  end;
  TSEValueArray = array of TSEValue;
  PPSEValue = ^PSEValue;

  TSEValueListAncestor = specialize TList<TSEValue>;
  TSEValueList = class(TSEValueListAncestor)
  public
    function Ptr(const P: Cardinal): PSEValue;
  end;

  TSEBinary = class(TSEValueList)
  public
    BinaryName: String;
  end;

  PSEGCNode = ^TSEGCNode;
  TSEGCNode = record
    Value: TSEValue;
    Garbage: Boolean;
    Lock: Boolean;
    Visit: Byte;
    Marked,
    Prev,
    Next: Cardinal;
  end;
  TSEGCNodeListAncestor = specialize TList<TSEGCNode>;
  TSEGCNodeList = class(TSEGCNodeListAncestor)
  public
    function Ptr(const P: Cardinal): PSEGCNode;
  end;
  TSEGCNodeAvailStack = specialize TStack<Integer>;

  TSEGarbageCollectorPhase = (
    segcpRest,
    segcpInitial,
    segcpMark,
    segcpSweep
  );

  {$ifdef SE_THREADS}
  TSEGarbageCollectorMarkJob = class(TThread)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Execute; override;
  end;
  {$endif}

  TSEGarbageCollector = class
  private
    FVMThreadList: TSEVMList;
    FPhase: TSEGarbageCollectorPhase;
    FLockFlag: Boolean;
    {$ifdef SE_THREADS}
    FLock: TRTLCriticalSection;
    {$endif}
    FObjects,
    FObjectThreshold,
    FObjectsLastTimeVisited,
    FObjectsOld: Cardinal;
    FReachableValueList: TSEValueList;
    FNodeList: TSEGCNodeList;
    FNodeAvailStack: TSEGCNodeAvailStack;
    FNodeLastYoung,
    FNodeLastOld: Cardinal;
    FRunCount: Cardinal;
    FTicks: QWord;
    FInterval: Cardinal;
    FPromotion: Byte;
    FOldObjectCheckCycle: Byte;
    FEnableParallel: Boolean;
    procedure Initial;
    procedure Sweep(const AFirst: Cardinal);
    procedure Mark(const PValue: PSEValue);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddToList(const PValue: PSEValue);
    procedure CheckForGC;
    procedure CheckForGCFast;
    procedure GC(const Forced: Boolean = False);
    procedure AllocBuffer(const PValue: PSEValue; const Size: Integer);
    procedure AllocMap(const PValue: PSEValue);
    procedure AllocString(const PValue: PSEValue; const S: String);
    procedure AllocPascalObject(const PValue: PSEValue; const Obj: TObject; const IsManaged: Boolean);
    procedure UnManaged(const PValue: PSEValue);
    procedure Managed(const PValue: PSEValue);
    procedure Lock;
    procedure Unlock;
    property ValueList: TSEGCNodeList read FNodeList;
    property ObjectCount: Cardinal read FObjects;
    property OldObjectCount: Cardinal read FObjectsOld;
    property RunCount: Cardinal read FRunCount;
    property Interval: Cardinal read FInterval write FInterval;
    property Promotion: Byte read FPromotion write FPromotion;
    property OldObjectCheckCycle: Byte read FOldObjectCheckCycle write FOldObjectCheckCycle;
    property ObjectThreshold: Cardinal read FObjectThreshold write FObjectThreshold;
    property ReachableValueList: TSEValueList read FReachableValueList;
    property Phase: TSEGarbageCollectorPhase read FPhase write FPhase;
    property EnableParallel: Boolean read FEnableParallel write FEnableParallel;
  end;

  TSECallingConvention = (
    seccAuto,
    seccStdcall,
    seccCdecl
  );

  TSEAtomKind = (
    seakVoid,
    seakI8,
    seakI16,
    seakI32,
    seakI64,
    seakU8,
    seakU16,
    seakU32,
    seakU64,
    seakF32,
    seakF64,
    seakBuffer,
    seakWBuffer
  );
  TSEAtomKindArray = array of TSEAtomKind;

  TSEFuncNativeKind = (sefnkNormal, sefnkSelf);
  TSEFunc = function(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue of object;
  TSEFuncWithSelf = function(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: TSEValue): TSEValue of object;

  TSEFuncNativeInfo = record
    Name: String;
    Func: TSEFunc;
    ArgCount: Integer;
    Kind: TSEFuncNativeKind;
  end;
  PSEFuncNativeInfo = ^TSEFuncNativeInfo;

  TSEFuncScriptInfo = record
    Name: String;
    BinaryPos: Integer;
    ArgCount: Integer;
    VarCount: Integer;
    VarSymbols: TStrings;
  end;
  PSEFuncScriptInfo = ^TSEFuncScriptInfo;

  TSEFuncImportInfo = record
    Name: String;
    Func: Pointer;
    Args: TSEAtomKindArray;
    Return: TSEAtomKind;
    CallingConvention: TSECallingConvention;
  end;
  PSEFuncImportInfo = ^TSEFuncImportInfo;

  TSEFuncNativeListAncestor = specialize TList<TSEFuncNativeInfo>;
  TSEFuncNativeList = class(TSEFuncNativeListAncestor)
  public
    function Ptr(const P: Cardinal): PSEFuncNativeInfo;
  end;

  TSEFuncScriptListAncestor = specialize TList<TSEFuncScriptInfo>;
  TSEFuncScriptList = class(TSEFuncScriptListAncestor)
  public
    function Ptr(const P: Cardinal): PSEFuncScriptInfo;
  end;

  TSEFuncImportListAncestor = specialize TList<TSEFuncImportInfo>;
  TSEFuncImportList = class(TSEFuncImportListAncestor)
  public
    function Ptr(const P: Cardinal): PSEFuncImportInfo;
  end;

  TSESymbolKind = (
    sesConst
  );

  TSESymbol = record
    Name: String;
    Kind: TSESymbolKind;
    Binary: Integer;
    Code: Integer;
  end;
  PSESymbol = ^TSESymbol;
  TSESymbolListAncestor = specialize TList<TSESymbol>;
  TSESymbolList = class(TSESymbolListAncestor)
  public
    function Ptr(const P: Cardinal): PSESymbol;
  end;

  TSELineOfCode = record
    BinaryCount: Integer;
    BinaryPtr: Integer;
    Line: Integer;
    Module: String;
  end;
  TSELineOfCodeList = specialize TList<TSELineOfCode>;

  TSEConstMap = specialize TSEDictionary<String, TSEValue>;
  TSEStack = TSEValueListAncestor;
  TSEVarMap = TSEValue;
  TSEFrame = record
    Code: Integer;
    Stack: PSEValue;
    Binary: Integer;
    Func: PSEFuncScriptInfo;
  end;
  PSEFrame = ^TSEFrame;
  TSETrap = record
    FramePtr: PSEFrame;
    Stack: PSEValue;
    Binary: Integer;
    CatchCode: Integer;
  end;
  PSETrap = ^TSETrap;

  PSEValueArrayManagedRecord = ^TSEValueArrayManagedRecord;
  TSEValueArrayManagedRecord = record
    Data: PSEValue;
    Size: Cardinal;
    RefCount: Integer;
  end;

  TSEValueArrayManaged = record
    Value: PSEValueArrayManagedRecord;
    procedure Alloc(const ASize: Cardinal);
    function Ref: TSEValueArrayManaged;
    procedure Free;
  end;

  PSEBinariesManagedRecord = ^TSEBinariesManagedRecord;
  TSEBinariesManagedRecord = record
    Data: array of TSEBinary;
    Size: Cardinal;
    RefCount: Integer;
  end;

  TSEBinariesManaged = record
    Value: PSEBinariesManagedRecord;
    procedure Alloc(const ASize: Cardinal);
    function Ref: TSEBinariesManaged;
    procedure Free;
  end;

  {$ifdef SE_THREADS}
  TSEVMThread = class(TThread)
  public
    IsDone: Boolean;
    IsRequestForSuspendByGC: Boolean;
    VM: TSEVM;
    constructor Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
    destructor Destroy; override;
    procedure Execute; override;
  end;
  TSEVMThreadList = specialize TList<TSEVMThread>;
  {$endif}

  TSEVMCoroutine = class
    FStackPtr: PSEValue;
    FBinaryPtr: Integer;
    IsDone: Boolean;
    IsExecuting: Boolean;
    IsTerminated: Boolean;
    VM: TSEVM;
    constructor Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
    destructor Destroy; override;
    function Execute: TSEValue;
    procedure Reset(const Fn: TSEValue; const Args: PSEValue; const ArgCount: Cardinal);
  end;
  TSEVMCoroutineList = specialize TList<TSEVMCoroutine>;

  TEvilC = class;
  TSEVM = class
  public
    Name: String;
    {$ifdef SE_THREADS}
    ThreadOwner: TSEVMThread;
    {$endif}
    CoroutineOwner: TSEVMCoroutine;
    IsPaused: Boolean;
    IsDone: Boolean;
    IsYielded: Boolean;
    {$ifdef UNIX}
    IsRequestForSuspend: Boolean;
    {$endif}
    Global: TSEValueArrayManaged;
    Stack: array of TSEValue;
    Frame: array of TSEFrame;
    Trap: array of TSETrap;
    ConstStrings: TStringList;
    CodePtr: Integer;
    StackPtr: PSEValue;
    BinaryPtr: Integer;
    FramePtr: PSEFrame;
    TrapPtr: PSETrap;
    StackSize: Integer;
    FrameSize: Integer;
    TrapSize: Integer;
    Parent: TEvilC;
    Binaries: TSEBinariesManaged;
    SymbolList: TSESymbolList;

    constructor Create;
    destructor Destroy; override;
    procedure Reset;
    procedure Exec;
    procedure BinaryClear;
    function Fork(const AStackSize: Cardinal): TSEVM;
    procedure SetGlobalVariable(const AName: String; const AValue: TSEValue);
    function GetGlobalVariable(const AName: String): PSEValue;
    procedure ModifyGlobalVariable(const AName: String; const AValue: TSEValue);
  end;

  TSECache = record
    Binaries: array of TSEBinary;
    GlobalVarCount: Cardinal;
    GlobalVarSymbols: TStrings;
    LineOfCodeList: TSELineOfCodeList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
    ConstStrings: TStringList;
    SymbolList: TSESymbolList;
  end;
  TSECacheMapAncestor = specialize TSEDictionary<String, TSECache>;
  TSECacheMap = class(TSECacheMapAncestor)
  public
    procedure ClearSingle(const AName: String);
    procedure Clear;
  end;

  TSETokenKind = (
    tkEOF,
    tkDot,
    tkAdd,
    tkSub,
    tkMul,
    tkDiv,
    tkMod,
    tkPow,
    tkShiftLeft,
    tkShiftRight,
    tkOpAssign,
    tkEqual,
    tkNotEqual,
    tkSmaller,
    tkGreater,
    tkSmallerOrEqual,
    tkGreaterOrEqual,
    tkBegin,
    tkEnd,
    tkColon,
    tkQuestion,
    tkBracketOpen,
    tkBracketClose,
    tkNegative,
    tkNumber,
    tkString,
    tkComma,
    tkIf,
    tkSwitch,
    tkCase,
    tkDefault,
    tkIdent,
    tkFunction,
    tkFunctionDecl,
    tkVariable,
    tkConst,
    tkLocal,
    tkUnknown,
    tkElse,
    tkWhile,
    tkBreak,
    tkContinue,
    tkYield,
    tkSquareBracketOpen,
    tkSquareBracketClose,
    tkAnd,
    tkOr,
    tkXor,
    tkNot,
    tkFor,
    tkIn,
    tkTo,
    tkDownto,
    tkStep,
    tkReturn,
    tkAtom,
    tkImport,
    tkDo,
    tkTry,
    tkCatch,
    tkThrow,
    tkAccess
  );
TSETokenKinds = set of TSETokenKind;

const
  TokenNames: array[TSETokenKind] of String = (
    'EOF', '.', '+', '-', '*', 'div', 'mod', '^', '<<', '>>', 'operator assign', '=', '!=', '<',
    '>', '<=', '>=', '{', '}', ':', '?', '(', ')', 'neg', 'number', 'string',
    ',', 'if', 'switch', 'case', 'default', 'identity', 'function', 'fn', 'variable', 'const', 'local',
    'unknown', 'else', 'while', 'break', 'continue', 'yield',
    '[', ']', 'and', 'or', 'xor', 'not', 'for', 'in', 'to', 'downto', 'step', 'return',
    'atom', 'import', 'do', 'try', 'catch', 'throw', 'access'
  );
  ValueKindNames: array[TSEValueKind] of String = (
    'null', 'number', 'string', 'map', 'buffer', 'pointer', 'boolean', 'function', 'pasobject', 'packedstring'
  );
  OpcodeSizes: array[TSEOpcode] of Byte = (
    2, // opPushConst,
    2, // opPushConstString,
    2, // opPushGlobalVar,
    3, // opPushLocalVar,
    5, // opPushVar2,
    2, // opPushArrayPop,
    2, // opPushArrayPopString,
    1, // opPopConst,
    1, // opPopFrame,
    2, // opAssignGlobalVar,
    3, // opAssignGlobalArray,
    3, // opAssignLocalVar,
    4, // opAssignLocalArray,
    4, // opAssignArrayFast,
    4, // opAssignMapFast,
    2, // opJumpEqual,
    3, // opJumpEqual1,
    2, // opJumpUnconditional,
    6, // opJumpEqualOrGreater2,
    6, // opJumpEqualOrLesser2,

    3, // opJumpEqual1Rel,
    2, // opJumpUnconditionalRel,

    4, // opOperatorInc,

    2, // opOperatorAdd0,
    2, // opOperatorMul0,
    2, // opOperatorDiv0,

    3, // opOperatorAdd1,
    3, // opOperatorSub1,
    3, // opOperatorMul1,
    3, // opOperatorDiv1,

    5, // opOperatorAdd2,
    5, // opOperatorSub2,
    5, // opOperatorMul2,
    5, // opOperatorDiv2,

    1, // opOperatorAdd,
    1, // opOperatorSub,
    1, // opOperatorMul,
    1, // opOperatorDiv,
    1, // opOperatorMod,
    1, // opOperatorPow,
    1, // opOperatorNegative,
    1, // opOperatorLesser,
    1, // opOperatorLesserOrEqual,
    1, // opOperatorGreater,
    1, // opOperatorGreaterOrEqual,
    1, // opOperatorEqual,
    1, // opOperatorNotEqual,
    1, // opOperatorAnd,
    1, // opOperatorOr,
    1, // opOperatorXor,
    1, // opOperatorNot,
    1, // opOperatorShiftLeft,
    1, // opOperatorShiftRight,

    4, // opCallRef,
    4, // opCallNative,
    4, // opCallScript,
    4, // opCallImport,
    1, // opYield,
    1, // opHlt,

    {$ifdef UNIX}
    1, // opBlockCleanup
    {$endif}
    2, // opPushTrap,
    1, // opPopTrap,
    1  // opThrow
  );

type
  TSEIdentKind = (
    ikVariable,
    ikFunc
  );

  TSEIdent = record
    Kind: TSEIdentKind;
    Addr: Integer;
    IsUsed: Boolean;
    IsAssigned: Boolean;
    IsConst: Boolean;
    ConstValue: TSEValue;
    Local: Integer;
    Block: Integer;
    Ln: Integer;
    Col: Integer;
    Name: String;
  end;
  PSEIdent = ^TSEIdent;

  TSEIdentListAncestor = specialize TList<TSEIdent>;
  TSEIdentList = class(TSEIdentListAncestor)
  public
    function Ptr(const P: Cardinal): PSEIdent;
  end;

  TSEToken = record
    Kind: TSETokenKind;
    BelongedFileName,
    Value: String;
    Ln, Col: Integer;
  end;
  PSEToken = ^TSEToken;
  TSETokenList = specialize TList<TSEToken>;

  TEvilC = class
  private
    FSource: String;
    FInternalIdentCount: QWord;
    procedure SetSource(V: String);
    function InternalIdent: String;
  public
    Owner: TObject;
    OptimizePeephole,         // True = enable peephole optimization, default is true
    OptimizeConstantFolding,  // True = enable constant folding optimization, default is true
    OptimizeAsserts: Boolean; // True = ignore assert, default is true
    ErrorLn, ErrorCol: Integer;
    VM: TSEVM;
    {$ifdef SE_THREADS}
    VMThreadList: TSEVMThreadList;
    {$endif}
    IncludePathList,
    IncludeList: TStrings;
    TokenList: TSETokenList;
    OpcodeInfoList: TSEOpcodeInfoList;
    LocalVarCountList: TSEIntegerList;
    GlobalVarCount: Integer;
    GlobalVarSymbols: TStrings;
    VarList: TSEIdentList;
    FuncNativeList: TSEFuncNativeList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
    ConstMap: TSEConstMap;
    ScopeStack: TSEScopeStack;
    ScopeFunc: TSEScopeStack;
    LineOfCodeList: TSELineOfCodeList;
    StackTraceHandler: TSEStackTraceSymbolProc;
    IsLex,
    IsParsed: Boolean;
    IsDone: Boolean;
    FuncCurrent: Integer;
    FuncTraversal: Integer;
    BlockTraversal: Integer;
    CurrentFileList: TStrings;
    BinaryPos: Integer; // This is mainly for storing line of code for runtime
    Binary: TSEBinary; // Current working binary
    constructor Create(const StackSize: LongWord = 2048);
    destructor Destroy; override;
    procedure AddDefaultConsts;
    function GetIsPaused: Boolean;
    procedure SetIsPaused(V: Boolean);
    function IsYielded: Boolean;
    procedure Lex(const IsIncluded: Boolean = False);
    procedure Parse;
    procedure Reset;
    function Exec: TSEValue;
    // Execute a function only, currently this does not support yield!
    function ExecFuncOnly(const Name: String; const Args: array of TSEValue): TSEValue; overload;
    // This method is equivalent of calling Exec(), then ExecFuncOnly()
    function ExecFunc(const Name: String; const Args: array of TSEValue): TSEValue; overload;
    function ExecFuncOnly(const AIndex: Integer; const Args: array of TSEValue): TSEValue; overload;
    function ExecFunc(const AIndex: Integer; const Args: array of TSEValue): TSEValue; overload;
    procedure RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: Integer);
    procedure RegisterFuncWithSelf(const Name: String; const Func: TSEFuncWithSelf; const ArgCount: Integer);
    function RegisterScriptFunc(const Name: String; const ArgCount: Integer): PSEFuncScriptInfo;
    procedure RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind; const CC: TSECallingConvention = seccAuto);
    function Backup: TSECache;
    procedure Restore(const Cache: TSECache);
    function FindFunc(const Name: String): Pointer; inline; overload;
    function FindFuncNative(const Name: String; var Ind: Integer): PSEFuncNativeInfo; inline;
    function FindFuncScript(const Name: String; var Ind: Integer): PSEFuncScriptInfo; inline;
    function FindFuncImport(const Name: String; var Ind: Integer): PSEFuncImportInfo; inline;
    function FindFunc(const Name: String; var Kind: TSEFuncKind; var Ind: Integer): Pointer; inline; overload;
    procedure PatchSymbols;

    property IsPaused: Boolean read GetIsPaused write SetIsPaused;
    property Source: String read FSource write SetSource;
  end;

  TScriptEngine = TEvilC;

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
function SESize(constref Value: TSEValue): Cardinal; inline;
procedure SEValidateType(V: PSEValue; Expected: TSEValueKind; At: DWord; const FuncName: String); inline;
procedure SEMapDelete(constref V: TSEValue; constref I: Integer); inline; overload;
procedure SEMapDelete(constref V: TSEValue; constref S: String); inline; overload;
procedure SEMapDelete(constref V, I: TSEValue); inline; overload;
function SEMapGet(constref V: TSEValue; constref I: Integer): TSEValue; inline; overload;
function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
procedure SEMapSet(constref V: TSEValue; constref I: Integer; const A: TSEValue); inline; overload;
procedure SEMapSet(constref V: TSEValue; constref S: String; const A: TSEValue); inline; overload;
procedure SEMapSet(constref V, I: TSEValue; const A: TSEValue); inline; overload;
function SEMapIsValidArray(constref V: TSEValue): Boolean; inline;
procedure SEDisAsm(const VM: TSEVM; var Res: String);
function SEGet(const AName: String): TSEValue;
procedure SESet(const AName: String; const AValue: TSEValue);

operator := (V: TSENumber) R: TSEValue;
operator := (V: String) R: TSEValue;
operator := (V: Boolean) R: TSEValue;
operator := (V: TSEValueArray) R: TSEValue;
operator := (V: Pointer) R: TSEValue;
operator := (V: TSEValue) R: Integer;
{$ifdef CPU64}
operator := (V: TSEValue) R: Int64;
{$endif}
operator := (V: TSEValue) R: Boolean;
operator := (V: TSEValue) R: TSENumber;
operator := (V: TSEValue) R: String;
operator := (V: TSEValue) R: TSEValueArray;
operator := (V: TSEValue) R: Pointer;
operator + (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator + (V1: TSEValue; V2: String) R: TSEValue;
operator + (V1: TSEValue; V2: Pointer) R: TSEValue;
operator - (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator - (V1: TSEValue; V2: Pointer) R: TSEValue;
operator * (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator / (V1: TSEValue; V2: TSENumber) R: TSEValue;
operator + (V1, V2: TSEValue) R: TSEValue;
operator - (V1, V2: TSEValue) R: TSEValue;
operator - (V: TSEValue) R: TSEValue;
operator * (V1, V2: TSEValue) R: TSEValue;
operator / (V1, V2: TSEValue) R: TSEValue;
operator < (V1: TSEValue; V2: TSENumber) R: Boolean;
operator > (V1: TSEValue; V2: TSENumber) R: Boolean;
operator <= (V1: TSEValue; V2: TSENumber) R: Boolean;
operator >= (V1: TSEValue; V2: TSENumber) R: Boolean;
operator = (V1: TSEValue; V2: TSENumber) R: Boolean;
operator <> (V1: TSEValue; V2: String) R: Boolean;
operator < (V1, V2: TSEValue) R: Boolean;
operator > (V1, V2: TSEValue) R: Boolean;
operator <= (V1, V2: TSEValue) R: Boolean;
operator >= (V1, V2: TSEValue) R: Boolean;
operator = (V1, V2: TSEValue) R: Boolean;
operator <> (V1, V2: TSEValue) R: Boolean;

var
  ScriptVarMap: TSEVarMap;
  GC: TSEGarbageCollector;
  {$ifdef SE_THREADS}
  GCMarkJob: TSEGarbageCollectorMarkJob;
  {$endif}
  ScriptCacheMap: TSECacheMap;
  SENull: TSEValue;
  JumpTable: array[TSEOpcode] of Pointer;

implementation

uses
  Math, Strings;

const
  SE_REG_GLOBAL = $FFFFFFFF;

type
  TBuiltInFunction = class
    class function SEBufferCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferLength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferFillF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferGetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferSetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringToBuffer(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEWBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEArrayToBufferF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEArrayToBufferF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferToArrayF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBufferToArrayF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;

    class function SETypeOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEKindOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEWrite(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEWriteln(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SERandom(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SERnd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SERound(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFloor(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECeil(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SETrunc(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SENumber(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SELength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMapCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMapClone(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMapKeyDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMapKeysGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMapClear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEArrayResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEArrayToMap(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEArrayFill(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SELerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESLerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESign(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECos(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SETan(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECot(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESqrt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEAbs(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFrac(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SERange(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEMax(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEPow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SESleep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringGrep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringSplit(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringFind(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringInsert(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringCompare(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringReplace(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringReplaceIgnoreCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringFormat(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringUpperCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringLowerCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringFindRegex(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringTrim(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringTrimLeft(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringTrimRight(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringExtractName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringExtractPath(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEStringExtractExt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEGetTickCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTNow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTSetDate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTSetTime(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTDayAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTMonthAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTYearAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTGetYear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTGetMonth(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTGetDay(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTGetHour(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDTGetMinute(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEGCObjectCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEGCObjectOldCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEGCCollect(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEChar(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEOrd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineResume(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECoroutineIsExecuting(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    {$ifdef SE_THREADS}
    class function SEThreadCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEThreadStart(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEThreadIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEThreadSuspend(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEThreadTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEThreadWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECriticalCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECriticalEnter(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECriticalLeave(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SECriticalTry(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEEventCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEEventSet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEEventWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEEventReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    {$endif}
    class function SEFileReadText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileReadBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileWriteText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileWriteBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileRename(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileGetSize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEFileGetAge(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDirectoryCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDirectoryDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDirectoryFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEDirectoryExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;

    class function SEBase64Encode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEBase64Decode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;

    class function SEJSONParse(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
    class function SEJSONStringify(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;

    class function SEPasObjectClassName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  end;

  TDynlibMap = specialize TSEDictionary<String, TLibHandle>;

var
  DynlibMap: TDynlibMap;
  VMList: TSEVMList;
  {$ifdef SE_THREADS}
  CS: TRTLCriticalSection;
  {$endif}
  FS: TFormatSettings;
  CommonNativeFuncList: TSEFuncNativeList;
  FunctionAssert: array of TSEValue;
  FunctionThrow: array of TSEValue;

{$ifdef SE_THREADS}
threadvar
{$endif}
  IsThread: Cardinal;

function PointStrToFloat(S: String): Double; inline;
begin
  Result := StrToFloat(S, FS);
end;

function PointFloatToStr(X: Double): String; inline;
begin
  Result := FloatToStr(X, FS);
end;

function ReadFileAsString(const Name: String): String; overload;
var
  MS: TMemoryStream;
begin
  if not FileExists(Name) then
    Exit;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(Name);
    if MS.Size > 0 then
    begin
      SetLength(Result, MS.Size div SizeOf(Char));
      MS.ReadBuffer(Pointer(Result)^, MS.Size div SizeOf(Char));
    end;
  finally
    MS.Free;
  end;
end;

procedure ReadFileAsString(const Name: String; var Str: String); overload;
var
  MS: TMemoryStream;
begin
  if not FileExists(Name) then
    Exit;
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(Name);
    if MS.Size > 0 then
    begin
      SetLength(Str, MS.Size div SizeOf(Char));
      MS.ReadBuffer(Pointer(Str)^, MS.Size div SizeOf(Char));
    end;
  finally
    MS.Free;
  end;
end;

function GetOS: String; inline;
begin
  {$if defined(WINDOWS)}
  Result := 'windows';
  {$elseif defined(LINUX)}
  Result := 'linux';
  {$elseif defined(DARWIN)}
  Result := 'darwin';
  {$elseif defined(FREEBSD)}
  Result := 'freebsd';
  {$elseif defined(WASI)}
  Result := 'wasi';
  {$elseif defined(GO32v2)}
  Result := 'dos';
  {$else}
  Result := 'unknown';
  {$endif}
end;

procedure SEValidateType(V: PSEValue; Expected: TSEValueKind; At: DWord; const FuncName: String); inline;
var
  S1, S2: String;
begin
  if V^.Kind <> Expected then
  begin
    WriteStr(S1, Expected);
    WriteStr(S2, V^.Kind);
    raise Exception.Create(Format('[%s] Parameter #%d: Expected %s, got %s', [FuncName, At, S1, S2]));
  end;
end;

function StringIndexOf(S, P: String): Integer; inline;
begin
  {$ifdef SE_STRING_UTF8}
  Result := UTF8Pos(P, S);
  Dec(Result);
  {$else}
  Result := S.IndexOf(P);
  {$endif}
end;

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
var
  Key, S: String;
  IsValidArray: Boolean;
  I: Integer = 0;
begin
  case Value.Kind of
    sevkString:
      begin
        if IsRoot then
          Result := Value.VarString^
        else
          Result := '"' + Value.VarString^ + '"';
      end;
    sevkNumber:
      Result := PointFloatToStr(Value.VarNumber);
    sevkBoolean:
      Result := BoolToStr(Boolean(Round(Value.VarNumber)), 'true', 'false');
    sevkMap:
      begin
        Result := '[';
        IsValidArray := SEMapIsValidArray(Value);
        if IsValidArray then
        begin
          for I := 0 to TSEValueMap(Value.VarMap).Count - 1 do
          begin
            if I > 0 then
              Result := Result + ', ';
            Result := Result + SEValueToText(SEMapGet(Value, I), False);
          end;
        end else
        begin
          TSEValueMap(Value.VarMap).Lock;
          try
            for Key in TSEValueMap(Value.VarMap).Map.Keys do
            begin
              if I > 0 then
                Result := Result + ', ';
              Result := Result + '"' + Key + '": ' + SEValueToText(SEMapGet(Value, Key), False);
              Inc(I);
            end;
          finally
            TSEValueMap(Value.VarMap).Unlock;
          end;
        end;
        Result := Result + ']'
      end;
    sevkFunction:
      begin
        WriteStr(S, Value.VarFuncKind);
        Result := 'fn@' + S + ':' + IntToStr(Value.VarFuncIndx);
      end;
    sevkNull:
      Result := 'null';
    sevkBuffer:
      begin
        Result := 'buffer@' + IntToStr(QWord(Value.VarBuffer^.Ptr));
        if Value.VarBuffer^.Base <> nil then
        begin
          Result := Result + ' <' + IntToStr(MemSize(Value.VarBuffer^.Base) - 16) + ' bytes>';
        end;
      end;
    sevkPointer:
      begin
        Result := IntToStr(Integer(Value.VarPointer));
      end;
    sevkPascalObject:
      begin
        Result := 'pasobject@' + IntToStr(QWord(Value.VarPascalObject^.Value));
      end;
    sevkPackedString:
      Result := '.' + Value.VarPackedString;
    else
      Result := Value;
  end;
end;

function SESize(constref Value: TSEValue): Cardinal; inline;
begin
  case Value.Kind of
    sevkMap:
      begin
        if SEMapIsValidArray(Value) then
          Result := TSEValueMap(Value.VarMap).Count
        else
          Result := TSEValueMap(Value.VarMap).Map.Count;
      end;
    sevkBuffer:
      begin
        Result := MemSize(Value.VarBuffer^.Base) - 16;
      end;
    sevkString:
      begin
        Result := Length(Value.VarString^);
      end;
    else
      Result := -1;
  end;
end;

procedure SEMapDelete(constref V: TSEValue; constref I: Integer); inline; overload;
begin
  TSEValueMap(V.VarMap).Del2(I);
end;

procedure SEMapDelete(constref V: TSEValue; constref S: String); inline; overload;
begin
  TSEValueMap(V.VarMap).Del2(S);
end;

procedure SEMapDelete(constref V, I: TSEValue); inline; overload;
begin
  case I.Kind of
    sevkString:
      begin
        TSEValueMap(V.VarMap).Del2(I.VarString^);
      end;
    sevkNumber:
      begin
        TSEValueMap(V.VarMap).Del2(Round(I.VarNumber));
      end;
  end;
end;



function SEMapGet(constref V: TSEValue; constref I: Integer): TSEValue; inline; overload;
begin
  try
    Result := TSEValueMap(V.VarMap).Items[I];
  except
    Result := SENull;
  end;
end;

function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
begin
  try
    Result := TSEValueMap(V.VarMap).Map[S];
  except
    Result := SENull;
  end;
end;

function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
begin
  try
    case I.Kind of
      sevkString:
        begin
          Result := TSEValueMap(V.VarMap).Map[I.VarString^];
        end;
      sevkNumber:
        begin
          Result := TSEValueMap(V.VarMap).Items[Round(I.VarNumber)];
        end;
      sevkPackedString:
        begin
          Result := TSEValueMap(V.VarMap).Map[I.VarPackedString];
        end;
      else
        Exit(SENull);
    end;
  except
    Result := SENull;
  end;
end;

procedure SEMapSet(constref V: TSEValue; constref I: Integer; const A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(I, A);
end;

procedure SEMapSet(constref V: TSEValue; constref S: String; const A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(S, A);
end;

procedure SEMapSet(constref V, I: TSEValue; const A: TSEValue); inline; overload;
begin
  case I.Kind of
    sevkString:
      TSEValueMap(V.VarMap).Set2(I.VarString^, A);
    sevkNumber:
      TSEValueMap(V.VarMap).Set2(Round(I.VarNumber), A);
    sevkPackedString:
      TSEValueMap(V.VarMap).Set2(I.VarPackedString, A);
  end;
end;

function SEMapIsValidArray(constref V: TSEValue): Boolean; inline;
begin
  if V.Kind <> sevkMap then
    Exit(False);
  Result := TSEValueMap(V.VarMap).IsValidArray;
end;

procedure SEDisAsm(const VM: TSEVM; var Res: String);
var
  I, J, K: Integer;
  SB: TStringBuilder;
  Binary: TSEBinary;
  Op: TSEOpcode;
  S: String;
begin
  SB := TStringBuilder.Create;
  try
    for J := 0 to VM.Binaries.Value^.Size - 1 do
    begin
      Binary := VM.Binaries.Value^.Data[J];
      if J > 0 then
        SB.Append(Format('--- @%d (%s) ---'#10, [J - 1, Binary.BinaryName]))
      else
        SB.Append('--- @main ---'#10);
      I := 0;
      while I <= Binary.Count - 1 do
      begin
        Op := TSEOpcode(QWord(Binary[I].VarPointer));
        System.WriteStr(S, Op);
        SB.Append(IntToStr(I) + ': ' + S);
        for K := 1 to OpcodeSizes[Op] - 1 do
        begin
          SB.Append(' ' + SEValueToText(Binary[I + K]));
          if K < OpcodeSizes[Op] - 1 then
            SB.Append(',');
        end;
        SB.Append(#10);
        Inc(I, OpcodeSizes[Op]);
      end;
      SB.Append(#10);
    end;
    SB.Append('--- STRING DATA ---'#10);
    for I := 0 to VM.ConstStrings.Count - 1 do
    begin
      S := VM.ConstStrings[I];
      if Length(S) > 255 then
      begin
        SetLength(S, 252);
        S := S + '...';
      end;
      SB.Append(Format('%d: %s'#10, [I, S]));
    end;
  finally
    Res := SB.ToString;
    SB.Free;
  end;
end;

function SEGet(const AName: String): TSEValue;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    try
      Exit(SEMapGet(ScriptVarMap, AName))
    except
      on E: Exception do
        Result := SENull;
    end;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure SESet(const AName: String; const AValue: TSEValue);
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    try
      SEMapSet(ScriptVarMap, AName, AValue);
    except
      on E: Exception do ;
    end;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

function SEClone(constref V: TSEValue): TSEValue;
var
  I: Integer;
  S, Key: String;
begin
  case V.Kind of
    sevkNumber, sevkBoolean:
      begin
        Result.VarNumber := V.VarNumber;
        Result.Kind := V.Kind;
      end;
    sevkPointer:
      begin
        Result.VarPointer := V.VarPointer;
        Result.Kind := sevkPointer;
      end;
    sevkString:
      begin
        S := V.VarString^;
        GC.AllocString(@Result, S);
      end;
    sevkMap:
      begin
        GC.AllocMap(@Result);
        if not SEMapIsValidArray(V) then
        begin
          TSEValueMap(V.VarMap).Lock;
          try
            for Key in TSEValueMap(V.VarMap).Map.Keys do
            begin
              SEMapSet(Result, Key, TSEValueMap(V.VarMap).Get2(Key));
            end;
          finally
            TSEValueMap(V.VarMap).Unlock;
          end;
        end else
        begin
          for I := 0 to TSEValueMap(V.VarMap).Count - 1 do
          begin
            SEMapSet(Result, I, TSEValueMap(V.VarMap).Get2(I));
          end;
        end;
      end;
  end;
end;

procedure TSEValueHelper.AllocBuffer(constref Size: Integer); inline;
begin
  GC.AllocBuffer(@Self, Size);
end;

procedure TSEValueHelper.AllocMap; inline;
begin
  GC.AllocMap(@Self);
end;

procedure TSEValueHelper.AllocString(const S: String); inline;
begin
  GC.AllocString(@Self, S);
end;

procedure TSEValueHelper.AllocPascalObject(const Obj: TObject; const IsManaged: Boolean); inline;
begin
  GC.AllocPascalObject(@Self, Obj, IsManaged);
end;

function TSEValueHelper.GetValue(constref I: Integer): TSEValue; inline; overload;
begin
  Result := SEMapGet(Self, I);
end;

function TSEValueHelper.GetValue(constref S: String): TSEValue; inline; overload;
begin
  Result := SEMapGet(Self, S);
end;

function TSEValueHelper.GetValue(constref I: TSEValue): TSEValue; inline; overload;
begin
  Result := SEMapGet(Self, I);
end;

procedure TSEValueHelper.SetValue(constref I: Integer; const A: TSEValue); inline; overload;
begin
  SEMapSet(Self, I, A);
end;

procedure TSEValueHelper.SetValue(constref S: String; const A: TSEValue); inline; overload;
begin
  SEMapSet(Self, S, A);
end;

procedure TSEValueHelper.SetValue(I: TSEValue; const A: TSEValue); inline; overload;
begin
  SEMapSet(Self, I, A);
end;

function TSEValueHelper.ContainsKey(constref S: String): Boolean; inline; overload;
begin
  if Self.Kind <> sevkMap then
    Exit(False);
  if SEMapIsValidArray(Self) then
    Exit(False);
  Exit(TSEValueMap(Self.VarMap).Map.{$ifdef SE_MAP_AVK959}Contains{$else}ContainsKey{$endif}(S));
end;

procedure TSEValueHelper.UnManaged; inline;
begin
  GC.UnManaged(@Self);
end;

procedure TSEValueHelper.Managed; inline;
begin
  GC.Managed(@Self);
end;

function TSEValueHelper.Clone: TSEValue; inline;
begin
  Result := SEClone(Self);
end;

function TSEValueHelper.IsValidArray: Boolean; inline;
begin
  Result := SEMapIsValidArray(Self);
end;

procedure TSEValueHelper.FromJSON(constref S: String);
var
  V: TSEValue;
begin
  V := S;
  Self := TBuiltInFunction(nil).SEJSONParse(nil, @V, 1);
end;

function TSEValueHelper.ToJSON: String;
begin
  Result := TBuiltInFunction(nil).SEJSONStringify(nil, @Self, 1);
end;

function TSEValueHelper.ToString: String;
begin
  Result := SEValueToText(Self);
end;

function TSEValueHelper.Size: Integer;
begin
  Result := SESize(Self);
end;

class function TBuiltInFunction.SEBufferCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkNumber, 1, {$I %CURRENTROUTINE%});
  GC.AllocBuffer(@Result, Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEBufferLength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result := SESize(Args[0]);
end;

class function TBuiltInFunction.SEBufferCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkBuffer, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  Move(Args[1].VarBuffer^.Ptr^, Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillChar(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Byte(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Word(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillDWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), DWord(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), QWord(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillChar(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), ShortInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), SmallInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillDWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), LongInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Int64(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  V: Single;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  V := Args[1].VarNumber;
  FillDWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), DWord((@V)^));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  V: Double;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  V := Args[1].VarNumber;
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), QWord((@V)^));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferGetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Byte((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Word((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := LongWord((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := QWord((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := ShortInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := SmallInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := LongInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Int64((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := TSENumber(Single((Args[0].VarBuffer^.Ptr)^));
end;

class function TBuiltInFunction.SEBufferGetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := TSENumber((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferSetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Byte(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Word(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  case Args[1].Kind of
    sevkBuffer:
      LongWord(Args[0].VarBuffer^.Ptr^) := LongWord(Args[1].VarBuffer^.Ptr);
    else
      LongWord(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  end;
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  case Args[1].Kind of
    sevkBuffer:
      QWord(Args[0].VarBuffer^.Ptr^) := QWord(Args[1].VarBuffer^.Ptr);
    else
      QWord(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  end;
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  ShortInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  P: Pointer;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  P := Pointer(Round(Args[0].VarNumber));
  SmallInt(P^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  LongInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Int64(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Single(Args[0].VarBuffer^.Ptr^) := Single(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  TSENumber(Args[0].VarBuffer^.Ptr^) := Args[1];
  Result := SENull;
end;

class function TBuiltInFunction.SEStringToBuffer(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkString, 1, {$I %CURRENTROUTINE%});
  GC.AllocBuffer(@Result, Length(Args[0].VarString^));
  Move(Args[0].VarString^[1], PByte(Result.VarBuffer^.Ptr)[1], Length(Args[0].VarString^));
end;

class function TBuiltInFunction.SEBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  S: String;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  S := PChar(Args[0].VarBuffer^.Ptr);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SEWBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  WS: UnicodeString;
  S: String;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  WS := PWideChar(Args[0].VarBuffer^.Ptr);
  S := UTF8Encode(WS);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SEArrayToBufferF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  Size: QWord;
begin
  SEValidateType(@Args[0], sevkMap, 1, {$I %CURRENTROUTINE%});
  Size := SESize(Args[0]);
  GC.AllocBuffer(@Result, Size * 4);
  for I := 0 to Size - 1 do
  begin
    Single((Result.VarBuffer^.Ptr + I * 4)^) := SEMapGet(Args[0], I).VarNumber;
  end;
end;

class function TBuiltInFunction.SEArrayToBufferF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  Size: QWord;
begin
  SEValidateType(@Args[0], sevkMap, 1, {$I %CURRENTROUTINE%});
  Size := SESize(Args[0]);
  GC.AllocBuffer(@Result, Size * 8);
  for I := 0 to Size - 1 do
  begin
    Double((Result.VarBuffer^.Ptr + I * 8)^) := SEMapGet(Args[0], I).VarNumber;
  end;
end;

class function TBuiltInFunction.SEBufferToArrayF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  Size: QWord;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  Size := Round(Args[1].VarNumber);
  GC.AllocMap(@Result);
  TSEValueMap(Result.VarMap).Count := Size;
  for I := 0 to Size - 1 do
  begin
    SEMapSet(Result, I, Single((Args[0].VarBuffer^.Ptr + I * 4)^))
  end;
end;

class function TBuiltInFunction.SEBufferToArrayF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  Size: QWord;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  Size := Round(Args[1].VarNumber);
  GC.AllocMap(@Result);
  TSEValueMap(Result.VarMap).Count := Size;
  for I := 0 to Size - 1 do
  begin
    SEMapSet(Result, I, Double((Args[0].VarBuffer^.Ptr + I * 8)^))
  end;
end;

class function TBuiltInFunction.SETypeOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  case Args[0].Kind of
    sevkMap:
      if SEMapIsValidArray(Args[0]) then
        Result := 'array'
      else
        Result := 'map';
    else
      Result := ValueKindNames[Args[0].Kind];
  end;
end;

class function TBuiltInFunction.SEKindOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := TSENumber(Integer(Args[0].Kind));
end;

class function TBuiltInFunction.SEWrite(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
begin
  if ArgCount > 0 then
    for I := 0 to ArgCount - 1 do
      Write(SEValueToText(Args[I]));
  Result := SENull;
end;

class function TBuiltInFunction.SEWriteln(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
begin
  TBuiltInFunction.SEWrite(VM, Args, ArgCount);
  Writeln;
  Result := SENull;
end;

class function TBuiltInFunction.SERandom(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Random(Round(Args[0].VarNumber)));
end;

class function TBuiltInFunction.SERnd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Random);
end;

class function TBuiltInFunction.SERound(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEFloor(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Floor(Args[0].VarNumber));
end;

class function TBuiltInFunction.SECeil(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Ceil(Args[0].VarNumber));
end;

class function TBuiltInFunction.SETrunc(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Trunc(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    try
      Exit(SEMapGet(ScriptVarMap, Args[0].VarString^))
    except
      on E: Exception do
        Result := SENull;
    end;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

class function TBuiltInFunction.SESet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    SEMapSet(ScriptVarMap, Args[0].VarString^, Args[1]);
    Result := SENull;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

class function TBuiltInFunction.SEString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(SEValueToText(Args[0]));
end;

class function TBuiltInFunction.SENumber(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(PointStrToFloat(Trim(Args[0])));
end;

class function TBuiltInFunction.SELength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  case Args[0].Kind of
    sevkString:
      {$ifdef SE_STRING_UTF8}
      Exit(UTF8Length(String(Args[0].VarString^)));
      {$else}
      Exit(Length(String(Args[0].VarString^)));
      {$endif}
    sevkMap, sevkBuffer:
      begin
        Exit(SESize(Args[0]));
      end;
    else
      Exit(0);
  end;
end;

class function TBuiltInFunction.SEMapCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  while I < ArgCount - 1 do
  begin
    if Args[I].Kind = sevkString then
      SEMapSet(Result, Args[I].VarString^, Args[I + 1])
    else
      SEMapSet(Result, Round(Args[I].VarNumber), Args[I + 1]);
    Inc(I, 2);
  end;
end;

class function TBuiltInFunction.SEMapClone(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(SEClone(Args[0]));
end;

class function TBuiltInFunction.SEMapKeyDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Args[0];
  SEMapDelete(Result, Args[1]);
end;

class function TBuiltInFunction.SEMapKeysGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Key: String;
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  if not SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).Lock;
    try
      for Key in TSEValueMap(Args[0].VarMap).Map.Keys do
      begin
        SEMapSet(Result, I, Key);
        Inc(I);
      end;
    finally
      TSEValueMap(Args[0].VarMap).Unlock;
    end;
  end else
  begin
    for I := 0 to TSEValueMap(Args[0].VarMap).Count - 1 do
    begin
      SEMapSet(Result, I, I);
    end;
  end;
end;

class function TBuiltInFunction.SEMapClear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).Clear;
  end else
  begin
    TSEValueMap(Args[0].VarMap).Map.Clear;
  end;
end;

class function TBuiltInFunction.SEArrayResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).Count := Args[1];
  end;
  Result := Args[0];
end;

class function TBuiltInFunction.SEArrayToMap(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  if Args[0].Kind = sevkMap then
    TSEValueMap(Args[0].VarMap).ToMap;
  Result := Args[0];
end;

class function TBuiltInFunction.SEArrayFill(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    for I := 0 to TSEValueMap(Args[0].VarMap).Count - 1 do
      TSEValueMap(Args[0].VarMap)[I] := Args[1];
  end;
  Result := Args[0];
end;

class function TBuiltInFunction.SELerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  A, B, T: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  Exit(A + (B - A) * T);
end;

class function TBuiltInFunction.SESLerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  A, B, T, T2: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  T2 := (1 - Cos(T * PI)) * 0.5;
  Exit(A * (1 - T2) + B * T2);
end;

class function TBuiltInFunction.SESign(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Sign(Args[0].VarNumber));
end;

class function TBuiltInFunction.SERange(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  function EpsilonRound(V: TSENumber): TSENumber;
  begin
    if Abs(Frac(V)) < 1E-12 then
      Result := Round(V)
    else
      Result := V;
  end;

var
  V: TSENumber;
  I: Integer = 0;
begin
  GC.AllocMap(@Result);
  V := Args[0];
  if ArgCount = 3 then
    TSEValueMap(Result.VarMap).Capacity := Round(Args[1].VarNumber * (1 / Args[2].VarNumber)) // Set capacity beforehand
  else
    TSEValueMap(Result.VarMap).Capacity := Round(Args[1].VarNumber); // Set capacity beforehand
  while EpsilonRound(V) <= Args[1].VarNumber do
  begin
    SEMapSet(Result, I, V);
    if ArgCount = 3 then
      V := V + Args[2].VarNumber
    else
      V := V + 1;
    Inc(I);
  end;
end;

class function TBuiltInFunction.SEMin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
begin
  for I := 0 to ArgCount - 2 do
    if Args[I] < Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEMax(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
begin
  for I := 0 to ArgCount - 2 do
    if Args[I] > Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEPow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Power(Args[0].VarNumber, Args[1].VarNumber));
end;

class function TBuiltInFunction.SESleep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Sleep(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEStringGrep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  A: TStringDynArray;
  V: String;
begin
  Result := '';
  A := SplitString(Args[0], #10);
  for V in A do
    for I := 0 to SESize(Args[1]) - 1 do
      if StringIndexOf(V, SEMapGet(Args[1], I).VarString^) >= 0 then
      begin
        if Result = '' then
          Result := V
        else
          Result := Result + #10 + V;
      end;
end;

class function TBuiltInFunction.SEStringSplit(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  D: TStringDynArray;
  I: Integer;
begin
  D := SplitString(Args[0], Args[1]);
  GC.AllocMap(@Result);
  for I := 0 to Length(D) - 1 do
    SEMapSet(Result, I, D[I]);
end;

class function TBuiltInFunction.SEStringFind(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := StringIndexOf(Args[0].VarString^, Args[1]);
end;

class function TBuiltInFunction.SEStringDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Delete(Args[0].VarString^, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$else}
  Delete(Args[0].VarString^, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringCompare(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := CompareStr(Args[0].VarString^, Args[1].VarString^);
end;

class function TBuiltInFunction.SEStringInsert(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Insert(Args[1].VarString^, Args[0].VarString^, Round(Args[2].VarNumber + 1));
  {$else}
  Insert(Args[1].VarString^, Args[0].VarString^, Round(Args[2].VarNumber + 1));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringReplace(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  S: String;
begin
  S := StringReplace(Args[0], Args[1], Args[2], [rfReplaceAll]);
  Result := S;
end;

class function TBuiltInFunction.SEStringReplaceIgnoreCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  S: String;
begin
  S := StringReplace(Args[0], Args[1], Args[2], [rfReplaceAll, rfIgnoreCase]);
  Result := S;
end;

class function TBuiltInFunction.SEStringFormat(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  I: Integer;
  S, V: String;
  Value: TSEValue;
begin
  S := Args[0].VarString^;
  for I := 0 to SESize(Args[1]) - 1 do
  begin
    V := '';
    Value := SEMapGet(Args[1], I);
    V := SEValueToText(Value);
    S := StringReplace(S, '{' + IntToStr(I) + '}', V, [rfReplaceAll]);
  end;
  Result := S;
end;

class function TBuiltInFunction.SEStringUpperCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := UpperCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := UpperCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringLowerCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := LowerCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := LowerCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringFindRegex(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  R: TRegExpr;
  I: Integer;
  C: Integer = 0;
  V: TSEValue;
begin
  GC.AllocMap(@Result);
  R := TRegExpr.Create(Args[1].VarString^);
  if R.Exec(Args[0].VarString^) then
  repeat
    for I := 1 to R.SubExprMatchCount do
    begin
      GC.AllocMap(@V);
      SEMapSet(V, 0, R.Match[I]);
      SEMapSet(V, 1, R.MatchPos[I] - 1);
      SEMapSet(Result, C, V);
      Inc(C);
    end;
  until not R.ExecNext;
end;

class function TBuiltInFunction.SEStringTrim(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Trim(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimLeft(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := TrimLeft(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimRight(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := TrimRight(Args[0]);
end;

class function TBuiltInFunction.SEStringExtractName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := ExtractFileName(Args[0].VarString^);
end;

class function TBuiltInFunction.SEStringExtractPath(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := ExtractFilePath(Args[0].VarString^);
end;

class function TBuiltInFunction.SEStringExtractExt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := ExtractFileExt(Args[0].VarString^);
end;

class function TBuiltInFunction.SESin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Sin(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECos(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Cos(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SETan(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Tan(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECot(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Cot(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SESqrt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Sqrt(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEAbs(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Abs(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEFrac(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(Frac(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEGetTickCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Exit(GetTickCount64);
end;

class function TBuiltInFunction.SEDTNow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Now;
end;

class function TBuiltInFunction.SEDTSetDate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := EncodeDate(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber));
end;

class function TBuiltInFunction.SEDTSetTime(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := EncodeTime(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber), Round(Args[3].VarNumber));
end;

class function TBuiltInFunction.SEDTDayAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := IncDay(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTMonthAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := IncMonth(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTYearAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := IncYear(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTGetYear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := Y;
end;

class function TBuiltInFunction.SEDTGetMonth(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := M;
end;

class function TBuiltInFunction.SEDTGetDay(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := D;
end;

class function TBuiltInFunction.SEDTGetHour(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := H;
end;

class function TBuiltInFunction.SEDTGetMinute(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := M;
end;

class function TBuiltInFunction.SEGCObjectCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := GC.ObjectCount;
end;

class function TBuiltInFunction.SEGCObjectOldCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := GC.OldObjectCount;
end;

class function TBuiltInFunction.SEGCCollect(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  GC.GC(True);
  Result := SENull;
end;

class function TBuiltInFunction.SEChar(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Char(Floor(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEOrd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := Byte(Args[0].VarString^[1]);
end;

class function TBuiltInFunction.SECoroutineCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Coroutine: TSEVMCoroutine;
begin
  SEValidateType(@Args[0], sevkFunction, 1, {$I %CURRENTROUTINE%});
  Coroutine := TSEVMCoroutine.Create(VM, Args[0], @Args[1], ArgCount - 1, 512);
  GC.AllocPascalObject(@Result, Coroutine, True);
  // Push "self" onto stack
  Coroutine.VM.Stack[(SE_STACK_RESERVED - 1) + ArgCount] := Result;
end;

class function TBuiltInFunction.SECoroutineReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkFunction, 2, {$I %CURRENTROUTINE%});
  TSEVMCoroutine(Args[0].VarPascalObject^.Value).Reset(Args[1], @Args[2], ArgCount - 3);
end;

class function TBuiltInFunction.SECoroutineResume(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).Execute;
end;

class function TBuiltInFunction.SECoroutineIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsTerminated;
end;

class function TBuiltInFunction.SECoroutineTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsTerminated := True;
end;

class function TBuiltInFunction.SECoroutineIsExecuting(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsExecuting;
end;

{$ifdef SE_THREADS}
class function TBuiltInFunction.SEThreadCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Thread: TSEVMThread;
begin
  SEValidateType(@Args[0], sevkFunction, 1, {$I %CURRENTROUTINE%});
  Thread := TSEVMThread.Create(VM, Args[0], @Args[1], ArgCount - 1, 512);
  GC.AllocPascalObject(@Result, Thread, True);
  // Push "self" onto stack
  Thread.VM.Stack[(SE_STACK_RESERVED - 1) + ArgCount] := Result;
end;

class function TBuiltInFunction.SEThreadStart(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  while TSEVMThread(Args[0].VarPascalObject^.Value).IsRequestForSuspendByGC do Sleep(1);
  TSEVMThread(Args[0].VarPascalObject^.Value).Start;
end;

class function TBuiltInFunction.SEThreadIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMThread(Args[0].VarPascalObject^.Value).Terminated;
end;

class function TBuiltInFunction.SEThreadSuspend(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  if not TSEVMThread(Args[0].VarPascalObject^.Value).Terminated then
    TSEVMThread(Args[0].VarPascalObject^.Value).Suspend;
end;

class function TBuiltInFunction.SEThreadTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  if not TSEVMThread(Args[0].VarPascalObject^.Value).Terminated then
    TSEVMThread(Args[0].VarPascalObject^.Value).Terminate;
end;

class function TBuiltInFunction.SEThreadWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TSEVMThread(Args[0].VarPascalObject^.Value).WaitFor;
end;

class function TBuiltInFunction.SECriticalCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Critical: TCriticalSection;
begin
  Critical := TCriticalSection.Create;
  GC.AllocPascalObject(@Result, Critical, True);
end;

class function TBuiltInFunction.SECriticalEnter(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TCriticalSection(Args[0].VarPascalObject^.Value).Enter;
end;

class function TBuiltInFunction.SECriticalLeave(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TCriticalSection(Args[0].VarPascalObject^.Value).Leave;
end;

class function TBuiltInFunction.SECriticalTry(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TCriticalSection(Args[0].VarPascalObject^.Value).TryEnter;
end;

class function TBuiltInFunction.SEEventCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  Event: TEventObject;
begin
  Event := TEvent.Create(nil, True, False, '');
  GC.AllocPascalObject(@Result, Event, True);
end;

class function TBuiltInFunction.SEEventSet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TEventObject(Args[0].VarPascalObject^.Value).SetEvent;
end;

class function TBuiltInFunction.SEEventWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSENumber(Integer(TEventObject(Args[0].VarPascalObject^.Value).WaitFor(Round(Args[1].VarNumber))));
end;

class function TBuiltInFunction.SEEventReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TEventObject(Args[0].VarPascalObject^.Value).ResetEvent;
end;
{$endif}

class function TBuiltInFunction.SEFileReadText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := ReadFileAsString(Args[0]);
end;

class function TBuiltInFunction.SEFileReadBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  FS: TFileStream;
  SizeToRead: Int64;
begin
  FS := TFileStream.Create(Args[0], fmOpenRead);
  Result := SENull;
  try
    if ArgCount = 1 then
    begin
      GC.AllocBuffer(@Result, FS.Size);
      FS.Read(Result.VarBuffer^.Ptr^, FS.Size);
    end else
    if ArgCount = 3 then
    begin
      SizeToRead := Min(FS.Size - Round(Args[1].VarNumber), Round(Args[2].VarNumber));
      if SizeToRead > 0 then
      begin
        GC.AllocBuffer(@Result, SizeToRead);
        FS.Position := Round(Args[1].VarNumber);
        FS.Read(Result.VarBuffer^.Ptr^, SizeToRead);
      end;
    end;
  finally
    FS.Free;
  end;
end;

class function TBuiltInFunction.SEFileWriteText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  FS: TFileStream;
begin
  if FileExists(Args[0].VarString^) then
    FS := TFileStream.Create(Args[0], fmOpenWrite)
  else
    FS := TFileStream.Create(Args[0], fmCreate);
  try
    FS.Position := FS.Size;
    FS.Write(Args[1].VarString^[1], Length(Args[1].VarString^));
  finally
    FS.Free;
  end;
end;

class function TBuiltInFunction.SEFileWriteBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  FS: TFileStream;
begin
  if FileExists(Args[0].VarString^) then
    FS := TFileStream.Create(Args[0], fmOpenWrite)
  else
    FS := TFileStream.Create(Args[0], fmCreate);
  try
    FS.Position := FS.Size;
    FS.Write(Args[1].VarBuffer^.Ptr^, Args[2]);
  finally
    FS.Free;
  end;
end;

class function TBuiltInFunction.SEFileCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := False;
  {$ifdef SE_HAS_FILEUTIL}
  if FileExists(Args[0].VarString^) then
  begin
    Result := CopyFile(Args[0].VarString^, Args[1], [cffOverwriteFile], False);
  end;
  {$endif}
end;

class function TBuiltInFunction.SEFileExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := FileExists(Args[0].VarString^);
end;

class function TBuiltInFunction.SEFileDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  DeleteFile(Args[0].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEFileRename(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  RenameFile(Args[0].VarString^, Args[1].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEFileFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  SL: TStringList;
  I: Integer;
begin
  Result := SENull;
  {$ifdef SE_HAS_FILEUTIL}
  SL := TStringList.Create;
  try
    FindAllFiles(SL, Args[0], Args[1], Boolean(Round(Args[2].VarNumber)), Round(Args[3].VarNumber));
    GC.AllocMap(@Result);
    for I := 0 to SL.Count - 1 do
      SEMapSet(Result, I, SL[I]);
  finally
    SL.Free;
  end;
  {$endif}
end;

class function TBuiltInFunction.SEFileGetSize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  F: File of Byte;
begin
  Result := 0;
  if FileExists(Args[0].VarString^) then
  begin
    AssignFile(F, Args[0].VarString^);
    Reset(F);
    Result := FileSize(F);
    CloseFile(F);
  end;
end;

class function TBuiltInFunction.SEFileGetAge(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  F: File of Byte;
begin
  Result := -1;
  if FileExists(Args[0].VarString^) then
  begin
    Result := FileAge(Args[0].VarString^);
  end;
end;

class function TBuiltInFunction.SEDirectoryCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  ForceDirectories(Args[0].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEDirectoryDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  {$ifdef SE_HAS_FILEUTIL}
  DeleteDirectory(Args[0], False);
  {$endif}
  Result := SENull;
end;

class function TBuiltInFunction.SEDirectoryFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
var
  SL: TStringList;
  I: Integer;
begin
  Result := SENull;
  {$ifdef SE_HAS_FILEUTIL}
  SL := TStringList.Create;
  try
    FindAllDirectories(SL, Args[0], Args[1]);
    GC.AllocMap(@Result);
    for I := 0 to SL.Count - 1 do
      SEMapSet(Result, I, SL[I]);
  finally
    SL.Free;
  end;
  {$endif}
end;

class function TBuiltInFunction.SEDirectoryExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := DirectoryExists(Args[0].VarString^);
end;

class function TBuiltInFunction.SEBase64Encode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := EncodeStringBase64(Args[0]);
end;

class function TBuiltInFunction.SEBase64Decode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  Result := DecodeStringBase64(Args[0]);
end;

class function TBuiltInFunction.SEJSONParse(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
  procedure QueryForObject(out R: TSEValue; Data: TJSONData); forward;

  procedure QueryForArray(out R: TSEValue; Data: TJSONData);
  var
    I: Integer;
    D: TJSONData;
    Name: String;
    V: TSEValue;
  begin
    GC.AllocMap(@R);
    for I := 0 to Data.Count - 1 do
    begin
      D := Data.Items[I];
      case D.JSONType of
        jtArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, I, V);
          end;
        jtString:
          begin
            SEMapSet(R, I, D.AsString);
          end;
        jtNumber:
          begin
            SEMapSet(R, I, D.AsFloat);
          end;
        jtBoolean:
          begin
            SEMapSet(R, I, D.AsBoolean);
          end;
        jtNull:
          begin
            SEMapSet(R, I, SENull);
          end;
        jtObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, I, V);
          end;
      end;
    end;
  end;

  procedure QueryForObject(out R: TSEValue; Data: TJSONData);
  var
    I: Integer;
    D: TJSONData;
    V: TSEValue;
    Name: String;
  begin
    GC.AllocMap(@R);
    TSEValueMap(R.VarMap).ToMap;
    for I := 0 to Data.Count - 1 do
    begin
      Name := TJSONObject(Data).Names[I];
      D := Data.FindPath(Name);
      case D.JSONType of
        jtArray:
          begin
            QueryForArray(V, D);
            SEMapSet(R, Name, V);
          end;
        jtString:
          begin
            SEMapSet(R, Name, D.AsString);
          end;
        jtNumber:
          begin
            SEMapSet(R, Name, D.AsFloat);
          end;
        jtBoolean:
          begin
            SEMapSet(R, Name, D.AsBoolean);
          end;
        jtNull:
          begin
            SEMapSet(R, Name, SENull);
          end;
        jtObject:
          begin
            QueryForObject(V, D);
            SEMapSet(R, Name, V);
          end;
      end;
    end;
  end;

var
  Json: TJSONData;
  ErrorStr: String = '';
begin
  SEValidateType(@Args[0], sevkString, 1, {$I %CURRENTROUTINE%});
  Result := SENull;
  Json := GetJSON(Args[0].VarString^);
  try
    try
      if Json.JSONType = jtArray then
        QueryForArray(Result, Json)
      else
        QueryForObject(Result, Json);
    except
      on E: Exception do
      begin
        ErrorStr := E.Message;
      end;
    end;
  finally
    Json.Free;
    if ErrorStr <> '' then
      raise Exception.Create(ErrorStr);
  end;
end;

class function TBuiltInFunction.SEJSONStringify(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;

  procedure DecodeJSONArray(SB: TStringBuilder; const Map: TSEValue); forward;
  procedure DecodeJSONObject(SB: TStringBuilder; const Map: TSEValue); forward;

  procedure Decide(SB: TStringBuilder; const Map: TSEValue);
  begin
    if SEMapIsValidArray(Map) then
      DecodeJSONArray(SB, Map)
    else
      DecodeJSONObject(SB, Map);
  end;

  procedure DecodeJSONArray(SB: TStringBuilder; const Map: TSEValue);
  var
    I: Integer = 0;
    J: Integer = 0;
    V: TSEValue;
  begin
    SB.Append('[');
    for I := 0 to TSEValueMap(Map.VarMap).Count - 1 do
    begin
      V := SEMapGet(Map, I);
      if V.Kind = sevkPascalObject then
        continue;
      if (J > 0) then
        SB.Append(',');
      case V.Kind of
        sevkString:
          SB.Append('"' + StringToJSONString(V.VarString^) + '"');
        sevkNumber:
          SB.Append(PointFloatToStr(V.VarNumber));
        sevkBoolean:
          SB.Append(BoolToStr(Boolean(Round(V.VarNumber)), 'true', 'false'));
        sevkMap:
          begin
            Decide(SB, V);
          end;
        sevkNull:
          SB.Append('null');
        else
          begin
            raise Exception.Create(Format('Array element "%d" with type "%s" is not a valid JSON value!', [I, ValueKindNames[V.Kind]]))
          end;
      end;
      Inc(J);
    end;
    SB.Append(']');
  end;

  procedure DecodeJSONObject(SB: TStringBuilder; const Map: TSEValue);
  var
    I: Integer = 0;
    V: TSEValue;
    Key: String;
  begin
    TSEValueMap(Map.VarMap).Lock;
    try
      SB.Append('{');
      for Key in TSEValueMap(Map.VarMap).Map.Keys do
      begin
        V := SEMapGet(Map, Key);
        if V.Kind = sevkPascalObject then
          continue;
        if (I > 0) then
          SB.Append(',');
        SB.Append('"' + StringToJSONString(Key) + '":');
        case V.Kind of
          sevkString:
            SB.Append('"' + StringToJSONString(V.VarString^) + '"');
          sevkNumber:
            SB.Append(PointFloatToStr(V.VarNumber));
          sevkBoolean:
            SB.Append(BoolToStr(Boolean(Round(V.VarNumber)), 'true', 'false'));
          sevkMap:
            begin
              Decide(SB, V);
            end;
          sevkNull:
            SB.Append('null');
          else
            begin
              raise Exception.Create(Format('Key "%s" with type "%s" is not a valid JSON value!', [Key, ValueKindNames[V.Kind]]))
            end;
        end;
        Inc(I);
      end;
      SB.Append('}');
    finally
      TSEValueMap(Map.VarMap).Unlock;
    end;
  end;

var
  SB: TStringBuilder;
begin
  SB := TStringBuilder.Create;
  try
    if Args[0].Kind = sevkMap then
      Decide(SB, Args[0]);
    Result := SB.ToString;
  finally
    SB.Free;
  end;
end;

class function TBuiltInFunction.SEPasObjectClassName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TObject(Args[0].VarPascalObject^.Value).ClassName;
end;

function TSEGCNodeList.Ptr(const P: Cardinal): PSEGCNode; inline;
begin
  Result := @FItems[P];
end;

function TSEOpcodeInfoList.Ptr(const P: Cardinal): PSEOpcodeInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncNativeList.Ptr(const P: Cardinal): PSEFuncNativeInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncScriptList.Ptr(const P: Cardinal): PSEFuncScriptInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEFuncImportList.Ptr(const P: Cardinal): PSEFuncImportInfo; inline;
begin
  Result := @FItems[P];
end;

function TSEIdentList.Ptr(const P: Cardinal): PSEIdent; inline;
begin
  Result := @FItems[P];
end;

function TSEValueList.Ptr(const P: Cardinal): PSEValue; inline;
begin
  Result := @FItems[P];
end;

function TSESymbolList.Ptr(const P: Cardinal): PSESymbol; inline;
begin
  Result := @FItems[P];
end;

// ----- Fast inline TSEValue operations -----

procedure SEValueAdd(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
var
  I, Len: Integer;
  Temp: TSEValue;
  Key, S: String;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber + V2.VarNumber;
      end;
    sevkString:
      begin
        GC.AllocString(@R, V1.VarString^ + V2.VarString^);
      end;
    sevkMap:
      begin
        GC.AllocMap(@Temp);
        if (not SEMapIsValidArray(V1)) and (not SEMapIsValidArray(V2)) then
        begin
          for S in TSEValueMap(V1.VarMap).Map.Keys do
            SEMapSet(Temp, S, SEMapGet(V1, S));
          for S in TSEValueMap(V2.VarMap).Map.Keys do
            SEMapSet(Temp, S, SEMapGet(V2, S));
        end else
        begin
          Len := SESize(V1);
          TSEValueMap(Temp.VarMap).Count := Len + SESize(V2);
          for I := 0 to Len - 1 do
            SEMapSet(Temp, I, SEMapGet(V1, I));
          for I := Len to Len + SESize(V2) - 1 do
            SEMapSet(Temp, I, SEMapGet(V2, I - Len));
        end;
        R := Temp;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := V1.VarPointer + V2.VarPointer;
      end;
  end
  else
    if (V1.Kind = sevkBuffer) and (V2.Kind = sevkNumber) then
    begin
      GC.AllocBuffer(@Temp, 0);
      Temp.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
      R := Temp;
    end;
end;

procedure SEValueSub(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
var
  Temp: TSEValue;
begin
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber - V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := Pointer(V1.VarPointer - V2.VarPointer);
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@Temp, 0);
        Temp.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
        R := Temp;
      end;
  end;
end;

procedure SEValueNot(out R: TSEValue; constref V: TSEValue); inline;
begin
  case V.Kind of
    sevkNumber, sevkBoolean:
      begin
        R := not (V.VarNumber <> 0);
      end;
    sevkNull:
      begin
        R := True;
      end;
    sevkString:
      begin
        R := False;
      end;
  end;
end;

procedure SEValueNeg(out R: TSEValue; constref V: TSEValue); inline;
begin
  R.VarNumber := -V.VarNumber;
end;

procedure SEValueMul(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2.VarNumber;
end;

procedure SEValueDiv(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2.VarNumber;
end;

procedure SEValueLesser(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber < V2.VarNumber;
end;

procedure SEValueGreater(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber > V2.VarNumber;
end;

procedure SEValueLesserOrEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber <= V2.VarNumber;
end;

procedure SEValueGreaterOrEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R := V1.VarNumber >= V2.VarNumber;
end;

procedure SEValueEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      R := V1.VarNumber = V2.VarNumber;
    sevkString:
      R := V1.VarString^ = V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind = V2.VarFuncKind) and (V1.VarFuncIndx = V2.VarFuncIndx);
    sevkNull:
      R := True;
    sevkPascalObject:
      R := V1.VarPascalObject^.Value = V2.VarPascalObject^.Value;
    else
      R := V1.VarPointer = V2.VarPointer;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    R := (V1.VarNumber <> 0) = V2
  else
    R := False;
end;

procedure SEValueNotEqual(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      R := V1.VarNumber <> V2.VarNumber;
    sevkString:
      R := V1.VarString^ <> V2.VarString^;
    sevkFunction:
      R := (V1.VarFuncKind <> V2.VarFuncKind) or (V1.VarFuncIndx <> V2.VarFuncIndx);
    sevkNull:
      R := False;
    sevkPascalObject:
      R := V1.VarPascalObject^.Value <> V2.VarPascalObject^.Value;
    else
      R := V1.VarPointer <> V2.VarPointer;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    R := (V1.VarNumber <> 0) <> V2
  else
    R := True;
end;

procedure SEValueShiftLeft(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := Round(V1.VarNumber) shl Round(V2.VarNumber);
      end;
  end;
end;

procedure SEValueShiftRight(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := Round(V1.VarNumber) shr Round(V2.VarNumber);
      end;
  end;
end;

function SEValueLesser(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber < V2.VarNumber;
end;

function SEValueGreater(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber > V2.VarNumber;
end;

function SEValueLesserOrEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber <= V2.VarNumber;
end;

function SEValueGreaterOrEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  Result := V1.VarNumber >= V2.VarNumber;
end;

function SEValueEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      Result := V1.VarNumber = V2.VarNumber;
    sevkString:
      Result := V1.VarString^ = V2.VarString^;
    sevkFunction:
      Result := (V1.VarFuncKind = V2.VarFuncKind) and (V1.VarFuncIndx = V2.VarFuncIndx);
    sevkNull:
      Result := True;
    sevkPascalObject:
      Result := V1.VarPascalObject^.Value = V2.VarPascalObject^.Value;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    Result := (V1.VarNumber <> 0) = V2
  else
    Result := False;
end;

function SEValueNotEqual(constref V1, V2: TSEValue): Boolean; inline; overload;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber, sevkBoolean:
      Result := V1.VarNumber <> V2.VarNumber;
    sevkString:
      Result := V1.VarString^ <> V2.VarString^;
    sevkFunction:
      Result := (V1.VarFuncKind <> V2.VarFuncKind) or (V1.VarFuncIndx <> V2.VarFuncIndx);
    sevkNull:
      Result := False;
    sevkPascalObject:
      Result := V1.VarPascalObject^.Value <> V2.VarPascalObject^.Value;
  end else
  if (V1.Kind = sevkNumber) and (V2.Kind = sevkBoolean) then
    Result := (V1.VarNumber <> 0) <> V2
  else
    Result := True;
end;

// ----- TSEValue operator overloading

operator := (V: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V;
end;

operator := (V: String) R: TSEValue; inline;
begin
  R := Default(TSEValue);
  GC.AllocString(@R, V);
end;

operator := (V: Boolean) R: TSEValue; inline;
begin
  R.Kind := sevkBoolean;
  R.VarNumber := Integer(V);
end;
operator := (V: TSEValueArray) R: TSEValue; inline;
var
  I: Integer;
begin
  GC.AllocMap(@R);
  for I := 0 to Length(V) - 1 do
    SEMapSet(R, I, V[I]);
end;
operator := (V: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkPointer;
  R.VarPointer := V;
end;

operator := (V: TSEValue) R: Integer; inline;
begin
  R := Round(V.VarNumber);
end;
{$ifdef CPU64}
operator := (V: TSEValue) R: Int64; inline;
begin
  R := Round(V.VarNumber);
end;
{$endif}
operator := (V: TSEValue) R: Boolean; inline;
begin
  R := Round(V.VarNumber) <> 0;
end;
operator := (V: TSEValue) R: TSENumber; inline;
begin
  R := V.VarNumber;
end;
operator := (V: TSEValue) R: String; inline;
begin
  if V.Kind = sevkString then
    R := V.VarString^
  else
    R := '';
end;
operator := (V: TSEValue) R: TSEValueArray; inline;
var
  Len, I: Integer;
begin
  if V.Kind <> sevkMap then
    Exit;
  Len := SESize(V.VarMap);
  SetLength(R, Len);
  for I := 0 to Len - 1 do
    R[I] := SEMapGet(V, I);
end;
operator := (V: TSEValue) R: Pointer; inline;
begin
  R := V.VarPointer;
end;

operator + (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber + V2;
end;

operator + (V1: TSEValue; V2: String) R: TSEValue; inline;
var
  S: String;
begin
  if V1.Kind = sevkString then
  begin
    S := V1.VarString^;
    R := S + V2;
  end else
    R := V2;
end;

operator + (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkPointer;
  R.VarPointer := V1.VarPointer + V2;
end;

operator - (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber - V2;
end;
operator - (V1: TSEValue; V2: Pointer) R: TSEValue; inline;
begin
  R.Kind := sevkString;
  R.VarPointer := V1.VarPointer + V2;
end;

operator * (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2;
end;

operator / (V1: TSEValue; V2: TSENumber) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2;
end;

operator + (V1, V2: TSEValue) R: TSEValue; inline;
var
  I, Len: Integer;
  S: String;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber + V2.VarNumber;
      end;
    sevkString:
      begin
        if V2.Kind = sevkString then
          GC.AllocString(@R, V1.VarString^ + V2.VarString^)
        else
          GC.AllocString(@R, V1.VarString^);
      end;
    sevkMap:
      begin
        GC.AllocMap(@R);
        if (not SEMapIsValidArray(V1)) and (not SEMapIsValidArray(V2)) then
        begin
          for S in TSEValueMap(V1.VarMap).Map.Keys do
            SEMapSet(R, S, SEMapGet(V1, S));
          for S in TSEValueMap(V2.VarMap).Map.Keys do
            SEMapSet(R, S, SEMapGet(V2, S));
        end else
        begin
          Len := SESize(V1);
          TSEValueMap(R.VarMap).Count := Len + SESize(V2);
          for I := 0 to Len - 1 do
            SEMapSet(R, I, SEMapGet(V1, I));
          for I := Len to Len + SESize(V2) - 1 do
            SEMapSet(R, I, SEMapGet(V2, I - Len));
        end;
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@R, 0);
        R.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := V1.VarPointer + V2.VarPointer;
      end;
  end else
    if (V1.Kind = sevkBuffer) and (V2.Kind = sevkNumber) then
    begin
      GC.AllocBuffer(@R, 0);
      R.VarBuffer^.Ptr := V1.VarBuffer^.Ptr + Pointer(Round(V2.VarNumber));
    end;
end;
operator - (V: TSEValue) R: TSEValue; inline;
begin
  R.VarNumber := -V.VarNumber;
end;
operator - (V1, V2: TSEValue) R: TSEValue; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      begin
        R.Kind := sevkNumber;
        R.VarNumber := V1.VarNumber - V2.VarNumber;
      end;
    sevkPointer:
      begin
        R.Kind := sevkPointer;
        R.VarPointer := Pointer(V1.VarPointer - V2.VarPointer);
      end;
    sevkBuffer:
      begin
        GC.AllocBuffer(@R, 0);
        R.VarBuffer^.Ptr := Pointer(QWord(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
      end;
  end;
end;
operator * (V1, V2: TSEValue) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber * V2.VarNumber;
end;
operator / (V1, V2: TSEValue) R: TSEValue; inline;
begin
  R.Kind := sevkNumber;
  R.VarNumber := V1.VarNumber / V2.VarNumber;
end;

operator < (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber < V2;
end;
operator > (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber > V2;
end;
operator <= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber <= V2;
end;
operator >= (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber >= V2;
end;
operator = (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber = V2;
end;

operator = (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  R := V1.VarString^ = V2;
end;

operator <> (V1: TSEValue; V2: TSENumber) R: Boolean; inline;
begin
  R := V1.VarNumber <> V2;
end;

operator <> (V1: TSEValue; V2: String) R: Boolean; inline;
begin
  R := V1.VarString^ <> V2;
end;

operator < (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber < V2.VarNumber;
end;
operator > (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber > V2.VarNumber;
end;
operator <= (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber <= V2.VarNumber;
end;
operator >= (V1, V2: TSEValue) R: Boolean; inline;
begin
  R := V1.VarNumber >= V2.VarNumber;
end;
operator = (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      R := V1.VarNumber = V2.VarNumber;
    sevkBoolean:
      R := Boolean(Round(V1.VarNumber)) = Boolean(Round(V2.VarNumber));
    sevkString:
      R := V1.VarString^ = V2.VarString^;
    sevkNull:
      R := True;
    sevkPascalObject:
      R := V1.VarPascalObject^.Value = V2.VarPascalObject^.Value;
    else
      R := V1.VarPointer = V2.VarPointer;
  end else
    R := False;
end;
operator <> (V1, V2: TSEValue) R: Boolean; inline;
begin
  if V1.Kind = V2.Kind then
  case V1.Kind of
    sevkNumber:
      R := V1.VarNumber <> V2.VarNumber;
    sevkBoolean:
      R := Boolean(Round(V1.VarNumber)) <> Boolean(Round(V2.VarNumber));
    sevkString:
      R := V1.VarString^ <> V2.VarString^;
    sevkNull:
      R := False;
    sevkPascalObject:
      R := V1.VarPascalObject^.Value <> V2.VarPascalObject^.Value;
    else
      R := V1.VarPointer <> V2.VarPointer;
  end else
    R := True;
end;

constructor TSEValueMap.Create;
begin
  inherited;
  Self.FIsValidArray := True;
  Self.FMap := TSEValueDict.Create;
  {$ifdef SE_THREADS}
  InitCriticalSection(Self.FLock);
  {$endif}
end;

destructor TSEValueMap.Destroy;
begin
  {$ifdef SE_THREADS}
  DoneCriticalSection(Self.FLock);
  {$endif}
  Self.FMap.Free;
  inherited;
end;

procedure TSEValueMap.Lock;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(Self.FLock);
  {$endif}
end;

procedure TSEValueMap.Unlock;
begin
  {$ifdef SE_THREADS}
  LeaveCriticalSection(Self.FLock);
  {$endif}
end;

function TSEValueMap.TryLock: Boolean;
begin
  {$ifdef SE_THREADS}
  Result := TryEnterCriticalSection(Self.FLock) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

procedure TSEValueMap.ToMap;
var
  I: Integer;
begin
  Self.Lock;
  try
    if Self.FIsValidArray then
    begin
      for I := 0 to Self.Count - 1 do
        Self.FMap.AddOrSetValue(IntToStr(I), Self[I]);
      Self.FIsValidArray := False;
      Self.Clear;
    end;
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Set2(const Key: String; const AValue: TSEValue);
begin
  Self.Lock;
  try
    if Self.FIsValidArray then
    begin
      Self.ToMap;
    end;
    Self.FMap.AddOrSetValue(Key, AValue);
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Set2(const Index: Int64; const AValue: TSEValue);
var
  I: Integer;
begin
  Self.Lock;
  try
    if Index > Self.Count - 1 then
    begin
      Self.Count := Index + 1;
    end;
    Self.FItems[Index] := AValue;
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Del2(const Key: String);
begin
  Self.Lock;
  try
    Self.FMap.Remove(Key);
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Del2(const Index: Int64);
begin
  Self.Lock;
  try
    if Index <= Self.Count - 1 then
    begin
      Self.Delete(Index);
    end;
  finally
    Self.Unlock;
  end;
end;

function TSEValueMap.Get2(const Key: String): TSEValue;
begin
  Result := Self.FMap[Key];
end;

function TSEValueMap.Get2(const Index: Int64): TSEValue;
begin
  if Index <= Self.Count - 1 then
    Result := Self.FItems[Index]
  else
    Result := SENull;
end;

function TSEValueMap.Ptr(const I: Integer): PSEValue;
begin
  Result := @Self.FItems[I];
end;

function DumpCallStack: String;
var
  I: Longint;
  prevbp: Pointer;
  CallerFrame,
  CallerAddress,
  bp: Pointer;
const
  MaxDepth = 20;
begin
  Result := '';
  bp := get_frame;
  // This trick skip SendCallstack item
  // bp:= get_caller_frame(get_frame);
  try
    prevbp := bp - 1;
    I := 0;
    while bp > prevbp do begin
       CallerAddress := get_caller_addr(bp);
       CallerFrame := get_caller_frame(bp);
       if (CallerAddress = nil) then
         Break;
       Result := Result + BackTraceStrFunc(CallerAddress) + LineEnding;
       Inc(I);
       if (I >= MaxDepth) or (CallerFrame = nil) then
         Break;
       prevbp := bp;
       bp := CallerFrame;
    end;
  except
    { prevent endless dump if an exception occured }
  end;
end;

{$ifdef SE_THREADS}
constructor TSEGarbageCollectorMarkJob.Create;
begin
  inherited Create(True);
  Self.FreeOnTerminate := True;
end;

destructor TSEGarbageCollectorMarkJob.Destroy;
begin
  inherited;
end;

procedure TSEGarbageCollectorMarkJob.Execute;
var
  I: Integer;
begin
  while True do
  begin
    if Self.Terminated then
      Exit;
    if GC.Phase = segcpMark then
    begin
      {$ifdef SE_LOG}
      Writeln('[GC] ', Self.Phase);
      {$endif}
      for I := 0 to GC.ReachableValueList.Count - 1 do
        GC.Mark(GC.ReachableValueList.Ptr(I));
      GC.Phase := segcpSweep;
      Self.Suspend;
    end;
  end;
end;
{$endif}

constructor TSEGarbageCollector.Create;
var
  Ref0: TSEGCNode;
begin
  inherited;
  {$ifdef SE_THREADS}
  InitCriticalSection(Self.FLock);
  {$endif}
  Self.FNodeList := TSEGCNodeList.Create;
  Self.FNodeList.Capacity := 65536 * 8;
  Ref0 := Default(TSEGCNode);
  Self.FNodeList.Add(Ref0);
  Self.FNodeList.Add(Ref0); // Young generation's root
  Self.FNodeList.Add(Ref0); // Old generation's root
  Self.FNodeLastYoung := 1;
  Self.FNodeLastOld := 2;
  Self.FNodeAvailStack := TSEGCNodeAvailStack.Create;
  Self.FNodeAvailStack.Capacity := 65536 * 8;
  Self.FTicks := GetTickCount64;
  Self.FInterval := 5000;
  Self.FPromotion := 10;
  Self.FOldObjectCheckCycle := 10;
  Self.FObjectThreshold := 700;
  Self.FReachableValueList := TSEValueList.Create;
  Self.FVMThreadList := TSEVMList.Create;
  Self.EnableParallel := {$ifdef SE_MAP_AVK959}True{$else}False{$endif};
end;

destructor TSEGarbageCollector.Destroy;
var
  I: Integer;
  Value: TSEGCNode;
begin
  for I := 1 to Self.FNodeList.Count - 1 do
  begin
    Value := Self.FNodeList[I];
    Value.Garbage := True;
    Self.FNodeList[I] := Value;
  end;
  Self.Sweep(1);
  Self.Sweep(2);
  Self.FNodeAvailStack.Free;
  Self.FNodeList.Free;
  Self.FReachableValueList.Free;
  Self.FVMThreadList.Free;
  {$ifdef SE_THREADS}
  DoneCriticalSection(Self.FLock);
  {$endif}
  inherited;
end;

procedure TSEGarbageCollector.AddToList(const PValue: PSEValue); inline;
var
  Value: TSEGCNode;
begin
  Value := Default(TSEGCNode);
  Value.Prev := Self.FNodeLastYoung;
  if Self.FNodeAvailStack.Count = 0 then
  begin
    PValue^.Ref := Self.FNodeList.Count;
    Value.Value := PValue^;
    Self.FNodeList.Add(Value);
  end else
  begin
    PValue^.Ref := Self.FNodeAvailStack.Pop;
    Value.Value := PValue^;
    Self.FNodeList[PValue^.Ref] := Value;
  end;
  Self.FNodeList.Ptr(Self.FNodeLastYoung)^.Next := PValue^.Ref;
  Self.FNodeLastYoung := PValue^.Ref;
  Inc(Self.FObjects);
end;

procedure TSEGarbageCollector.CheckForGCFast; inline;
begin
  if GetTickCount64 - Self.FTicks > Self.Interval then
  begin
    Self.GC;
  end;
end;

procedure TSEGarbageCollector.CheckForGC; inline;
begin
  if GetTickCount64 - Self.FTicks > Self.Interval then
  begin
    Self.GC;
  end;
end;

procedure TSEGarbageCollector.Initial;
var
  I, J: Integer;
  Value, PrevValue: PSEGCNode;
begin
  if Self.FRunCount mod Self.FOldObjectCheckCycle = 0 then
  begin
    I := Self.FNodeLastOld;
    while I <> 0 do
    begin
      Value := Self.FNodeList.Ptr(I);
      Value^.Garbage := not Value^.Lock;
      I := Value^.Prev;
    end;
  end else
  begin
    I := Self.FNodeLastYoung;
    while I <> 0 do
    begin
      Value := Self.FNodeList.Ptr(I);
      J := I;
      I := Value^.Prev;
      if Value^.Visit >= Self.FPromotion then
      begin
        // Detach from young generation
        if J <> Self.FNodeLastYoung then
        begin
          PrevValue := Self.FNodeList.Ptr(Value^.Prev);
          PrevValue^.Next := Value^.Next;
          Self.FNodeList.Ptr(Value^.Next)^.Prev := Value^.Prev;
        end else
        begin
          Self.FNodeLastYoung := Value^.Prev;
          Self.FNodeList.Ptr(Self.FNodeLastYoung)^.Next := 0;
        end;
        // Attach to old generation
        Value^.Prev := Self.FNodeLastOld;
        Value^.Next := 0;
        Self.FNodeList.Ptr(Self.FNodeLastOld)^.Next := J;
        Self.FNodeLastOld := J;
        Inc(Self.FObjectsOld);
      end else
      begin
        Value^.Garbage := not Value^.Lock;
        Inc(Value^.Visit);
      end;
    end;
  end;
end;

procedure TSEGarbageCollector.Sweep(const AFirst: Cardinal); inline;
var
  Value: PSEGCNode;
  I, MS: Integer;
  LastPtr: PCardinal;

  procedure Detach;
  var
    PrevValue: PSEGCNode;
  begin
    if I <> LastPtr^ then
    begin
      PrevValue := Self.FNodeList.Ptr(Value^.Prev);
      PrevValue^.Next := Value^.Next;
      Self.FNodeList.Ptr(Value^.Next)^.Prev := Value^.Prev;
    end else
    begin
      LastPtr^ := Value^.Prev;
      Self.FNodeList.Ptr(LastPtr^)^.Next := 0;
    end;
    Value^.Value := Default(TSEValue);
    Self.FNodeAvailStack.Push(I);
    Dec(Self.FObjects);
    if AFirst = 2 then
      Dec(Self.FObjectsOld);
  end;

begin
  case AFirst of
    1: LastPtr := @Self.FNodeLastYoung;
    2: LastPtr := @Self.FNodeLastOld;
    else
      raise Exception.Create('AFirst must be 1 or 2!');
  end;
  I := LastPtr^;
  while I <> 0 do
  begin
    Value := Self.FNodeList.Ptr(I);
    if Value^.Garbage then
    begin
      case Value^.Value.Kind of
        sevkMap:
          begin
            if Value^.Value.VarMap <> nil then
            begin
              Value^.Value.VarMap.Free;
            end;
            Detach;
          end;
        sevkString:
          begin
            if Value^.Value.VarString <> nil then
            begin
              Dispose(Value^.Value.VarString);
            end;
            Detach;
          end;
        sevkBuffer:
          begin
            if Value^.Value.VarBuffer <> nil then
            begin
              if Value^.Value.VarBuffer^.Base <> nil then
              begin
                FreeMem(Value^.Value.VarBuffer^.Base);
              end;
              Dispose(Value^.Value.VarBuffer);
            end;
            Detach;
          end;
        sevkPascalObject:
          begin
            if Value^.Value.VarPascalObject <> nil then
            begin
              if Value^.Value.VarPascalObject^.IsManaged then
                Value^.Value.VarPascalObject^.Value.Free;
              Dispose(Value^.Value.VarPascalObject);
            end;
            Detach;
          end;
      end;
    end;
    I := Value^.Prev;
  end;
  Self.FObjectsLastTimeVisited := Self.FObjects;
end;

procedure TSEGarbageCollector.Mark(const PValue: PSEValue); inline;
var
  Value: PSEGCNode;
  RValue: TSEValue;
  Key: String;
  I: Integer;
begin
  if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) and (PValue^.Kind <> sevkBuffer) and (PValue^.Kind <> sevkPascalObject) then
    Exit;
  Value := Self.FNodeList.Ptr(PValue^.Ref);
  if Value^.Marked >= Self.FRunCount then
    Exit;
  Value^.Marked := Self.FRunCount;
  Value^.Garbage := False;
  if Value^.Value.VarPointer = PValue^.VarPointer then
  begin
    case Value^.Value.Kind of
      sevkMap:
        begin
          if PValue^.VarMap <> nil then
          begin
            if SEMapIsValidArray(PValue^) then
            begin
              TSEValueMap(PValue^.VarMap).Lock;
              try
                for I := 0 to TSEValueMap(PValue^.VarMap).Count - 1 do
                begin
                  RValue := SEMapGet(PValue^, I);
                  if (RValue.Kind <> sevkMap) and (RValue.Kind <> sevkString) and (RValue.Kind <> sevkBuffer) and (RValue.Kind <> sevkPascalObject) then
                    Continue;
                  Mark(@RValue);
                end;
              finally
                TSEValueMap(PValue^.VarMap).Unlock;
              end;
            end else
            begin
              TSEValueMap(PValue^.VarMap).Lock;
              try
                for Key in TSEValueMap(PValue^.VarMap).Map.Keys do
                begin
                  RValue := SEMapGet(PValue^, Key);
                  if (RValue.Kind <> sevkMap) and (RValue.Kind <> sevkString) and (RValue.Kind <> sevkBuffer) and (RValue.Kind <> sevkPascalObject) then
                    Continue;
                  Mark(@RValue);
                end;
              finally
                TSEValueMap(PValue^.VarMap).Unlock;
              end;
            end;
          end;
        end;
    end;
  end;
end;

procedure TSEGarbageCollector.GC(const Forced: Boolean = False);
var
  Value: PSEGCNode;
  PrevValue: PSEGCNode;
  P, P2: PSEValue;
  V: TSEValue;
  VM: TSEVM;
  I: Integer;
  Key: String;
  Cache: TSECache;
  Binary: TSEBinary;

  procedure SuspendThreads;
  var
    I: Integer;
  begin
    {$ifdef SE_THREADS}
      Self.FVMThreadList.Clear;
      Self.FPhase := segcpInitial;
      for I := 0 to VMList.Count - 1 do
      begin
        if (VMList[I].ThreadOwner <> nil) and (not VMList[I].ThreadOwner.Suspended) then
        begin
          VMList[I].ThreadOwner.IsRequestForSuspendByGC := True;
          {$ifdef UNIX}
          VMList[I].IsRequestForSuspend := True;
          while not VMList[I].ThreadOwner.Suspended do ;
          {$else}
          VMList[I].ThreadOwner.Suspend;
          {$endif}
          FVMThreadList.Add(VMList[I]);
        end;
      end;
      {$ifdef UNIX}
      for I := 0 to Self.FVMThreadList.Count - 1 do
      begin
        while not Self.FVMThreadList[I].ThreadOwner.Suspended do ;
      end;
      {$endif}
    {$endif}
  end;

  procedure ResumeThreads;
  var
    I: Integer;
  begin
    {$ifdef SE_THREADS}
    for I := 0 to FVMThreadList.Count - 1 do
    begin
      Self.FVMThreadList[I].ThreadOwner.Resume;
      Self.FVMThreadList[I].ThreadOwner.IsRequestForSuspendByGC := False;
    end;
    Self.FVMThreadList.Clear;
    {$endif}
  end;

  procedure Marking;
  var
    I: Integer;
  begin
  {$ifdef SE_THREADS}
    if Self.EnableParallel then
    begin
      Self.FReachableValueList.Clear;
      for I := 0 to VMList.Count - 1 do
      begin
        VM := VMList[I];
        P := @VM.Stack[0];
        while P < VM.StackPtr do
        begin
          Self.FReachableValueList.Add(P^);
          Inc(P);
        end;
        P := @VM.Global.Value^.Data[0];
        P2 := @VM.Global.Value^.Data[VM.Global.Value^.Size - 1];
        while P <= P2 do
        begin
          Self.FReachableValueList.Add(P^);
          Inc(P);
        end;
        for Key in VM.Parent.ConstMap.Keys do
        begin
          V := VM.Parent.ConstMap[Key];
          Self.FReachableValueList.Add(V);
        end;
      end;
      Self.FReachableValueList.Add(ScriptVarMap);
      GCMarkJob.Resume;
    end else
  {$endif}
    begin
      for I := 0 to VMList.Count - 1 do
      begin
        VM := VMList[I];
        P := @VM.Stack[0];
        while P <= VM.StackPtr do
        begin
          Self.Mark(P);
          Inc(P);
        end;
        P := @VM.Global.Value^.Data[0];
        P2 := @VM.Global.Value^.Data[VM.Global.Value^.Size - 1];
        while P <= P2 do
        begin
          Self.Mark(P);
          Inc(P);
        end;
        for Key in VM.Parent.ConstMap.Keys do
        begin
          V := VM.Parent.ConstMap[Key];
          Self.Mark(@V)
        end;
      end;
      Mark(@ScriptVarMap);
      Self.FPhase := segcpSweep;
    end;
  end;

begin
  if Self.FLockFlag then
    Exit;
  if Self.FNodeLastYoung = 0 then
    Exit;
  if (not Forced) and (Self.FObjectsLastTimeVisited + Self.FObjectThreshold > Self.FObjects) then
    Exit;
  if IsThread > 0 then
    Exit;
  {$ifdef SE_THREADS}
  if TryEnterCriticalSection(CS) = 0 then
  begin
    Self.FTicks := GetTickCount64;
    Exit;
  end;
  {$endif}
  FVMThreadList := TSEVMList.Create;
  try
    try
      if Self.FPhase = segcpRest then
      begin
        Self.FPhase := segcpInitial;
        SuspendThreads;
        {$ifdef SE_LOG}
        Writeln('[GC] ', GC.Phase);
        {$endif}
        Inc(Self.FRunCount);
        {$ifdef SE_LOG}
        Writeln('[GC] Number of objects before cleaning: ', Self.FObjects);
        Writeln('[GC] Number of old objects before cleaning: ', Self.FObjectsOld);
        Writeln('[GC] Number of objects in object pool: ', Self.FNodeAvailStack.Count);
        {$endif}
        Self.Initial;

        Self.FPhase := segcpMark;
        {$ifdef SE_LOG}
        Writeln('[GC] ', Self.FPhase);
        {$endif}
        Marking;
        if Self.EnableParallel then
          Exit;
      end;

      // Wait for the thread to finish it's job
      {$ifdef SE_THREADS}
      if Self.EnableParallel then
        if Self.FPhase = segcpMark then
          Exit;
      {$endif}

      if Self.FPhase = segcpSweep then
      begin
        {$ifdef SE_LOG}
        Writeln('[GC] ', Self.FPhase);
        {$endif}
        if Self.FRunCount mod Self.FOldObjectCheckCycle = 0 then
        begin
          Sweep(2);
        end else
        begin
          Sweep(1);
        end;
        {$ifdef SE_LOG}
        Writeln('[GC] Number of objects after cleaning: ', Self.FObjects);
        Writeln('[GC] Number of old objects after cleaning: ', Self.FObjectsOld);
        Writeln('[GC] Number of objects in object pool: ', Self.FNodeAvailStack.Count);
        Writeln('[GC] Time: ', GetTickCount64 - T, 'ms');
        {$endif}
      end;
    except
      on E: Exception do
      begin
        Writeln(DumpCallStack);
        Writeln(E.Message);
        Halt;
      end;
    end;
  finally
    if Self.FPhase = segcpSweep then
    begin
      Self.FPhase := segcpRest;
      {$ifdef SE_LOG}
      Writeln('[GC] ', Self.FPhase);
      {$endif}
    end;
    ResumeThreads;
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
    Self.FTicks := GetTickCount64;
  end;
end;

procedure TSEGarbageCollector.AllocBuffer(const PValue: PSEValue; const Size: Integer);
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    PValue^.Kind := sevkBuffer;
    New(PValue^.VarBuffer);
    if Size > 0 then
    begin
      GetMem(PValue^.VarBuffer^.Base, Size + 16);
      PValue^.VarBuffer^.Ptr := Pointer(QWord(PValue^.VarBuffer^.Base) + QWord(PValue^.VarBuffer^.Base) mod 16);
    end else
    begin
      PValue^.VarBuffer^.Base := nil;
      PValue^.VarBuffer^.Ptr := nil;
    end;
    Self.AddToList(PValue);
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure TSEGarbageCollector.AllocMap(const PValue: PSEValue);
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    PValue^.Kind := sevkMap;
    PValue^.VarMap := TSEValueMap.Create;
    Self.AddToList(PValue);
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure TSEGarbageCollector.AllocString(const PValue: PSEValue; const S: String);
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    PValue^.Kind := sevkString;
    New(PValue^.VarString);
    PValue^.VarString^ := S;
    Self.AddToList(PValue);
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure  TSEGarbageCollector.AllocPascalObject(const PValue: PSEValue; const Obj: TObject; const IsManaged: Boolean);
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    PValue^.Kind := sevkPascalObject;
    New(PValue^.VarPascalObject);
    PValue^.VarPascalObject^.Value := Obj;
    PValue^.VarPascalObject^.IsManaged := IsManaged;
    Self.AddToList(PValue);
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure TSEGarbageCollector.UnManaged(const PValue: PSEValue);
var
  Value: TSEGCNode;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) and (PValue^.Kind <> sevkBuffer) and (PValue^.Kind <> sevkPascalObject) then
      Exit;
    Value := Self.FNodeList[PValue^.Ref];
    Value.Lock := True;
    Self.FNodeList[PValue^.Ref] := Value;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure TSEGarbageCollector.Managed(const PValue: PSEValue);
var
  Value: TSEGCNode;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(CS);
  {$endif}
  try
    if (PValue^.Kind <> sevkMap) and (PValue^.Kind <> sevkString) and (PValue^.Kind <> sevkBuffer) and (PValue^.Kind <> sevkPascalObject) then
      Exit;
    Value := Self.FNodeList[PValue^.Ref];
    Value.Lock := False;
    Self.FNodeList[PValue^.Ref] := Value;
  finally
    {$ifdef SE_THREADS}
    LeaveCriticalSection(CS);
    {$endif}
  end;
end;

procedure TSEGarbageCollector.Lock;
begin
  {$ifdef SE_THREADS}
  EnterCriticalSection(Self.FLock);
  {$endif}
  Self.FLockFlag := True;
end;

procedure TSEGarbageCollector.Unlock;
begin
  Self.FLockFlag := False;
  {$ifdef SE_THREADS}
  LeaveCriticalSection(Self.FLock);
  {$endif}
end;

procedure TSEVM.BinaryClear;
var
  I: Integer;
begin
  for I := 0 to Self.Binaries.Value^.Size - 1 do
    FreeAndNil(Self.Binaries.Value^.Data[I]);
  Self.Binaries.Alloc(1);
  Self.Binaries.Value^.Data[0] := TSEBinary.Create;
end;

function TSEVM.Fork(const AStackSize: Cardinal): TSEVM;
var
  StackCount: Cardinal;
  I: Integer;
begin
  Result := TSEVM.Create;
  Result.StackSize := AStackSize;
  Result.Parent := Self.Parent;
  Result.IsPaused := False;
  Result.IsDone := False;
  Result.Parent.IsDone := False;
  Result.Global := Self.Global.Ref;
  SetLength(Result.Stack, AStackSize);
  SetLength(Result.Frame, Result.FrameSize);
  SetLength(Result.Trap, Result.TrapSize);
  Result.StackPtr := PSEValue(@Result.Stack[0]) + SE_STACK_RESERVED;
  Result.FramePtr := @Result.Frame[0];
  Result.FramePtr^.Stack := Result.StackPtr;
  Result.TrapPtr := @Result.Trap[0];
  Dec(Result.TrapPtr);
  //
  Result.Binaries := Self.Binaries.Ref;
  // TODO: Make sure these works correctly without crash
  Result.SymbolList := Self.SymbolList;
  Result.ConstStrings := Self.ConstStrings;
end;

procedure TSEVM.ModifyGlobalVariable(const AName: String; const AValue: TSEValue);
begin
  Self.SetGlobalVariable(AName, AValue);
end;

procedure TSEVM.SetGlobalVariable(const AName: String; const AValue: TSEValue);
var
  I: Integer;
begin
  for I := 0 to Self.Parent.GlobalVarSymbols.Count - 1 do
  begin
    if Self.Parent.GlobalVarSymbols[I] = AName then
    begin
      Self.Global.Value^.Data[I] := AValue;
      break;
    end;
  end;
end;

function TSEVM.GetGlobalVariable(const AName: String): PSEValue;
var
  I: Integer;
begin
  for I := 0 to Self.Parent.GlobalVarSymbols.Count - 1 do
  begin
    if Self.Parent.GlobalVarSymbols[I] = AName then
    begin
      Result := @Self.Global.Value^.Data[I];
      break;
    end;
  end;
end;

procedure TSEValueArrayManaged.Alloc(const ASize: Cardinal);
begin
  if Self.Value = nil then
  begin
    New(Self.Value);
    Self.Value^.Data := GetMem(SizeOf(TSEValue) * ASize);
    Self.Value^.RefCount := 1;
    Self.Value^.Size := ASize;
  end else
  begin
    ReAllocMem(Self.Value^.Data, SizeOf(TSEValue) * ASize);
    Self.Value^.Size := ASize;
  end;
end;

function TSEValueArrayManaged.Ref: TSEValueArrayManaged;
begin
  Inc(Self.Value^.RefCount);
  Result := Self;
end;

procedure TSEValueArrayManaged.Free;
begin
  Dec(Self.Value^.RefCount);
  Assert(Self.Value^.RefCount >= 0, 'RefCount < 0');
  if Self.Value^.RefCount = 0 then
  begin
    FreeMem(Self.Value^.Data);
    Dispose(Self.Value);
  end;
end;

procedure TSEBinariesManaged.Alloc(const ASize: Cardinal);
begin
  if Self.Value = nil then
  begin
    New(Self.Value);
    SetLength(Self.Value^.Data, ASize);
    Self.Value^.RefCount := 1;
    Self.Value^.Size := ASize;
  end else
  begin
    SetLength(Self.Value^.Data, ASize);
    Self.Value^.Size := ASize;
  end;
end;

function TSEBinariesManaged.Ref: TSEBinariesManaged;
begin
  Inc(Self.Value^.RefCount);
  Result := Self;
end;

procedure TSEBinariesManaged.Free;
var
  I: Integer;
begin
  Dec(Self.Value^.RefCount);
  Assert(Self.Value^.RefCount >= 0, 'RefCount < 0');
  if Self.Value^.RefCount = 0 then
  begin
    for I := 0 to High(Self.Value^.Data) do
      Self.Value^.Data[I].Free;
    Dispose(Self.Value);
  end;
end;

constructor TSEVM.Create;
begin
  inherited;
  Self.CodePtr := 0;
  Self.IsPaused := False;
  Self.IsDone := True;
  Self.StackSize := 2048;
  Self.FrameSize := 1024;
  Self.TrapSize := 1024;
  if VMList = nil then
    VMList := TSEVMList.Create;
  if GC = nil then
    GC := TSEGarbageCollector.Create;
  VMList.Add(Self);
  Self.Binaries := Default(TSEBinariesManaged);
  Self.Binaries.Alloc(1);
  Self.Binaries.Value^.Data[0] := TSEBinary.Create;
  Self.ConstStrings := TStringList.Create;
  Self.ConstStrings.Capacity := 64;
  Self.SymbolList := TSESymbolList.Create;
  Self.Global := Default(TSEValueArrayManaged);
end;

destructor TSEVM.Destroy;
begin
  Self.Binaries.Free;
  if VMList <> nil then
    VMList.Remove(Self);
  if {$ifdef SE_THREADS}(Self.ThreadOwner = nil) and{$endif} (Self.CoroutineOwner = nil) then
  begin
    Self.ConstStrings.Free;
    Self.SymbolList.Free;
  end;
  Self.Global.Free;
  inherited;
end;

procedure TSEVM.Reset;
begin
  Self.CodePtr := 0;
  Self.BinaryPtr := 0;
  Self.IsPaused := False;
  Self.IsDone := False;
  Self.Parent.IsDone := False;
  Self.Global.Alloc(Self.Parent.GlobalVarCount);
  SetLength(Self.Stack, Self.StackSize);
  SetLength(Self.Frame, Self.FrameSize);
  SetLength(Self.Trap, Self.TrapSize);
  FillChar(Self.Global.Value^.Data[0], Self.Parent.GlobalVarCount * SizeOf(TSEValue), 0);
  FillChar(Self.Stack[0], Length(Self.Stack) * SizeOf(TSEValue), 0);
  // The GC does not walk through Frames and Traps so we do not need to be filled those with null
  // FillChar(Self.Frame[0], Length(Self.Frame) * SizeOf(TSEFrame), 0);
  // FillChar(Self.Trap[0], Length(Self.Trap) * SizeOf(TSETrap), 0);
  Self.FramePtr := @Self.Frame[0];
  Self.StackPtr := @Self.Stack[0];
  Self.FramePtr^.Stack := Self.StackPtr;
  Self.TrapPtr := @Self.Trap[0];
  Dec(Self.TrapPtr);
end;

procedure TSEVM.Exec;
var
  A, B, C, V,
  OA, OB, OC, OV: PSEValue;
  TV, TV2: TSEValue;
  S, S1, S2: String;
  WS, WS1, WS2: UnicodeString;
  FuncNativeInfo: PSEFuncNativeInfo;
  FuncScriptInfo: PSEFuncScriptInfo;
  FuncImportInfo: PSEFuncImportInfo;
  I, J, ArgCountStack, ArgCount, ArgSize, DeepCount: Integer;
  This: TSEValue;
  CodePtrLocal: Integer;
  GlobalLocal: PSEValue;
  BinaryPtrLocal: Integer;
  BinaryLocal: PSEValue;
  FuncImport, P, PP, PC: Pointer;
  LineOfCode: TSELineOfCode;
  IsScriptException: Boolean = False;

  procedure GetLineOfCode;
  var
    I: Integer;
  begin
    if FramePtr = @Self.Frame[0] then
    begin
      I := Self.Parent.LineOfCodeList.Count - 1;
      while I >= 0 do
      begin
        LineOfCode := Self.Parent.LineOfCodeList[I];
        if (CodePtrLocal >= LineOfCode.BinaryCount) and (LineOfCode.BinaryPtr = 0) then
          break;
        Dec(I);
      end;
    end else
    begin
      I := 0;
      while I < Self.Parent.LineOfCodeList.Count - 1 do
      begin
        LineOfCode := Self.Parent.LineOfCodeList[I];
        if (CodePtrLocal < LineOfCode.BinaryCount) and (BinaryPtrLocal = LineOfCode.BinaryPtr) then
          break;
        Inc(I);
      end;
    end;
  end;

  procedure PrintEvilScriptStackTrace(Message: String);

    procedure AddChildNode(Node: PSEStackTraceSymbol; const AName: String; AValue: PSEValue);
    var
      I, C: Integer;
      Key: String;
      V: TSEValue;
    begin
      C := Length(Node^.Childs) + 1;
      SetLength(Node^.Childs, C);
      Node := @Node^.Childs[C - 1];
      Node^.Name := AName;
      Node^.Kind := AValue^.Kind;
      Node^.Ref := AValue;
      case AValue^.Kind of
        sevkMap:
          begin
            if SEMapIsValidArray(AValue^) then
            begin
              for I := 0 to TSEValueMap(AValue^.VarMap).Count - 1 do
              begin
                V := SEMapGet(AValue^, I);
                AddChildNode(Node, IntToStr(I), @V);
              end;
            end else
            begin
              TSEValueMap(AValue^.VarMap).Lock;
              try
                for Key in TSEValueMap(AValue^.VarMap).Map.Keys do
                begin
                  V := SEMapGet(AValue^, Key);
                  AddChildNode(Node, Key, @V);
                end;
              finally
                TSEValueMap(AValue^.VarMap).Unlock;
              end;
            end;
          end;
        else
          begin
            Node^.Value := SEValueToText(AValue^);
          end;
      end;
    end;

  var
    CurFrame: PSEFrame;
    CurFunc: PSEFuncScriptInfo;
    I, J: Integer;
    LineOfCode: TSELineOfCode;
    Nodes: TSEStackTraceSymbolArray;
    NodeCount: Integer = 0;
    BinaryPos: Integer;
  begin
    if Self.Parent.StackTraceHandler <> nil then
    begin
      for I := Self.FrameSize - 1 downto 1 do
      begin
        CurFrame := @Self.Frame[I];
        if CurFrame <= Self.FramePtr then
        begin
          Inc(NodeCount);
          SetLength(Nodes, NodeCount);
          CurFunc := CurFrame^.Func;

          J := Self.Parent.LineOfCodeList.Count - 1;
          while J >= 0 do
          begin
            LineOfCode := Self.Parent.LineOfCodeList[J];
            if I = 1 then
              BinaryPos := 0
            else
              BinaryPos := Self.Frame[I - 1].Func^.BinaryPos;
            if (CurFrame^.Binary < LineOfCode.BinaryCount) and (BinaryPos = LineOfCode.BinaryPtr) then
              break;
            Dec(J);
          end;
          Nodes[NodeCount - 1].Name := CurFunc^.Name + ' [' + LineOfCode.Module + ':' + IntToStr(LineOfCode.Line) + ']';
          for J := 0 to CurFrame^.Func^.VarSymbols.Count - 1 do
          begin
            AddChildNode(@Nodes[NodeCount - 1], CurFunc^.VarSymbols[J], @CurFrame^.Stack[J - 1]);
          end;
        end;
      end;
      // Global
      Inc(NodeCount);
      SetLength(Nodes, NodeCount);
      Nodes[NodeCount - 1].Name := 'global_variables';
      for J := 0 to Self.Parent.GlobalVarSymbols.Count - 1 do
      begin
        AddChildNode(@Nodes[NodeCount - 1], Self.Parent.GlobalVarSymbols[J], @Self.Global.Value^.Data[J]);
      end;
      Self.Parent.StackTraceHandler(Message, Nodes);
    end;
  end;

  procedure Push(const Value: TSEValue); inline;
  begin
    Self.StackPtr^ := Value;
    Inc(Self.StackPtr);
  end;

  function Pop: PSEValue; inline;
  begin
    Dec(Self.StackPtr);
    Result := Self.StackPtr;
  end;

  procedure AssignGlobal(const I: Pointer; const Value: PSEValue); inline;
  begin
    GlobalLocal[Integer(I)] := Value^;
  end;

  procedure AssignLocal(const I: Pointer; const F: Integer; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.Stack + Integer(I))^ := Value^;
  end;

  function GetGlobal(const I: Pointer): PSEValue; inline;
  begin
    Exit(@GlobalLocal[Integer(I)]);
  end;

  function GetLocal(const I: Pointer; const F: Integer): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.Stack + Integer(I));
  end;

  function GetGlobalInt(const I: Integer): PSEValue; inline;
  begin
    Exit(@GlobalLocal[Integer(I)]);
  end;

  function GetLocalInt(const I, F: Integer): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.Stack + Integer(I));
  end;

  procedure AssignGlobalInt(const I: Integer; const Value: PSEValue); inline;
  begin
    GlobalLocal[Integer(I)] := Value^;
  end;

  procedure AssignLocalInt(const I: Integer; const F: Integer; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.Stack + Integer(I))^ := Value^;
  end;

  function GetVariable(const I: Pointer; const F: Pointer): PSEValue; inline;
  begin
    if F = Pointer(SE_REG_GLOBAL) then
      Exit(@GlobalLocal[Integer(I)])
    else
      Exit((Self.FramePtr - Integer(F))^.Stack + Integer(I));
  end;

  procedure SetVariable(const I: Pointer; const F: Pointer; const Value: PSEValue); inline;
  begin
    if F = Pointer(SE_REG_GLOBAL) then
      GlobalLocal[Integer(I)] := Value^
    else
      ((Self.FramePtr - Integer(F))^.Stack + Integer(I))^ := Value^;
  end;

  procedure CallImportFunc;
  var
    I: Integer;
    ImportBufferIndex: array [0..31] of QWord;
    ImportBufferData: array [0..8*31] of Byte;
    ImportBufferString: array [0..31] of String;
    ImportBufferWideString: array [0..31] of UnicodeString;
    ImportResult: QWord;
    ImportResultD: TSENumber;
    ImportResultS: Single;
    ArgCountStack, ArgCount, ArgSize: Integer;
    FuncImport, P, PP: Pointer;
    {$ifdef SE_LIBFFI}
    ffiCif: ffi_cif;
    ffiArgTypes: array [0..31] of pffi_type;
    ffiArgValues: array [0..31] of Pointer;
    ffiResultType: ffi_type;
    ffiAbi: ffi_abi;
    {$endif}
  begin
    FuncImportInfo := Self.Parent.FuncImportList.Ptr(Integer(BinaryLocal[CodePtrLocal + 1].VarPointer));
    {$ifndef SE_LIBFFI}
      raise Exception.Create('You need to enable SE_LIBFFI in order to call external function "' + FuncImportInfo^.Name + '"');
    {$else}
    FuncImport := FuncImportInfo^.Func;
    if FuncImport = nil then
      raise Exception.Create(Format('Function "%s" is null', [FuncImportInfo^.Name]));
    ArgCount := Length(FuncImportInfo^.Args);
    ArgSize := ArgCount * 8;

    for I := ArgCount - 1 downto 0 do
    begin
      case FuncImportInfo^.Args[I] of
        seakI8:
          begin
            Int64((@ImportBufferData[I * 8])^) := ShortInt(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_sint8;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakI16:
          begin
            Int64((@ImportBufferData[I * 8])^) := SmallInt(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_sint16;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakI32:
          begin
            Int64((@ImportBufferData[I * 8])^) := LongInt(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_sint32;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakI64:
          begin
            Int64((@ImportBufferData[I * 8])^) := Int64(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_sint64;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU8:
          begin
            QWord((@ImportBufferData[I * 8])^) := Byte(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint8;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU16:
          begin
            QWord((@ImportBufferData[I * 8])^) := Word(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint16;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU32:
          begin
            QWord((@ImportBufferData[I * 8])^) := LongWord(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint32;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU64:
          begin
            QWord((@ImportBufferData[I * 8])^) := QWord(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint64;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakF32:
          begin
            Single((@ImportBufferData[I * 8])^) := Single(Pop^.VarNumber);
            ffiArgTypes[I] := @ffi_type_float;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakF64:
          begin
            TSENumber((@ImportBufferData[I * 8])^) := Pop^.VarNumber;
            ffiArgTypes[I] := @ffi_type_double;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakBuffer:
          begin
            A := Pop;
            if A^.Kind = sevkString then
            begin
              ImportBufferString[I] := A^.VarString^ + #0;
              PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferString[I]);
            end else
            if A^.Kind = sevkBuffer then
              PChar((@ImportBufferData[I * 8])^) := PChar(A^.VarBuffer^.Ptr)
            else
              QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
            ffiArgTypes[I] := @ffi_type_pointer;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakWBuffer:
          begin
            A := Pop;
            if A^.Kind = sevkString then
            begin
              ImportBufferWideString[I] := UTF8Decode(A^.VarString^ + #0);
              PChar((@ImportBufferData[I * 8])^) := PChar(ImportBufferWideString[I]);
            end else
            if A^.Kind = sevkBuffer then
              PWideChar((@ImportBufferData[I * 8])^) := PWideChar(A^.VarBuffer^.Ptr)
            else
              QWord((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
            ffiArgTypes[I] := @ffi_type_pointer;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
      end;
    end;
    case FuncImportInfo^.Return of
      seakI8:
        begin
          ffiResultType := ffi_type_sint8;
        end;
      seakI16:
        begin
          ffiResultType := ffi_type_sint16;
        end;
      seakI32:
        begin
          ffiResultType := ffi_type_sint32;
        end;
      seakI64:
        begin
          ffiResultType := ffi_type_sint64;
        end;
      seakU8:
        begin
          ffiResultType := ffi_type_uint8;
        end;
      seakU16:
        begin
          ffiResultType := ffi_type_uint16;
        end;
      seakU32:
        begin
          ffiResultType := ffi_type_uint32;
        end;
      seakU64:
        begin
          ffiResultType := ffi_type_uint64;
        end;
      seakF32:
        begin
          ffiResultType := ffi_type_float;
        end;
      seakF64:
        begin
          ffiResultType := ffi_type_double;
        end;
      seakBuffer, seakWBuffer:
        begin
          ffiResultType := ffi_type_pointer;
        end;
    end;
    case FuncImportInfo^.CallingConvention of
      seccAuto:
        ffiAbi := FFI_DEFAULT_ABI;
      {$ifdef CPUI386}
      seccStdcall:
        ffiAbi := FFI_STDCALL;
      seccCdecl:
        ffiAbi := FFI_MS_CDECL;
      {$endif}
      else
        ffiAbi := FFI_DEFAULT_ABI;
    end;
    I := Integer(ffi_prep_cif(@ffiCif, ffiAbi, ArgCount, @ffiResultType, @ffiArgTypes[0]));
    if I <> Integer(FFI_OK) then
      raise Exception.Create('FFI status is not OK (' + IntToStr(I) + ') while calling external function "' + FuncImportInfo^.Name + '"');
    ffi_call(@ffiCif, ffi_fn(FuncImport), @ImportResult, @ffiArgValues[0]);
    if FuncImportInfo^.Return = seakF32 then
      ImportResultS := PSingle(@ImportResult)^
    else
    if FuncImportInfo^.Return = seakF64 then
      ImportResultD := PDouble(@ImportResult)^;

    case FuncImportInfo^.Return of
      seakI8, seakI16, seakI32:
        begin
          TV := Int64(LongInt(ImportResult));
        end;
      seakI64:
        begin
          TV := Int64(ImportResult);
        end;
      seakU8, seakU16, seakU32:
        begin
          TV := QWord(LongWord(ImportResult));
        end;
      seakU64:
        begin
          TV := QWord(ImportResult);
        end;
      seakBuffer, seakWBuffer:
        begin
          GC.AllocBuffer(@TV, 0);
          TV.VarBuffer^.Ptr := Pointer(QWord(ImportResult));
        end;
      seakF32:
        begin
          TV := ImportResultS;
        end;
      seakF64:
        begin
          TV := ImportResultD;
        end;
    end;
    Push(TV);
    Inc(CodePtrLocal, 4);
    {$endif}
  end;

{$ifdef SE_COMPUTED_GOTO}
  {$if defined(CPUX86_64) or defined(CPUi386)}
    {$define DispatchGoto :=
      P := DispatchTable[TSEOpcode(Integer(BinaryLocal[CodePtrLocal].VarPointer))];
      asm
        jmp P;
      end
    }
  {$elseif defined(CPUARM) or defined(CPUAARCH64)}
    {$define DispatchGoto :=
      P := DispatchTable[TSEOpcode(Integer(BinaryLocal[CodePtrLocal].VarPointer))];
      asm
        b P;
      end
    }
  {$endif}
{$else}
  {$define DispatchGoto := ;}
{$endif}
{$ifdef Unix}
  {$define CheckForSuspend :=
    if Self.ThreadOwner <> nil then
    begin
      if Self.IsRequestForSuspend then
        Self.ThreadOwner.Suspend;
      Self.IsRequestForSuspend := False;
    end
  }
{$else}
  {$define CheckForSuspend := ;}
{$endif}

label
  labelStart, CallScript, CallNative, CallImport,
  labelPushConst,
  labelPushConstString,
  labelPushGlobalVar,
  labelPushLocalVar,
  labelPushVar2,
  labelPushArrayPop,
  labelPushArrayPopString,
  labelPopConst,
  labelPopFrame,
  labelAssignGlobalVar,
  labelAssignGlobalArray,
  labelAssignLocalVar,
  labelAssignLocalArray,
  labelAssignArrayFast,
  labelAssignMapFast,
  labelJumpEqual,
  labelJumpEqual1,
  labelJumpUnconditional,
  labelJumpEqualOrGreater2,
  labelJumpEqualOrLesser2,

  labelJumpEqual1Rel,
  labelJumpUnconditionalRel,

  labelOperatorInc,

  labelOperatorAdd0,
  labelOperatorMul0,
  labelOperatorDiv0,

  labelOperatorAdd1,
  labelOperatorSub1,
  labelOperatorMul1,
  labelOperatorDiv1,

  labelOperatorAdd2,
  labelOperatorSub2,
  labelOperatorMul2,
  labelOperatorDiv2,

  labelOperatorAdd,
  labelOperatorSub,
  labelOperatorMul,
  labelOperatorDiv,
  labelOperatorMod,
  labelOperatorPow,
  labelOperatorNegative,
  labelOperatorLesser,
  labelOperatorLesserOrEqual,
  labelOperatorGreater,
  labelOperatorGreaterOrEqual,
  labelOperatorEqual,
  labelOperatorNotEqual,
  labelOperatorAnd,
  labelOperatorOr,
  labelOperatorXor,
  labelOperatorNot,
  labelOperatorShiftLeft,
  labelOperatorShiftRight,

  labelCallRef,
  labelCallNative,
  labelCallScript,
  labelCallImport,
  labelYield,
  labelHlt,
  {$ifdef UNIX}
  labelBlockCleanup,
  {$endif}
  labelPushTrap,
  labelPopTrap,
  labelThrow;

{$ifdef SE_COMPUTED_GOTO}
var
  DispatchTable: array[TSEOpcode] of Pointer = (
    @labelPushConst,
    @labelPushConstString,
    @labelPushGlobalVar,
    @labelPushLocalVar,
    @labelPushVar2,
    @labelPushArrayPop,
    @labelPushArrayPopString,
    @labelPopConst,
    @labelPopFrame,
    @labelAssignGlobalVar,
    @labelAssignGlobalArray,
    @labelAssignLocalVar,
    @labelAssignLocalArray,
    @labelAssignArrayFast,
    @labelAssignMapFast,
    @labelJumpEqual,
    @labelJumpEqual1,
    @labelJumpUnconditional,
    @labelJumpEqualOrGreater2,
    @labelJumpEqualOrLesser2,

    @labelJumpEqual1Rel,
    @labelJumpUnconditionalRel,

    @labelOperatorInc,

    @labelOperatorAdd0,
    @labelOperatorMul0,
    @labelOperatorDiv0,

    @labelOperatorAdd1,
    @labelOperatorSub1,
    @labelOperatorMul1,
    @labelOperatorDiv1,

    @labelOperatorAdd2,
    @labelOperatorSub2,
    @labelOperatorMul2,
    @labelOperatorDiv2,

    @labelOperatorAdd,
    @labelOperatorSub,
    @labelOperatorMul,
    @labelOperatorDiv,
    @labelOperatorMod,
    @labelOperatorPow,
    @labelOperatorNegative,
    @labelOperatorLesser,
    @labelOperatorLesserOrEqual,
    @labelOperatorGreater,
    @labelOperatorGreaterOrEqual,
    @labelOperatorEqual,
    @labelOperatorNotEqual,
    @labelOperatorAnd,
    @labelOperatorOr,
    @labelOperatorXor,
    @labelOperatorNot,
    @labelOperatorShiftLeft,
    @labelOperatorShiftRight,

    @labelCallRef,
    @labelCallNative,
    @labelCallScript,
    @labelCallImport,
    @labelYield,
    @labelHlt,

    {$ifdef UNIX}
    @labelBlockCleanup,
    {$endif}
    @labelPushTrap,
    @labelPopTrap,
    @labelThrow
  );
{$endif}

begin
  if Self.IsDone then
    Self.Reset;
  Self.IsYielded := False;
  if Self.IsPaused then
    Exit;
  GlobalLocal := @Self.Global.Value^.Data[0];
  CodePtrLocal := Self.CodePtr;
  BinaryPtrLocal := Self.BinaryPtr;
  BinaryLocal := Self.Binaries.Value^.Data[Self.BinaryPtr].Ptr(0);
  GC.CheckForGC;

labelStart:
  while True do
  try
    DispatchGoto;
    while True do
    begin
      {$ifndef SE_COMPUTED_GOTO}
      case TSEOpcode(Integer(BinaryLocal[CodePtrLocal].VarPointer)) of
      {$endif}
      {$ifndef SE_COMPUTED_GOTO}opOperatorInc:{$endif}
        begin
        labelOperatorInc:
          P  := BinaryLocal[CodePtrLocal + 1].VarPointer;
          PP := BinaryLocal[CodePtrLocal + 2].VarPointer;
          V  := GetVariable(P, PP);
          V^.VarNumber := V^.VarNumber + BinaryLocal[CodePtrLocal + 3].VarNumber;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd0:{$endif}
        begin
        labelOperatorAdd0:
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber + BinaryLocal[CodePtrLocal + 1].VarNumber
          else
            SEValueAdd(Self.StackPtr^, A^, BinaryLocal[CodePtrLocal + 1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMul0:{$endif}
        begin
        labelOperatorMul0:
          A := Pop;
          Self.StackPtr^.VarNumber := A^.VarNumber * BinaryLocal[CodePtrLocal + 1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv0:{$endif}
        begin
        labelOperatorDiv0:
          A := Pop;
          Self.StackPtr^.VarNumber := A^.VarNumber / BinaryLocal[CodePtrLocal + 1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd:{$endif}
        begin
        labelOperatorAdd:
          B := Pop;
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber + B^.VarNumber
          else
            SEValueAdd(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorSub:{$endif}
        begin
        labelOperatorSub:
          B := Pop;
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber - B^.VarNumber
          else
            SEValueSub(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMul:{$endif}
        begin
        labelOperatorMul:
          B := Pop;
          A := Pop;
          SEValueMul(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv:{$endif}
        begin
        labelOperatorDiv:
          B := Pop;
          A := Pop;
          SEValueDiv(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMod:{$endif}
        begin
        labelOperatorMod:
          B := Pop;
          A := Pop;
          Push(A^.VarNumber - B^.VarNumber * Int(A^.VarNumber / B^.VarNumber));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorEqual:{$endif}
        begin
        labelOperatorEqual:
          B := Pop;
          A := Pop;
          SEValueEqual(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNotEqual:{$endif}
        begin
        labelOperatorNotEqual:
          B := Pop;
          A := Pop;
          SEValueNotEqual(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorShiftLeft:{$endif}
        begin
        labelOperatorShiftLeft:
          B := Pop;
          A := Pop;
          SEValueShiftLeft(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorShiftRight:{$endif}
        begin
        labelOperatorShiftRight:
          B := Pop;
          A := Pop;
          SEValueShiftRight(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorLesser:{$endif}
        begin
        labelOperatorLesser:
          B := Pop;
          A := Pop;
          SEValueLesser(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorLesserOrEqual:{$endif}
        begin
        labelOperatorLesserOrEqual:
          B := Pop;
          A := Pop;
          SEValueLesserOrEqual(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreater:{$endif}
        begin
        labelOperatorGreater:
          B := Pop;
          A := Pop;
          SEValueGreater(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreaterOrEqual:{$endif}
        begin
        labelOperatorGreaterOrEqual:
          B := Pop;
          A := Pop;
          SEValueGreaterOrEqual(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAnd:{$endif}
        begin
        labelOperatorAnd:
          B := Pop;
          A := Pop;
          Push(Integer(A^) and Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorOr:{$endif}
        begin
        labelOperatorOr:
          B := Pop;
          A := Pop;
          Push(Integer(A^) or Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorXor:{$endif}
        begin
        labelOperatorXor:
          B := Pop;
          A := Pop;
          Push(Integer(A^) xor Integer(B^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNot:{$endif}
        begin
        labelOperatorNot:
          A := Pop;
          SEValueNot(Self.StackPtr^, A^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNegative:{$endif}
        begin
        labelOperatorNegative:
          A := Pop;
          SEValueNeg(Self.StackPtr^, A^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd1:{$endif}
        begin
        labelOperatorAdd1:
          A := Pop;
          P := BinaryLocal[CodePtrLocal + 2].VarPointer;
          B := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber + B^.VarNumber
          else
            SEValueAdd(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorSub1:{$endif}
        begin
        labelOperatorSub1:
          A := Pop;
          P := BinaryLocal[CodePtrLocal + 2].VarPointer;
          B := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber - B^.VarNumber
          else
            SEValueSub(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMul1:{$endif}
        begin
        labelOperatorMul1:
          A := Pop;
          P := BinaryLocal[CodePtrLocal + 2].VarPointer;
          B := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          SEValueMul(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv1:{$endif}
        begin
        labelOperatorDiv1:
          A := Pop;
          P := BinaryLocal[CodePtrLocal + 2].VarPointer;
          B := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          SEValueDiv(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd2:{$endif}
        begin
        labelOperatorAdd2:
          P  := BinaryLocal[CodePtrLocal + 3].VarPointer;
          PP := BinaryLocal[CodePtrLocal + 4].VarPointer;
          A := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          B := GetVariable(BinaryLocal[CodePtrLocal + 2], PP);
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber + B^.VarNumber
          else
            SEValueAdd(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 5);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorSub2:{$endif}
        begin
        labelOperatorSub2:
          P  := BinaryLocal[CodePtrLocal + 3].VarPointer;
          PP := BinaryLocal[CodePtrLocal + 4].VarPointer;
          A := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          B := GetVariable(BinaryLocal[CodePtrLocal + 2], PP);
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber - B^.VarNumber
          else
            SEValueSub(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 5);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMul2:{$endif}
        begin
        labelOperatorMul2:
          P  := BinaryLocal[CodePtrLocal + 3].VarPointer;
          PP := BinaryLocal[CodePtrLocal + 4].VarPointer;
          A := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          B := GetVariable(BinaryLocal[CodePtrLocal + 2], PP);
          SEValueMul(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 5);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv2:{$endif}
        begin
        labelOperatorDiv2:
          P  := BinaryLocal[CodePtrLocal + 3].VarPointer;
          PP := BinaryLocal[CodePtrLocal + 4].VarPointer;
          A := GetVariable(BinaryLocal[CodePtrLocal + 1], P);
          B := GetVariable(BinaryLocal[CodePtrLocal + 2], PP);
          SEValueDiv(Self.StackPtr^, A^, B^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 5);
          DispatchGoto;
        end;

      {$ifndef SE_COMPUTED_GOTO}opPushConst:{$endif}
        begin
        labelPushConst:
          Push(BinaryLocal[CodePtrLocal + 1]);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushConstString:{$endif}
        begin
        labelPushConstString:
          Push(Self.ConstStrings[Integer(BinaryLocal[CodePtrLocal + 1].VarPointer)]);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushGlobalVar:{$endif}
        begin
        labelPushGlobalVar:
          Push(GetGlobal(BinaryLocal[CodePtrLocal + 1].VarPointer)^);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushLocalVar:{$endif}
        begin
        labelPushLocalVar:
          Push(GetLocal(BinaryLocal[CodePtrLocal + 1].VarPointer, Integer(BinaryLocal[CodePtrLocal + 2].VarPointer))^);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushVar2:{$endif}
        begin
        labelPushVar2:
          Push(GetVariable(BinaryLocal[CodePtrLocal + 1].VarPointer, BinaryLocal[CodePtrLocal + 3].VarPointer)^);
          Push(GetVariable(BinaryLocal[CodePtrLocal + 2].VarPointer, BinaryLocal[CodePtrLocal + 4].VarPointer)^);
          Inc(CodePtrLocal, 5);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushArrayPop:{$endif}
        begin
        labelPushArrayPop:
          A := @BinaryLocal[CodePtrLocal + 1];
          if A^.Kind = sevkNull then
            A := Pop;
          B := Pop;
          case B^.Kind of
            sevkString:
              {$ifdef SE_STRING_UTF8}
                Push(UTF8Copy(B^.VarString^, Integer(A^) + 1, 1));
              {$else}
                Push(B^.VarString^[Integer(A^) + 1]);
              {$endif}
            sevkMap:
              Push(SEMapGet(B^, A^));
            else
              Push(0);
          end;
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushArrayPopString:{$endif}
        begin
        labelPushArrayPopString:
          B := Pop;
          Push(SEMapGet(B^, Self.ConstStrings[Integer(BinaryLocal[CodePtrLocal + 1].VarPointer)]));
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPopConst:{$endif}
        begin
        labelPopConst:
          Dec(Self.StackPtr); // Pop;
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqual:{$endif}
        begin
        labelJumpEqual:
          B := Pop;
          A := Pop;
          if SEValueEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal[CodePtrLocal + 1].VarPointer)
          else
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqual1:{$endif}
        begin
        labelJumpEqual1:
          A := Pop;
          if SEValueEqual(A^, BinaryLocal[CodePtrLocal + 1]) then
            CodePtrLocal := Integer(BinaryLocal[CodePtrLocal + 2].VarPointer)
          else
            Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpUnconditional:{$endif}
        begin
        labelJumpUnconditional:
          CodePtrLocal := Integer(BinaryLocal[CodePtrLocal + 1].VarPointer);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqualOrGreater2:{$endif}
        begin
        labelJumpEqualOrGreater2:
          B := GetVariable(BinaryLocal[CodePtrLocal + 3].VarPointer, BinaryLocal[CodePtrLocal + 4].VarPointer);
          A := GetVariable(BinaryLocal[CodePtrLocal + 1].VarPointer, BinaryLocal[CodePtrLocal + 2].VarPointer);
          if SEValueGreaterOrEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal[CodePtrLocal + 5].VarPointer)
          else
            Inc(CodePtrLocal, 6);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqualOrLesser2:{$endif}
        begin
        labelJumpEqualOrLesser2:
          B := GetVariable(BinaryLocal[CodePtrLocal + 3].VarPointer, BinaryLocal[CodePtrLocal + 4].VarPointer);
          A := GetVariable(BinaryLocal[CodePtrLocal + 1].VarPointer, BinaryLocal[CodePtrLocal + 2].VarPointer);
          if SEValueLesserOrEqual(A^, B^) then
            CodePtrLocal := Integer(BinaryLocal[CodePtrLocal + 5].VarPointer)
          else
            Inc(CodePtrLocal, 6);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqual1Rel:{$endif}
        begin
        labelJumpEqual1Rel:
          A := Pop;
          if SEValueEqual(A^, BinaryLocal[CodePtrLocal + 1]) then
            CodePtrLocal := CodePtrLocal + Integer(BinaryLocal[CodePtrLocal + 2].VarPointer)
          else
            Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpUnconditionalRel:{$endif}
        begin
        labelJumpUnconditionalRel:
          CodePtrLocal := CodePtrLocal + Integer(BinaryLocal[CodePtrLocal + 1].VarPointer);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallRef:{$endif}
        begin
        labelCallRef:
          A := Pop; // Ref or map
          DeepCount := 0;
          case A^.Kind of
            sevkFunction:
              begin
                // Do nothing
              end;
            sevkMap:
              begin
                DeepCount := Integer(BinaryLocal[CodePtrLocal + 3].VarPointer);
                if DeepCount = 0 then
                  raise Exception.Create('Not a function reference');
                Self.StackPtr := Self.StackPtr - DeepCount;
                C := Self.StackPtr;
                for I := 0 to DeepCount - 1 do
                begin
                  TV2 := A^;
                  TV := SEMapGet(A^, C^);
                  A := @TV;
                  Inc(C);
                end;
              end;
            else
              raise Exception.Create('Not a function reference');
          end;
          BinaryLocal[CodePtrLocal + 1] := Pointer(A^.VarFuncIndx);
          case A^.VarFuncKind of
            sefkScript:
              begin
                if DeepCount > 1 then
                  (Self.StackPtr - 1)^ := TV2;
                goto CallScript;
              end;
            sefkImport:
              begin
                Pop; // import has no this
                goto CallImport;
              end;
            sefkNative:
              begin
                if DeepCount > 1 then
                  (Self.StackPtr - 1)^ := TV2;
                This := Pop^;
                Dec(BinaryLocal[CodePtrLocal + 2].VarPointer); // ArgCount contains this, so we minus it by 1
                goto CallNative;
              end;
          end;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallNative:{$endif}
        begin
        labelCallNative:
        CallNative:
          GC.CheckForGCFast;
          FuncNativeInfo := Self.Parent.FuncNativeList.Ptr(Integer(BinaryLocal[CodePtrLocal + 1].VarPointer));
          ArgCount := Integer(BinaryLocal[CodePtrLocal + 2].VarPointer);
          Self.StackPtr := Self.StackPtr - ArgCount;
          if FuncNativeInfo^.Kind = sefnkNormal then
            TV := TSEFunc(FuncNativeInfo^.Func)(Self, Self.StackPtr, ArgCount)
          else
            TV := TSEFuncWithSelf(FuncNativeInfo^.Func)(Self, Self.StackPtr, ArgCount, This);
          if IsDone then
          begin
            Exit;
          end;
          Push(TV);
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallScript:{$endif}
        begin
        labelCallScript:
        CallScript:
          ArgCount := Integer(BinaryLocal[CodePtrLocal + 2].VarPointer);
          FuncScriptInfo := Self.Parent.FuncScriptList.Ptr(Integer(BinaryLocal[CodePtrLocal + 1].VarPointer));
          Inc(Self.FramePtr);
          if Self.FramePtr > @Self.Frame[Self.FrameSize - 1] then
            raise Exception.Create('Too much recursion');
          Self.FramePtr^.Stack := Self.StackPtr - ArgCount;
          Self.FramePtr^.Code := CodePtrLocal + 4;
          Self.FramePtr^.Binary := BinaryPtrLocal;
          Self.FramePtr^.Func := FuncScriptInfo;
          Self.StackPtr := Self.StackPtr + FuncScriptInfo^.VarCount;
          CodePtrLocal := 0;
          BinaryPtrLocal := FuncScriptInfo^.BinaryPos;
          BinaryLocal := Self.Binaries.Value^.Data[BinaryPtrLocal].Ptr(0);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPopFrame:{$endif}
        begin
        labelPopFrame:
          CodePtrLocal := Self.FramePtr^.Code;
          Self.StackPtr := Self.FramePtr^.Stack;
          BinaryPtrLocal := Self.FramePtr^.Binary;
          BinaryLocal := Self.Binaries.Value^.Data[BinaryPtrLocal].Ptr(0);
          Dec(Self.FramePtr);
          if Self.FramePtr < @Self.Frame[0] then
          begin
            Self.IsDone := True;
            Break;
          end;
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignGlobalVar:{$endif}
        begin
        labelAssignGlobalVar:
          AssignGlobal(BinaryLocal[CodePtrLocal + 1], Pop);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignLocalVar:{$endif}
        begin
        labelAssignLocalVar:
          AssignLocal(BinaryLocal[CodePtrLocal + 1], Integer(BinaryLocal[CodePtrLocal + 2].VarPointer), Pop);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignArrayFast:{$endif}
        begin
        labelAssignArrayFast:
          A := GetVariable(BinaryLocal[CodePtrLocal + 1].VarPointer, BinaryLocal[CodePtrLocal + 2].VarPointer);
          B := Pop;
          C := @BinaryLocal[CodePtrLocal + 3];
          if C^.Kind = sevkNull then
            C := Pop;
          TSEValueMap(A^.VarMap).Lock;
          TSEValueMap(A^.VarMap).Items[Trunc(C^.VarNumber)] := B^;
          TSEValueMap(A^.VarMap).Unlock;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignMapFast:{$endif}
        begin
        labelAssignMapFast:
          A := GetVariable(BinaryLocal[CodePtrLocal + 1].VarPointer, BinaryLocal[CodePtrLocal + 2].VarPointer);
          B := Pop;
          TSEValueMap(A^.VarMap).Lock;
          TSEValueMap(A^.VarMap).Map.AddOrSetValue(Self.ConstStrings[Integer(BinaryLocal[CodePtrLocal + 3].VarPointer)], B^);
          TSEValueMap(A^.VarMap).Unlock;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignGlobalArray:{$endif}
        begin
        labelAssignGlobalArray:
          A := @BinaryLocal[CodePtrLocal + 1];
          V := GetGlobalInt(Integer(A^));
          B := Pop;
          ArgCount := BinaryLocal[CodePtrLocal + 2];
          if ArgCount = 1 then
            C := Pop
          else
          begin
            Self.StackPtr := Self.StackPtr - ArgCount;
            C := Self.StackPtr;
            for I := 1 to ArgCount - 1 do
            begin
              OC := C;
              OV := V;
              TV := SEMapGet(V^, C^);
              V := @TV;
              Inc(C);
            end;
          end;
          case B^.Kind of
            sevkString:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S2 := B^.VarString^;
                    UTF8Delete(V^.VarString^, Integer(C^) + 1, 1);
                    S := UTF8Copy(S2, 1, 1);
                    UTF8Insert(S, V^.VarString^, Integer(C^) + 1);
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := B^.VarString^[1];
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignGlobalInt(Integer(A^), V);
                end;
              end;
            sevkNumber:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    UTF8Delete(V^.VarString^, Integer(C^) + 1, 1);
                    S := Char(Round(B^.VarNumber));
                    UTF8Insert(S, V^.VarString^, Integer(C^) + 1);
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := Char(Round(B^.VarNumber));
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignGlobalInt(Integer(A^), V);
                end;
              end;
            else
              begin
                SEMapSet(V^, C^, B^);
                if ArgCount = 1 then
                  AssignGlobalInt(Integer(A^), V);
              end;
          end;
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignLocalArray:{$endif}
        begin
        labelAssignLocalArray:
          A := @BinaryLocal[CodePtrLocal + 1];
          J := Integer(BinaryLocal[CodePtrLocal + 3].VarPointer);
          V := GetLocalInt(Integer(A^), J);
          B := Pop;
          ArgCount := BinaryLocal[CodePtrLocal + 2];
          if ArgCount = 1 then
            C := Pop
          else
          begin
            Self.StackPtr := Self.StackPtr - ArgCount;
            C := Self.StackPtr;
            for I := 1 to ArgCount - 1 do
            begin
              OC := C;
              OV := V;
              TV := SEMapGet(V^, C^);
              V := @TV;
              Inc(C);
            end;
          end;
          case B^.Kind of
            sevkString:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    S2 := B^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := UTF8Copy(S2, 1, 1);
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := B^.VarString^[1];
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignLocalInt(Integer(A^), J, V);
                end;
              end;
            sevkNumber:
              begin
                if V^.Kind = sevkString then
                begin
                  {$ifdef SE_STRING_UTF8}
                    S1 := V^.VarString^;
                    UTF8Delete(S1, Integer(C^) + 1, 1);
                    S := Char(Round(B^.VarNumber));
                    UTF8Insert(S, S1, Integer(C^) + 1);
                    V^.VarString^ := S1;
                  {$else}
                    V^.VarString^[Integer(C^) + 1] := Char(Round(B^.VarNumber));
                  {$endif}
                  // Self.Stack[A] := S;
                  if ArgCount >= 2 then
                    SEMapSet(OV^, OC^, V^);
                end else
                begin
                  SEMapSet(V^, C^, B^);
                  if ArgCount = 1 then
                    AssignLocalInt(Integer(A^), J, V);
                end;
              end;
            else
              begin
                SEMapSet(V^, C^, B^);
                if ArgCount = 1 then
                  AssignLocalInt(Integer(A^), J, V);
              end;
          end;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifdef UNIX}
      {$ifndef SE_COMPUTED_GOTO}opBlockCleanup:{$endif}
        begin
        labelBlockCleanup:
          Inc(CodePtrLocal);
          CheckForSuspend;
          DispatchGoto;
        end;
      {$endif}
      {$ifndef SE_COMPUTED_GOTO}opYield:{$endif}
        begin
        labelYield:
          Self.IsYielded := True;
          Inc(CodePtrLocal);
          Self.CodePtr := CodePtrLocal;
          Self.BinaryPtr := BinaryPtrLocal;
          Exit;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorPow:{$endif}
        begin
        labelOperatorPow:
          B := Pop;
          A := Pop;
          Push(Power(A^.VarNumber, B^.VarNumber));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opHlt:{$endif}
        begin
        labelHlt:
          Self.CodePtr := CodePtrLocal;
          Self.BinaryPtr := BinaryPtrLocal;
          Self.IsDone := True;
          Self.Parent.IsDone := True;
          Exit;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushTrap:{$endif}
        begin
        labelPushTrap:
          Inc(Self.TrapPtr);
          Self.TrapPtr^.FramePtr := Self.FramePtr;
          Self.TrapPtr^.Stack := Self.StackPtr;
          Self.TrapPtr^.Binary := BinaryPtrLocal;
          Self.TrapPtr^.CatchCode := Integer(BinaryLocal[CodePtrLocal + 1].VarPointer);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPopTrap:{$endif}
        begin
        labelPopTrap:
          Dec(Self.TrapPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opThrow:{$endif}
        begin
        labelThrow:
          IsScriptException := True;
          if Self.TrapPtr < @Self.Trap[0] then
            raise Exception.Create(SEValueToText(Pop^))
          else
          begin
            TV := Pop^;
            Self.FramePtr := Self.TrapPtr^.FramePtr;
            CodePtrLocal := Self.TrapPtr^.CatchCode;
            Self.StackPtr := Self.TrapPtr^.Stack;
            BinaryPtrLocal := Self.TrapPtr^.Binary;
            BinaryLocal := Self.Binaries.Value^.Data[BinaryPtrLocal].Ptr(0);
            Push(TV);
            Dec(Self.TrapPtr);
          end;
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallImport:{$endif}
        begin
        labelCallImport:
        CallImport:
          CallImportFunc;
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}
      end;
      if Self.IsPaused then
      begin
        Self.CodePtr := CodePtrLocal;
        Self.BinaryPtr := BinaryPtrLocal;
        Exit;
      end;
      {$endif}
    end;
    Break;
  except
    on E: Exception do
    begin
      S := #10 + DumpCallStack + #10;
      {$ifdef SE_COMPUTED_GOTO}
      if Self.TrapPtr < @Self.Trap[0] then
      {$endif}
      begin
        GetLineOfCode;
        if LineOfCode.Module = '' then
          S := S + Format('Runtime error %s: "%s" at line %d', [E.ClassName, E.Message, LineOfCode.Line])
        else
          S := S + Format('Runtime error %s: "%s" at line %d (%s)', [E.ClassName, E.Message, LineOfCode.Line, LineOfCode.Module]);
        PrintEvilScriptStackTrace(S);
        raise Exception.Create(S);
      {$ifdef SE_COMPUTED_GOTO}
      end else
      if not IsScriptException then
      begin
        GetLineOfCode;
        if LineOfCode.Module = '' then
          S := S + Format('Runtime error %s: "%s" at line %d', [E.ClassName, E.Message, LineOfCode.Line])
        else
          S := S + Format('Runtime error %s: "%s" at line %d (%s)', [E.ClassName, E.Message, LineOfCode.Line, LineOfCode.Module]);
        IsScriptException := False;
        Push(S);
        ArgCount := 1;
        FuncScriptInfo := Self.Parent.FuncScriptList.Ptr(1);
        Inc(Self.FramePtr);
        if Self.FramePtr > @Self.Frame[Self.FrameSize - 1] then
          raise Exception.Create('Too much recursion');
        Self.FramePtr^.Stack := Self.StackPtr - ArgCount;
        Self.FramePtr^.Code := CodePtrLocal;
        Self.FramePtr^.Binary := BinaryPtrLocal;
        Self.FramePtr^.Func := FuncScriptInfo;
        Self.StackPtr := Self.StackPtr + FuncScriptInfo^.VarCount;
        CodePtrLocal := 0;
        BinaryPtrLocal := FuncScriptInfo^.BinaryPos;
        BinaryLocal := Self.Binaries.Value^.Data[BinaryPtrLocal].Ptr(0);
        DispatchGoto;
      end else
      begin
        Self.FramePtr := Self.TrapPtr^.FramePtr;
        CodePtrLocal := Self.TrapPtr^.CatchCode;
        Self.StackPtr := Self.TrapPtr^.Stack;
        BinaryPtrLocal := Self.FramePtr^.Binary;
        BinaryLocal := Self.Binaries.Value^.Data[BinaryPtrLocal].Ptr(0);
        Push(E.Message);
        Dec(Self.TrapPtr);
        DispatchGoto;
        Break;
      {$endif}
      end;
    end;
  end;
  Self.CodePtr := CodePtrLocal;
  Self.BinaryPtr := BinaryPtrLocal;
end;

{$ifdef SE_THREADS}
constructor TSEVMThread.Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
var
  I: Integer;
begin
  Self.VM := AVM.Fork(AStackSize);
  Self.VM.ThreadOwner := Self;
  for I := 0 to ArgCount - 1 do
  begin
    Self.VM.StackPtr[0] := Args[I];
    Inc(Self.VM.StackPtr);
  end;
  Self.VM.StackPtr := Self.VM.StackPtr + Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].VarCount;
  Self.VM.BinaryPtr := Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].BinaryPos;

  inherited Create(True);
  Self.VM.Parent.VMThreadList.Add(Self);
  Self.FreeOnTerminate := False;
end;

procedure TSEVMThread.Execute;
begin
  Inc(IsThread);
  try
    try
      while not Self.VM.IsDone do
      begin
        Self.VM.Exec;
        {$ifdef Unix}
        if Self.VM.IsYielded then
          Self.Yield;
        if Self.VM.IsRequestForSuspend then
          Self.Suspend;
        Self.VM.IsRequestForSuspend := False;
        {$endif}
      end;
    except
      on E: Exception do
        Writeln('[TSEVMThread] ', E.Message);
    end;
  finally
    Self.VM.Parent.VMThreadList.Remove(Self);
    Self.Terminate;
    Self.IsDone := True;
  end;
end;

destructor TSEVMThread.Destroy;
begin
  Self.VM.Free;
  inherited;
end;
{$endif}

constructor TSEVMCoroutine.Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
var
  I: Integer;
begin
  inherited Create;
  Self.VM := AVM.Fork(AStackSize);
  Self.VM.CoroutineOwner := Self;
  for I := 0 to ArgCount - 1 do
  begin
    Self.VM.StackPtr[0] := Args[I];
    Inc(Self.VM.StackPtr);
  end;
  Self.VM.StackPtr := Self.VM.StackPtr + Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].VarCount;
  Self.VM.BinaryPtr := Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].BinaryPos;
  Self.FStackPtr := Self.VM.StackPtr;
  Self.FBinaryPtr := Self.VM.BinaryPtr;
end;

function TSEVMCoroutine.Execute: TSEValue;
begin
  if not Self.IsTerminated then
  begin
    try
      Self.IsExecuting := True;
      Self.VM.Exec;
      Self.IsExecuting := False;
      Result := (PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED - 1)^;
      if Self.VM.IsDone then
      begin
        Self.IsTerminated := True;
        Self.IsDone := True;
      end;
    except
      on E: Exception do
        Writeln('[TSEVMCoroutine] ', E.Message);
    end;
  end;
end;

procedure TSEVMCoroutine.Reset(const Fn: TSEValue; const Args: PSEValue; const ArgCount: Cardinal);
var
  I: Integer;
begin
  Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
  for I := 0 to ArgCount - 1 do
  begin
    Self.VM.StackPtr[0] := Args[I];
    Inc(Self.VM.StackPtr);
  end;
  Self.VM.BinaryPtr := Self.FBinaryPtr;
  Self.VM.StackPtr := Self.FStackPtr;
  Self.IsTerminated := False;
  Self.IsDone := False;
  Inc(Self.VM.FramePtr);
  Self.VM.IsDone := False;
end;

destructor TSEVMCoroutine.Destroy;
begin
  Self.VM.Free;
  inherited;
end;

constructor TEvilC.Create(const StackSize: LongWord = 2048);
begin
  inherited Create;
  Self.VM := TSEVM.Create;
  Self.VM.StackSize := StackSize;
  {$ifdef SE_THREADS}
  Self.VMThreadList := TSEVMThreadList.Create;
  {$endif}
  Self.GlobalVarSymbols := TStringList.Create;
  Self.TokenList := TSETokenList.Create;
  Self.OpcodeInfoList := TSEOpcodeInfoList.Create;
  Self.VarList := TSEIdentList.Create;
  Self.FuncNativeList := TSEFuncNativeList.Create;
  Self.FuncScriptList := TSEFuncScriptList.Create;
  Self.FuncImportList := TSEFuncImportList.Create;
  Self.ConstMap := TSEConstMap.Create;
  Self.ScopeStack := TSEScopeStack.Create;
  Self.ScopeFunc := TSEScopeStack.Create;
  Self.LineOfCodeList := TSELineOfCodeList.Create;
  Self.IncludeList := TStringList.Create;
  Self.IncludePathList := TStringList.Create;
  Self.CurrentFileList := TStringList.Create;
  Self.LocalVarCountList := TSEIntegerList.Create;
  //
  Self.OptimizeAsserts := True;
  Self.OptimizeConstantFolding := True;
  Self.OptimizePeephole := True;
  //
  Self.TokenList.Capacity := 1024;
  Self.VarList.Capacity := 256;
  Self.FuncNativeList.Capacity := 64;
  Self.FuncScriptList.Capacity := 64;
  Self.FuncImportList.Capacity := 64;
  Self.ScopeStack.Capacity := 16;
  Self.LineOfCodeList.Capacity := 1024;
  //
  Self.VM.Parent := Self;
  if CommonNativeFuncList.Count = 0 then
  begin
    Self.RegisterFunc('buffer_create', @TBuiltInFunction(nil).SEBufferCreate, 1);
    Self.RegisterFunc('buffer_length', @TBuiltInFunction(nil).SEBufferLength, 1);
    Self.RegisterFunc('buffer_copy', @TBuiltInFunction(nil).SEBufferCopy, 3);
    Self.RegisterFunc('buffer_u8_fill', @TBuiltInFunction(nil).SEBufferFillU8, 3);
    Self.RegisterFunc('buffer_u16_fill', @TBuiltInFunction(nil).SEBufferFillU16, 3);
    Self.RegisterFunc('buffer_u32_fill', @TBuiltInFunction(nil).SEBufferFillU32, 3);
    Self.RegisterFunc('buffer_u64_fill', @TBuiltInFunction(nil).SEBufferFillU64, 3);
    Self.RegisterFunc('buffer_i8_fill', @TBuiltInFunction(nil).SEBufferFillI8, 3);
    Self.RegisterFunc('buffer_i16_fill', @TBuiltInFunction(nil).SEBufferFillI16, 3);
    Self.RegisterFunc('buffer_i32_fill', @TBuiltInFunction(nil).SEBufferFillI32, 3);
    Self.RegisterFunc('buffer_i64_fill', @TBuiltInFunction(nil).SEBufferFillI64, 3);
    Self.RegisterFunc('buffer_f32_fill', @TBuiltInFunction(nil).SEBufferFillF32, 3);
    Self.RegisterFunc('buffer_f64_fill', @TBuiltInFunction(nil).SEBufferFillF64, 3);
    Self.RegisterFunc('buffer_u8_get', @TBuiltInFunction(nil).SEBufferGetU8, 1);
    Self.RegisterFunc('buffer_u16_get', @TBuiltInFunction(nil).SEBufferGetU16, 1);
    Self.RegisterFunc('buffer_u32_get', @TBuiltInFunction(nil).SEBufferGetU32, 1);
    Self.RegisterFunc('buffer_u64_get', @TBuiltInFunction(nil).SEBufferGetU64, 1);
    Self.RegisterFunc('buffer_i8_get', @TBuiltInFunction(nil).SEBufferGetI8, 1);
    Self.RegisterFunc('buffer_i16_get', @TBuiltInFunction(nil).SEBufferGetI16, 1);
    Self.RegisterFunc('buffer_i32_get', @TBuiltInFunction(nil).SEBufferGetI32, 1);
    Self.RegisterFunc('buffer_i64_get', @TBuiltInFunction(nil).SEBufferGetI64, 1);
    Self.RegisterFunc('buffer_f32_get', @TBuiltInFunction(nil).SEBufferGetF32, 1);
    Self.RegisterFunc('buffer_f64_get', @TBuiltInFunction(nil).SEBufferGetF64, 1);
    Self.RegisterFunc('buffer_u8_set', @TBuiltInFunction(nil).SEBufferSetU8, 2);
    Self.RegisterFunc('buffer_u16_set', @TBuiltInFunction(nil).SEBufferSetU16, 2);
    Self.RegisterFunc('buffer_u32_set', @TBuiltInFunction(nil).SEBufferSetU32, 2);
    Self.RegisterFunc('buffer_u64_set', @TBuiltInFunction(nil).SEBufferSetU64, 2);
    Self.RegisterFunc('buffer_i8_set', @TBuiltInFunction(nil).SEBufferSetI8, 2);
    Self.RegisterFunc('buffer_i16_set', @TBuiltInFunction(nil).SEBufferSetI16, 2);
    Self.RegisterFunc('buffer_i32_set', @TBuiltInFunction(nil).SEBufferSetI32, 2);
    Self.RegisterFunc('buffer_i64_set', @TBuiltInFunction(nil).SEBufferSetI64, 2);
    Self.RegisterFunc('buffer_f32_set', @TBuiltInFunction(nil).SEBufferSetF32, 2);
    Self.RegisterFunc('buffer_f64_set', @TBuiltInFunction(nil).SEBufferSetF64, 2);
    Self.RegisterFunc('string_to_buffer', @TBuiltInFunction(nil).SEStringToBuffer, 1);
    Self.RegisterFunc('buffer_to_string', @TBuiltInFunction(nil).SEBufferToString, 1);
    Self.RegisterFunc('wbuffer_to_string', @TBuiltInFunction(nil).SEWBufferToString, 1);
    Self.RegisterFunc('array_to_buffer_f32', @TBuiltInFunction(nil).SEArrayToBufferF32, 1);
    Self.RegisterFunc('array_to_buffer_f64', @TBuiltInFunction(nil).SEArrayToBufferF64, 1);
    Self.RegisterFunc('buffer_to_array_f32', @TBuiltInFunction(nil).SEBufferToArrayF32, 2);
    Self.RegisterFunc('buffer_to_array_f64', @TBuiltInFunction(nil).SEBufferToArrayF64, 2);
    Self.RegisterFunc('typeof', @TBuiltInFunction(nil).SETypeOf, 1);
    Self.RegisterFunc('kindof', @TBuiltInFunction(nil).SEKindOf, 1);
    Self.RegisterFunc('get', @TBuiltInFunction(nil).SEGet, 1);
    Self.RegisterFunc('set', @TBuiltInFunction(nil).SESet, 2);
    Self.RegisterFunc('string', @TBuiltInFunction(nil).SEString, 1);
    Self.RegisterFunc('number', @TBuiltInFunction(nil).SENumber, 1);
    Self.RegisterFunc('length', @TBuiltInFunction(nil).SELength, 1);
    Self.RegisterFunc('map_create', @TBuiltInFunction(nil).SEMapCreate, -1);
    Self.RegisterFunc('___map_create', @TBuiltInFunction(nil).SEMapCreate, -1);
    Self.RegisterFunc('map_clone', @TBuiltInFunction(nil).SEMapClone, 1);
    Self.RegisterFunc('map_key_delete', @TBuiltInFunction(nil).SEMapKeyDelete, 2);
    Self.RegisterFunc('map_keys_get', @TBuiltInFunction(nil).SEMapKeysGet, 1);
    Self.RegisterFunc('map_clear', @TBuiltInFunction(nil).SEMapClear, 1);
    Self.RegisterFunc('array_resize', @TBuiltInFunction(nil).SEArrayResize, 2);
    Self.RegisterFunc('array_to_map', @TBuiltInFunction(nil).SEArrayToMap, 1);
    Self.RegisterFunc('array_fill', @TBuiltInFunction(nil).SEArrayFill, 2);
    Self.RegisterFunc('array_delete', @TBuiltInFunction(nil).SEMapKeyDelete, 2);
    Self.RegisterFunc('array_clear', @TBuiltInFunction(nil).SEMapClear, 1);
    Self.RegisterFunc('sign', @TBuiltInFunction(nil).SESign, 1);
    Self.RegisterFunc('min', @TBuiltInFunction(nil).SEMin, -1);
    Self.RegisterFunc('max', @TBuiltInFunction(nil).SEMax, -1);
    Self.RegisterFunc('range', @TBuiltInFunction(nil).SERange, -1);
    Self.RegisterFunc('pow', @TBuiltInFunction(nil).SEPow, 2);
    Self.RegisterFunc('sleep', @TBuiltInFunction(nil).SESleep, 1);
    Self.RegisterFunc('string_grep', @TBuiltInFunction(nil).SEStringGrep, 2);
    Self.RegisterFunc('string_format', @TBuiltInFunction(nil).SEStringFormat, 2);
    Self.RegisterFunc('string_split', @TBuiltInFunction(nil).SEStringSplit, 2);
    Self.RegisterFunc('string_find', @TBuiltInFunction(nil).SEStringFind, 2);
    Self.RegisterFunc('string_delete', @TBuiltInFunction(nil).SEStringDelete, 3);
    Self.RegisterFunc('string_insert', @TBuiltInFunction(nil).SEStringInsert, 3);
    Self.RegisterFunc('string_replace', @TBuiltInFunction(nil).SEStringReplace, 3);
    Self.RegisterFunc('string_replace_ignorecase', @TBuiltInFunction(nil).SEStringReplaceIgnoreCase, 3);
    Self.RegisterFunc('string_uppercase', @TBuiltInFunction(nil).SEStringUpperCase, 1);
    Self.RegisterFunc('string_lowercase', @TBuiltInFunction(nil).SEStringLowerCase, 1);
    Self.RegisterFunc('string_find_regex', @TBuiltInFunction(nil).SEStringFindRegex, 2);
    Self.RegisterFunc('string_compare', @TBuiltInFunction(nil).SEStringCompare, 2);
    Self.RegisterFunc('string_trim', @TBuiltInFunction(nil).SEStringTrim, 1);
    Self.RegisterFunc('string_trim_left', @TBuiltInFunction(nil).SEStringTrimLeft, 1);
    Self.RegisterFunc('string_trim_right', @TBuiltInFunction(nil).SEStringTrimRight, 1);
    Self.RegisterFunc('string_extract_name', @TBuiltInFunction(nil).SEStringExtractName, 1);
    Self.RegisterFunc('string_extract_path', @TBuiltInFunction(nil).SEStringExtractPath, 1);
    Self.RegisterFunc('string_extract_ext', @TBuiltInFunction(nil).SEStringExtractExt, 1);
    Self.RegisterFunc('lerp', @TBuiltInFunction(nil).SELerp, 3);
    Self.RegisterFunc('slerp', @TBuiltInFunction(nil).SESLerp, 3);
    Self.RegisterFunc('write', @TBuiltInFunction(nil).SEWrite, -1);
    Self.RegisterFunc('writeln', @TBuiltInFunction(nil).SEWriteln, -1);
    Self.RegisterFunc('ticks', @TBuiltInFunction(nil).SEGetTickCount, 0);
    Self.RegisterFunc('dt_now', @TBuiltInFunction(nil).SEDTNow, 0);
    Self.RegisterFunc('dt_year_get', @TBuiltInFunction(nil).SEDTGetYear, 1);
    Self.RegisterFunc('dt_month_get', @TBuiltInFunction(nil).SEDTGetMonth, 1);
    Self.RegisterFunc('dt_day_get', @TBuiltInFunction(nil).SEDTGetDay, 1);
    Self.RegisterFunc('dt_hour_get', @TBuiltInFunction(nil).SEDTGetHour, 1);
    Self.RegisterFunc('dt_minute_get', @TBuiltInFunction(nil).SEDTGetMinute, 1);
    Self.RegisterFunc('dt_date_set', @TBuiltInFunction(nil).SEDTSetDate, 3);
    Self.RegisterFunc('dt_time_set', @TBuiltInFunction(nil).SEDTSetTime, 4);
    Self.RegisterFunc('dt_day_add', @TBuiltInFunction(nil).SEDTDayAdd, 2);
    Self.RegisterFunc('dt_month_add', @TBuiltInFunction(nil).SEDTMonthAdd, 2);
    Self.RegisterFunc('dt_year_add', @TBuiltInFunction(nil).SEDTYearAdd, 2);
    Self.RegisterFunc('random', @TBuiltInFunction(nil).SERandom, 1);
    Self.RegisterFunc('rnd', @TBuiltInFunction(nil).SERnd, 0);
    Self.RegisterFunc('round', @TBuiltInFunction(nil).SERound, 1);
    Self.RegisterFunc('floor', @TBuiltInFunction(nil).SEFloor, 1);
    Self.RegisterFunc('ceil', @TBuiltInFunction(nil).SECeil, 1);
    Self.RegisterFunc('trunc', @TBuiltInFunction(nil).SETrunc, 1);
    Self.RegisterFunc('sin', @TBuiltInFunction(nil).SESin, 1);
    Self.RegisterFunc('cos', @TBuiltInFunction(nil).SECos, 1);
    Self.RegisterFunc('tan', @TBuiltInFunction(nil).SETan, 1);
    Self.RegisterFunc('cot', @TBuiltInFunction(nil).SECot, 1);
    Self.RegisterFunc('sqrt', @TBuiltInFunction(nil).SESqrt, 1);
    Self.RegisterFunc('abs', @TBuiltInFunction(nil).SEAbs, 1);
    Self.RegisterFunc('frac', @TBuiltInFunction(nil).SEFrac, 1);
    Self.RegisterFunc('mem_object_old_count', @TBuiltInFunction(nil).SEGCObjectOldCount, 0);
    Self.RegisterFunc('mem_object_count', @TBuiltInFunction(nil).SEGCObjectCount, 0);
    Self.RegisterFunc('mem_gc', @TBuiltInFunction(nil).SEGCCollect, 0);
    Self.RegisterFunc('fs_file_delete', @TBuiltInFunction(nil).SEFileDelete, 1);
    Self.RegisterFunc('fs_file_rename', @TBuiltInFunction(nil).SEFileRename, 2);
    Self.RegisterFunc('fs_file_exists', @TBuiltInFunction(nil).SEFileExists, 1);
    Self.RegisterFunc('fs_file_read', @TBuiltInFunction(nil).SEFileReadText, 1);
    Self.RegisterFunc('fs_file_read_text', @TBuiltInFunction(nil).SEFileReadText, 1);
    Self.RegisterFunc('fs_file_read_binary', @TBuiltInFunction(nil).SEFileReadBinary, -1);
    Self.RegisterFunc('fs_file_write', @TBuiltInFunction(nil).SEFileWriteText, 2);
    Self.RegisterFunc('fs_file_write_text', @TBuiltInFunction(nil).SEFileWriteText, 2);
    Self.RegisterFunc('fs_file_write_binary', @TBuiltInFunction(nil).SEFileWriteBinary, 3);
    Self.RegisterFunc('fs_file_copy', @TBuiltInFunction(nil).SEFileCopy, 2);
    Self.RegisterFunc('fs_file_size_get', @TBuiltInFunction(nil).SEFileGetSize, 1);
    Self.RegisterFunc('fs_file_age_get', @TBuiltInFunction(nil).SEFileGetAge, 1);
    Self.RegisterFunc('fs_file_find_all', @TBuiltInFunction(nil).SEFileFindAll, 4);
    Self.RegisterFunc('fs_directory_create', @TBuiltInFunction(nil).SEDirectoryCreate, 1);
    Self.RegisterFunc('fs_directory_delete', @TBuiltInFunction(nil).SEDirectoryDelete, 1);
    Self.RegisterFunc('fs_directory_find_all', @TBuiltInFunction(nil).SEDirectoryFindAll, 2);
    Self.RegisterFunc('fs_directory_exists', @TBuiltInFunction(nil).SEDirectoryExists, 1);
    Self.RegisterFunc('base64_encode', @TBuiltInFunction(nil).SEBase64Encode, 1);
    Self.RegisterFunc('base64_decode', @TBuiltInFunction(nil).SEBase64Decode, 1);
    {$ifdef SE_HAS_JSON}
    Self.RegisterFunc('json_parse', @TBuiltInFunction(nil).SEJSONParse, 1);
    Self.RegisterFunc('json_stringify', @TBuiltInFunction(nil).SEJSONStringify, 1);
    {$endif}
    Self.RegisterFunc('pasobject_classname', @TBuiltInFunction(nil).SEPasObjectClassName, 1);
    Self.RegisterFunc('chr', @TBuiltInFunction(nil).SEChar, 1);
    Self.RegisterFunc('ord', @TBuiltInFunction(nil).SEOrd, 1);

    Self.RegisterFunc('coroutine_create', @TBuiltInFunction(nil).SECoroutineCreate, -1);
    Self.RegisterFunc('coroutine_reset', @TBuiltInFunction(nil).SECoroutineReset, -1);
    Self.RegisterFunc('coroutine_start', @TBuiltInFunction(nil).SECoroutineResume, 1);
    Self.RegisterFunc('coroutine_resume', @TBuiltInFunction(nil).SECoroutineResume, 1);
    Self.RegisterFunc('coroutine_is_terminated', @TBuiltInFunction(nil).SECoroutineIsTerminated, 1);
    Self.RegisterFunc('coroutine_terminate', @TBuiltInFunction(nil).SECoroutineTerminate, 1);
    Self.RegisterFunc('coroutine_is_running', @TBuiltInFunction(nil).SECoroutineIsExecuting, 1);
    {$ifdef SE_THREADS}
    Self.RegisterFunc('thread_create', @TBuiltInFunction(nil).SEThreadCreate, -1);
    Self.RegisterFunc('thread_start', @TBuiltInFunction(nil).SEThreadStart, 1);
    Self.RegisterFunc('thread_is_terminated', @TBuiltInFunction(nil).SEThreadIsTerminated, 1);
    Self.RegisterFunc('thread_suspend', @TBuiltInFunction(nil).SEThreadSuspend, 1);
    Self.RegisterFunc('thread_resume', @TBuiltInFunction(nil).SEThreadStart, 1);
    Self.RegisterFunc('thread_terminate', @TBuiltInFunction(nil).SEThreadTerminate, 1);
    Self.RegisterFunc('thread_wait', @TBuiltInFunction(nil).SEThreadWait, 1);
    Self.RegisterFunc('critical_create', @TBuiltInFunction(nil).SECriticalCreate, 0);
    Self.RegisterFunc('critical_enter', @TBuiltInFunction(nil).SECriticalEnter, 1);
    Self.RegisterFunc('critical_leave', @TBuiltInFunction(nil).SECriticalLeave, 1);
    Self.RegisterFunc('critical_try', @TBuiltInFunction(nil).SECriticalTry, 1);
    Self.RegisterFunc('event_create', @TBuiltInFunction(nil).SEEventCreate, 0);
    Self.RegisterFunc('event_set', @TBuiltInFunction(nil).SEEventSet, 1);
    Self.RegisterFunc('event_wait', @TBuiltInFunction(nil).SEEventWait, 2);
    Self.RegisterFunc('event_reset', @TBuiltInFunction(nil).SEEventReset, 1);
    {$endif}
    CommonNativeFuncList.AddRange(Self.FuncNativeList);
  end else
    Self.FuncNativeList.AddRange(CommonNativeFuncList);
  Self.AddDefaultConsts;
  Self.Source := '';
end;

destructor TEvilC.Destroy;
var
  I: Integer;
begin
  for I := 0 to Self.FuncScriptList.Count - 1 do
    Self.FuncScriptList[I].VarSymbols.Free;
  {$ifdef SE_THREADS}
  for I := Self.VMThreadList.Count - 1 downto 0 do
    Self.VMThreadList[I].Terminate;
  FreeAndNil(Self.VMThreadList);
  {$endif}
  FreeAndNil(Self.VM);
  FreeAndNil(Self.TokenList);
  FreeAndNil(Self.OpcodeInfoList);
  FreeAndNil(Self.VarList);
  FreeAndNil(Self.FuncNativeList);
  FreeAndNil(Self.FuncScriptList);
  FreeAndNil(Self.FuncImportList);
  FreeAndNil(Self.ConstMap);
  FreeAndNil(Self.ScopeStack);
  FreeAndNil(Self.ScopeFunc);
  FreeAndNil(Self.LineOfCodeList);
  FreeAndNil(Self.IncludeList);
  FreeAndNil(Self.IncludePathList);
  FreeAndNil(Self.CurrentFileList);
  FreeAndNil(Self.LocalVarCountList);
  FreeAndNil(Self.GlobalVarSymbols);
  inherited;
end;

procedure TEvilC.AddDefaultConsts;
begin
  Self.ConstMap.AddOrSetValue('PI', PI);
  Self.ConstMap.AddOrSetValue('true', True);
  Self.ConstMap.AddOrSetValue('false', False);
  Self.ConstMap.AddOrSetValue('null', SENull);
  Self.ConstMap.AddOrSetValue('os', GetOS);
  Self.ConstMap.AddOrSetValue('sevkNumber', TSENumber(Integer(sevkNumber)));
  Self.ConstMap.AddOrSetValue('sevkString', TSENumber(Integer(sevkString)));
  Self.ConstMap.AddOrSetValue('sevkPascalObject', TSENumber(Integer(sevkPascalObject)));
  Self.ConstMap.AddOrSetValue('sevkBuffer', TSENumber(Integer(sevkBuffer)));
  Self.ConstMap.AddOrSetValue('sevkMap', TSENumber(Integer(sevkMap)));
  Self.ConstMap.AddOrSetValue('sevkNull', TSENumber(Integer(sevkNull)));
  Self.ConstMap.AddOrSetValue('sevkFunction', TSENumber(Integer(sevkFunction)));
  Self.ConstMap.AddOrSetValue('sevkPointer', TSENumber(Integer(sevkPointer)));
  {$ifdef SE_THREADS}
  Self.ConstMap.AddOrSetValue('wrSignaled', TSENumber(Integer(wrSignaled)));
  Self.ConstMap.AddOrSetValue('wrTimeout', TSENumber(Integer(wrTimeout)));
  Self.ConstMap.AddOrSetValue('wrAbandoned', TSENumber(Integer(wrAbandoned)));
  Self.ConstMap.AddOrSetValue('wrError', TSENumber(Integer(wrError)));
  {$endif}
end;

procedure TEvilC.SetSource(V: String);
begin
  Self.Reset;
  Self.FSource := V;
end;

function TEvilC.InternalIdent: String; inline;
begin
  Inc(Self.FInternalIdentCount);
  Result := IntToStr(FInternalIdentCount);
end;

function TEvilC.GetIsPaused: Boolean;
begin
  Exit(Self.VM.IsPaused);
end;

procedure TEvilC.SetIsPaused(V: Boolean);
begin
  Self.VM.IsPaused := V;
end;

function TEvilC.IsYielded: Boolean;
begin
  Exit(Self.VM.IsYielded);
end;

procedure TEvilC.Lex(const IsIncluded: Boolean = False);
var
  Ln, Col: Integer;
  Pos: Integer = 0;
  Token: TSEToken;
  C, PC, NC: Char;
  IsScientificNotation: Boolean;

  function PeekAtNextChar: Char; inline;
  var
    P: Integer;
  begin
    P := Pos + 1;
    if P > Length(Self.Source) then
      Exit(#0);
    Exit(Self.Source[P]);
  end;

  function NextChar: Char; inline;
  begin
    Inc(Pos);
    Inc(Col);
    if Pos > Length(Self.Source) then
      Exit(#0);
    if Self.Source[Pos] = #10 then
    begin
      Inc(Ln);
      Col := 1;
    end;
    Exit(Self.Source[Pos]);
  end;

  procedure Error(const S: String; const N: String = '');
  begin
    ErrorLn := Ln;
    ErrorCol := Col;
    if N = '' then
      raise Exception.CreateFmt('[%d:%d] %s', [Ln, Col, S])
    else
      raise Exception.CreateFmt('[%s:%d:%d] %s', [N, Ln, Col, S]);
  end;

  procedure FindFiles(const Path: String; out Files: TStringDynArray);
  var
    Info: TSearchRec;
  begin
    if FindFirst (Path, faAnyFile, Info)=0 then
    begin
      repeat
        if (Info.Attr and faDirectory) <> faDirectory then
        begin
          SetLength(Files, Length(Files) + 1);
          Files[Length(Files) - 1] := Info.Name;
        end;
      until FindNext(info) <> 0;
      FindClose(Info);
    end;
  end;

var
  IsLoopDone: Boolean;
  PrevQuote: Char;
  BackupSource: String;
  IsPathFound: Boolean;
  S,
  Path: String;
  Paths: TStringDynArray;
  IsString: Boolean = False;

label
  IsStringLabel, EndLabel;

begin
  Ln := 1;
  Col := 1;
  ErrorLn := -1;
  ErrorCol := -1;
  Self.LineOfCodeList.Clear;
  repeat
    Token.Value := '';
    repeat
      C := NextChar;
    until (not (C in [#1..#32])) and (C <> ';');
    Token.Ln := Ln;
    Token.Col := Col;
    if Self.CurrentFileList.Count > 0 then
      Token.BelongedFileName := Self.CurrentFileList[Self.CurrentFileList.Count - 1]
    else
      Token.BelongedFileName := '';
    case C of
      #0:
        if not IsIncluded then
          Token.Kind := tkEOF
        else
          continue;
      '@':
        Token.Kind := tkAccess;
      '.':
        Token.Kind := tkDot;
      '&':
        begin
          if PeekAtNextChar = '&' then
          begin
            NextChar;
          end;
          Token.Kind := tkAnd;
        end;
      '|':
        begin
          if PeekAtNextChar = '|' then
          begin
            NextChar;
          end;
          Token.Kind := tkOr;
        end;
      '~':
        begin
          if PeekAtNextChar = '~' then
          begin
            NextChar;
          end;
          Token.Kind := tkXor;
        end;
      '!':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkNotEqual;
          end else
          if Pos > 1 then
          begin
            PC := Self.Source[Pos - 1];
            NC := PeekAtNextChar;
            if ((PC = ' ') or (PC = '(') or (PC = '=') or (PC = ',')) and (NC <> ' ') then
              Token.Kind := tkNot;
          end;
        end;
      ',':
        Token.Kind := tkComma;
      '(':
        Token.Kind := tkBracketOpen;
      ')':
        Token.Kind := tkBracketClose;
      '[':
        Token.Kind := tkSquareBracketOpen;
      ']':
        Token.Kind := tkSquareBracketClose;
      '{':
        Token.Kind := tkBegin;
      '}':
        begin
          if IsString then
            goto IsStringLabel;
          Token.Kind := tkEnd;
        end;
      ':':
        Token.Kind := tkColon;
      '?':
        Token.Kind := tkQuestion;
      '''', '"':
        begin
          PrevQuote := C;
          Token.Kind := tkString;
          repeat
            IsLoopDone := False;
            C := NextChar;
            case C of
              #0:
                Error('Unterminated string literal', Token.BelongedFileName);
              '$':
                begin
                  if PeekAtNextChar = '{' then
                  begin
                    NextChar;
                    TokenList.Add(Token);
                    // Add a plus sign
                    Token.Value := '';
                    Token.Kind := tkAdd;
                    TokenList.Add(Token);
                    // Add string function
                    Token.Value := 'string';
                    Token.Kind := tkIdent;
                    TokenList.Add(Token);
                    Token.Value := '';
                    Token.Kind := tkBracketOpen;
                    TokenList.Add(Token);
                    //
                    IsString := True;
                    goto EndLabel;
                    //
                  IsStringLabel:
                    IsString := False;
                    Token.Value := '';
                    Token.Kind := tkBracketClose;
                    TokenList.Add(Token);
                    // Add a plus sign
                    Token.Kind := tkAdd;
                    TokenList.Add(Token);
                    Token.Kind := tkString;
                  end else
                    Token.Value := Token.Value + C;
                end;
              '\':
                begin
                  C := PeekAtNextChar;
                  if C = 'n' then
                  begin
                    NextChar;
                    Token.Value := Token.Value + #10;
                  end else
                  if C = 'r' then
                  begin
                    NextChar;
                    Token.Value := Token.Value + #13;
                  end else
                  if C = 't' then
                  begin
                    NextChar;
                    Token.Value := Token.Value + #9;
                  end else
                  if (C = 'x') or (C = 'u') then
                  begin
                    NextChar;
                    if not (PeekAtNextChar in ['0'..'9', 'A'..'F', 'a'..'f']) then
                      Error('Invalid number');
                    S := '';
                    while PeekAtNextChar in ['0'..'9', 'A'..'F', 'a'..'f'] do
                    begin
                      S := S + NextChar;
                    end;
                    Token.Value := Token.Value + UTF8Encode(UnicodeChar(Hex2Dec64(S)));
                  end else
                  if C <> #0 then
                  begin
                    Token.Value := Token.Value + NextChar;
                  end;
                end;
              else
                begin
                  if C = PrevQuote then
                    IsLoopDone := True
                  else
                    Token.Value := Token.Value + C;
                end;
            end;
          until IsLoopDone;
        end;
      '+':
        begin
          Token.Kind := tkAdd;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '^':
        begin
          Token.Kind := tkPow;
        end;
      '-':
        begin
          Token.Kind := tkSub;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end else
          if Pos > 1 then
          begin
            PC := Self.Source[Pos - 1];
            NC := PeekAtNextChar;
            if ((PC = ' ') or (PC = '(') or (PC = '=') or (PC = ',') or (PC = '[') or
                (PC = '+') or (PC = '*') or (PC = '/') or (PC = '^') or (PC = '&') or
                (PC = '|') or (PC = '~') or (PC = '!'))
              and (NC <> ' ') then
              Token.Kind := tkNegative;
          end;
        end;
      '*':
        begin
          Token.Kind := tkMul;
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '/':
        begin
          Token.Kind := tkDiv;
          if PeekAtNextChar = '/' then
          begin
            repeat
              NextChar;
            until (PeekAtNextChar = #10) or (PeekAtNextChar = #0);
            continue;
          end else
          if PeekAtNextChar = '*' then
          begin
            repeat
              C := NextChar;
            until ((C = '*') and (PeekAtNextChar = '/')) or (C = #0);
            NextChar;
            continue;
          end else
          if PeekAtNextChar = '=' then
          begin
            Token.Kind := tkOpAssign;
            Token.Value := C;
            NextChar;
          end;
        end;
      '=':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
          end;
          Token.Kind := tkEqual;
        end;
      '<':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkSmallerOrEqual;
          end else
          if PeekAtNextChar = '<' then
          begin
            NextChar;
            Token.Kind := tkShiftLeft;
          end else
          if PeekAtNextChar = '>' then
          begin
            NextChar;
            Token.Kind := tkNotEqual;
          end else
            Token.Kind := tkSmaller;
        end;
      '>':
        begin
          if PeekAtNextChar = '=' then
          begin
            NextChar;
            Token.Kind := tkGreaterOrEqual;
          end else
          if PeekAtNextChar = '>' then
          begin
            NextChar;
            Token.Kind := tkShiftRight;
          end else
            Token.Kind := tkGreater;
        end;
      '%':
        Token.Kind := tkMod;
      '0'..'9':
        begin
          IsScientificNotation := False;
          Token.Kind := tkNumber;
          if (C = '0') and (LowerCase(PeekAtNextChar) = 'x') then
          begin
            NextChar;
            while PeekAtNextChar in ['0'..'9', 'A'..'F', 'a'..'f'] do
            begin
              C := NextChar;
              Token.Value := Token.Value + C;
            end;
            Token.Value := IntToStr(Hex2Dec64(Token.Value));
          end else
          begin
            Token.Value := C;
            while PeekAtNextChar in ['0'..'9', '.', 'e', 'E'] do
            begin
              C := NextChar;
              Token.Value := Token.Value + C;
              if (C = '.') and not (PeekAtNextChar in ['0'..'9']) then
                Error('Invalid number');
              if (C in ['e', 'E']) then
              begin
                if IsScientificNotation then
                  Error('Invalid number');
                IsScientificNotation := True;
                if PeekAtNextChar = '-' then
                  Token.Value := Token.Value + NextChar;
              end;
            end;
          end;
        end;
      '#':
        begin
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;
          case Token.Value of
            'require':
              begin
                Token.Value := '';
                C := PeekAtNextChar;
                while C = ' ' do
                begin
                  NextChar;
                  C := PeekAtNextChar;
                end;
                C := PeekAtNextChar;
                while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
                begin
                  Token.Value := Token.Value + NextChar;
                  C := PeekAtNextChar;
                end;
                if Token.Value <> GetOS then
                begin
                  C := #0;
                  goto EndLabel;
                end else
                  Continue;
              end
            else
              Error('Unhandled directive ' + C);
          end;
        end;
      'A'..'Z', 'a'..'z', '_':
        begin
          Token.Value := C;
          C := PeekAtNextChar;
          while C in ['0'..'9', 'A'..'Z', 'a'..'z', '_'] do
          begin
            Token.Value := Token.Value + NextChar;
            C := PeekAtNextChar;
          end;
          case Token.Value of
            'using':
              begin
                C := PeekAtNextChar;
                while C = ' ' do
                begin
                  NextChar;
                  C := PeekAtNextChar;
                end;

                C := NextChar;
                if not (C in ['''', '"']) then
                  Error('Expected "''"');

                Token.Value := '';
                C := PeekAtNextChar;
                while (C <> '''') and (C <> '"') and (C <> #10) and (C <> #0) do
                begin
                  Token.Value := Token.Value + NextChar;
                  C := PeekAtNextChar;
                end;

                C := NextChar;
                if not (C in ['''', '"']) then
                  Error('Expected "''"');

                SetLength(Paths, 0);
                Token.Value := Trim(Token.Value);
                Path := Token.Value;
                if not FileExists(Path) then
                begin
                  IsPathFound := False;
                  for S in Self.IncludePathList do
                  begin
                    Path := S + Token.Value;
                    if FileExists(Path) then
                    begin
                      IsPathFound := True;
                      break;
                    end;
                  end;
                end;
                if not IsPathFound then
                begin
                  // Try to search for the whole directory instead
                  if Path.IndexOf('*') >= 0 then
                  begin
                    FindFiles(Path, Paths);
                    if Length(Paths) > 0 then
                      IsPathFound := True;
                  end;
                end;
                //
                if not IsPathFound then
                  Error(Format('"%s" not found', [Path]));
                if Length(Paths) = 0 then
                begin
                  SetLength(Paths, 1);
                  Paths[0] := Path;
                end;
                for Path in Paths do
                begin
                  if Self.IncludeList.IndexOf(Path) < 0 then
                  begin
                    BackupSource := Source;
                    Self.CurrentFileList.Add(Path);
                    ReadFileAsString(Path, FSource);
                    Self.Lex(True);
                    Self.CurrentFileList.Pop;
                    FSource := BackupSource;
                    Self.IncludeList.Add(Path);
                  end;
                end;
                C := PeekAtNextChar;
                continue;
              end;
            'if':
              Token.Kind := tkIf;
            'else':
              Token.Kind := tkElse;
            'for':
              Token.Kind := tkFor;
            'in':
              Token.Kind := tkIn;
            'to':
              Token.Kind := tkTo;
            'do':
              Token.Kind := tkDo;
            'downto':
              Token.Kind := tkDownto;
            'step':
              Token.Kind := tkStep;
            'while':
              Token.Kind := tkWhile;
            'switch':
              Token.Kind := tkSwitch;
            'case':
              Token.Kind := tkCase;
            'const':
              Token.Kind := tkConst;
            'local':
              Token.Kind := tkLocal;
            'default':
              Token.Kind := tkDefault;
            'continue':
              Token.Kind := tkContinue;
            'break':
              Token.Kind := tkBreak;
            'yield':
              Token.Kind := tkYield;
            'return':
              Token.Kind := tkReturn;
            'fn':
              Token.Kind := tkFunctionDecl;
            'void', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64', 'buffer', 'wbuffer':
              Token.Kind := tkAtom;
            'import':
              Token.Kind := tkImport;
            'try':
              Token.Kind := tkTry;
            'catch':
              Token.Kind := tkCatch;
            'throw':
              Token.Kind := tkThrow;
            else
              Token.Kind := tkIdent;
          end;
          C := #32;
        end;
      else
        Error('Unhandled symbol ' + C);
    end;
    TokenList.Add(Token);
EndLabel:
  until C = #0;
  Self.IsLex := True;
end;

function TEvilC.FindFunc(const Name: String): Pointer; inline; overload;
var
  I: Integer;
begin
  for I := Self.FuncScriptList.Count - 1 downto 0 do
  begin
    Result := Self.FuncScriptList.Ptr(I);
    if PSEFuncScriptInfo(Result)^.Name = Name then
      Exit(Result);
  end;
  for I := Self.FuncImportList.Count - 1 downto 0 do
  begin
    Result := Self.FuncImportList.Ptr(I);
    if PSEFuncImportInfo(Result)^.Name = Name then
      Exit(Result);
  end;
  for I := Self.FuncNativeList.Count - 1 downto 0 do
  begin
    Result := Self.FuncNativeList.Ptr(I);
    if PSEFuncNativeInfo(Result)^.Name = Name then
      Exit(Result);
  end;
  Exit(nil);
end;

function TEvilC.FindFuncNative(const Name: String; var Ind: Integer): PSEFuncNativeInfo; inline;
var
  I: Integer;
begin
  for I := Self.FuncNativeList.Count - 1 downto 0 do
  begin
    Result := Self.FuncNativeList.Ptr(I);
    if Result^.Name = Name then
    begin
      Ind := I;
      Exit(Result);
    end;
  end;
  Exit(nil);
end;

function TEvilC.FindFuncScript(const Name: String; var Ind: Integer): PSEFuncScriptInfo; inline;
var
  I: Integer;
begin
  for I := Self.FuncScriptList.Count - 1 downto 0 do
  begin
    Result := Self.FuncScriptList.Ptr(I);
    if Result^.Name = Name then
    begin
      Ind := I;
      Exit(Result);
    end;
  end;
  Exit(nil);
end;

function TEvilC.FindFuncImport(const Name: String; var Ind: Integer): PSEFuncImportInfo; inline;
var
  I: Integer;
begin
  for I := Self.FuncImportList.Count - 1 downto 0 do
  begin
    Result := Self.FuncImportList.Ptr(I);
    if Result^.Name = Name then
    begin
      Ind := I;
      Exit(Result);
    end;
  end;
  Exit(nil);
end;

function TEvilC.FindFunc(const Name: String; var Kind: TSEFuncKind; var Ind: Integer): Pointer; inline; overload;
begin
  Result := FindFuncScript(Name, Ind);
  if Result = nil then
  begin
    Result := FindFuncNative(Name, Ind);
    if Result = nil then
    begin
      Result := FindFuncImport(Name, Ind);
      if Result <> nil then
        Kind := sefkImport;
    end else
      Kind := sefkNative;
  end else
    Kind := sefkScript;
end;

procedure TEvilC.Parse;
var
  Pos: Integer = -1;
  CurrentLine: Integer = -1;
  Token: TSEToken;
  ContinueStack: TSEListStack;
  BreakStack: TSEListStack;
  ReturnStack: TSEListStack;
  CanEmit: Boolean = True;

  procedure Error(const S: String; const Token: TSEToken);
  begin
    ErrorLn := Token.Ln;
    ErrorCol := Token.Col;
    if Token.BelongedFileName = '' then
      raise Exception.CreateFmt('[%d:%d] %s', [Token.Ln, Token.Col, S])
    else
      raise Exception.CreateFmt('[%s:%d:%d] %s', [Token.BelongedFileName, Token.Ln, Token.Col, S]);
  end;

  function FindVar(const Name: String; const IsSameLocal: Boolean = False): PSEIdent; inline;
  var
    I: Integer;
  begin
    for I := Self.VarList.Count - 1 downto 0 do
    begin
      Result := Self.VarList.Ptr(I);
      if Result^.Name = Name then
        if (not IsSameLocal) or (IsSameLocal and (Result^.Local = Self.FuncTraversal) and (Result^.Block >= Self.BlockTraversal)) then
          Exit(Result);
    end;
    Exit(nil);
  end;

  procedure AddSymbol(Kind: TSESymbolKind; Name: String; Code: Integer);
  var
    Symbol: TSESymbol;
  begin
    Symbol.Binary := Self.BinaryPos;
    Symbol.Code := Code;
    Symbol.Kind := Kind;
    Symbol.Name := Name;
    Self.VM.SymbolList.Add(Symbol);
  end;

  function PeekAtNextToken: TSEToken; inline;
  var
    P: Integer;
  begin
    P := Pos + 1;
    if P >= Self.TokenList.Count then
      P := P - 1;
    Exit(Self.TokenList[P]);
  end;

  function PeekAtNextNextToken: TSEToken; inline;
  var
    P: Integer;
  begin
    P := Pos + 2;
    if P >= Self.TokenList.Count then
      P := P - 2;
    Exit(Self.TokenList[P]);
  end;

  function NextToken: TSEToken; inline;
  var
    LineOfCode: TSELineOfCode;
  begin
    Pos := Pos + 1;
    if Pos >= Self.TokenList.Count then
      Pos := Pos - 1;
    Result := Self.TokenList[Pos];
    if (Self.LineOfCodeList.Count = 0) or
       ((Self.LineOfCodeList.Count > 0) and (CurrentLine <> Result.Ln)) then
    begin
      LineOfCode.BinaryCount := Self.Binary.Count;
      LineOfCode.BinaryPtr := Self.BinaryPos;
      LineOfCode.Line := Result.Ln;
      LineOfCode.Module := Result.BelongedFileName;
      CurrentLine := LineOfCode.Line;
      Self.LineOfCodeList.Add(LineOfCode);
    end;
  end;

  function TokenTypeString(const Kinds: TSETokenKinds): String; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := '';
    for Kind in Kinds do
      Result := Result + '"' + TokenNames[Kind] + '", ';
  end;

  function NextTokenExpected(const Expected: TSETokenKinds): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := NextToken;
    for Kind in Expected do
      if Kind = Result.Kind then
        Exit;
    Error(Format('Expected %s but got %s', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function PeekAtNextTokenExpected(const Expected: TSETokenKinds): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := PeekAtNextToken;
    for Kind in Expected do
      if Kind = Result.Kind then
        Exit;
    Error(Format('Expected %s but got "%s"', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function PeekAtPrevOp(const Ind: Integer): PSEOpcodeInfo; inline;
  var
    I: Integer;
  begin
    I := Self.OpcodeInfoList.Count - 1 - Ind;
    if I >= 0 then
      Result := Self.OpcodeInfoList.Ptr(I)
    else
      Result := nil;
  end;

  function PeekAtPrevOpExpected(const Ind: Integer; const Expected: TSEOpcodes): PSEOpcodeInfo; inline;
  var
    Op: TSEOpcode;
  begin
    Result := PeekAtPrevOp(Ind);
    if Result <> nil then
      for Op in Expected do
        if Op = Result^.Op then
          Exit;
    Result := nil;
  end;

  procedure DeleteOps(const Count: Integer);
  var
    I: Integer;
    Size: Integer = 0;
  begin
    for I := Self.OpcodeInfoList.Count - Count - 1 to Count - 1 do
      Size := Size + Self.OpcodeInfoList.Ptr(I)^.Size;
    Self.Binary.DeleteRange(Self.Binary.Count - Size, Size);
    Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - Count, Count);
  end;

  function CreateIdent(const Kind: TSEIdentKind; const Token: TSEToken; const IsUsed: Boolean; const IsConst: Boolean): TSEIdent; inline;
  begin
    if Kind = ikVariable then
    begin
      if Self.FuncCurrent >= 0 then
        Self.FuncScriptList.Ptr(Self.FuncCurrent)^.VarSymbols.Add(Token.Value)
      else
        Self.GlobalVarSymbols.Add(Token.Value);
    end;
    Result.Kind := Kind;
    Result.Ln := Token.Ln;
    Result.Col := Token.Col;
    Result.Name := Token.Value;
    Result.Local := Self.FuncTraversal;
    Result.Block := Self.BlockTraversal;
    Result.IsUsed := IsUsed;
    Result.IsConst := IsConst;
    Result.ConstValue := SENull;
    Result.IsAssigned := False;
    if Result.Local > 0 then
    begin
      Result.Addr := Self.LocalVarCountList.Last;
      Self.LocalVarCountList[Self.LocalVarCountList.Count - 1] := Self.LocalVarCountList.Last + 1;
    end else
    begin
      Result.Addr := Self.GlobalVarCount;
      Inc(Self.GlobalVarCount);
    end;
    Self.VarList.Add(Result);
  end;

  function CreateConstString(const S: String): Integer; inline;
  begin
    Result := Self.VM.ConstStrings.Add(S);
  end;

  function CreatePackedString(const S: String): TSEValue; inline;
  begin
    Result.Kind := sevkPackedString;
    Result.VarPackedString := S;
  end;

  procedure Rewind(const StartAddr, Count: Integer); inline;
  var
    Addr, I: Integer;
  begin
    for I := 0 to Count - 1 do
    begin
      Addr := StartAddr + I;
      Self.Binary.Add(Self.Binary[Addr]);
    end;
    Self.Binary.DeleteRange(StartAddr, Count);
  end;

  function Emit(const Data: array of TSEValue): Integer; inline;
  var
    I: Integer;
    OpcodeInfo: TSEOpcodeInfo;
  begin
    if not CanEmit then
      Exit(Self.Binary.Count);
    OpcodeInfo.Pos := Self.Binary.Count;
    OpcodeInfo.Size := Length(Data);
    OpcodeInfo.Binary := Self.Binary;
    if (Integer(Data[0].VarPointer) = Integer(opPushConst)) and (Data[1].Kind = sevkString) then
    begin
      // Use EmitConstString() instead
      OpcodeInfo.Op := opPushConstString;
      Self.Binary.Add(Pointer(opPushConstString));
      Self.Binary.Add(Pointer(CreateConstString(Data[1].VarString^)));
    end else
    begin
      OpcodeInfo.Op := TSEOpcode(Integer(Data[0].VarPointer));
      for I := Low(Data) to High(Data) do
      begin
        Self.Binary.Add(Data[I]);
      end;
    end;
    Self.OpcodeInfoList.Add(OpcodeInfo);
    Exit(Self.Binary.Count);
  end;

  function EmitConstString(const AString: String): Integer; inline;
  var
    OpcodeInfo: TSEOpcodeInfo;
  begin
    if not CanEmit then
      Exit(Self.Binary.Count);
    OpcodeInfo.Pos := Self.Binary.Count;
    OpcodeInfo.Size := 2;
    OpcodeInfo.Binary := Self.Binary;
    OpcodeInfo.Op := opPushConstString;
    Self.Binary.Add(Pointer(opPushConstString));
    Self.Binary.Add(Pointer(CreateConstString(AString)));
    Self.OpcodeInfoList.Add(OpcodeInfo);
    Exit(Self.Binary.Count);
  end;

  function GetVarFrame(const Ident: TSEIdent): Pointer; inline;
  begin
    if Ident.Local > 0 then
      Result := Pointer(Self.FuncTraversal - Ident.Local)
    else
      Result := Pointer(SE_REG_GLOBAL);
  end;

  function PeepholePushVar2Optimization: Boolean;
  var
    A, B: TSEValue;
    I: Integer;
    P, PP: Pointer;
    OpInfoPrev1,
    OpInfoPrev2: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar, opPushLocalVar]);
    OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushGlobalVar, opPushLocalVar]);
    if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
    begin
      if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
        Exit;
      if OpInfoPrev1^.Op = opPushLocalVar then
        PP := Self.Binary[OpInfoPrev1^.Pos + 2].VarPointer
      else
        PP := Pointer(SE_REG_GLOBAL);
      if OpInfoPrev2^.Op = opPushLocalVar then
        P := Self.Binary[OpInfoPrev2^.Pos + 2].VarPointer
      else
        P := Pointer(SE_REG_GLOBAL);
      A := Self.Binary[OpInfoPrev2^.Pos + 1];
      B := Self.Binary[OpInfoPrev1^.Pos + 1];
      Self.Binary.DeleteRange(Self.Binary.Count - (OpInfoPrev1^.Size + OpInfoPrev2^.Size), OpInfoPrev1^.Size + OpInfoPrev2^.Size);
      Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
      Emit([Pointer(Integer(opPushVar2)), A.VarPointer, B.VarPointer, Pointer(P), Pointer(PP)]);
      Result := True;
    end;
  end;

  function EmitPushVar(const Ident: TSEIdent; const IsPotentialRewind: Boolean = False): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opPushLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opPushGlobalVar), Pointer(Ident.Addr)]);
    if not IsPotentialRewind then
      PeepholePushVar2Optimization;
  end;

  function EmitAssignVar(const Ident: TSEIdent): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalVar), Pointer(Ident.Addr)]);
  end;

  function EmitAssignArray(const Ident: TSEIdent; const ArgCount: Integer): Integer; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalArray), Ident.Addr, ArgCount, Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalArray), Ident.Addr, ArgCount]);
  end;

  function EmitAssignArrayFast(const Ident: TSEIdent; const AValue: TSEValue): Integer; inline;
  begin
    Result := Emit([Pointer(opAssignArrayFast), Pointer(Ident.Addr), GetVarFrame(Ident), AValue])
  end;

  function EmitAssignMapFast(const Ident: TSEIdent; const AValue: String): Integer; inline;
  begin
    Result := Emit([Pointer(opAssignMapFast), Pointer(Ident.Addr), GetVarFrame(Ident), Pointer(CreateConstString(Avalue))]);
  end;

  procedure Patch(const Addr: Integer; const Data: TSEValue); inline;
  begin
    Self.Binary[Addr] := Data;
  end;

  function PatchRange(const Addr: Integer; const Data: array of TSEValue): Integer; inline;
  var
    I: Integer;
  begin
    for I := Low(Data) to High(Data) do
    begin
      Self.Binary[Addr + I] := Data[I];
    end;
    Exit(Addr + I + 1);
  end;

  function IdentifyIdent(const Ident: String; const IsLocal: Boolean = False): TSETokenKind; inline;
  begin
    if FindVar(Ident, IsLocal) <> nil then
      Exit(tkVariable);
    if FindFunc(Ident) <> nil then
      Exit(tkFunction);
    if Self.ConstMap.{$ifdef SE_MAP_AVK959}Contains{$else}ContainsKey{$endif}(Ident) then
      Exit(tkConst);
    Exit(tkUnknown);
  end;

  function GetIdentLocalValue(const Ident: TSEIdent): Pointer;
  begin
    if Ident.Local <= 0 then
      Result := Pointer(SE_REG_GLOBAL)
    else
      Result := Pointer(Self.FuncTraversal - Ident.Local);
  end;

  procedure ParseFuncCall(const Name: String); forward;
  procedure ParseFuncRefCall(const ThisRefIdent: PSEIdent = nil); forward;
  procedure ParseFuncRefCallByName(const Name: String); forward;
  procedure ParseBlock(const IsCase: Boolean = False); forward;
  procedure ParseArrayAssign; forward;
  procedure ParseFuncAnonDecl(const ATraversal: Cardinal = 1); forward;

  function OpToOp1(const Op: TSEOpcode): TSEOpcode; inline;
  begin
    case Op of
      opOperatorAdd:
        Result := opOperatorAdd1;
      opOperatorSub:
        Result := opOperatorSub1;
      opOperatorMul:
        Result := opOperatorMul1;
      opOperatorDiv:
        Result := opOperatorDiv1;
    end;
  end;

  function OpToOp2(const Op: TSEOpcode): TSEOpcode; inline;
  begin
    case Op of
      opOperatorAdd:
        Result := opOperatorAdd2;
      opOperatorSub:
        Result := opOperatorSub2;
      opOperatorMul:
        Result := opOperatorMul2;
      opOperatorDiv:
        Result := opOperatorDiv2;
    end;
  end;

  function PeepholeArrayAssignOptimization: Boolean;
  var
    A: TSEValue;
    Size,
    I: Integer;
    P: Pointer;
    OpInfoPrev1,
    OpInfoPrev2: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushArrayPop]);
    OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConst]);
    if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
    begin
      Size := OpInfoPrev1^.Size + OpInfoPrev2^.Size;
      A := Self.Binary[OpInfoPrev2^.Pos + 1];
      Self.Binary.DeleteRange(Self.Binary.Count - Size, Size);
      Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
      Emit([Pointer(opPushArrayPop), A]);
      Result := True;
    end;
  end;

  function PeepholeIncOptimization: Boolean;
  var
    A: TSEValue;
    Size,
    I: Integer;
    P: Pointer;
    VarBase, VarAddr, VarBasePush, VarBaseAddr: Pointer;
    OpInfoPrev1,
    OpInfoPrev2,
    OpInfoPrev3: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    OpInfoPrev1 := PeekAtPrevOpExpected(0, [opAssignGlobalVar, opAssignLocalVar]);
    OpInfoPrev2 := PeekAtPrevOpExpected(1, [opOperatorAdd0]);
    OpInfoPrev3 := PeekAtPrevOpExpected(2, [opPushGlobalVar, opPushLocalVar]);
    if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) and (OpInfoPrev3 <> nil) then
    begin
      if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev3^.Binary <> Pointer(Self.Binary)) then
        Exit;
      VarBase := Self.Binary[OpInfoPrev1^.Pos + 1];
      VarBasePush := Self.Binary[OpInfoPrev3^.Pos + 1];
      if VarBasePush <> VarBase then
        Exit;

      if OpInfoPrev1^.Op = opAssignLocalVar then
        VarAddr := Self.Binary[OpInfoPrev1^.Pos + 2]
      else
        VarAddr := Pointer(SE_REG_GLOBAL);
      if OpInfoPrev3^.Op = opPushLocalVar then
        VarBaseAddr := Self.Binary[OpInfoPrev3^.Pos + 2]
      else
        VarBaseAddr := Pointer(SE_REG_GLOBAL);
      if VarBaseAddr <> VarAddr then
        Exit;

      A := Self.Binary[OpInfoPrev2^.Pos + 1];
      if OpInfoPrev2^.Op = opOperatorSub then
        A := -A;
      Size := OpInfoPrev1^.Size + OpInfoPrev2^.Size + OpInfoPrev3^.Size;;
      Self.Binary.DeleteRange(Self.Binary.Count - Size, Size);
      Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 3, 3);
      Emit([Pointer(opOperatorInc), VarBase, VarAddr, A]);
      Result := True;
    end;
  end;

  function PeepholeOp0Optimization(Op: TSEOpcode): Boolean;
  var
    A, B: TSEValue;
    I: Integer;
    P: Pointer;
    OpInfoPrev1,
    OpInfoPrev2: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    case Op of
      opOperatorAdd,
      opOperatorSub:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
          OpInfoPrev2 := PeekAtPrevOpExpected(1, [
            opPushGlobalVar, opPushLocalVar, opPushArrayPop,
            opOperatorAdd0, opOperatorMul0, opOperatorDiv0,
            opOperatorAdd1, opOperatorSub1, opOperatorMul1, opOperatorDiv1,
            opOperatorAdd2, opOperatorSub2, opOperatorMul2, opOperatorDiv2,
            opCallScript, opCallNative, opCallImport
          ]);
          if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
              Exit;
            A := Self.Binary[OpInfoPrev1^.Pos + 1];
            if A.Kind <> sevkNumber then
              Exit;
            Self.Binary.DeleteRange(Self.Binary.Count - 2, 2);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
            if Op = opOperatorAdd then
              Emit([Pointer(Integer(opOperatorAdd0)), A.VarNumber])
            else
              Emit([Pointer(Integer(opOperatorAdd0)), -A.VarNumber]);
            Result := True;
          end else
          begin
            if Op <> opOperatorAdd then
              Exit;
            OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar, opPushLocalVar]);
            OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConst]);
            // TODO: Handle opPushArrayPop
            if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
            begin
              if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
                Exit;
              A := Self.Binary[OpInfoPrev2^.Pos + 1];
              if A.Kind <> sevkNumber then
                Exit;
              Self.Binary.DeleteRange(Self.Binary.Count - (OpInfoPrev1^.Size + OpInfoPrev2^.Size), 2);
              Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 1);
              Emit([Pointer(Integer(opOperatorAdd0)), A.VarNumber]);
              Result := True;
            end;
          end;
        end;
      opOperatorMul:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
          OpInfoPrev2 := PeekAtPrevOpExpected(1, [
            opPushGlobalVar, opPushLocalVar, opPushArrayPop,
            opOperatorAdd0, opOperatorMul0, opOperatorDiv0,
            opOperatorAdd1, opOperatorSub1, opOperatorMul1, opOperatorDiv1,
            opOperatorAdd2, opOperatorSub2, opOperatorMul2, opOperatorDiv2,
            opCallScript, opCallNative, opCallImport
          ]);
          if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
              Exit;
            A := Self.Binary[OpInfoPrev1^.Pos + 1];
            if A.Kind <> sevkNumber then
              Exit;
            Self.Binary.DeleteRange(Self.Binary.Count - 2, 2);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
            Emit([Pointer(Integer(opOperatorMul0)), A.VarNumber]);
            Result := True;
          end else
          begin
            OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar, opPushLocalVar]);
            OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConst]);
            if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
            begin
              if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
                Exit;
              A := Self.Binary[OpInfoPrev2^.Pos + 1];
              if A.Kind <> sevkNumber then
                Exit;
              Self.Binary.DeleteRange(Self.Binary.Count - (OpInfoPrev1^.Size + OpInfoPrev2^.Size), 2);
              Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 1);
              Emit([Pointer(Integer(opOperatorMul0)), A.VarNumber]);
              Result := True;
            end;
          end;
        end;
      opOperatorDiv:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
          OpInfoPrev2 := PeekAtPrevOpExpected(1, [
            opPushGlobalVar, opPushLocalVar,
            opOperatorAdd0, opOperatorMul0, opOperatorDiv0,
            opOperatorAdd1, opOperatorSub1, opOperatorMul1, opOperatorDiv1,
            opOperatorAdd2, opOperatorSub2, opOperatorMul2, opOperatorDiv2,
            opPushArrayPop, opCallScript, opCallNative, opCallImport
          ]);
          if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
              Exit;
            A := Self.Binary[OpInfoPrev1^.Pos + 1];
            if A.Kind <> sevkNumber then
              Exit;
            Self.Binary.DeleteRange(Self.Binary.Count - 2, 2);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
            Emit([Pointer(Integer(opOperatorDiv0)), A.VarNumber]);
            Result := True;
          end;
        end;
    end;
  end;

  function PeepholeOp1Optimization(Op: TSEOpcode): Boolean;
  var
    A: TSEValue;
    I: Integer;
    P: Pointer;
    OpInfoPrev1,
    OpInfoPrev2: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    case Op of
      opOperatorAdd,
      opOperatorSub,
      opOperatorMul,
      opOperatorDiv:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar, opPushLocalVar]);
          if (OpInfoPrev1 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) then
              Exit;
            if OpInfoPrev1^.Op = opPushLocalVar then
              P := Self.Binary[OpInfoPrev1^.Pos + 2].VarPointer
            else
              P := Pointer(SE_REG_GLOBAL);
            A := Self.Binary[OpInfoPrev1^.Pos + 1];
            Op := OpToOp1(Op);
            Self.Binary.DeleteRange(Self.Binary.Count - OpInfoPrev1^.Size, OpInfoPrev1^.Size);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
            Emit([Pointer(Integer(Op)), A.VarPointer, Pointer(P)]);
            Result := True;
          end;
        end;
    end;
  end;

  function PeepholeOp2Optimization(Op: TSEOpcode): Boolean;
  var
    A, B: TSEValue;
    I: Integer;
    P, PP: Pointer;
    OpInfoPrev1,
    OpInfoPrev2: PSEOpcodeInfo;
  begin
    Result := False;
    if not Self.OptimizePeephole then
      Exit;
    case Op of
      opOperatorAdd,
      opOperatorSub,
      opOperatorMul,
      opOperatorDiv:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushGlobalVar, opPushLocalVar]);
          OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushGlobalVar, opPushLocalVar]);
          if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
              Exit;
            if OpInfoPrev1^.Op = opPushLocalVar then
              PP:= Self.Binary[OpInfoPrev1^.Pos + 2].VarPointer
            else
              PP := Pointer(SE_REG_GLOBAL);
            if OpInfoPrev2^.Op = opPushLocalVar then
              P := Self.Binary[OpInfoPrev2^.Pos + 2].VarPointer
            else
              P := Pointer(SE_REG_GLOBAL);
            B := Self.Binary[OpInfoPrev1^.Pos + 1];
            A := Self.Binary[OpInfoPrev2^.Pos + 1];
            Op := OpToOp2(Op);
            Self.Binary.DeleteRange(Self.Binary.Count - (OpInfoPrev1^.Size + OpInfoPrev2^.Size), OpInfoPrev1^.Size + OpInfoPrev2^.Size);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
            Emit([Pointer(Integer(Op)), A.VarPointer, B.VarPointer, Pointer(P), Pointer(PP)]);
            Result := True;
          end;
        end;
    end;
  end;

  function PeepholeOpXOptimization(Op: TSEOpcode): Boolean;
  var
    IsOptimized: Boolean;
  begin
    Result := False;
    repeat
      IsOptimized := False;
      IsOptimized := PeepholeOp0Optimization(Op);
      if IsOptimized then
      begin
        Result := True;
        Op := PeekAtPrevOp(0)^.Op;
        continue;
      end;
      IsOptimized := PeepholeOp2Optimization(Op);
      if IsOptimized then
      begin
        Result := True;
        Op := PeekAtPrevOp(0)^.Op;
        continue;
      end;
      IsOptimized := PeepholeOp1Optimization(Op);
      if IsOptimized then
      begin
        Result := True;
        Op := PeekAtPrevOp(0)^.Op;
        continue;
      end;
    until not IsOptimized;
  end;

  procedure ParseExpr(const IsParsedAtFuncCall: Boolean = False);
  type
    TProc = TSENestedProc;
  var
    PushConstCount: Integer = 0;
    OpCountStart: Integer;
    IsTailed: Boolean = False;
    FuncRefIdent: TSEIdent;
    FuncRefToken: TSEToken;

    procedure Logic; forward;

    procedure AllocFuncRef;
    begin
      if FuncRefToken.Value = '' then
      begin
        FuncRefToken.Value := '___f' + Self.InternalIdent;
        FuncRefToken.Kind := tkIdent;
        FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True, False);
      end;
    end;

    procedure EmitExpr(const Data: array of TSEValue); inline;
    var
      Op: TSEOpcode;
      V1, V2, V: TSEValue;

      function ConstantFoldingNumberOptimization: Boolean;
      var
        OpInfoPrev1,
        OpInfoPrev2: PSEOpcodeInfo;

        function SameKind: Boolean; inline;
        begin
          V2 := Self.Binary[Self.Binary.Count - 1];
          V1 := Self.Binary[Self.Binary.Count - 3];
          Result := V1.Kind = V2.Kind;
        end;

        procedure Pop2; inline;
        begin
          Self.Binary.DeleteRange(Self.Binary.Count - 4, 4);
          Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
          Dec(PushConstCount);
        end;
      begin
        Result := False;
        if (PushConstCount < 2) or (IsTailed) then Exit;
        OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
        OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConst]);
        if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) and SameKind then
        begin
          if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
            Exit;
          Result := True;
          case Op of
            opOperatorAdd:
              begin
                Pop2;
                SEValueAdd(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorSub:
              begin
                Pop2;
                SEValueSub(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorMul:
              begin
                Pop2;
                SEValueMul(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorDiv:
              begin
                Pop2;
                SEValueDiv(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorMod:
              begin
                Pop2;
                Emit([Pointer(opPushConst), V1 - V2 * Int(TSENumber(V1 / V2))]);
              end;
            opOperatorAnd:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) and Integer(V2)]);
              end;
            opOperatorOr:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) or Integer(V2)]);
              end;
            opOperatorXor:
              begin
                Pop2;
                Emit([Pointer(opPushConst), Integer(V1) xor Integer(V2)]);
              end;
            opOperatorGreater:
              begin
                Pop2;
                SEValueGreater(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorGreaterOrEqual:
              begin
                Pop2;
                SEValueGreaterOrEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorLesser:
              begin
                Pop2;
                SEValueLesser(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorLesserOrEqual:
              begin
                Pop2;
                SEValueLesserOrEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorEqual:
              begin
                Pop2;
                SEValueEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorNotEqual:
              begin
                Pop2;
                SEValueNotEqual(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorShiftLeft:
              begin
                Pop2;
                SEValueShiftLeft(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            opOperatorShiftRight:
              begin
                Pop2;
                SEValueShiftRight(V, V1, V2);
                Emit([Pointer(opPushConst), V]);
              end;
            else
              begin
                PushConstCount := 0;
                Result := False;
              end;
          end;
        end;
      end;

      function ConstantFoldingStringOptimization: Boolean;
      var
        S1, S2: String;
        OpInfoPrev1,
        OpInfoPrev2: PSEOpcodeInfo;
        function SameKind: Boolean; inline;
        begin
          S2 := Self.VM.ConstStrings[Integer(Self.Binary[Self.Binary.Count - 1].VarPointer)];
          S1 := Self.VM.ConstStrings[Integer(Self.Binary[Self.Binary.Count - 3].VarPointer)];
          Result := True;
        end;

        procedure Pop2; inline;
        begin
          Self.Binary.DeleteRange(Self.Binary.Count - 4, 4);
          Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
          Dec(PushConstCount);
        end;
      begin
        Result := False;
        if (PushConstCount < 2) or (IsTailed) or (Op <> opOperatorAdd) then Exit;
        OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConstString]);
        OpInfoPrev2 := PeekAtPrevOpExpected(1, [opPushConstString]);
        if (OpInfoPrev1 <> nil) and (OpInfoPrev2 <> nil) and SameKind then
        begin
          if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) or (OpInfoPrev2^.Binary <> Pointer(Self.Binary)) then
            Exit;
          Result := True;
          Pop2;
          EmitConstString(S1 + S2);
        end;
      end;

    begin
      try
        Op := TSEOpcode(Integer(Data[0].VarPointer));
        if Op = opPushConst then
        begin
          Emit(Data);
          Inc(PushConstCount)
        end else
        if (PeepholeOp0Optimization(Op) or PeepholeOpXOptimization(Op)) then
          PushConstCount := 0
        else
        if Self.OptimizeConstantFolding and (ConstantFoldingNumberOptimization or ConstantFoldingStringOptimization) then
        else
          Emit(Data);
      except
        on E: Exception do
          raise Exception.Create(Format('Error while performing optimization! (%s)', [E.Message]));
      end;
    end;

    procedure BinaryOp(const Op: TSEOpcode; const Func: TProc); inline;
    begin
      NextToken;
      PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkDot, tkNumber, tkString, tkNegative, tkIdent]);
      Func;
      EmitExpr([Pointer({$ifdef CPU64}Int64(Op){$else}Op{$endif})]);
    end;

    procedure Tail;
    var
      Token: TSEToken;
    begin
      case PeekAtNextToken.Kind of
        tkSquareBracketOpen:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            ParseExpr;
            NextTokenExpected([tkSquareBracketClose]);
            AllocFuncRef;
            EmitAssignVar(FuncRefIdent);
            EmitPushVar(FuncRefIdent, True);
            EmitExpr([Pointer(opPushArrayPop), SENull]);
            PeepholeArrayAssignOptimization;
            Tail;
          end;
        tkDot:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            Token := NextTokenExpected([tkIdent]);
            AllocFuncRef;
            EmitAssignVar(FuncRefIdent);
            EmitPushVar(FuncRefIdent, True);
            if Length(Token.Value) <= 8 then
              EmitExpr([Pointer(opPushArrayPop), CreatePackedString(Token.Value)])
            else
              EmitExpr([Pointer(opPushArrayPopString), Pointer(CreateConstString(Token.Value))]);
            Tail;
          end;
      end;
    end;

    procedure Factor;
    var
      Token, Token2: TSEToken;
      Ident: PSEIdent;
      FuncValue: TSEValue;
      Ind: Integer;
      P: Pointer;

      procedure FuncTail(IsFirst: Boolean = True);
      begin
        while PeekAtNextToken.Kind = tkBracketOpen do
        begin
          if FuncRefToken.Value <> '' then
            ParseFuncRefCall(@FuncRefIdent)
          else
            ParseFuncRefCall(Ident);
          IsFirst := True;
          while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
          begin
            if IsFirst then
            begin
              AllocFuncRef;
              IsFirst := False;
              EmitAssignVar(FuncRefIdent);
            end;
            EmitPushVar(FuncRefIdent, True);
            Tail;
          end;
        end;
      end;

    begin
      Token := PeekAtNextTokenExpected([
        tkBracketOpen, tkBracketClose, tkSquareBracketOpen, tkDot, tkNumber, tkEOF,
        tkNegative, tkNot, tkString, tkIdent, tkFunctionDecl]);
      case Token.Kind of
        tkBracketOpen:
          begin
            NextToken;
            if PeekAtNextToken.Kind = tkFunctionDecl then
            begin
              Factor;
              NextTokenExpected([tkBracketClose]);
              if PeekAtNextToken.Kind = tkBracketOpen then
              begin
                AllocFuncRef;
                EmitAssignVar(FuncRefIdent);
                EmitPushVar(FuncRefIdent, True);
                FuncTail(False);
              end;
            end else
            begin
              PeekAtNextTokenExpected([tkNegative, tkNot, tkBracketOpen, tkNumber, tkIdent, tkFunctionDecl]);
              Logic();
              NextTokenExpected([tkBracketClose]);
            end;
          end;
        tkFunctionDecl:
          begin
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            if IsParsedAtFuncCall then
              ParseFuncAnonDecl(2)
            else
              ParseFuncAnonDecl;
          end;
        tkSquareBracketOpen:
          begin
            NextToken;
            ParseArrayAssign;
          end;
        tkNumber:
          begin
            NextToken;
            EmitExpr([Pointer(opPushConst), PointStrToFloat(Token.Value)]);
          end;
        tkString:
          begin
            NextToken;
            EmitExpr([Pointer(opPushConst), Token.Value]);
          end;
        tkIdent:
          begin
            case IdentifyIdent(Token.Value) of
              tkVariable:
                begin
                  NextToken;
                  if PeekAtNextToken.Kind = tkBracketOpen then // Likely function ref
                  begin
                    ParseFuncRefCallByName(Token.Value);
                  end else
                  begin
                    Ident := FindVar(Token.Value);
                    Ident^.IsUsed := True;
                    if Ident^.IsConst and (Ident^.ConstValue.Kind <> sevkNull) then
                    begin
                      EmitExpr([Pointer(opPushConst), Ident^.ConstValue]);
                    end else
                    begin
                      case PeekAtNextToken.Kind of
                        tkSquareBracketOpen:
                          begin
                            PushConstCount := 0;
                            IsTailed := True;
                            NextToken;
                            EmitPushVar(Ident^, True);
                            ParseExpr;
                            Emit([Pointer(opPushArrayPop), SENull]);
                            PeepholeArrayAssignOptimization;
                            NextTokenExpected([tkSquareBracketClose]);
                            Tail;
                            FuncTail;
                          end;
                        tkDot:
                          begin
                            PushConstCount := 0;
                            IsTailed := True;
                            NextToken;
                            Token2 := NextTokenExpected([tkIdent]);
                            EmitPushVar(Ident^, True);
                            if Length(Token2.Value) <= 8 then
                              Emit([Pointer(opPushArrayPop), CreatePackedString(Token2.Value)])
                            else
                              Emit([Pointer(opPushArrayPopString), Pointer(CreateConstString(Token2.Value))]);
                            Tail;
                            FuncTail;
                          end;
                        else
                          EmitPushVar(Ident^);
                      end;
                    end;
                  end;
                end;
              tkConst:
                begin
                  NextToken;
                  EmitExpr([Pointer(opPushConst), Self.ConstMap[Token.Value]]);
                  AddSymbol(sesConst, Token.Value, Self.Binary.Count - 1);
                end;
              tkFunction:
                begin
                  NextToken;
                  if PeekAtNextToken.Kind <> tkBracketOpen then // Likely function ref
                  begin
                    P := FindFunc(Token.Value, FuncValue.VarFuncKind, Ind);
                    if P = nil then
                      Error(Format('Function "%s" not found', [Token.Value]), Token);
                    FuncValue.VarFuncIndx := Ind;
                    FuncValue.Kind := sevkFunction;
                    PushConstCount := 0;
                    EmitExpr([Pointer(opPushConst), FuncValue]);
                  end else
                  begin
                    ParseFuncCall(Token.Value);
                  end;
                  if PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] then
                  begin
                    FuncRefToken.Value := '___f' + Self.InternalIdent;
                    FuncRefToken.Kind := tkIdent;
                    FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True, False);
                    EmitAssignVar(FuncRefIdent);
                    EmitPushVar(FuncRefIdent, True);
                    Tail;
                    FuncTail;
                  end;
                end;
              else
                Error(Format('Unknown identifier "%s"', [Token.Value]), Token);
            end;
          end;
      end;
    end;

    procedure SignedFactor;
    var
      Token: TSEToken;
    begin
      Factor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkNegative:
            begin
              NextToken;
              PeekAtNextTokenExpected([tkBracketOpen, tkNumber, tkIdent]);
              Factor;
              EmitExpr([Pointer(opOperatorNegative)]);
            end;
          tkNot:
            begin
              NextToken;
              PeekAtNextTokenExpected([tkBracketOpen, tkNumber, tkIdent]);
              Factor;
              EmitExpr([Pointer(opOperatorNot)]);
            end;
          else
            Exit;
        end;
      end;
    end;

    procedure Pow;
    var
      Token: TSEToken;
    begin
      SignedFactor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkPow:
            BinaryOp(opOperatorPow, @SignedFactor);
          else
            Exit;
        end;
      end;
    end;

    procedure Term;
    var
      Token: TSEToken;
    begin
      Pow;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkMul:
            BinaryOp(opOperatorMul, @Pow);
          tkDiv:
            BinaryOp(opOperatorDiv, @Pow);
          tkMod:
            BinaryOp(opOperatorMod, @Pow);
          else
            Exit;
        end;
      end;
    end;

    procedure Expr;
    var
      Token: TSEToken;
    begin
      Term;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkAdd:
            BinaryOp(opOperatorAdd, @Term);
          tkSub:
            BinaryOp(opOperatorSub, @Term);
          else
            Exit;
        end;
      end;
    end;

    procedure Bitwise;
    var
      Token: TSEToken;
    begin
      Expr;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkShiftLeft:
            BinaryOp(opOperatorShiftLeft, @Expr);
          tkShiftRight:
            BinaryOp(opOperatorShiftRight, @Expr);
          else
            Exit;
        end;
      end;
    end;

    procedure Logic;
    var
      Token: TSEToken;
    begin
      Bitwise;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkEqual:
            BinaryOp(opOperatorEqual, @Bitwise);
          tkNotEqual:
            BinaryOp(opOperatorNotEqual, @Bitwise);
          tkGreater:
            BinaryOp(opOperatorGreater, @Bitwise);
          tkGreaterOrEqual:
            BinaryOp(opOperatorGreaterOrEqual, @Bitwise);
          tkSmaller:
            BinaryOp(opOperatorLesser, @Bitwise);
          tkSmallerOrEqual:
            BinaryOp(opOperatorLesserOrEqual, @Bitwise);
          tkAnd:
            BinaryOp(opOperatorAnd, @Bitwise);
          tkOr:
            BinaryOp(opOperatorOr, @Bitwise);
          tkXor:
            BinaryOp(opOperatorXor, @Bitwise);
          else
            Exit;
        end;
      end;
    end;

  var
    Expr2Block,
    EndBlock,
    JumpEnd,
    JumpExpr2: Integer;

  begin
    OpCountStart := Self.OpcodeInfoList.Count;
    Logic;
    // Handle ternary
    if PeekAtNextToken.Kind = tkQuestion then
    begin
      NextToken;
      JumpExpr2 := Emit([Pointer(opJumpEqual1Rel), False, Pointer(0)]);
      ParseExpr;
      NextTokenExpected([tkColon]);
      JumpEnd := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
      Expr2Block := Self.Binary.Count;
      ParseExpr;
      EndBlock := Self.Binary.Count;
      Patch(JumpExpr2 - 1, Pointer(Expr2Block) - (JumpExpr2 - 3));
      Patch(JumpEnd - 1, Pointer(EndBlock) - (JumpEnd - 2));
    end;
  end;

  procedure ParseFuncRefCallByMapRewind(const Ident: TSEIdent; const DeepCount, RewindStartAdd: Integer; const ThisRefIdent: PSEIdent = nil);
  var
    Token: TSEToken;
    ArgCount: Integer = 1;
    RewindCount: Integer;
    This: PSEIdent;
  begin
    RewindCount := Self.Binary.Count - RewindStartAdd;
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr(True);
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^, True)
    else
    begin
      This := FindVar('self');
      if (This <> nil) and (This^.Local > 0) then
        EmitPushVar(This^)
      else
        Emit([Pointer(opPushConst), SENull]);
    end;
    // Push map to stack
    Rewind(RewindStartAdd, RewindCount);
    EmitPushVar(Ident, True);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount), Pointer(DeepCount)]);
    if PeekAtNextToken.Kind = tkBracketOpen then
      ParseFuncRefCall;
  end;

  procedure ParseFuncRefCall(const ThisRefIdent: PSEIdent = nil);
  var
    FuncIdent: TSEIdent;
    FuncToken: TSEToken;
    Token: TSEToken;
    ArgCount: Integer = 1;
    This: PSEIdent;
  begin
    FuncToken.Value := '___fn' + Self.InternalIdent;
    FuncToken.Kind := tkIdent;
    FuncIdent := CreateIdent(ikVariable, FuncToken, True, False);
    EmitAssignVar(FuncIdent);
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr(True);
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^, True)
    else
    begin
      This := FindVar('self');
      if (This <> nil) and (This^.Local > 0) then
        EmitPushVar(This^)
      else
        Emit([Pointer(opPushConst), SENull]);
    end;
    EmitPushVar(FuncIdent);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount), Pointer(0)]);
    if PeekAtNextToken.Kind = tkBracketOpen then
      ParseFuncRefCall;
  end;

  procedure ParseFuncRefCallByName(const Name: String);
  var
    Token: TSEToken;
    ArgCount: Integer = 1;
    This: PSEIdent;
  begin
    NextTokenExpected([tkBracketOpen]);
    // Allocate stack for result
    Emit([Pointer(opPushConst), SENull]);
    Token := PeekAtNextToken;
    if Token.Kind = tkBracketClose then
      NextToken;
    while not (Token.Kind = tkBracketClose) do
    begin
      ParseExpr(True);
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    This := FindVar('self');
    if (This <> nil) and (This^.Local > 0) then
      EmitPushVar(This^)
    else
      Emit([Pointer(opPushConst), SENull]);
    // We now push func def to stack
    EmitPushVar(FindVar(Name)^);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount), Pointer(0)]);
    if PeekAtNextToken.Kind = tkBracketOpen then
      ParseFuncRefCall;
  end;

  procedure ParseFuncCall(const Name: String);
  var
    FuncNativeInfo: PSEFuncNativeInfo = nil;
    FuncScriptInfo: PSEFuncScriptInfo = nil;
    FuncImportInfo: PSEFuncImportInfo = nil;
    I, Ind: Integer;
    DefinedArgCount: Integer;
    ArgCount: Integer = 0;
    Token: TSEToken;
    This: PSEIdent;
  begin
    FuncNativeInfo := FindFuncNative(Name, Ind);
    if FuncNativeInfo <> nil then
      DefinedArgCount := FuncNativeInfo^.ArgCount
    else
    begin
      FuncScriptInfo := FindFuncScript(Name, Ind);
      if FuncScriptInfo <> nil then
        DefinedArgCount := FuncScriptInfo^.ArgCount
      else
      begin
        FuncImportInfo := FindFuncImport(Name, Ind);
        if FuncImportInfo <> nil then
          DefinedArgCount := Length(FuncImportInfo^.Args);
      end;
    end;
    if FuncScriptInfo <> nil then // Allocate stack for result
      Emit([Pointer(opPushConst), SENull]);
    if DefinedArgCount > 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      for I := 0 to DefinedArgCount - 1 do
      begin
        ParseExpr(True);
        if I < DefinedArgCount - 1 then
          NextTokenExpected([tkComma]);
        Inc(ArgCount);
      end;
      NextTokenExpected([tkBracketClose]);
    end else
    if DefinedArgCount < 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      repeat
        ParseExpr(True);
        Token := NextTokenExpected([tkComma, tkBracketClose]);
        Inc(ArgCount);
      until Token.Kind = tkBracketClose;
    end else
    begin
      NextTokenExpected([tkBracketOpen]);
      NextTokenExpected([tkBracketClose]);
    end;
    if FuncNativeInfo <> nil then
    begin
      Emit([Pointer(opCallNative), Pointer(Ind), Pointer(ArgCount), Pointer(0)]);
    end else
    if FuncScriptInfo <> nil then
    begin
      This := FindVar('self');
      if (This <> nil) and (This^.Local > 0) then
        EmitPushVar(This^)
      else
        Emit([Pointer(opPushConst), SENull]); // this
      Inc(ArgCount);
      Emit([Pointer(opCallScript), Pointer(Ind), Pointer(ArgCount), Pointer(0)])
    end
    else
      Emit([Pointer(opCallImport), Pointer(Ind), Pointer(0), Pointer(0)]);
    if PeekAtNextToken.Kind = tkBracketOpen then
      ParseFuncRefCall;
  end;

  function ParseFuncDecl(const IsAnon: Boolean = False): TSEToken;
  var
    Token, TokenResult: TSEToken;
    Name: String;
    OldFuncCurrent: Integer;
    ArgCount: Integer = 0;
    I, FuncIndex: Integer;
    ReturnList: TList;
    Func: PSEFuncScriptInfo;
    ParentBinary: TSEBinary;
    ParentBinaryPos: Integer;
    VarSymbols: TStrings;
  begin
    ReturnList := TList.Create;
    VarSymbols := TStringList.Create;
    try
      OldFuncCurrent := Self.FuncCurrent;
      ReturnStack.Push(ReturnList);
      if not IsAnon then
      begin
        Token := NextTokenExpected([tkIdent]);
        Name := Token.Value;
        if (Self.FuncTraversal = 0) and (FindFunc(Name) <> nil) then
          Error(Format('Duplicate function declaration "%s"', [Token.Value]), Token);
      end else
      begin
        Token.Kind := tkIdent;
        Token.Value := '___fn' + Self.InternalIdent;
        Name := Token.Value;
      end;
      Result := Token;
      Func := RegisterScriptFunc(Name, 0);

      TokenResult.Value := 'result';
      TokenResult.Kind := tkIdent;
      CreateIdent(ikVariable, TokenResult, True, False);

      NextTokenExpected([tkBracketOpen]);
      repeat
        if PeekAtNextToken.Kind = tkIdent then
        begin
          Token := NextTokenExpected([tkIdent]);
          CreateIdent(ikVariable, Token, False, False);
          Inc(ArgCount);
        end;
        Token := NextTokenExpected([tkComma, tkBracketClose]);
      until Token.Kind = tkBracketClose;

      Token.Value := 'self';
      Token.Kind := tkIdent;
      CreateIdent(ikVariable, Token, True, False);

      Func^.ArgCount := ArgCount;
      for I := 0 to VarSymbols.Count - 1 do
        Func^.VarSymbols.Add(VarSymbols[I]);
      FuncIndex := Self.FuncScriptList.Count - 1;
      ParentBinary := Self.Binary;
      ParentBinaryPos := Self.BinaryPos;
      Self.Binary := Self.VM.Binaries.Value^.Data[Func^.BinaryPos];
      Self.BinaryPos := Func^.BinaryPos;
      if PeekAtNextToken.Kind = tkEqual then
        Self.TokenList.Insert(Pos + 1, TokenResult);
      ParseBlock;

      ReturnList := ReturnStack.Pop;
      for I := 0 to ReturnList.Count - 1 do
        Patch(Integer(ReturnList[I]), Pointer(Self.Binary.Count));
      Emit([Pointer(opPopFrame)]);

      // The pointer may be changed due to reallocation, need to query for it again
      Func := Self.FuncScriptList.Ptr(FuncIndex);
      Func^.VarCount := Self.LocalVarCountList[Self.LocalVarCountList.Count - 1] - ArgCount;
      Self.Binary := ParentBinary;
      Self.BinaryPos := ParentBinaryPos;
    finally
      Self.FuncCurrent := OldFuncCurrent;
      ReturnList.Free;
      VarSymbols.Free;
    end;
  end;

  procedure ParseFuncAnonDecl(const ATraversal: Cardinal = 1);
  var
    I, J: Integer;
    FuncValue: TSEValue;
    Ind: Integer;
    P: Pointer;
  begin
    Inc(Self.FuncTraversal, ATraversal);
    Self.LocalVarCountList.Add(-1);
    Self.ScopeStack.Push(Self.VarList.Count);
    Self.ScopeFunc.Push(Self.FuncScriptList.Count + 1);
    Token := ParseFuncDecl(True);
    I := Self.ScopeStack.Pop;
    Self.VarList.DeleteRange(I, Self.VarList.Count - I);
    I := Self.ScopeFunc.Pop;
    for J := I to Self.FuncScriptList.Count - 1 do
    begin
      if Self.FuncScriptList.Ptr(J)^.Name.IndexOf('___fn') <> 0 then
        Self.FuncScriptList.Ptr(J)^.Name := '';
    end;
    Self.LocalVarCountList.Delete(Self.LocalVarCountList.Count - 1);
    Dec(Self.FuncTraversal, ATraversal);
    //
    P := FindFunc(Token.Value, FuncValue.VarFuncKind, Ind);
    if P = nil then
      Error(Format('Function "%s" not found', [Token.Value]), Token);
    case FuncValue.VarFuncKind of
      sefkScript, sefkImport:
        FuncValue.VarFuncIndx := Ind;
      sefkNative:
        FuncValue.VarFuncIndx := QWord(P);
    end;
    FuncValue.Kind := sevkFunction;
    Emit([Pointer(opPushConst), FuncValue]);
  end;

  procedure ParseFuncImport;

    function GetAtom(const Token: TSEToken; const IsVoidForbid: Boolean = False): TSEAtomKind;
    begin
      case Token.Value of
        'void':
          begin
            if IsVoidForbid then
              Error('"void" type it not allowed as parameter', Token);
            Result := seakVoid;
          end;
        'u8':
          Result := seakU8;
        'u16':
          Result := seakU16;
        'u32':
          Result := seakU32;
        'u64':
          Result := seakU64;
        'i8':
          Result := seakI8;
        'i16':
          Result := seakI16;
        'i32':
          Result := seakI32;
        'i64':
          Result := seakI64;
        'f32':
          Result := seakF32;
        'f64':
          Result := seakF64;
        'buffer':
          Result := seakBuffer;
        'wbuffer':
          Result := seakWBuffer;
      end;
    end;

    procedure FuncImport(const Lib: String);
    var
      Token: TSEToken;
      CC: TSECallingConvention = seccAuto;
      Name, ActualName: String;
      Return: TSEAtomKind;
      Args: TSEAtomKindArray;
    begin
      NextTokenExpected([tkFunctionDecl]);
      Token := NextTokenExpected([tkIdent]);
      if PeekAtNextToken.Kind = tkIdent then
      begin
        // Calling convention
        case Token.Value of
          'stdcall':
            CC := seccStdcall;
          'cdecl':
            CC := seccCdecl;
          else
            Error(Format('Unsupported calling convention "%s"', [Token.Value]), Token);
        end;
        Token := NextTokenExpected([tkIdent]);
      end;
      Name := Token.Value;

      if FindFunc(Name) <> nil then
        Error(Format('Duplicate function declaration "%s"', [Token.Value]), Token);

      NextTokenExpected([tkBracketOpen]);
      repeat
        if PeekAtNextToken.Kind = tkAtom then
        begin
          Token := NextTokenExpected([tkAtom]);
          SetLength(Args, Length(Args) + 1);
          Args[Length(Args) - 1] := GetAtom(Token, True);
        end;
        Token := NextTokenExpected([tkComma, tkBracketClose]);
      until Token.Kind = tkBracketClose;
      NextTokenExpected([tkColon]);
      Token := NextTokenExpected([tkAtom]);
      Return := GetAtom(Token);
      if PeekAtNextToken.Kind = tkString then
      begin
        Token := NextToken;
        ActualName := Token.Value;
      end else
        ActualName := Name;

      Self.RegisterImportFunc(Name, ActualName, Lib, Args, Return, CC);
    end;

  var
    Token: TSEToken;
    Lib: TLibHandle = 0;
    LibName: String;
    LibNames: TStrings;
  begin
    LibNames := TStringList.Create;
    try
      Token := NextTokenExpected([tkString]);
      LibNames.Add(Token.Value);
      while PeekAtNextToken.Kind = tkComma do
      begin
        NextToken;
        Token := NextTokenExpected([tkString]);
        LibNames.Add(Token.Value);
      end;

      for LibName in LibNames do
      begin
        if DynlibMap.{$ifdef SE_MAP_AVK959}Contains{$else}ContainsKey{$endif}(LibName) then
          Lib := DynlibMap[LibName]
        else
        begin
          {$ifdef SE_LOG}
          Writeln('Trying to load dynamic library "', LibName ,'"');
          if FileExists(LibName) then
            Writeln(' - Found the library in root directory')
          else
            Writeln(' - The library not exists in root directory');
          {$endif}
          Lib := LoadLibrary(LibName);
          if Lib <> 0 then
            DynlibMap.Add(LibName, Lib);
          {$ifdef SE_LOG}
          Writeln(' - Library''s pointer: ', QWord(Lib));
          {$endif}
        end;
        if Lib <> 0 then
        begin
          Break;
        end;
      end;
    finally
      LibNames.Free;
    end;
    if PeekAtNextToken.Kind <> tkBegin then
      FuncImport(LibName)
    else
    begin
      NextToken;
      while True do
      begin
        FuncImport(LibName);
        if PeekAtNextTokenExpected([tkEnd, tkFunctionDecl]).Kind = tkEnd then
        begin
          NextToken;
          break;
        end;
      end;
    end;
  end;

  procedure ParseWhile;
  var
    StartBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
    IsComparison: Boolean = True;
    OpCount: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);
      StartBlock := Self.Binary.Count;
      if IsComparison then
      begin
        OpCount := Self.OpcodeInfoList.Count;
        ParseExpr;
        if (Self.OptimizePeephole) and
           ((Self.OpcodeInfoList.Count - OpCount) = 1) and
           (Self.OpcodeInfoList[OpCount].Op = opPushConst) and
           (Self.Binary[Self.OpcodeInfoList[OpCount].Pos + 1].VarNumber <> 0) then
        begin
          Self.Binary.DeleteRange(Self.Binary.Count - 2, 2);
          Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
          IsComparison := False;
        end else
        begin
          JumpEnd := Emit([Pointer(opJumpEqual1), False, Pointer(0)]);
        end;
      end;
      ParseBlock;
      JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(StartBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      if IsComparison then
        Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

  procedure ParseDoWhile;
  var
    StartBlock,
    ContinueBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
    IsComparison: Boolean = True;
    OpCount: Integer;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);
      StartBlock := Self.Binary.Count;
      ParseBlock;
      ContinueBlock := Self.Binary.Count;
      NextTokenExpected([tkWhile]);
      if IsComparison then
      begin
        OpCount := Self.OpcodeInfoList.Count;
        ParseExpr;
        if (Self.OptimizePeephole) and
           ((Self.OpcodeInfoList.Count - OpCount) = 1) and
           (Self.OpcodeInfoList[OpCount].Op = opPushConst) and
           (Self.Binary[Self.OpcodeInfoList[OpCount].Pos + 1].VarNumber <> 0) then
        begin
          Self.Binary.DeleteRange(Self.Binary.Count - 2, 2);
          Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
          IsComparison := False;
        end else
        begin
          JumpEnd := Emit([Pointer(opJumpEqual1), False, Pointer(0)]);
        end;
      end;
      JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(ContinueBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      if IsComparison then
        Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

 procedure ParseFor;
  var
    StartBlock,
    ContinueBlock,
    EndBlock,
    JumpBlock,
    JumpEnd: Integer;
    BreakList,
    ContinueList: TList;
    I: Integer;
    Token: TSEToken;
    VarIdent,
    VarHiddenTargetIdent,
    VarHiddenCountIdent,
    VarHiddenArrayIdent: TSEIdent;
    VarHiddenTargetName,
    VarHiddenCountName,
    VarHiddenArrayName: String;
    Ind: Integer;
    Step: Single = 1;
  begin
    ContinueList := TList.Create;
    BreakList := TList.Create;
    try
      ContinueStack.Push(ContinueList);
      BreakStack.Push(BreakList);

      Token := NextTokenExpected([tkVariable, tkIdent]);
      // FIXME: tkVariable?
      if Token.Kind = tkIdent then
      begin
        VarIdent := CreateIdent(ikVariable, Token, True, False);
      end else
      begin
        VarIdent := FindVar(Token.Value)^;
      end;
      Token := NextTokenExpected([tkEqual, tkIn, tkComma]);

      VarHiddenTargetName := '___t' + VarIdent.Name;
      Token.Value := VarHiddenTargetName;
      VarHiddenTargetIdent := CreateIdent(ikVariable, Token, True, False);

      if Token.Kind = tkEqual then
      begin

        ParseExpr;
        EmitAssignVar(VarIdent);

        Token := NextTokenExpected([tkTo, tkDownto]);

        ParseExpr;

        if PeekAtNextToken.Kind = tkStep then
        begin
          NextToken;
          Step := PointStrToFloat(NextTokenExpected([tkNumber]).Value);
        end;

        if Token.Kind = tkDownto then
        begin
          Step := -Step;
        end;
        Emit([Pointer(opOperatorAdd0), Step]);
        EmitAssignVar(VarHiddenTargetIdent);

        StartBlock := Self.Binary.Count;
        //EmitPushVar(VarIdent);
        //EmitPushVar(VarHiddenTargetIdent);
        if Token.Kind = tkTo then
        begin
          JumpEnd := Emit([Pointer(opJumpEqualOrGreater2), Pointer(VarIdent.Addr), GetIdentLocalValue(VarIdent), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(0)]);
        end else
        if Token.Kind = tkDownto then
        begin
          JumpEnd := Emit([Pointer(opJumpEqualOrLesser2), Pointer(VarIdent.Addr), GetIdentLocalValue(VarIdent), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(0)]);
        end;

        ParseBlock;

        ContinueBlock := Self.Binary.Count;
        Emit([Pointer(opOperatorInc), Pointer(VarIdent.Addr), GetVarFrame(VarIdent), Step]);
        JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        EndBLock := JumpBlock;
      end else
      begin
        if Token.Kind = tkComma then
        begin
          Token := NextTokenExpected([tkIdent]);
          VarHiddenCountName := Token.Value;
          NextTokenExpected([tkIn]);
        end else
          VarHiddenCountName := '___c' + VarIdent.Name;
        VarHiddenArrayName := '___a' + VarIdent.Name;
        Token.Value := VarHiddenCountName;
        VarHiddenCountIdent := CreateIdent(ikVariable, Token, True, False);
        Token.Value := VarHiddenArrayName;
        VarHiddenArrayIdent := CreateIdent(ikVariable, Token, True, False);

        ParseExpr;

        EmitAssignVar(VarHiddenArrayIdent);
        Emit([Pointer(opPushConst), 0]);
        EmitAssignVar(VarHiddenCountIdent);

        EmitPushVar(VarHiddenArrayIdent);
        FindFuncNative('length', Ind);
        Emit([Pointer(opCallNative), Pointer(Ind), Pointer(1), Pointer(0)]);
        EmitAssignVar(VarHiddenTargetIdent);

        StartBlock := Self.Binary.Count;
        //EmitPushVar(VarHiddenTargetIdent);
        //EmitPushVar(VarHiddenCountIdent);
        JumpEnd := Emit([Pointer(opJumpEqualOrLesser2), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(VarHiddenCountIdent.Addr), GetIdentLocalValue(VarHiddenCountIdent), Pointer(0)]);

        EmitPushVar(VarHiddenArrayIdent);
        EmitPushVar(VarHiddenCountIdent);
        Emit([Pointer(opPushArrayPop), SENull]);
        PeepholeArrayAssignOptimization;
        EmitAssignVar(VarIdent);

        ParseBlock;

        ContinueBlock := Self.Binary.Count;
        Emit([Pointer(opOperatorInc), Pointer(VarHiddenCountIdent.Addr), GetVarFrame(VarHiddenCountIdent), 1]);
        JumpBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        EndBLock := JumpBlock;
      end;

      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(Integer(ContinueList[I]), Pointer(ContinueBlock));
      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
      Patch(JumpBlock - 1, Pointer(StartBlock));
      Patch(JumpEnd - 1, Pointer(EndBlock));
    finally
      ContinueList.Free;
      BreakList.Free;
    end;
  end;

  procedure ParseIf;
  var
    StartBlock1,
    StartBlock2,
    EndBlock2,
    JumpBlock1,
    JumpBlock2,
    JumpEnd: Integer;
  begin
    ParseExpr;
    JumpBlock1 := Emit([Pointer(opJumpEqual1), True, Pointer(0)]);
    JumpBlock2 := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
    StartBlock1 := Self.Binary.Count;
    ParseBlock;
    JumpEnd := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
    StartBlock2 := Self.Binary.Count;
    if PeekAtNextToken.Kind = tkElse then
    begin
      NextToken;
      ParseBlock;
    end;
    EndBlock2 := Self.Binary.Count;
    Patch(JumpBlock1 - 1, Pointer(StartBlock1));
    Patch(JumpBlock2 - 1, Pointer(StartBlock2));
    Patch(JumpEnd - 1, Pointer(EndBlock2));
  end;

  procedure ParseSwitch;
  var
    Token: TSEToken;
    VarHiddenIdent: TSEIdent;
    BreakList: TList;
    JumpBlock1,
    JumpBlock2,
    StartCaseBlock,
    EndCaseBlock,
    JumpNextBlock,
    EndBlock,
    I: Integer;
  begin
    Token.Kind := tkIdent;
    Token.Value := '___s' + Self.InternalIdent;
    VarHiddenIdent := CreateIdent(ikVariable, Token, True, False);

    ParseExpr;
    EmitAssignVar(VarHiddenIdent);

    NextTokenExpected([tkBegin]);
    BreakList := TList.Create;
    JumpNextBlock := -1;
    try
      BreakStack.Push(BreakList);

      while PeekAtNextToken.Kind in [tkCase, tkDefault] do
      begin
        Token := NextToken;
        if Token.Kind = tkCase then
        begin
          ParseExpr;
          EmitPushVar(VarHiddenIdent);
          JumpBlock1 := Emit([Pointer(opJumpEqual), Pointer(0)]);
          JumpBlock2 := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
        end;
        StartCaseBlock := Self.Binary.Count;
        if JumpNextBlock <> -1 then
        begin
          Patch(JumpNextBlock - 1, Pointer(StartCaseBlock));
          JumpNextBlock := -1;
        end;
        PeekAtNextTokenExpected([tkColon]);
        ParseBlock(True);
        if Token.Kind = tkCase then
        begin
          JumpNextBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);
          EndCaseBlock := Self.Binary.Count;
          Patch(JumpBlock1 - 1, Pointer(StartCaseBlock));
          Patch(JumpBlock2 - 1, Pointer(EndCaseBlock));
        end else
          Break;
      end;
      NextTokenExpected([tkEnd]);
      EndBlock := Self.Binary.Count;

      BreakList := BreakStack.Pop;

      for I := 0 to BreakList.Count - 1 do
        Patch(Integer(BreakList[I]), Pointer(EndBlock));
    finally
      BreakList.Free;
    end;
  end;

  procedure ParseArrayAssign;
  var
    FuncNativeInfo: PSEFuncNativeInfo;
    I, Ind: Integer;
    ArgCount: Integer = 0;
    Token: TSEToken;
  begin
    I := 0;
    FuncNativeInfo := FindFuncNative('___map_create', Ind);
    repeat
      if PeekAtNextToken.Kind <> tkSquareBracketClose then
      begin
        if ((PeekAtNextToken.Kind = tkIdent) or (PeekAtNextToken.Kind = tkString)) and (PeekAtNextNextToken.Kind = tkColon) then
        begin
          Token := NextToken;
          Emit([Pointer(opPushConst), Token.Value]);
          NextToken;
          ParseExpr;
          Inc(ArgCount, 2);
        end else
        begin
          Emit([Pointer(opPushConst), I]);
          ParseExpr;
          Inc(ArgCount, 2);
          Inc(I);
        end;
      end;
      Token := NextTokenExpected([tkComma, tkSquareBracketClose]);
    until Token.Kind = tkSquareBracketClose;
    Emit([Pointer(opCallNative), Pointer(Ind), Pointer(ArgCount), Pointer(0)]);
  end;

  procedure ParseAssignTail;
  var
    Token, FuncRefToken: TSEToken;
    FuncRefIdent: TSEIdent;
  begin
    while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
    begin
      if FuncRefToken.Value = '' then
      begin
        FuncRefToken.Value := '___f' + Self.InternalIdent;
        FuncRefToken.Kind := tkIdent;
        FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True, False);
      end;
      EmitAssignVar(FuncRefIdent);
      EmitPushVar(FuncRefIdent, True);
      while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
      begin
        case PeekAtNextToken.Kind of
          tkSquareBracketOpen:
            begin
              NextToken;
              ParseExpr;
              NextTokenExpected([tkSquareBracketClose]);
              EmitAssignVar(FuncRefIdent);
              EmitPushVar(FuncRefIdent, True);
              Emit([Pointer(opPushArrayPop), SENull]);
              PeepholeArrayAssignOptimization;
            end;
          tkDot:
            begin
              NextToken;
              Token := NextTokenExpected([tkIdent]);
              EmitAssignVar(FuncRefIdent);
              EmitPushVar(FuncRefIdent, True);
              if Length(Token.Value) <= 8 then
                Emit([Pointer(opPushArrayPop), CreatePackedString(Token.Value)])
              else
                Emit([Pointer(opPushArrayPopString), Pointer(CreateConstString(Token.Value))]);
            end;
        end;
      end;
      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
        ParseFuncRefCall(@FuncRefIdent);
      end;
    end;
    Emit([Pointer(opPopConst)]);
  end;

  procedure ParseVarAssign(const Name: String; const IsNew, IsAccess: Boolean);
  var
    Ident: PSEIdent;
    Token, Token2: TSEToken;
    ArgCount: Integer = 0;
    I, J,
    RewindStartAddr,
    OpBinaryStart,
    OpBinaryEnd,
    VarStartTokenPos,
    VarEndTokenPos: Integer;
    AccessNumber: TSEValue;
    AccessString: String;
    OpInfoPrev1: PSEOpcodeInfo;
  begin
    Ident := FindVar(Name);
    if Ident^.IsAssigned and Ident^.IsConst then
      Error(Format('Cannot reassign value to constant "%s"', [Name]), PeekAtNextToken);
      RewindStartAddr := Self.Binary.Count;
    VarStartTokenPos := Pos;
    if not IsAccess then
    begin
      while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
      begin
        if IsNew then
          Error(Format('Variable "%s" is not an array / a map', [Name]), PeekAtNextToken);
        case PeekAtNextToken.Kind of
          tkSquareBracketOpen:
            begin
              NextToken;
              ParseExpr;
              NextTokenExpected([tkSquareBracketClose]);
            end;
          tkDot:
            begin
              NextToken;
              Token2 := NextTokenExpected([tkIdent]);
              if Length(Token2.Value) <= 8 then
                Emit([Pointer(opPushConst), CreatePackedString(Token2.Value)])
              else
                EmitConstString(Token2.Value);
            end;
        end;
        Inc(ArgCount);
      end;
    end else
    begin
      PeekAtNextTokenExpected([tkSquareBracketOpen, tkDot]);
      if PeekAtNextToken.Kind = tkSquareBracketOpen then
      begin
        AccessNumber := SENull;
        NextToken;
        OpBinaryStart := Self.OpcodeInfoList.Count;
        ParseExpr;
        OpBinaryEnd := Self.OpcodeInfoList.Count;
        if (OpBinaryEnd - OpBinaryStart) = 1 then
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
          if (OpInfoPrev1 <> nil) then
          begin
            if (OpInfoPrev1^.Binary <> Pointer(Self.Binary)) then
              Exit;
            AccessNumber := Self.Binary[OpInfoPrev1^.Pos + 1];
            Self.Binary.DeleteRange(Self.Binary.Count - OpInfoPrev1^.Size, OpInfoPrev1^.Size);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 1, 1);
          end;
        end;
        NextTokenExpected([tkSquareBracketClose]);
      end else
      begin
        NextToken;
        AccessString := NextTokenExpected([tkIdent]).Value;
      end;
    end;

    Token := PeekAtNextTokenExpected([tkEqual, tkOpAssign, tkBracketOpen]);
    case Token.Kind of
      tkEqual,
      tkOpAssign:
        begin
          VarEndTokenPos := Pos;
          NextToken;
          if Token.Kind = tkOpAssign then
          begin
            if (ArgCount > 0) or IsAccess then
            begin
              J := Pos + 1;
              for I := VarStartTokenPos to VarEndTokenPos do
              begin
                Self.TokenList.Insert(J, Self.TokenList[I]);
                Inc(J);
              end;
              ParseExpr;
            end else
              EmitPushVar(Ident^);
          end;
          ParseExpr;
          if Token.Kind = tkOpAssign then
          begin
            case Token.Value of
              '+':
                if not PeepholeOpXOptimization(opOperatorAdd) then
                  Emit([Pointer(opOperatorAdd)]);
              '-':
                if not PeepholeOpXOptimization(opOperatorSub) then
                  Emit([Pointer(opOperatorSub)]);
              '*':
                if not PeepholeOpXOptimization(opOperatorMul) then
                  Emit([Pointer(opOperatorMul)]);
              '/':
                if not PeepholeOpXOptimization(opOperatorDiv) then
                  Emit([Pointer(opOperatorDiv)]);
            end;
          end;
          if IsAccess then
          begin
            if AccessString = '' then
              EmitAssignArrayFast(Ident^, AccessNumber)
            else
              EmitAssignMapFast(Ident^, AccessString);
          end else
          begin
            if ArgCount > 0 then
              EmitAssignArray(Ident^, ArgCount)
            else
            begin
              EmitAssignVar(Ident^);
              PeepholeIncOptimization;
            end;
          end;
        end;
      tkBracketOpen:
        begin
          if IsNew or IsAccess then
            Error(Format('Variable "%s" is not a function', [Name]), PeekAtNextToken);
          ParseFuncRefCallByMapRewind(Ident^, ArgCount, RewindStartAddr, Ident);
          ParseAssignTail;
        end;
    end;
    Ident^.IsAssigned := True;
  end;

  procedure ParseTrap;
  var
    Token: TSEToken;
    VarIdent: TSEIdent;
    PVarIdent: PSEIdent;
    I,
    JumpCatchBlock,
    CatchBlock,
    JumpFinallyBlock: Integer;
  begin
    JumpCatchBlock := Emit([Pointer(opPushTrap), Pointer(0)]);
    ParseBlock;
    Emit([Pointer(opPopTrap)]);
    JumpFinallyBlock := Emit([Pointer(opJumpUnconditional), Pointer(0)]);

    Self.ScopeStack.Push(Self.VarList.Count);
    CatchBlock := Self.Binary.Count;
    NextTokenExpected([tkCatch]);
    NextTokenExpected([tkBracketOpen]);
    Token := NextTokenExpected([tkIdent]);
    PVarIdent := FindVar(Token.Value);
    if PVarIdent = nil then
    begin
      VarIdent := CreateIdent(ikVariable, Token, True, False);
      EmitAssignVar(VarIdent);
    end else
      EmitAssignVar(PVarIdent^);
    NextTokenExpected([tkBracketClose]);
    ParseBlock;

    Patch(JumpCatchBlock - 1, Pointer(CatchBlock));
    Patch(JumpFinallyBlock - 1, Pointer(Self.Binary.Count));
    I := Self.ScopeStack.Pop;
    Self.VarList.DeleteRange(I, Self.VarList.Count - I);
  end;

  procedure ParseThrow;
  begin
    ParseExpr;
    Emit([Pointer(opThrow)]);
  end;

  procedure ParseIdent(const Token: TSEToken; const IsConst, IsLocal, IsAccess: Boolean);
  var
    OpCountBefore,
    OpCountAfter: Integer;
    Ident: TSEIdent;
  begin
    case IdentifyIdent(Token.Value, IsLocal) of
      tkUnknown:
        begin
          NextToken;
          CreateIdent(ikVariable, Token, False, IsConst);
          OpCountBefore := Self.OpcodeInfoList.Count;
          ParseVarAssign(Token.Value, True, False);
          OpCountAfter := Self.OpcodeInfoList.Count;
          if (IsConst) and
            (Self.OptimizePeephole) and
            ((OpCountAfter - OpCountBefore) = 2) and
            (Self.OpcodeInfoList[OpCountAfter - 2].Op = opPushConst) and
            ((Self.OpcodeInfoList[OpCountAfter - 1].Op = opAssignLocalVar) or (Self.OpcodeInfoList[OpCountAfter - 1].Op = opAssignGlobalVar)) and
            (Self.Binary[Self.OpcodeInfoList[OpCountAfter - 2].Pos + 1].Kind = sevkNumber) then
          begin
            Ident := Self.VarList[Self.VarList.Count - 1];
            Ident.ConstValue := Self.Binary[Self.OpcodeInfoList[OpCountAfter - 2].Pos + 1];
            Self.VarList[Self.VarList.Count - 1] := Ident;
            if Self.OpcodeInfoList[OpCountAfter - 1].Op = opAssignLocalVar then
              Self.Binary.DeleteRange(Self.Binary.Count - 5, 5)
            else
              Self.Binary.DeleteRange(Self.Binary.Count - 4, 4);
            Self.OpcodeInfoList.DeleteRange(Self.OpcodeInfoList.Count - 2, 2);
          end;
        end;
      tkVariable:
        begin
          NextToken;
          if not IsAccess then
          begin
            if PeekAtNextToken.Kind = tkBracketOpen then // Likely function ref
            begin
              ParseFuncRefCallByName(Token.Value);
              ParseAssignTail;
            end else
              ParseVarAssign(Token.Value, False, False);
          end else
          begin
            ParseVarAssign(Token.Value, False, True);
          end;
        end;
      tkFunction:
        begin
          if Self.OptimizeAsserts and (Token.Value = 'assert') then
            CanEmit := False;
          NextToken;
          ParseFuncCall(Token.Value);
          ParseAssignTail;
          if Self.OptimizeAsserts and (Token.Value = 'assert') then
            CanEmit := True;
        end;
      else
        Error('Invalid statement', Token);
    end;
  end;

  procedure ParseBlock(const IsCase: Boolean = False);
  var
    IsConst: Boolean = False;
    Token: TSEToken;
    Ident: TSEIdent;
    List: TList;
    I, J: Integer;
  begin
    Inc(Self.BlockTraversal);
    Token := PeekAtNextToken;
    case Token.Kind of
      tkConst:
        begin
          NextToken;
          Token := PeekAtNextTokenExpected([tkIdent]);
          ParseIdent(Token, True, False, False);
        end;
      tkLocal:
        begin
          NextToken;
          if PeekAtNextToken.Kind = tkConst then
          begin
            IsConst := True;
            NextToken;
          end;
          Token := PeekAtNextTokenExpected([tkIdent]);
          ParseIdent(Token, IsConst, True, False);
        end;
      tkIf:
        begin
          NextToken;
          ParseIf;
        end;
      tkFor:
        begin
          NextToken;
          ParseFor;
        end;
      tkDo:
        begin
          NextToken;
          ParseDoWhile;
        end;
      tkWhile:
        begin
          NextToken;
          ParseWhile;
        end;
      tkSwitch:
        begin
          NextToken;
          ParseSwitch;
        end;
      tkBreak:
        begin
          NextToken;
          if BreakStack.Count = 0 then
            Error('Not in loop but "break" found', Token);
          List := BreakStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), Pointer(0)]) - 1));
        end;
      tkContinue:
        begin
          NextToken;
          if ContinueStack.Count = 0 then
            Error('Not in loop but "continue" found', Token);
          List := ContinueStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditional), Pointer(0)]) - 1));
        end;
      tkReturn:
        begin
          NextToken;
          if PeekAtNextToken.Kind = tkBracketOpen then
          begin
            NextToken;
            Token.Kind := tkEqual;
            TokenList.Insert(Pos + 1, Token); // Insert equal token
            ParseVarAssign('result', False, False);
            NextTokenExpected([tkBracketClose]);
          end;
          if Self.FuncTraversal = 0 then
            Emit([Pointer(opHlt)])
          else
          begin
            Emit([Pointer(opPopFrame)])
          end;
        end;
      tkFunctionDecl:
        begin
          NextToken;
          Inc(Self.FuncTraversal);
          Self.LocalVarCountList.Add(-1);
          Self.ScopeStack.Push(Self.VarList.Count);
          Self.ScopeFunc.Push(Self.FuncScriptList.Count + 1);
          ParseFuncDecl;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
          I := Self.ScopeFunc.Pop;
          for J := I to Self.FuncScriptList.Count - 1 do
          begin
            if Self.FuncScriptList.Ptr(J)^.Name.IndexOf('___fn') <> 0 then
              Self.FuncScriptList.Ptr(J)^.Name := '';
          end;
          Self.LocalVarCountList.Delete(Self.LocalVarCountList.Count - 1);
          Dec(Self.FuncTraversal);
        end;
      tkYield:
        begin
          NextToken;
          if PeekAtNextToken.Kind = tkBracketOpen then
          begin
            NextToken;
            Token.Kind := tkEqual;
            TokenList.Insert(Pos + 1, Token); // Insert equal token
              ParseVarAssign('result', False, False);
            NextTokenExpected([tkBracketClose]);
          end;
          Emit([Pointer(opYield)]);
        end;
      tkColon:
        begin
          if not IsCase then
            Error('Invalid statement ' + TokenNames[Token.Kind], Token);
          Self.ScopeStack.Push(Self.VarList.Count);
          NextToken;
          Token := PeekAtNextToken;
          while not (Token.Kind in [tkEnd, tkCase, tkDefault]) do
          begin
            if Token.Kind = tkEOF then
              Error('Expected end, got EOF instead', Token);
            ParseBlock;
            Token := PeekAtNextToken;
          end;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
        end;
      tkBegin:
        begin
          Self.ScopeStack.Push(Self.VarList.Count);
          NextToken;
          Token := PeekAtNextToken;
          while Token.Kind <> tkEnd do
          begin
            if Token.Kind = tkEOF then
              Error('Expected end, got EOF instead', Token);
            ParseBlock;
            Token := PeekAtNextToken;
          end;
          I := Self.ScopeStack.Pop;
          Self.VarList.DeleteRange(I, Self.VarList.Count - I);
          NextToken;
        end;
      tkAccess:
        begin
          NextToken;
          Token := PeekAtNextTokenExpected([tkIdent]);
          ParseIdent(Token, False, False, True);
        end;
      tkIdent:
        begin
          ParseIdent(Token, False, False, False);
        end;
      tkImport:
        begin
          NextToken;
          ParseFuncImport;
        end;
      tkTry:
        begin
          NextToken;
          ParseTrap;
        end;
      tkThrow:
        begin
          NextToken;
          ParseThrow;
        end;
      tkEOF:
        Exit;
      else
        Error('Invalid statement ' + TokenNames[Token.Kind], Token);
    end;
    {$ifdef UNIX}
    Emit([Pointer(opBlockCleanup)]);
    {$endif}
    Dec(Self.BlockTraversal);
  end;

begin
  ContinueStack := TSEListStack.Create;
  BreakStack := TSEListStack.Create;
  ReturnStack := TSEListStack.Create;
  try
    Self.LocalVarCountList.Clear;
    Self.Binary := Self.VM.Binaries.Value^.Data[0];
    repeat
      ParseBlock;
    until PeekAtNextToken.Kind = tkEOF;
    Emit([Pointer(opHlt)]);
    Self.IsParsed := True;
  finally
    FreeAndNil(ContinueStack);
    FreeAndNil(BreakStack);
    FreeAndNil(ReturnStack);
  end;
end;

procedure TEvilC.Reset;
var
  Ident: TSEIdent;
  I: Integer;
begin
  Self.GlobalVarCount := 2;
  Self.GlobalVarSymbols.Clear;
  Self.GlobalVarSymbols.Add('result');
  Self.GlobalVarSymbols.Add('___result');
  for I := 0 to Self.FuncScriptList.Count - 1 do
    Self.FuncScriptList[I].VarSymbols.Free;
  Self.FuncScriptList.Count := 0;
  Self.FuncImportList.Count := 0;
  Self.CurrentFileList.Clear;
  Self.LocalVarCountList.Count := 0;
  Self.VM.Reset;

  Self.VM.BinaryClear;
  Self.VM.ConstStrings.Clear;
  Self.VM.IsDone := True;
  Self.Vm.IsPaused := False;
  Self.BinaryPos := 0;
  Self.IsDone := False;
  Self.IsParsed := False;
  Self.IsLex := False;
  Self.VarList.Count := 0;
  Self.TokenList.Count := 0;
  Self.OpcodeInfoList.Count := 0;
  Self.IncludeList.Clear;
  Self.ScopeFunc.Clear;
  Self.ScopeStack.Clear;
  Self.VarList.Count := Self.GlobalVarCount; // Safeguard
  Ident.Kind := ikVariable;
  Ident.Addr := 0;
  Ident.Name := 'result';
  Ident.Local := 0;
  Ident.ConstValue := False;
  Ident.IsUsed := False;
  Ident.IsAssigned := False;
  Self.VarList[0] := Ident;
  Ident.Name := '___result';
  Self.VarList[1] := Ident;
  ErrorLn := -1;
  ErrorCol := -1;
  Self.FuncTraversal := 0;
  Self.FuncCurrent := -1;

  // Implement assert function
  Self.RegisterScriptFunc('assert', 2);
  Self.Binary := Self.VM.Binaries.Value^.Data[1];
  Self.Binary.AddRange(FunctionAssert);
  // Implement ___throw function
  Self.RegisterScriptFunc('___throw', 1);
  Self.Binary := Self.VM.Binaries.Value^.Data[2];
  Self.Binary.AddRange(FunctionThrow);
end;

function TEvilC.Exec: TSEValue;
begin
  {$ifdef SE_PROFILER}
  FrameProfiler.Start('TEvilC.Exec');
  {$endif}
  try
    if not Self.IsLex then
      Self.Lex;
    if not Self.IsParsed then
    begin
      Self.Parse;
    end;
    Self.VM.Exec;
    Exit(Self.VM.Global.Value^.Data[0]);
  finally
    {$ifdef SE_PROFILER}
    FrameProfiler.Stop('TEvilC.Exec');
    {$endif}
  end;
end;

{
  StackPtr:
  - Return value (-1)
  - Parameters (0..X)
  - Variables (X+1..Y)
}
function TEvilC.ExecFuncOnly(const Name: String; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := Self.FuncScriptList.Count - 1 downto 0 do
  begin
    if Name = Self.FuncScriptList[I].Name then
    begin
      Exit(Self.ExecFuncOnly(I, Args));
    end;
  end;
  Exit(SENull);
end;

function TEvilC.ExecFuncOnly(const AIndex: Integer; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
  Stack: PSEValue;
  Func: PSEFuncScriptInfo;
begin
  {$ifdef SE_PROFILER}
  FrameProfiler.Start('TEvilC.ExecFunc');
  {$endif}
  try
    if not Self.IsLex then
      Self.Lex;
    if not Self.IsParsed then
    begin
      Self.Parse;
    end;
    Self.VM.CodePtr := 0;
    Self.VM.BinaryPtr := 0;
    Self.VM.IsPaused := False;
    Self.VM.IsDone := False;
    Self.VM.FramePtr := @Self.VM.Frame[0];
    Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
    Self.VM.FramePtr^.Stack := Self.VM.StackPtr;
    Self.VM.TrapPtr := @Self.VM.Trap[0];
    Dec(Self.VM.TrapPtr);
    Func := Self.FuncScriptList.Ptr(AIndex);
    Self.VM.BinaryPtr := Func^.BinaryPos;
    Self.VM.StackPtr := Self.VM.StackPtr + Func^.ArgCount + Func^.VarCount;
    if Self.VM.BinaryPtr <> 0 then
    begin
      Stack := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
      for I := 0 to Length(Args) - 1 do
      begin
        Stack[I] := Args[I];
      end;
      Self.VM.Exec;
      Exit(Stack[-1]);
    end else
      Exit(SENull);
  finally
    {$ifdef SE_PROFILER}
    FrameProfiler.Stop('TEvilC.ExecFunc');
    {$endif}
  end;
end;

function TEvilC.ExecFunc(const Name: String; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
begin
  for I := Self.FuncScriptList.Count - 1 downto 0 do
  begin
    if Name = Self.FuncScriptList[I].Name then
    begin
      Exit(Self.ExecFunc(I, Args));
    end;
  end;
  Exit(SENull);
end;

function TEvilC.ExecFunc(const AIndex: Integer; const Args: array of TSEValue): TSEValue;
var
  I: Integer;
  Stack: PSEValue;
  Func: PSEFuncScriptInfo;
  V: TSEValue;
begin
  {$ifdef SE_PROFILER}
  FrameProfiler.Start('TEvilC.ExecFunc');
  {$endif}
  try
    Result := SENull;
    if (not Self.VM.IsDone) and (Self.VM.IsPaused or Self.VM.IsYielded) then
    begin
      for V in Args do
        if V.Kind in [sevkMap, sevkString, sevkPascalObject, sevkBuffer] then
          GC.Managed(@V);
      Self.VM.Exec;
      for V in Args do
        if V.Kind in [sevkMap, sevkString, sevkPascalObject, sevkBuffer] then
          GC.UnManaged(@V);
      if Self.VM.IsDone then
      begin
        Stack := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
        Exit(Stack[-1]);
      end;
    end else
    begin
      Self.VM.CodePtr := 0;
      Self.VM.BinaryPtr := 0;
      Self.VM.IsPaused := False;
      Self.VM.IsDone := False;
      Self.VM.FramePtr := @Self.VM.Frame[0];
      Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
      Self.VM.FramePtr^.Stack := Self.VM.StackPtr;
      Self.VM.TrapPtr := @Self.VM.Trap[0];
      Dec(Self.VM.TrapPtr);
      Func := Self.FuncScriptList.Ptr(AIndex);
      Self.VM.BinaryPtr := Func^.BinaryPos;
      Self.VM.StackPtr := Self.VM.StackPtr + Func^.ArgCount + Func^.VarCount;
      if Self.VM.BinaryPtr <> 0 then
      begin
        Stack := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
        for I := 0 to Length(Args) - 1 do
        begin
          Stack[I] := Args[I];
        end;
        Self.VM.Exec;
        if Self.VM.IsDone then
        begin
          Exit(Stack[-1]);
        end;
      end;
    end;
  finally
    {$ifdef SE_PROFILER}
    FrameProfiler.Stop('TEvilC.ExecFunc');
    {$endif}
  end;
end;

procedure TEvilC.RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: Integer);
var
  FuncNativeInfo: TSEFuncNativeInfo;
begin
  FuncNativeInfo.ArgCount := ArgCount;
  FuncNativeInfo.Func := Func;
  FuncNativeInfo.Name := Name;
  FuncNativeInfo.Kind := sefnkNormal;
  Self.FuncNativeList.Add(FuncNativeInfo);
end;

procedure TEvilC.RegisterFuncWithSelf(const Name: String; const Func: TSEFuncWithSelf; const ArgCount: Integer);
var
  FuncNativeInfo: TSEFuncNativeInfo;
begin
  FuncNativeInfo.ArgCount := ArgCount;
  FuncNativeInfo.Func := TSEFunc(Func);
  FuncNativeInfo.Name := Name;
  FuncNativeInfo.Kind := sefnkSelf;
  Self.FuncNativeList.Add(FuncNativeInfo);
end;

function TEvilC.RegisterScriptFunc(const Name: String; const ArgCount: Integer): PSEFuncScriptInfo;
var
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  Self.VM.Binaries.Alloc(Self.VM.Binaries.Value^.Size + 1);
  Self.VM.Binaries.Value^.Data[Self.VM.Binaries.Value^.Size - 1] := TSEBinary.Create;
  Self.VM.Binaries.Value^.Data[Self.VM.Binaries.Value^.Size - 1].BinaryName := Name;
  FuncScriptInfo.ArgCount := ArgCount;
  FuncScriptInfo.BinaryPos := Self.VM.Binaries.Value^.Size - 1;
  FuncScriptInfo.Name := Name;
  FuncScriptInfo.VarSymbols := TStringList.Create;
  Self.FuncScriptList.Add(FuncScriptInfo);
  Result := Self.FuncScriptList.Ptr(Self.FuncScriptList.Count - 1);
  Self.FuncCurrent := Self.FuncScriptList.Count - 1;
end;

procedure TEvilC.RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind; const CC: TSECallingConvention = seccAuto);
var
  FuncImportInfo: TSEFuncImportInfo;
  Lib: TLibHandle;
begin
  if DynlibMap.{$ifdef SE_MAP_AVK959}Contains{$else}ContainsKey{$endif}(LibName) then
    Lib := DynlibMap[LibName]
  else
  begin
    {$ifdef SE_LOG}
    Writeln('Trying to load dynamic library "', LibName ,'"');
    if FileExists(LibName) then
      Writeln(' - Found the library in root directory')
    else
      Writeln(' - The library not exists in root directory');
    {$endif}
    Lib := LoadLibrary(LibName);
    DynlibMap.Add(LibName, Lib);
    {$ifdef SE_LOG}
    Writeln(' - Library''s pointer: ', QWord(Lib));
    {$endif}
  end;

  FuncImportInfo.Args := Args;
  FuncImportInfo.Return := Return;
  FuncImportInfo.Name := Name;
  FuncImportInfo.Func := nil;
  FuncImportInfo.CallingConvention := CC;
  if Lib <> 0 then
  begin
    FuncImportInfo.Func := GetProcAddress(Lib, ActualName);
  end;
  Self.FuncImportList.Add(FuncImportInfo);
end;

function TEvilC.Backup: TSECache;
var
  I, J: Integer;
  BackupBinary, SrcBinary: TSEBinary;
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  Result.LineOfCodeList := TSELineOfCodeList.Create;
  Result.FuncScriptList := TSEFuncScriptList.Create;
  Result.FuncImportList := TSEFuncImportList.Create;
  Result.GlobalVarSymbols := TStringList.Create;
  Result.ConstStrings := TStringList.Create;
  Result.SymbolList := TSESymbolList.Create;
  SetLength(Result.Binaries, Self.VM.Binaries.Value^.Size);
  for J := 0 to Self.VM.Binaries.Value^.Size - 1 do
  begin
    BackupBinary := TSEBinary.Create;
    Result.Binaries[J] := BackupBinary;
    SrcBinary := Self.VM.Binaries.Value^.Data[J];
    for I := 0 to SrcBinary.Count - 1 do
    begin
      BackupBinary.Add(SrcBinary[I]);
    end;
  end;
  for I := 0 to Self.LineOfCodeList.Count - 1 do
  begin
    Result.LineOfCodeList.Add(Self.LineOfCodeList[I]);
  end;
  for I := 0 to Self.FuncScriptList.Count - 1 do
  begin
    FuncScriptInfo := Self.FuncScriptList[I];
    FuncScriptInfo.VarSymbols := TStringList.Create;
    FuncScriptInfo.VarSymbols.Assign(Self.FuncScriptList[I].VarSymbols);
    Result.FuncScriptList.Add(FuncScriptInfo);
  end;
  for I := 0 to Self.FuncImportList.Count - 1 do
  begin
    Result.FuncImportList.Add(Self.FuncImportList[I]);
  end;
  for I := 0 to Self.VM.SymbolList.Count - 1 do
  begin
    Result.SymbolList.Add(Self.VM.SymbolList[I]);
  end;
  Result.GlobalVarSymbols.Assign(Self.GlobalVarSymbols);
  Result.GlobalVarCount := Self.GlobalVarCount;
  Result.ConstStrings.Assign(Self.VM.ConstStrings);
end;

procedure TEvilC.Restore(const Cache: TSECache);
var
  I, J: Integer;
  BackupBinary, DstBinary: TSEBinary;
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  Self.VM.BinaryClear;
  Self.VM.SymbolList.Count := 0;
  Self.LineOfCodeList.Count := 0;
  Self.GlobalVarSymbols.Clear;
  for I := 0 to Cache.LineOfCodeList.Count - 1 do
    Self.LineOfCodeList.Add(Cache.LineOfCodeList[I]);
  for I := 0 to Self.VM.Binaries.Value^.Size - 1 do
    Self.VM.Binaries.Value^.Data[I].Free;
  Self.VM.Binaries.Alloc(Length(Cache.Binaries));
  for I := 0 to High(Cache.Binaries) do
  begin
    BackupBinary := Cache.Binaries[I];
    DstBinary := TSEBinary.Create;
    Self.VM.Binaries.Value^.Data[I] := DstBinary;
    for J := 0 to BackupBinary.Count - 1 do
      DstBinary.Add(BackupBinary[J]);
  end;
  for I := 0 to Cache.FuncScriptList.Count - 1 do
  begin
    FuncScriptInfo := Cache.FuncScriptList[I];
    FuncScriptInfo.VarSymbols := TStringList.Create;
    FuncScriptInfo.VarSymbols.Assign(Cache.FuncScriptList[I].VarSymbols);
    Self.FuncScriptList.Add(FuncScriptInfo);
  end;
  for I := 0 to Cache.FuncImportList.Count - 1 do
    Self.FuncImportList.Add(Cache.FuncImportList[I]);
  for I := 0 to Cache.SymbolList.Count - 1 do
    Self.VM.SymbolList.Add(Cache.SymbolList[I]);
  Self.GlobalVarSymbols.Assign(Cache.GlobalVarSymbols);
  Self.GlobalVarCount := Cache.GlobalVarCount;
  Self.VM.ConstStrings.Assign(Cache.ConstStrings);
  Self.IsParsed := True;
end;

procedure TEvilC.PatchSymbols;
var
  I: Integer;
  P: PSESymbol;
  Bin: TSEBinary;
begin
  for I := 0 to Self.VM.SymbolList - 1 do
  begin
    P := Self.VM.SymbolList.Ptr(I);
    case P^.Kind of
      sesConst:
        begin
          Bin := Self.VM.Binaries.Value^.Data[P^.Binary];
          Bin[P^.Code] := Self.ConstMap[P^.Name];
        end;
    end;
  end;
end;

procedure TSECacheMap.ClearSingle(const AName: String);
var
  Cache: TSECache;
  I: Integer;
begin
  try
    Cache := Self[AName];
    for I := 0 to High(Cache.Binaries) do
      Cache.Binaries[I].Free;
    Cache.LineOfCodeList.Free;
    for I := 0 to Cache.FuncScriptList.Count - 1 do
      Cache.FuncScriptList[I].VarSymbols.Free;
    Cache.FuncScriptList.Free;
    Cache.FuncImportList.Free;
    Cache.GlobalVarSymbols.Free;
    Cache.ConstStrings.Free;
    Cache.SymbolList.Free;
    Self.Remove(AName);
  except
  end;
end;

procedure TSECacheMap.Clear;
var
  S: String;
  Cache: TSECache;
  I: Integer;
begin
  for S in Self.Keys do
  begin
    Cache := Self[S];
    for I := 0 to High(Cache.Binaries) do
      Cache.Binaries[I].Free;
    Cache.LineOfCodeList.Free;
    Cache.FuncScriptList.Free;
    Cache.FuncImportList.Free;
    Cache.GlobalVarSymbols.Free;
    Cache.ConstStrings.Free;
    Cache.SymbolList.Free;
  end;
  inherited;
end;

var
  I: Integer;

initialization
  CommonNativeFuncList := TSEFuncNativeList.Create;
  {$ifdef SE_THREADS}
  InitCriticalSection(CS);
  {$endif}
  FS := FormatSettings;
  FS.DecimalSeparator := '.';
  SENull.Kind := sevkNull;
  SENull.Ref := 0;
  SENull.VarNumber := Floor(0);
  DynlibMap := TDynlibMap.Create;
  GC := TSEGarbageCollector.Create;
  {$ifdef SE_THREADS}
  GCMarkJob := TSEGarbageCollectorMarkJob.Create;
  {$endif}
  ScriptCacheMap := TSECacheMap.Create;
  GC.AllocMap(@ScriptVarMap);
  IsThread := 0;
  FunctionAssert := [
    Pointer(opPushLocalVar), Pointer(0), Pointer(0),
    Pointer(opPushConst), false,
    Pointer(opOperatorEqual),
    Pointer(opJumpEqual1), true, Pointer(11),
    Pointer(opJumpUnconditional), Pointer(17),
    Pointer(opPushLocalVar), Pointer(1), Pointer(0),
    Pointer(opThrow),
    Pointer(opJumpUnconditional), Pointer(17),
    Pointer(opPopFrame)
  ];
  FunctionThrow := [
    Pointer(opPushLocalVar), Pointer(0), Pointer(0),
    Pointer(opThrow),
    Pointer(opPopFrame)
  ];

finalization
  if VMList <> nil then
  begin
    {$ifdef SE_THREADS}
    for I := VMList.Count - 1 downto 0 do
    begin
      if VMList[I].ThreadOwner <> nil then
      begin
        VMList[I].ThreadOwner.Terminate;
        VMList[I].ThreadOwner.WaitFor;
      end;
    end;
    {$endif}
    VMList.Free;
  end;
  VMList := nil;
  {$ifdef SE_THREADS}
  GCMarkJob.Terminate;
  GCMarkJob.Resume;
  {$endif}
  GC.Free;
  ScriptCacheMap.Free;
  DynlibMap.Free;
  {$ifdef SE_THREADS}
  DoneCriticalSection(CS);
  {$endif}
  CommonNativeFuncList.Free;

end.

