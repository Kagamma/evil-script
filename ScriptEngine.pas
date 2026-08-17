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
{$ifndef WASI}
  {$define SE_COMPUTED_GOTO}
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
{.$define SE_MAP_SHORTSTRING}
// enable this to replace FP's TDirectory with avk959's TGChainHashMap. It is a lot faster than TDirectory.
// requires https://github.com/avk959/LGenerics
// note: enable this will undef SE_MAP_SHORTSTRING, because this optimization is not necessary for TGChainHashMap
{$define SE_MAP_AVK959}
{$ifdef SE_MAP_AVK959}
  {$undef SE_MAP_SHORTSTRING}
{$endif}
// Enable this if you want multi-threading support
{$ifndef GO32v2}
  {$define SE_THREADS}
{$endif}
{$ifdef CPU64}
  {$align 16}
{$endif}
{$ifdef CPU32}
  {$align 4}
{$endif}
{$packenum 4}
{$optimization REGVAR}

interface

uses
  {$ifdef WINDOWS}
  Windows,
  {$endif}
  {$ifdef UNIX}
  BaseUnix, Unix,
  {$endif}
  SysUtils, Classes, Generics.Collections, StrUtils, Types, DateUtils, RegExpr, {$ifdef SE_THREADS}syncobjs,{$endif}
  contnrs, Rtti, TypInfo,
  {$ifdef SE_PROFILER}
  CastleTimeUtils,
  {$endif}
  {$ifdef SE_MAP_AVK959}
  lghashmap, lgHelpers, lgHash,
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
    opPushArrayPop,
    opPopConst,
    opPopFrame,
    opAssignGlobalVar,
    opAssignGlobalArray,
    opAssignLocalVar,
    opAssignLocalArray,
    opJumpEqualRel,
    opJumpEqual1Rel,
    opJumpUnconditionalRel,
    opJumpEqualOrGreater2Rel,
    opJumpEqualOrLesser2Rel,

    opOperatorInc,

    opOperatorAdd0,
    opOperatorMul0,
    opOperatorDiv0,

    opOperatorAdd1,
    opOperatorSub1,
    opOperatorMul1,
    opOperatorDiv1,

    opOperatorAdd,
    opOperatorSub,
    opOperatorMul,
    opOperatorDiv,
    opOperatorMod,
    opOperatorNegative,

    opOperatorLesser0,
    opOperatorLesserOrEqual0,
    opOperatorGreater0,
    opOperatorGreaterOrEqual0,
    opOperatorEqual0,
    opOperatorNotEqual0,
    opOperatorAnd0,
    opOperatorOr0,

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
    opPushConstFromConstList,

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
    opThrow,

    opJITBlock,
    opJITBlockPotential
  );
  TSEOpcodeSet = set of TSEOpcode;
  TSEOpcodeInfo = record
    Op: TSEOpcode;
    Pos: NativeInt;
    Binary: Pointer;
    Size: NativeInt;
  end;
  PSEOpcodeInfo = ^TSEOpcodeInfo;

  generic TSEListPtr<TT> = class(specialize TList<TT>)
  public
    type
      PTT = ^TT;
    function Ptr(const Index: SizeInt): PTT; inline;
  end;

  TSEOpcodeList = class(specialize TSEListPtr<TSEOpcode>);
  TSEOpcodeInfoList = class(specialize TSEListPtr<TSEOpcodeInfo>);

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
    sevkConstString
  );
  TSEValueKindSet = set of TSEValueKind;
  PSECommonString = ^RawByteString;
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
  TSEScopeStack = specialize TStack<NativeInt>;
  TSEIntegerList = specialize TList<NativeInt>;
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
      sevkConstString:
        (
          VarConstStringIndex: Cardinal;
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
    procedure AllocBuffer(constref Size: NativeInt); inline;
    procedure AllocMap; inline;
    procedure AllocString(const S: String); inline;
    procedure AllocPascalObject(const Obj: TObject; const IsManaged: Boolean); inline;
    function GetValue(constref I: NativeInt): TSEValue; inline; overload;
    function GetValue(constref S: String): TSEValue; inline; overload;
    function GetValue(constref I: TSEValue): TSEValue; inline; overload;
    procedure SetValue(constref I: NativeInt; const A: TSEValue); inline; overload;
    procedure SetValue(constref S: String; const A: TSEValue); inline; overload;
    procedure SetValue(I: TSEValue; const A: TSEValue); inline; overload;
    function GetProp(I: TSEValue): TSEValue;
    procedure SetProp(I: TSEValue; const A: TSEValue);
    function Invoke(constref MethodName: String; const Args: PSEValue; const ArgCount: NativeInt): TSEValue;
    function ContainsKey(constref S: String): Boolean; inline; overload;
    procedure UnManaged; inline;
    procedure Managed; inline;
    function Clone: TSEValue; inline;
    function IsValidArray: Boolean; inline;
    procedure FromJSON(constref S: String);
    function ToJSON: String;
    function ToString: String;
    function Size: SizeInt;
  end;

  {$ifdef SE_MAP_AVK959}
    {$define TSEDictionary := TGChainHashMap}
    TSEStringEq = class
    public
      class function HashCode(const AKey: String): SizeInt; static; inline;
      class function Equal(const L, R: String): Boolean; static; inline;
    end;
    TSEValueDict = specialize TGLiteChainHashMap<String, TSEValue, TSEStringEq>.TMap;
  {$else}
    {$define TSEDictionary := TDictionary}
    TSEValueDict = specialize TSEDictionary<{$ifdef SE_MAP_SHORTSTRING}ShortString{$else}String{$endif}, TSEValue>;
  {$endif}
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
    procedure Set2(const Key: PString; constref AValue: TSEValue); overload; inline;
    procedure Set2(const Index: SizeInt; constref AValue: TSEValue); overload; inline;
    function Get2(const Key: PString): TSEValue; overload; inline;
    function Get2(const Index: SizeInt): TSEValue; overload; inline;
    procedure Del2(const Key: PString); overload; inline;
    procedure Del2(const Index: SizeInt); overload; inline;
    function Ptr(const I: NativeInt): PSEValue;
    property Map: TSEValueDict read FMap;
    property IsValidArray: Boolean read FIsValidArray;
  end;
  TSEValueArray = array of TSEValue;
  PPSEValue = ^PSEValue;

  TSEValueList = specialize TSEListPtr<TSEValue>;
  TSEJITBlock = record
    Code: Pointer;
    CodeSize: NativeUInt;
  end;
  TSEJITBlockList = specialize TSEListPtr<TSEJITBlock>;

  TSEBinary = class(TSEValueList)
  public
    BinaryName: String;
    constructor Create;
    destructor Destroy; override;
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
  TSEGCNodeList = class(specialize TSEListPtr<TSEGCNode>);
  TSEGCNodeAvailStack = specialize TStack<NativeInt>;

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
    FTicks: NativeUInt;
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
    procedure AllocBuffer(const PValue: PSEValue; const Size: NativeInt);
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

  TSEFunc = function(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue of object;

  TSEFuncNativeInfo = record
    PossibleKinds: TSEValueKindSet;
    Name: String;
    Func: TSEFunc;
    ArgCount: NativeInt;
  end;
  PSEFuncNativeInfo = ^TSEFuncNativeInfo;

  TSEFuncScriptInfo = record
    PossibleKinds: TSEValueKindSet;
    Name: String;
    CodeSegmentIndex: NativeInt;
    ArgCount: NativeInt;
    VarCount: NativeInt;
    VarSymbols: TStrings;
    HasSelf: Boolean;
    HasOverride: Boolean;
  end;
  PSEFuncScriptInfo = ^TSEFuncScriptInfo;

  TSEFuncImportInfo = record
    PossibleKinds: TSEValueKindSet;
    Name: String;
    Func: Pointer;
    Args: TSEAtomKindArray;
    Return: TSEAtomKind;
    CallingConvention: TSECallingConvention;
  end;
  PSEFuncImportInfo = ^TSEFuncImportInfo;

  TSEStringLookupMap = specialize TSEDictionary<String, Cardinal>;
  TSEStringList = class(specialize TSEListPtr<RawByteString>);
  TSEFuncNativeList = class(specialize TSEListPtr<TSEFuncNativeInfo>);
  TSEFuncScriptList = class(specialize TSEListPtr<TSEFuncScriptInfo>);
  TSEFuncImportList = class(specialize TSEListPtr<TSEFuncImportInfo>);

  TSELineOfCode = record
    CodeIndex: NativeInt;
    CodeSegmentIndex: NativeInt;
    Line: NativeInt;
    Module: String;
  end;
  TSELineOfCodeList = specialize TList<TSELineOfCode>;

  TSEConstLookup = specialize TSEDictionary<String, NativeInt>;
  TSEStack = TSEValueList;
  TSEVarMap = TSEValue;
  TSEFrame = record
    CodePtr: PSEValue;
    StackPtr: PSEValue;
    CodeSegmentIndex: NativeInt;
    Func: PSEFuncScriptInfo;
  end;
  PSEFrame = ^TSEFrame;
  TSETrap = record
    FramePtr: PSEFrame;
    StackPtr: PSEValue;
    CodeSegmentIndex: NativeInt;
    CatchCodeIndex: NativeInt;
  end;
  PSETrap = ^TSETrap;

  PSEValueArrayManagedRecord = ^TSEValueArrayManagedRecord;
  TSEValueArrayManagedRecord = record
    Data: PSEValue;
    Size: Cardinal;
    RefCount: NativeInt;
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
    RefCount: NativeInt;
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
    FBinaryPtr: NativeInt;
    IsDone: Boolean;
    IsExecuting: Boolean;
    IsTerminated: Boolean;
    VM: TSEVM;
    constructor Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
    destructor Destroy; override;
    function Execute: TSEValue;
    procedure Reset(const Fn: TSEValue; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue);
  end;
  TSEVMCoroutineList = specialize TList<TSEVMCoroutine>;

  TSEJITBlockSignatureStack = specialize TStack<NativeInt>;

  TEvilC = class;
  TSEVM = class
  private
    JITBlockList: TSEJITBlockList;
  public
    Name: String;
    Owner: TSEVM;
    {$ifdef SE_THREADS}
    ThreadOwner: TSEVMThread;
    {$endif}
    CoroutineOwner: TSEVMCoroutine;
    EnableJIT: Boolean;
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
    CodePtr: PSEValue;
    StackPtr: PSEValue;
    CodeSegmentIndex: NativeInt;
    FramePtr: PSEFrame;
    TrapPtr: PSETrap;
    StackSize: NativeInt;
    FrameSize: NativeInt;
    TrapSize: NativeInt;
    Parent: TEvilC;
    Binaries: TSEBinariesManaged;

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
    tkVar,
    tkTry,
    tkCatch,
    tkThrow,
    tkOverride
  );
TSETokenKindSet = set of TSETokenKind;

const
  TokenNames: array[TSETokenKind] of RawByteString = (
    'EOF', '.', '+', '-', '*', 'div', 'mod', '^', '<<', '>>', 'operator assign', '=', '!=', '<',
    '>', '<=', '>=', '{', '}', ':', '?', '(', ')', 'neg', 'number', 'string',
    ',', 'if', 'switch', 'case', 'default', 'identity', 'function', 'fn', 'variable', 'const', 'local',
    'unknown', 'else', 'while', 'break', 'continue', 'yield',
    '[', ']', 'and', 'or', 'xor', 'not', 'for', 'in', 'to', 'downto', 'step', 'return',
    'atom', 'import', 'do', 'var', 'try', 'catch', 'throw', 'override'
  );
  ValueKindNames: array[TSEValueKind] of RawByteString = (
    'null', 'number', 'string', 'map', 'buffer', 'pointer', 'boolean', 'function', 'pasobject', 'packedstring'
  );
  OpcodeSizes: array[TSEOpcode] of Byte = (
    2, // opPushConst,
    2, // opPushConstString,
    2, // opPushGlobalVar,
    3, // opPushLocalVar,
    2, // opPushArrayPop,
    1, // opPopConst,
    1, // opPopFrame,
    2, // opAssignGlobalVar,
    3, // opAssignGlobalArray,
    3, // opAssignLocalVar,
    4, // opAssignLocalArray,
    2, // opJumpEqualRel,
    3, // opJumpEqual1Rel,
    2, // opJumpUnconditionalRel,
    6, // opJumpEqualOrGreater2Rel,
    6, // opJumpEqualOrLesser2Rel,

    4, // opOperatorInc,

    2, // opOperatorAdd0,
    2, // opOperatorMul0,
    2, // opOperatorDiv0,

    3, // opOperatorAdd1,
    3, // opOperatorSub1,
    3, // opOperatorMul1,
    3, // opOperatorDiv1,

    1, // opOperatorAdd,
    1, // opOperatorSub,
    1, // opOperatorMul,
    1, // opOperatorDiv,
    1, // opOperatorMod,
    1, // opOperatorNegative,

    2, // opOperatorLesser0,
    2, // opOperatorLesserOrEqual0,
    2, // opOperatorGreater0,
    2, // opOperatorGreaterOrEqual0,
    2, // opOperatorEqual0,
    2, // opOperatorNotEqual0,
    2, // opOperatorAnd0,
    2, // opOperatorOr0,
  
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
    2, // opPushConstFromConstList,

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
    1, // opThrow
    2, // opJITBlock
    2  // opJITBlockPotential
  );

type
  TSEIdentKind = (
    ikVariable,
    ikFunc
  );

  TSEIdent = record
    PossibleKinds: TSEValueKindSet;
    Kind: TSEIdentKind;
    Addr: NativeInt;
    IsUsed: Boolean;
    IsAssigned: Boolean;
    IsConst: Boolean;
    ConstValue: TSEValue;
    Local: NativeInt;
    Block: NativeInt;
    Ln: NativeInt;
    Col: NativeInt;
    Name: String;
  end;
  PSEIdent = ^TSEIdent;

  TSEIdentList = class(specialize TSEListPtr<TSEIdent>);

  TSEToken = record
    Kind: TSETokenKind;
    BelongedFileName,
    Value: String;
    Ln, Col: NativeInt;
  end;
  PSEToken = ^TSEToken;
  TSETokenList = specialize TList<TSEToken>;

  TEvilC = class
  private
    FSource: String;
    FInternalIdentCount: NativeUInt;
    JITBlockSignatureStack: TSEJITBlockSignatureStack;
    JITBlockCount: NativeInt;
    procedure SetSource(V: String);
    function InternalIdent: String;
  public
    Owner: TObject;
    OptimizeConstants,        // True = enable optimization for constant values stored in ConstList
    OptimizePeephole,         // True = enable peephole optimization, default is true
    OptimizeConstantFolding,  // True = enable constant folding optimization, default is true
    OptimizeAsserts: Boolean; // True = ignore assert, default is true
    ErrorLn, ErrorCol: NativeInt;
    VM: TSEVM;
    {$ifdef SE_THREADS}
    VMThreadList: TSEVMThreadList;
    {$endif}
    ConstLookup: TSEConstLookup;
    ConstList: TSEValueList;
    IncludePathList,
    IncludeList: TStrings;
    TokenList: TSETokenList;
    OpcodeInfoList: TSEOpcodeInfoList;
    LocalVarCountList: TSEIntegerList;
    GlobalVarCount: NativeInt;
    GlobalVarSymbols: TStrings;
    VarList: TSEIdentList;
    FuncNativeList: TSEFuncNativeList;
    FuncScriptList: TSEFuncScriptList;
    FuncImportList: TSEFuncImportList;
    ScopeStack: TSEScopeStack;
    ScopeFunc: TSEScopeStack;
    LineOfCodeList: TSELineOfCodeList;
    StackTraceHandler: TSEStackTraceSymbolProc;
    IsLex,
    IsParsed: Boolean;
    IsDone: Boolean;
    FuncCurrent: NativeInt;
    FuncTraversal: NativeInt;
    BlockTraversal: NativeInt;
    CurrentFileList: TStrings;
    CodeSegmentIndex: NativeInt; // This is mainly for storing line of code for runtime
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
    function ExecFuncOnly(const AIndex: NativeInt; const Args: array of TSEValue): TSEValue; overload;
    function ExecFunc(const AIndex: NativeInt; const Args: array of TSEValue): TSEValue; overload;
    procedure RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: NativeInt);
    function RegisterScriptFunc(const Name: String; const ArgCount: NativeInt; var AIndex: Cardinal; const IsOverride: Boolean = False): PSEFuncScriptInfo;
    procedure RegisterImportFunc(const Name, ActualName, LibName: String; const Args: TSEAtomKindArray; const Return: TSEAtomKind; const CC: TSECallingConvention = seccAuto);
    function Backup: TSECache;
    procedure Restore(const Cache: TSECache);
    function FindFunc(const Name: String): Pointer; inline; overload;
    function FindFuncNative(const Name: String; var Ind: Cardinal): PSEFuncNativeInfo; inline;
    function FindFuncScript(const Name: String; var Ind: Cardinal): PSEFuncScriptInfo; inline;
    function FindFuncImport(const Name: String; var Ind: Cardinal): PSEFuncImportInfo; inline;
    function FindFunc(const Name: String; var Kind: TSEFuncKind; var Ind: Cardinal): Pointer; inline; overload;
    procedure SetConst(const Name: String; const Value: TSEValue); inline; overload;

    property IsPaused: Boolean read GetIsPaused write SetIsPaused;
    property Source: String read FSource write SetSource;
  end;

  TScriptEngine = TEvilC;

  { ===============================
  X64 emitter
  =============================== }

type
  TX64Reg = (
    regRAX = 0, regRCX = 1, regRDX = 2, regRBX = 3,
    regRSP = 4, regRBP = 5, regRSI = 6, regRDI = 7,
    regR8 = 8, regR9 = 9, regR10 = 10, regR11 = 11,
    regR12 = 12, regR13 = 13, regR14 = 14, regR15 = 15
  );

  TXMMReg = (
    regXMM0 = 0, regXMM1 = 1, regXMM2 = 2, regXMM3 = 3,
    regXMM4 = 4, regXMM5 = 5, regXMM6 = 6, regXMM7 = 7,
    regXMM8 = 8, regXMM9 = 9, regXMM10 = 10, regXMM11 = 11,
    regXMM12 = 12, regXMM13 = 13, regXMM14 = 14, regXMM15 = 15
  );

  TX64Label = Integer;

  TX64Condition = (
    ccO = 0,   // overflow
    ccNO = 1,  // not overflow
    ccB = 2,   // below/carry
    ccAE = 3,  // above/equal/not carry
    ccE = 4,   // equal/zero
    ccNE = 5,  // not equal/not zero
    ccBE = 6,  // below/equal
    ccA = 7,   // above
    ccS = 8,   // sign
    ccNS = 9,  // not sign
    ccP = 10,  // parity
    ccNP = 11, // not parity
    ccL = 12,  // less
    ccGE = 13, // greater/equal
    ccLE = 14, // less/equal
    ccG = 15   // greater
  );

  TLabelInfo = record
    Bound: boolean;
    Position: Integer;
  end;

  TJumpPatch = record
    LabelID: TX64Label;
    DisplacementOffset: Integer;
  end;

  { Base/Index = -1 means "not present". }
  TX64Mem = record
    Base: Integer;
    Index: Integer;
    Scale: Byte;
    Disp: LongInt;
  end;

  TX64CodeList = specialize TSEListPtr<Byte>;
  TX64LabelInfoList = specialize TSEListPtr<TLabelInfo>;
  TX64JumpPatchList = specialize TSEListPtr<TJumpPatch>;

  TX64Emitter = class
  private
    FCode: TX64CodeList;
    FLabels: TX64LabelInfoList;
    FJumps: TX64JumpPatchList;

    FExecutableMemory: Pointer;
    FExecutableSize: NativeUInt;

    procedure EmitByte(B: Byte);
    procedure EmitU16(V: Word);
    procedure EmitU32(V: LongWord);
    procedure EmitU64(V: QWord);
    procedure EmitI8(V: shortint);

    procedure EmitRex(W: boolean; RegField, IndexField, BaseField: Integer);
    procedure EmitRexByte(RegField, IndexField, BaseField: Integer);

    procedure EmitModRM(ModBits: Byte; RegField, RMField: Integer);

    procedure EmitSIB(ScaleBits, IndexField, BaseField: Integer);

    procedure EmitMemModRM(RegField: Integer; const M: TX64Mem);

    procedure EmitRM(Opcode: Byte; W: boolean; RegField: Integer; const M: TX64Mem);

    procedure EmitRM2(Opcode1, Opcode2: Byte; W: boolean; RegField: Integer; const M: TX64Mem);

    procedure EmitRegReg(Opcode: Byte; W: boolean; Dst, Src: TX64Reg);

    procedure EmitGroup1Imm(Group: Byte; Dst: TX64Reg; Value: LongInt);

    procedure EmitGroup2Imm(Group: Byte; Dst: TX64Reg; Count: Byte);

    procedure EmitGroup2CL(Group: Byte; Dst: TX64Reg);

    procedure EmitSSEMem(Prefix: Byte; Opcode1, Opcode2: Byte; XMM: TXMMReg; const M: TX64Mem);

    procedure EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte; Dst, Src: TXMMReg); overload;

    procedure EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte; Dst: TXMMReg; Src: TX64Reg); overload;

    procedure EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte; Dst: TX64Reg; Src: TXMMReg); overload;

    procedure EmitSSEMemImm8(Prefix: Byte; Opcode1, Opcode2: Byte; XMM: TXMMReg; const M: TX64Mem; Imm8: Byte); overload;

    procedure EmitSSEMemImm8(Prefix: Byte; Opcode1, Opcode2, Opcode3: Byte; XMM: TXMMReg; const M: TX64Mem; Imm8: Byte); overload;

    procedure EmitSSERegImm8(Prefix: Byte; Opcode1, Opcode2, Opcode3: Byte; Dst, Src: TXMMReg; Imm8: Byte);

    procedure EmitArithMemImm(Group: Byte; W: Boolean; const M: TX64Mem; Imm: Int64);

    procedure ResolveLabels;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    { -----------------------------------------------------------------
      Memory operands
      ----------------------------------------------------------------- }

    class function Mem(Base: TX64Reg; Disp: LongInt = 0): TX64Mem; static;

    class function MemIndex(Base, Index: TX64Reg; Scale: Byte; Disp: LongInt = 0): TX64Mem; static;

    class function MemAbsolute(Address: Pointer): TX64Mem; static;

    { -----------------------------------------------------------------
      Raw data / utility
      ----------------------------------------------------------------- }

    procedure DB(B: Byte);
    procedure DW(V: Word);
    procedure DD(V: LongWord);
    procedure DQ(V: QWord);

    procedure Nop;
    procedure NopN(Count: Integer);
    procedure Int3;
    procedure Ud2;

    { Align code to power-of-two boundary. }
    procedure Align(Alignment: Integer; FillByte: Byte = $90);

    { -----------------------------------------------------------------
      Labels / branches
      ----------------------------------------------------------------- }

    function CreateLabel: TX64Label;
    procedure BindLabel(L: TX64Label);

    procedure Jmp(L: TX64Label);
    procedure Jcc(Condition: TX64Condition; L: TX64Label);

    procedure Je(L: TX64Label);
    procedure Jne(L: TX64Label);
    procedure Jg(L: TX64Label);
    procedure Jge(L: TX64Label);
    procedure Jl(L: TX64Label);
    procedure Jle(L: TX64Label);
    procedure Ja(L: TX64Label);
    procedure Jae(L: TX64Label);
    procedure Jb(L: TX64Label);
    procedure Jbe(L: TX64Label);
    procedure Jo(L: TX64Label);
    procedure Jno(L: TX64Label);
    procedure Js(L: TX64Label);
    procedure Jns(L: TX64Label);

    { -----------------------------------------------------------------
      Integer moves
      ----------------------------------------------------------------- }

    procedure MovRegImm64(Dst: TX64Reg; Value: QWord);
    procedure MovRegImm32(Dst: TX64Reg; Value: LongWord);
    procedure MovRegImm32SExt(Dst: TX64Reg; Value: LongInt);

    procedure MovRegReg64(Dst, Src: TX64Reg);
    procedure MovRegReg32(Dst, Src: TX64Reg);

    procedure MovReg64Mem(Dst: TX64Reg; const M: TX64Mem);

    procedure MovReg32Mem(Dst: TX64Reg; const M: TX64Mem);

    procedure MovMem64Reg(const M: TX64Mem; Src: TX64Reg);

    procedure MovMem32Reg(const M: TX64Mem; Src: TX64Reg);

    procedure MovReg8Reg8(Dst, Src: TX64Reg);
    procedure MovReg8Mem(Dst: TX64Reg; const M: TX64Mem);
    procedure MovMem8Reg(const M: TX64Mem; Src: TX64Reg);
    procedure MovMemImm8(const M: TX64Mem; Value: Byte);

    procedure MovMemImm32(const M: TX64Mem; Value: LongWord);

    procedure MovMemImm64(const M: TX64Mem; Value: QWord);

    procedure MovZXReg8(Dst, Src: TX64Reg);
    procedure MovZXReg16(Dst, Src: TX64Reg);
    procedure MovSXReg8(Dst, Src: TX64Reg);
    procedure MovSXReg16(Dst, Src: TX64Reg);

    procedure MovZXReg8Mem(Dst: TX64Reg; const M: TX64Mem);
    procedure MovZXReg16Mem(Dst: TX64Reg; const M: TX64Mem);

    procedure MovSXReg8Mem(Dst: TX64Reg; const M: TX64Mem);
    procedure MovSXReg16Mem(Dst: TX64Reg; const M: TX64Mem);

    procedure LeaRegMem(Dst: TX64Reg; const M: TX64Mem);

    procedure XchgRegReg(A, B: TX64Reg);

    { -----------------------------------------------------------------
      Integer arithmetic
      ----------------------------------------------------------------- }

    procedure AddRegReg(Dst, Src: TX64Reg);
    procedure SubRegReg(Dst, Src: TX64Reg);
    procedure AdcRegReg(Dst, Src: TX64Reg);
    procedure SbbRegReg(Dst, Src: TX64Reg);

    procedure AndRegReg(Dst, Src: TX64Reg);
    procedure OrRegReg(Dst, Src: TX64Reg);
    procedure XorRegReg(Dst, Src: TX64Reg);

    procedure AddRegImm32(Dst: TX64Reg; Value: LongInt);
    procedure SubRegImm32(Dst: TX64Reg; Value: LongInt);
    procedure AndRegImm32(Dst: TX64Reg; Value: LongInt);
    procedure OrRegImm32(Dst: TX64Reg; Value: LongInt);
    procedure XorRegImm32(Dst: TX64Reg; Value: LongInt);

    procedure AdcRegImm32(Dst: TX64Reg; Value: LongInt);
    procedure SbbRegImm32(Dst: TX64Reg; Value: LongInt);

    procedure IncReg(R: TX64Reg);
    procedure DecReg(R: TX64Reg);
    procedure NegReg(R: TX64Reg);
    procedure NotReg(R: TX64Reg);

    procedure IMulRegReg(Dst, Src: TX64Reg);
    procedure IMulRegRegImm32(Dst, Src: TX64Reg; Value: LongInt);

    procedure MulReg(Src: TX64Reg);
    procedure DivReg(Src: TX64Reg);
    procedure IDivReg(Src: TX64Reg);

    procedure Cqo;

    procedure AddMem64Imm(const M: TX64Mem; Imm: LongInt);
    procedure SubMem64Imm(const M: TX64Mem; Imm: LongInt);
    procedure AndMem64Imm(const M: TX64Mem; Imm: LongInt);
    procedure OrMem64Imm(const M: TX64Mem; Imm: LongInt);
    procedure XorMem64Imm(const M: TX64Mem; Imm: LongInt);
    procedure CmpMem64Imm(const M: TX64Mem; Imm: LongInt);

    procedure AddMem32Imm(const M: TX64Mem; Imm: LongInt);
    procedure SubMem32Imm(const M: TX64Mem; Imm: LongInt);
    procedure AndMem32Imm(const M: TX64Mem; Imm: LongInt);
    procedure OrMem32Imm(const M: TX64Mem; Imm: LongInt);
    procedure XorMem32Imm(const M: TX64Mem; Imm: LongInt);
    procedure CmpMem32Imm(const M: TX64Mem; Imm: LongInt);

    { -----------------------------------------------------------------
      Compare / test
      ----------------------------------------------------------------- }

    procedure CmpRegReg(A, B: TX64Reg);
    procedure CmpRegImm32(A: TX64Reg; Value: LongInt);

    procedure TestRegReg(A, B: TX64Reg);
    procedure TestRegImm32(A: TX64Reg; Value: LongInt);

    procedure Setcc(Condition: TX64Condition; Dst: TX64Reg);

    procedure Sete(Dst: TX64Reg);
    procedure Setne(Dst: TX64Reg);
    procedure Setg(Dst: TX64Reg);
    procedure Setge(Dst: TX64Reg);
    procedure Setl(Dst: TX64Reg);
    procedure Setle(Dst: TX64Reg);

    procedure Cmovcc(Condition: TX64Condition; Dst, Src: TX64Reg);

    procedure Cmove(Dst, Src: TX64Reg);
    procedure Cmovne(Dst, Src: TX64Reg);
    procedure Cmovg(Dst, Src: TX64Reg);
    procedure Cmovge(Dst, Src: TX64Reg);
    procedure Cmovl(Dst, Src: TX64Reg);
    procedure Cmovle(Dst, Src: TX64Reg);

    { -----------------------------------------------------------------
      Shifts / rotates
      ----------------------------------------------------------------- }

    procedure ShlRegImm(Dst: TX64Reg; Count: Byte);
    procedure ShrRegImm(Dst: TX64Reg; Count: Byte);
    procedure SarRegImm(Dst: TX64Reg; Count: Byte);

    procedure ShlRegCL(Dst: TX64Reg);
    procedure ShrRegCL(Dst: TX64Reg);
    procedure SarRegCL(Dst: TX64Reg);

    procedure RolRegImm(Dst: TX64Reg; Count: Byte);
    procedure RorRegImm(Dst: TX64Reg; Count: Byte);

    procedure RolRegCL(Dst: TX64Reg);
    procedure RorRegCL(Dst: TX64Reg);

    { -----------------------------------------------------------------
      Bit operations
      ----------------------------------------------------------------- }

    procedure BTRegReg(Base, Bit: TX64Reg);
    procedure BTSRegReg(Base, Bit: TX64Reg);
    procedure BTRRegReg(Base, Bit: TX64Reg);
    procedure BTCRegReg(Base, Bit: TX64Reg);

    procedure BTRegImm(Base: TX64Reg; Bit: Byte);
    procedure BTSRegImm(Base: TX64Reg; Bit: Byte);
    procedure BTRRegImm(Base: TX64Reg; Bit: Byte);
    procedure BTCRegImm(Base: TX64Reg; Bit: Byte);

    procedure BsfRegReg(Dst, Src: TX64Reg);
    procedure BsrRegReg(Dst, Src: TX64Reg);

    procedure Bswap(R: TX64Reg);

    { -----------------------------------------------------------------
      Stack
      ----------------------------------------------------------------- }

    procedure PushReg(R: TX64Reg);
    procedure PopReg(R: TX64Reg);

    procedure PushImm32(Value: LongInt);

    procedure PushFlags;
    procedure PopFlags;

    procedure Enter(StackSize: Word; NestingLevel: Byte);
    procedure Leave;

    { -----------------------------------------------------------------
      Calls / returns
      ----------------------------------------------------------------- }

    procedure CallReg(R: TX64Reg);
    procedure CallAbsolute(R: TX64Reg; Address: Pointer);

    procedure JmpReg(R: TX64Reg);
    procedure JmpAbsolute(R: TX64Reg; Address: Pointer);

    procedure Ret;
    procedure RetImm16(Value: Word);

    { -----------------------------------------------------------------
      System / CPU
      ----------------------------------------------------------------- }

    procedure Syscall;
    procedure Sysret;
    procedure Cpuid;
    procedure Rdtsc;
    procedure Rdtscp;

    { -----------------------------------------------------------------
      SSE2 scalar double
      ----------------------------------------------------------------- }

    procedure MovSDXMMFromMem(Dst: TXMMReg; const M: TX64Mem);

    procedure MovSDMemFromXMM(const M: TX64Mem; Src: TXMMReg);

    procedure MovSDXMMFromReg(Dst: TXMMReg; Src: TX64Reg);

    procedure MovRegFromSDXMM(Dst: TX64Reg; Src: TXMMReg);

    procedure MovSDXMM(Dst, Src: TXMMReg);

    procedure AddSD(Dst, Src: TXMMReg);
    procedure SubSD(Dst, Src: TXMMReg);
    procedure MulSD(Dst, Src: TXMMReg);
    procedure DivSD(Dst, Src: TXMMReg);

    procedure SqrtSD(Dst, Src: TXMMReg);
    procedure MinSD(Dst, Src: TXMMReg);
    procedure MaxSD(Dst, Src: TXMMReg);

    procedure AddSDMem(Dst: TXMMReg; const M: TX64Mem);
    procedure SubSDMem(Dst: TXMMReg; const M: TX64Mem);
    procedure MulSDMem(Dst: TXMMReg; const M: TX64Mem);
    procedure DivSDMem(Dst: TXMMReg; const M: TX64Mem);

    procedure ComISD(Dst, Src: TXMMReg);

    procedure CvtSI2SD(Dst: TXMMReg; Src: TX64Reg);

    procedure CvttSD2SI(Dst: TX64Reg; Src: TXMMReg);

    procedure CvtSD2SI(Dst: TX64Reg; Src: TXMMReg);

    procedure RoundSD(Dst, Src: TXMMReg; Rounding: Byte);
    procedure RoundSDMem(Dst: TXMMReg; const M: TX64Mem; Rounding: Byte);

    procedure XorPD(Dst, Src: TXMMReg);
    procedure XorPDMem(Dst: TXMMReg; const M: TX64Mem);

    { -----------------------------------------------------------------
      SSE2 packed Integer / double operations
      ----------------------------------------------------------------- }

    procedure MovDQU(Dst, Src: TXMMReg);
    procedure MovDQUFromMem(Dst: TXMMReg; const M: TX64Mem);
    procedure MovDQUMem(const M: TX64Mem; Src: TXMMReg);

    procedure Pxor(Dst, Src: TXMMReg);
    procedure Pand(Dst, Src: TXMMReg);
    procedure Por(Dst, Src: TXMMReg);

    procedure PaddQ(Dst, Src: TXMMReg);
    procedure PsubQ(Dst, Src: TXMMReg);

    { -----------------------------------------------------------------
      Finalization
      ----------------------------------------------------------------- }

    function MakeExecutable: Pointer;

    property Code: TX64CodeList read FCode;
    property ExecutableSize: NativeUInt read FExecutableSize;
  end;

function SEValueToText(const Value: TSEValue; const IsRoot: Boolean = True): String;
function SESize(constref Value: TSEValue): SizeInt; inline;
procedure SEValidateType(V: PSEValue; Expected: TSEValueKind; At: DWord; const FuncName: String); inline;
procedure SEMapDelete(constref V: TSEValue; const I: NativeInt); inline; overload;
procedure SEMapDelete(constref V: TSEValue; constref S: String); inline; overload;
procedure SEMapDelete(constref V, I: TSEValue); inline; overload;
function SEMapGet(constref V: TSEValue; const I: NativeInt): TSEValue; inline; overload;
function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
procedure SEMapGet(out R: TSEValue; constref V, I: TSEValue); inline; overload;
procedure SEMapSet(constref V: TSEValue; const I: NativeInt; constref A: TSEValue); inline; overload;
procedure SEMapSet(constref V: TSEValue; constref S: String; constref A: TSEValue); inline; overload;
procedure SEMapSet(constref V, I: TSEValue; constref A: TSEValue); inline; overload;
function SEMapIsValidArray(constref V: TSEValue): Boolean; inline;
procedure SEDisAsm(const VM: TSEVM; var Res: String);
function SEGet(const AName: String): TSEValue;
procedure SESet(const AName: String; const AValue: TSEValue);

operator := (V: TSENumber) R: TSEValue;
operator := (V: String) R: TSEValue;
operator := (V: Boolean) R: TSEValue;
operator := (V: TSEValueArray) R: TSEValue;
operator := (V: Pointer) R: TSEValue;
operator := (V: TSEValue) R: NativeInt;
operator := (V: TSEValue) R: TValue;
operator := (V: TValue) R: TSEValue;
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
  SEStackSize,
  SEThreadStackSize,
  SEFrameSize,
  SETrapSize: Cardinal;

implementation

uses
  Math, Strings;

const
  SE_REG_GLOBAL = $FFFFFFFF;

type
  TBuiltInFunction = class
    class function SEBufferCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferLength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferFillF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferGetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferSetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringToBuffer(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEWBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEArrayToBufferF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEArrayToBufferF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferToArrayF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBufferToArrayF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;

    class function SETypeOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEKindOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEWrite(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEWriteln(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SERandom(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SERnd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SERound(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SERoundTo(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFloor(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECeil(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SETrunc(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SENumber(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SELength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMapCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMapClone(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMapKeyDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMapKeysGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMapClear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEArrayResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEArrayToMap(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEArrayFill(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SELerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESLerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESign(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECos(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SETan(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECot(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESqrt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEAbs(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFrac(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SERange(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEMax(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEPow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SESleep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringGrep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringSplit(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringFind(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringInsert(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringCompare(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringReplace(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringReplaceIgnoreCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringFormat(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringUpperCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringLowerCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringFindRegex(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringTrim(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringTrimLeft(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringTrimRight(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringExtractName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringExtractPath(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEStringExtractExt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEGetTickCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTNow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTSetDate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTSetTime(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTDayAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTMonthAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTYearAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTGetYear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTGetMonth(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTGetDay(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTGetHour(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDTGetMinute(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEGCObjectCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEGCObjectOldCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEGCCollect(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEChar(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEOrd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineResume(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECoroutineIsExecuting(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    {$ifdef SE_THREADS}
    class function SEThreadCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEThreadStart(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEThreadIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEThreadSuspend(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEThreadTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEThreadWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECriticalCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECriticalEnter(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECriticalLeave(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SECriticalTry(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEEventCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEEventSet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEEventWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEEventReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    {$endif}
    class function SEFileReadText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileReadBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileWriteText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileWriteBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileRename(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileGetSize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEFileGetAge(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDirectoryCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDirectoryDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDirectoryFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEDirectoryExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;

    class function SEBase64Encode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEBase64Decode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;

    class function SEJSONParse(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEJSONStringify(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;

    class function SEPasObjectClassName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
    class function SEInvoke(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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
  ConstStrings: TSEStringList;
  ConstStringsLookup: TSEStringLookupMap;
  Negative2QWords: array[0..1] of QWord = ($8000000000000000, $8000000000000000);

{ =====================================================================
  Basic Byte emission
  ===================================================================== }

constructor TX64Emitter.Create;
begin
  inherited Create;

  Self.FCode := TX64CodeList.Create;
  Self.FCode.Capacity := 4096;
  Self.FLabels := TX64LabelInfoList.Create;
  Self.FLabels.Capacity := 16;
  Self.FJumps := TX64JumpPatchList.Create;
  Self.FJumps.Capacity := 16;
end;

destructor TX64Emitter.Destroy;
begin
  Self.FJumps.Free;
  Self.FLabels.Free;
  Self.FCode.Free;
  inherited Destroy;
end;

procedure TX64Emitter.Clear;
begin
  Self.FCode.Count := 0;
  Self.FLabels.Count := 0;
  Self.FJumps.Count := 0;
end;

procedure TX64Emitter.EmitByte(B: Byte);
begin
  Self.FCode.Add(B);
end;

procedure TX64Emitter.EmitU16(V: Word);
begin
  EmitByte(Byte(V));
  EmitByte(Byte(V shr 8));
end;

procedure TX64Emitter.EmitU32(V: LongWord);
begin
  EmitByte(Byte(V));
  EmitByte(Byte(V shr 8));
  EmitByte(Byte(V shr 16));
  EmitByte(Byte(V shr 24));
end;

procedure TX64Emitter.EmitU64(V: QWord);
begin
  EmitByte(Byte(V));
  EmitByte(Byte(V shr 8));
  EmitByte(Byte(V shr 16));
  EmitByte(Byte(V shr 24));
  EmitByte(Byte(V shr 32));
  EmitByte(Byte(V shr 40));
  EmitByte(Byte(V shr 48));
  EmitByte(Byte(V shr 56));
end;

procedure TX64Emitter.EmitI8(V: shortint);
begin
  EmitByte(Byte(V));
end;

procedure TX64Emitter.DB(B: Byte);
begin
  EmitByte(B);
end;

procedure TX64Emitter.DW(V: Word);
begin
  EmitU16(V);
end;

procedure TX64Emitter.DD(V: LongWord);
begin
  EmitU32(V);
end;

procedure TX64Emitter.DQ(V: QWord);
begin
  EmitU64(V);
end;

procedure TX64Emitter.Nop;
begin
  EmitByte($90);
end;

procedure TX64Emitter.NopN(Count: Integer);
begin
  while Count > 0 do
  begin
    EmitByte($90);
    Dec(Count);
  end;
end;

procedure TX64Emitter.Int3;
begin
  EmitByte($CC);
end;

procedure TX64Emitter.Ud2;
begin
  EmitByte($0F);
  EmitByte($0B);
end;

procedure TX64Emitter.Align(Alignment: Integer; FillByte: Byte);
var
  N: Integer;
begin
  if Alignment <= 0 then
    raise Exception.Create('Invalid alignment');

  if (Alignment and (Alignment - 1)) <> 0 then
    raise Exception.Create('Alignment must be a power of two');

  while (Self.FCode.Count and (Alignment - 1)) <> 0 do
    EmitByte(FillByte);

  N := Self.FCode.Count;
  if N < 0 then
    raise Exception.Create('Code size overflow');
end;

{ =====================================================================
  Memory operands
  ===================================================================== }

class function TX64Emitter.Mem(Base: TX64Reg; Disp: LongInt): TX64Mem;
begin
  Result.Base := Ord(Base);
  Result.Index := -1;
  Result.Scale := 1;
  Result.Disp := Disp;
end;

class function TX64Emitter.MemIndex(Base, Index: TX64Reg; Scale: Byte;
  Disp: LongInt): TX64Mem;
begin
  if not (Scale in [1, 2, 4, 8]) then
    raise Exception.Create('Scale must be 1, 2, 4 or 8');

  if (Ord(Index) and 7) = 4 then
    raise Exception.Create('RSP/R12 cannot be used as SIB index');

  Result.Base := Ord(Base);
  Result.Index := Ord(Index);
  Result.Scale := Scale;
  Result.Disp := Disp;
end;

class function TX64Emitter.MemAbsolute(Address: Pointer): TX64Mem;
begin
  { Encoded as [disp32] by the normal memory encoder.
    Therefore the address must fit the x64 absolute disp32 encoding
    used by the chosen addressing form. For arbitrary 64-bit absolute
    addresses, load the address into a register and use [reg]. }

  if PtrUInt(Address) > $FFFFFFFF then
    raise Exception.Create(
      'MemAbsolute requires a 32-bit address; use a register for arbitrary addresses');

  Result.Base := -1;
  Result.Index := -1;
  Result.Scale := 1;
  Result.Disp := LongInt(PtrUInt(Address));
end;

{ =====================================================================
  REX / ModRM / SIB
  ===================================================================== }

procedure TX64Emitter.EmitRex(W: boolean; RegField, IndexField, BaseField: Integer);
var
  R, X, B, V: Byte;
begin
  R := 0;
  X := 0;
  B := 0;

  if RegField >= 8 then
    R := 1;

  if IndexField >= 8 then
    X := 1;

  if BaseField >= 8 then
    B := 1;

  V := $40;

  if W then
    V := V or $08;

  if R <> 0 then
    V := V or $04;

  if X <> 0 then
    V := V or $02;

  if B <> 0 then
    V := V or $01;

  if V <> $40 then
    EmitByte(V);
end;

procedure TX64Emitter.EmitRexByte(RegField, IndexField, BaseField: Integer);
var
  NeedRex: Boolean;
begin
  NeedRex :=
    (RegField >= 4) or
    (IndexField >= 8) or
    (BaseField >= 8);

  if NeedRex then
  begin
    if RegField >= 8 then
      EmitRex(False, RegField, IndexField, BaseField)
    else
    begin
      { RegField 4..7 needs a REX prefix, but has no R bit. }
      EmitRex(False, 0, IndexField, BaseField);
    end;
  end
  else
    EmitRex(False, RegField, IndexField, BaseField);
end;

procedure TX64Emitter.EmitModRM(ModBits: Byte; RegField, RMField: Integer);
begin
  EmitByte(
    (ModBits shl 6) or ((RegField and 7) shl 3) or (RMField and 7)
    );
end;

procedure TX64Emitter.EmitSIB(ScaleBits, IndexField, BaseField: Integer);
begin
  EmitByte(
    ((ScaleBits and 3) shl 6) or ((IndexField and 7) shl 3) or (BaseField and 7)
    );
end;

procedure TX64Emitter.EmitMemModRM(RegField: Integer; const M: TX64Mem);
var
  ModBits: Byte;
  RMField: Integer;
  BaseLow, IndexLow: Integer;
  NeedSIB: boolean;
  ScaleBits: Integer;
begin
  if M.Index < -1 then
    raise Exception.Create('Invalid memory index');

  if (M.Index >= 0) and ((M.Index and 7) = 4) then
    raise Exception.Create('RSP/R12 cannot be SIB index');

  BaseLow := 0;
  IndexLow := 4;

  if M.Base >= 0 then
    BaseLow := M.Base and 7;

  if M.Index >= 0 then
    IndexLow := M.Index and 7;

  NeedSIB :=
    (M.Base < 0) or (BaseLow = 4) or (M.Index >= 0);

  if M.Base < 0 then
  begin
    { No base: SIB base=101, mod=00, disp32. }
    EmitModRM(0, RegField, 4);

    if M.Scale = 1 then ScaleBits := 0
    else if M.Scale = 2 then ScaleBits := 1
    else if M.Scale = 4 then ScaleBits := 2
    else if M.Scale = 8 then ScaleBits := 3
    else
      raise Exception.Create('Invalid SIB scale');

    EmitSIB(ScaleBits, IndexLow, 5);
    EmitU32(LongWord(M.Disp));
    Exit;
  end;

  if M.Disp = 0 then
  begin
    { regRBP/R13 require a displacement even when it is zero. }
    if BaseLow = 5 then
      ModBits := 1
    else
      ModBits := 0;
  end
  else if (M.Disp >= -128) and (M.Disp <= 127) then
    ModBits := 1
  else
    ModBits := 2;

  if NeedSIB then
    RMField := 4
  else
    RMField := BaseLow;

  EmitModRM(ModBits, RegField, RMField);

  if NeedSIB then
  begin
    if M.Scale = 1 then ScaleBits := 0
    else if M.Scale = 2 then ScaleBits := 1
    else if M.Scale = 4 then ScaleBits := 2
    else if M.Scale = 8 then ScaleBits := 3
    else
      raise Exception.Create('Invalid SIB scale');

    EmitSIB(ScaleBits, IndexLow, BaseLow);
  end;

  if ModBits = 1 then
    EmitI8(shortint(M.Disp))
  else if ModBits = 2 then
    EmitU32(LongWord(M.Disp));
end;

procedure TX64Emitter.EmitRM(Opcode: Byte; W: boolean; RegField: Integer;
  const M: TX64Mem);
begin
  EmitRex(W, RegField, M.Index, M.Base);
  EmitByte(Opcode);
  EmitMemModRM(RegField, M);
end;

procedure TX64Emitter.EmitRM2(Opcode1, Opcode2: Byte; W: boolean;
  RegField: Integer; const M: TX64Mem);
begin
  EmitRex(W, RegField, M.Index, M.Base);
  EmitByte(Opcode1);
  EmitByte(Opcode2);
  EmitMemModRM(RegField, M);
end;

procedure TX64Emitter.EmitRegReg(Opcode: Byte; W: boolean; Dst, Src: TX64Reg);
begin
  EmitRex(W, Ord(Src), -1, Ord(Dst));
  EmitByte(Opcode);
  EmitModRM(3, Ord(Src), Ord(Dst));
end;

procedure TX64Emitter.EmitGroup1Imm(Group: Byte; Dst: TX64Reg; Value: LongInt);
begin
  EmitRex(True, Group, -1, Ord(Dst));
  EmitByte($81);
  EmitModRM(3, Group, Ord(Dst));
  EmitU32(LongWord(Value));
end;

procedure TX64Emitter.EmitGroup2Imm(Group: Byte; Dst: TX64Reg; Count: Byte);
begin
  EmitRex(True, Group, -1, Ord(Dst));

  if Count = 1 then
  begin
    EmitByte($D1);
    EmitModRM(3, Group, Ord(Dst));
  end
  else
  begin
    EmitByte($C1);
    EmitModRM(3, Group, Ord(Dst));
    EmitByte(Count);
  end;
end;

procedure TX64Emitter.EmitGroup2CL(Group: Byte; Dst: TX64Reg);
begin
  EmitRex(True, Group, -1, Ord(Dst));
  EmitByte($D3);
  EmitModRM(3, Group, Ord(Dst));
end;

{ =====================================================================
  Labels / jumps
  ===================================================================== }

function TX64Emitter.CreateLabel: TX64Label;
var
  Lbl: TLabelInfo;
begin
  Result := Self.FLabels.Count;

  Lbl.Bound := False;
  Lbl.Position := 0;
  Self.FLabels.Add(Lbl);
end;

procedure TX64Emitter.BindLabel(L: TX64Label);
begin
  if (L < 0) or (L >= Self.FLabels.Count) then
    raise Exception.Create('Invalid label');

  if Self.FLabels.Ptr(L)^.Bound then
    raise Exception.Create('Label already bound');

  Self.FLabels.Ptr(L)^.Bound := True;
  Self.FLabels.Ptr(L)^.Position := Self.FCode.Count;
end;

procedure TX64Emitter.Jmp(L: TX64Label);
var
  J: TJumpPatch;
begin
  EmitByte($E9);

  J.LabelID := L;
  J.DisplacementOffset := Self.FCode.Count;
  Self.FJumps.Add(J);

  EmitU32(0);
end;

procedure TX64Emitter.Jcc(Condition: TX64Condition; L: TX64Label);
var
  J: TJumpPatch;
begin
  EmitByte($0F);
  EmitByte($80 + Ord(Condition));

  J.LabelID := L;
  J.DisplacementOffset := Self.FCode.Count;
  Self.FJumps.Add(J);

  EmitU32(0);
end;

procedure TX64Emitter.Je(L: TX64Label);
begin
  Jcc(ccE, L);
end;

procedure TX64Emitter.Jne(L: TX64Label);
begin
  Jcc(ccNE, L);
end;

procedure TX64Emitter.Jg(L: TX64Label);
begin
  Jcc(ccG, L);
end;

procedure TX64Emitter.Jge(L: TX64Label);
begin
  Jcc(ccGE, L);
end;

procedure TX64Emitter.Jl(L: TX64Label);
begin
  Jcc(ccL, L);
end;

procedure TX64Emitter.Jle(L: TX64Label);
begin
  Jcc(ccLE, L);
end;

procedure TX64Emitter.Ja(L: TX64Label);
begin
  Jcc(ccA, L);
end;

procedure TX64Emitter.Jae(L: TX64Label);
begin
  Jcc(ccAE, L);
end;

procedure TX64Emitter.Jb(L: TX64Label);
begin
  Jcc(ccB, L);
end;

procedure TX64Emitter.Jbe(L: TX64Label);
begin
  Jcc(ccBE, L);
end;

procedure TX64Emitter.Jo(L: TX64Label);
begin
  Jcc(ccO, L);
end;

procedure TX64Emitter.Jno(L: TX64Label);
begin
  Jcc(ccNO, L);
end;

procedure TX64Emitter.Js(L: TX64Label);
begin
  Jcc(ccS, L);
end;

procedure TX64Emitter.Jns(L: TX64Label);
begin
  Jcc(ccNS, L);
end;

procedure TX64Emitter.ResolveLabels;
var
  I: Integer;
  L: TX64Label;
  PatchPos: Integer;
  TargetPos: Integer;
  NextInstruction: Integer;
  Rel: int64;
  V: LongWord;
begin
  for I := 0 to Self.FJumps.Count - 1 do
  begin
    L := Self.FJumps.Ptr(I)^.LabelID;

    if (L < 0) or (L >= Self.FLabels.Count) then
      raise Exception.Create('Invalid jump label');

    if not Self.FLabels.Ptr(L)^.Bound then
      raise Exception.Create('Unbound label');

    PatchPos := Self.FJumps.Ptr(I)^.DisplacementOffset;
    NextInstruction := PatchPos + 4;
    TargetPos := Self.FLabels.Ptr(L)^.Position;

    Rel :=
      int64(TargetPos) - int64(NextInstruction);

    if (Rel < Low(LongInt)) or (Rel > High(LongInt)) then
      raise Exception.Create('Jump out of rel32 range');

    V := LongWord(LongInt(Rel));

    Self.FCode[PatchPos + 0] := Byte(V);
    Self.FCode[PatchPos + 1] := Byte(V shr 8);
    Self.FCode[PatchPos + 2] := Byte(V shr 16);
    Self.FCode[PatchPos + 3] := Byte(V shr 24);
  end;
end;

{ =====================================================================
  Moves
  ===================================================================== }

procedure TX64Emitter.MovRegImm64(Dst: TX64Reg; Value: QWord);
var
  R: Integer;
begin
  R := Ord(Dst);

  if R >= 8 then
    EmitByte($49)
  else
    EmitByte($48);

  EmitByte($B8 + (R and 7));
  EmitU64(Value);
end;

procedure TX64Emitter.MovRegImm32(Dst: TX64Reg; Value: LongWord);
var
  R: Integer;
begin
  R := Ord(Dst);

  if R >= 8 then
    EmitByte($41);

  EmitByte($B8 + (R and 7));
  EmitU32(Value);
end;

procedure TX64Emitter.MovRegImm32SExt(Dst: TX64Reg; Value: LongInt);
begin
  EmitRex(True, 0, -1, Ord(Dst));
  EmitByte($C7);
  EmitModRM(3, 0, Ord(Dst));
  EmitU32(LongWord(Value));
end;

procedure TX64Emitter.MovRegReg64(Dst, Src: TX64Reg);
begin
  EmitRegReg($89, True, Dst, Src);
end;

procedure TX64Emitter.MovRegReg32(Dst, Src: TX64Reg);
begin
  EmitRegReg($89, False, Dst, Src);
end;

procedure TX64Emitter.MovReg64Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM($8B, True, Ord(Dst), M);
end;

procedure TX64Emitter.MovReg32Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM($8B, False, Ord(Dst), M);
end;

procedure TX64Emitter.MovMem64Reg(const M: TX64Mem; Src: TX64Reg);
begin
  EmitRM($89, True, Ord(Src), M);
end;

procedure TX64Emitter.MovMem32Reg(const M: TX64Mem; Src: TX64Reg);
begin
  EmitRM($89, False, Ord(Src), M);
end;

procedure TX64Emitter.MovReg8Reg8(Dst, Src: TX64Reg);
begin
  { MOV r/m8, r8
    88 /r

    Dst is encoded in r/m.
    Src is encoded in reg.
  }
  EmitRexByte(Ord(Src), -1, Ord(Dst));
  EmitByte($88);
  EmitModRM(3, Ord(Src), Ord(Dst));
end;

procedure TX64Emitter.MovReg8Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  { MOV r8, r/m8
    8A /r
  }
  EmitRexByte(Ord(Dst), M.Index, M.Base);
  EmitByte($8A);
  EmitMemModRM(Ord(Dst), M);
end;

procedure TX64Emitter.MovMem8Reg(const M: TX64Mem; Src: TX64Reg);
begin
  { MOV r/m8, r8
    88 /r
  }
  EmitRexByte(Ord(Src), M.Index, M.Base);
  EmitByte($88);
  EmitMemModRM(Ord(Src), M);
end;

procedure TX64Emitter.MovMemImm8(const M: TX64Mem; Value: Byte);
begin
  { MOV r/m8, imm8
    C6 /0 ib
  }
  EmitRex(False, 0, M.Index, M.Base);
  EmitByte($C6);
  EmitMemModRM(0, M);
  EmitByte(Value);
end;

procedure TX64Emitter.MovMemImm32(const M: TX64Mem; Value: LongWord);
begin
  EmitRex(False, 0, M.Index, M.Base);
  EmitByte($C7);
  EmitMemModRM(0, M);
  EmitU32(Value);
end;

procedure TX64Emitter.MovMemImm64(const M: TX64Mem; Value: QWord);
begin
  { MOV r/m64,imm32 cannot represent arbitrary 64-bit constants.
    Use regRAX as a temporary. }
  MovRegImm64(regRAX, Value);
  MovMem64Reg(M, regRAX);
end;

procedure TX64Emitter.MovZXReg8(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($B6);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.MovZXReg16(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($B7);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.MovSXReg8(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($BE);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.MovSXReg16(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($BF);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.MovZXReg8Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM2($0F, $B6, True, Ord(Dst), M);
end;

procedure TX64Emitter.MovZXReg16Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM2($0F, $B7, True, Ord(Dst), M);
end;

procedure TX64Emitter.MovSXReg8Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM2($0F, $BE, True, Ord(Dst), M);
end;

procedure TX64Emitter.MovSXReg16Mem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM2($0F, $BF, True, Ord(Dst), M);
end;

procedure TX64Emitter.LeaRegMem(Dst: TX64Reg; const M: TX64Mem);
begin
  EmitRM($8D, True, Ord(Dst), M);
end;

procedure TX64Emitter.XchgRegReg(A, B: TX64Reg);
begin
  EmitRex(True, Ord(B), -1, Ord(A));
  EmitByte($87);
  EmitModRM(3, Ord(B), Ord(A));
end;

{ =====================================================================
  Arithmetic / logical
  ===================================================================== }

procedure TX64Emitter.AddRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($01, True, Dst, Src);
end;

procedure TX64Emitter.SubRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($2B, True, Dst, Src);
end;

procedure TX64Emitter.AdcRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($11, True, Dst, Src);
end;

procedure TX64Emitter.SbbRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($19, True, Dst, Src);
end;

procedure TX64Emitter.AndRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($21, True, Dst, Src);
end;

procedure TX64Emitter.OrRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($09, True, Dst, Src);
end;

procedure TX64Emitter.XorRegReg(Dst, Src: TX64Reg);
begin
  EmitRegReg($31, True, Dst, Src);
end;

procedure TX64Emitter.AddRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(0, Dst, Value);
end;

procedure TX64Emitter.SubRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(5, Dst, Value);
end;

procedure TX64Emitter.AndRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(4, Dst, Value);
end;

procedure TX64Emitter.OrRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(1, Dst, Value);
end;

procedure TX64Emitter.XorRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(6, Dst, Value);
end;

procedure TX64Emitter.AdcRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(2, Dst, Value);
end;

procedure TX64Emitter.SbbRegImm32(Dst: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(3, Dst, Value);
end;

procedure TX64Emitter.IncReg(R: TX64Reg);
begin
  EmitRex(True, 0, -1, Ord(R));
  EmitByte($FF);
  EmitModRM(3, 0, Ord(R));
end;

procedure TX64Emitter.DecReg(R: TX64Reg);
begin
  EmitRex(True, 1, -1, Ord(R));
  EmitByte($FF);
  EmitModRM(3, 1, Ord(R));
end;

procedure TX64Emitter.NegReg(R: TX64Reg);
begin
  EmitRex(True, 3, -1, Ord(R));
  EmitByte($F7);
  EmitModRM(3, 3, Ord(R));
end;

procedure TX64Emitter.NotReg(R: TX64Reg);
begin
  EmitRex(True, 2, -1, Ord(R));
  EmitByte($F7);
  EmitModRM(3, 2, Ord(R));
end;

procedure TX64Emitter.IMulRegReg(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($AF);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.IMulRegRegImm32(Dst, Src: TX64Reg; Value: LongInt);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($69);
  EmitModRM(3, Ord(Dst), Ord(Src));
  EmitU32(LongWord(Value));
end;

procedure TX64Emitter.MulReg(Src: TX64Reg);
begin
  EmitRex(True, 4, -1, Ord(Src));
  EmitByte($F7);
  EmitModRM(3, 4, Ord(Src));
end;

procedure TX64Emitter.DivReg(Src: TX64Reg);
begin
  EmitRex(True, 6, -1, Ord(Src));
  EmitByte($F7);
  EmitModRM(3, 6, Ord(Src));
end;

procedure TX64Emitter.IDivReg(Src: TX64Reg);
begin
  EmitRex(True, 7, -1, Ord(Src));
  EmitByte($F7);
  EmitModRM(3, 7, Ord(Src));
end;

procedure TX64Emitter.Cqo;
begin
  EmitByte($48);
  EmitByte($99);
end;

procedure TX64Emitter.AddMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(0, True, M, Imm);
end;

procedure TX64Emitter.SubMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(5, True, M, Imm);
end;

procedure TX64Emitter.AndMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(4, True, M, Imm);
end;

procedure TX64Emitter.OrMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(1, True, M, Imm);
end;

procedure TX64Emitter.XorMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(6, True, M, Imm);
end;

procedure TX64Emitter.CmpMem64Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(7, True, M, Imm);
end;

procedure TX64Emitter.AddMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(0, False, M, Imm);
end;

procedure TX64Emitter.SubMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(5, False, M, Imm);
end;

procedure TX64Emitter.AndMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(4, False, M, Imm);
end;

procedure TX64Emitter.OrMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(1, False, M, Imm);
end;

procedure TX64Emitter.XorMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(6, False, M, Imm);
end;

procedure TX64Emitter.CmpMem32Imm(const M: TX64Mem; Imm: LongInt);
begin
  EmitArithMemImm(7, False, M, Imm);
end;

{ =====================================================================
  Compare / test
  ===================================================================== }

procedure TX64Emitter.CmpRegReg(A, B: TX64Reg);
begin
  EmitRegReg($39, True, A, B);
end;

procedure TX64Emitter.CmpRegImm32(A: TX64Reg; Value: LongInt);
begin
  EmitGroup1Imm(7, A, Value);
end;

procedure TX64Emitter.TestRegReg(A, B: TX64Reg);
begin
  EmitRex(True, Ord(B), -1, Ord(A));
  EmitByte($85);
  EmitModRM(3, Ord(B), Ord(A));
end;

procedure TX64Emitter.TestRegImm32(A: TX64Reg; Value: LongInt);
begin
  EmitRex(True, 0, -1, Ord(A));
  EmitByte($F7);
  EmitModRM(3, 0, Ord(A));
  EmitU32(LongWord(Value));
end;

procedure TX64Emitter.Setcc(Condition: TX64Condition; Dst: TX64Reg);
begin
  { SETcc r/m8.
    This intentionally targets the low Byte of the GPR.
    With a REX prefix, the low-Byte registers are AL/CL/DL/BL
    and regR8B-R15B; AH/CH/DH/BH are not available. }

  EmitRex(False, 0, -1, Ord(Dst));
  EmitByte($0F);
  EmitByte($90 + Ord(Condition));
  EmitModRM(3, 0, Ord(Dst));
end;

procedure TX64Emitter.Sete(Dst: TX64Reg);
begin
  Setcc(ccE, Dst);
end;

procedure TX64Emitter.Setne(Dst: TX64Reg);
begin
  Setcc(ccNE, Dst);
end;

procedure TX64Emitter.Setg(Dst: TX64Reg);
begin
  Setcc(ccG, Dst);
end;

procedure TX64Emitter.Setge(Dst: TX64Reg);
begin
  Setcc(ccGE, Dst);
end;

procedure TX64Emitter.Setl(Dst: TX64Reg);
begin
  Setcc(ccL, Dst);
end;

procedure TX64Emitter.Setle(Dst: TX64Reg);
begin
  Setcc(ccLE, Dst);
end;

procedure TX64Emitter.Cmovcc(Condition: TX64Condition; Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($40 + Ord(Condition));
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.Cmove(Dst, Src: TX64Reg);
begin
  Cmovcc(ccE, Dst, Src);
end;

procedure TX64Emitter.Cmovne(Dst, Src: TX64Reg);
begin
  Cmovcc(ccNE, Dst, Src);
end;

procedure TX64Emitter.Cmovg(Dst, Src: TX64Reg);
begin
  Cmovcc(ccG, Dst, Src);
end;

procedure TX64Emitter.Cmovge(Dst, Src: TX64Reg);
begin
  Cmovcc(ccGE, Dst, Src);
end;

procedure TX64Emitter.Cmovl(Dst, Src: TX64Reg);
begin
  Cmovcc(ccL, Dst, Src);
end;

procedure TX64Emitter.Cmovle(Dst, Src: TX64Reg);
begin
  Cmovcc(ccLE, Dst, Src);
end;

{ =====================================================================
  Shifts / rotates
  ===================================================================== }

procedure TX64Emitter.ShlRegImm(Dst: TX64Reg; Count: Byte);
begin
  EmitGroup2Imm(4, Dst, Count);
end;

procedure TX64Emitter.ShrRegImm(Dst: TX64Reg; Count: Byte);
begin
  EmitGroup2Imm(5, Dst, Count);
end;

procedure TX64Emitter.SarRegImm(Dst: TX64Reg; Count: Byte);
begin
  EmitGroup2Imm(7, Dst, Count);
end;

procedure TX64Emitter.ShlRegCL(Dst: TX64Reg);
begin
  EmitGroup2CL(4, Dst);
end;

procedure TX64Emitter.ShrRegCL(Dst: TX64Reg);
begin
  EmitGroup2CL(5, Dst);
end;

procedure TX64Emitter.SarRegCL(Dst: TX64Reg);
begin
  EmitGroup2CL(7, Dst);
end;

procedure TX64Emitter.RolRegImm(Dst: TX64Reg; Count: Byte);
begin
  EmitGroup2Imm(0, Dst, Count);
end;

procedure TX64Emitter.RorRegImm(Dst: TX64Reg; Count: Byte);
begin
  EmitGroup2Imm(1, Dst, Count);
end;

procedure TX64Emitter.RolRegCL(Dst: TX64Reg);
begin
  EmitGroup2CL(0, Dst);
end;

procedure TX64Emitter.RorRegCL(Dst: TX64Reg);
begin
  EmitGroup2CL(1, Dst);
end;

{ =====================================================================
  Bit operations
  ===================================================================== }

procedure TX64Emitter.BTRegReg(Base, Bit: TX64Reg);
begin
  EmitRex(True, Ord(Bit), -1, Ord(Base));
  EmitByte($0F);
  EmitByte($A3);
  EmitModRM(3, Ord(Bit), Ord(Base));
end;

procedure TX64Emitter.BTSRegReg(Base, Bit: TX64Reg);
begin
  EmitRex(True, Ord(Bit), -1, Ord(Base));
  EmitByte($0F);
  EmitByte($AB);
  EmitModRM(3, Ord(Bit), Ord(Base));
end;

procedure TX64Emitter.BTRRegReg(Base, Bit: TX64Reg);
begin
  EmitRex(True, Ord(Bit), -1, Ord(Base));
  EmitByte($0F);
  EmitByte($B3);
  EmitModRM(3, Ord(Bit), Ord(Base));
end;

procedure TX64Emitter.BTCRegReg(Base, Bit: TX64Reg);
begin
  EmitRex(True, Ord(Bit), -1, Ord(Base));
  EmitByte($0F);
  EmitByte($BB);
  EmitModRM(3, Ord(Bit), Ord(Base));
end;

procedure TX64Emitter.BTRegImm(Base: TX64Reg; Bit: Byte);
begin
  EmitRex(True, 4, -1, Ord(Base));
  EmitByte($0F);
  EmitByte($BA);
  EmitModRM(3, 4, Ord(Base));
  EmitByte(Bit);
end;

procedure TX64Emitter.BTSRegImm(Base: TX64Reg; Bit: Byte);
begin
  EmitRex(True, 5, -1, Ord(Base));
  EmitByte($0F);
  EmitByte($BA);
  EmitModRM(3, 5, Ord(Base));
  EmitByte(Bit);
end;

procedure TX64Emitter.BTRRegImm(Base: TX64Reg; Bit: Byte);
begin
  EmitRex(True, 6, -1, Ord(Base));
  EmitByte($0F);
  EmitByte($BA);
  EmitModRM(3, 6, Ord(Base));
  EmitByte(Bit);
end;

procedure TX64Emitter.BTCRegImm(Base: TX64Reg; Bit: Byte);
begin
  EmitRex(True, 7, -1, Ord(Base));
  EmitByte($0F);
  EmitByte($BA);
  EmitModRM(3, 7, Ord(Base));
  EmitByte(Bit);
end;

procedure TX64Emitter.BsfRegReg(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($BC);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.BsrRegReg(Dst, Src: TX64Reg);
begin
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($BD);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.Bswap(R: TX64Reg);
begin
  EmitRex(True, 0, -1, Ord(R));
  EmitByte($0F);
  EmitByte($C8 + (Ord(R) and 7));
end;

{ =====================================================================
  Stack
  ===================================================================== }

procedure TX64Emitter.PushReg(R: TX64Reg);
var
  V: Integer;
begin
  V := Ord(R);

  if V >= 8 then
    EmitByte($41);

  EmitByte($50 + (V and 7));
end;

procedure TX64Emitter.PopReg(R: TX64Reg);
var
  V: Integer;
begin
  V := Ord(R);

  if V >= 8 then
    EmitByte($41);

  EmitByte($58 + (V and 7));
end;

procedure TX64Emitter.PushImm32(Value: LongInt);
begin
  EmitByte($68);
  EmitU32(LongWord(Value));
end;

procedure TX64Emitter.PushFlags;
begin
  EmitByte($9C);
end;

procedure TX64Emitter.PopFlags;
begin
  EmitByte($9D);
end;

procedure TX64Emitter.Enter(StackSize: Word; NestingLevel: Byte);
begin
  EmitByte($C8);
  EmitU16(StackSize);
  EmitByte(NestingLevel);
end;

procedure TX64Emitter.Leave;
begin
  EmitByte($C9);
end;

{ =====================================================================
  Calls / jumps
  ===================================================================== }

procedure TX64Emitter.CallReg(R: TX64Reg);
begin
  EmitRex(False, 2, -1, Ord(R));
  EmitByte($FF);
  EmitModRM(3, 2, Ord(R));
end;

procedure TX64Emitter.CallAbsolute(R: TX64Reg; Address: Pointer);
begin
  MovRegImm64(R, PtrUInt(Address));
  CallReg(R);
end;

procedure TX64Emitter.JmpReg(R: TX64Reg);
begin
  EmitRex(False, 4, -1, Ord(R));
  EmitByte($FF);
  EmitModRM(3, 4, Ord(R));
end;

procedure TX64Emitter.JmpAbsolute(R: TX64Reg; Address: Pointer);
begin
  MovRegImm64(R, PtrUInt(Address));
  JmpReg(R);
end;

procedure TX64Emitter.Ret;
begin
  EmitByte($C3);
end;

procedure TX64Emitter.RetImm16(Value: Word);
begin
  EmitByte($C2);
  EmitU16(Value);
end;

{ =====================================================================
  CPU/system
  ===================================================================== }

procedure TX64Emitter.Syscall;
begin
  EmitByte($0F);
  EmitByte($05);
end;

procedure TX64Emitter.Sysret;
begin
  EmitByte($0F);
  EmitByte($07);
end;

procedure TX64Emitter.Cpuid;
begin
  EmitByte($0F);
  EmitByte($A2);
end;

procedure TX64Emitter.Rdtsc;
begin
  EmitByte($0F);
  EmitByte($31);
end;

procedure TX64Emitter.Rdtscp;
begin
  EmitByte($0F);
  EmitByte($01);
  EmitByte($F9);
end;

{ =====================================================================
  SSE helpers
  ===================================================================== }

procedure TX64Emitter.EmitSSEMem(Prefix: Byte; Opcode1, Opcode2: Byte;
  XMM: TXMMReg; const M: TX64Mem);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(False, Ord(XMM), M.Index, M.Base);

  EmitByte(Opcode1);
  EmitByte(Opcode2);

  EmitMemModRM(Ord(XMM), M);
end;

procedure TX64Emitter.EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte;
  Dst, Src: TXMMReg);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(False, Ord(Dst), -1, Ord(Src));

  EmitByte(Opcode1);
  EmitByte(Opcode2);

  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte;
  Dst: TXMMReg; Src: TX64Reg);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(True, Ord(Dst), -1, Ord(Src));

  EmitByte(Opcode1);
  EmitByte(Opcode2);

  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.EmitSSEReg(Prefix: Byte; Opcode1, Opcode2: Byte;
  Dst: TX64Reg; Src: TXMMReg);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(True, Ord(Src), -1, Ord(Dst));

  EmitByte(Opcode1);
  EmitByte(Opcode2);

  EmitModRM(3, Ord(Src), Ord(Dst));
end;

procedure TX64Emitter.EmitSSEMemImm8(Prefix: Byte; Opcode1, Opcode2: Byte;
  XMM: TXMMReg; const M: TX64Mem; Imm8: Byte);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(False, Ord(XMM), M.Index, M.Base);

  EmitByte(Opcode1);
  EmitByte(Opcode2);

  EmitMemModRM(Ord(XMM), M);

  EmitByte(Imm8);
end;

procedure TX64Emitter.EmitSSEMemImm8(Prefix: Byte; Opcode1, Opcode2, Opcode3: Byte;
  XMM: TXMMReg; const M: TX64Mem; Imm8: Byte);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(False, Ord(XMM), M.Index, M.Base);

  EmitByte(Opcode1);
  EmitByte(Opcode2);
  EmitByte(Opcode3);

  EmitMemModRM(Ord(XMM), M);

  EmitByte(Imm8);
end;

procedure TX64Emitter.EmitSSERegImm8(Prefix: Byte; Opcode1, Opcode2, Opcode3: Byte; Dst, Src: TXMMReg; Imm8: Byte);
begin
  if Prefix <> 0 then
    EmitByte(Prefix);

  EmitRex(False, Ord(Dst), -1, Ord(Src));

  EmitByte(Opcode1);
  EmitByte(Opcode2);
  EmitByte(Opcode3);

  EmitModRM(3, Ord(Dst), Ord(Src));

  EmitByte(Imm8);
end;

procedure TX64Emitter.EmitArithMemImm(Group: Byte; W: Boolean; const M: TX64Mem; Imm: Int64);
begin
  if Group > 7 then
    raise Exception.Create('Invalid arithmetic group');

  EmitRex(W, 0, M.Index, M.Base);

  if (Imm >= -128) and (Imm <= 127) then
  begin
    EmitByte($83);
    EmitMemModRM(Group, M);
    EmitI8(ShortInt(Imm));
  end
  else
  begin
    if (Imm < -2147483648) or (Imm > 2147483647) then
      raise Exception.Create('Immediate does not fit signed 32-bit');

    EmitByte($81);
    EmitMemModRM(Group, M);
    EmitU32(LongWord(Int32(Imm)));
  end;
end;

{ =====================================================================
  SSE2 scalar double
  ===================================================================== }

procedure TX64Emitter.MovSDXMMFromMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F2, $0F, $10, Dst, M);
end;

procedure TX64Emitter.MovSDMemFromXMM(const M: TX64Mem; Src: TXMMReg);
begin
  EmitSSEMem($F2, $0F, $11, Src, M);
end;

procedure TX64Emitter.MovSDXMMFromReg(Dst: TXMMReg; Src: TX64Reg);
begin
  EmitSSEReg($66, $0F, $6E, Dst, Src);
end;

procedure TX64Emitter.MovRegFromSDXMM(Dst: TX64Reg; Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $7E, Dst, Src);
end;

procedure TX64Emitter.MovSDXMM(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $10, Dst, Src);
end;

procedure TX64Emitter.AddSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $58, Dst, Src);
end;

procedure TX64Emitter.SubSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $5C, Dst, Src);
end;

procedure TX64Emitter.MulSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $59, Dst, Src);
end;

procedure TX64Emitter.DivSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $5E, Dst, Src);
end;

procedure TX64Emitter.SqrtSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $51, Dst, Src);
end;

procedure TX64Emitter.MinSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $5D, Dst, Src);
end;

procedure TX64Emitter.MaxSD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($F2, $0F, $5F, Dst, Src);
end;

procedure TX64Emitter.AddSDMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F2, $0F, $58, Dst, M);
end;

procedure TX64Emitter.SubSDMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F2, $0F, $5C, Dst, M);
end;

procedure TX64Emitter.MulSDMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F2, $0F, $59, Dst, M);
end;

procedure TX64Emitter.DivSDMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F2, $0F, $5E, Dst, M);
end;

procedure TX64Emitter.ComISD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $2E, Dst, Src);
end;

procedure TX64Emitter.CvtSI2SD(Dst: TXMMReg; Src: TX64Reg);
begin
  EmitByte($F2);
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($2A);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.CvttSD2SI(Dst: TX64Reg; Src: TXMMReg);
begin
  EmitByte($F2);
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($2C);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.CvtSD2SI(Dst: TX64Reg; Src: TXMMReg);
begin
  EmitByte($F2);
  EmitRex(True, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($2D);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.RoundSD(Dst, Src: TXMMReg; Rounding: Byte);
begin
  EmitSSERegImm8($66, $0F, $3A, $0B, Dst, Src, Rounding);
end;

procedure TX64Emitter.RoundSDMem(Dst: TXMMReg; const M: TX64Mem; Rounding: Byte);
begin
  EmitSSEMemImm8($66, $0F, $3A, $0B, Dst, M, Rounding);
end;

procedure TX64Emitter.XorPD(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $57, Dst, Src);
end;

procedure TX64Emitter.XorPDMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($66, $0F, $57, Dst, M);
end;

{ =====================================================================
  SSE2 packed operations
  ===================================================================== }

procedure TX64Emitter.MovDQU(Dst, Src: TXMMReg);
begin
  EmitByte($F3);
  EmitRex(False, Ord(Dst), -1, Ord(Src));
  EmitByte($0F);
  EmitByte($6F);
  EmitModRM(3, Ord(Dst), Ord(Src));
end;

procedure TX64Emitter.MovDQUFromMem(Dst: TXMMReg; const M: TX64Mem);
begin
  EmitSSEMem($F3, $0F, $6F, Dst, M);
end;

procedure TX64Emitter.MovDQUMem(const M: TX64Mem; Src: TXMMReg);
begin
  EmitSSEMem($F3, $0F, $7F, Src, M);
end;

procedure TX64Emitter.Pxor(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $EF, Dst, Src);
end;

procedure TX64Emitter.Pand(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $DB, Dst, Src);
end;

procedure TX64Emitter.Por(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $EB, Dst, Src);
end;

procedure TX64Emitter.PaddQ(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $D4, Dst, Src);
end;

procedure TX64Emitter.PsubQ(Dst, Src: TXMMReg);
begin
  EmitSSEReg($66, $0F, $FB, Dst, Src);
end;

{ =====================================================================
  Executable memory
  ===================================================================== }

function TX64Emitter.MakeExecutable: Pointer;
var
  Size: NativeUInt;
  AllocSize: NativeUInt;
  P: Pointer;
{$ifdef WINDOWS}
  OldProtect: DWord;
{$endif}
begin
  if Self.FCode.Count = 0 then
    raise Exception.Create('Cannot execute empty code');

  if FExecutableMemory <> nil then
    raise Exception.Create('Code is already executable');

  ResolveLabels;
  Size := NativeUInt(Self.FCode.Count);
  { Round up to page size (normally 4 KiB). }
  AllocSize := (Size + 4095) and not NativeUInt(4095);
  Self.FExecutableSize := AllocSize;

{$ifdef WINDOWS}
  P := VirtualAlloc(nil, AllocSize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);

  if P = nil then
    RaiseLastOSError;

  Move(Self.FCode.Ptr(0)^, P^, Size);
  if not VirtualProtect(P, AllocSize, PAGE_EXECUTE_READ, OldProtect) then
  begin
    VirtualFree(P, 0, MEM_RELEASE);
    RaiseLastOSError;
  end;
  FlushInstructionCache(GetCurrentProcess, P, Size);

{$else}
  { Unix / Linux path }
  P := fpmmap(nil, AllocSize, PROT_READ or PROT_WRITE, MAP_PRIVATE or MAP_ANONYMOUS, -1, 0);

  if P = MAP_FAILED then          { MAP_FAILED = Pointer(-1) }
    RaiseLastOSError;

  Move(Self.FCode.Ptr(0)^, P^, Size);
  if FpMProtect(P, AllocSize, PROT_READ or PROT_EXEC) <> 0 then
  begin
    FpMunMap(P, AllocSize);
    RaiseLastOSError;
  end;
  { Instruction cache is coherent on x86/x86-64; no explicit flush needed. }
{$endif}

  Result := P;
end;

{ ===================================================================== }

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

function StringIndexOf(S, P: String): NativeInt; inline;
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
  I: NativeInt = 0;
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
        Result := 'buffer@' + IntToStr(NativeUInt(Value.VarBuffer^.Ptr));
        if Value.VarBuffer^.Base <> nil then
        begin
          Result := Result + ' <' + IntToStr(MemSize(Value.VarBuffer^.Base) - 16) + ' bytes>';
        end;
      end;
    sevkPointer:
      begin
        Result := IntToStr(NativeInt(Value.VarPointer));
      end;
    sevkPascalObject:
      begin
        Result := 'pasobject@' + IntToStr(NativeUInt(Value.VarPascalObject^.Value));
      end;
    sevkConstString:
      Result := '.' + ConstStrings.Ptr(Value.VarConstStringIndex)^;
    else
      Result := Value;
  end;
end;

function SESize(constref Value: TSEValue): SizeInt; inline;
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

procedure SEMapDelete(constref V: TSEValue; const I: NativeInt); inline; overload;
begin
  TSEValueMap(V.VarMap).Del2(I);
end;

procedure SEMapDelete(constref V: TSEValue; constref S: String); inline; overload;
begin
  TSEValueMap(V.VarMap).Del2(@S);
end;

procedure SEMapDelete(constref V, I: TSEValue); inline; overload;
begin
  case I.Kind of
    sevkString:
      begin
        TSEValueMap(V.VarMap).Del2(I.VarString);
      end;
    sevkNumber:
      begin
        TSEValueMap(V.VarMap).Del2(Round(I.VarNumber));
      end;
  end;
end;



function SEMapGet(constref V: TSEValue; const I: NativeInt): TSEValue; inline; overload;
begin
  Result := TSEValueMap(V.VarMap).Items[I];
end;

function SEMapGet(constref V: TSEValue; constref S: String): TSEValue; inline; overload;
begin
  Result := TSEValueMap(V.VarMap).Get2(@S);
end;

function SEMapGet(constref V, I: TSEValue): TSEValue; inline; overload;
begin
  case I.Kind of
    sevkString:
      begin
        Result := TSEValueMap(V.VarMap).Get2(I.VarString);
      end;
    sevkNumber:
      begin
        Result := TSEValueMap(V.VarMap).Get2(Round(I.VarNumber));
      end;
    sevkConstString:
      begin
        Result := TSEValueMap(V.VarMap).Get2(ConstStrings.Ptr(I.VarConstStringIndex));
      end;
    else
      Exit(SENull);
  end;
end;

procedure SEMapGet(out R: TSEValue; constref V, I: TSEValue); inline; overload;
begin
  case I.Kind of
    sevkString:
      begin
        R := TSEValueMap(V.VarMap).Get2(I.VarString);
      end;
    sevkNumber:
      begin
        R := TSEValueMap(V.VarMap).Get2(Round(I.VarNumber));
      end;
    sevkConstString:
      begin
        R := TSEValueMap(V.VarMap).Get2(ConstStrings.Ptr(I.VarConstStringIndex));
      end;
    else
      R := SENull;
  end;
end;

procedure SEMapSet(constref V: TSEValue; const I: NativeInt; constref A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(I, A);
end;

procedure SEMapSet(constref V: TSEValue; constref S: String; constref A: TSEValue); inline; overload;
begin
  TSEValueMap(V.VarMap).Set2(@S, A);
end;

procedure SEMapSet(constref V, I: TSEValue; constref A: TSEValue); inline; overload;
begin
  case I.Kind of
    sevkString:
      TSEValueMap(V.VarMap).Set2(I.VarString, A);
    sevkNumber:
      TSEValueMap(V.VarMap).Set2(Round(I.VarNumber), A);
    sevkConstString:
      TSEValueMap(V.VarMap).Set2(ConstStrings.Ptr(I.VarConstStringIndex), A);
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
  I, J, K: NativeInt;
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
        Op := TSEOpcode(NativeUInt(Binary[I].VarPointer));
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
    for I := 0 to ConstStrings.Count - 1 do
    begin
      S := ConstStrings[I];
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
  I: NativeInt;
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
              SEMapSet(Result, Key, TSEValueMap(V.VarMap).Get2(@Key));
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

constructor TSEBinary.Create;
begin
  inherited;
end;

destructor TSEBinary.Destroy;
begin
  inherited;
end;

procedure TSEValueHelper.AllocBuffer(constref Size: NativeInt); inline;
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

function TSEValueHelper.GetValue(constref I: NativeInt): TSEValue; inline; overload;
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

procedure TSEValueHelper.SetValue(constref I: NativeInt; const A: TSEValue); inline; overload;
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

function TSEValueHelper.GetProp(I: TSEValue): TSEValue;
var
  Obj: TObject;
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  PName: String;
begin
  Obj := Self.VarPascalObject^.Value;
  Ctx := TRttiContext.Create;
  try
    case I.Kind of
      sevkString:
        PName := I.VarString^;
      sevkConstString:
        PName := ConstStrings.Ptr(I.VarConstStringIndex)^;
    end;
    RttiType := Ctx.GetType(Obj.ClassType);
    for Prop in RttiType.GetProperties do
    begin
      if Prop.Name = PName then
      begin
        Result := Prop.GetValue(Obj);
        break;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

procedure TSEValueHelper.SetProp(I: TSEValue; const A: TSEValue);
var
  Obj: TObject;
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Prop: TRttiProperty;
  PName: String;
  V: TValue;
begin
  Obj := Self.VarPascalObject^.Value;
  Ctx := TRttiContext.Create;
  try
    case I.Kind of
      sevkString:
        PName := I.VarString^;
      sevkConstString:
        PName := ConstStrings.Ptr(I.VarConstStringIndex)^;
    end;
    RttiType := Ctx.GetType(Obj.ClassType);
    for Prop in RttiType.GetProperties do
    begin
      if Prop.Name = PName then
      begin
        case Prop.PropertyType.TypeKind of
          tkSet,
          tkInteger,
          tkQWord,
          tkInt64:
            V := Round(A.VarNumber);
          tkFloat:
            V := A.VarNumber;
          tkBool:
            V := Boolean(Round(A.VarNumber));
          tkLString,
          tkAString,
          tkWString,
          tkSString:
            V := A.VarString^;
          tkUChar,
          tkWChar,
          tkChar:
            V := Char(Round(A.VarNumber));
          tkObject:
            V := A.VarPascalObject^.Value;
          else
          begin
            WriteStr(PName, Prop.PropertyType.TypeKind);
            raise Exception.Create('Type "' + PName + '" not supported');
          end;
        end;
        Prop.SetValue(Obj, V);
        break;
      end;
    end;
  finally
    Ctx.Free;
  end;
end;

function TSEValueHelper.Invoke(constref MethodName: String; const Args: PSEValue; const ArgCount: NativeInt): TSEValue;
var
  Obj: TObject;
  MethodArgs: array of TValue;
  Ctx: TRttiContext;
  RttiType: TRttiType;
  Method: TRttiMethod;
  I: NativeInt;
begin
  Obj := Self.VarPascalObject^.Value;
  Ctx := TRttiContext.Create;
  try
    RttiType := Ctx.GetType(Obj.ClassType);
    Method := RttiType.GetMethod(MethodName);
    if Method <> nil then
    begin
      SetLength(MethodArgs, ArgCount);
      for I := 0 to ArgCount - 1 do
        MethodArgs[I] := Args[I];
      Result := Method.Invoke(Obj, MethodArgs);
    end else
      raise Exception.Create('Method "' + MethodName + '" not found!');
  finally
    Ctx.Free;
  end;
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
  Self := TBuiltInFunction(nil).SEJSONParse(nil, @V, 1, nil);
end;

function TSEValueHelper.ToJSON: String;
begin
  Result := TBuiltInFunction(nil).SEJSONStringify(nil, @Self, 1, nil);
end;

function TSEValueHelper.ToString: String;
begin
  Result := SEValueToText(Self);
end;

function TSEValueHelper.Size: SizeInt;
begin
  Result := SESize(Self);
end;

class function TBuiltInFunction.SEBufferCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkNumber, 1, {$I %CURRENTROUTINE%});
  GC.AllocBuffer(@Result, Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEBufferLength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result := SESize(Args[0]);
end;

class function TBuiltInFunction.SEBufferCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkBuffer, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  Move(Args[1].VarBuffer^.Ptr^, Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillChar(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Byte(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Word(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillDWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), DWord(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), NativeUInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillChar(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), ShortInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), SmallInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillDWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), LongInt(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), Int64(Round(Args[1].VarNumber)));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferFillF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEBufferFillF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  V: Double;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkNumber, 2, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[2], sevkNumber, 3, {$I %CURRENTROUTINE%});
  V := Args[1].VarNumber;
  FillQWord(Args[0].VarBuffer^.Ptr^, Round(Args[2].VarNumber), NativeUInt((@V)^));
  Result := Args[0];
end;

class function TBuiltInFunction.SEBufferGetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Byte((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Word((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := LongWord((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := NativeUInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := ShortInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := SmallInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := LongInt((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := Int64((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferGetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := TSENumber(Single((Args[0].VarBuffer^.Ptr)^));
end;

class function TBuiltInFunction.SEBufferGetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Result.Kind := sevkNumber;
  Result.VarNumber := TSENumber((Args[0].VarBuffer^.Ptr)^);
end;

class function TBuiltInFunction.SEBufferSetU8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Byte(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetU16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Word(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetU32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEBufferSetU64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  case Args[1].Kind of
    sevkBuffer:
      NativeUInt(Args[0].VarBuffer^.Ptr^) := NativeUInt(Args[1].VarBuffer^.Ptr);
    else
      NativeUInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  end;
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI8(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  ShortInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI16(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  P: Pointer;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  P := Pointer(Round(Args[0].VarNumber));
  SmallInt(P^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  LongInt(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetI64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Int64(Args[0].VarBuffer^.Ptr^) := Round(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  Single(Args[0].VarBuffer^.Ptr^) := Single(Args[1].VarNumber);
  Result := SENull;
end;

class function TBuiltInFunction.SEBufferSetF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  TSENumber(Args[0].VarBuffer^.Ptr^) := Args[1];
  Result := SENull;
end;

class function TBuiltInFunction.SEStringToBuffer(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkString, 1, {$I %CURRENTROUTINE%});
  GC.AllocBuffer(@Result, Length(Args[0].VarString^));
  Move(Args[0].VarString^[1], PByte(Result.VarBuffer^.Ptr)[0], Length(Args[0].VarString^));
end;

class function TBuiltInFunction.SEBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  S: String;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  S := PChar(Args[0].VarBuffer^.Ptr);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SEWBufferToString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  WS: UnicodeString;
  S: String;
begin
  SEValidateType(@Args[0], sevkBuffer, 1, {$I %CURRENTROUTINE%});
  WS := PWideChar(Args[0].VarBuffer^.Ptr);
  S := UTF8Encode(WS);
  GC.AllocString(@Result, S);
end;

class function TBuiltInFunction.SEArrayToBufferF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
  Size: NativeUInt;
begin
  SEValidateType(@Args[0], sevkMap, 1, {$I %CURRENTROUTINE%});
  Size := SESize(Args[0]);
  GC.AllocBuffer(@Result, Size * 4);
  for I := 0 to Size - 1 do
  begin
    Single((Result.VarBuffer^.Ptr + I * 4)^) := SEMapGet(Args[0], I).VarNumber;
  end;
end;

class function TBuiltInFunction.SEArrayToBufferF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
  Size: NativeUInt;
begin
  SEValidateType(@Args[0], sevkMap, 1, {$I %CURRENTROUTINE%});
  Size := SESize(Args[0]);
  GC.AllocBuffer(@Result, Size * 8);
  for I := 0 to Size - 1 do
  begin
    Double((Result.VarBuffer^.Ptr + I * 8)^) := SEMapGet(Args[0], I).VarNumber;
  end;
end;

class function TBuiltInFunction.SEBufferToArrayF32(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
  Size: NativeUInt;
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

class function TBuiltInFunction.SEBufferToArrayF64(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
  Size: NativeUInt;
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

class function TBuiltInFunction.SETypeOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEKindOf(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := TSENumber(NativeInt(Args[0].Kind));
end;

class function TBuiltInFunction.SEWrite(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
begin
  if ArgCount > 0 then
    for I := 0 to ArgCount - 1 do
      Write(SEValueToText(Args[I]));
  Result := SENull;
end;

class function TBuiltInFunction.SEWriteln(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
begin
  TBuiltInFunction.SEWrite(VM, Args, ArgCount, nil);
  Writeln;
  Result := SENull;
end;

class function TBuiltInFunction.SERandom(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Random(Round(Args[0].VarNumber)));
end;

class function TBuiltInFunction.SERnd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Random);
end;

class function TBuiltInFunction.SERound(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SERoundTo(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(RoundTo(Args[0].VarNumber, Round(Args[1].VarNumber)));
end;

class function TBuiltInFunction.SEFloor(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Floor(Args[0].VarNumber));
end;

class function TBuiltInFunction.SECeil(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Ceil(Args[0].VarNumber));
end;

class function TBuiltInFunction.SETrunc(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Trunc(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SESet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEString(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(SEValueToText(Args[0]));
end;

class function TBuiltInFunction.SENumber(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(PointStrToFloat(Trim(Args[0])));
end;

class function TBuiltInFunction.SELength(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEMapCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt = 0;
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

class function TBuiltInFunction.SEMapClone(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(SEClone(Args[0]));
end;

class function TBuiltInFunction.SEMapKeyDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Args[0];
  SEMapDelete(Result, Args[1]);
end;

class function TBuiltInFunction.SEMapKeysGet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Key: String;
  I: NativeInt = 0;
begin
  SEValidateType(@Args[0], sevkMap, 1, {$I %CURRENTROUTINE%});
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

class function TBuiltInFunction.SEMapClear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).Clear;
  end else
  begin
    TSEValueMap(Args[0].VarMap).Map.Clear;
  end;
end;

class function TBuiltInFunction.SEArrayResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    TSEValueMap(Args[0].VarMap).Count := Args[1];
  end;
  Result := Args[0];
end;

class function TBuiltInFunction.SEArrayToMap(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  if Args[0].Kind = sevkMap then
    TSEValueMap(Args[0].VarMap).ToMap;
  Result := Args[0];
end;

class function TBuiltInFunction.SEArrayFill(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
begin
  if SEMapIsValidArray(Args[0]) then
  begin
    for I := 0 to TSEValueMap(Args[0].VarMap).Count - 1 do
      TSEValueMap(Args[0].VarMap)[I] := Args[1];
  end;
  Result := Args[0];
end;

class function TBuiltInFunction.SELerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  A, B, T: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  Exit(A + (B - A) * T);
end;

class function TBuiltInFunction.SESLerp(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  A, B, T, T2: TSENumber;
begin
  A := Args[0];
  B := Args[1];
  T := Args[2];
  T2 := (1 - Cos(T * PI)) * 0.5;
  Exit(A * (1 - T2) + B * T2);
end;

class function TBuiltInFunction.SESign(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Sign(Args[0].VarNumber));
end;

class function TBuiltInFunction.SERange(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
  function EpsilonRound(V: TSENumber): TSENumber;
  begin
    if Abs(Frac(V)) < 1E-12 then
      Result := Round(V)
    else
      Result := V;
  end;

var
  V: TSENumber;
  I: NativeInt = 0;
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

class function TBuiltInFunction.SEMin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
begin
  for I := 0 to ArgCount - 2 do
    if Args[I] < Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEMax(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
begin
  for I := 0 to ArgCount - 2 do
    if Args[I] > Args[I + 1] then
      Result := Args[I]
    else
      Result := Args[I + 1];
end;

class function TBuiltInFunction.SEPow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Power(Args[0].VarNumber, Args[1].VarNumber));
end;

class function TBuiltInFunction.SESleep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Sleep(Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEStringGrep(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
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

class function TBuiltInFunction.SEStringResize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Args[0];
  SetLength(Result.VarString^, Round(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEStringSplit(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  D: TStringDynArray;
  I: NativeInt;
begin
  D := SplitString(Args[0], Args[1]);
  GC.AllocMap(@Result);
  for I := 0 to Length(D) - 1 do
    SEMapSet(Result, I, D[I]);
end;

class function TBuiltInFunction.SEStringFind(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := StringIndexOf(Args[0].VarString^, Args[1]);
end;

class function TBuiltInFunction.SEStringDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Delete(AnsiString(Args[0].VarString^), Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$else}
  Delete(Args[0].VarString^, Round(Args[1].VarNumber + 1), Round(Args[2].VarNumber));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringCompare(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := CompareStr(Args[0].VarString^, Args[1].VarString^);
end;

class function TBuiltInFunction.SEStringInsert(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  {$ifdef SE_STRING_UTF8}
  UTF8Insert(AnsiString(Args[1].VarString^), AnsiString(Args[0].VarString^), Round(Args[2].VarNumber + 1));
  {$else}
  Insert(Args[1].VarString^, Args[0].VarString^, Round(Args[2].VarNumber + 1));
  {$endif}
  Result := Args[0].VarString^;
end;

class function TBuiltInFunction.SEStringReplace(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  S: String;
begin
  S := StringReplace(Args[0], Args[1], Args[2], [rfReplaceAll]);
  Result := S;
end;

class function TBuiltInFunction.SEStringReplaceIgnoreCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  S: String;
begin
  S := StringReplace(Args[0], Args[1], Args[2], [rfReplaceAll, rfIgnoreCase]);
  Result := S;
end;

class function TBuiltInFunction.SEStringFormat(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  I: NativeInt;
  S: String;
begin
  S := Args[0].VarString^;
  for I := 1 to ArgCount do
  begin
    S := StringReplace(S, '{' + IntToStr(I - 1) + '}', SEValueToText(Args[1]), [rfReplaceAll]);
  end;
  Result := S;
end;

class function TBuiltInFunction.SEStringUpperCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := UpperCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := UpperCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringLowerCase(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := '';
  case Args[0].Kind of
    sevkString: Result := LowerCase(Args[0].VarString^);
    sevkBoolean,
    sevkNumber: Result := LowerCase(Char(Round(Args[0].VarNumber)));
  end;
end;

class function TBuiltInFunction.SEStringFindRegex(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  R: TRegExpr;
  I: NativeInt;
  C: NativeInt = 0;
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

class function TBuiltInFunction.SEStringTrim(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Trim(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimLeft(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := TrimLeft(Args[0]);
end;

class function TBuiltInFunction.SEStringTrimRight(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := TrimRight(Args[0]);
end;

class function TBuiltInFunction.SEStringExtractName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := ExtractFileName(Args[0].VarString^);
end;

class function TBuiltInFunction.SEStringExtractPath(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := ExtractFilePath(Args[0].VarString^);
end;

class function TBuiltInFunction.SEStringExtractExt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := ExtractFileExt(Args[0].VarString^);
end;

class function TBuiltInFunction.SESin(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Sin(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECos(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Cos(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SETan(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Tan(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SECot(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Cot(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SESqrt(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Sqrt(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEAbs(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Abs(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEFrac(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(Frac(TSENumber(Args[0])));
end;

class function TBuiltInFunction.SEGetTickCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Exit(GetTickCount64);
end;

class function TBuiltInFunction.SEDTNow(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Now;
end;

class function TBuiltInFunction.SEDTSetDate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := EncodeDate(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber));
end;

class function TBuiltInFunction.SEDTSetTime(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := EncodeTime(Round(Args[0].VarNumber), Round(Args[1].VarNumber), Round(Args[2].VarNumber), Round(Args[3].VarNumber));
end;

class function TBuiltInFunction.SEDTDayAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := IncDay(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTMonthAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := IncMonth(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTYearAdd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := IncYear(Args[0].VarNumber, Round(Args[1].VarNumber));
end;

class function TBuiltInFunction.SEDTGetYear(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := Y;
end;

class function TBuiltInFunction.SEDTGetMonth(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := M;
end;

class function TBuiltInFunction.SEDTGetDay(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Y, M, D: Word;
begin
  DecodeDate(Args[0].VarNumber, Y, M, D);
  Result := D;
end;

class function TBuiltInFunction.SEDTGetHour(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := H;
end;

class function TBuiltInFunction.SEDTGetMinute(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  H, M ,S, MS: Word;
begin
  DecodeTime(Args[0].VarNumber, H, M, S, MS);
  Result := M;
end;

class function TBuiltInFunction.SEGCObjectCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := GC.ObjectCount;
end;

class function TBuiltInFunction.SEGCObjectOldCount(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := GC.OldObjectCount;
end;

class function TBuiltInFunction.SEGCCollect(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  GC.GC(True);
  Result := SENull;
end;

class function TBuiltInFunction.SEChar(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Char(Floor(Args[0].VarNumber));
end;

class function TBuiltInFunction.SEOrd(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := Byte(Args[0].VarString^[1]);
end;

class function TBuiltInFunction.SECoroutineCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Coroutine: TSEVMCoroutine;
begin
  SEValidateType(@Args[0], sevkFunction, 1, {$I %CURRENTROUTINE%});
  Coroutine := TSEVMCoroutine.Create(VM, Args[0], @Args[1], ArgCount - 1, SEThreadStackSize);
  GC.AllocPascalObject(@Result, Coroutine, True);
  // Push "self" onto stack
  Coroutine.VM.Stack[(SE_STACK_RESERVED - 1) + ArgCount] := Result;
end;

class function TBuiltInFunction.SECoroutineReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkFunction, 2, {$I %CURRENTROUTINE%});
  TSEVMCoroutine(Args[0].VarPascalObject^.Value).Reset(Args[1], @Args[2], ArgCount - 3, nil);
end;

class function TBuiltInFunction.SECoroutineResume(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).Execute;
end;

class function TBuiltInFunction.SECoroutineIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsTerminated;
end;

class function TBuiltInFunction.SECoroutineTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsTerminated := True;
end;

class function TBuiltInFunction.SECoroutineIsExecuting(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMCoroutine(Args[0].VarPascalObject^.Value).IsExecuting;
end;

{$ifdef SE_THREADS}
class function TBuiltInFunction.SEThreadCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Thread: TSEVMThread;
begin
  SEValidateType(@Args[0], sevkFunction, 1, {$I %CURRENTROUTINE%});
  Thread := TSEVMThread.Create(VM, Args[0], @Args[1], ArgCount - 1, SEThreadStackSize);
  GC.AllocPascalObject(@Result, Thread, True);
  // Push "self" onto stack
  Thread.VM.Stack[(SE_STACK_RESERVED - 1) + ArgCount] := Result;
end;

class function TBuiltInFunction.SEThreadStart(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  while TSEVMThread(Args[0].VarPascalObject^.Value).IsRequestForSuspendByGC do Sleep(1);
  TSEVMThread(Args[0].VarPascalObject^.Value).Start;
end;

class function TBuiltInFunction.SEThreadIsTerminated(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSEVMThread(Args[0].VarPascalObject^.Value).Terminated;
end;

class function TBuiltInFunction.SEThreadSuspend(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  if not TSEVMThread(Args[0].VarPascalObject^.Value).Terminated then
    TSEVMThread(Args[0].VarPascalObject^.Value).Suspend;
end;

class function TBuiltInFunction.SEThreadTerminate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  if not TSEVMThread(Args[0].VarPascalObject^.Value).Terminated then
    TSEVMThread(Args[0].VarPascalObject^.Value).Terminate;
end;

class function TBuiltInFunction.SEThreadWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TSEVMThread(Args[0].VarPascalObject^.Value).WaitFor;
end;

class function TBuiltInFunction.SECriticalCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Critical: TCriticalSection;
begin
  Critical := TCriticalSection.Create;
  GC.AllocPascalObject(@Result, Critical, True);
end;

class function TBuiltInFunction.SECriticalEnter(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TCriticalSection(Args[0].VarPascalObject^.Value).Enter;
end;

class function TBuiltInFunction.SECriticalLeave(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TCriticalSection(Args[0].VarPascalObject^.Value).Leave;
end;

class function TBuiltInFunction.SECriticalTry(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TCriticalSection(Args[0].VarPascalObject^.Value).TryEnter;
end;

class function TBuiltInFunction.SEEventCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  Event: TEventObject;
begin
  Event := TEvent.Create(nil, True, False, '');
  GC.AllocPascalObject(@Result, Event, True);
end;

class function TBuiltInFunction.SEEventSet(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TEventObject(Args[0].VarPascalObject^.Value).SetEvent;
end;

class function TBuiltInFunction.SEEventWait(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TSENumber(NativeInt(TEventObject(Args[0].VarPascalObject^.Value).WaitFor(Round(Args[1].VarNumber))));
end;

class function TBuiltInFunction.SEEventReset(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  TEventObject(Args[0].VarPascalObject^.Value).ResetEvent;
end;
{$endif}

class function TBuiltInFunction.SEFileReadText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := ReadFileAsString(Args[0]);
end;

class function TBuiltInFunction.SEFileReadBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEFileWriteText(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEFileWriteBinary(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  FS: TFileStream;
begin
  if FileExists(Args[0].VarString^) then
    FS := TFileStream.Create(Args[0], fmOpenWrite)
  else
    FS := TFileStream.Create(Args[0], fmCreate);
  try
    FS.Position := FS.Size;
    FS.Write(Args[1].VarBuffer^.Ptr^, Round(Args[2].VarNumber));
  finally
    FS.Free;
  end;
end;

class function TBuiltInFunction.SEFileCopy(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := False;
  {$ifdef SE_HAS_FILEUTIL}
  if FileExists(Args[0].VarString^) then
  begin
    Result := CopyFile(Args[0].VarString^, Args[1], [cffOverwriteFile], False);
  end;
  {$endif}
end;

class function TBuiltInFunction.SEFileExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := FileExists(Args[0].VarString^);
end;

class function TBuiltInFunction.SEFileDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  DeleteFile(Args[0].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEFileRename(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  RenameFile(Args[0].VarString^, Args[1].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEFileFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  SL: TStringList;
  I: NativeInt;
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

class function TBuiltInFunction.SEFileGetSize(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
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

class function TBuiltInFunction.SEFileGetAge(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  F: File of Byte;
begin
  Result := -1;
  if FileExists(Args[0].VarString^) then
  begin
    Result := FileAge(Args[0].VarString^);
  end;
end;

class function TBuiltInFunction.SEDirectoryCreate(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  ForceDirectories(Args[0].VarString^);
  Result := SENull;
end;

class function TBuiltInFunction.SEDirectoryDelete(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  {$ifdef SE_HAS_FILEUTIL}
  DeleteDirectory(Args[0], False);
  {$endif}
  Result := SENull;
end;

class function TBuiltInFunction.SEDirectoryFindAll(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
var
  SL: TStringList;
  I: NativeInt;
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

class function TBuiltInFunction.SEDirectoryExists(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := DirectoryExists(Args[0].VarString^);
end;

class function TBuiltInFunction.SEBase64Encode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := EncodeStringBase64(Args[0]);
end;

class function TBuiltInFunction.SEBase64Decode(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  Result := DecodeStringBase64(Args[0]);
end;

class function TBuiltInFunction.SEJSONParse(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
  procedure QueryForObject(out R: TSEValue; Data: TJSONData); forward;

  procedure QueryForArray(out R: TSEValue; Data: TJSONData);
  var
    I: NativeInt;
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
    I: NativeInt;
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

class function TBuiltInFunction.SEJSONStringify(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;

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
    I: NativeInt = 0;
    J: NativeInt = 0;
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
    I: NativeInt = 0;
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

class function TBuiltInFunction.SEPasObjectClassName(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  Result := TObject(Args[0].VarPascalObject^.Value).ClassName;
end;

class function TBuiltInFunction.SEInvoke(const VM: TSEVM; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue): TSEValue;
begin
  SEValidateType(@Args[0], sevkPascalObject, 1, {$I %CURRENTROUTINE%});
  SEValidateType(@Args[1], sevkString, 2, {$I %CURRENTROUTINE%});
  Result := Args[0].Invoke(Args[1], @Args[2], ArgCount - 2);
end;

function TSEListPtr.Ptr(const Index: SizeInt): PTT;
begin
  Result := @FItems[Index];
end;

// ----- Fast inline TSEValue operations -----

procedure SEValueAdd(out R: TSEValue; constref V1, V2: TSEValue); overload;
var
  I, Len: NativeInt;
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
      Temp.VarBuffer^.Ptr := Pointer(NativeUInt(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
      R := Temp;
    end;
end;

procedure SEValueSub(out R: TSEValue; constref V1, V2: TSEValue); overload;
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
        Temp.VarBuffer^.Ptr := Pointer(NativeUInt(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
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
        R := not (Length(V.VarString^) > 0);
      end;
    sevkFunction,
    sevkPascalObject,
    sevkBuffer,
    sevkMap:
      begin
        R := False;
      end;
  end;
end;

procedure SEValueNeg(out R: TSEValue; constref V: TSEValue); inline;
begin
  R.VarNumber := -V.VarNumber;
end;

procedure SEValueMul(out R: TSEValue; constref V2, V1: TSEValue); inline; overload;
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      R := (V1.VarNumber <> 0) = Boolean(Round(V2.VarNumber));
    sevkString:
      R := (Length(V1.VarString^) > 0) = Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      R := True = Boolean(Round(V2.VarNumber));
    sevkNull:
      R := False = Boolean(Round(V2.VarNumber));
  end
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      R := (V1.VarNumber <> 0) <> Boolean(Round(V2.VarNumber));
    sevkString:
      R := (Length(V1.VarString^) > 0) <> Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      R := True <> Boolean(Round(V2.VarNumber));
    sevkNull:
      R := False <> Boolean(Round(V2.VarNumber));
  end
  else
    R := True;
end;

procedure SEValueShiftLeft(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := Round(V1.VarNumber) shl Round(V2.VarNumber);
end;

procedure SEValueShiftRight(out R: TSEValue; constref V1, V2: TSEValue); inline; overload;
begin
  R.Kind := sevkNumber;
  R.VarNumber := Round(V1.VarNumber) shr Round(V2.VarNumber);
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      Result := (V1.VarNumber <> 0) = Boolean(Round(V2.VarNumber));
    sevkString:
      Result := (Length(V1.VarString^) > 0) = Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      Result := True = Boolean(Round(V2.VarNumber));
    sevkNull:
      Result := False = Boolean(Round(V2.VarNumber));
  end
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      Result := (V1.VarNumber <> 0) <> Boolean(Round(V2.VarNumber));
    sevkString:
      Result := (Length(V1.VarString^) > 0) <> Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      Result := True <> Boolean(Round(V2.VarNumber));
    sevkNull:
      Result := False <> Boolean(Round(V2.VarNumber));
  end
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
  R.VarNumber := NativeInt(V);
end;
operator := (V: TSEValueArray) R: TSEValue; inline;
var
  I: NativeInt;
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

operator := (V: TSEValue) R: NativeInt; inline;
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
  Len, I: NativeInt;
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
operator := (V: TSEValue) R: TValue;
begin
  case V.Kind of
    sevkBoolean:
      R := Boolean(Round(V.VarNumber));
    sevkNumber:
      R := V.VarNumber;
    sevkString:
      R := V.VarString^;
    sevkPascalObject:
      R := V.VarPascalObject^.Value;
    sevkBuffer:
      R := NativeUInt(V.VarBuffer^.Ptr);
    else
      R := NativeUInt(V.VarPointer);
  end;
end;
operator := (V: TValue) R: TSEValue;
var
  PName: String;
begin
  case V.Kind of
    tkSet,
    tkInteger,
    tkQWord,
    tkInt64:
      R := V.AsInt64;
    tkFloat:
      R := TSEValue(V.AsExtended);
    tkBool:
      R := V.AsBoolean;
    tkLString,
    tkAString,
    tkWString,
    tkSString:
      R := V.AsString;
    tkUChar,
    tkWChar,
    tkChar:
      R := TSEValue(V.AsChar);
    tkObject:
      GC.AllocPascalObject(@R, V.AsObject, False);
    else
    begin
      WriteStr(PName, V.Kind);
      raise Exception.Create('Type "' + PName + '" not supported');
    end;
  end;
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
  I, Len: NativeInt;
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
        R.VarBuffer^.Ptr := Pointer(NativeUInt(V1.VarBuffer^.Ptr) + Round(V2.VarNumber));
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
        R.VarBuffer^.Ptr := Pointer(NativeUInt(V1.VarBuffer^.Ptr) - Round(V2.VarNumber));
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      R := (V1.VarNumber <> 0) = Boolean(Round(V2.VarNumber));
    sevkString:
      R := (Length(V1.VarString^) > 0) = Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      R := True = Boolean(Round(V2.VarNumber));
    sevkNull:
      R := False = Boolean(Round(V2.VarNumber));
  end
  else
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
  if V2.Kind = sevkBoolean then
  case V1.Kind of
    sevkNumber:
      R := (V1.VarNumber <> 0) <> Boolean(Round(V2.VarNumber));
    sevkString:
      R := (Length(V1.VarString^) > 0) <> Boolean(Round(V2.VarNumber));
    sevkMap,
    sevkPascalObject,
    sevkFunction:
      R := True <> Boolean(Round(V2.VarNumber));
    sevkNull:
      R := False <> Boolean(Round(V2.VarNumber));
  end
  else
    R := True;
end;

{$ifdef SE_MAP_AVK959}
class function TSEStringEq.HashCode(const AKey: String): SizeInt;
begin
  {$ifdef CPU64}
  Result := TxxHash64LE.HashStr(AKey);
  {$else}
  Result := TxxHash32LE.HashStr(AKey);
  {$endif}
  //Result := String.HashCode(AKey);
end;

class function TSEStringEq.Equal(const L, R: String): Boolean;
begin
  Result := L = R;
end;
{$endif}

constructor TSEValueMap.Create;
begin
  inherited;
  Self.FIsValidArray := True;
  {$ifdef SE_THREADS}
  InitCriticalSection(Self.FLock);
  {$endif}
end;

destructor TSEValueMap.Destroy;
begin
  {$ifdef SE_THREADS}
  DoneCriticalSection(Self.FLock);
  {$endif}
  {$ifdef SE_MAP_AVK959}
    Self.FMap.Clear;
  {$else}
    if Self.FMap <> nil then
      Self.FMap.Free;
  {$endif}
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
  Result := System.TryEnterCriticalSection(Self.FLock) <> 0;
  {$else}
  Result := True;
  {$endif}
end;

procedure TSEValueMap.ToMap;
var
  I: NativeInt;
begin
  Self.Lock;
  try
    if Self.FIsValidArray then
    begin
      {$ifndef SE_MAP_AVK959}
      Self.FMap := TSEValueDict.Create;
      {$endif}
      for I := 0 to Self.Count - 1 do
        Self.FMap.AddOrSetValue(IntToStr(I), Self[I]);
      Self.FIsValidArray := False;
      Self.Clear;
    end;
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Set2(const Key: PString; constref AValue: TSEValue);
begin
  if Self.FIsValidArray then
    Self.ToMap;
  Self.Lock;
  try
    Self.FMap.AddOrSetValue(Key^, AValue);
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Set2(const Index: SizeInt; constref AValue: TSEValue);
begin
  if Index < 0 then
    Exit;
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

procedure TSEValueMap.Del2(const Key: PString);
begin
  Self.Lock;
  try
    Self.FMap.Remove(Key^);
  finally
    Self.Unlock;
  end;
end;

procedure TSEValueMap.Del2(const Index: SizeInt);
begin
  Self.Lock;
  try
    if (Index <= Self.Count - 1) and (Index >= 0) then
    begin
      Self.Delete(Index);
    end;
  finally
    Self.Unlock;
  end;
end;

function TSEValueMap.Get2(const Key: PString): TSEValue;
begin
  Result := SENull;
  {$ifndef SE_MAP_AVK959}
  if Self.FMap <> nil then
  {$endif}
    Self.FMap.TryGetValue(Key^, Result);
end;

function TSEValueMap.Get2(const Index: SizeInt): TSEValue;
begin
  if (Index <= Self.Count - 1) and (Index >= 0) then
    Result := Self.FItems[Index]
  else
    Result := SENull;
end;

function TSEValueMap.Ptr(const I: NativeInt): PSEValue;
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
  I: NativeInt;
begin
  while True do
  begin
    if Self.Terminated then
      Exit;
    if GC.Phase = segcpMark then
    begin
      {$ifdef SE_LOG}
      Writeln('[GC] ', GC.Phase);
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
  Self.FReachableValueList.Capacity := 65536;
  Self.FVMThreadList := TSEVMList.Create;
  Self.EnableParallel := {$ifdef SE_MAP_AVK959}True{$else}False{$endif};
end;

destructor TSEGarbageCollector.Destroy;
var
  I: NativeInt;
  Value: PSEGCNode;
begin
  I := Self.FNodeLastOld;
  while I <> 0 do
  begin
    Value := Self.FNodeList.Ptr(I);
    Value^.Garbage := not Value^.Lock;
    I := Value^.Prev;
  end;
  I := Self.FNodeLastYoung;
  while I <> 0 do
  begin
    Value := Self.FNodeList.Ptr(I);
    Value^.Garbage := not Value^.Lock;
    I := Value^.Prev;
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
  I, J: NativeInt;
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
  I, MS: NativeInt;
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
  I: NativeInt;
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
  I: NativeInt;
  Key: String;
  Cache: TSECache;
  Binary: TSEBinary;

  procedure SuspendThreads;
  var
    I: NativeInt;
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
    I: NativeInt;
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
    I, J: NativeInt;
  begin
  {$ifdef SE_THREADS}
    if Self.EnableParallel then
    begin
      Self.FReachableValueList.Count := 0;
      for I := 0 to VMList.Count - 1 do
      begin
        VM := VMList[I];
        P := @VM.Stack[0];
        while P < VM.StackPtr do
        begin
          Self.FReachableValueList.Add(P^);
          Inc(P);
        end;
        if VM.Owner = nil then
        begin
          P := @VM.Global.Value^.Data[0];
          P2 := @VM.Global.Value^.Data[VM.Global.Value^.Size - 1];
          while P <= P2 do
          begin
            Self.FReachableValueList.Add(P^);
            Inc(P);
          end;
          for J := 0 to VM.Parent.ConstList.Count - 1 do
          begin
            Self.FReachableValueList.Add(VM.Parent.ConstList[J]);
          end;
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
        if VM.Owner = nil then
        begin
          for J := 0 to VM.Parent.ConstList.Count - 1 do
          begin
            V := VM.Parent.ConstList[J];
            Self.Mark(@V)
          end;
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
  if System.TryEnterCriticalSection(CS) = 0 then
  begin
    Self.FTicks := GetTickCount64;
    Exit;
  end;
  {$endif}
  FVMThreadList := TSEVMList.Create;
  {$ifdef SE_PROFILER}
  FrameProfiler.Start('TEvilC.GC');
  {$endif}
  try
    try
      if Self.FPhase = segcpRest then
      begin
        Self.FPhase := segcpInitial;
        SuspendThreads;
        {$ifdef SE_LOG}
        Writeln('[GC] ', Self.FPhase);
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
        Writeln('[GC] Time: ', GetTickCount64 - Self.FTicks, 'ms');
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
    {$ifdef SE_PROFILER}
    FrameProfiler.Stop('TEvilC.GC');
    {$endif}
  end;
end;

procedure TSEGarbageCollector.AllocBuffer(const PValue: PSEValue; const Size: NativeInt);
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
      PValue^.VarBuffer^.Ptr := Pointer(NativeUInt(PValue^.VarBuffer^.Base) + NativeUInt(PValue^.VarBuffer^.Base) mod 16);
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
  I: NativeInt;
begin
  for I := 0 to Self.Binaries.Value^.Size - 1 do
    FreeAndNil(Self.Binaries.Value^.Data[I]);
  Self.Binaries.Alloc(1);
  Self.Binaries.Value^.Data[0] := TSEBinary.Create;
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
  Assert(Self.Value <> nil, 'Self.Value = nil');
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
  Assert(Self.Value <> nil, 'Self.Value = nil');
  Inc(Self.Value^.RefCount);
  Result := Self;
end;

procedure TSEBinariesManaged.Free;
var
  I: NativeInt;
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
  Self.CodePtr := nil;
  Self.IsPaused := False;
  Self.IsDone := True;
  Self.StackSize := SEStackSize;
  Self.FrameSize := SEFrameSize;
  Self.TrapSize := SETrapSize;
  if VMList = nil then
    VMList := TSEVMList.Create;
  if GC = nil then
    GC := TSEGarbageCollector.Create;
  VMList.Add(Self);
  Self.Binaries := Default(TSEBinariesManaged);
  Self.Binaries.Alloc(1);
  Self.Binaries.Value^.Data[0] := TSEBinary.Create;
  Self.Global := Default(TSEValueArrayManaged);
  Self.JITBlockList := TSEJITBlockList.Create;
end;

destructor TSEVM.Destroy;
var
  I: NativeInt;
begin
  Self.Binaries.Free;
  if VMList <> nil then
  begin
    VMList.Remove(Self);
    for I := VMList.Count - 1 downto 0 do
      if VMList[I].Owner = Self then
      begin
        if VMList[I].CoroutineOwner <> nil then
        begin
          VMList[I].CoroutineOwner.VM := nil;
          VMList[I].Free;
        end;
      end;
  end;
  Self.Global.Free;
  for I := 0 to Self.JITBlockList.Count - 1 do
  begin
    {$ifdef WINDOWS}
    VirtualFree(Self.JITBlockList.Ptr(I)^.Code, 0, MEM_RELEASE);
    {$endif}
    {$ifdef UNIX}
    munmap(Self.JITBlockList.Ptr(I)^.Code, Self.JITBlockList.Ptr(I)^.CodeSize);
    {$endif}
  end;
  Self.JITBlockList.Free;
  inherited;
end;

function TSEVM.Fork(const AStackSize: Cardinal): TSEVM;
var
  StackCount: Cardinal;
  I: NativeInt;
begin
  Result := TSEVM.Create;
  Result.Binaries.Free;

  Result.Owner := Self;
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
  Result.FramePtr^.StackPtr := Result.StackPtr;
  Result.TrapPtr := @Result.Trap[0];
  Dec(Result.TrapPtr);
  //
  Result.Binaries := Self.Binaries.Ref;
end;

procedure TSEVM.ModifyGlobalVariable(const AName: String; const AValue: TSEValue);
begin
  Self.SetGlobalVariable(AName, AValue);
end;

procedure TSEVM.SetGlobalVariable(const AName: String; const AValue: TSEValue);
var
  I: NativeInt;
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
  I: NativeInt;
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

procedure TSEVM.Reset;
begin
  Self.CodePtr := nil;
  Self.CodeSegmentIndex := 0;
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
  Self.FramePtr^.StackPtr := Self.StackPtr;
  Self.TrapPtr := @Self.Trap[0];
  Dec(Self.TrapPtr);
end;

var
  PingCount: Integer = 0;

procedure Ping(A: Int64);
begin
  Writeln('PING ', PingCount, ', ', A);
  Inc(PingCount);
end;

procedure TSEVM.Exec;
type
  TSEJITCodeProc = procedure(A, B, C, D: Pointer);
var
  A, B, C, V,
  OA, OB, OC, OV: PSEValue;
  TV, TV2: TSEValue;
  S, S1, S2: String;
  WS, WS1, WS2: UnicodeString;
  FuncNativeInfo: PSEFuncNativeInfo;
  FuncScriptInfo: PSEFuncScriptInfo;
  FuncImportInfo: PSEFuncImportInfo;
  I, J, ArgCountStack, ArgCount, ArgSize, DeepCount: NativeInt;
  This: PSEValue;
  GlobalLocal: PSEValue;
  CodeSegmentIndexLocal: NativeInt;
  CodePtrLocal: PSEValue;
  FuncImport, P, PP, PC: Pointer;
  LineOfCode: TSELineOfCode;
  IsScriptException: Boolean = False;
  CodeProc: TSEJITCodeProc;

  procedure GetLineOfCode;
  var
    I: NativeInt;
    CodeIndex: NativeUInt;
  begin
    if FramePtr = @Self.Frame[0] then
    begin
      I := Self.Parent.LineOfCodeList.Count - 1;
      while I >= 0 do
      begin
        LineOfCode := Self.Parent.LineOfCodeList[I];
        CodeIndex := NativeUInt(CodePtrLocal - Self.Binaries.Value^.Data[LineOfCode.CodeSegmentIndex].Ptr(0)) div SizeOf(TSEValue);
        if (CodeIndex >= LineOfCode.CodeIndex) and (LineOfCode.CodeSegmentIndex = 0) then
          break;
        Dec(I);
      end;
    end else
    begin
      I := 0;
      while I <= Self.Parent.LineOfCodeList.Count - 1 do
      begin
        LineOfCode := Self.Parent.LineOfCodeList[I];
        CodeIndex := NativeUInt(CodePtrLocal - Self.Binaries.Value^.Data[LineOfCode.CodeSegmentIndex].Ptr(0)) div SizeOf(TSEValue);
        if (CodeIndex < LineOfCode.CodeIndex) and (CodeSegmentIndexLocal = LineOfCode.CodeSegmentIndex) then
          break;
        Inc(I);
      end;
    end;
  end;

  procedure PrintEvilScriptStackTrace(Message: String);

    procedure AddChildNode(Node: PSEStackTraceSymbol; const AName: String; AValue: PSEValue);
    var
      I, C: NativeInt;
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
    I, J: NativeInt;
    LineOfCode: TSELineOfCode;
    Nodes: TSEStackTraceSymbolArray;
    NodeCount: NativeInt = 0;
    CodeSegmentIndex: NativeInt;
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
              CodeSegmentIndex := 0
            else
              CodeSegmentIndex := Self.Frame[I - 1].Func^.CodeSegmentIndex;
            if (CurFrame^.CodeSegmentIndex < LineOfCode.CodeIndex) and (CodeSegmentIndex = LineOfCode.CodeSegmentIndex) then
              break;
            Dec(J);
          end;
          Nodes[NodeCount - 1].Name := CurFunc^.Name + ' [' + LineOfCode.Module + ':' + IntToStr(LineOfCode.Line) + ']';
          for J := 0 to CurFrame^.Func^.VarSymbols.Count - 1 do
          begin
            AddChildNode(@Nodes[NodeCount - 1], CurFunc^.VarSymbols[J], @CurFrame^.StackPtr[J - 1]);
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

  procedure Push(constref Value: TSEValue); inline;
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
    GlobalLocal[NativeInt(I)] := Value^;
  end;

  procedure AssignLocal(const I: Pointer; const F: NativeInt; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.StackPtr + NativeInt(I))^ := Value^;
  end;

  function GetGlobal(const I: Pointer): PSEValue; inline;
  begin
    Exit(@GlobalLocal[NativeInt(I)]);
  end;

  function GetLocal(const I: Pointer; const F: NativeInt): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.StackPtr + NativeInt(I));
  end;

  function GetGlobalInt(const I: NativeInt): PSEValue; inline;
  begin
    Exit(@GlobalLocal[NativeInt(I)]);
  end;

  function GetLocalInt(const I, F: NativeInt): PSEValue; inline;
  begin
    Exit((Self.FramePtr - F)^.StackPtr + NativeInt(I));
  end;

  procedure AssignGlobalInt(const I: NativeInt; const Value: PSEValue); inline;
  begin
    GlobalLocal[NativeInt(I)] := Value^;
  end;

  procedure AssignLocalInt(const I: NativeInt; const F: NativeInt; const Value: PSEValue); inline;
  begin
    ((Self.FramePtr - F)^.StackPtr + NativeInt(I))^ := Value^;
  end;

  function GetVariable(const I: Pointer; const F: Pointer): PSEValue; inline;
  begin
    if F = Pointer(SE_REG_GLOBAL) then
      Exit(@GlobalLocal[NativeInt(I)])
    else
      Exit((Self.FramePtr - NativeInt(F))^.StackPtr + NativeInt(I));
  end;

  procedure SetVariable(const I: Pointer; const F: Pointer; const Value: PSEValue); inline;
  begin
    if F = Pointer(SE_REG_GLOBAL) then
      GlobalLocal[NativeInt(I)] := Value^
    else
      ((Self.FramePtr - NativeInt(F))^.StackPtr + NativeInt(I))^ := Value^;
  end;

  procedure CallImportFunc;
  var
    I: NativeInt;
    ImportBufferIndex: array [0..31] of NativeUInt;
    ImportBufferData: array [0..8*31] of Byte;
    ImportBufferString: array [0..31] of String;
    ImportBufferWideString: array [0..31] of UnicodeString;
    ImportResult: NativeUInt;
    ImportResultD: TSENumber;
    ImportResultS: Single;
    ArgCountStack, ArgCount, ArgSize: NativeInt;
    FuncImport, P, PP: Pointer;
    {$ifdef SE_LIBFFI}
    ffiCif: ffi_cif;
    ffiArgTypes: array [0..31] of pffi_type;
    ffiArgValues: array [0..31] of Pointer;
    ffiResultType: ffi_type;
    ffiAbi: ffi_abi;
    {$endif}
  begin
    FuncImportInfo := Self.Parent.FuncImportList.Ptr(NativeInt(CodePtrLocal[1].VarPointer));
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
            NativeUInt((@ImportBufferData[I * 8])^) := Byte(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint8;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU16:
          begin
            NativeUInt((@ImportBufferData[I * 8])^) := Word(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint16;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU32:
          begin
            NativeUInt((@ImportBufferData[I * 8])^) := LongWord(Round(Pop^.VarNumber));
            ffiArgTypes[I] := @ffi_type_uint32;
            ffiArgValues[I] := @ImportBufferData[I * 8];
          end;
        seakU64:
          begin
            NativeUInt((@ImportBufferData[I * 8])^) := NativeUInt(Round(Pop^.VarNumber));
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
              NativeUInt((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
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
              NativeUInt((@ImportBufferData[I * 8])^) := Round(A^.VarNumber);
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
    I := NativeInt(ffi_prep_cif(@ffiCif, ffiAbi, ArgCount, @ffiResultType, @ffiArgTypes[0]));
    if I <> NativeInt(FFI_OK) then
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
          TV := NativeInt(LongInt(ImportResult));
        end;
      seakI64:
        begin
          TV := Int64(ImportResult);
        end;
      seakU8, seakU16, seakU32:
        begin
          TV := NativeUInt(LongWord(ImportResult));
        end;
      seakU64:
        begin
          TV := UInt64(ImportResult);
        end;
      seakBuffer, seakWBuffer:
        begin
          GC.AllocBuffer(@TV, 0);
          TV.VarBuffer^.Ptr := Pointer(NativeUInt(ImportResult));
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
      P := DispatchTable[TSEOpcode(NativeUInt(CodePtrLocal^.VarPointer))];
      asm
        jmp P;
      end
    }
  {$elseif defined(CPUARM) or defined(CPUAARCH64)}
    {$define DispatchGoto :=
      P := DispatchTable[TSEOpcode(NativeUInt(CodePtrLocal^.VarPointer))];
      asm
        ldr x16,P
        br  x16
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
  labelStart,
  labelPushConst, labelPushConstEnd,
  labelPushConstString, labelPushConstStringEnd,
  labelPushGlobalVar, labelPushGlobalVarEnd,
  labelPushLocalVar, labelPushLocalVarEnd,
  labelPushVar2, labelPushVar2End,
  labelPushArrayPop, labelPushArrayPopEnd,
  labelPopConst, labelPopConstEnd,
  labelPopFrame,
  labelAssignGlobalVar, labelAssignGlobalVarEnd,
  labelAssignGlobalArray, labelAssignGlobalArrayEnd,
  labelAssignLocalVar, labelAssignLocalVarEnd,
  labelAssignLocalArray, labelAssignLocalArrayEnd,
  labelJumpEqualRel,
  labelJumpEqual1Rel,
  labelJumpUnconditionalRel,
  labelJumpEqualOrGreater2Rel,
  labelJumpEqualOrLesser2Rel,

  labelOperatorInc, labelOperatorIncEnd,

  labelOperatorAdd0,
  labelOperatorMul0,
  labelOperatorDiv0,

  labelOperatorAdd1,
  labelOperatorSub1,
  labelOperatorMul1,
  labelOperatorDiv1,

  labelOperatorAdd,
  labelOperatorSub,
  labelOperatorMul,
  labelOperatorDiv,
  labelOperatorMod,
  labelOperatorNegative,

  labelOperatorLesser0,
  labelOperatorLesserOrEqual0,
  labelOperatorGreater0,
  labelOperatorGreaterOrEqual0,
  labelOperatorEqual0,
  labelOperatorNotEqual0,
  labelOperatorAnd0,
  labelOperatorOr0,

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
  labelPushConstFromConstList,

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
  labelThrow,
  labelJITBlock,
  labelJITBlockPotential;

var
  DispatchTable: array[TSEOpcode] of Pointer = (
    @labelPushConst,
    @labelPushConstString,
    @labelPushGlobalVar,
    @labelPushLocalVar,
    @labelPushArrayPop,
    @labelPopConst,
    @labelPopFrame,
    @labelAssignGlobalVar,
    @labelAssignGlobalArray,
    @labelAssignLocalVar,
    @labelAssignLocalArray,
    @labelJumpEqualRel,
    @labelJumpEqual1Rel,
    @labelJumpUnconditionalRel,
    @labelJumpEqualOrGreater2Rel,
    @labelJumpEqualOrLesser2Rel,

    @labelOperatorInc,

    @labelOperatorAdd0,
    @labelOperatorMul0,
    @labelOperatorDiv0,

    @labelOperatorAdd1,
    @labelOperatorSub1,
    @labelOperatorMul1,
    @labelOperatorDiv1,

    @labelOperatorAdd,
    @labelOperatorSub,
    @labelOperatorMul,
    @labelOperatorDiv,
    @labelOperatorMod,
    @labelOperatorNegative,

    @labelOperatorLesser0,
    @labelOperatorLesserOrEqual0,
    @labelOperatorGreater0,
    @labelOperatorGreaterOrEqual0,
    @labelOperatorEqual0,
    @labelOperatorNotEqual0,
    @labelOperatorAnd0,
    @labelOperatorOr0,

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
    @labelPushConstFromConstList,

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
    @labelThrow,
    @labelJITBlock,
    @labelJITBlockPotential
  );

  procedure JITHandler(JitCodePtrLocal: PSEValue);
  var
    I, J, BIndex, BFinish: NativeInt;
    Op: TSEOpcode;
    IsInvalidOpcode: Boolean;
    IsStackOverflow: Boolean; // Stack overflow when XMMStackPtr > 16
    E: TX64Emitter;
    XMMStackPtr: Byte;
    P: Pointer;
    IsAssigned: Boolean;
    JITBlock: TSEJITBlock;

    procedure GenPing(Reg: TX64Reg);
    begin
      E.MovRegReg64(regRCX, Reg);
      E.SubRegImm32(regRSP, 40);
      E.CallAbsolute(regR9, @Ping);
      E.AddRegImm32(regRSP, 40);
    end;

    procedure GenGetGlobalVariable(IsValueOnly: Boolean = True);
    begin
      { Load global variable index to R8 }
      // mov r8, qword ptr [r15 + code[1].VarPointer]
      E.MovRegImm64(regR8, SizeOf(TSEValue) * NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer));
      { Load global variable to stack }
        // movsd xmm?, qword ptr [r12 + r8 + .VarNumber]
      E.MovSDXMMFromMem(TXMMReg(XMMStackPtr), E.MemIndex(regR12, regR8, 1, NativeUInt(@TSEValue(nil^).VarNumber)));
      if not IsValueOnly then
        { We get the address of the local variable }
        E.LeaRegMem(regR8, E.MemIndex(regR12, regR8, 1, 0));
      Inc(XMMStackPtr);
    end;

    procedure GenGetLocalVariable(IsValueOnly: Boolean = True);
    begin
      { R8 = current frame }
      // mov r8, r11
      E.MovRegReg64(regR8, regR11);
      if NativeUInt(JitCodePtrLocal[BIndex + 2].VarPointer) <> 0 then
      begin
        { Load frame relative index to RAX }
        // mov rax, code[2].VarPointer
        E.MovRegImm64(regRAX, NativeUInt(JitCodePtrLocal[BIndex + 2].VarPointer) * SizeOf(TSEFrame));
        { R8 = current frame - relative index }
        // sub r8, rax
        E.SubRegReg(regR8, regRAX);
      end;
      { Load local vraiable index to RAX }
      // mov rdx, code[1].VarPointer
      E.MovRegImm64(regRAX, NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer) * SizeOf(TSEValue));
      { R8 = current frame's stack pointer }
      // mov r8, qword ptr [r8 + .StackPtr]
      E.MovReg64Mem(regR8, E.Mem(regR8, NativeUInt(@TSEFrame(nil^).StackPtr)));
      if IsValueOnly then
      { XMM? = local variable }
      // movsd xmm?, qword ptr [r8 + rax + .VarNumber]
      E.MovSDXMMFromMem(TXMMReg(XMMStackPtr), E.MemIndex(regR8, regRAX, 1, NativeUInt(@TSEValue(nil^).VarNumber)));
      if not IsValueOnly then
        { We get the address of the local variable }
        E.LeaRegMem(regR8, E.MemIndex(regR8, regRAX, 1, 0));
      Inc(XMMStackPtr);
    end;

    procedure GenGetVariable(IsValueOnly: Boolean = True);
    begin
      if JitCodePtrLocal[BIndex + 2].VarPointer = Pointer(SE_REG_GLOBAL) then
        GenGetGlobalVariable(IsValueOnly)
      else
        GenGetLocalVariable(IsValueOnly);
    end;

  begin
    E := TX64Emitter.Create;
    try
      BIndex := 0;
      BFinish := NativeInt(JitCodePtrLocal[1].VarPointer);

      XMMStackPtr := 0;
      IsInvalidOpcode := False;
      IsAssigned := False;
      {$ifdef WINDOWS}
      { R8, R9, R10 are for scratch }
      // R15 = CodePtrLocal
      E.MovReg64Mem(regR15, E.Mem(regRCX, 0));
      { R13 = @StackPtr }
      E.MovRegReg64(regR13, regRDX);
      { R14 = StackPtr }
      E.MovReg64Mem(regR14, E.Mem(regR13, 0));
      { R12 = GlobalVar }
      E.MovReg64Mem(regR12, E.Mem(regR8, 0));
      { R11 = FramePtr}
      E.MovReg64Mem(regR11, E.Mem(regR9, 0));
      { Move to the next opcode }
      E.AddRegImm32(regR15, OpcodeSizes[opJITBlock] * SizeOf(TSEValue));
      {$else}
      // R15 = CodePtrLocal
      E.MovReg64Mem(regR15, E.Mem(regRDI, 0));
      { R13 = @StackPtr }
      E.MovRegReg64(regR13, regRSI);
      { R14 = StackPtr }
      E.MovReg64Mem(regR14, E.Mem(regR13, 0));
      { R12 = GlobalVar }
      E.MovReg64Mem(regR12, E.Mem(regRDX, 0));
      { R11 = FramePtr}
      E.MovReg64Mem(regR11, E.Mem(regRCX, 0));
      { Move to the next opcode }
      E.AddRegImm32(regR15, OpcodeSizes[opJITBlock] * SizeOf(TSEValue));
      {$endif}
      //
      BIndex := BIndex + OpcodeSizes[opJITBlock];
      //Writeln('JIT from ', BIndex, ' to ', BFinish);
      while BIndex <= BFinish do
      begin
        Op := TSEOpcode(NativeUInt(JitCodePtrLocal[BIndex].VarPointer));
        //Writeln(' - ', Op);
        case Op of
          opPushConst:
            begin
              E.MovSDXMMFromMem(TXMMReg(XMMStackPtr), E.Mem(regR15, SizeOf(TSEValue) + NativeUInt(@TSEValue(nil^).VarNumber)));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Inc(XMMStackPtr);
            end;
          opOperatorAdd:
            begin
              E.AddSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Dec(XMMStackPtr, 1);
            end;
          opOperatorSub:
            begin
              E.SubSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Dec(XMMStackPtr, 1);
            end;
          opOperatorMul:
            begin
              E.MulSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Dec(XMMStackPtr, 1);
            end;
          opOperatorDiv:
            begin
              E.DivSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Dec(XMMStackPtr, 1);
            end;
          opOperatorNegative:
            begin
              // mov r8, @Negative2QWords
              E.MovRegImm64(regR8, NativeUInt(@Negative2QWords[0]));
              // xorpd xmm?, [r8]
              E.XorPDMem(TXMMReg(XMMStackPtr - 1), E.Mem(regR8, 0));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;

          opOperatorAdd0:
            begin
              // mov r8, code[1].VarPointer
              E.MovRegImm64(regR8, NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer));
              // movq xmm, r8
              E.MovSDXMMFromReg(TXMMReg(XMMStackPtr), regR8);
              Inc(XMMStackPtr);
              { Add }
              E.AddSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;
          opOperatorMul0:
            begin
              // mov r8, code[1].VarPointer
              E.MovRegImm64(regR8, NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer));
              // movq xmm, r8
              E.MovSDXMMFromReg(TXMMReg(XMMStackPtr), regR8);
              Inc(XMMStackPtr);
              { Mul }
              E.MulSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;
          opOperatorDiv0:
            begin
              // mov r8, code[1].VarPointer
              E.MovRegImm64(regR8, NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer));
              // movq xmm, r8
              E.MovSDXMMFromReg(TXMMReg(XMMStackPtr), regR8);
              Inc(XMMStackPtr);
              { Div }
              E.DivSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;

          opOperatorInc:
            begin
              IsInvalidOpcode := True;
              break;
              GenGetVariable(False);
              E.MovRegImm64(regR9, NativeUInt(JitCodePtrLocal[BIndex + 3].VarNumber));
              E.MovSDXMMFromReg(regXMM1, regR9);
              { Add }
              E.AddSD(regXMM0, regXMM1);
              { Assign to address at R8 }
              E.MovSDMemFromXMM(E.Mem(regR8, NativeUInt(@TSEValue(nil^).VarNumber)), regXMM0);
              { Mark as number }
              E.MovRegImm32(regRAX, Cardinal(sevkNumber));
              E.MovMem32Reg(E.Mem(regR8, NativeUInt(@TSEValue(nil^).Kind)), regRAX);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              IsAssigned := True;
              break;
            end;
          opOperatorAdd1:
            begin
              GenGetVariable;
              { Add }
              E.AddSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;
          opOperatorSub1:
            begin
              GenGetVariable;
              { Sub }
              E.SubSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;
          opOperatorMul1:
            begin
              GenGetVariable;
              { Mul }
              E.MulSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;
          opOperatorDiv1:
            begin
              GenGetVariable;
              { Div }
              E.DivSD(TXMMReg(XMMStackPtr - 2), TXMMReg(XMMStackPtr - 1));
              Dec(XMMStackPtr, 1);
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
            end;

          opPushGlobalVar:
            begin
              { Load global variable index to R8 }
              // mov r8, qword ptr [r15 + (1).VarPointer]
              E.MovReg64Mem(regR8, E.Mem(regR15, SizeOf(TSEValue) + NativeUInt(@TSEValue(nil^).VarPointer)));
              // shl r8, 4
              E.ShlRegImm(regR8, 4);
              { Load global variable to stack }
                // movsd xmm?, qword ptr [r12 + r8 + .VarNumber]
              E.MovSDXMMFromMem(TXMMReg(XMMStackPtr), E.MemIndex(regR12, regR8, 1, NativeUInt(@TSEValue(nil^).VarNumber)));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Inc(XMMStackPtr);
            end;
          opPushLocalVar:
            begin
              { R8 = current frame }
              // mov r8, r11
              E.MovRegReg64(regR8, regR11);
              if NativeUInt(JitCodePtrLocal[BIndex + 2].VarPointer) <> 0 then
              begin
                { Load frame relative index to RAX }
                // mov rax, code[2].VarPointer
                E.MovRegImm64(regRAX, NativeUInt(JitCodePtrLocal[BIndex + 2].VarPointer) * SizeOf(TSEFrame));
                { R8 = current frame - relative index }
                // sub r8, rax
                E.SubRegReg(regR8, regRAX);
              end;
              { Load local vraiable index to RAX }
              // mov rdx, code[1].VarPointer
              E.MovRegImm64(regRAX, NativeUInt(JitCodePtrLocal[BIndex + 1].VarPointer) * SizeOf(TSEValue));
              { R8 = current frame's stack pointer }
              // mov r8, qword ptr [r8 + .StackPtr]
              E.MovReg64Mem(regR8, E.Mem(regR8, NativeUInt(@TSEFrame(nil^).StackPtr)));
              { XMM? = local variable }
              // mov xmm?, qword ptr [r8 + rax + .VarNumber]
              E.MovSDXMMFromMem(TXMMReg(XMMStackPtr), E.MemIndex(regR8, regRAX, 1, NativeUInt(@TSEValue(nil^).VarNumber)));
              //
              E.AddRegImm32(regR15, OpcodeSizes[Op] * SizeOf(TSEValue));
              Inc(XMMStackPtr);
            end;
          else
            begin
              IsInvalidOpcode := True;
              break;
              // TODO: Either roll back, or push the remaining XMM values to the stack and continue
            end;
        end;
        Inc(BIndex, OpcodeSizes[Op]);
      end;
      if IsInvalidOpcode then
      begin
        JitCodePtrLocal[1] := nil;
      end else
      begin
        if not IsAssigned then
        begin
          { Move XMM0 to the stack }
          E.MovSDMemFromXMM(E.Mem(regR14, NativeUInt(@TSEValue(nil^).VarNumber)), regXMM0);
          { Mark this as number }
          E.MovRegImm32(regRAX, Cardinal(sevkNumber));
          E.MovMem32Reg(E.Mem(regR14, NativeUInt(@TSEValue(nil^).Kind)), regRAX);
          { Increase stack by 1 }
          E.AddRegImm32(regR14, SizeOf(TSEValue));
          E.MovMem64Reg(E.Mem(regR13, 0), regR14);
        end;
        { Increase CodePtr }
        E.MovRegImm64(regR14, NativeUInt(@CodePtrLocal));
        E.MovMem64Reg(E.Mem(regR14, 0), regR15);
        E.Ret;
        // Patch the code to pass the memory block
        JITBlock.Code := E.MakeExecutable;
        JITBlock.CodeSize := E.ExecutableSize;
        JitCodePtrLocal[0] := Pointer(opJITBlock);
        JitCodePtrLocal[1] := JITBlock.Code;
        Self.JITBlockList.Add(JITBlock);
      end;
    finally
      E.Free;
    end;
  end;

begin
  if Self.IsDone then
    Self.Reset;
  Self.IsYielded := False;
  if Self.IsPaused then
    Exit;
  GlobalLocal := @Self.Global.Value^.Data[0];
  CodeSegmentIndexLocal := Self.CodeSegmentIndex;
  if Self.CodePtr <> nil then
    CodePtrLocal := Self.CodePtr
  else
    CodePtrLocal := Self.Binaries.Value^.Data[Self.CodeSegmentIndex].Ptr(0);
  GC.CheckForGC;

labelStart:
  while True do
  try
    DispatchGoto;
    while True do
    begin
      {$ifndef SE_COMPUTED_GOTO}
      case TSEOpcode(NativeUInt(CodePtrLocal^.VarPointer)) of
      {$endif}
      {$ifndef SE_COMPUTED_GOTO}opJITBlockPotential:{$endif}
        begin
        labelJITBlockPotential:
          P := CodePtrLocal[1].VarPointer;
          {$ifdef WINDOWS}
          if P <> nil then
          begin
            JITHandler(CodePtrLocal);
          end else
          {$endif}
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJITBlock:{$endif}
        begin
        labelJITBlock:
          CodeProc := TSEJITCodeProc(CodePtrLocal[1].VarPointer);// R15 = CodePtrLocal
          CodeProc(@CodePtrLocal, @StackPtr, @GlobalLocal, @FramePtr);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorInc:{$endif}
        begin
        labelOperatorInc:
          V  := GetVariable(CodePtrLocal[1].VarPointer, CodePtrLocal[2].VarPointer);
          V^.VarNumber := V^.VarNumber + CodePtrLocal[3].VarNumber;
          Inc(CodePtrLocal, 4);
        labelOperatorIncEnd:
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd0:{$endif}
        begin
        labelOperatorAdd0:
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^.VarNumber := A^.VarNumber + CodePtrLocal[1].VarNumber
          else
            SEValueAdd(Self.StackPtr^, A^, CodePtrLocal[1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorMul0:{$endif}
        begin
        labelOperatorMul0:
          Self.StackPtr^.VarNumber := Pop^.VarNumber * CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv0:{$endif}
        begin
        labelOperatorDiv0:
          Self.StackPtr^.VarNumber := Pop^.VarNumber / CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;

      {$ifndef SE_COMPUTED_GOTO}opOperatorLesser0:{$endif}
        begin
        labelOperatorLesser0:
          Self.StackPtr^ := Pop^.VarNumber < CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorLesserOrEqual0:{$endif}
        begin
        labelOperatorLesserOrEqual0:
          Self.StackPtr^ := Pop^.VarNumber <= CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreater0:{$endif}
        begin
        labelOperatorGreater0:
          Self.StackPtr^ := Pop^.VarNumber > CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreaterOrEqual0:{$endif}
        begin
        labelOperatorGreaterOrEqual0:
          Self.StackPtr^ := Pop^.VarNumber >= CodePtrLocal[1].VarNumber;
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorEqual0:{$endif}
        begin
        labelOperatorEqual0:
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^ := A^.VarNumber = CodePtrLocal[1].VarNumber
          else
            SEValueEqual(Self.StackPtr^, A^, CodePtrLocal[1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNotEqual0:{$endif}
        begin
        labelOperatorNotEqual0:
          A := Pop;
          if A^.Kind = sevkNumber then
            Self.StackPtr^ := A^.VarNumber <> CodePtrLocal[1].VarNumber
          else
            SEValueNotEqual(Self.StackPtr^, A^, CodePtrLocal[1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAnd0:{$endif}
        begin
        labelOperatorAnd0:
          Self.StackPtr^.VarNumber := NativeInt(Pop^) and NativeInt(CodePtrLocal[1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorOr0:{$endif}
        begin
        labelOperatorOr0:
          Self.StackPtr^.VarNumber := NativeInt(Pop^) or NativeInt(CodePtrLocal[1]);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;

      {$ifndef SE_COMPUTED_GOTO}opOperatorNegative:{$endif}
        begin
        labelOperatorNegative:
          SEValueNeg(Self.StackPtr^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
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
          SEValueMul(Self.StackPtr^, {B}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv:{$endif}
        begin
        labelOperatorDiv:
          SEValueDiv(Self.StackPtr^, {A}Pop^, Pop^);
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
      {$ifndef SE_COMPUTED_GOTO}opOperatorLesser:{$endif}
        begin
        labelOperatorLesser:
          SEValueLesser(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorLesserOrEqual:{$endif}
        begin
        labelOperatorLesserOrEqual:
          SEValueLesserOrEqual(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreater:{$endif}
        begin
        labelOperatorGreater:
          SEValueGreater(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorGreaterOrEqual:{$endif}
        begin
        labelOperatorGreaterOrEqual:
          SEValueGreaterOrEqual(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorEqual:{$endif}
        begin
        labelOperatorEqual:
          SEValueEqual(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNotEqual:{$endif}
        begin
        labelOperatorNotEqual:
          SEValueNotEqual(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorAnd:{$endif}
        begin
        labelOperatorAnd:
          Push(NativeInt({A}Pop^) and NativeInt(Pop^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorOr:{$endif}
        begin
        labelOperatorOr:
          Push(NativeInt({A}Pop^) or NativeInt(Pop^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorXor:{$endif}
        begin
        labelOperatorXor:
          Push(NativeInt({A}Pop^) xor NativeInt(Pop^));
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorNot:{$endif}
        begin
        labelOperatorNot:
          SEValueNot(Self.StackPtr^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;

      {$ifndef SE_COMPUTED_GOTO}opOperatorAdd1:{$endif}
        begin
        labelOperatorAdd1:
          A := Pop;
          B := GetVariable(CodePtrLocal[1], {P}CodePtrLocal[2].VarPointer);
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
          B := GetVariable(CodePtrLocal[1], {P}CodePtrLocal[2].VarPointer);
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
          SEValueMul(Self.StackPtr^, {B}GetVariable(CodePtrLocal[1], {P}CodePtrLocal[2].VarPointer)^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorDiv1:{$endif}
        begin
        labelOperatorDiv1:
          SEValueDiv(Self.StackPtr^, Pop^, {B}GetVariable(CodePtrLocal[1], {P}CodePtrLocal[2].VarPointer)^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorShiftLeft:{$endif}
        begin
        labelOperatorShiftLeft:
          SEValueShiftLeft(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opOperatorShiftRight:{$endif}
        begin
        labelOperatorShiftRight:
          SEValueShiftRight(Self.StackPtr^, {A}Pop^, Pop^);
          Inc(Self.StackPtr);
          Inc(CodePtrLocal);
          DispatchGoto;
        end;

      {$ifndef SE_COMPUTED_GOTO}opPushConst:{$endif}
        begin
        labelPushConst:
          Push(CodePtrLocal[1]);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushConstString:{$endif}
        begin
        labelPushConstString:
          Push(ConstStrings.Ptr(NativeInt(CodePtrLocal[1].VarPointer))^);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushGlobalVar:{$endif}
        begin
        labelPushGlobalVar:
          Push(GetGlobal(CodePtrLocal[1].VarPointer)^);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushLocalVar:{$endif}
        begin
        labelPushLocalVar:
          Push(GetLocal(CodePtrLocal[1].VarPointer, NativeInt(CodePtrLocal[2].VarPointer))^);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushArrayPop:{$endif}
        begin
        labelPushArrayPop:
          A := @CodePtrLocal[1];
          if A^.Kind = sevkNull then
            A := Pop;
          B := Pop;
          case B^.Kind of
            sevkMap:
              Push(SEMapGet(B^, A^));
            sevkPascalObject:
              Push(B^.GetProp(A^));
            sevkString:
              {$ifdef SE_STRING_UTF8}
                Push(UTF8Copy(B^.VarString^, NativeInt(A^) + 1, 1));
              {$else}
                Push(B^.VarString^[NativeInt(A^) + 1]);
              {$endif}
            else
              Push(SENull);
          end;
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
      {$ifndef SE_COMPUTED_GOTO}opJumpEqualRel:{$endif}
        begin
        labelJumpEqualRel:
          if SEValueEqual(Pop^, Pop^) then
            CodePtrLocal := CodePtrLocal + NativeInt(CodePtrLocal[1].VarPointer)
          else
            Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqual1Rel:{$endif}
        begin
        labelJumpEqual1Rel:
          if SEValueEqual(Pop^, CodePtrLocal[1]) then
            CodePtrLocal := CodePtrLocal + NativeInt(CodePtrLocal[2].VarPointer)
          else
            Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpUnconditionalRel:{$endif}
        begin
        labelJumpUnconditionalRel:
          CodePtrLocal := CodePtrLocal + NativeInt(CodePtrLocal[1].VarPointer);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqualOrGreater2Rel:{$endif}
        begin
        labelJumpEqualOrGreater2Rel:
          if SEValueGreaterOrEqual(
            GetVariable(CodePtrLocal[1].VarPointer, CodePtrLocal[2].VarPointer)^,
            GetVariable(CodePtrLocal[3].VarPointer, CodePtrLocal[4].VarPointer)^)
          then
            CodePtrLocal := CodePtrLocal + NativeInt(CodePtrLocal[5].VarPointer)
          else
            Inc(CodePtrLocal, 6);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opJumpEqualOrLesser2Rel:{$endif}
        begin
        labelJumpEqualOrLesser2Rel:
          if SEValueLesserOrEqual(
            GetVariable(CodePtrLocal[1].VarPointer, CodePtrLocal[2].VarPointer)^,
            GetVariable(CodePtrLocal[3].VarPointer, CodePtrLocal[4].VarPointer)^)
          then
            CodePtrLocal := CodePtrLocal + NativeInt(CodePtrLocal[5].VarPointer)
          else
            Inc(CodePtrLocal, 6);
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
                DeepCount := NativeInt(CodePtrLocal[3].VarPointer);
                if DeepCount = 0 then
                  raise Exception.Create('Not a function reference');
                Self.StackPtr := Self.StackPtr - DeepCount;
                C := Self.StackPtr;
                for I := 0 to DeepCount - 1 do
                begin
                  TV2 := A^;
                  SEMapGet(TV, A^, C^);
                  A := @TV;
                  Inc(C);
                end;
              end;
            else
              raise Exception.Create('Not a function reference');
          end;
          CodePtrLocal[1] := Pointer(A^.VarFuncIndx);
          case A^.VarFuncKind of
            sefkScript:
              begin
                if DeepCount > 1 then
                  (Self.StackPtr - 1)^ := TV2;
                goto labelCallScript;
              end;
            sefkImport:
              begin
                Pop; // import has no this
                goto labelCallImport;
              end;
            sefkNative:
              begin
                if DeepCount > 1 then
                  (Self.StackPtr - 1)^ := TV2;
                This := Pop;
                Dec(CodePtrLocal[2].VarPointer); // ArgCount contains this, so we minus it by 1
                goto labelCallNative;
              end;
          end;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallNative:{$endif}
        begin
        labelCallNative:
          FuncNativeInfo := Self.Parent.FuncNativeList.Ptr(NativeInt(CodePtrLocal[1].VarPointer));
          ArgCount := NativeInt(CodePtrLocal[2].VarPointer);
          Self.StackPtr := Self.StackPtr - ArgCount;
          TV := TSEFunc(FuncNativeInfo^.Func)(Self, Self.StackPtr, ArgCount, This);
          if IsDone then
          begin
            Exit;
          end;
          Push(TV);
          GC.CheckForGCFast;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallScript:{$endif}
        begin
        labelCallScript:
          FuncScriptInfo := Self.Parent.FuncScriptList.Ptr(NativeInt(CodePtrLocal[1].VarPointer));
          Inc(Self.FramePtr);
          if Self.FramePtr > @Self.Frame[Self.FrameSize - 1] then
            raise Exception.Create('Too much recursion');
          Self.FramePtr^.StackPtr := Self.StackPtr - {ArgCount}NativeInt(CodePtrLocal[2].VarPointer);
          Self.FramePtr^.CodePtr := CodePtrLocal + 4;
          Self.FramePtr^.CodeSegmentIndex := CodeSegmentIndexLocal;
          Self.FramePtr^.Func := FuncScriptInfo;
          Self.StackPtr := Self.StackPtr + FuncScriptInfo^.VarCount;
          CodeSegmentIndexLocal := FuncScriptInfo^.CodeSegmentIndex;
          CodePtrLocal := Self.Binaries.Value^.Data[CodeSegmentIndexLocal].Ptr(0);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPopFrame:{$endif}
        begin
        labelPopFrame:
          CodePtrLocal := Self.FramePtr^.CodePtr;
          Self.StackPtr := Self.FramePtr^.StackPtr;
          CodeSegmentIndexLocal := Self.FramePtr^.CodeSegmentIndex;
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
          AssignGlobal(CodePtrLocal[1], Pop);
          Inc(CodePtrLocal, 2);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignLocalVar:{$endif}
        begin
        labelAssignLocalVar:
          AssignLocal(CodePtrLocal[1], NativeInt(CodePtrLocal[2].VarPointer), Pop);
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignGlobalArray:{$endif}
        begin
        labelAssignGlobalArray:
          A := @CodePtrLocal[1];
          TV := GetGlobalInt(NativeInt(A^))^;
          B := Pop;
          ArgCount := CodePtrLocal[2];
          if ArgCount = 1 then
            C := Pop
          else
          begin
            Self.StackPtr := Self.StackPtr - ArgCount;
            C := Self.StackPtr;
            for I := 1 to ArgCount - 1 do
            begin
              SEMapGet(TV, TV, C^);
              Inc(C);
            end;
          end;
          case TV.Kind of
            sevkMap:
              SEMapSet(TV, C^, B^);
            sevkPascalObject:
              TV.SetProp(C^, B^);
            sevkString:
              case B^.Kind of
                sevkString:
                  begin
                    {$ifdef SE_STRING_UTF8}
                      S2 := B^.VarString^;
                      UTF8Delete(AnsiString(TV.VarString^), NativeInt(C^) + 1, 1);
                      S := UTF8Copy(S2, 1, 1);
                      UTF8Insert(S, AnsiString(TV.VarString^), NativeInt(C^) + 1);
                    {$else}
                      TV.VarString^[NativeInt(C^) + 1] := B^.VarString^[1];
                    {$endif}
                  end;
                sevkNumber:
                  begin
                    {$ifdef SE_STRING_UTF8}
                      UTF8Delete(AnsiString(TV.VarString^), NativeInt(C^) + 1, 1);
                      S := Char(Round(B^.VarNumber));
                      UTF8Insert(S, AnsiString(TV.VarString^), NativeInt(C^) + 1);
                    {$else}
                      TV.VarString^[NativeInt(C^) + 1] := Char(Round(B^.VarNumber));
                    {$endif}
                  end;
              end;
          end;
          Inc(CodePtrLocal, 3);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opAssignLocalArray:{$endif}
        begin
        labelAssignLocalArray:
          A := @CodePtrLocal[1];
          TV := GetLocalInt(NativeInt(A^), NativeInt(CodePtrLocal[3].VarPointer))^;
          B := Pop;
          ArgCount := CodePtrLocal[2];
          if ArgCount = 1 then
            C := Pop
          else
          begin
            Self.StackPtr := Self.StackPtr - ArgCount;
            C := Self.StackPtr;
            for I := 1 to ArgCount - 1 do
            begin
              SEMapGet(TV, TV, C^);
              Inc(C);
            end;
          end;
          case TV.Kind of
            sevkMap:
              SEMapSet(TV, C^, B^);
            sevkPascalObject:
              TV.SetProp(C^, B^);
            sevkString:
              case B^.Kind of
                sevkString:
                  begin
                    {$ifdef SE_STRING_UTF8}
                      S1 := TV.VarString^;
                      S2 := B^.VarString^;
                      UTF8Delete(S1, NativeInt(C^) + 1, 1);
                      S := UTF8Copy(S2, 1, 1);
                      UTF8Insert(S, S1, NativeInt(C^) + 1);
                      TV.VarString^ := S1;
                    {$else}
                      TV.VarString^[NativeInt(C^) + 1] := B^.VarString^[1];
                    {$endif}
                  end;
                sevkNumber:
                  begin
                    {$ifdef SE_STRING_UTF8}
                      S1 := TV.VarString^;
                      UTF8Delete(S1, NativeInt(C^) + 1, 1);
                      S := Char(Round(B^.VarNumber));
                      UTF8Insert(S, S1, NativeInt(C^) + 1);
                      TV.VarString^ := S1;
                    {$else}
                      TV.VarString^[NativeInt(C^) + 1] := Char(Round(B^.VarNumber));
                    {$endif}
                  end;
              end;
          end;
          Inc(CodePtrLocal, 4);
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushConstFromConstList:{$endif}
        begin
        labelPushConstFromConstList:
          Push(Self.Parent.ConstList[NativeInt(CodePtrLocal[1].VarPointer)]);
          Inc(CodePtrLocal, 2);
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
          Self.CodeSegmentIndex := CodeSegmentIndexLocal;
          Exit;
        end;
      {$ifndef SE_COMPUTED_GOTO}opHlt:{$endif}
        begin
        labelHlt:
          Self.CodePtr := nil;
          Self.CodeSegmentIndex := CodeSegmentIndexLocal;
          Self.IsDone := True;
          Self.Parent.IsDone := True;
          Exit;
        end;
      {$ifndef SE_COMPUTED_GOTO}opPushTrap:{$endif}
        begin
        labelPushTrap:
          Inc(Self.TrapPtr);
          Self.TrapPtr^.FramePtr := Self.FramePtr;
          Self.TrapPtr^.StackPtr := Self.StackPtr;
          Self.TrapPtr^.CodeSegmentIndex := CodeSegmentIndexLocal;
          Self.TrapPtr^.CatchCodeIndex := NativeInt(CodePtrLocal[1].VarPointer);
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
            Self.StackPtr := Self.TrapPtr^.StackPtr;
            CodePtrLocal := Self.Binaries.Value^.Data[CodeSegmentIndexLocal].Ptr(0) + Self.TrapPtr^.CatchCodeIndex;
            CodeSegmentIndexLocal := Self.TrapPtr^.CodeSegmentIndex;
            Push(TV);
            Dec(Self.TrapPtr);
          end;
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}opCallImport:{$endif}
        begin
        labelCallImport:
          CallImportFunc;
          DispatchGoto;
        end;
      {$ifndef SE_COMPUTED_GOTO}
      end;
      if Self.IsPaused then
      begin
        Self.CodeSegmentIndex := CodeSegmentIndexLocal;
        Self.CodePtr := CodePtrLocal;
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
        Self.FramePtr^.StackPtr := Self.StackPtr - ArgCount;
        Self.FramePtr^.CodePtr := CodePtrLocal;
        Self.FramePtr^.CodeSegmentIndex := CodeSegmentIndexLocal;
        Self.FramePtr^.Func := FuncScriptInfo;
        Self.StackPtr := Self.StackPtr + FuncScriptInfo^.VarCount;
        CodeSegmentIndexLocal := FuncScriptInfo^.CodeSegmentIndex;
        CodePtrLocal := Self.Binaries.Value^.Data[CodeSegmentIndexLocal].Ptr(0);
        DispatchGoto;
      end else
      begin
        Self.FramePtr := Self.TrapPtr^.FramePtr;
        Self.StackPtr := Self.TrapPtr^.StackPtr;
        CodeSegmentIndexLocal := Self.FramePtr^.CodeSegmentIndex;
        CodePtrLocal := Self.Binaries.Value^.Data[CodeSegmentIndexLocal].Ptr(0) + Self.TrapPtr^.CatchCodeIndex;
        Push(E.Message);
        Dec(Self.TrapPtr);
        DispatchGoto;
        Break;
      {$endif}
      end;
    end;
  end;
  Self.CodeSegmentIndex := CodeSegmentIndexLocal;
  Self.CodePtr := CodePtrLocal;
end;

{$ifdef SE_THREADS}
constructor TSEVMThread.Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
var
  I: NativeInt;
begin
  Self.VM := AVM.Fork(AStackSize);
  Self.VM.ThreadOwner := Self;
  for I := 0 to ArgCount - 1 do
  begin
    Self.VM.StackPtr[0] := Args[I];
    Inc(Self.VM.StackPtr);
  end;
  Self.VM.StackPtr := Self.VM.StackPtr + Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].VarCount;
  Self.VM.CodeSegmentIndex := Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].CodeSegmentIndex;

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
    Self.VM.Free;
  end;
end;

destructor TSEVMThread.Destroy;
begin
  inherited;
end;
{$endif}

constructor TSEVMCoroutine.Create(const AVM: TSEVM; const Fn: TSEValue; const Args: PSEValue; const ArgCount, AStackSize: Cardinal);
var
  I: NativeInt;
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
  Self.VM.CodeSegmentIndex := Self.VM.Parent.FuncScriptList[Fn.VarFuncIndx].CodeSegmentIndex;
  Self.FStackPtr := Self.VM.StackPtr;
  Self.FBinaryPtr := Self.VM.CodeSegmentIndex;
end;

function TSEVMCoroutine.Execute: TSEValue;
begin
  if Self.VM = nil then
    Exit;
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

procedure TSEVMCoroutine.Reset(const Fn: TSEValue; const Args: PSEValue; const ArgCount: Cardinal; const This: PSEValue);
var
  I: NativeInt;
begin
  if Self.VM = nil then
    Exit;
  Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
  for I := 0 to ArgCount - 1 do
  begin
    Self.VM.StackPtr[0] := Args[I];
    Inc(Self.VM.StackPtr);
  end;
  Self.VM.CodeSegmentIndex := Self.FBinaryPtr;
  Self.VM.StackPtr := Self.FStackPtr;
  Self.IsTerminated := False;
  Self.IsDone := False;
  Inc(Self.VM.FramePtr);
  Self.VM.IsDone := False;
end;

destructor TSEVMCoroutine.Destroy;
begin
  if Self.VM <> nil then
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
  Self.ConstLookup := TSEConstLookup.Create;
  Self.ConstList := TSEValueList.Create;
  Self.JITBlockSignatureStack := TSEJITBlockSignatureStack.Create;
  Self.ScopeStack := TSEScopeStack.Create;
  Self.ScopeFunc := TSEScopeStack.Create;
  Self.LineOfCodeList := TSELineOfCodeList.Create;
  Self.IncludeList := TStringList.Create;
  Self.IncludePathList := TStringList.Create;
  Self.CurrentFileList := TStringList.Create;
  Self.LocalVarCountList := TSEIntegerList.Create;
  //
  Self.OptimizeConstants := True;
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
  Self.JITBlockCount := $1FFFF;
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
    Self.RegisterFunc('string_resize', @TBuiltInFunction(nil).SEStringResize, 2);
    Self.RegisterFunc('string_format', @TBuiltInFunction(nil).SEStringFormat, -1);
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
    Self.RegisterFunc('round_to', @TBuiltInFunction(nil).SERoundTo, 2);
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
    Self.RegisterFunc('invoke', @TBuiltInFunction(nil).SEInvoke, -1);
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
  I: NativeInt;
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
  FreeAndNil(Self.ConstList);
  FreeAndNil(Self.ConstLookup);
  FreeAndNil(Self.ScopeStack);
  FreeAndNil(Self.ScopeFunc);
  FreeAndNil(Self.LineOfCodeList);
  FreeAndNil(Self.IncludeList);
  FreeAndNil(Self.IncludePathList);
  FreeAndNil(Self.CurrentFileList);
  FreeAndNil(Self.LocalVarCountList);
  FreeAndNil(Self.GlobalVarSymbols);
  FreeAndNil(Self.JITBlockSignatureStack);
  inherited;
end;

procedure TEvilC.AddDefaultConsts;
begin
  Self.SetConst('PI', PI);
  Self.SetConst('true', True);
  Self.SetConst('false', False);
  Self.SetConst('null', SENull);
  Self.SetConst('os', GetOS);
  Self.SetConst('sevkNumber', TSENumber(NativeInt(sevkNumber)));
  Self.SetConst('sevkString', TSENumber(NativeInt(sevkString)));
  Self.SetConst('sevkPascalObject', TSENumber(NativeInt(sevkPascalObject)));
  Self.SetConst('sevkBuffer', TSENumber(NativeInt(sevkBuffer)));
  Self.SetConst('sevkMap', TSENumber(NativeInt(sevkMap)));
  Self.SetConst('sevkNull', TSENumber(NativeInt(sevkNull)));
  Self.SetConst('sevkFunction', TSENumber(NativeInt(sevkFunction)));
  Self.SetConst('sevkPointer', TSENumber(NativeInt(sevkPointer)));
  {$ifdef SE_THREADS}
  Self.SetConst('wrSignaled', TSENumber(NativeInt(wrSignaled)));
  Self.SetConst('wrTimeout', TSENumber(NativeInt(wrTimeout)));
  Self.SetConst('wrAbandoned', TSENumber(NativeInt(wrAbandoned)));
  Self.SetConst('wrError', TSENumber(NativeInt(wrError)));
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
  Ln, Col: NativeInt;
  Pos: NativeInt = 0;
  Token: TSEToken;
  C, PC, NC: Char;
  IsScientificNotation: Boolean;

  function PeekAtNextChar: Char; inline;
  var
    P: NativeInt;
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
            'var':
              Token.Kind := tkVar;
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
            'override':
              Token.Kind := tkOverride;
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
  I: NativeInt;
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

function TEvilC.FindFuncNative(const Name: String; var Ind: Cardinal): PSEFuncNativeInfo; inline;
var
  I: Cardinal;
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

function TEvilC.FindFuncScript(const Name: String; var Ind: Cardinal): PSEFuncScriptInfo; inline;
var
  I: Cardinal;
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

function TEvilC.FindFuncImport(const Name: String; var Ind: Cardinal): PSEFuncImportInfo; inline;
var
  I: Cardinal;
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

function TEvilC.FindFunc(const Name: String; var Kind: TSEFuncKind; var Ind: Cardinal): Pointer; inline; overload;
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

procedure TEvilC.SetConst(const Name: String; const Value: TSEValue);
var
  Index: NativeInt;
begin
  if not Self.ConstLookup.TryGetValue(Name, Index) then
  begin
    Self.ConstList.Add(Value);
    Index := ConstList.Count - 1;
    Self.ConstLookup.Add(Name, Index);
  end else
  begin
    Self.ConstList[Index] := Value;
  end;
end;

procedure TEvilC.Parse;
var
  Pos: NativeInt = -1;
  CurrentLine: NativeInt = -1;
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
    I: NativeInt;
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

  function PeekAtNextToken: TSEToken; inline;
  var
    P: NativeInt;
  begin
    P := Pos + 1;
    if P >= Self.TokenList.Count then
      P := P - 1;
    Exit(Self.TokenList[P]);
  end;

  function PeekAtNextNextToken: TSEToken; inline;
  var
    P: NativeInt;
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
      LineOfCode.CodeIndex := Self.Binary.Count;
      LineOfCode.CodeSegmentIndex := Self.CodeSegmentIndex;
      LineOfCode.Line := Result.Ln;
      LineOfCode.Module := Result.BelongedFileName;
      CurrentLine := LineOfCode.Line;
      Self.LineOfCodeList.Add(LineOfCode);
    end;
  end;

  function TokenTypeString(const Kinds: TSETokenKindSet): String; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := '';
    for Kind in Kinds do
      Result := Result + '"' + TokenNames[Kind] + '", ';
  end;

  function NextTokenExpected(const Expected: TSETokenKindSet): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := NextToken;
    if Result.Kind in Expected then
      Exit;
    Error(Format('Expected %s but got %s', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function PeekAtNextTokenExpected(const Expected: TSETokenKindSet): TSEToken; inline;
  var
    Kind: TSETokenKind;
  begin
    Result := PeekAtNextToken;
    if Result.Kind in Expected then
      Exit;
    Error(Format('Expected %s but got "%s"', [TokenTypeString(Expected), TokenNames[Result.Kind]]), Result);
  end;

  function PeekAtPrevOp(const Ind: NativeInt): PSEOpcodeInfo; inline;
  var
    I: NativeInt;
  begin
    I := Self.OpcodeInfoList.Count - 1 - Ind;
    if I >= 0 then
      Result := Self.OpcodeInfoList.Ptr(I)
    else
      Result := nil;
  end;

  function PeekAtPrevOpExpected(const Ind: NativeInt; const Expected: TSEOpcodeSet): PSEOpcodeInfo; inline;
  var
    Op: TSEOpcode;
  begin
    Result := PeekAtPrevOp(Ind);
    if Result <> nil then
      if Result^.Op in Expected then
        Exit;
    Result := nil;
  end;

  procedure DeleteOps(const Count: NativeInt);
  var
    I: NativeInt;
    Size: NativeInt = 0;
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
    Result.PossibleKinds := [];
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

  function CreateConstString(const S: String): Cardinal; inline;
  begin
    if not ConstStringsLookup.TryGetValue(S, Result) then
    begin
      ConstStrings.Add(S);
      Result := ConstStrings.Count - 1;
      ConstStringsLookup.Add(S, Result);
    end;
  end;

  function CreateConstStringValue(const S: String): TSEValue; inline;
  begin
    Result.Kind := sevkConstString;
    Result.VarConstStringIndex := CreateConstString(S);
  end;

  procedure Rewind(const StartAddr, Count: NativeInt); inline;
  var
    Addr, I: NativeInt;
  begin
    for I := 0 to Count - 1 do
    begin
      Addr := StartAddr + I;
      Self.Binary.Add(Self.Binary[Addr]);
    end;
    Self.Binary.DeleteRange(StartAddr, Count);
  end;

  function Emit(const Data: array of TSEValue): NativeInt; inline;
  var
    I: NativeInt;
    OpcodeInfo: TSEOpcodeInfo;
  begin
    if not CanEmit then
      Exit(Self.Binary.Count);
    OpcodeInfo.Pos := Self.Binary.Count;
    OpcodeInfo.Size := Length(Data);
    OpcodeInfo.Binary := Self.Binary;
    if (NativeInt(Data[0].VarPointer) = NativeInt(opPushConst)) and (Data[1].Kind = sevkString) then
    begin
      // Use EmitConstString() instead
      OpcodeInfo.Op := opPushConstString;
      Self.Binary.Add(Pointer(opPushConstString));
      Self.Binary.Add(Pointer(CreateConstString(Data[1].VarString^)));
    end else
    begin
      OpcodeInfo.Op := TSEOpcode(NativeInt(Data[0].VarPointer));
      for I := Low(Data) to High(Data) do
      begin
        Self.Binary.Add(Data[I]);
      end;
    end;
    Self.OpcodeInfoList.Add(OpcodeInfo);
    Exit(Self.Binary.Count);
  end;

  function EmitConstString(const AString: String): NativeInt; inline;
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

  procedure MarkJITBlock;
  begin
    Self.JITBlockSignatureStack.Push(Self.JITBlockCount);
    Emit([Pointer(opJITBlockPotential), Pointer(Self.JITBlockCount)]);
    Inc(Self.JITBlockCount);
  end;

  function VerifyJITBlock(const APossibleKinds: TSEValueKindSet): TSEValueKindSet;
  var
    Sig: NativeInt;
    BIndex, BIndex2, OpCount: NativeInt;
    Op, Op2: TSEOpcode;
  begin
    Result := APossibleKinds;
    Sig := Self.JITBlockSignatureStack.Pop;
    BIndex := 0;
    while BIndex <= Self.Binary.Count - 1 do
    begin
      Op := TSEOpcode(NativeInt(Self.Binary.Ptr(BIndex)^.VarPointer));
      if (Op = opJITBlockPotential) and (Self.Binary.Ptr(BIndex + 1)^.VarPointer = Pointer(Sig)) then
      begin
        OpCount := 0;
        BIndex2 := BIndex;
        //
        while BIndex2 <= Self.Binary.Count - 1 do
        begin
          Op2 := TSEOpcode(NativeInt(Self.Binary.Ptr(BIndex2)^.VarPointer));
          Inc(OpCount);
          Inc(BIndex2, OpcodeSizes[Op2]);
        end;
        //
        if (APossibleKinds <> [sevkNumber]) or (OpCount < 3) then
        begin
          Self.Binary.DeleteRange(BIndex, OpcodeSizes[Op]);
        end else
        begin
          Self.Binary.Ptr(BIndex + 1)^.VarPointer := Pointer(Self.Binary.Count - 1 - BIndex);
        end;
        break;
      end;
      Inc(BIndex, OpcodeSizes[Op]);
    end;
  end;

  function EmitPushVar(const Ident: TSEIdent): NativeInt; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opPushLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opPushGlobalVar), Pointer(Ident.Addr)]);
  end;

  function EmitAssignVar(const Ident: TSEIdent): NativeInt; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalVar), Pointer(Ident.Addr), Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalVar), Pointer(Ident.Addr)]);
  end;

  function EmitAssignArray(const Ident: TSEIdent; const ArgCount: NativeInt): NativeInt; inline;
  begin
    if Ident.Local > 0 then
      Result := Emit([Pointer(opAssignLocalArray), Ident.Addr, ArgCount, Pointer(Self.FuncTraversal - Ident.Local)])
    else
      Result := Emit([Pointer(opAssignGlobalArray), Ident.Addr, ArgCount]);
  end;

  procedure Patch(const Addr: NativeInt; const Data: TSEValue); inline;
  begin
    Self.Binary[Addr] := Data;
  end;

  function PatchRange(const Addr: NativeInt; const Data: array of TSEValue): NativeInt; inline;
  var
    I: NativeInt;
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
    if Self.ConstLookup.{$ifdef SE_MAP_AVK959}Contains{$else}ContainsKey{$endif}(Ident) then
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

  function ParseFuncCall(const Name: String): TSEValueKindSet; forward;
  procedure ParseFuncRefCall(const ThisRefIdent: PSEIdent = nil); forward;
  procedure ParseFuncRefCallByName(const Name: String); forward;
  procedure ParseBlock(const IsCase: Boolean = False); forward;
  procedure ParseArrayAssign; forward;
  procedure ParseFuncAnonDecl(const ATraversal: Cardinal = 1); forward;

  function OpToOp0(const Op: TSEOpcode): TSEOpcode; inline;
  begin
    case Op of
      opOperatorAdd:
        Result := opOperatorAdd0;
      opOperatorMul:
        Result := opOperatorMul0;
      opOperatorDiv:
        Result := opOperatorDiv0;
      opOperatorAnd:
        Result := opOperatorAnd0;
      opOperatorOr:
        Result := opOperatorOr0;
      opOperatorEqual:
        Result := opOperatorEqual0;
      opOperatorNotEqual:
        Result := opOperatorNotEqual0;
      opOperatorGreater:
        Result := opOperatorGreater0;
      opOperatorGreaterOrEqual:
        Result := opOperatorGreaterOrEqual0;
      opOperatorLesser:
        Result := opOperatorLesser0;
      opOperatorLesserOrEqual:
        Result := opOperatorLesserOrEqual0;
    end;
  end;

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

  function PeepholeArrayAssignOptimization: Boolean;
  var
    A: TSEValue;
    Size,
    I: NativeInt;
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
    I: NativeInt;
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
    I: NativeInt;
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
            opOperatorAdd, opOperatorSub, opOperatorMul, opOperatorDiv,
            opOperatorGreater, opOperatorGreaterOrEqual, opOperatorLesser, opOperatorLesserOrEqual,
            opOperatorEqual, opOperatorNotEqual, opOperatorAnd, opOperatorOr, opOperatorXor, opOperatorNot,
            opOperatorInc, opOperatorNegative,
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
              Emit([Pointer(NativeInt(opOperatorAdd0)), A.VarNumber])
            else
              Emit([Pointer(NativeInt(opOperatorAdd0)), -A.VarNumber]);
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
              Emit([Pointer(NativeInt(opOperatorAdd0)), A.VarNumber]);
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
            opOperatorAdd, opOperatorSub, opOperatorMul, opOperatorDiv,
            opOperatorGreater, opOperatorGreaterOrEqual, opOperatorLesser, opOperatorLesserOrEqual,
            opOperatorEqual, opOperatorNotEqual, opOperatorAnd, opOperatorOr, opOperatorXor, opOperatorNot,
            opOperatorInc, opOperatorNegative,
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
            Emit([Pointer(NativeInt(opOperatorMul0)), A.VarNumber]);
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
              Emit([Pointer(NativeInt(opOperatorMul0)), A.VarNumber]);
              Result := True;
            end;
          end;
        end;
      opOperatorDiv,
      opOperatorEqual,
      opOperatorNotEqual,
      opOperatorGreater,
      opOperatorGreaterOrEqual,
      opOperatorLesser,
      opOperatorLesserOrEqual,
      opOperatorAnd,
      opOperatorOr:
        begin
          OpInfoPrev1 := PeekAtPrevOpExpected(0, [opPushConst]);
          OpInfoPrev2 := PeekAtPrevOpExpected(1, [
            opPushGlobalVar, opPushLocalVar,
            opOperatorAdd0, opOperatorMul0, opOperatorDiv0,
            opOperatorAdd1, opOperatorSub1, opOperatorMul1, opOperatorDiv1,
            opOperatorAdd, opOperatorSub, opOperatorMul, opOperatorDiv,
            opOperatorGreater, opOperatorGreaterOrEqual, opOperatorLesser, opOperatorLesserOrEqual,
            opOperatorEqual, opOperatorNotEqual, opOperatorAnd, opOperatorOr, opOperatorXor, opOperatorNot,
            opOperatorInc, opOperatorNegative,
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
            Emit([Pointer(NativeInt(OpToOp0(Op))), A.VarNumber]);
            Result := True;
          end;
        end;
    end;
  end;

  function PeepholeOp1Optimization(Op: TSEOpcode): Boolean;
  var
    A: TSEValue;
    I: NativeInt;
    P: Pointer;
    OpInfoPrev1: PSEOpcodeInfo;
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
            Emit([Pointer(NativeInt(Op)), A.VarPointer, Pointer(P)]);
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
      IsOptimized := PeepholeOp1Optimization(Op);
      if IsOptimized then
      begin
        Result := True;
        Op := PeekAtPrevOp(0)^.Op;
        continue;
      end;
    until not IsOptimized;
  end;

  function ParseExpr(const IsParsedAtFuncCall: Boolean = False): TSEValueKindSet;
  type
    TProc = TSENestedProc;
  var
    PushConstCount: NativeInt = 0;
    OpCountStart: NativeInt;
    IsTailed: Boolean = False;
    FuncRefIdent: TSEIdent;
    FuncRefToken: TSEToken;
    AssignReturnFuncRefCount: NativeInt = 0;
    AssignReturnFuncRefOpStart,
    AssignReturnFuncRefOpEnd,
    AssignReturnFuncRefStart,
    AssignReturnFuncRefEnd: NativeInt;

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

    procedure AssignReturnFuncRef;
    begin
      if AssignReturnFuncRefCount > 0 then
      begin
        Self.Binary.DeleteRange(AssignReturnFuncRefStart, AssignReturnFuncRefEnd - AssignReturnFuncRefStart);
        Self.OpcodeInfoList.DeleteRange(AssignReturnFuncRefOpStart, AssignReturnFuncRefOpEnd - AssignReturnFuncRefOpStart);
      end;
      AssignReturnFuncRefStart := Self.Binary.Count;
      AssignReturnFuncRefOpStart := Self.OpcodeInfoList.Count;
      EmitAssignVar(FuncRefIdent);
      EmitPushVar(FuncRefIdent);
      AssignReturnFuncRefEnd := Self.Binary.Count;
      AssignReturnFuncRefOpEnd := Self.OpcodeInfoList.Count;
      Inc(AssignReturnFuncRefCount);
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
                SEValueMul(V, V2, V1);
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
                Emit([Pointer(opPushConst), NativeInt(V1) and NativeInt(V2)]);
              end;
            opOperatorOr:
              begin
                Pop2;
                Emit([Pointer(opPushConst), NativeInt(V1) or NativeInt(V2)]);
              end;
            opOperatorXor:
              begin
                Pop2;
                Emit([Pointer(opPushConst), NativeInt(V1) xor NativeInt(V2)]);
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
          S2 := ConstStrings[NativeInt(Self.Binary[Self.Binary.Count - 1].VarPointer)];
          S1 := ConstStrings[NativeInt(Self.Binary[Self.Binary.Count - 3].VarPointer)];
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
        Op := TSEOpcode(NativeInt(Data[0].VarPointer));
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
            Result := Result + [sevkMap];
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            ParseExpr(False);
            NextTokenExpected([tkSquareBracketClose]);
            AllocFuncRef;
            AssignReturnFuncRef;
            EmitExpr([Pointer(opPushArrayPop), SENull]);
            PeepholeArrayAssignOptimization;
            Tail;
          end;
        tkDot:
          begin
            Result := Result + [sevkMap];
            PushConstCount := 0;
            IsTailed := True;
            NextToken;
            Token := NextTokenExpected([tkIdent]);
            AllocFuncRef;
            AssignReturnFuncRef;
            EmitExpr([Pointer(opPushArrayPop), CreateConstStringValue(Token.Value)]);
            Tail;
          end;
      end;
    end;

    procedure Factor;
    var
      Token, Token2: TSEToken;
      Ident: PSEIdent;
      V,
      FuncValue: TSEValue;
      Ind: Cardinal;
      P: Pointer;

      procedure FuncTail(IsFirst: Boolean = True);
      begin
        while PeekAtNextToken.Kind = tkBracketOpen do
        begin
          Result := Result + [sevkFunction];
          AssignReturnFuncRefCount := 0;
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
            EmitPushVar(FuncRefIdent);
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
              Result := Result + [sevkFunction];
              Factor;
              NextTokenExpected([tkBracketClose]);
              if PeekAtNextToken.Kind = tkBracketOpen then
              begin
                AllocFuncRef;
                EmitAssignVar(FuncRefIdent);
                EmitPushVar(FuncRefIdent);
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
            Result := Result + [sevkFunction];
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
            Result := Result + [sevkNumber];
            NextToken;
            EmitExpr([Pointer(opPushConst), PointStrToFloat(Token.Value)]);
          end;
        tkString:
          begin
            Result := Result + [sevkString];
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
                    Result := Result + [sevkFunction];
                    ParseFuncRefCallByName(Token.Value);
                  end else
                  begin
                    Ident := FindVar(Token.Value);
                    Result := Result + Ident^.PossibleKinds;
                    Ident^.IsUsed := True;
                    if Ident^.IsConst and (Ident^.ConstValue.Kind <> sevkNull) then
                    begin
                      EmitExpr([Pointer(opPushConst), Ident^.ConstValue]);
                    end else
                    begin
                      case PeekAtNextToken.Kind of
                        tkSquareBracketOpen:
                          begin
                            Result := Result + [sevkMap];
                            PushConstCount := 0;
                            IsTailed := True;
                            NextToken;
                            EmitPushVar(Ident^);
                            ParseExpr(False);
                            Emit([Pointer(opPushArrayPop), SENull]);
                            PeepholeArrayAssignOptimization;
                            NextTokenExpected([tkSquareBracketClose]);
                            Tail;
                            FuncTail;
                          end;
                        tkDot:
                          begin
                            Result := Result + [sevkMap];
                            PushConstCount := 0;
                            IsTailed := True;
                            NextToken;
                            Token2 := NextTokenExpected([tkIdent]);
                            EmitPushVar(Ident^);
                            Emit([Pointer(opPushArrayPop), CreateConstStringValue(Token2.Value)]);
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
                  Ind := Self.ConstLookup[Token.Value];
                  V := Self.ConstList[Ind];
                  if (not Self.OptimizeConstants) or (V.Kind in [sevkBuffer, sevkString, sevkMap, sevkPascalObject]) then
                  begin
                    EmitExpr([Pointer(opPushConstFromConstList), Pointer(Ind)])
                  end else
                  begin
                    EmitExpr([Pointer(opPushConst), V]);
                  end;
                  Result := Result + [V.Kind];
                end;
              tkFunction:
                begin
                  NextToken;
                  if PeekAtNextToken.Kind <> tkBracketOpen then // Likely function ref
                  begin
                    Result := Result + [sevkFunction];
                    P := FindFunc(Token.Value, FuncValue.VarFuncKind, Ind);
                    if P = nil then
                      Error(Format('Function "%s" not found', [Token.Value]), Token);
                    FuncValue.VarFuncIndx := Ind;
                    FuncValue.Kind := sevkFunction;
                    PushConstCount := 0;
                    EmitExpr([Pointer(opPushConst), FuncValue]);
                  end else
                  begin
                    Result := Result + ParseFuncCall(Token.Value);
                  end;
                  if PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] then
                  begin
                    Result := Result + [sevkMap];
                    FuncRefToken.Value := '___f' + Self.InternalIdent;
                    FuncRefToken.Kind := tkIdent;
                    FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True, False);
                    EmitAssignVar(FuncRefIdent);
                    EmitPushVar(FuncRefIdent);
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
      FuncInfo: PSEFuncNativeInfo;
      FuncIndex: Cardinal;
    begin
      SignedFactor;
      while True do
      begin
        Token := PeekAtNextToken;
        case Token.Kind of
          tkPow:
            begin
              // BinaryOp(opOperatorPow, @SignedFactor);
              NextToken;
              PeekAtNextTokenExpected([tkBracketOpen, tkSquareBracketOpen, tkDot, tkNumber, tkString, tkNegative, tkIdent]);
              SignedFactor;
              FuncInfo := FindFuncNative('pow', FuncIndex);
              Emit([Pointer(opCallNative), Pointer(FuncIndex), Pointer(2), Pointer(0)])
            end;
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
    JumpExpr2: NativeInt;

  begin
    Result := [];
    OpCountStart := Self.OpcodeInfoList.Count;
    Logic;
    //
    if AssignReturnFuncRefCount > 0 then
    begin
      Self.Binary.DeleteRange(AssignReturnFuncRefStart, AssignReturnFuncRefEnd - AssignReturnFuncRefStart);
      Self.OpcodeInfoList.DeleteRange(AssignReturnFuncRefOpStart, AssignReturnFuncRefOpEnd - AssignReturnFuncRefOpStart);
    end;
    // Handle ternary
    if PeekAtNextToken.Kind = tkQuestion then
    begin
      // We consider jump block as sevkFunction, this will hel the JIT to ignore generate code for the block...
      Result := Result + [sevkFunction];
      NextToken;
      JumpExpr2 := Emit([Pointer(opJumpEqual1Rel), False, Pointer(0)]);
      Result := Result + ParseExpr(False);
      NextTokenExpected([tkColon]);
      JumpEnd := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
      Expr2Block := Self.Binary.Count;
      Result := Result + ParseExpr(False);
      EndBlock := Self.Binary.Count;
      Patch(JumpExpr2 - 1, Pointer(Expr2Block) - (JumpExpr2 - 3));
      Patch(JumpEnd - 1, Pointer(EndBlock) - (JumpEnd - 2));
    end;
  end;

  procedure ParseFuncRefCallByMapRewind(const Ident: TSEIdent; const DeepCount, RewindStartAdd: NativeInt; const ThisRefIdent: PSEIdent = nil);
  var
    Token: TSEToken;
    ArgCount: NativeInt = 1;
    RewindCount: NativeInt;
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
      MarkJITBlock;
      VerifyJITBlock(ParseExpr(True));
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^)
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
    EmitPushVar(Ident);
    Emit([Pointer(opCallRef), Pointer(0), Pointer(ArgCount), Pointer(DeepCount)]);
    if PeekAtNextToken.Kind = tkBracketOpen then
      ParseFuncRefCall;
  end;

  procedure ParseFuncRefCall(const ThisRefIdent: PSEIdent = nil);
  var
    FuncIdent: TSEIdent;
    FuncToken: TSEToken;
    Token: TSEToken;
    ArgCount: NativeInt = 1;
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
      MarkJITBlock;
      VerifyJITBlock(ParseExpr(True));
      Inc(ArgCount);
      Token := NextTokenExpected([tkComma, tkBracketClose]);
    end;
    // Allocate stack for this
    if ThisRefIdent <> nil then
      EmitPushVar(ThisRefIdent^)
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
    ArgCount: NativeInt = 1;
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
      MarkJITBlock;
      VerifyJITBlock(ParseExpr(True));
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

  function ParseFuncCall(const Name: String): TSEValueKindSet;
  var
    FuncNativeInfo: PSEFuncNativeInfo = nil;
    FuncScriptInfo: PSEFuncScriptInfo = nil;
    FuncImportInfo: PSEFuncImportInfo = nil;
    I: NativeInt;
    Ind: Cardinal;
    DefinedArgCount: NativeInt;
    ArgCount: NativeInt = 0;
    Token: TSEToken;
    This: PSEIdent;
  begin
    FuncNativeInfo := FindFuncNative(Name, Ind);
    if FuncNativeInfo <> nil then
    begin
      DefinedArgCount := FuncNativeInfo^.ArgCount;
      Result := FuncNativeInfo^.PossibleKinds;
    end else
    begin
      FuncScriptInfo := FindFuncScript(Name, Ind);
      if FuncScriptInfo <> nil then
      begin
        DefinedArgCount := FuncScriptInfo^.ArgCount;
        Result := FuncScriptInfo^.PossibleKinds;
      end else
      begin
        FuncImportInfo := FindFuncImport(Name, Ind);
        if FuncImportInfo <> nil then
        begin
          DefinedArgCount := Length(FuncImportInfo^.Args);
          Result := FuncImportInfo^.PossibleKinds;
        end;
      end;
    end;
    if FuncScriptInfo <> nil then // Allocate stack for result
      Emit([Pointer(opPushConst), SENull]);
    if DefinedArgCount > 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      for I := 0 to DefinedArgCount - 1 do
      begin
        MarkJITBlock;
        VerifyJITBlock(ParseExpr(True));
        if I < DefinedArgCount - 1 then
          NextTokenExpected([tkComma]);
        Inc(ArgCount);
      end;
      NextTokenExpected([tkBracketClose]);
    end else
    if DefinedArgCount < 0 then
    begin
      NextTokenExpected([tkBracketOpen]);
      if PeekAtNextToken.Kind <> tkBracketClose then
      begin
        repeat
          MarkJITBlock;
          VerifyJITBlock(ParseExpr(True));
          Inc(ArgCount);
          Token := NextTokenExpected([tkComma, tkBracketClose]);
          if (Token.Kind = tkComma) and (PeekAtNextToken.Kind = tkBracketClose) then
          begin
            NextToken;
            break;
          end;
        until Token.Kind = tkBracketClose;
      end else
        NextToken;
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
    OldFuncCurrent: NativeInt;
    ArgCount: NativeInt = 0;
    I: NativeInt;
    FuncIndex: Cardinal;
    ReturnList: TList;
    Func: PSEFuncScriptInfo;
    ParentBinary: TSEBinary;
    ParentBinaryPos: NativeInt;
    VarSymbols: TStrings;
    This: PSEIdent;
    HasOverride: Boolean = False;
  begin
    ReturnList := TList.Create;
    VarSymbols := TStringList.Create;
    try
      OldFuncCurrent := Self.FuncCurrent;
      ReturnStack.Push(ReturnList);
      if not IsAnon then
      begin
        if PeekAtNextToken.Kind = tkOverride then
        begin
          NextToken;
          HasOverride := True;
        end;
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
      Func := RegisterScriptFunc(Name, 0, FuncIndex, HasOverride);

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
      ParentBinary := Self.Binary;
      ParentBinaryPos := Self.CodeSegmentIndex;
      Self.Binary := Self.VM.Binaries.Value^.Data[Func^.CodeSegmentIndex];
      Self.CodeSegmentIndex := Func^.CodeSegmentIndex;
      if PeekAtNextToken.Kind = tkEqual then
        Self.TokenList.Insert(Pos + 1, TokenResult);
      ParseBlock;

      This := FindVar('self', True);
      if (not This^.IsUsed) or (not This^.IsAssigned) then
        Func^.HasSelf := False;

      ReturnList := ReturnStack.Pop;
      for I := 0 to ReturnList.Count - 1 do
        Patch(NativeInt(ReturnList[I]), Pointer(Self.Binary.Count) - (NativeInt(ReturnList[I]) - 2));
      Emit([Pointer(opPopFrame)]);

      // The pointer may be changed due to reallocation, need to query for it again
      Func := Self.FuncScriptList.Ptr(FuncIndex);
      Func^.VarCount := Self.LocalVarCountList[Self.LocalVarCountList.Count - 1] - ArgCount;
      Self.Binary := ParentBinary;
      Self.CodeSegmentIndex := ParentBinaryPos;
    finally
      Self.FuncCurrent := OldFuncCurrent;
      ReturnList.Free;
      VarSymbols.Free;
    end;
  end;

  procedure ParseFuncAnonDecl(const ATraversal: Cardinal = 1);
  var
    I, J: NativeInt;
    FuncValue: TSEValue;
    Ind: Cardinal;
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
        FuncValue.VarFuncIndx := NativeUInt(P);
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
          Writeln(' - Library''s pointer: ', NativeUInt(Lib));
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
    JumpEnd: NativeInt;
    BreakList,
    ContinueList: TList;
    I: NativeInt;
    IsComparison: Boolean = True;
    OpCount: NativeInt;
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
        ParseExpr(False);
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
          JumpEnd := Emit([Pointer(opJumpEqual1Rel), False, Pointer(0)]);
        end;
      end;
      ParseBlock;
      JumpBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(NativeInt(ContinueList[I]), Pointer(StartBlock) - (NativeInt(ContinueList[I]) - 1));
      for I := 0 to BreakList.Count - 1 do
        Patch(NativeInt(BreakList[I]), Pointer(EndBlock) - (NativeInt(BreakList[I]) - 1));
      Patch(JumpBlock - 1, Pointer(StartBlock) - (JumpBlock - 2));
      if IsComparison then
        Patch(JumpEnd - 1, Pointer(EndBlock) - (JumpEnd - 3));
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
    JumpEnd: NativeInt;
    BreakList,
    ContinueList: TList;
    I: NativeInt;
    IsComparison: Boolean = True;
    OpCount: NativeInt;
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
        ParseExpr(False);
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
          JumpEnd := Emit([Pointer(opJumpEqual1Rel), False, Pointer(0)]);
        end;
      end;
      JumpBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
      EndBlock := Self.Binary.Count;
      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(NativeInt(ContinueList[I]), Pointer(ContinueBlock) - (NativeInt(ContinueList[I]) - 1));
      for I := 0 to BreakList.Count - 1 do
        Patch(NativeInt(BreakList[I]), Pointer(EndBlock) - (NativeInt(BreakList[I]) - 1));
      Patch(JumpBlock - 1, Pointer(StartBlock) - (JumpBlock - 2));
      if IsComparison then
        Patch(JumpEnd - 1, Pointer(EndBlock) - (JumpEnd - 3));
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
    JumpEnd: NativeInt;
    BreakList,
    ContinueList: TList;
    I: NativeInt;
    Token: TSEToken;
    VarIdent,
    VarHiddenTargetIdent,
    VarHiddenCountIdent,
    VarHiddenArrayIdent: TSEIdent;
    VarHiddenTargetName,
    VarHiddenCountName,
    VarHiddenArrayName: String;
    Ind: Cardinal;
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

        ParseExpr(False);
        EmitAssignVar(VarIdent);

        Token := NextTokenExpected([tkTo, tkDownto]);

        ParseExpr(False);

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
          JumpEnd := Emit([Pointer(opJumpEqualOrGreater2Rel), Pointer(VarIdent.Addr), GetIdentLocalValue(VarIdent), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(0)]);
        end else
        if Token.Kind = tkDownto then
        begin
          JumpEnd := Emit([Pointer(opJumpEqualOrLesser2Rel), Pointer(VarIdent.Addr), GetIdentLocalValue(VarIdent), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(0)]);
        end;

        ParseBlock;

        ContinueBlock := Self.Binary.Count;
        Emit([Pointer(opOperatorInc), Pointer(VarIdent.Addr), GetVarFrame(VarIdent), Step]);
        JumpBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
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

        ParseExpr(False);

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
        JumpEnd := Emit([Pointer(opJumpEqualOrLesser2Rel), Pointer(VarHiddenTargetIdent.Addr), GetIdentLocalValue(VarHiddenTargetIdent), Pointer(VarHiddenCountIdent.Addr), GetIdentLocalValue(VarHiddenCountIdent), Pointer(0)]);

        EmitPushVar(VarHiddenArrayIdent);
        EmitPushVar(VarHiddenCountIdent);
        Emit([Pointer(opPushArrayPop), SENull]);
        PeepholeArrayAssignOptimization;
        EmitAssignVar(VarIdent);

        ParseBlock;

        ContinueBlock := Self.Binary.Count;
        Emit([Pointer(opOperatorInc), Pointer(VarHiddenCountIdent.Addr), GetVarFrame(VarHiddenCountIdent), 1]);
        JumpBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
        EndBLock := JumpBlock;
      end;

      ContinueList := ContinueStack.Pop;
      BreakList := BreakStack.Pop;
      for I := 0 to ContinueList.Count - 1 do
        Patch(NativeInt(ContinueList[I]), Pointer(ContinueBlock) - (NativeInt(ContinueList[I]) - 1));
      for I := 0 to BreakList.Count - 1 do
        Patch(NativeInt(BreakList[I]), Pointer(EndBlock) - (NativeInt(BreakList[I]) - 1));
      Patch(JumpBlock - 1, Pointer(StartBlock) - (JumpBlock - 2));
      Patch(JumpEnd - 1, Pointer(EndBlock) - (JumpEnd - 6));
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
    JumpEnd: NativeInt;
  begin
    ParseExpr(False);
    JumpBlock1 := Emit([Pointer(opJumpEqual1Rel), True, Pointer(0)]);
    JumpBlock2 := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
    StartBlock1 := Self.Binary.Count;
    ParseBlock;
    StartBlock2 := Self.Binary.Count;
    JumpEnd := -1;
    if PeekAtNextToken.Kind = tkElse then
    begin
      JumpEnd := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
      StartBlock2 := Self.Binary.Count;
      NextToken;
      ParseBlock;
    end;
    EndBlock2 := Self.Binary.Count;
    Patch(JumpBlock1 - 1, Pointer(StartBlock1) - (JumpBlock1 - 3));
    Patch(JumpBlock2 - 1, Pointer(StartBlock2) - (JumpBlock2 - 2));
    if JumpEnd >= 0 then
      Patch(JumpEnd - 1, Pointer(EndBlock2) - (JumpEnd - 2));
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
    I: NativeInt;
  begin
    Token.Kind := tkIdent;
    Token.Value := '___s' + Self.InternalIdent;
    VarHiddenIdent := CreateIdent(ikVariable, Token, True, False);

    ParseExpr(False);
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
          ParseExpr(False);
          EmitPushVar(VarHiddenIdent);
          JumpBlock1 := Emit([Pointer(opJumpEqualRel), Pointer(0)]);
          JumpBlock2 := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
        end;
        StartCaseBlock := Self.Binary.Count;
        if JumpNextBlock <> -1 then
        begin
          Patch(JumpNextBlock - 1, Pointer(StartCaseBlock) - (JumpNextBlock - 2));
          JumpNextBlock := -1;
        end;
        PeekAtNextTokenExpected([tkColon]);
        ParseBlock(True);
        if Token.Kind = tkCase then
        begin
          JumpNextBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);
          EndCaseBlock := Self.Binary.Count;
          Patch(JumpBlock1 - 1, Pointer(StartCaseBlock) - (JumpBlock1 - 2));
          Patch(JumpBlock2 - 1, Pointer(EndCaseBlock) - (JumpBlock2 - 2));
        end else
          Break;
      end;
      NextTokenExpected([tkEnd]);
      EndBlock := Self.Binary.Count;

      BreakList := BreakStack.Pop;

      for I := 0 to BreakList.Count - 1 do
        Patch(NativeInt(BreakList[I]), Pointer(EndBlock) - (NativeInt(BreakList[I]) - 1));
    finally
      BreakList.Free;
    end;
  end;

  procedure ParseArrayAssign;
  var
    FuncNativeInfo: PSEFuncNativeInfo;
    I: NativeInt;
    Ind: Cardinal;
    ArgCount: NativeInt = 0;
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
          ParseExpr(False);
          Inc(ArgCount, 2);
        end else
        begin
          Emit([Pointer(opPushConst), I]);
          ParseExpr(False);
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
    AssignReturnFuncRefCount: NativeInt = 0;
    AssignReturnFuncRefOpStart,
    AssignReturnFuncRefOpEnd,
    AssignReturnFuncRefStart,
    AssignReturnFuncRefEnd: NativeInt;

    procedure AssignReturnFuncRef;
    begin
      if AssignReturnFuncRefCount > 0 then
      begin
        Self.Binary.DeleteRange(AssignReturnFuncRefStart, AssignReturnFuncRefEnd - AssignReturnFuncRefStart);
        Self.OpcodeInfoList.DeleteRange(AssignReturnFuncRefOpStart, AssignReturnFuncRefOpEnd - AssignReturnFuncRefOpStart);
      end;
      AssignReturnFuncRefStart := Self.Binary.Count;
      AssignReturnFuncRefOpStart := Self.OpcodeInfoList.Count;
      EmitAssignVar(FuncRefIdent);
      EmitPushVar(FuncRefIdent);
      AssignReturnFuncRefEnd := Self.Binary.Count;
      AssignReturnFuncRefOpEnd := Self.OpcodeInfoList.Count;
      Inc(AssignReturnFuncRefCount);
    end;

  begin
    while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
    begin
      if FuncRefToken.Value = '' then
      begin
        FuncRefToken.Value := '___f' + Self.InternalIdent;
        FuncRefToken.Kind := tkIdent;
        FuncRefIdent := CreateIdent(ikVariable, FuncRefToken, True, False);
      end;
      AssignReturnFuncRef;
      while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
      begin
        case PeekAtNextToken.Kind of
          tkSquareBracketOpen:
            begin
              NextToken;
              ParseExpr(False);
              NextTokenExpected([tkSquareBracketClose]);
              AssignReturnFuncRef;
              Emit([Pointer(opPushArrayPop), SENull]);
              PeepholeArrayAssignOptimization;
            end;
          tkDot:
            begin
              NextToken;
              Token := NextTokenExpected([tkIdent]);
              AssignReturnFuncRef;
              Emit([Pointer(opPushArrayPop), CreateConstStringValue(Token.Value)]);
            end;
        end;
      end;
      if PeekAtNextToken.Kind = tkBracketOpen then
      begin
        AssignReturnFuncRefCount := 0;
        ParseFuncRefCall(@FuncRefIdent);
      end;
    end;
    if AssignReturnFuncRefCount > 0 then
    begin
      Self.Binary.DeleteRange(AssignReturnFuncRefStart, AssignReturnFuncRefEnd - AssignReturnFuncRefStart);
      Self.OpcodeInfoList.DeleteRange(AssignReturnFuncRefOpStart, AssignReturnFuncRefOpEnd - AssignReturnFuncRefOpStart);
    end;
    Emit([Pointer(opPopConst)]);
  end;

  procedure ParseVarAssign(const Name: String; const IsNew: Boolean);
  var
    Ident: PSEIdent;
    Token, Token2: TSEToken;
    ArgCount: NativeInt = 0;
    I, J,
    RewindStartAddr,
    OpBinaryStart,
    OpBinaryEnd,
    VarStartTokenPos,
    VarEndTokenPos: NativeInt;
    AccessNumber: TSEValue;
    AccessString: String;
    OpInfoPrev1: PSEOpcodeInfo;
  begin
    Ident := FindVar(Name);
    if Ident^.IsAssigned and Ident^.IsConst then
      Error(Format('Cannot reassign value to constant "%s"', [Name]), PeekAtNextToken);
      RewindStartAddr := Self.Binary.Count;
    VarStartTokenPos := Pos;
    while PeekAtNextToken.Kind in [tkSquareBracketOpen, tkDot] do
    begin
      if IsNew then
        Error(Format('Variable "%s" is not an array / a map', [Name]), PeekAtNextToken);
      case PeekAtNextToken.Kind of
        tkSquareBracketOpen:
          begin
            NextToken;
            ParseExpr(False);
            NextTokenExpected([tkSquareBracketClose]);
          end;
        tkDot:
          begin
            NextToken;
            Token2 := NextTokenExpected([tkIdent]);
            Emit([Pointer(opPushConst), CreateConstStringValue(Token2.Value)]);
          end;
      end;
      Inc(ArgCount);
    end;

    Token := PeekAtNextTokenExpected([tkEqual, tkOpAssign, tkBracketOpen]);
    case Token.Kind of
      tkEqual,
      tkOpAssign:
        begin
          VarEndTokenPos := Pos;
          NextToken;
          MarkJITBlock;
          if Token.Kind = tkOpAssign then
          begin
            if ArgCount > 0 then
            begin
              J := Pos + 1;
              for I := VarStartTokenPos to VarEndTokenPos do
              begin
                Self.TokenList.Insert(J, Self.TokenList[I]);
                Inc(J);
              end;
              Ident^.PossibleKinds := Ident^.PossibleKinds + ParseExpr(False);
            end else
              EmitPushVar(Ident^);
          end;
          Ident^.PossibleKinds := Ident^.PossibleKinds + ParseExpr(False);
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
          if ArgCount > 0 then
          begin
            VerifyJITBlock(Ident^.PossibleKinds);
            EmitAssignArray(Ident^, ArgCount);
          end else
          begin
            VerifyJITBlock(Ident^.PossibleKinds);
            EmitAssignVar(Ident^);
            PeepholeIncOptimization;
          end;
        end;
      tkBracketOpen:
        begin
          if IsNew then
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
    JumpFinallyBlock: NativeInt;
  begin
    JumpCatchBlock := Emit([Pointer(opPushTrap), Pointer(0)]);
    ParseBlock;
    Emit([Pointer(opPopTrap)]);
    JumpFinallyBlock := Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]);

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
    Patch(JumpFinallyBlock - 1, Pointer(Self.Binary.Count) - (JumpFinallyBlock - 2));
    I := Self.ScopeStack.Pop;
    Self.VarList.DeleteRange(I, Self.VarList.Count - I);
  end;

  procedure ParseThrow;
  begin
    ParseExpr(False);
    Emit([Pointer(opThrow)]);
  end;

  procedure ParseIdent(const Token: TSEToken; const IsConst, IsLocal: Boolean);
  var
    OpCountBefore,
    OpCountAfter: NativeInt;
    Ident: TSEIdent;
  begin
    case IdentifyIdent(Token.Value, IsLocal) of
      tkUnknown:
        begin
          NextToken;
          CreateIdent(ikVariable, Token, False, IsConst);
          OpCountBefore := Self.OpcodeInfoList.Count;
          ParseVarAssign(Token.Value, True);
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
          if PeekAtNextToken.Kind = tkBracketOpen then // Likely function ref
          begin
            ParseFuncRefCallByName(Token.Value);
            ParseAssignTail;
          end else
            ParseVarAssign(Token.Value, False);
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

  procedure ParseVar;
  var
    IsConst: Boolean = False;
    IsLocal: Boolean;
  begin
    repeat
      IsLocal := False;
      Token := PeekAtNextTokenExpected([tkIdent, tkLocal]);
      if Token.Kind = tkLocal then
      begin
        NextToken;
        IsLocal := True;
        Token := PeekAtNextTokenExpected([tkIdent]);
      end;
      if PeekAtNextNextToken.Kind = tkEqual then
      begin
        ParseIdent(Token, False, IsLocal);
      end else
      begin
        NextToken;
        if IdentifyIdent(Token.Value, IsLocal) = tkUnknown then
          CreateIdent(ikVariable, Token, False, False)
        else
          Error(Format('Duplicate variable declaration "%s"', [Token.Value]), Token);
      end;
      if PeekAtNextToken.Kind = tkComma then
      begin
        NextToken;
      end else
        break;
    until False;
  end;

  procedure ParseBlock(const IsCase: Boolean = False);
  var
    IsConst: Boolean = False;
    Token: TSEToken;
    Ident: TSEIdent;
    List: TList;
    I, J: NativeInt;
  begin
    Inc(Self.BlockTraversal);
    Token := PeekAtNextToken;
    case Token.Kind of
      tkConst:
        begin
          NextToken;
          Token := PeekAtNextTokenExpected([tkIdent]);
          ParseIdent(Token, True, False);
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
          ParseIdent(Token, IsConst, True);
        end;
      tkVar:
        begin
          NextToken;
          ParseVar;
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
          List.Add(Pointer(Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]) - 1));
        end;
      tkContinue:
        begin
          NextToken;
          if ContinueStack.Count = 0 then
            Error('Not in loop but "continue" found', Token);
          List := ContinueStack.Peek;
          List.Add(Pointer(Emit([Pointer(opJumpUnconditionalRel), Pointer(0)]) - 1));
        end;
      tkReturn:
        begin
          NextToken;
          if PeekAtNextToken.Kind = tkBracketOpen then
          begin
            NextToken;
            Token.Kind := tkEqual;
            TokenList.Insert(Pos + 1, Token); // Insert equal token
            ParseVarAssign('result', False);
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
              ParseVarAssign('result', False);
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
      tkIdent:
        begin
          ParseIdent(Token, False, False);
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

var
  Dummy: Cardinal;

begin
  // Implement assert function
  Self.RegisterScriptFunc('assert', 2, Dummy);
  if not Self.OptimizeAsserts then
  begin
    Self.Binary := Self.VM.Binaries.Value^.Data[1];
    Self.Binary.AddRange(FunctionAssert);
  end;
  // Implement ___throw function
  Self.RegisterScriptFunc('___throw', 1, Dummy);
  Self.Binary := Self.VM.Binaries.Value^.Data[2];
  Self.Binary.AddRange(FunctionThrow);
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
  I: NativeInt;
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
  Self.VM.IsDone := True;
  Self.Vm.IsPaused := False;
  Self.CodeSegmentIndex := 0;
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
  I: NativeInt;
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

function TEvilC.ExecFuncOnly(const AIndex: NativeInt; const Args: array of TSEValue): TSEValue;
var
  I: NativeInt;
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
    Self.VM.CodePtr := nil;
    Self.VM.CodeSegmentIndex := 0;
    Self.VM.IsPaused := False;
    Self.VM.IsDone := False;
    Self.VM.FramePtr := @Self.VM.Frame[0];
    Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
    Self.VM.FramePtr^.StackPtr := Self.VM.StackPtr;
    Self.VM.TrapPtr := @Self.VM.Trap[0];
    Dec(Self.VM.TrapPtr);
    Func := Self.FuncScriptList.Ptr(AIndex);
    Self.VM.CodeSegmentIndex := Func^.CodeSegmentIndex;
    Self.VM.StackPtr := Self.VM.StackPtr + Func^.ArgCount + Func^.VarCount;
    if Self.VM.CodeSegmentIndex <> 0 then
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
  I: NativeInt;
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

function TEvilC.ExecFunc(const AIndex: NativeInt; const Args: array of TSEValue): TSEValue;
var
  I: NativeInt;
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
      Self.VM.CodePtr := nil;
      Self.VM.CodeSegmentIndex := 0;
      Self.VM.IsPaused := False;
      Self.VM.IsDone := False;
      Self.VM.FramePtr := @Self.VM.Frame[0];
      Self.VM.StackPtr := PSEValue(@Self.VM.Stack[0]) + SE_STACK_RESERVED;
      Self.VM.FramePtr^.StackPtr := Self.VM.StackPtr;
      Self.VM.TrapPtr := @Self.VM.Trap[0];
      Dec(Self.VM.TrapPtr);
      Func := Self.FuncScriptList.Ptr(AIndex);
      Self.VM.CodeSegmentIndex := Func^.CodeSegmentIndex;
      Self.VM.StackPtr := Self.VM.StackPtr + Func^.ArgCount + Func^.VarCount;
      if Self.VM.CodeSegmentIndex <> 0 then
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

procedure TEvilC.RegisterFunc(const Name: String; const Func: TSEFunc; const ArgCount: NativeInt);
var
  FuncNativeInfo: TSEFuncNativeInfo;
begin
  FuncNativeInfo.ArgCount := ArgCount;
  FuncNativeInfo.Func := Func;
  FuncNativeInfo.Name := Name;
  FuncNativeInfo.PossibleKinds := [sevkNumber, sevkString, sevkNull, sevkMap, sevkFunction];
  Self.FuncNativeList.Add(FuncNativeInfo);
end;

function TEvilC.RegisterScriptFunc(const Name: String; const ArgCount: NativeInt; var AIndex: Cardinal; const IsOverride: Boolean = False): PSEFuncScriptInfo;
var
  P: PSEFuncScriptInfo;
  FuncScriptInfo: TSEFuncScriptInfo;
  I: Cardinal;
begin
  if IsOverride then
  begin
    for I := Self.FuncScriptList.Count - 1 downto 0 do
    begin
      P := Self.FuncScriptList.Ptr(I);
      if P^.Name = Name then
      begin
        AIndex := I;
        Self.FuncCurrent := I;
        Self.VM.Binaries.Value^.Data[P^.CodeSegmentIndex].Clear;
        P^.HasOverride := IsOverride;
        Exit(P);
      end;
    end;
  end;
  Self.VM.Binaries.Alloc(Self.VM.Binaries.Value^.Size + 1);
  Self.VM.Binaries.Value^.Data[Self.VM.Binaries.Value^.Size - 1] := TSEBinary.Create;
  Self.VM.Binaries.Value^.Data[Self.VM.Binaries.Value^.Size - 1].BinaryName := Name;
  FuncScriptInfo.ArgCount := ArgCount;
  FuncScriptInfo.CodeSegmentIndex := Self.VM.Binaries.Value^.Size - 1;
  FuncScriptInfo.Name := Name;
  FuncScriptInfo.VarSymbols := TStringList.Create;
  FuncScriptInfo.PossibleKinds := [sevkNumber, sevkString, sevkNull, sevkMap, sevkFunction];
  FuncScriptInfo.HasSelf := True;
  FuncScriptInfo.HasOverride := IsOverride;
  Self.FuncScriptList.Add(FuncScriptInfo);
  AIndex := Self.FuncScriptList.Count - 1;
  Result := Self.FuncScriptList.Ptr(AIndex);
  Self.FuncCurrent := AIndex;
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
    Writeln(' - Library''s pointer: ', NativeUInt(Lib));
    {$endif}
  end;

  FuncImportInfo.Args := Args;
  FuncImportInfo.Return := Return;
  FuncImportInfo.Name := Name;
  FuncImportInfo.Func := nil;
  FuncImportInfo.CallingConvention := CC;
  FuncImportInfo.PossibleKinds := [sevkNumber, sevkString, sevkNull, sevkMap, sevkFunction];
  if Lib <> 0 then
  begin
    FuncImportInfo.Func := GetProcAddress(Lib, ActualName);
  end;
  Self.FuncImportList.Add(FuncImportInfo);
end;

function TEvilC.Backup: TSECache;
var
  I, J: NativeInt;
  BackupBinary, SrcBinary: TSEBinary;
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  Result.LineOfCodeList := TSELineOfCodeList.Create;
  Result.FuncScriptList := TSEFuncScriptList.Create;
  Result.FuncImportList := TSEFuncImportList.Create;
  Result.GlobalVarSymbols := TStringList.Create;
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
  Result.GlobalVarSymbols.Assign(Self.GlobalVarSymbols);
  Result.GlobalVarCount := Self.GlobalVarCount;
end;

procedure TEvilC.Restore(const Cache: TSECache);
var
  I, J: NativeInt;
  BackupBinary, DstBinary: TSEBinary;
  FuncScriptInfo: TSEFuncScriptInfo;
begin
  Self.VM.BinaryClear;
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
  Self.GlobalVarSymbols.Assign(Cache.GlobalVarSymbols);
  Self.GlobalVarCount := Cache.GlobalVarCount;
  Self.IsParsed := True;
end;

procedure TSECacheMap.ClearSingle(const AName: String);
var
  Cache: TSECache;
  I: NativeInt;
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
    Self.Remove(AName);
  except
  end;
end;

procedure TSECacheMap.Clear;
var
  S: String;
  Cache: TSECache;
  I: NativeInt;
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
  end;
  inherited;
end;

var
  I: NativeInt;

initialization
  SEStackSize := 2048;
  SEThreadStackSize := 256;
  SEFrameSize := 1024;
  SETrapSize := 1024;
  ConstStrings := TSEStringList.Create;
  ConstStringsLookup := TSEStringLookupMap.Create;
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
    Pointer(opJumpEqual1Rel), true, Pointer(5),
    Pointer(opJumpUnconditionalRel), Pointer(6),
    Pointer(opPushLocalVar), Pointer(1), Pointer(0),
    Pointer(opThrow),
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
  ConstStringsLookup.Free;
  ConstStrings.Free;

end.

