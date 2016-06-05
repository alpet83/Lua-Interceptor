////////////////////////////////////////////////////////////////////////////
//  Module 	    : luaicp.dpr
//  Modified 	    : 16.03.2016
//  Author	    : Alexander Petrov
//  Description     : X-Ray Engine extended functionality provider
////////////////////////////////////////////////////////////////////////////

library luaicp; // LUA Capture by alpet (C) 2009
{$WARN SYMBOL_PLATFORM OFF}
{$APPTYPE CONSOLE}
{$R *.res}
{$I stkdef.inc}
{$IMAGEBASE $840000}

{$IFDEF RELEASE}
{$D-}
{$ENDIF}

uses
  FastMM4 in '..\lib\FastMM4.pas',
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  COMSupport in 'COMSupport.pas',
  XrayMM in 'XrayMM.pas',
  Vcl.Graphics,
  madMapFile,
  Windows,
  SysUtils,
  ComObj,
  ComServ,
  ActiveX,
  StrUtils,
  AnsiStrings,
  Classes,
  Math,
  ContNrs,
  IniFiles,
  ShellAPI,
  SHFolder,
  TlHelp32,
  madStackTrace,
  hkmount in 'hkmount.pas',
  WThreads in '..\lib\WThreads.pas',
  WinHeap in '..\lib\WinHeap.pas',
  FastSync in '..\lib\FastSync.pas',
  DateTimeTools in '..\lib\DateTimeTools.pas',
  StrClasses in '..\lib\StrClasses.pas',
  ArrayTypes in '..\lib\ArrayTypes.pas',
  UniArray in '..\lib\UniArray.pas',
  MemStat in '..\lib\MemStat.pas',
  Algs in '..\lib\Algs.pas',
  LuaTypes in '..\lib\LuaTypes.pas',
  ModuleMgr in '..\lib\ModuleMgr.pas',
  misc in '..\lib\misc.pas',
  XrayLua in 'XrayLua.pas',
  luahelp in 'luahelp.pas',
  MD5 in '..\lib\MD5.pas',
  PsUtils in '..\lib\PsUtils.pas',
  Debugger in '..\lib\Debugger.pas',
  XrayRegistry in 'XrayRegistry.pas',
  LCGlobals in 'LCGlobals.pas',
  BaseLib in '..\lib\BaseLib.pas',
  VxTools in 'VxTools.pas',
  texcap in 'texcap.pas',
  AccSec in '..\lib\AccSec.pas',
  IPCUtils in '..\lib\IPCUtils.pas',
  Singularity in 'Singularity.pas',
  LCDebug in 'GUI\LCDebug.pas' {DebugMonitor},
  MultiProc in 'MultiProc.pas',
  LuaTools in '..\lib\LuaTools.pas',
  FileWorks in 'FileWorks.pas',
  XrayExt in 'XrayExt.pas',
  XrayStatic in 'XrayStatic.pas',
  TablePack in 'TablePack.pas',
  FastMM4Messages in '..\lib\FastMM4Messages.pas',
  LuaWebPacket in '..\lib\LuaWebPacket.pas',
  EasyECDSA in '..\lib\EasyECDSA.pas',
  ByteCode in 'ByteCode.pas',
  XrayImports in 'XrayImports.pas',
  msvcrt in '..\lib\msvcrt.pas',
  LuaTextFile in '..\lib\LuaTextFile.pas',
  luaicp_TLB in 'luaicp_TLB.pas';

const
    KLF_SETFORPROCESS = $00000100;
    PMF_GAME_CONSOLE  = $40;

    CHCMD_FILE = '#';
    CHCMD_FUNC = '!';
    IDS_YES = 'Yep';

{$I sak_shield.inc}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

type

  THelperThread = class (TWorkerThread)
  private
    FLuaState: lua_State;
    FWorkGlobals: TStrMap;
    FScriptLines: TStrMap;

    function                  MakeState: lua_State;
    procedure                 LoadScript(L: lua_State);

  protected
   procedure                  ProcessInit; override;
   function                   ProcessRequest(const rqs: String; rqobj: TObject): Integer; override;
   procedure                  ProcessThreadStop; override;


  public

   property                   ScriptLines: TStrMap read FScriptLines;
   property                   WorkGlobals: TStrMap read FWorkGlobals;
   { methods }
  end;

  TDumperThread = class (TWorkerThread)
  protected
   function                   ProcessRequest(const rqs: String; rqobj: TObject): Integer; override;
  end;


  TSignalHandler = procedure (L: lua_State; signal: PAnsiChar); stdcall;

  TUserDLLDesc = class
  public
   hDLL: DWORD;
   lua_icpfunc: TLUAInterceptFunc;
   sig_handler: TSignalHandler;
   { C & D }
   destructor       Destroy; override;
  end;

  TThreadLuaState = record // луа-стейт, привязанный  к некоторой нити выпонения
   Lchild: lua_State;
   key: array [0..15] of CHAR;
   TID: DWORD;
  end; // TThreadLuaState

  PThreadLuaState = ^TThreadLuaState;


  TLUAStateDesc = class
  protected
     FThreadID: DWORD;
      FLParent: lua_State;
   FParentName: String;
         FName: String;

  public
   tls_list: array [0..31] of TThreadLuaState;
   old_panicf: lua_CFunction;

    f_realloc: lua_Alloc;
    p_realloc: Pointer;

   is_primary: Boolean;



   property Lparent: lua_State read FLParent;
   property ThreadID: DWORD read FThreadID;



   { C &  D}
   constructor  Create (lpt: lua_State);
   destructor   Destroy; override;
   { methods }


   function     AllocTLS (lct: lua_State; ThreadID: DWORD): PThreadLuaState;
   function     FindTLS (lct: lua_State; ThreadID: DWORD = $FFFFFFFF): PThreadLuaState;
   procedure    Clear;
   procedure    Dump;
   procedure    Register;           // прописка в луа-стейте, с триггером на сборку мусора при удалении
   function     SpawnThread: lua_State;
   procedure    Unregister;
  end; // TLuaStateDesc


  TFileScanContext = record
      count: Integer;
     rbytes: Int64;
    fs_root: String;
    op_path: String;
    cl_path: String;
  end;
  PFileScanContext = ^TFileScanContext;

const
    LSC_THREADS = 'LSC_THREADS';



var
   SaveGetExceptionObject: function(P: PExceptionRecord):Exception;
             last_ex_ptrs: _EXCEPTION_POINTERS;
              DumpContext: procedure; cdecl;


function  AddVectoredExceptionHandler(FirstHandler: DWORD; VectoredHandler: TVectoredHandler): Pointer; stdcall; external 'kernel32.dll';
function  RemoveVectoredExceptionHandler (VectoredHandler: TVectoredHandler): LongBool; stdcall; external 'kernel32.dll';

{ ==================================== forward declarations ============================================= }
function        AtPanicHandler (L: lua_State): Integer; cdecl; forward;
procedure       CheckInstallVEHFlt (uninst_last: Boolean; handler: TVectoredHandler); forward;
function        DebugDumpAll(L: lua_State = nil): Integer; cdecl; forward;
procedure       DumpMemoryStatus; forward;
function        DumpPointers ( pctx: PCONTEXT ): String; forward;
function        DumpProcessStack( hProcess: THandle; const ctx: TContext; E: Windows.PExceptionRecord ): String; forward;
function        DumpVar (L: lua_State; index: Integer; short: Boolean = TRUE; const sp: String = ''): String; forward;
function        GetPtrInfo ( p: Pointer ): String; forward;
procedure       Main; forward;
procedure       RegularTimerProc (h: HWND; uMsg, idEvent, dwTime: DWORD); stdcall; forward;
function        RegDLLStuff (L: lua_State): Integer; cdecl; forward;
procedure       XrayLogStackTrace (pExPtrs: misc.PEXCEPTION_POINTERS); cdecl; forward;


{ ======================================================================================================= }


function stlog(const msg: String): Boolean;
begin
 startup_log := startup_log + msg + #13#10;
 result := TRUE;
end;


{ ================ EXCEPTION CATCHING ===================== }
var
    g_dumper: TDumperThread = nil;
    save_esp: DWORD = 0;
    save_ebp: DWORD = 0;
    stsv_cnt: Integer = 0;
     def_ver: DWORD = 1;
      g_perf: TProfileTimer = nil;





function DumpExceptionDetails(const ExceptionInfo: misc.PEXCEPTION_POINTERS; const from: PChar): Integer; register;
var
 log_suffix: String;
      flags: DWORD;
        flt: DWORD;
        ecd: DWORD;
        psa: PAnsiChar;
        nxt: Boolean;
         eo: Exception;
         fn: String;
          s: String;
          n: Integer;

begin
  result := EXCEPTION_EXECUTE_HANDLER; // EXCEPTION_CONTINUE_EXECUTION

  if _exception_quiet > 0 then
  with ExceptionInfo.ExceptionRecord^ do
    begin
     ODS ('[~T].~C0C #EXCEPTION_SUPPRESSED:~C07 ' + FormatException (ExceptionCode, ExceptionInformation[0], ExceptionInformation[1] ) +
                       ' at EIP = ' + GetPtrInfo (Ptr(ExceptionInfo.ContextRecord.Eip)) );
     exit;
    end;

 ecd   := ExceptionInfo.ExceptionRecord.ExceptionCode;
 flags := ExceptionInfo.ExceptionRecord.ExceptionFlags;
 flt := ecd and $0FFF0000;
 _exception_flag := 200;
 MY_SEH_install := -1;

 if ( g_dumper <> nil ) and ( g_dumper.ThreadID = GetCurrentThreadID ) then exit;


 log_suffix := 'EXCEPTION_SEH';

 if Assigned (destr_log) and (destr_log.Count > 0) then
    destr_log.SaveToFile (gLogPath + 'destr_objects.log');
 // while not IsDebuggerPresent do Sleep(100);

 nxt := ( flags and 1 = 0 ) or ( ecd < NT_WARNING ); // EH_NONCONTINUABLE ?

 case ecd of

  MS_VC_EXCEPTION: exit; // SetThreadName
     EC_ASSERTION:
        with ExceptionInfo.ExceptionRecord^ do
         begin
          ODS('[~T/~U/~I].~C09 #DELPHI_EXCEPT: catched by LuaVEHandle, code: ~C0F'#13#10#9#9 +
                       FormatException (ExceptionCode, ExceptionInformation[0], ExceptionInformation[1] ) +
                       ' at EIP = ' + GetPtrInfo ( Ptr(ExceptionInfo.ContextRecord.Eip) ) +  '~C07' );

          s := '';
          for n := 0 to High ( ExceptionInformation ) do
              s := s + FormatPtr ( Ptr ( ExceptionInformation [n] ) ) + ' ';

          ODS (#9#9' #DBG: dump ExceptionInformation: ~C0F' + s + '~C07');

          eo := Ptr ( ExceptionInformation [1] );

          if not IsBadReadPtr (eo, 20) then
                 ODS(#9#9' Exception class: ' + eo.ClassName + ', message: ' + eo.Message );

          ODS ( SimpleStackTrace ( ExceptionInfo.ContextRecord ) );

          log_suffix := 'EXCEPTION_DELPHI';


          exit;
         end;
 end;


 if (flt <> $0000000) or ( ecd = $0000071A )  then
  with ExceptionInfo.ExceptionRecord^ do
   begin
    Inc (game_except_count);

    if ( ecd <> DBG_PRINTEXCEPTION_C ) then
        ODS('[~T/~U/~I].~C09 #QUIET_EXCEPT(' + IntToStr(game_except_count) + '): catched by ' + from + ' +, code: ~C0F'#13#10#9#9 +
                 FormatException (ExceptionCode, ExceptionInformation[0], ExceptionInformation[1] ) +
                 ' at EIP = ' + GetPtrInfo ( Ptr(ExceptionInfo.ContextRecord.Eip) ) +  '~C07' );
    result := EXCEPTION_EXECUTE_HANDLER;
    exit;
   end;


 with ExceptionInfo.ExceptionRecord^ do
   ProcessMsg( Format('~C0C #SEH_EVENT!~C07 CODE = $%08x, EIP=$%08x, info[$%x, $%x] : %s',
               [ecd, ExceptionInfo.ContextRecord.Eip, ExceptionInformation[0], ExceptionInformation[1],
               FormatException(ecd,  ExceptionInformation[0], ExceptionInformation[1] )]
                ));

 if g_except_count = 2 then
    collapse_time := PreciseTime + 35 * DT_ONE_SECOND;
 DumpMemoryStatus;


 if g_except_count >= 5 then
   begin
    PrintError ('To many exceptions catched in one minute! Process will be terminated!');
    Sleep (500);
    TerminateProcess ( GetCurrentProcess, ExceptionInfo.ExceptionRecord.ExceptionCode  );
   end;

 try

   ODS('<LuaVEHandler>');


  with ExceptionInfo.ExceptionRecord^ do
   begin
    ODS('[~T/~U/~I].~C0C #EXCEPT(' + from + '): Exception catched, except_count = ' + IntToStr(g_except_count) + ', code: ~C0F'#13#10#9#9 +
                 FormatException (ExceptionCode, ExceptionInformation[0], ExceptionInformation[1] ) +
                 ' at EIP = ' + GetPtrInfo ( Ptr(ExceptionInfo.ContextRecord.Eip) )  + '~C07' );
    ODS( #9#9' CheckPoints dump: ' + gCheckPoints.dump );


    if ExceptionCode and NT_ERROR = NT_ERROR then nxt := FALSE; // надо вываливать игру!


    psa := Ptr (ExceptionInfo.ContextRecord.Ecx);

    if (ExceptionCode = STATUS_BREAKPOINT) and ( DWORD(psa) > $10000 ) and ( not IsBadReadPtr (psa, 64) )  then
       begin
        {  for i := 80 downto 50 do
            PlayBeep (i * 10, 25); }

        log_suffix := 'ASSERTION';

        ODS('[~T]. #DBG: Probably assertion with message: ~C0F'#13#10 + AnsiTrim2W (psa) + '~C07');
        PlaySoundFile ( 'assert.wav', FALSE, FALSE );

        Inc ( g_except_count );
        // nxt := ( StrLen (psa) > 25 ) and ( g_except_count < 10 ) ;
        nxt := TRUE;


       end;


   end;


  if Pos( log_suffix, gLogFileName) = 0 then
     gLogFileRename [0] := AnsiReplaceStr ( gLogFilename, '.log', '+' + log_suffix +  '.log' );

  if  g_except_count >= 4 then
      CheckInstallVEHFlt ( TRUE, nil );


  ODS (' <dump_registers>');
  ODS ( DumpRegisters ( ExceptionInfo.ContextRecord^, '~C07', #9 ) );
  ODS ( DumpPointers (  ExceptionInfo.ContextRecord ) );
  ODS (' </dump_registers>');

  DebugDumpAll ();

  if Assigned (SymSetOptions) then
     SymSetOptions ( SYMOPT_LOAD_ANYTHING or SymGetOptions );

  if Assigned(DumpContext) then
     DumpContext();


  XrayLogStackTrace (ExceptionInfo);

  s := '<thead_stack>'#13#10;
  s := s + DumpProcessStack ( 0, ExceptionInfo.ContextRecord^, ExceptionInfo.ExceptionRecord );
  s := s + ' </thead_stack>';
  DeleteColorTags(s);
  fn := ExtractFilePath (gLogFilename) + 'except_stack_' + IntToStr (PostInc (stsv_cnt)) + '.xml';


  if StrClasses.PutFileContents (fn, s) then
     wprintf (' Stack trace detailed saved to %s ', [fn]);
  ODS('</LuaVEHandler>');

  while PreciseTime - Max(last_msg_time, log_flush_time) < 3 * DT_ONE_SECOND do Sleep (250);

 except
  on E: EAccessViolation do
     PrintError('AV in LuaVEHandler catched:~C0F ' + E.Message);
  on E: Exception do
     PrintError('Exception in LuaVEHandler catched:~C0F ' + E.Message);
 end;

 if ignore_assert then
  begin
   ODS('[~T]. #DBG: trying ignore assertion...');
   result := EXCEPTION_EXECUTE_HANDLER;
   exit;
  end;

 if nxt then
   begin
    ODS('[~T/~B]. #DBG: exit from LuaVEHandlerImpl with code~C0F EXCEPTION_CONTINUE_SEARCH~C07');
    result := EXCEPTION_CONTINUE_SEARCH // EXCEPTION_CONTINUE_EXECUTION
   end
 else
    result := VEHandler (ExceptionInfo);
end;


function TopLevelExceptionFilter(const ep: _EXCEPTION_POINTERS): Integer; stdcall;
begin
 result := DumpExceptionDetails(@ep, 'TopLevelExceptionFilter');

 if result = EXCEPTION_CONTINUE_SEARCH then
    result := LPTOP_LEVEL_EXCEPTION_FILTER (prvExceptionFilter) ( @ep );
end;

procedure  InstallTopLevelExceptionFilter;
var
   ef: TFNTopLevelExceptionFilter;
begin
 ef := SetUnhandledExceptionFilter ( @TopLevelExceptionFilter );
 if @prvExceptionFilter = nil then
     prvExceptionFilter := ef;
 ODS ('[~T]. #DBG: Replaced ExceptionFilter = ' + FormatPtr (@ef) );
end;

function VerifyContext (pCtx: PContext): Boolean;
begin
 result := ( DWORD(pCtx) > $10000 ) and  not IsBadReadPtr(pCtx, 716) and
           ( pCtx.SegCs = $23 ) and ( pCtx.SegDs = $2B );
end;

function HookGetExceptionObject(P: PExceptionRecord):Exception;
var
   pStack: PPtrArray absolute P;
     pCtx: PContext;
        n: Integer;
begin
  last_ex_ptrs.ExceptionRecord := Pointer(P);
  asm
    mov eax, [ebp + $104]
    mov last_ex_ptrs.ContextRecord, eax
  end;
  if not VerifyContext (last_ex_ptrs.ContextRecord) then
    for n := -1 to 4 do
     begin
      pCtx := pStack[n];
      if VerifyContext (pCtx) then
         begin
          last_ex_ptrs.ContextRecord := pCtx;
          break;
         end;
     end;

  result := SaveGetExceptionObject(P);
end;


procedure FlushLog;
begin
 {$IFNDEF NEWEST_BUILD}
 if Assigned (_stkflush) and ( not silent_mode ) then _stkflush();
 {$ENDIF}
end;

function TestIsCommand (rs: String): Boolean;
begin
 result := FALSE;
 while ( Length (rs) > 1) and CharInSet ( rs [1], ['~', '@', ' '])  do Delete (rs, 1, 1);

    if ( Pos('load ', rs) = 1 ) then
     begin
      if upd_timeout > 0 then
         ODS('[~T]. #DBG: savegame load detected - stopping timeout timer.');
      upd_timeout := 0;
      global_pt.StartOne (4);
     end;

 if ( Pos('flush', rs) = 0 ) then exit;

 result := TRUE;

 cmdlist.Add(rs);
 wprintf ('[~T]. #DBG:~C0E captured command~C0A %s~C07, in buffer~C0D %d%~C07 commands', [rs, cmdlist.Count]);
end;

var
   engine_started: Boolean = FALSE;



procedure _logCB(pmsg: PAnsiChar); cdecl;
var
   c, s: String;
     ch: CHAR;
     po: Integer;
     pc: Integer;
     ft: Text;
     fn: String;
begin
 silent_exception := NT_ERROR +5;
 try
     if pmsg = nil then exit;
     s := String (pmsg);
     s := Trim(s);


     if game_log = nil then game_log := TStrMap.Create;


     if ( Pos('~ quit', s) = 1 ) and (gToolThread <> nil) then
          gToolThread.scan_cpu := TRUE;

     if Pos('<DEBUG_CONTEXT_DUMP/>', s) > 0 then
        DebugDumpAll;


     {if game_log.TryLock ('WS', 500) then
      try
       game_log.Add ( InfoFmt(c) );
      finally
       game_log.Unlock;
      end;}

     // XrTryAlloc (16);

     if Length(s) = 0 then exit;

     ch := s[1];

     if (not engine_started) and ( Pos('Starting engine...', s) > 0 ) then
       begin
        ODS('[~T/~U].~CF0 #PERF: pre-init complete!~C07');
        fn := FmtStalkerPath('$logs$') + 'loadlevel_perf.csv';
        AssignFile(ft, fn);
        {$I-}
        if FileExists(fn) then
           Append(ft)
        else
           ReWrite(ft);
        if IOresult = 0 then
          begin
           WriteLn(ft, Format('#STARTUP: init stage time = %.1f ms', [(Now - ps_creation_time) / DT_ONE_MSEC]));
           CloseFile (ft);
          end;


        ps_creation_time := Now;
        engine_started := TRUE;
       end;


     if g_game_build > 5000 then
       begin
        po := Pos('[', s);
        pc := Pos('].', s);

        if (po >= 0) and (pc >= 10) and
            (po <= 2) and (pc <= 16 ) then
             begin
              s := Copy (s, pc + 2); // extract time
              if ch = '[' then ch := ' ';
             end;


       end;

     if (game_verbose > 4) or
        ( ( Pos('sv ', s) <> 1 )  and ( Pos ('cl ', s) <> 1 ) )  and
          ( Pos('se destr', s) = 0 ) and ( Pos('se object release', s) = 0 ) and
          ( Pos('xrServer::entity_Destroy', s) = 0 ) then
      begin
       c := Copy (s, 2);

       case ch of
        '!': c := '~C0C' + c;
    '~','$': c := '~C0E' + c;
        '-': c := '~C0A' + c;
        '#': c := '~C0B' + c;
        '*': c := '~C0F' + c;
        '^': c := '~C0D' + c;
        '&': c := '~C09' + c;
        ' ': // nothing ;
        else
          c := '~C07' + ch + c;
       end;

       c := '[~T/~U/~B].~C03 #XRAY: ' + c + '~C07';
       c := AnsiReplaceStr(c, '%c[255,128,128,128]', '~C07');
       c := AnsiReplaceStr(c, '%c[255,64,64,64]',    '~C08');
       c := AnsiReplaceStr(c, '%c[255,128,128,255]', '~C09');
       c := AnsiReplaceStr(c, '%c[255,128,255,128]', '~C0A');
       c := AnsiReplaceStr(c, '%c[255,128,255,255]', '~C0B');
       c := AnsiReplaceStr(c, '%c[255,255,128,255]', '~C0D');
       c := AnsiReplaceStr(c, '%c[255,255,128,128]', '~C0C');
       c := AnsiReplaceStr(c, '%c[255,255,255,128]', '~C0E');
       c := AnsiReplaceStr(c, '%c[255,255,255,255]', '~C0F');

       ODS(c);
       if Pos('LUA_ERROR', c) > 0 then
         begin
          PlaySoundFile ( 'chord.wav', FALSE, FALSE );
           if Pos( '+LUA_ERROR', gLogFileName) = 0 then
              gLogFileRename [0] := AnsiReplaceStr ( gLogFilename, '.log', '+LUA_ERROR.log' );
         end;
      end;

     if Pos('[x-ray]: economy: strings[', s) > 0 then
       DumpMemInfo;


     if TestIsCommand (s) then
      begin
       if parse_log then
          begin
           parse_log := FALSE;
           ODS('[~T]. #DBG: Отключен парсинг лога, поскольку работает перехват через колбек.');
          end;
      end
     else
       XrayRegistry.TestObjectEvent (s);


     if ( Pos('stack trace:', s) > 0 ) then
          PlayBeep (1000, 50);
     if Assigned (orig_logCB) then
        orig_logCB (pmsg);
 except
   on E: Exception do
       silent_exception := 0;
 end;
 silent_exception := 0;
end;

procedure SetLogCallback;
{$IFNDEF SOC}
var
   prv: TLogCallback;
{$ENDIF}
begin
 if inst_logCB and Assigned(_SetLogCB) then
  begin
   {$IFDEF SOC}
    _SetLogCB (_logCB);
    ODS('[~T]. #DBG: _SetLogCB executed...');
   {$ELSE}
    prv := _SetLogCB (_logCB);
    if Addr (prv) <> Addr(_logCB) then
       orig_logCB := prv;
    wprintf('[~T]. #DBG: _SetLogCB executed. Original callback = $%p ', [Addr(prv)]);
  {$ENDIF}


  end;
end;


function  CmpPtrObj (iTest: Integer; pData, pValue: Pointer): Integer;
type
  TPtrInfoList = array [0..1048575] of TPtrInfo;
  PPtrInfoList = ^TPtrInfoList;

var
   l: PPtrInfoList;
   p, ex: TPtrInfo;
begin
 l := pData;
 p := l[iTest];
 ex := pValue;

 result := 0;
 if ( DWORD (p.PK) > DWORD (ex.PK) ) then result := +1;
 if ( DWORD (p.PK) < DWORD (ex.PK) ) then result := -1;
end;

function CmpPtrInfos (a, b: Pointer): Integer;
var
   ap: TPtrInfo;
   bp: TPtrInfo;

begin
 ap := a;
 bp := b;
 result := 0;
 if ( DWORD (ap.PK) > DWORD (bp.PK) ) then result := +1;
 if ( DWORD (ap.PK) < DWORD (bp.PK) ) then result := -1;
end;


procedure BT_UninstallSehFilter;
var
   h: DWORD;
begin
 // stdcall; external 'bugtrap.dll';
 if not Assigned (_btfltu) then
  begin
   h := GetModuleHandle ('bugtrap.dll');
   if h <> 0 then
    _btfltu := GetProcAddress (h, 'BT_UninstallSehFilter');
  end;
 if Assigned (_btfltu) then _btfltu();
end;

procedure StalkerODS (s: String; flags: DWORD);
begin
 if in_ODS then exit;
 in_ODS := TRUE;
 try
  s := AnsiReplaceStr (s, '~CRLF', #13#10);
  s := AnsiReplaceStr (s, '~CR', #13);
  if (Pos('$#CONTEXT',s) + Pos('$#DUMP_CONTEXT', s) = 0) then
      ODS(PChar(s), flags);

  if Assigned (_stklog) and ( flags and PMF_GAME_CONSOLE <> 0 ) and ( game_con_log ) and ( not g_Config.silent_mode )  then
   begin
    _conmsgs.Lock ('addmsg');
    try
     s := RemoveColorTags ( InfoFmt (s) );
     _conmsgs.Add (s);
    finally
     _conmsgs.Unlock;
    end;
   end;
 finally
  in_ODS := FALSE;
 end; // try
end;


function  DumpMMStat(L: lua_State): Integer; cdecl;
begin
 result := 0;
 {ODS( CFormat('[~T]. #DBG(MM): allocated = %.3f MiB in %d calls, fake_allocated = %.3f MiB, freed = %.3f MiB, diff = %.3f MiB', '~C07',
            [ alloc_sum / MiB, alloc_cnt, afake_sum / MiB,
              freed_sum / MiB, (alloc_sum - freed_sum) / MiB ]));}
end;

function DumpConsoleBuff (L: lua_State = nil): Integer; cdecl;
var
   i: Integer;
   msg: String;
begin
 result := 0;

 if (_conmsgs.Count > 0) and Assigned (_stklog) then
  begin
   if _conmsgs.TryLock ('dump-msgs') then
   try
    for i := 0 to _conmsgs.Count - 1 do
     begin
      msg := _conmsgs [i];
      _stklog ( AnsiArg (msg) );
     end;
    _conmsgs.Clear;
    _stklw := TRUE;
   finally
    _conmsgs.Unlock;
   end;
  end; // if

end;

procedure BroadcastSignal (L: lua_State; const sig: String);
var
   udd: TUserDLLDesc;
    sa: AnsiString;
     n: Integer;
begin
 sa := AnsiString (sig);
 for n := 0 to usr_dlls.Count - 1 do
  begin
   udd := TUserDLLDesc ( usr_dlls.Objects [n] );
   if ( udd = nil ) or ( @udd.sig_handler = nil ) then continue;
   udd.sig_handler ( L, PAnsiChar (sa) );
  end;

end;


function FormatRight (const s, addl: String): String;
begin
 result := addl + AnsiReplaceStr (s, #10, #10 + addl);
end;

function GetDbgVarsDump(in_func_e: PChar = nil): String;
var
   v, s: String;
      i: Integer;

begin
  result := 'dbg_vars = nil';
  if not Assigned (dbg_vars) then exit;
  v := '';
  for i := 0 to dbg_vars.Count - 1 do
    begin
     s := '~C0E' + dbg_vars.Names[i] + '~C0B=~C0F' + dbg_vars.ValueFromIndex[i] + '~C07'#13#10;
     v := v + s;
    end;
  v := ReplaceChar (v, '§', #13);
  v := ReplaceChar (v, '¶', #10);
  s := '~C0F<dbg_vars>~C0E'#13#10 + FormatRight(v, #9'  ') + #13#10;

  s := UnhideSP (s);

  s := AnsiReplaceStr (s, '{-*', '~C0B{~C0E');
  s := AnsiReplaceStr (s, '*-}', '~C0B }~C0A');
  s := AnsiReplaceStr (s, 'stack traceback:', ' ~C0Fcall stack:~C0A');
  s := s + '~C0F</dbg_vars>'#13#10;
  s := s + '~C0F<global_vars>~C09'#13#10 + FormatRight (global_vars.Text, '  ') + #13#10'~C0F</global_vars>~C07'#13#10;
  if in_func_e <> nil then
     s := s + CFormat ('~C0F in_func_e = %s ', '~C07', [String(in_func_e)]);
  s := s + CFormat ('~C0F in_func_r = %s, in_func = %s ', '~C07', [String(in_func_r), String(in_func)]);
  s := s + #13#10'!-------------------------------------------------------------------------------------!'#13#10'';
  result := s;
end;

function FormatLuaStack (L: lua_State; const desc: String): String;
var
   stack, trace: String;
begin
 stack := 'L = ' + lvm_name(L);
 if (0 = Pos('~', stack)) and (DWORD(L) > $1000) then
 try
  trace := LuaTraceBack (L, #9);
  stack := CFormat('~C0E traceback for ' + desc + ' L = %s: \n %s~C07', '~C0E', [LVM_name(L), trace])  + #13#10;        // Вероятно вложенный стек вызовов будет глубже
  stack := stack + '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'#13#10;
 except
  on E: Exception do
     stack := stack + #13#19'~C0C Exception catched in FormatLuaStack: ~C0F' + E.Message + '~C07, L =  ' + LVM_name(L) + #13#10;
 end; // try-excpt
 result := stack;
end; // FormatLuaStack

function lua_getstr(L: lua_State; const name: String): String;
var
   t: Integer;
begin
 result := '';
 t := lua_gettop(L);
 lua_getglobal (L, name);
 if lua_type(L, -1) = LUA_TSTRING then
    result := LuaStrArg (L, -1);
 lua_settop (L, t);
end;



function DebugDumpAll(L: lua_State = nil): Integer; cdecl;
var
   r, s, stack: String;
         flags: Integer;
begin
  result := 0;
  flags := 0;
  if (active_L = nil) then
      active_L := L;

  r := '~C0F<dbg_dump>~C0B'#13#10#9 +
       InfoFmt ('<context time="~T" uptime="~U" thread="~i"> ') +
       CFormat ('active_L = $%p, aux_L = $%p, pcall_L = $%p, game_L = $%p</context>'#13#10, '~C07', [active_L, aux_L, pcall_L, game_L] );
  s := GetDbgVarsDump;
  stack := '<lua_stacks>'#13#10;
  try
    if (L <> active_L) and (L <> nil) then
        stack := stack + FormatLuaStack (active_L, 'current' );

    if (active_L <> nil) then
        stack := stack + FormatLuaStack (active_L, 'active' )
     else
        stack := stack + ' active_L = nil '#13#10;

    if (aux_L <> nil) then
        stack := stack + FormatLuaStack (aux_L, 'aux' )
     else
        stack := stack + ' aux_L = nil '#13#10;

    if (game_L <> nil) then
        stack := stack + FormatLuaStack (game_L, 'game' )
     else
        stack := stack + ' game_L = nil '#13#10;
  except
   on E: Exception do
      stack := 'Exception catched while building stacks: ' + E.Message;
  end;



  stack := stack + '</lua_stacks>'#13#10;
  r := r + FormatRight (stack, #9'  ') + '~C07';
  r := r + #9'-------------------------------------------------------------------------------'#13#10;
  r := r + #9'~C09'#13#10 + FormatRight ( s, #9'  ') + '~C07'#13#10;
  r := r + #9'<trace_log>'#13#10;
  r := r + FormatRight (trace_log.Text, #9'  ') + #13#10;
  r := r + #9'</trace_log>'#13#10;

  r := r + '~C0F</dbg_dump>~C07'#13#10;


  if XrayRegistry.RootRegistry <> nil then
     XrayRegistry.RootRegistry.DebugDump;

  if (L <> nil) and (lua_gettop(L) > 0) then
     flags := lua_tointeger (L, 1);

  if flags and 1 = 0 then
     StalkerODS (r, $7 ); // no blocked print
  if flags and 2 <> 0 then // never possible if L = nil
    begin
     result := 1;
     lua_pushwstr (L, r);
    end;
end; // DebugDumpAll

function DumpPointers ( pctx: PCONTEXT ): String;
const
   REG_NAMES: array [0..7] of String = ( 'EDI', 'ESI', 'EBX', 'EDX', 'ECX', 'EAX', 'EBP', 'EIP' );
var
   pl: PPointerArray;


    n: Integer;
begin
 pl := @pctx.Edi;
 result := '';
 for n := 0 to High (REG_NAMES) do
     result := result + #9 + REG_NAMES[n] + ' = ' + GetPtrInfo ( pl^ [n] ) + #13#10;

 result := result + #9 + 'ESP = ' + GetPtrInfo ( Ptr (pctx.Esp)  ) + #13#10;
end;


function PtrToModPtr(p: Pointer): String;
var
   me: TModuleEntry32;


begin
 result := Format('$%p', [p]);

 if gToolThread = nil then exit;
 me := gToolThread.ModuleByPtr (p);
 if me.dwSize = 0 then exit;
 result := Trim (me.szModule) + '+' + Format ('$%x', [DWORD(p) - DWORD (me.modBaseAddr)]);
end;


function StringAtPtr(p: Pointer): String;
var
    mbi: TMemoryBasicInformation;
    src: CHAR;
    tag: String;
     ps: PAnsiChar;
     pw: PWideChar;
     sq: Boolean;
     uc: Boolean;
      n,
     mx,
      c: Integer;

     function AddChar ( var dst: String ): Boolean;

     begin
      result := TRUE;
      sq := ( n > 0 );

      case src of
        #00: result := ( c < 10 );
        #10: dst := dst + '\n';
        #13: dst := dst + '\r';
        #09, #$20..#$B6:
             dst := dst + src;
        else
           begin
            dst := dst + '.';
            sq := FALSE;
            c := 0;
           end;
       end; // case
      if sq then
         Inc (c); // sequence counter
      mx := Max (mx, c);
     end;



begin
 mx := 0;
 uc := FALSE;
 result := '';
 try
  if (NativeInt(p) < $10000) or (not Assigned(p)) then exit;

  FillChar (mbi, sizeof(mbi), 0);

  if 0 = VirtualQuery (p, mbi, sizeof(mbi)) then exit;


  //  result := Format('~C0F BA = $%p, S = $%x, P = $%x ~C0E', [mbi.BaseAddress, mbi.State, mbi.Protect]);

  {ODS(result);
  Sleep(150);}

  if ( mbi.BaseAddress = nil ) or ( mbi.State and MEM_COMMIT = 0 ) or ( mbi.Protect and 1 <> 0 ) then exit;
  if ( mbi.Protect and $66 = 0 ) then exit; // ro / rw check


  ps := p;
  sq := FALSE;

  c := 0;
  for n := 0 to 31 do
   if ps [n * 2 + 1] = #0 then
      Inc (c)
   else
      break;
  uc := ( c > 10 );


  c := 0;
  mx := 0;
  pw := Pointer (ps);

  for n := 0 to 127 do
   begin
    if uc then
       src := pw [n]
    else
       src := Char ( ps [n] );


    if not AddChar ( result ) then break;
   end;

  repeat
   n := Pos('...', result);
   if n > 0 then
      Delete (result, n, 2);
  until n = 0;


 except
  on E: Exception do result := '+Exception';
 end;

 tag := IfV ( uc, 'wide_str', 'ansi_str' );

 if uc then
    result := RemoveColorTags ( result );

 if mx < 10 then
   result := ''
 else
   result := '<' + tag + '>' + result + '</' + tag + '>~C07';
end;

function  DumpProcessStack( hProcess: THandle; const ctx: TContext; E: Windows.PExceptionRecord ): String;
var
           rd: NativeUInt;
        sz, n: DWORD;
   nmax, nmin: Integer;
     top, btm: DWORD;
        stack: array [0..63] of PPointer;
        s, cs: String;
         pinf: String;

  function FmtStackSlotInfo ( va: DWORD ): String;
  var
     nptr: DWORD;
   begin
    nptr := va + n * sizeof(Pointer);

    result := Format ( '~C08    <line n="%2d">~EOL', [n] ) + '    ~C0B <addr>';
    result := result + IfV( nptr = va, '~CA0', '~C0A') + '$' + IntToHex(nptr, 6) + '~C0B</addr>';

    if DWORD ( stack [n] ) > $10000 then
      begin
        cs := FormatPtr (stack[n]);
        result := result + '~EOL     <val>~C0F' + cs;
        pinf := GetPtrInfo( stack[n] );
        if pinf <> cs then
           result := result + Format ( ' = %-30s', [pinf] );
      end
    else
      result := result + '<val>~C0F' + IntToStr ( DWORD ( stack [n] ) );


    result := result + '~C0B</val>';

    if ( DWORD (stack[n]) > $F0000 ) and
         TestRegionProtect( stack[n], 16, 'r' ) and ( not IsBadReadPtr ( stack [n], 4 ) ) then
      begin
       result := result + '~EOL     <target>~C0E' + GetPtrInfo ( stack[n]^ )  + '~C0B</target>';
       cs := StringAtPtr( stack [n] );
       if cs <> '' then
          result := result + #13#10#9'    ~C09' + cs;
      end;


    result := result + '~EOL   ~C08</line>~C07~EOL';
   end;

begin
  if hProcess = 0 then
     hProcess := GetCurrentProcess;

  sz := sizeof(stack);
  nmin := Low (stack);


  FillChar (stack, sz, 0);
  with ctx do
  if esp < hInstance then
   begin
    // все данные выше ESP, являются забитыми в стек.
    top := GetStackTop;
    btm := GetStackBottom;

    s := '';


    if ( top + btm > 0 ) then
       s := Format ( '  <thread_stack bottom="$%x" top="$%x" />'#13#10, [btm, top] );


    nmax := High (stack);
    if ( top > 0 ) and ( esp < top ) then
         nmax := Min ( nmax, top - esp );


    if ReadProcessMemory ( hProcess, Ptr ( esp ), @stack, (nmax + 1) * 4, rd) and (rd > 0) then
       begin
        s := s + Format( '~C0B  <stack_dump from="esp" to="esp + $%x"> ~C0A'#13#10, [nmax * sizeof(Pointer)] );
        for n := nmin to nmax do
          if stack [n] <> nil then
             s := s + FmtStackSlotInfo (ctx.esp);
        s := s + '~C0B  </stack_dump>~C07'#13#10;
       end
    else
       s := #9'~C0C ReadProcessMemory fails:~C0F ' + err2Str (GetLastError) + '~C07'#13#10;


    nmax := High (stack);
    if ( top > 0 ) and ( ebp < top ) then
         nmax := Min ( nmax, top - ebp );

    FillChar ( stack, sizeof(stack), 0 );

    if ( ebp < top ) and ReadProcessMemory ( hProcess, Ptr ( ebp ), @stack, (nmax + 1) * 4, rd) and (rd > 0) then
       begin
        s := s + Format( '~C0B  <stack_dump from="ebp" to="ebp + $%x"> ~C0A'#13#10, [nmax * sizeof(Pointer)] );
        for n := nmin to nmax do
        if stack [n] <> nil then
           s := s + FmtStackSlotInfo (ctx.Ebp);

        s := s + '~C0B  </stack_dump>~C07'#13#10;
       end
    else
       s := s + #13#10#9'~C0C ReadProcessMemory fails:~C0F ' + err2Str (GetLastError) + '~C07'#13#10;



    if ( hProcess = GetCurrentProcess ) and Assigned (CaptureStackBackTrace) then
     try
      FillChar ( stack, sizeof(stack), 0 );

      sz := CaptureStackBackTrace (2, High(stack), @stack );
      if sz > 0 then
       begin
        s := s + '~C0B  <stack_dump from="CaptureStackBackTrace"> ~C0A'#13#10;
        for n := 0 to sz - 1 do
          s := s + FmtStackSlotInfo ( DWORD( stack [0] ) );
        s := s + '~C0B  </stack_dump>~C07'#13#10;
       end;

     except
      on E: Exception do
         s := s + ' inner exception: ' + E.Message;

     end;

  end;

 s := AnsiReplaceStr (s, '~EOL', #13#10);
 result := s + '~C07';
end; // DumpProcessStack

procedure DumpMemoryStatus;
var
   mst: TMemoryStatusEx;
begin
 mst := GetMemoryStatusEx;

 try
  CalcTexMemUsage;
 except
  on E: Exception do;
 end;

 ODS( CFormat('[~T]. #DBG(GlobalMemoryStatusEx in GiB): total_ph = %.3f, free_ph = %.3f, total_vm = %.3f, free_vm = %.3f, free_exvm = %.3f, textures = %.3f ', '~C07',
                        [mst.ullTotalPhys / GiB, mst.ullAvailPhys / GiB,
                         mst.ullTotalVirtual / GiB, mst.ullAvailVirtual / GiB,
                         mst.ullAvailExtendedVirtual / GiB,
                         gTexMemUsage / GiB])  );
end;


procedure CheckInstallVEHFlt (uninst_last: Boolean; handler: TVectoredHandler);
begin
 if uninst_last and (SEH_pointer <> nil) then
   begin
    if RemoveVectoredExceptionHandler ( SEH_pointer ) then
       ODS('~C0F[~T/~I]. #DBG: Обработчик VEH LuaVEHandler отключен.~C07')
    else
       PrintError('Function RemoveVectoredExceptionHandler failed.');

    SEH_pointer := nil;
   end;

 if MY_SEH_install >= 0 then
   begin
    if Assigned (SEH_pointer) then exit;

    SEH_pointer := AddVectoredExceptionHandler(MY_SEH_install, Handler);
    if Assigned (SEH_pointer)  then
      begin
       if not uninst_last then
          ODS('[~T/~I]. #DBG: Vectored exception handler "LuaVEHandler" added ');
      end
    else
       PrintError('Function AddVectoredExceptionHandler failed.');

   end
  else
   if not uninst_last then
     ODS('~C0F[~T]. #DBG: Используется оригинальный обработчик VEH.~C07');
end;


procedure XrayLogStackTrace (pExPtrs: misc.PEXCEPTION_POINTERS); cdecl;
var
   x_msg: PAnsiChar;
   x_esp: NativeUInt;
   x_ebp: NativeUInt;
    lCtx: TContext;
begin
 x_esp := 0;
 x_ebp := 0;
 if not Assigned(pExPtrs.ContextRecord) then exit;
 with pExPtrs^ do
 if Assigned (LogStackTraceEx)  then
     try
      wprintf (' trying LogStackTraceEx...  context size = %d ',
                                [sizeof(TContext)]);
      CopyMemory(@lCtx, ContextRecord, sizeof(TContext));
      pExPtrs.ContextRecord := @lCtx;
      // save_ctx.ContextFlags := CONTEXT_FULL;
      LogStackTraceEx (pExPtrs); // modifed EIP while stack walk
     except
      on E: Exception do
         PrintError('inner exception catched');
     end
  else
    try
      x_esp := ContextRecord.esp;
      x_ebp := ContextRecord.ebp;
      x_msg := 'Exception stack trace:';
      ODS(' trying LogStackTrace... ');
      asm
       push  ebx
       push  edx
       mov   ebx, x_msg
       mov   save_ebp, ebp
       mov   save_esp, esp
       mov   eax, x_esp
       mov   edx, x_ebp
       cmp   eax, 100h
       jbe   @not_upd_stack
       mov   esp, eax
       mov   ebp, edx
@not_upd_stack:
       mov   eax, [LogStackTrace]
       push  ebx
       call  eax
       mov   esp, save_esp
       mov   ebp, save_ebp
       pop   edx
       pop   ebx
      end;     // }
    except
     on E: Exception do
        PrintError('inner exception catched');
    end;
end;


function LuaVEHandlerImpl (ExceptionInfo: misc.PEXCEPTION_POINTERS): DWORD; cdecl;

var
   save_ctx: PContext;
   save_eps: misc.PEXCEPTION_POINTERS;

begin
 ve_exception := ExceptionInfo;
 save_ctx := AllocMem (sizeof(TContext));
 CopyMemory (save_ctx, ExceptionInfo.ContextRecord, sizeof(TContext));
 save_eps := AllocMem (sizeof(EXCEPTION_POINTERS));
 CopyMemory (save_eps, ExceptionInfo, sizeof(EXCEPTION_POINTERS));
 save_eps.ContextRecord := save_ctx; // оригинальные данные затирает винда?

 result := DumpExceptionDetails (save_eps, 'LuaVEHandlerImpl');
end;

function LuaVEHandler (ExceptionInfo: misc.PEXCEPTION_POINTERS): DWORD; stdcall;
const
  TM_FMT = 'dd mmm yyyy hh:nn:ss.zzz';
var
   mm: Integer;
   ef: String;
   sl: TStrings;
begin

 result := EXCEPTION_CONTINUE_SEARCH;
 if ( ExceptionInfo.ExceptionRecord.ExceptionFlags and 2 <> 0 ) then exit; // EH_UNWINDING

 CopyMemory (@last_ex_ptrs, ExceptionInfo, sizeof(EXCEPTION_POINTERS));

 result := EXCEPTION_EXECUTE_HANDLER;
 if ( g_in_veh > 0 ) or ( ExceptionInfo.ExceptionRecord.ExceptionCode = silent_exception ) then exit;
 mm := inside_mm;
 if mm <> 0 then
   begin
    OutputDebugString('Exception catched with inside_mm <> 0');
    exit;
   end;


 InterlockedIncrement (g_in_veh);
 InterlockedIncrement (in_except);
 try
  ef := gLogPath + '\last_exceptions.log';

  sl := TStringList.Create;
  if n_except > 0 then
     sl.LoadFromFile ( ef );

  Inc (n_except);

  if ExceptionInfo.ExceptionRecord <> nil then
    with ExceptionInfo.ExceptionRecord^ do
       sl.Add ( Format('%d;%s;$%x;$%p', [ n_except, FormatDateTime(TM_FMT, PreciseTime), ExceptionCode, ExceptionAddress] ) )
  else
       sl.Add ( IntToStr(n_except) + ';' + FormatDateTime(TM_FMT, PreciseTime) + ';madness');

  sl.SaveToFile ( ef );
  sl.Free;

  result := LuaVEHandlerImpl (ExceptionInfo);
  //  PutFileContents ( gLogPath + '\_in_veh.log', 'false');
 finally
  InterlockedDecrement (g_in_veh);
  InterlockedDecrement (in_except);
 end;

end;




function LuaSetBreakpoint(L: lua_State): Integer; cdecl;
var
     ctx: TContext;
   flags: String;
   onWrite, OnRead: Boolean;
   n, addr, bpdesc, mask_l, mask_h, xmask, dsize: DWORD;


   shift_L: BYTE;
   shift_H: BYTE;
         h: THandle;
begin
 result := 0;
 if lua_gettop (L) < 3 then exit;

 n := lua_tointeger(L, 1);
 addr := LuaDWORD (L, 2);

 flags := LuaStrArg (L, 3);

 if not (n in [1..4]) then exit;

 h := h_main_thread;
 shift_l := (n - 1) * 2;           // смещение маски младшего слова
 shift_h := (n - 1) * 4 + 16;      // смещение маски старшего слова


 onWrite := ( Pos ('w', flags) > 0 );
 onRead  := ( Pos ('r', flags) > 0 );
 // atPtr   := ( Pos ('e', flags) > 0 );


 dsize := 3;
 bpdesc := (dsize and 3) shl 2;           // сдвинуть на 2 бита = LENW
 if onWrite then bpdesc := bpdesc or 1;   // wo
 if onRead  then bpdesc := bpdesc or 3;   // r/w

 mask_l := (1 shl shift_l) or $100;    // разрешение локальной ловушки, с несъёмным значением
 mask_h := bpdesc shl shift_h; // старшее слово DR7s



 xmask := (3 shl shift_l) or (15 shl shift_h); // стирающая маска

 ctx.ContextFlags := CONTEXT_DEBUG_REGISTERS;
 if GetThreadContext (h, ctx) then
  begin
   case n of
    1: ctx.DR0 := addr;
    2: ctx.DR1 := addr;
    3: ctx.DR2 := addr;
    4: ctx.DR3 := addr;
   end;
   ctx.DR7 := (ctx.DR7 or xmask) xor xmask; // стереть биты соответствующие этому бряку
   if addr <> 0 then
      ctx.DR7 :=  ctx.DR7 or mask_l or mask_h;

   ctx.ContextFlags := CONTEXT_DEBUG_REGISTERS;

   if SetThreadContext (h, ctx) then
     begin
      ODS('[~T]. #DBG: Breakpoint #' + IntToStr(n) +
           '~C0D successfully~C07 installed for ptr = ~C0D $' + IntToHex(addr, 4) +
           '~C07 DR7 =~C0D $' + IntToHex (ctx.Dr7, 8) + '~C07');
      {if PInteger (addr)^ = 0 then
        asm
         nop
        end;}
     end
   else
      PrintError ('SetThreadContext failed in LuaSetBreakpoint: ' + err2str (GetLastError));
  end;
 // ResumeThread (h);
end; // LuaSetBreakpoint

function LuaSetDirectory(L: lua_State): Integer; cdecl;
var
   dir: String;
begin
 dir := LuaStrArg(L);
 result := 1;
 if DirectoryExists (dir) and FileCheck(dir) then
    lua_pushboolean (L, SetCurrentDirectory (PChar(dir)) )
 else
    lua_pushboolean (L, FALSE);
end;

procedure PlaySnd (const fname: String; async: Boolean = TRUE);
begin
 PlaySoundFile ( FmtStalkerPath (fname), FALSE, async );
end;

function UnhideText (const s: String): String;
begin
 result := UnhideSP (s);
 result := ReplaceChar (result, '§', #13);
 result := ReplaceChar (result, '¶', #10);
end;


procedure OnFatalError;
begin
 Inc (fatal_err_cnt);
 if fatal_err_cnt > 10 then
    begin
     PrintError ('Превышен лимит фатальных ошибок. Процесс будет аварийно завершен!');
     PlayBeep (500, 500);
     TerminateProcess ( GetCurrentProcess, $8030 );
    end;
end;

// function  luaL_dostring(L: lua_State; szStr: PAnsiChar): lua_Result; external LUA_DLL;


function DMAlloc(ud, p: Pointer; osize, nsize: SIZE_T): Pointer; cdecl;
var
   fma: TFastMemAllocator;
   ok: Boolean;
begin
 result := nil;

 if ud <> nil then
  begin
   fma := ud;
   ok := FALSE;
   // если предыдущий размер больше, но неболее чем на 256 байт - оптимизировать реаллок
   if (osize > 0) and (nsize > 0) then
      begin
       result := fma.ReAllocMem (p, osize, nsize);
       exit; // fast-realloc )
      end;

   if nsize > 0 then result := fma.AllocMem(nsize);

   if (osize > 0) and (p <> nil) then
    begin
     // if (result <> nil) then Move(p^, result^, Min (nsize, osize) );
     ok := fma.FreeMem(p, osize);
    end;

   if ( result <> nil ) then exit;

   if not ok and Assigned (orig_hf) then
      orig_hf (orig_ud, p, osize, nsize);

   exit;
  end;


 if nsize = 0 then
    FreeMem (p)
 else
  begin
   // WriteLn ('#DBG: DMAlloc nsize = ' + IntToStr(nsize));
   if p = nil then
    begin
     p := AllocMem ( nsize );
     // FillChar (p^, nsize, $00);
     // ba [0] := $12BADBAD;
    end
   else
     ReAllocMem (p, nsize);
   result := p;
  end;
end; // DMAlloc


function ReQuote(s: String): AnsiString;
var n: Integer;
begin
 result := AnsiString(s);
 for n := 1 to Length(result) do
   if result[n] = '"' then result [n] := #$27;
end;

function FindStateDesc ( lpt: lua_State; bAdd: Boolean): TLuaStateDesc;
var
   n: Integer;
   rc: TLuaStateDesc;
   lck: Boolean;
begin
 result := nil;
 lck := captured.TryLock('FindStateDesc');
 try
  for n := 0 to captured.Count - 1 do
   begin
    rc := TLuaStateDesc ( captured.Objects [n] );
    if rc = nil then continue;
    if rc.Lparent <> lpt then continue;
    result := rc;
    exit;
   end;

  if bAdd  then
    begin
     result := TLuaStateDesc.Create (lpt);
     Assert (captured.Count < 100, 'To many captured lua_State pointers');
    end;

 finally
  if lck then
     captured.Unlock;
 end;
end; // FindStateDesc

procedure RemoveStateDesc ( lsd: TLuaStateDesc );
var
   i: Integer;
begin
 captured.Lock('Remove');
 try
  i := captured.IndexOfObject (lsd);
  if i >= 0 then
     captured.Objects [i] := nil;
  lsd.Free;
 finally
  captured.Unlock;
 end;
end;

procedure SetGameLuaState(L: lua_State);
var
   lsd: TLuaStateDesc;
begin
 if game_L = L then exit;
 game_L := L;
 if L = nil then exit;

 // ODS('[~T]. #DBG: game_L = ' + FormatPtr(L));

 lsd := FindStateDesc (L, FALSE);
 if lsd <> nil then
   begin
    orig_hf := lsd.f_realloc;
    orig_ud := lsd.p_realloc;
    lsd.is_primary := TRUE;
   end;

end;


function ParseFSGame: TStrMap;



var
   sltmp, slrow: TStrMap;
   s, pk, k, v: String;
   n: Integer;
begin
 result := TStrMap.Create();
 result.AutoHideSP := FALSE;
 result.Add ( '$fs_root$=' + UnhideSP (game_root) );

 s := game_root + 'fsgame.ltx';
 if not FileExists (s) then
   begin
    PrintError('ParseFSGame - not found file ~C0F' + s);
    exit;
   end;
 AllowReadWrite(s);

 sltmp := TStrMap.Create();
 slrow := TStrMap.Create();
 try
  sltmp.LoadFromFile(s);
  if sltmp.Count = 0 then exit;

  stlog('[~T]. #DBG: fsgame.ltx raw dump:');

  for n := 0 to sltmp.Count - 1 do
   begin
    s := UnhideSP ( sltmp [n] );
    s := AnsiReplaceStr (s, #9, ' ');
    s := AnsiReplaceStr (s, '=', '|');
    s := AnsiReplaceStr (s, ',', '|');
    repeat
     k := s;
     s := AnsiReplaceStr (s, '$ ', '$');
     s := AnsiReplaceStr (s, ' $', '$');
     s := AnsiReplaceStr (s, '| ', '|');
     s := AnsiReplaceStr (s, ' |', '|');
     s := AnsiReplaceStr (s, '=', '|');
    until (k = s);
    // $game_data$-0   		  = false-1, true-2,  	gamedata-3\
    slrow.Split('|', s);
    slrow.Delimiter := #9;
    if slrow.Count <= 3 then continue;
    pk := Trim (slrow [0]);             // код пути
    // значения 1 и 2 игнорируются

    stlog(#9 + slrow.DelimitedText);

    k := slrow [3];                     // root
    v := k;                             // no-parametrized
    if Pos('$', k) > 0 then
       v := AddSlash (result.Values [k])
    else
    if Pos(':', k) = 0 then
       v := AddSlash (result.Values['$fs_root$']) + Trim(v);

    if slrow.Count >= 5 then
       v := v + Trim (slrow [4]);


    if Pos('$app_data_root$', s) > 0 then
       app_data_root := v;

    if result.IndexOfName (pk) < 0 then
       result.Add( pk + '=' + UnhideSP (v) )
    else
       result.Values [pk] := UnhideSP (v);
   end;




 finally
  sltmp.Free;
  slrow.Free;
 end;
end; // ParseFSGame

var
   last_path_check: DWORD;

function FindLastXrayLog: String;
var
   s: String;
   st: TSystemTime;
   srec: TSearchRec;
   fdt, flast: TDateTime;
   fpath: array [0..261] of CHAR;


begin
 result := '';


 if xray_log_path = '' then
   begin
    SHGetFolderPath (0, CSIDL_COMMON_DOCUMENTS, DWORD(-1), SHGFP_TYPE_DEFAULT, fpath);
    s := Trim (fpath);
    if s = '' then exit;
    s := AddSlash (s) + 'STALKER-SHOC\logs\';
    xray_log_path := s;
   end;

 flast := 0;
 ODS('[~T]. #DBG: Поиск файла в папке~C0A ' + xray_log_path + '~C07' );

 if FindFirst (xray_log_path + 'xray*.log', faAnyFile, srec) = 0 then
   repeat
    FileTimeToSystemTime ( srec.FindData.ftLastWriteTime, st);
    fdt := SystemTimeToDateTime (st);
    if fdt > flast then
     begin
      flast := fdt;
      result := xray_log_path + srec.Name; // last log file
     end;
   until FindNext (srec) <> 0;
 FindClose (srec);
 if result <> '' then
    ODS('[~T]. #DBG: Результат поиска log-файла: ~C0A' + result + '~C07')
 else
   begin
    if GetTickCount < last_path_check + 10000 then exit;

    last_path_check := GetTickCount;
    ODS ('[~T].~C0C #WARN: Не найден файл *xray.log, путь поиска:~C0F ' + xray_log_path + '~C07');
    Inc (log_errors);
   end;
end; // FindLastXrayLog


procedure CheckCommandsInLog;
var
   hFile: THandle;
   fsize, rb, i: Integer;
   rbc: DWORD;
   buff: array of AnsiChar;
   s: String;
   rs: String;
   tmpl: TStrMap;
begin
 if xray_log_file = '' then
   begin
    xray_log_file := FindLastXrayLog;
    ODS('[~T]. #DBG: Ожидаемый источник консольных команд ~C0F' + xray_log_file + '~C07');
   end;
 if (xray_log_file = '') or (not FileExists (xray_log_file)) then exit;

 hFile := CreateFile ( PChar (xray_log_file), GENERIC_READ, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
 if hFile = INVALID_HANDLE_VALUE then exit;
 tmpl := nil;
 try
  // Считывание остатка файла в буфер
  fsize := GetFileSize (hFile, nil);
  if fsize <= xray_log_lsize then exit;
  SetFilePointer (hFile, xray_log_lsize, nil, FILE_BEGIN);
  rb := fsize - xray_log_lsize;
  SetLength (buff, rb + 1);
  FillChar (buff[0], rb + 1, 0);
  ReadFile (hFile, buff[0], rb, rbc, nil);

  xray_log_tail := xray_log_tail + String ( PAnsiChar (buff) );

  // поиск окончания последней строки -- S1...#13#10S2..#13#10S3...#13#10AB
  rb := Length (xray_log_tail);
  i := rb;
  while (i > 1) and (xray_log_tail [i] <> #10) do Dec (i);

  s := Copy (xray_log_tail, 1, i);      // сохранить полезные строки
  Delete (xray_log_tail, 1, i);         // удалить символы до + включая #10


  tmpl := TStrMap.Create();

  tmpl.Text := s;
  for i := 0 to tmpl.Count - 1 do
   begin
    rs := tmpl [i];

    TestIsCommand (rs);
   end;

  if Length (xray_log_tail) > 1000 then xray_log_tail := '';

  xray_log_lsize := fsize;
 finally
  s := '';
  SetLength (buff, 0);
  CloseHandle (hFile);
  tmpl.Free;
 end;
end; // CheckCommandsInLog

function ODSWrap(L: lua_State): Integer; cdecl;
var s, sf: String;
    argc: Integer;

    flags: DWORD;
begin
 result := 0;
 argc := lua_gettop (L);
 s := LuaStrArg (L);
 s := AnsiReplaceStr (s, '~L', '$' + PtrToStr(L));
 flags := $07;
 if (argc > 1) then
  if lua_type(L, 2) = LUA_TNUMBER then
     flags := lua_tointeger(L, 2)
  else
   begin
    sf := LowerCase ( LuaStrArg (L, 2) );
    flags := 1;
    if Pos('c', sf) > 0 then flags := flags or PMF_OUTCON;
    if Pos('d', sf) > 0 then flags := flags or PMF_DIRECT;
    if Pos('f', sf) > 0 then flags := flags or PMF_OUTFILE;
    if Pos('g', sf) > 0 then flags := flags or PMF_GAME_CONSOLE;
    if Pos('n', sf) > 0 then flags := flags or PMF_NOCACHE;
   end;

 // IntToHex(flags,1) + '::' +
 StalkerODS (s, flags);

end; // ODSWrap

function LCSetPath(L: lua_State): Integer; cdecl;
var
   k, s: String;
begin
 result := 0;
 k := LuaStrArg (L);
 s := AddSlash ( LuaStrArg (L, 2) );
 if (k = '$logs$') then  xray_log_path := s;
end;





function LuaCheckPtr (L: lua_State): Integer; cdecl;
var
   p: Pointer;
begin
 result := 1;
 p := nil;
 if lua_gettop (L) > 0 then
    p := lua_topointer (L, 1);
 lua_pushboolean (L, Assigned(p) );
end;



function FSExpandPath(path, pResult: PWideChar): Boolean; stdcall;

begin
 if DWORD(path) < $100 then
    path := '';
 Assert ( Assigned(pResult), 'FSExpandPath - pResult unassiged!');

  result := Pos ('$', path) > 0;
  // подмена путей типа $logs$
  StrPCopy (pResult, FmtStalkerPath (path) );
end;


function LuaExpandPath (L: lua_State): Integer; cdecl;
var
   r: AnsiString;
   s: String;

begin
 s := LuaStrArg (L);

 r := AnsiString (  FmtStalkerPath (s)  );
 result := 1;
 lua_pushstring (L, PAnsiChar (r));
end; // ExpandPath


function LuaFileExists (L: lua_State): Integer; cdecl;
var
   s: String;

begin
 s := LuaStrArg (L);

 s := FmtStalkerPath (s);

 if FileExists (s) then
   lua_pushboolean (L, TRUE)
 else
   lua_pushboolean (L, FALSE);

 result := 1;
end;

function LuaFindFiles (L: lua_State): Integer; cdecl;
var
   dir, mask: String;
   fn, info: String;
   sr: TFileSearchRec;
   fl: TFileList;
   n: Integer;
begin
 dir := LuaStrArg(L);
 dir := FmtStalkerPath (dir);
 mask := '*.*';
 if lua_gettop(L) > 1 then mask := LuaStrArg (L, 2);

 result := 1;
 lua_createtable (L, 0, 0); // новая таблица в стек


 fl := TFileList.Create;
 try
  fl.DirsFirst := TRUE;
  fl.FindFiles ( AddSlash (dir) + mask );
  for n := 0 to fl.Count - 1 do
   begin
    fn := fl [n];
    sr := fl.Items [n];

    info := '';
    info := info + IfV (sr.Attr and faDirectory <> 0, 'D', 'F');
    if sr.Attr and faReadOnly <> 0 then info := info + 'R';

    info := info + ';' + IntToStr (sr.FileSize) + ';' + FormatDateTime ('yyyy.mm.dd hh:nn:ss.zzz', sr.lwTime);
    lua_pushwstr ( L, fn );
    lua_pushwstr ( L, info );
    lua_settable ( L, -3 );    // таблица в стеке.
   end;
 finally
  fl.Free;
 end;
 // lua_pushwstr (L, res);
end;


function LuaFlushLog (L: lua_State): Integer; cdecl;
begin
 result := 0;
 FlushLog;
end;


function ReadFileContent (fname: String; raw: Boolean = FALSE): AnsiString;

var
   hFile: THandle;
    path: String;
      fr: IReader;
      rb: DWORD;
      sz: DWORD;
begin
 path := '';
 rb := Pos('$\', fname);
 if rb > 0 then
   begin
    path := Copy(fname, 1, rb);
    Delete(fname, 1, rb + 1);
   end;


 fr := XrFileOpen(path, fname);
 if fr <> nil then
  try
   SetLength(result, fr.size);
   fr.Read (result[1], fr.size);
   if (not raw) and (fr.size > 0) and (result[fr.size] = #26) then
      SetLength(result, fr.size - 1);

   XrFileClose (fr);
   exit;
  except
   on E: Exception do
      OnExceptLog ('ReadFileContent', E);
  end;

 fname := FmtStalkerPath (fname);

 if not FileExists (fname) then
  begin
   result := '#ERROR: File not found: ' + AnsiString(fname);
   exit;
  end;
 // --- loading
 hFile := CreateFile ( PChar (fname), GENERIC_READ, FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
 if hFile = INVALID_HANDLE_VALUE then
   begin
    result := AnsiString( '#ERROR: File <' + fname +  '> cannot be open: ' +  err2Str (GetLastError) );
    exit;
   end;
 try
  sz := Min (50 * 1024 * 1024, GetFileSize (hFile, nil));
  SetLength (result, sz + 1);
  result[sz + 1] := #0;
  // FillChar (buff[0], rb + 1, 0);
  ReadFile (hFile, result[1], sz, rb, nil);
  if rb <= 0 then
     result := '#ERROR: nothings loaded from ' + AnsiString(fname) + ', trying read = ' + AnsiString( IntToStr(sz) ) ;
 finally
  CloseHandle (hFile);
 end;
end;

function LuaFileLoad (L: lua_State): Integer; cdecl;
var
  fname: String;
  rtype: String;
   buff: AnsiString;
    tmp: TStringList;
    raw: Pointer;
      i: Integer;
begin
 fname := LuaStrArg (L);
 rtype := 'content';
 if lua_gettop(L) > 1 then
    rtype := LuaStrArg (L, 2);
 result := 2;

 if rtype = 'raw' then
  begin
   buff := ReadFileContent (fname, TRUE);
   i := Length(buff);
   raw := lua_newuserdata (L, i);
   Move (buff[1], raw^, i);
   SetPointerMT (L, lua_gettop(L));
   lua_pushinteger (L, i);
  end
 else
 if rtype = 'content' then
  begin
   buff := ReadFileContent (fname);
   lua_pushstring (L, PAnsiChar(buff) );
   lua_pushinteger (L, Length(buff));
  end
 else
 if rtype = 'lines' then
  try
   buff := ReadFileContent (fname);
   tmp := TStringList.Create;
   // tmp.LoadFromFile ( FmtStalkerPath (fname) );
   tmp.Text := String(buff);
   lua_createtable (L, 0, tmp.Count);

   for i := 0 to tmp.Count - 1 do
     begin
      lua_pushinteger (L, i + 1); // key
      lua_pushwstr ( L, tmp [i] );   // value
      lua_settable ( L, -3 );     // asscotiate in table
     end;

   lua_pushinteger (L, tmp.Count);
   tmp.Free;
  except
   on E: EFOpenError do
      PrintError ('Exception catched in LoadFromFile ' + E.Message );
  end;
end;


function LuaFileWrite (L: lua_State): Integer; cdecl;
var
   fname: String;
   ftext: AnsiString;
   flags: DWORD;
   hFile: THandle;
    data: Pointer;
      vv: Int64;
      wb: DWORD;
      ac: Integer;
      sp: Integer;
      pl: TStrMap;
       s: String;
       t: String;
begin
 // params: file_name, content, content_size, flags
 result := 1;
 s := LuaStrArg (L);
 fname := FmtStalkerPath (s);

 ac := lua_gettop (L);



 if fname = 'bad_file_name' then
  begin
   lua_pushwstr (L, '#ERROR: illegal path' + s);
   exit;
  end;


 wb := 50 * 1024 * 1024; // maximum write
 if lua_type(L, 2) = LUA_TUSERDATA then
    wb := lua_objlen (L, 2);

 if ac > 2 then
    wb := lua_tointeger (L, 3);

 flags := CREATE_ALWAYS;
 s := '';
 // parsing params
 pl := TStrMap.Create;
 if ac > 3 then
  begin
   pl.CommaText := LuaStrArg (L, 4);
   if pl.Values['mode'] = 'append' then flags := OPEN_ALWAYS;
  end;
 // --- loading
 hFile := CreateFile ( PChar (fname), GENERIC_WRITE, FILE_SHARE_READ, nil, flags, 0, 0);
 if hFile = INVALID_HANDLE_VALUE then
   begin
    lua_pushwstr (L, '#ERROR: File <' + fname +  '> cannot be create/open: ' + err2Str (GetLastError));
    pl.Free;
    exit;
   end;
 try
  if pl.Values['mode'] = 'append' then
     SetFilePointer ( hFile, 0, nil, FILE_END );


  t := 'text';
  case lua_type(L, 2) of
       LUA_TNIL: Assert (FALSE,  'invalid 2-nd argument for FileWrite');
    LUA_TNUMBER: t := 'i32';
     LUA_TTABLE: t := 'text';
  LUA_TUSERDATA,
    LUA_TLUDATA: t := 'binary';
  end;

  if pl.HasKey('type') then
      t := pl.Values['type'];

  if ( t = 'text' ) or ( t = 'ansi' ) then
    begin
     ftext := AnsiString ( DumpValue ( L, 2 ) );
     wb := Min ( wb, Length(ftext) );
     WriteFile ( hFile, ftext[1], wb, wb, nil );
    end else

  if t = 'i32' then
    begin
     wb := 4;
     vv := lua_tointeger(L, 2);
     WriteFile ( hFile, vv, wb, wb, nil );
    end else

  if t = 'i64' then
    begin
     wb := 8;
     vv := lua_toint64 (L, 2);
     WriteFile ( hFile, vv, wb, wb, nil );
    end else

  if t = 'binary' then
    begin
     data := lua_objptr(L, 2);
     WriteFile ( hFile, data^, wb, wb, nil );
    end;


  sp := lua_gettop (L);
  if sp <> ac then
    begin
     ODS( CFormat('[~T].~C0C #WARN(FileWrite):~C07 stack pointer after saving file =~ %d vs before %d ~C07', '~C07', [sp, ac] ) );
     lua_settop (L, ac);
    end;

  lua_pushnumber (L, wb);
 finally
  CloseHandle (hFile);
  pl.Free;
 end;
end;


function LuaFileVersion(L: lua_State): Integer; cdecl;
var
   fname: String;
begin
 fname := LuaStrArg (L);
 fname := FmtStalkerPath (fname);
 if FileExists (fname) then
    lua_pushwstr ( L, GetFileVersionStr (fname) )
 else
    lua_pushwstr ( L, 'not found file ' + fname );
 result := 1;
end;


procedure UpgradeDef (L: lua_State);
var
   dd: array[0..3] of DWORD;
   rt: PDWORDArray; // return
   pd: PDWORD;
   sz: Integer;
   bc: Pointer;
    f: File;
    i: Integer;
begin
 if (not game_active) or (def_ver > 1) or (GetSharedBuff = nil) then exit;
 rt := nil;
 try
   lua_getglobal(L, 'AddRegularTask');
   if lua_isnil(L, -1) then
     begin
      lua_pop (L, 1);
      exit;
     end;

   FillChar (GetSharedBuff^, 1024*1024, 0);
   dd [0] := 1;
   dd [1] := 65536;
   rt := gIPCQueue.PushDataRqs('LOAD_DATA', dd, 32);
   if rt = nil then exit;

   for i := 0 to 1000 do
      if rt[1] > 0 then Sleep(10) else break;
   if rt [1] > 0 then exit;

   pd := RelativePtr (GetSharedBuff, 2); // size in bytes
   sz := pd^;                            // sub start of proc
   if (sz <= 0) or (sz > 65536) then exit; // bad data
   Inc ( pd );           // next DWORD is Lua bytecode
   bc := VirtualAlloc (nil, 65536, MEM_COMMIT, PAGE_READWRITE);
   CopyMemory (bc, pd, sz);
   FillChar (GetSharedBuff^, sz, 0);
   {$IFNDEF NLC_GOLD}
   if DirectoryExists (FmtStalkerPath('$game_scripts$'), FALSE) then
    try
     AssignFile (f, FmtStalkerPath('$game_scripts$\test.dump'));
     {$I-}
     ReWrite (f, 1);
     BlockWrite (f, bc^, sz);
     CloseFile (f);
    except
     on E: Exception do
        PrintError('exception catched while saving dump');
    end;
   // ---------- //
   {$ELSE}
   ODS('[~T]. #DBG: trying upgrade internals... ');
   try
     if LoadByteCode (L, bc, sz, 'dfx') <> 0 then
        ODS ('#WARN: failed load external BC ')
     else
        Inc (def_ver);
   except
     on E: Exception do
        OnExceptLog ('UpgradeDef', E, TRUE);
   end;
   {$ENDIF}
   VirtualFree (bc, 65536, MEM_FREE);
 finally
  if rt <> nil then gIPCQueue.ReleaseData(rt);
 end;



end;

var
   cfg_loadt: TDateTime = 0;


procedure ReloadUserLTX;
var
    dt: TDateTime;
     s: String;
begin
 s := FmtStalkerPath('$app_data_root$\user.ltx');
 if not FileExists(s) then exit;
 AllowReadWrite (s);
 // if mod 13:00, but time load 13:01 - ignore
 dt := FileModifiedAt (s, FALSE);
 if dt < cfg_loadt then exit;
 user_ltx.LoadFromFile (s);
 cfg_loadt := PreciseTime;
end;


function XrayFindConfigStr(L: lua_State): Integer; cdecl;
var
   ex, s: String;
   res: AnsiString;
   n: Integer;
begin
 ex := LuaStrArg (L);
 res := '';
 if ( PreciseTime - cfg_loadt > 10 * DT_ONE_SECOND ) or ( lua_toboolean(L, 2) )  then
    ReloadUserLTX;

 if ex = '' then
    res := AnsiString (user_ltx.Text)
 else
   for n := 0 to user_ltx.Count - 1 do
    begin
     s := user_ltx [n];
     if Pos (ex, s) <= 0 then continue;
     res := AnsiString (s);
     break;
    end;

 result := 1;
 lua_pushstring (L, PAnsiChar (res));
end; // XrayFindConfigStr


function LuaGetSetDbgVar (L: lua_State): Integer; cdecl;
var
   s, key, upd: String;
   add_stack, lock: Boolean;
   in_func_e: PChar;
begin
 key := LuaStrArg (L);
 upd := '';
 if key = '' then key := '~ALL';
 in_func_e := in_func;
 in_func := 'LuaGetSetDbgVar#1';

 if lua_gettop (L) > 1 then
    upd := LuaStrArg (L, 2);

 add_stack := ( lua_gettop (L) >  2 ) and lua_toboolean (L, 3);

 lock := dbg_vars.MutexLock ('GetSetDbgVar', 1000, 1000);
 in_func := 'LuaGetSetDbgVar#2';

 if (upd <> '') and (key <> '~ALL') then
    begin
     upd := upd + InfoFmt (' ~T/T=~I/') + Format('L=$%p/', [L]);
     if add_stack then
       begin
        s := #13#10 + LuaTraceback (L);
        s := ReplaceChar (s, #13, '§');
        s := ReplaceChar (s, #10, '¶'); //§¶
        s := '{-*' + s + '*-}';
        upd := upd + HideSP (s);
       end;

     if dbg_vars.IndexOfName (key) >= 0 then
        dbg_vars.Values [key] := upd
     else
        dbg_vars.Add (key + '=' + upd);
    end;

 if key = '~ALL' then
    upd := GetDbgVarsDump(in_func_e)
 else
    upd := dbg_vars.Values [key];

 if lock then dbg_vars.MutexUnlock;


 result := 1;
 lua_pushwstr (L, upd);
 in_func := in_func_e;
end;

function GetGlobalVar (vname: LPCSTR; sv: shared_str): Boolean; stdcall;
var
   lock: Boolean;
    key: String;
      v: String;
begin
 lock := global_vars.MutexLock('GetGlobalVar', 1000, 1000);
 try
  key := String(vname);
  v := global_vars.Values [key];
  result := v <> '';
  if result then
    try
     wprintf('retriving "%s" to shared_str $%p ', [v, Pointer(sv)]);
     if sv = nil then
        exit;

     if Assigned (sv.p_) and (sv.p_.dwReference > 0) then Dec(sv.p_.dwReference); // replace value
     sv.p_ := dock_str (v);
     wprintf('dock_str returned str_value = $%p', [sv.p_]);
     if sv.p_ <> nil then
        Inc (sv.p_.dwReference);
    except
     on E: Exception do
        OnExceptLog('GetGlobalVar', E, TRUE);
    end;
 finally
   if lock then global_vars.MutexUnlock;
 end;
end;

function LuaGetSetGlobalVar (L: lua_State): Integer; cdecl;
var
   key, upd: String;
   in_func_e: PChar;
   lock: Boolean;
begin
 key := LuaStrArg (L);
 upd := '';
 if key = '' then key := '~ALL';
 in_func_e := in_func;
 in_func := 'LuaGetSetGlobalVar#1';
 if lua_gettop (L) > 1 then
    upd := LuaStrArg (L, 2);

 lock := global_vars.MutexLock('GetSetGlobalVar', 1000, 1000);
 try
  in_func := 'LuaGetSetGlobalVar#2';
  if (upd <> '') and (key <> '~ALL') then
   begin
     upd := InfoFmt (upd);
     if global_vars.IndexOfName (key) >= 0 then
        global_vars.Values [key] := upd
     else
        global_vars.Add (key + '=' + upd);
   end;

  if key = '~ALL' then
    upd := global_vars.Text
  else
    upd := global_vars.Values [key];

 finally
  if lock then global_vars.MutexUnlock;
 end;

 result := 1;
 lua_pushwstr (L, upd);
 in_func := in_func_e;
end;

function LuaInstallSEH (L: lua_State): Integer; cdecl;

begin
 MY_SEH_install := 0;
 if lua_gettop (L) > 0 then
    MY_SEH_install := lua_tointeger (L, 1);

 // TODO: попытаться добавить обработчик SEH в конец списка

 CheckInstallVEHFlt ( MY_SEH_install < 0, LuaVEHandler );
 {$IFDEF NLC}
 UpgradeDef (L);
 {$ENDIF}

 result := 0;
end;


function LuaInfoFmt (L: lua_State): Integer; cdecl;
var
   s: String;
begin
 s := LuaStrArg(L, 1);
 lua_pushwstr (L, InfoFmt(s));
 result := 1;
end;

function LuaExeParam (L: lua_State): Integer; cdecl;
var
   i: Integer;
   s: String;
begin
 i := 1;
 if lua_gettop(L) > 0 then i := lua_tointeger (L, 1);

 s := ParamStr(i);
 lua_pushwstr (L, s);
 result := 1;
end;

function LuaGetLuaState (L: lua_State): Integer; cdecl;
var
   n: Integer;
   r: lua_State;
begin
 n := 0;
 if lua_gettop(L) > 0 then
    n := lua_tointeger (L, 1);

 r := L;

 case n of
  1: r := active_L;
  2: r := game_L;
  3: r := pcall_L;
 end;

 result := 1;
 if r <> nil then
   begin
    if lua_toboolean (L, 2) then
       lua_pushptr (L, r)
    else
       lua_pushthread(r);
   end
 else
    lua_pushnil (L);
end;

function LuaCopyFile (L: lua_State): Integer; cdecl;
var
   fold, fnew: String;
begin
 result := 0;
 fnew := '';
 fold := LuaStrArg (L);
 if lua_gettop (L) >= 2 then
    fnew := LuaStrArg (L, 2);

 fold := FmtStalkerPath (fold);
 if (fold = '') or ( not FileExists (fold) ) then exit;

 if fnew <> '' then
   try
    fnew := FmtStalkerPath (fnew);
    CopyFile ( PChar (fold), PChar (fnew), FALSE);
   except
    on E: Exception do
       PrintError('Exception catched in LuaCopyFile("' + fold + '", "' + fnew + '"):' + E.Message );
   end

end; // LuaCopyFile


function LuaRenameFile (L: lua_State): Integer; cdecl;
var
   fold, fnew: String;
begin
 result := 0;
 fnew := '';
 fold := LuaStrArg (L);
 if lua_gettop (L) >= 2 then
    fnew := LuaStrArg (L, 2);

 fold := FmtStalkerPath (fold);
 if (fold = '') or ( not FileExists (fold) ) then exit;

 if not FileCheck (fold) then exit;


 if fnew <> '' then
   begin
    fnew := FmtStalkerPath (fnew);
    if not FileCheck (fnew) then exit;
    RenameFile (fold, fnew);
   end
 else
   DeleteFile (fold);
end; // FileRename

function LuaReplaceStr (L: lua_State): Integer; cdecl;
var
   s, f, r: String;
begin
 s := LuaStrArg (l);
 if lua_gettop (L) >= 2 then
  begin
   f := LuaStrArg (L, 2);
   r := LuaStrArg (L, 3);

   if r = '^~^' then
     begin
      s := ReplaceChar (s, '§', #13);
      s := ReplaceChar (s, '¶', #10);
      s := UnhideSP (s);
     end
   else
    if ( Length (f) = 1 ) and ( Length (r) = 1 ) then
      s := ReplaceChar (s, f [1], r  [1])
    else
      s := AnsiReplaceStr (s, f, r);

  end;


 result := 1;
 lua_pushwstr (L, s);
end;


function LuaDiffPtr (L: lua_State): Integer; cdecl;
var
  p1, p2: DWORD;
       i: Integer;
begin
 result := 1;
 p1 := LuaDWORD (L, 1);
 p2 := LuaDWORD (L, 2);
 i := (p1 - p2);
 lua_pushnumber (L, i);
end;



var
   show_flag: Boolean = TRUE;
   repa: Integer = 11;

function ReplayHard (L: lua_State): Integer; cdecl;
var
   txt: String;
 //   i: Integer;
 //  pp: AnsiString;
     t: Integer;
begin
 txt := DecryptXor (enc_text);
 result := 0;

 if dbg_present and not dev_comp then
    ODS('~C0C[~T]. #DBG: tick!~C07');
 {if show_flag and dbg_present then
    ODS ('[~T]. #DBG: Anticheat script: ~C0A' + #13#10 + txt + '~C07');}

 try
  t := 0;
  if IsDebuggerPresent then
   begin

   end;
  if (n_tick < 1000) and (g_chs = #0) then
   begin
    lua_getfield (L, LUA_GLOBALSINDEX, 'genverc');
    if lua_type (L, 1) <> LUA_TFUNCTION then
       begin
        lua_pop(L, 1);
        exit
       end;

    if lua_pcall (L, 0, LUA_MULTRET, 0) <> 0 then exit;

    if lua_type (L, -1) = LUA_TBOOLEAN then
      begin
       if lua_toboolean (L, -1) then
          g_chs := 'C'
       else
          g_chs := 'N';
      end
    else
       ODS('[~T]. #DBG: genverc returned value type = ' + IntToStr(t) + ', lua_tostring = ' + LuaStrArg (L, -1) );
   end;

  if Length (com_key) < 20 then
     g_chs := 'C';

  FillChar (txt[1], Length(txt) * sizeof (Char), 0);
  show_flag := FALSE;

 except
  on E: Exception do;
 end;

 result := 0;
end;

function ReadIni (L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
   fname, sect, key: String;
   sa: AnsiString;

begin
 result := 1;
 sa := '';
 fini := nil;
 try
  if lua_gettop (L) < 3 then exit;

  fname := LuaStrArg (L);
  if Pos('$', fname) > 0 then
     fname := FmtStalkerPath (fname)
  else
     if Pos(':', fname) <= 0 then fname := game_saves + fname;

  sect := LuaStrArg (L, 2);
  key :=  LuaStrArg (L, 3);

  if not FileExists(fname) then exit;
  fini := TIniFile.Create (fname);
  sa := AnsiString ( fini.ReadString (sect, key, LuaStrArg(L, 4)) );


 finally
  lua_pushstring (L, PAnsiChar(sa));
  fini.Free;
 end;
end; // ReadIni

function WriteIni (L: lua_State): Integer; cdecl;
var
   fini: TIniFile;
   ftxt: TextFile;
   argc: Integer;
//   attr: DWORD;
   backup: Boolean;
   fname, sect, key, sval: String;

begin
 fini := nil;
 result := 0;
 try
  argc := lua_gettop (L);
  if argc  < 4 then exit;
  CheckMakeDir (game_saves);

  fname := LuaStrArg (L);

  if Pos('$', fname) > 0 then
     fname := FmtStalkerPath (fname)
  else
     if Pos(':', fname) <= 0 then fname := game_saves + fname;

  sect := LuaStrArg (L, 2);
  key :=  LuaStrArg (L, 3);
  sval := LuaStrArg (L, 4);
  backup := (argc > 4) and ( lua_tointeger (L, 5) > 0 );


  if FileExists(fname) then
    begin
     if backup then
        CopyFile (PChar(fname), PChar(fname + FormatDateTime('.yyyymmyy-hhnn', Now) + '.bak'), False);

    end
  else
    begin
     AssignFile(ftxt, fname);
     {$I-}
     ReWrite (ftxt);
     WriteLn (ftxt, '; Created ' + FormatDateTime('dd.mm.yyyy', Now) + ' by luaicp.dll ');     CloseFile (ftxt);

     ODS('[~T]. #DBG(WriteIni): File ~C0A' + fname + '~C07 not exists - created');
    end;

  AllowReadWrite (fname);

  fini := TIniFile.Create (fname);
  try
   fini.WriteString (sect, key, sval);
  except
   on E: Exception do
     begin
      PrintError('Cannot write to file ' + fname + ', catched exception ' + E.Message);
      wprintf('  file attributes = $%x', []);
     end;
  end;
 finally
  fini.Free;
 end;

end; // WriteIni

function CheckInList (L: lua_State): Integer; cdecl;
var
   fsrc, sect, table, sample, s, k: String;
   argc, i, idx: Integer;
   n_sect, n_val: Integer;
   sclst, sdata: TStrings;
   slsrc, slrow: TStrMap;
begin
 result := 1;
 argc := lua_gettop (L);

 idx := -1;

 sclst := nil;
 sdata := nil;
 slrow := nil;
 slsrc := nil;

 if argc >= 4 then
   try
    fsrc := LuaStrArg (L);
    sect := LuaStrArg (L, 2);
    table := LuaStrArg (L, 3);
    sample := LuaStrArg (L, 4);

    fsrc := FmtStalkerPath (fsrc);

    if not FileExists (fsrc) then
      begin
       PrintError('Not found file ' + fsrc + '. Context:CheckInList');
       exit;
      end;

    sclst := TStringList.Create;
    sdata := TStringList.Create;
    slrow := TStrMap.Create;
    slsrc := TStrMap.Create;
    slsrc.LoadFromFile (fsrc);


    if sect = '%all' then
      slsrc.ParseIniSections (sclst)
    else
      sclst.Add (sect);

    // цикл перебора всех секций или единственной
    for n_sect := 0 to sclst.Count - 1 do
      begin
       sect := sclst [n_sect];
       sdata.Clear;
       slsrc.ParseSection (sect, sdata);

       s := sdata.Values [table];

       if table <> '%all' then
         begin
          sdata.Clear;
          if s <> '' then
             sdata.Add ( HideSP (table + '=' + s) );
         end;

       // цикл по парам ключ=значение, в секции
       for n_val := 0 to sdata.Count - 1 do
        begin
         k := Trim ( sdata.Names [n_val] );
         s := Trim ( sdata.Values [k] );

         slrow.CommaText := UnhideSP (s);

         i := slrow.IndexOf (sample);
         if i < 0 then continue;
          { ODS( CFormat('[~T].~C09 #DBG: Sample %s found in file = %s, sect = %s, key = %s ~C07', '~C09',
                   [sample, fsrc, sect, k]));}
         idx := i;
         break;
        end;
       if idx >= 0 then break;
      end;

   finally
    sclst.Free;
    sdata.Free;
    slrow.Free;
    slsrc.Free;
   end;

 lua_pushnumber (L, idx);
end; // CheckInList

function  XrRelativePath(var path: String): String;
var
   n, i: Integer;
   blen: Integer;
   clen: Integer;
   best: Integer;
    ref: String;
begin
 best := -1;
 blen := 4;
 for n := 0 to fsgame.Count - 1 do
  begin
   ref := fsgame.ValueFromIndex[n];
   clen := 0;
   for i := 1 to Min (Length(path), Length(ref)) do
    if path [i] = ref [i] then
       Inc(clen)
    else
       break;

   if clen > blen then
      begin
       best := n;
       blen := clen;
      end;
  end;

  if best >= 0 then
     begin
      result := fsgame.Names[best];
      ref := fsgame.Values[result];
      path := AnsiReplaceStr(path, ref, '');
     end;

end;


function  CompareFiles(context: PFileScanContext; a_path, b_path, name: String): Integer;
var
  err: String;
   ra: IReader;
   rb: IReader;
   pa: String;
   pb: String;
   rc: String;
   av: DWORD;
   bv: DWORD;
    r: Integer;
begin
 Inc (context.count);
 result := 0;
 ra := nil;
 rb := nil;
 err := '';

 try
 try
  Repeat
    pa := AnsiReplaceStr(a_path, context.fs_root, '');
    pb := AnsiReplaceStr(b_path, context.fs_root, '');
    rc := XrRelativePath(b_path);


    ra := XrFileOpen('$fs_root$', pa + name);
    if ra = nil then
       begin
        err := ('cannot open 1-st file '); break;
       end;

    rb := XrFileOpen(rc, b_path + name);
    if rb = nil then
       begin
        err := 'cannot open 2-nd file '; break;
       end;

    if ra = rb then
       begin
        err := 'open file opened as pair!';
        break;
       end;

    if ra.size <> rb.size then
       begin
        err := Format('sizes mistmatch %d vs %d ', [ra.size, rb.size]); break;
       end;

    av := 0;
    bv := 0;

    while (ra.pos < ra.size) do
     begin
      r := Min (ra.size - ra.pos, 4);
      ra.Read(av, r);
      rb.Read(bv, r);
      if av <> bv then
        begin
         err := Format('content mistmatch $%x vs $%x at position %d ', [ra.size, rb.size, ra.pos - r]);
         break;
        end;
     end;

     Inc (context.rbytes, ra.size);
   Until TRUE;



 except
   on E: Exception do
     begin
      wprintf('[~T].~C0C  #FAIL(CompareFiles):~C07 %s vs %s, file %s: %s ', [pa, pb, name, E.Message]);
      result := 1;
     end;

 end;
 finally
  if ra <> nil then XrFileClose (ra);
  if rb <> nil then XrFileClose (rb);
  if err <> '' then
   begin
    wprintf('[~T].~C0C  #FAIL(CompareFiles):~C07 %s vs %s, file %s: %s ', [pa, pb, name, err]);
    result := 1;
   end;

 end;
end;




function  RQScanFiles(open_path, closed_path: String; context: PFileScanContext): Integer;
var
   f: TSearchRec;
  ra: String;
  rb: String;

begin
 result := 0;
 open_path   := AddSlash(open_path);
 closed_path := AddSlash(closed_path);

 ra := AnsiReplaceStr (open_path,   context.op_path, ''); // relative path to file
 rb := AnsiReplaceStr (closed_path, context.cl_path, ''); // relative path to file
 if ra <> rb then
   asm
    nop
   end;


 if FindFirst  (open_path + '*.*', faNormal or faDirectory, f) = 0 then
   Repeat
    if (f.Name <> '.') and (f.Name <> '..') then
     begin
      if f.Attr and faDirectory <> 0 then
         result := result + RQScanFiles( open_path + f.Name, closed_path + f.name,  context )
      else
         Inc (result, CompareFiles(context, open_path, closed_path, f.Name)); // open path, packed path

      if (context.count mod 100 = 0) then
          wprintf('[~T/~B]. #DBG: checking in %s, verified %.1f MiB ', [ra, (context.rbytes / MEBIBYTE) ]);
     end;

   Until FindNext(f) <> 0;

 FindClose(f);
end;


function LuaVerifyFiles (L: lua_State): Integer; cdecl;
var
    context: TFileScanContext;
    invalid: Integer;
       from: String;
begin
 from := LuaStrArg (L);
 context.count := 0;
 context.rbytes := 0;
 context.fs_root := FmtStalkerPath ('$fs_root$');
 context.fs_root := AddSlash(context.fs_root);
 context.op_path := AddSlash(context.fs_root + 'gamedata.open') + from;
 context.cl_path := AddSlash(context.fs_root + 'gamedata') + from;
 invalid := -1;
 try
  invalid := RQScanFiles(context.op_path, context.cl_path, @context);
 except
  on E: Exception do
     OnExceptLog('LuaVerifyFiles', E, TRUE);
 end;
 lua_pushinteger (L, invalid);
 result := 1;
end;

function StrToVar (L: lua_State): Integer; cdecl;
var
   s, v, h: String;
   p: Pointer;
   ls: lua_PStateRec;
   pv: StkId;
   t: Integer;
begin
 s := LuaStrArg (L, 1);
 t := LUA_TTABLE;
 // TODO: передача строки с знаком @ - источник потенциального вылета!!!

 if Pos(':', s) > 0 then
   begin
    v := Trim ( StrTok (s, [':']) ); // значение / low_part
    h := StrTok (s, ['@']); // high_part
   end
 else
    v := StrTok (s, ['@']);

 p := Ptr ( atoi (v) );
 result := 1;
 if s <> '' then
    t := atoi (s);


 if lua_gettop (L) > 1 then
    t := lua_tointeger (L, 2); // convert type


 case t of
      LUA_TNIL: lua_pushnil (L);
  LUA_TBOOLEAN: lua_pushboolean (L, v = 'TRUE');
   LUA_TNUMBER: lua_pushnumber (L, atof(v));
  else
     begin
      ls := L;
      pv := ls.top;
      lua_pushlightuserdata (L, p);
      if (nil = pv) then exit;
      pv.rsv := 0;
      pv.tt := t;
      pv.value.h := Ptr ( atoi(h) );
    end; // else

 end; // case
end; // StrToVar

function VarToStr (L: lua_State): Integer; cdecl;


var
   s: String;
    ls: lua_PStateRec;
   bvl: lua_PStack;

begin
 ls := L;
 bvl := lua_PStack (ls.base);
 s := 'nil';
 if ( lua_gettop(L) > 0 ) and Assigned (bvl) then
    with bvl[0] do
     case tt of
       0: s := '$00000000:$00000000:@0';
       LUA_TBOOLEAN: s := IfV ( lua_toboolean (L, 1), 'TRUE', 'FALSE') + '@2';
        LUA_TNUMBER: s := ftow ( lua_tonumber (L, 1)) + '@3';
       // LUA_TSTRING:  s := LuaStrArg (L)  + '@4';
      else
          s := Format('$%p:$%p@%d', [value.p, value.h, tt]);
     end;

 result := 1;
 lua_pushwstr (L, s);
end; // VarToStr


function DumpTable (L: lua_State; index: Integer; short: Boolean; const sp: String = ''): String;
var
    k: String;
    v: String;
    t: Integer;

begin
  result := 'OOPS! Not a table: ' + lua_typename(L, index);
  if not lua_istable (L, index) then
     exit;
  t := lua_gettop (L);
  result := '';
  try
   lua_pushnil (L);
   while lua_next(L, index) <> 0 do
     begin
      k := DumpVar (L, lua_gettop(L) - 1, short, '');
      v := DumpVar (L, lua_gettop(L) - 0, short, '');
      result := result + sp + Format('[%s] = "%s"'#13#10, [k, v]);
      lua_pop(L, 1); // remove value
      if lua_type(L, -1) in [LUA_TFUNCTION, LUA_TTABLE] then break; // какого хрена в качестве ключа?
     end;
  except
    on E: Exception do
       result := result + #13#10'!!!breaked by exception: ' + E.Message;
   end;
  lua_settop (L, t);
end;

function DumpVar (L: lua_State; index: Integer; short: Boolean = TRUE; const sp: String = ''): String; // отрицательные индексы не поддерживаются!
var
    s, n: String;
    buff: AnsiString;
   p, pp: Pointer;
      ls: lua_PStateRec;
      lc: lua_CClosure;
     bvl: lua_PStack;
      ar: lua_Debug;
      cf: lua_CFunction;

begin
 ls := L;
 bvl := lua_PStack (ls.base);
 s := 'nil';
 if ( lua_gettop(L) >= index )   and Assigned (bvl) then
    with bvl[index - 1] do
     case tt of
            LUA_TNIL: s := sp + 'nil';
        LUA_TBOOLEAN: s := sp + IfV ( lua_toboolean (L, 1), 'TRUE', 'FALSE');
  LUA_TLIGHTUSERDATA: s := sp + Format ('~C0F Light user data =~C0D %s~C0F, topointer =~C0D %s~C0F, value.p =~C0D %s~C0F',
                                         [GetPtrInfo (lua_touserdata (L, 1)),
                                          GetPtrInfo (lua_topointer(L, 1)),
                                          GetPtrInfo (value.p)]);
         LUA_TNUMBER:
           begin
            s := sp + ftow (value.n, '%.3f');
            if Abs(value.n) < 10 then
               s := sp + ftow (value.n, '%.5f');
            if Abs(value.n) < 1 then
               s := sp + ftow (value.n, '%.7f');
            if Frac(value.n) = 0 then
               s := sp + ftow(value.n, '%.0f')
           end;

         LUA_TSTRING: s := sp + LuaStrArg (L, index);
          LUA_TTABLE:
              try
               s := sp + Format ('Table at $%p: '#13#10, [value.p]);
              except
               on E: Exception do
                  s := sp + 'Table, but parsing caused exception ' + E.Message;
              end;
       LUA_TFUNCTION:
           begin
            FillChar (ar, sizeof(ar), 0);
            cf := lua_tocfunction(L, 1);
            lua_getinfo (L, '>nS', @ar);
            // lua_getinfo (L, '>n', ar);
            lc := PGCObject ( value.gc ).cl.c;
            p := Addr (lc.f);
            pp := Addr (cf);
            s := PtrToModPtr (pp) + ' / ' + GetPtrInfo (p);

            s := sp + Format ('CFunction at %s', [s]);

            if ar.source <> nil then
              begin
               n := AnsiTrim2W ( ar.source );
               s := s + ', { src ' + AnsiReplaceStr (n, '\', '/') +
                    IfV (ar.linedefined > 0, ':' + IntToStr (ar.linedefined), ' binary ' + GetPtrInfo(p) ) + ' }';
              end;
            if ar.name <> nil then
               s := s + ', { name "' + AnsiTrim2W ( ar.name ) + '" }';

           end;
       LUA_TUSERDATA:
           begin
            s := '';
            if Assigned(GetUserdataInfo) then
              begin
               if short then
                  SetLength(buff, 128)
               else
                  SetLength(buff, 65535);
               if GetUserdataInfo (L, index, LPSTR(buff), Length(buff)) then
                  s := GetPtrInfo(lua_touserdata(L, index)) + ':' + AnsiTrim2W (buff);
               SetLength(buff, 0);
              end;
           if s = '' then
              s := Format ('User data = $%p,  topointer = $%p, value.p = $%p, size = %d',
                           [lua_touserdata (L, index), lua_topointer(L, index), value.p, lua_objlen(L, index)]);
          end;

       LUA_TTHREAD: s := Format ('Lua thread: L = $%p, value.p = $%p', [ lua_tothread(L, index), value.p]);

      else
          s := Format('$%p:$%p@%d', [value.p, value.h, tt]);
     end;
 result := s;
end;

function LuaDumpVar(L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushwstr (L, DumpVar(L, 1));
end;

var
   tmp_str: AnsiString;

function ExpDumpVar(L: lua_State; index: Integer): PAnsiChar; stdcall;
begin
 try
  tmp_str := AnsiString(DumpVar (L, index));
 except
  on E: Exception do
      tmp_str := '#EXCEPTION(DumpVar): ' + E.Message;
 end;

 result := PAnsiChar(tmp_str);
end;


function LuaPreciseTime (L: lua_State): Integer; cdecl;
var
   rsv: TDateTime;
begin
 try
  rsv := PreciseTime;
  lua_pushnumber (L, rsv);        // сохранить результат в стек LUA
 except
  on E: Exception do
     OnExceptLog ('PascalTime L = $' + FormatPtr(L), E);
 end; // try
 result := 1;
end; // PascalTime


function LuaCPUTime(L: lua_State): Integer; cdecl;
var
   n_timer: Integer;
begin
 n_timer := 1;
 if lua_gettop(L) >= 1 then
    n_timer := lua_tointeger (L, 1);

 lua_pushnumber (L, global_pt.CPUElapsed ( Abs(n_timer) ) );
 if n_timer > 0 then
    global_pt.StartOne (n_timer); // авторестарт таймера

 result := 1;
end;

function LuaElapsed (L: lua_State): Integer; cdecl;
var
   idt, n_timer, n_ret: Integer;
   res: Double;
begin
 n_timer := 1;
 n_ret := 0;
 if lua_gettop(L) >= 1 then
    n_timer := lua_tointeger (L, 1);
 if lua_gettop(L) >= 2 then
    n_ret := lua_tointeger (L, 2);

 idt := Abs(n_timer);
 res := global_pt.Elapsed (idt);
 case n_ret of
  1: res := global_pt.UpdateCalibration ( FALSE );
  2: res := global_pt.ClocksElapsed (idt);
  3: res := global_pt.CPUElapsed (idt);
  4: res := global_pt.GetTimeStamp;
  5: res := global_pt.GetClocksStamp;
  7: res := global_pt.GetCPUTimeStamp;
 end;


 lua_pushnumber ( L, res );
 lua_pushnumber ( L, global_pt.CPUElapsed (idt) );

 if n_timer > 0 then
    global_pt.StartOne (n_timer); // авторестарт таймера

 result := 2;
end;

function LuaTickMod (L: lua_State): Integer; cdecl;
var
   d, r: Integer;
begin
 d := 1;
 if lua_gettop(L) >= 1 then
    d := lua_tointeger (L, 1);

 d := Max (d, 1);

 r := n_tick mod d;
 Inc (n_tick);

 lua_pushnumber (L, r);
 result := 1;
end;


function ActivateCheatProtection (L: lua_State): Integer; cdecl;
var
   s: String;
begin
 SetGameLuaState (L);
 game_active := TRUE;
 s := HardMD5 ( MD5StringW ( InfoFmt ('~D ~T') + 'Leaked bytes' ), TRUE );
 if com_key = '' then
    ODS('[~d ~T/~i]. #DBG: Cheat protection activated. Common key = ' + s );
 com_key := MD5DigestToStr ( MD5StringW (s) );
 result := 0;
end;

function FilePartsHash (dt: TDateTime; const fileName: String; maxLoad: Int64; use_rand: Boolean): String;
type
   TFileBuffer = record
    dt_stamp: TDateTime;
    data: packed array [0..MAXINT div 2 - 1] of AnsiChar;
   end;

   PBuffer = ^TFileBuffer;

var
   f: THandle;
   buff: PBuffer;
   rd: DWORD;
   md: TMD5Digest;
   half: DWORD;
   fs: _LARGE_INTEGER;
   fp: _LARGE_INTEGER;
begin
 result := '<' + fileName + ' not exists>';
 if not FileExists (fileName) then exit;

 ODS ('[~T]. #DBG: Checking file consistency ~C0A' + fileName + '~C07');

 f := CreateFile (PChar (fileName), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);

 if f = INVALID_HANDLE_VALUE then
  begin
   result := '<error open ' + err2str (GetLastError) + '>';
   exit;
  end;



 buff := nil;
 if maxLoad > 10 then
  try
   half := maxLoad div 2;
   result := '<cannot read ' + IntToStr(maxLoad) + ' bytes>';
   buff := AllocMem ( maxLoad + 65536 );

   fs.LowPart := GetFileSize (f, @fs.HighPart);
   // читать кусок с начала
   ReadFile (f, buff.data, maxLoad - half, rd, nil);
   if rd = 0 then exit;
   buff.dt_stamp := dt;
   // читать кусок с конца
   fp.QuadPart := Max (0, fs.QuadPart - half);
   if SetFilePointer (f, fp.LowPart, @fp.HighPart, FILE_BEGIN) = fp.LowPart then
      ReadFile (f, buff.data [rd], fs.QuadPart - fp.QuadPart, rd, nil);
   if rd > 0 then
    begin
     md := MD5Buffer (buff^, maxLoad + 8);
     result := HardMD5 (md, use_rand);
    end;


  finally
   FreeMem (buff);
  end
 else
  result := '<maxLoad = ' + IntToStr(maxLoad) + '>';


 CloseHandle (f);

end;



const
     TIMESTAMP_FMT = 'dd.mm.yyyy hh:nn:ss.zzz';

function CalcDirHash(path: String; dt: TDateTime; maxl: Integer; use_rand: Boolean): String;
var
   frec: TSearchRec;
   dir, code, r, s, fh: String;
   tmp: TStrMap;
   st: TSystemTime;
   md: TMD5Digest;
begin
 code := path;

 path := FmtStalkerPath (path);
 path := AddSlash (path);

 if not DirectoryExists (path) then
   begin
    ODS('[~D ~T]. #DBG(DumpDir): Directory not exists ~C0A' + path + '~C07');
    exit;
   end;

 DateTimeToSystemTime (dt, st); // округлить до миллисекунд
 dt := SystemTimeToDateTime (st);


 tmp := TStrMap.Create;

 dir := FormatDateTime ('[' + TIMESTAMP_FMT + ']. ', dt) + '#DIR_DUMP(' + code + '):'#13#10;


 code := dir;

 if FindFirst (path + '*.*', faAnyFile, frec) = 0 then
   repeat
    fh := '';
    if frec.Attr and faDirectory = 0 then
      begin
       if frec.Size > 0 then
          fh := FilePartsHash (dt, path + frec.Name, Min (frec.Size, maxl), use_rand );
       r := ' ' + frec.Name + ' ';
      end
     else
       r := ' [' + frec.Name + ']';

    while Length (r) < 25 do r := r + ' ';


    s := IntToStr (frec.Size) + ' bytes'#9;


    while Length (s) < 20 do s := ' ' + s;
    r := r + ' ' + s + ' ' + FormatDateTime ('dd.mm.yyyy hh:nn:ss', frec.TimeStamp) + ' ' + fh;

    code := code + Copy (r, 1, Min (90, Length (r)) ) + #13#10;

    tmp.Add (r);
   until FindNext (frec) <> 0;

 tmp.Sort;
 dir := dir + tmp.Text;
 tmp.Free;

 // ODS ('#CODE: '#13#10 + code);
 md := MD5StringW (code + 'REGISTRY_A');

 result := (dir + #13#10 + 'Summary signed by key = ' + HardMD5 (md, use_rand) + '§' );

end; // LuaDumpDir

function StrictKey (s: String): String;
const maxch = 35;

var
   i, io, ic: Integer;
begin
 result := s;
 io := Pos ('{', s);
 ic := Pos ('}', s);

 if (io > 0) and (ic > io + maxch) then
    begin
     for i := ic downto io do
       if result [i] = '-' then
          Delete (result, i, 1);
     // ODS( Format ('io = %d,  ic = %d, chx = ' + result [io + 33], [io, ic]) );
     io := io + 33;
     while ( io < Length (result) ) and ( result [io] <> '}' ) do
       Delete ( result, io, 1 );

     // Delete (result, io + 38, ic - io - 37  );
    end;

end;

function LuaCheckDir(L: lua_State): Integer; cdecl;
var
   dir, txt, ref, cut,  tt, err: String;
   maxl, i, sl: Integer;
   tmp, tmpr: TStrMap;
   dt: TDateTime;
   ts: TTimeStamp;

begin
 dir := LuaStrArg (L, 1);
 txt := LuaStrArg (L, 2);
 maxl := 1048576;

 if lua_gettop (L) >= 3 then
    maxl := lua_tointeger (L, 3);
 result := 0;

 if txt = '' then exit;

 tmp := TStrMap.Create;
 tmpr := TStrMap.Create;
 try
  tmp.Text := txt;
  cut := tmp [0];
  sl := Length (TIMESTAMP_FMT);
  dt := 0;
  err := '';

  if (Length (cut) > 20) and ( cut [1] = '[' ) and ( cut [sl + 2] = ']' ) then
   begin
    tt := Copy (cut, 2, sl);
    //
    {DateSeparator := '.';
    TimeSeparator := '.';
    ShortDateFormat := 'dd/mm/yyy';
    ShortTimeFormat := 'hh:mm:ss:zzz';}

    ts := StrDateTimeDecode (tt);
    dt :=  TimeStampToDateTime (ts);




    cut := FormatDateTime (TIMESTAMP_FMT, dt);
    if cut <> tt then
      begin
       err := err + 'Wrong time format!'#13#10;
       dt := 0;
      end;
   end;

  if dt > 0 then
    begin
     ref := CalcDirHash (dir, dt, maxl, FALSE);
     tmpr.Text := ref;
     for i := 0 to Min (tmp.Count, tmpr.Count) - 1 do
      begin
       tmp [i] := StrictKey ( tmp [i] );
       tmpr [i] := StrictKey ( tmpr [i] );
      end;

     txt := tmp.Text;
     ref := tmpr.Text;


     sl := 0;
     if ref <> txt then
       for i := 0 to Min (tmp.Count, tmpr.Count) - 1 do
        if tmp [i] <> tmpr [i] then
          begin
           ODS ('[~T]. #DBG: diff in line ' + IntToStr(i) + #13#10#9'~C0A' + tmp [i] + '~C0F vs ~C0E'#13#10#9 + tmpr [i] + '~C07' );
           Inc (sl);
          end;

     if sl = 0 then
        ODS('[~T]. #DBG: Directory ~C0A' + dir + '~C07 and files inside~C0E ==~C0F sample!~C07')

    end;

  if err <> '' then
     PrintError ('CheckDir errors: '#13#10 + err);


 finally
  tmp.Free;
 end;
end;

function LuaDumpDir(L: lua_State): Integer; cdecl;
var
   path, txt: String;
   maxl: Integer;
begin
 path := LuaStrArg (L);
 result := 1;
 maxl := 1048576;
 if lua_gettop(L) > 1 then
    maxl := Max (1024, lua_tointeger (L, 2));
 txt := CalcDirHash (path, Now, maxl, TRUE);
 ODS (txt);
 lua_pushwstr (L, txt);
end;



function LuaExitProcess (L: lua_State): Integer; cdecl;
var
   msg: String;
   err: Integer;
    tb: String;

begin
 tb := LuaTraceback (L, #9);
 msg := LuaStrArg (L);
 if msg = '' then
    msg := '""~C0C from ~C0F'#13#10 + tb;
 msg := AnsiReplaceStr (msg, '$traceback', tb);

 if  ( Pos('Aborted', msg) + Pos('FATAL', msg) > 0 )  and ( Pos('+FATAL', gLogFileRename [0] ) = 0 ) then
   try
    gLogFileRename [0] := AnsiReplaceStr(gLogFilename, '.log', '+FATAL.log');
    // ODS('[~T].~C0C #FATAL: );
    // Sleep(500);
    raise   Exception.Create('ExitProcess called with msg ~C0A' + msg + '~C07');
   except
    on E: Exception do
       OnExceptLog ('LuaExitProcess', E);
   end;

 result := 0;

 err := 0;
 if lua_gettop(L) > 1 then
    err := lua_tointeger (L, 2);

 try
  TerminateProcess(GetCurrentProcess, err);
 except
  on E: Exception do OutputDebugString ('shit!');

 end;
end;


procedure fast_hook(L: lua_State; var ar: lua_Debug); cdecl;
var
   src: String;
   lsr: lua_PStateRec;
   dr: lua_Debug;
begin
 FillChar (dr, sizeof(dr), 0);

 lsr := L;

 dr.event := ar.event;
 dr.currentline := ar.currentline;

 if lsr.errorJmp <> nil then exit;

 ODS('[~T]. #DBG: lua_State = ' + FormatPtr (L));

 if ar.event <> 0 then
   begin
    lua_getinfo (L, '>Sl', @dr);
    src := String (dr.short_src);
    if src <> last_src then
       trace_log.Add ( '>>' + src );
    last_src := src;
   end;

 trace_log.Add ( IntToStr (ar.currentline ) );

 if trace_log.Count > 1000 then
    trace_log.Delete (0);
end;

function LuaSetGameState (L: lua_State): Integer; cdecl;
begin
 game_L := L;
 result := 0;
end;

function LuaSetHook (L: lua_State): Integer; cdecl;
var
   bset: Boolean;
   r, argc, msk: Integer;
   dr: lua_Debug;
begin
 bset := FALSE;
 msk := LUA_MASKCALL;
 argc := lua_gettop (L);

 if argc > 0 then
    bset := lua_toboolean (L, 1);
 if argc > 1 then
    msk := lua_tointeger (L, 2);

 if bset then
    r := lua_sethook (L, fast_hook, msk, $ABC)
 else
    r := lua_sethook (L, fast_hook, 0, 0);

 ODS('[~T]. #DBG: lua_sethook returned ' + IntToStr(r));

 FillChar (dr, sizeof(dr), 0);
 lua_getinfo (L, '>Sl', @dr);

 result := 0;
end;

function LuaFoo (L: lua_State): Integer; cdecl;
begin
 result := 0;
 ODS ( '[~d ~T].~C0B #DBG: foo called with args-count =~C0D ' + IntToStr(lua_gettop(L)) + '~C07' );
end;

function LuaSoundMsg (L: lua_State): Integer; cdecl;
var
   msg: String;
   freq, time: Integer;
begin
 msg := LuaStrArg (L);
 freq := 1500;
 time := 1000;

 if lua_gettop (L) > 1 then freq := lua_tointeger (L, 2);
 if lua_gettop (L) > 2 then time := lua_tointeger (L, 3);


 StalkerODS(msg, $FF);
 PlayBeep (freq, time);
 result := 0;
end;

function PtInPolygon (L: lua_State): Integer; cdecl;
const
   COORD_RATIO = 100;
var
   ptsc: array of Integer;
   n, argc, ptcnt: Integer;
   rgn: HRGN;
   fp: Single;
   pt: TPoint;
   rval: Boolean;

begin
 result := 1;
 rval := FALSE;
 argc := lua_gettop (L);

 if argc >= 3 then
  repeat
   pt.X := Round ( COORD_RATIO * lua_tonumber (L, 1) );
   pt.Y := Round ( COORD_RATIO * lua_tonumber (L, 2) );

   if not lua_istable (L, 3) then break;
   ptcnt := lua_objlen (L, 3);
   if ptcnt < 6 then break;

   SetLength (ptsc, ptcnt);

   for n := 1 to ptcnt do
    begin
     lua_rawgeti (L, 3, n);
     fp := lua_tonumber(L, -1);
     ptsc [n - 1] := Round ( COORD_RATIO * fp );
     lua_pop (L, 1); // почистить стек от ключа/значения(?)
    end;

   ptcnt := ptcnt div 2;

   rgn := CreatePolygonRgn (ptsc[0], ptcnt, WINDING);
   if rgn <> 0 then
    begin
     rval := PtInRegion (rgn, pt.X, pt.Y);
     DeleteObject (rgn);
    end;


  until TRUE;

 SetLength (ptsc, 0);
 lua_pushboolean (L, rval);
end;





procedure BrakeState (L: lua_State);
var n: Integer;
    lsd: TLuaStateDesc;
    tls: PThreadLuaState;
begin
 captured.Lock('BrakeState');
 try
  for n := 0 to captured.Count - 1 do
   begin
    lsd := TLuaStateDesc ( captured.Objects [n] );
    if lsd = nil then continue;

    repeat
     tls := lsd.FindTLS (L);
     if Assigned (tls) then
      begin
       tls.Lchild := nil;
       tls.TID := 0;
      end;
    until tls = nil;
   end;

  PrintError('lua_State ' + FormatPtr (L) + ' marked as broken!');
 finally
  captured.Unlock;
 end;
end; // BrakeState

function AtPanicHandler (L: lua_State): Integer; cdecl;
var
   lsd: TLuaStateDesc;
   s, msg: String;
begin
 result := 0;
 try
   s := LuaStrArg (L);
   BrakeState (L);
   msg := '~C0C*************** AtPanicHandler ***************'#13#10;
   msg := msg + '[~T/' + Lvm_name(L) + ']. #LUA_ERROR: AtPanicHandler executed with message: ~C0A'#13#10 + FormatRight (s, '  ' ) + '~C07';
   StalkerODS(msg, $FF);
   DebugDumpAll;
   gToolThread.AddRequest('BEEP');

   lsd := FindStateDesc (L, FALSE);
   if lsd = nil then exit;

   upd_timeout := 10000;
   if IsDebuggerPresent then
     asm
     end
  else
     if Assigned (lsd.old_panicf) and (@lsd.old_panicf <> @AtPanicHandler) then
        result := lsd.old_panicf(L);


 finally
   FlushLog();
 end;
end; // AtPanicHandler


function CheckAtPanic (L: lua_State; pf: lua_CFunction): Boolean;
var
   p: PPointer;
begin
 p := RelativePtr (L, $14);
 p := RelativePtr (p^, $58);
 result := p^ = Addr(pf);
end;


function SetAtPanicHandler (L: lua_State): Integer; cdecl;
var
   oldf: lua_CFunction;
   lsd: TLuaStateDesc;
begin
 result := 0;
 lsd := FindStateDesc (L, TRUE);
 if lsd <> nil then
  begin
   oldf := lsd.old_panicf;
   if Addr (oldf) = Addr (AtPanicHandler) then exit;
  end;
 ODS( Format('[~T]. #DBG: Setting AtPanic handler for lua_State %s', [LVM_name(L)]) );
 try
  oldf := lua_atpanic (L, AtPanicHandler);
  CheckAtPanic (L, AtPanicHandler);
  if ( lsd <> nil ) and ( @oldf <> @AtPanicHandler ) then lsd.old_panicf := oldf;
 except
  on E: Exception do
    PrintError ('Exception catched in SetAtPanicHandler: ' + E.Message);
 end;

end;

function NotifyReleased (L: lua_State): Integer; cdecl;
var
  ctx: String;
    e: Boolean;
    i: Integer;

begin
 ctx := '';
 i := lua_tointeger (L, 1);
 if lua_gettop(L) > 1 then
    ctx := LuaStrArg(L, 2);

 e := FALSE;
 if lua_gettop(L) > 2 then
    e := lua_toboolean (L, 3);

 OnObjectRelease (i, ctx, e );
 result := 0;
end;

function NotifySpawned (L: lua_State): Integer; cdecl;
var
   se, cl: PByteArray;
begin
 se := lua_topointer (L, 1);
 cl := nil;
 if lua_gettop (L) > 1 then
    cl := lua_topointer (L, 2);
 OnSpawn (se, cl);
 result := 0;
end;

function NearestVertexInfo (L: lua_State): Integer; cdecl;
var
   rq: TVxRequest;
   rr: PVxRequest;
   pe: PAQEvents;
   ac: Integer;
   tp: Integer;
   fn: String;
    i: Integer;



begin
 // loading vector
 rq.init (10000);
 ac := lua_gettop(L);

 tp := 100;

 i := 1;
 if lua_type(L, i) = LUA_TSTRING then
  begin
   fn := LuaStrArg ( L, i );
   SetStrZ ( rq.src_file, FmtStalkerPath(fn), 999 );
   Inc (i);
  end;


 if lua_isnumber (L, i) then
   with rq.vectors [0] do
    begin
     x := lua_tonumber (L, i + 0);
     y := lua_tonumber (L, i + 1);
     z := lua_tonumber (L, i + 2);
     tp := i + 3;
    end
 else
 if lua_type(L, i) in LUA_ANY_USERDATA then
    begin
     rq.vectors[0].import (L, i);
     tp := i + 1;
    end;

 if ( ac >= tp ) and lua_toboolean (L, tp) then
        rq.i_params [3] := 1;

 if gIPCQueue = nil then
    FindNearestVertex ( @rq )
 else
    rq := FindNearestVxAsync ( @rq, rq.i_params[3] <> 0 );


 result := 3;

 lua_pushinteger ( L, rq.lv_ids [0] );
 lua_pushinteger ( L, rq.gv_ids [0] );
 lua_pushinteger ( L, rq.lv_ids [1] );
end;

function LoadVertexList (L: lua_State): Integer; cdecl;
var
     fn: String;
     rq: TVxRequest;
     rr: PVxRequest;
     pe: PAQEvents;

begin
 result := 1;
 fn := FmtStalkerPath( LuaStrArg(L) );

 if not FileExists (fn) then
  begin
   lua_pushwstr ( L, '#FATAL: not exists file: ' + fn );
   exit;
  end;


 if current_lvf = fn then
    begin
     lua_pushwstr ( L, '#OK' );
     exit;
    end;

 if current_lvl <> nil then
   begin
    FreeMem (current_lvl);
    current_lvl := nil;
   end;

 if gIPCQueue = nil then
   begin
    if not LoadVertices (fn) then
     begin
      lua_pushwstr ( L, '#FATAL: LoadVertices failed in sync call' );
      exit;
     end;

    result := 5;
    lua_pushwstr ( L, '#OK' );
    with current_lvl^ do
     begin
      lua_pushnumber (L, min_x); lua_pushnumber (L, min_z);  lua_pushnumber (L, max_x);  lua_pushnumber (L, max_z);
     end;


   end
 else
   begin
    rq.Init (100);
    SetStrZ ( rq.src_file, fn, 250 );
    ODS('[~T]. #DBG: trying async load file ' + String (rq.src_file) );

    rr := gIPCQueue.PushDataRqs ('LOAD_VERTICES', rq, sizeof(rq) );
    pe := gIPCQueue.GetEvents; Assert ( rr <> nil, 'Cannot push data request' );
    while ( rr.tick_timeout > 0 ) do
            WaitForSingleObject (pe.FreeEvent, 100);

    result := 5;
    if rr.rq_error = '' then
     begin
      lua_pushwstr ( L, '#OK' );
      with rr^ do
       begin
        lua_pushnumber (L, vectors[0].x ); lua_pushnumber (L, vectors[0].z );
        lua_pushnumber (L, vectors[1].x ); lua_pushnumber (L, vectors[1].z );
       end;
     end
    else
     begin
      lua_pushwstr ( L, '#FATAL: ' + String(rr.rq_error) );
      result := 1;
     end;

    gIPCQueue.ReleaseData (rr);
   end;

end;


function SaveVertexInfo (L: lua_State): Integer; cdecl;

const
    BLOCK_SIZE = 128 * 1024;
// params (2): cmd, name
// params (5), gvid, lvid, x, y, z

var
   argc: Integer;
   gvid: Integer;
   lvid: Integer;
   size: Integer;
   fobj: TFileStream;
    lvx: TLevelVertex;
    cmd: String;
     fn: String;
     hf: THandle;
     tt: Integer;

begin
 result := 0;
 argc := lua_gettop (L);
 fobj := nil;

 tt := lua_type (L, 1);


 case tt of
  LUA_TSTRING: // command mode
     begin
      cmd := LowerCase ( LuaStrArg (L, 1) );

      if cmd = 'create file' then
         try
          fn := FmtStalkerPath(LuaStrArg(L, 2));
          CheckMakeDir (ExtractFilePath(fn));

          if FileExists (fn) then
             DeleteFile (fn);


          if current_lvl <> nil then
             FreeMem (current_lvl);

          current_lvl := AllocMem ( BLOCK_SIZE );
          current_lvl.mb_size := BLOCK_SIZE;

          SetLastError (0);
          hf := CreateFile ( PChar(fn), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0 );
          if hf = INVALID_HANDLE_VALUE then
             PrintError (' CreateFile ' + fn + ' failed: ' + err2str )
          else
             fobj := TFileStream.Create ( hf ); // fn, fmCreate or fmShareDenyWrite

          current_lvl.svctx := fobj;
         except
          on E: EFCreateError do;
         end;

      if cmd = 'close file' then
         begin
          current_lvl.Flush;

          fobj := current_lvl.svctx;
          FreeAndNil (fobj);
          // FreeMem (current_lvl);
          current_lvl.svctx := nil;
          current_lvf := '';
         end;

      if cmd = 'vertex count' then
         begin
           current_lvl.count := lua_tointeger (L, 2);
           current_lvl.last_lv := current_lvl.count - 1;
           wprintf('[~T]. #DBG: level vertices count limited to %d ', [current_lvl.count]);
         end;
     end; // 2 args

   LUA_TNUMBER:
    if current_lvl <> nil then
     try
      gvid := lua_tointeger (L, 1);
      lvid := lua_tointeger (L, 2);

      tt := lua_type (L, 3);
      if ( tt = LUA_TNUMBER ) and ( argc >= 5 ) then
         begin
          lvx.pos.init( lua_tonumber (L, 3), lua_tonumber (L, 4), lua_tonumber (L, 5) );
          if not lvx.pos.check_bound ( -1500, 1500 ) then
             begin
              wprintf ('[~T].~C0C #ERROR:~C07 bad vertex info  gvid = %d, lvid = %d, pos = { %.3f, %.3f, %3f } assigned to { 0, 0, 0 } ',
                        [gvid, lvid, lvx.pos.x, lvx.pos.y, lvx.pos.z] );
              lvx.pos.init (0, 0, 0);
             end;
         end
      else
      if ( tt in LUA_ANY_USERDATA ) then
         begin
          lvx.pos.import (L, 3);
         end;

      with current_lvl^ do
        if count = 0 then
         begin
          min_x := lvx.pos.x;
          min_z := lvx.pos.z;
          max_x := lvx.pos.x;
          max_z := lvx.pos.z;
         end
        else
         begin
          min_x := Min (min_x, lvx.pos.x);
          min_z := Min (min_z, lvx.pos.z);
          max_x := Max (max_x, lvx.pos.x);
          max_z := Max (max_z, lvx.pos.z);

          if gvid <> $FFFF then
             last_lv := Max (last_lv, lvid);

         end;


      current_lvl.count := Max (current_lvl.count, lvid + 1); // check -> extend

      size := current_lvl.CalcSize;

      // extending buffer if need
      while ( size > current_lvl.mb_size ) do
        begin
         current_lvl.mb_size := current_lvl.mb_size + BLOCK_SIZE;
         ReallocMem (current_lvl, current_lvl.mb_size);
        end;

      lvx.gvid := gvid;
      lvx.lv16 := lvid and $FFFF;  // справочный индекс, полный восстанавливается через адрес
      current_lvl.list [lvid] := lvx;


     except
       on E: Exception do
           OnExceptLog('SaveVertexInfo#5', E);
     end; // (5 args)
 end; // case


end;


function LockObject(L: lua_State): Integer; cdecl;
var
   need_lock: Boolean;
        name: String;
          cs: TCritSection;

begin
 need_lock := TRUE;
 result := 0;

 if ( lua_gettop(L) < 1 ) or not Assigned (lock_objs) then exit;

 name := Trim ( LuaStrArg (L) );

 if name = '' then exit;

 lock_objs.Lock('GetAdd');
 try

  cs := TCritSection ( lock_objs.FindObject(name) );
  if cs = nil then
    begin
     cs := TCritSection.Create(name);
     lock_objs.AddObject(name, cs)
    end;

 finally
  lock_objs.Unlock;
 end;

 if lua_gettop (L) >= 2 then need_lock := lua_toboolean (L, 2);

 if need_lock then
    cs.Lock('LockObject')
 else
    cs.Unlock;

end;


function GetThreadAsyncVar(L: lua_State): Integer; cdecl;
var
   argc: Integer;
    cmd: String;
     ht: THelperThread;
     ts: TTableStore;
begin
 result := 1; // table or string typically
 argc := lua_gettop (L);

 if argc < 2 then
   begin
    lua_pushwstr (L, '#FATAL: Need two or more arguments');
    exit;
   end;

 ht := lua_topointer (L, 1);
 if ht = nil then
   begin
    lua_pushwstr (L, '#FATAL: 1-st argument must be THelperThread object');
    exit;
   end;

 cmd := Trim ( LuaStrArg (L, 2) );

 // meta-commands
 if cmd = '~ALL' then
   begin
    ht.WorkGlobals.Lock('GetText');
    lua_pushwstr (L, ht.WorkGlobals.CommaText);
    ht.WorkGlobals.Unlock;
    exit;
   end;

 ht.WorkGlobals.Lock('GetResult');
 ts := TTableStore ( ht.WorkGlobals.FindObject(cmd) );
 // lua_pushwstr (L, ht.WorkGlobals.CommaText);
 ht.WorkGlobals.Unlock;

 if ts = nil then
   begin
    lua_pushwstr (L, '#WARN: no result with key ' + cmd);
    exit;
   end;

 ts.LuaExport (L);

end; // GetThreadAsyncVar


function SetThreadAsyncVar(L: lua_State): Integer; cdecl;
var
  rname: String;
   argc: Integer;
     ts: TTableStore;
     ht: THelperThread;

begin
 result := 1;

 argc := lua_gettop(L);

 if (argc < 3)  then
    begin
     lua_pushwstr (L, '#FATAL need 3+ arguments');
     exit;
    end;

 ht := lua_topointer(L, 1);

 if (ht = nil) or not (ht is THelperThread) then
    begin
     lua_pushwstr (L, '#FATAL: 1st arg must be THelperThread object ~= nil ');
     exit;
    end;

 rname := Trim ( LuaStrArg(L, 2) );

 if rname = '' then
    begin
     lua_pushwstr (L, '#FATAL: 2nd arg must be var name ~= "" ');
     exit;
    end;


 if lua_type (L, 3) <> LUA_TTABLE then
    begin
     lua_pushwstr (L, '#FATAL: 3rd arg must be table');
     exit;
    end;

 ht.WorkGlobals.Lock('GetSetVar');
 try
  ts := TTableStore ( ht.WorkGlobals.FindObject(rname) );

  if ts = nil then
    begin
     ts := TTableStore.Create;
     ht.WorkGlobals.AddObject(rname, ts);
    end;

  ts.LuaImport(L, 3);

 finally
  ht.WorkGlobals.Unlock;
 end;

 lua_pushwstr (L, 'OK');
end; // SetThreadAsyncVar


function ManageThreads(L: lua_State): Integer; cdecl;
var
  argc: Integer;
   cmd: String;
    sv: String;
    ht: THelperThread;
     i: Integer;
begin
 argc := lua_gettop (L);
 result := 0;
 if (argc < 2) or (ht_list = nil) then exit;

 cmd := LuaStrArg (L);

 if cmd = 'start' then
   begin
    result := 1;
    sv := Trim ( LuaStrArg (L, 2) );

    if sv = '' then
      begin
       lua_pushwstr (L, '#FATAL: thread name is required (2nd arg)');
       exit;
      end;
    ht := THelperThread ( ht_list.FindObject(sv) );
    if ht = nil then
     begin
      ht := THelperThread.Create(FALSE, sv);
      ht.WaitStart();
      ht_list.AddObject(sv, ht);
     end;
   lua_pushptr (L, ht);
  end; // start_new

 if (cmd = 'load_script') and (argc >= 2) then
   begin
    ht := lua_topointer (L, 2);
    if ht = nil then exit;
    sv := LuaStrArg(L, 3);
    if sv = '' then exit;
    ht.FScriptLines.Text := sv;
    ht.AddRequest('LOAD_SCRIPT');
   end;


 if (cmd = 'exec_script') and (argc >= 2) then
   begin
    ht := lua_topointer (L, 2);
    if ht = nil then exit;
    ht.AddRequest('EXEC_SCRIPT');
   end;

 if (cmd = 'request') and (argc >= 3) then
   begin
    ht := lua_topointer (L, 2);
    if ht = nil then exit;
    sv := LuaStrArg(L, 3);
    if sv = '' then exit;
    ht.AddRequest(sv);
   end;


 if (cmd = 'stop') then
   begin
    ht := lua_topointer (L, 2);
    if ht = nil then exit;
    ht.StopThread();
    i := ht_list.IndexOfObject(ht);

    if i >= 0 then
       ht_list.Delete(i)
    else
       ht.Free;
   end;

 if (cmd = 'dump') then
   begin
    result := 1;
    sv := LuaStrArg (L, 2);
    // TODO: here can be added busy, unused & etc
    if sv = 'all' then
       lua_pushwstr (L, ht_list.CommaText)
    else
       lua_pushwstr (L, 'bad second arg. for cmd <dump>');
   end;
end;

function SwitchKeyboardLayout (L: lua_State): Integer; cdecl;
var
   s, r: String;
   msg: tagMSG;
begin
 result := 1;
 s := LuaStrArg (L);
 SetLength (r, KL_NAMELENGTH);
 GetKeyboardLayoutName ( PWideChar(r) );
 r := Trim (r);
 lua_pushwstr (L, r);

 if s = 'ru' then ActivateKeyboardLayout (kbd_ru, KLF_SETFORPROCESS);
 if s = 'en' then ActivateKeyboardLayout (kbd_en, KLF_SETFORPROCESS);

 Sleep(2);
 while PeekMessage (msg, 0, 0, 0, PM_REMOVE) do
  begin
   TranslateMessage (msg);
   DispatchMessage(msg);
  end;
end;



function LuaVkArg (L: lua_State; idx: Integer = 1): BYTE;
var s: String;
begin
 result := $FF;
 if lua_gettop (L) > 0 then
   if lua_type(L, idx) = LUA_TSTRING then
      begin
       s := LuaStrArg (L, idx) + ' ';
       result := Ord (s [1]);
      end
    else
       result := lua_tointeger (L, idx) and $FF;
end; // LuaVkArg

function LuaAssertIgnore (L: lua_State): Integer; cdecl;
var
  err: String;
    i: Integer;
    s: AnsiString;
begin
 err := '';
 result := 1;

 try
  ignore_assert := TRUE;
  s := AnsiString ( LuaStrArg (L) );
  i := luaL_loadstring (L, PAnsiChar(s) );
  if i = 0 then
     i := lua_pcall ( L, 0, LUA_MULTRET, 0 );

  if i <> 0 then
     err := Format('luaL_loadstring failed %d: %s', [i, LuaStrArg(L, -1) ] );

 except
  on E: Exception do
     err := 'EXCEPTION ignored ' + E.ClassName + ': ' + E.Message ;
 end;

 if err <> '' then
    ODS ('[~T].~C0C #WARN: ~C07' + err);
 ignore_assert := FALSE;
 lua_pushwstr(L, err);
end;

function LuaKeyPressed (L: lua_State): Integer; cdecl;
var
   k, back: WORD;
begin
 k := LuaVkArg (L);
 back := 1;
 result := 1;
 if lua_gettop (L) > 1 then
    back := lua_tointeger (L, 2);

 lua_pushboolean (L, KeyPressedEx (k, back));
end;

function LuaKeyCombPressed (L: lua_State): Integer; cdecl;
var
   k1, k2, k3, back: WORD;
begin
 k1 := LuaVkArg (L, 1);
 k2 := LuaVkArg (L, 2);
 k3 := LuaVkArg (L, 3);
 back := 1;
 result := 1;
 if lua_gettop (L) > 3 then
    back := lua_tointeger (L, 4);

 lua_pushboolean (L, KeyCombPressed (k1, k2, k3, back));
end;


function LuaModuleInfo (L: lua_State): Integer; cdecl;
var
   fv, fn, mName, release: String;
begin
 mName := LuaStrArg (L, 1);
 release := '?';

 result := 1;
 lua_createtable (L, 0, 0);
 lua_setmap_p ( L, 'base_addr', Ptr ( GetModuleHandle (mName) ) );
 lua_setmap_i ( L, 'size',      ModuleSize ( mName ) );

 fn := ExePath + mName;
 if version_map = nil then
   begin
    version_map := TStrMap.Create();
    version_map.QuoteChar := '"';
    add_garbage (version_map);
   end;

 fv := version_map.Values[fn];
 if fv = '' then
   begin
    fv := GetFileVersionStr ( fn );
    version_map.values[fn] := fv;
   end;

 lua_setmap_s ( L, 'version', fn);

 mName := LowerCase (mName);
 if mName = 'xrgame.dll' then
    release := DetectXrGame;

 lua_Setmap_s ( L, 'release',   release );

end;

function LuaModuleSize (L: lua_State): Integer; cdecl;
var
   m: String;
   i: Integer;
begin
 m := LuaStrArg (L);
 i := ModuleSize (m);
 if i < 0 then
    PrintError('module "' + m + '" was not loaded now');
 lua_pushinteger (L, i);
 result := 1;
end;

function LuaMousePos (L: lua_State): Integer; cdecl;
var
   pt: TPoint;
begin
 GetCursorPos (pt);
 lua_pushnumber (L, pt.x);
 lua_pushnumber (L, pt.y);
 result := 2;
end;

function LuaReadln (L: lua_State): Integer; cdecl;
var
   s: String;
   msg: tagMSG;
begin

 if dbg_present then
 while not KeyPressedEx (VK_RETURN) do
  begin
   if PeekMessage (msg, 0, 0, 0, PM_REMOVE) then
     begin
      TranslateMessage (msg);
      DispatchMessage (msg);
     end;
   Sleep(50);
  end;

 PlayBeep (1500, 100);
 ReadLn(s);
 lua_pushwstr (L, s);
 result := 1;
end;



function LuaCalcPtr (L: lua_State): Integer; cdecl;
var
   values: array [1..8] of NativeUInt;
   addr: DWORD;
   n, cnt: Integer;
   s, op: String;
    cast: String;

begin
 cnt := lua_gettop(L);

 FillChar(values, sizeof(values), 0);

 cnt := Min(cnt, 9); /// strict argcnt

 s := '';
 cast := '@ptr';
 if cnt > 2 then
   begin
    s := LuaStrArg (L, cnt);
    if Pos('@', s) > 0 then
      begin
       cast := s;
       Dec (cnt); // -1 one operand
      end;
   end;


 for n := 1 to Min(2, cnt - 1) do
   begin
    values [n] := LuaAddr (L, n);
    s := s + ' $' + IntToHex (values[n], 4);
   end;

 op := LuaStrArg (L, cnt) + '  ';

 if log_verbose > 7 then
   ODS( Format('[~T]. #DBG(CalcPtr): calc operation~C0B %s~C07 for ~C0F%s~C07', [op, s]));


 addr := values[1];

 for n := 2 to cnt - 1 do
 case op[1] of
  '-': addr := addr -   values[n];
  'x': addr := addr xor values[n];
  'a': addr := addr and values[n];
  '~',
  'n': addr := not addr;
  '@': { nope };  // ignore cast
  's': if op[2] = 'r' then
          addr := addr shr values[n]
       else
          addr := addr shl values[n];


     else addr := addr + values[n];
 end;

 if addr = 0 then
   lua_pushnil (L)
 else
  begin
   if cast = '@int' then
      lua_pushinteger (L, addr)
   else
     begin
      lua_pushlightuserdata (L, Ptr(addr));
      SetPointerMT (L, -1);
     end;
  end;


 result := 1;
end;

function LuaCastToInt (L: lua_State): Integer; cdecl;
var
   r: Int64;
   u: UInt64 absolute r;
   f: Boolean;
begin
 result := 1;
 r := 0;
 f := lua_toboolean (L, 2);

 case lua_type (L, 1) of
   LUA_TBOOLEAN, LUA_TNUMBER:
       r := lua_tointeger (L, 1);
   LUA_TSTRING:
       r := atoi ( LuaStrArg (L) );
   LUA_TLIGHTUSERDATA:
       r := Int64 ( lua_topointer (L, 1) );
   LUA_TUSERDATA:
       r := Int64 ( lua_touserdata (L, 1) );
  end;

 if f then
   lua_pushnumber (L, u)
 else
   lua_pushnumber (L, r);
end;

function LuaCastToPtr (L: lua_State): Integer; cdecl;
var
   p: Pointer;
begin
 result := 1;
 case lua_type(L, 1) of
  LUA_TUSERDATA, LUA_TLIGHTUSERDATA:
      p := lua_topointer (L, 1);
  else
      p := Ptr ( LuaDWORD (L, 1) );
 end;
 lua_pushptr (L, p);
end;


function LuaCObjectPtr (L: lua_State): Integer; cdecl;
type
    TClientObjectWrapper = packed record
     unk: Pointer;
     obj: Pointer;
    end;

var
   p: ^TClientObjectWrapper;
begin
 result := 1;
 p := nil;
 case lua_type(L, 1) of
  LUA_TUSERDATA, LUA_TLIGHTUSERDATA:
     begin
      p := lua_topointer (L, 1); // direct cast
      if p <> nil then
         p := p.unk;
     end;
 end;

 if p <> nil then
    lua_pushptr (L, p.obj)
 else
    lua_pushnil (L);
end;

function LuaSObjectPtr (L: lua_State): Integer; cdecl;

var
   p: PPointer;
begin
 result := 1;
 p := nil;
 case lua_type(L, 1) of
  LUA_TUSERDATA, LUA_TLIGHTUSERDATA:
      p := lua_topointer (L, 1); // direct cast
 end;

 if p <> nil then
    lua_pushptr (L, p^)
 else
    lua_pushnil (L);
end;


function LuaObjectInfo (L: lua_State): Integer; cdecl;
var
   gobj: TGameObject;
    top: Integer;
     id: DWORD;

begin
 result := 1;
 id := lua_tointeger (L, 1);
 if id >= 65535 then
  begin
   lua_pushnil (L);
   exit;
  end;

 gobj := RootRegistry.Find (id);

 if ( gobj = nil ) then
    // дополнительный поиск мяса в удаленных объектах
      gobj := FindReleased (id);

 if gobj = nil then
  begin
   lua_pushnil (L);
   exit;
  end;

 lua_createtable (L, 0, 0);
 top := lua_gettop (L);
 lua_setmap_s ( L, 'name',      gobj.name,    top );
 lua_setmap_s ( L, 'section',   gobj.section, top );
 lua_setmap_s ( L, 'visual',    gobj.visual,  top );

 lua_pushptr  ( L, gobj.cl_addr(0) );
 lua_setfield ( L, top, 'cl_object' );
 lua_pushptr  ( L, gobj.se_addr(0) );
 lua_setfield ( L, top, 'se_object' );
 lua_setmap_b ( L, 'exists',    gobj.is_exists, top );
 lua_setmap_b ( L, 'online',    gobj.is_online, top );
 lua_setmap_i ( L, 'lvid',      gobj.lvid, top );
 lua_setmap_i ( L, 'gvid',      gobj.gvid, top );
 lua_setmap_f ( L, 'pos_x',     gobj.position.x, top );
 lua_setmap_f ( L, 'pos_y',     gobj.position.y, top);
 lua_setmap_f ( L, 'pos_z',     gobj.position.z, top);
end;

function LuaSetPointerMT(L: lua_State): Integer; cdecl;
begin
 if lua_isuserdata (L, 1) or lua_islightuserdata (L, 1) or lua_iscfunction(L, 1) then
    SetPointerMT (L, 1)
 else
    PrintError(' SetPointerMT: not applicable type ' + lua_typeof(L, 1) );
 result := 0;
end;

function LuaTBHash (L: lua_State): Integer; cdecl;
var
   md: TMD5Digest;
    s: String;
begin
 s := LuaTraceBack ( L, '' );
 md := MD5StringW (s);
 result := 1;
 lua_pushwstr ( L, MD5DigestToStr(md) );
end;

function LuaTracePtr (L: lua_State): Integer; cdecl;
var
   s, t: String;
   addr, ofs: Int64;
   loop: Integer;
   rel: Boolean;
   pdw: PDWORD;

begin
 s := LuaStrArg (L, 1);

 addr := 0;
 loop := 0;

 while (s <> '') and (loop < 32) do
  begin
   if Pos ('^', s) > 0 then
       t := StrTok (s, ['^'])
   else
     begin
       t := s;
       s := '';
     end;

   ofs := 0;
   rel := ( loop > 0 );

   if t <> '' then
     begin
      if Pos('.', t) > 0 then
         ofs := GetModuleHandle ( PChar (t) )
      else
      if Pos(':', t) > 0 then
       begin
        t := AnsiReplaceStr (t, ':', '');
        ofs := DWORD ( GetProcAddress (addr, PChar(t) ) );
        rel := FALSE; // absolute addr
       end
      else
       ofs := atoi (t);
     end;


   pdw := nil;

   if rel then
     try
      pdw := Ptr (addr + ofs);
      addr := pdw^;
     except
      on E: Exception do
         PrintError ( Format('exception catched in LuaTracePtr, pdw = $%p : ', [pdw]) + E.Message );
     end
   else
      addr := ofs; // absolute




   Inc (loop);
  end;


 lua_pushlightuserdata (L, Ptr(addr));
 result := 1;
end;


procedure lua_pushptr(L: lua_State; p: Pointer);
begin
 if DWORD(p) > $1000 then
    lua_pushlightuserdata (L, p)
 else
    lua_pushnil (L);
end;




function LuaRayPick (L: lua_State): Integer; cdecl;
// int CObjectSpace::RayPick(CObjectSpace *this, _vector3<float> *start, _vector3<float> *dir, float range, collide::rq_target tgt, collide::rq_result *R, CObject *ignore_object)

var
   rq_result: packed record
               pGameObj: PByteArray;
                  range: Single;
                element: Integer;
              end;
        argc: Integer;
        gobj: TGameObject;
       start: TVector3F;
         dir: TVector3F;
         rng: Single;
         tgt: Integer;
         pig: Pointer;


begin
 argc := lua_gettop (L);
 result := 0;

 if ( g_pGameLevel = nil ) or ( _RayPick = nil ) or ( argc < 3 ) then exit;

 result := 3;
 FillChar (rq_result, sizeof(rq_result), 0);

 start.import (L, 1);
 dir.import (L, 2);
 rng := lua_tonumber (L, 3);

 pig := nil; // ignore_object
 gobj := nil;

 if argc > 3 then tgt := lua_tointeger (L, 4);
 if argc > 4 then gobj := RootRegistry.Find  ( lua_tointeger (L, 5) );
 if gobj <> nil then pig := gobj.cl_addr (0);

 try
  asm
   pushad
   push  pig                  // 5. ignore_object
   lea   eax, rq_result
   push  eax                  // 4. rq_result
   push  tgt
   push  rng                  // 3. range
   lea   ecx, dir
   lea   edx, start
   push  ecx                  // 2. dir
   push  edx                  // 1. start
   mov   eax, [g_pGameLevel]  // address in xr3da.exe
   mov   eax, [eax]
   lea   ecx, [eax + $CC]
   mov   eax, _RayPick
   call  eax
   popad
  end;

  tgt := WRONG_IDX;
  if rq_result.pGameObj <> nil then
     tgt := PWORD ( @rq_result.pGameObj [g_offsets.id_word] )^;

  lua_pushinteger ( L, rq_result.element ); // lvid
  lua_pushnumber  ( L, rq_result.range );   // range
  lua_pushinteger ( L, tgt );               // object
 except
  on E: Exception do
     OnExceptLog ('LuaRayPick', E, FALSE);
 end;


end;


function LuaCopyDMA (L: lua_State): Integer; cdecl;
var
           src, dst, cb: DWORD;
             psrc, pdst: Pointer;
                   argc: Integer;
begin
 argc := lua_gettop(L);
 result := 1;

 if argc < 3 then
   begin
    lua_pushwstr (L, '#ERROR: need 3 arguments');
    exit;
   end;

 dst := LuaDWORD (L, 1);
 src := LuaDWORD (L, 2);
 cb  := LuaDWORD (L, 3);

 pdst := Ptr ( dst );
 psrc := Ptr ( src );
 try
  CopyMemory (pdst, psrc, cb);
 except
  on E: EAccessViolation do
     PrintError ('CopyDMA: AV catched in CopyMemory ' + E.Message);
 end;
end;


var
   sym_init: Boolean = FALSE;

function GetCodePtrInfo( p: Pointer; var suffix: String ): String;
var
    disp: UInt64;
     sym: PIMAGEHLP_SYMBOL64;
    line: TImageHlpLine64;
     pid: DWORD;
       c: Size_T;
begin
  result := '';
  if not Assigned (SymGetSymFromAddr64) then exit;

  pid  := GetCurrentThreadId;
  disp := 0;
  c := 1024;
  sym := AllocMem (c);
  sym.SizeOfStruct  := c;
  sym.MaxNameLength := ( c - sizeof(TSymbolInfo) ) - 1;
  SetLastError(0);

  if not sym_init then
    begin
     SymSetOptions ( SYMOPT_UNDNAME or
                     SYMOPT_LOAD_LINES or
                     SYMOPT_DEFERRED_LOADS or
                     SYMOPT_LOAD_ANYTHING or
                     SymGetOptions ( ) );
     sym_init := SymInitializeW ( pid, PChar (ExePath), TRUE );
    end;

  if not sym_init then exit;

  if SymGetSymFromAddr64 ( pid, UInt64(p), @disp, sym ) then
    begin
     suffix := '[OK]';
     result := String ( PAnsiChar ( @sym.Name ) );
     if disp > 0 then
        result := result + ' + ' + IntToStr(disp)
    end
  else
     suffix := #9'?';

  FillChar (line, sizeof(line), 0);
  line.SizeOfStruct := sizeof (line);

  disp := 0;

  if Assigned (SymGetLineFromAddr64) and
      SymGetLineFromAddr64 ( pid, UInt64(p), @disp, @line ) then
    begin
     if result <> '' then
        while Length(result) < 30 do result := result + ' ';
     result := result + ' at ' + line.FileName + ':' + IntToStr(line.LineNumber) ;
     if disp > 0 then
        result := result + ' + ' + IntToStr(disp);
    end
  else
     suffix := suffix + #9'?';

  if Length (result) > 5 then exit;
end;


function GetPtrInfo ( p: Pointer ): String;
var
   s, sp, mp: String;
   ex, found: DWORD;
     a, b, c: Integer;
    code_ptr: Boolean;
      suffix: String;
        buff: array [0..4095] of AnsiChar;
        pinf: TPtrInfo;

         mbi: TMemoryBasicInformation;
      // ofs: DWORD;  map: TMapFile; pub: TMfPublic;
          mm: TModuleMap;
          hp: THandle;


begin
 s := '';
 result := 'nil';
 suffix := '';

 if p = nil then exit;

 sp := '$' + IntToHex ( DWORD(p), 8 ); // default format
 mp := PtrToModPtr (p);

 FillChar (mbi, sizeof(mbi), 0);
 VirtualQuery (p, mbi, sizeof(mbi));

 code_ptr := ( mbi.Protect            and PAGE_EXEC_ACCESS <> 0 ) or
             ( mbi.AllocationProtect  and PAGE_EXEC_ACCESS <> 0 );

 if code_ptr and (Pos('luaicp.dll', mp) = 0) then
   begin
    result := GetCodePtrInfo (p, suffix);
    if (Length(result) > 5) and (Pos('.dll', result) = 0) then exit;
    if Assigned(GetFunctionInfo) then
      begin
       ex := 0;
       FillChar(buff, sizeof(buff), 0);
       GetFunctionInfo (p, buff, @ex);
       result := AnsiTrim2W(buff);
       if ex > 0 then
          result := result + ' + $' + IntToHex(ex, 4);
      end;
   end;

 mm := gToolThread.FindPtrMap (p);
 if mm <> nil then
  begin
   result := mm.PtrInfo (p, 0, 65535);
   if result <> '' then
      begin
       // IfV(code_ptr, 'CODE_PTR: ', 'DATA_PTR: ') +
       result := result;
       exit;
      end;
  end;

 if RootRegistry(FALSE) <> nil then
  begin
   result := RootRegistry.FindObjectByPtr ( p, 0, $500 );
   if result <> '' then exit;
  end;


 {
 me := gToolThread.ModuleByPtr (p);

 map := nil;
 if me.modBaseAddr <> nil then
    map := madMapFile.FindMapFile ( Ptr(me.hModule) );


 if (map <> nil) and map.IsValid then
  begin
   pub := map.FindPublic ( p );
   if (pub.IsValid) and (pub.Name <> '') then
     begin
      result := pub.Name;
      ofs := DWORD(p) - DWORD(pub.Address);
      if ofs > 0 then result := result + ' + $' + IntToHex (ofs, 2);
      exit;
     end;
  end
 else
  FreeAndNil (map); // }


 result := mp + suffix;

 if ( ptr_infos = nil ) or ( ptr_obj = nil ) or ( ptr_infos.Count = 0 ) then exit;

 a := -1;
 b := -1;


 ptr_obj.PK := p;
 c := ptr_infos.Find (ptr_obj, nil, @a, @b);   // найти ближайшие два контейнера

 if c >= 0 then a := c;

 if a < 0 then exit;

 pinf := TPtrInfo ( ptr_infos [a] );

 found := DWORD ( pinf.PK );

 ex := DWORD ( p );

 // если указатель найден в стандартных блоках
 if ex = found then
   begin
    result := '[' + sp + ']::' + pinf.Value;
    exit;
   end;

 result := TestObjectPtr (p);
 if result <> '' then exit;


 // если указатель в 16Мб области относительно ближайшего, предоставить смещение
 if (found > 0) and (ex > found) and (ex - found <= 16 * 1048576)  then
   begin
    result := pinf.Value + ' + $' + IntToHex ( ex - found, 5 );
    result := '[' + sp + ']::' + result;
   end
 else
    result := mp;
end;

function LuaSetPtrInfo (L: lua_State): Integer; cdecl;

var
   sn, flg: String;
   p: Pointer;
begin
 if lua_gettop (L) >= 2 then
  begin
   p := Ptr ( LuaDWORD (L, 1) );
   sn := LuaStrArg(L, 2);
   flg := '';
   RegPtr ( p, sn, TRUE );
  end;

 result := 0;
end;

function LuaNearestPtr (L: lua_State): Integer; cdecl;
var
   addr, ofs: DWORD;
          sn: String;

begin
 ofs := 0;
 addr := LuaDWORD (L, 1);
 if lua_gettop (L) > 1 then
    ofs := LuaDWORD (L, 2);

 addr := addr + ofs;

 sn := GetPtrInfo ( Ptr ( addr ) );
 result := 1;
 lua_pushwstr (L, sn);
end;



function LuaGetSetVerbosity (L: lua_State): Integer; cdecl;
begin
 result := 1;
 if lua_gettop(L) >= 1 then
    p_shared_vars.log_verbose_level := lua_tointeger (L, 1);
 lua_pushnumber(L, log_verbose);
end;


function LuaLoadFreeLib (L: lua_State): Integer; cdecl;
var
   libname: String;
   h: DWORD;
begin
 result := 1;

 if lua_gettop(L) < 1 then
  begin
   lua_pushnumber (L, -1);
   exit;
  end;

 case lua_type(L, 1) of
     LUA_TSTRING:
       begin
        libname := LuaStrArg(L);
        libname := FmtStalkerPath (libname);
        if not FileExists (libname) then
         begin
          PrintError('Not found library ' + libname);
          lua_pushnil(L);
         end;


        h := LoadLibrary ( PChar (libname) );
        lua_pushnumber (L, h);
       end;
      LUA_TNUMBER:
       begin
        h := lua_tointeger (L, 1);
        if (h > 0) and (FreeLibrary(h)) then
            lua_pushnumber(L, 0)
        else
            lua_pushnumber(L, -1);
       end;
   else
       lua_pushnumber (L, -1);
 end; // case


end;



function GetProcAddr (L: lua_State): String;

var
   libname: String;
    proc_w: PChar;
     procn: String;
  proc_ord: DWORD;
         h: HMODULE;
         p: Pointer;

begin
 libname := LuaStrArg(L);
 if lua_gettop(L) < 2 then
  begin
   result := '#ERROR: needs 2-nd arg';
   exit;
  end;

 h := GetModuleHandle ( PChar (libname) );

 if h = 0 then
  begin
   result := '#ERROR: wrong libname ' + libname;
   exit;
  end;

 if lua_type(L, 2) = LUA_TNUMBER then
    begin
     proc_ord := lua_tointeger (L, 2) and $FFFF;
     procn := IntToStr(proc_ord);
     proc_w := PChar ( proc_ord );
    end
  else
    begin
     procn := Trim ( LuaStrArg (L, 2) );
     proc_w := PChar (procn);
    end;

 p := GetProcAddress (h, proc_w);
  if (p = nil) and (procn <> '') then
    result := '#ERROR: func {' + procn + '} not found in ' + libname + '[$' + IntToHex (h, 8) + ']'
  else
    result := FormatPtr (p);
end;

function GetVarAddr (L: lua_State; var p: Pointer): String;

begin
 result := GetProcAddr(L);

 if Pos('#ERROR', result) = 0 then
  begin
   p := Ptr( atoi(result) );
   if ( DWORD(p) > $1000 ) and ( not IsBadReadPtr(p, 4) ) then
        result := 'OK';
  end;
end;

function LuaGetProcAddr (L: lua_State): Integer; cdecl;
begin
 lua_pushwstr (L, GetProcAddr(L));
 result := 1;
end;


function LuaGetVarPtr (L: lua_State): Integer; cdecl;
var
     p: Pointer;
    rs: String;
begin
 result := 1;
 rs := GetVarAddr (L, Pointer(p) );
 if rs = 'OK' then
   begin
    lua_pushptr (L, p);
    SetPointerMT (L, -1);
    exit;
   end;
 lua_pushnil (L);
 lua_pushwstr (L, rs);
 result := 2;
end;

function LuaGetVarValue (L: lua_State): Integer; cdecl;
var
     p: Pointer;
   tag: String;
    rs: String;
    cb: Integer;
begin
 result := 1;
 rs := GetVarAddr (L, p);
 if rs = 'OK' then
   begin
    // 1 - dllName, 2 - varName, 3 - tag, 4 - cb
    tag := LuaStrArg(L, 3);
    cb := lua_tointeger (L, 4);
    if tag = '' then
       tag := 'ptr';
    LuaPushMemValue (L, p, tag, cb);
    exit;
   end;

 lua_pushnil (L);
 lua_pushwstr (L, rs);
 result := 2;
end;

function LuaGetGameLevel (L: lua_State): Integer; cdecl;
begin
 result := 0;
 if (g_pGameLevel <> nil) and Assigned(g_pGameLevel^) then
   begin
    lua_pushptr ( L, g_pGameLevel^);
    SetPointerMT (L, -1);
   end
 else
    lua_pushnil ( L );

 lua_setglobal (L, 'g_pGameLevel');

end;


// фактически пустышка, для демаскировки стека
function LuaPopValues (L: lua_State): Integer; cdecl;
var i: Integer;
begin
 result := 0;
 ODS('[~T]. #DBG: LuaPopValues, #args = ' + IntToStr( lua_gettop(L) ) );
 if lua_gettop (L) = 0 then exit;

 for i := 1 to lua_gettop(L) do
  begin
   lua_pushvalue (L, i);
   Inc (result);
  end;

end; // LuaPopValues


function GetCachedThread (Lsrc: lua_State): lua_State;
var
   k, lst: String;
   lsd: TLuaStateDesc;
   tls: PThreadLuaState;
   tidx, t: Integer;


   in_func_e: PChar;
begin

 in_func_e := in_func;
 in_func := 'GetCachedThread#1';
 result := Lsrc;

 if Lsrc = nil then
  begin
   PrintError ('GetCachedThread: Lsrc = nil!');
   exit;
  end;


 lsd := FindStateDesc (Lsrc, TRUE); // найти или создать описатель, для корневого lua_State.
 lsc_dbg := 2;

 if lsd = nil then
     begin
      ODS('[~T].~C0C #WARN(GetCachedThread): returned original lua_State~C07');
      in_func := in_func_e;
      exit;
     end;

 in_func := 'GetCachedThread#2';
 tls := lsd.FindTLS ( nil, GetCurrentThreadId ); // поискать по потоку
 t := lua_gettop (Lsrc);
 if tls <> nil then
      repeat
       k := '';
       lst := '';
       // поискать наличие ключа в таблице
       lua_getglobal (Lsrc, LSC_THREADS);
       // lua_pushnil (Lsrc);             // first key - start enum
       in_func := 'GetCachedThread#3';
       if lua_istable(Lsrc, -1) then
        try
         lua_getfield(Lsrc, lua_gettop(Lsrc), AnsiArg(tls.key));
         if not lua_isnil(Lsrc, -1) then
            k := tls.key;

         (*
         repeat
          if lua_next(Lsrc, -2) = 0 then break;          // while lua_next
          in_func := 'GetCachedThread#3.1';
          if not lua_isstring(Lsrc, -2) then break;      // check key is string
          in_func := 'GetCachedThread#3.2';
          k := LuaStrArg (Lsrc, -2);
          in_func := 'GetCachedThread#3.3';
          lst := lst + k + ' ';
          lua_pop(Lsrc, 1);                              // up value
          in_func := 'GetCachedThread#3.4';
         until k = tls.key;
         *)
         lua_settop (Lsrc, t);       // restore stack
        except
         on E: Exception do
           begin
            DebugDumpAll(Lsrc);
            OnExceptLog ('Exception catched in GetCachedThread, lst = ' + lst + ', lua_gettop(Lsrc) = ' + IntToStr(lua_gettop(Lsrc)), E, TRUE);
            exit;
           end;

        end;

       in_func := 'GetCachedThread#4';
       if k <> tls.key then
         begin
          wprintf ('[~T].~C0C #WARN:~C07 key %s not found in table %s', [ String(tls.key), LSC_THREADS]);
          break;
         end;


       result := tls.Lchild; // ассоциированный с потоком
       exit;
      until TRUE;


 lua_settop (Lsrc, t);
 in_func := 'GetCachedThread#5';

 result := lsd.SpawnThread;
 in_func := in_func_e;
end;

function LuaCleanupImpl (Lsrc: lua_State): Integer; inline;
var
     n: Integer;
    fc: DWORD;
   lsd: TLuaStateDesc;
    fn: String;

begin
 ODS('[~T]. #DBG(LuaCleanup): Очистка переменных зависящих от среды Lua.');
 def_ver     := 1;
 game_active := FALSE;
 game_L      := nil;
 kcb_list.Clear;

 if destr_log.Count > 0 then
   begin
    destr_log.SaveToFile (gLogPath + 'destr_objects.log');
    destr_log.Clear;
   end;


 if lua_gettop(Lsrc) > 0 then
    fc := lua_tointeger (Lsrc, 1)
 else
    fc := $FFFFFFFF;


 if ( fc and 1 <> 0 ) and ( ht_list <> nil ) then
  try
   for n := 0 to ht_list.Count - 1 do
       THelperThread ( ht_list.Objects [n] ).StopThread();
   for n := 0 to ht_list.Count - 1 do
       THelperThread ( ht_list.Objects [n] ).WaitStop();
   ht_list.Clear;
  except
   on E: Exception do
     OnExceptLog ('LuaCleanup - ht_list clean', E);

  end;

 {$IFDEF NLC}
 if ( fc and $400000 <> 0 ) then
      Singularity.DoCleanup;
 {$ENDIF}


 if fc and 2 <> 0 then
   for n := captured.Count - 1 downto 0 do
    begin
     lsd := TLuaStateDesc (captured.Objects [n]);
     if lsd = nil then continue;
     lsd.Clear;
    end;

 active_L := nil;
 SetGameLuaState (nil);
 xray_L := nil;


 if rtp_timer <> 0 then
    KillTimer (0, rtp_timer);
 rtp_timer := 0;

 global_pt.StartOne (4);
 upd_timeout := 0;


 if fc and $04 <> 0 then
    FinalizeRegistry (FALSE);

 if fc and $08 <> 0 then
    InstallTopLevelExceptionFilter;

 if fc and $10 <> 0 then
   begin
    captured.FreeObjects;
    captured.Clear;
    wprintf('[~T]. #LEAK_CHECK: gLuaThreadCounter = %d, gLuaThreadRemoved = %d', [gLuaThreadCounter, gLuaThreadRemoved]);
    gLuaThreadCounter := 0;
    gLuaThreadRemoved := 0;
   end;



 if fc and $20 <> 0 then
    texcap.CleanupModule (Lsrc);

 ODS('[~T]. #DBG(LuaCleanup): Таймер обновления биндера выключен.');

 fn := FmtStalkerPath('$profile_dir$\player.');
 if FileExists (fn + 'wcp') then DeleteFile (fn + 'wcp');
 if FileExists (fn + 'dat.bak') then DeleteFile (fn + 'dat.bak');

 XrayRegistry.CleanupDMA;
 g_Config.silent_mode := TRUE;

 // lua_delglobal (Lsrc, 'lsc_threads');
 // lua_gc	(Lsrc, LUA_GCCOLLECT, 0);
 // lua_gc	(Lsrc, LUA_GCCOLLECT, 0);
 result := 0;
end;

function LuaCleanup (Lsrc: lua_State): Integer;  cdecl;
begin
 result := 0;
 try
  result := LuaCleanupImpl (Lsrc);
 except
  on E: Exception do
     OnExceptLog('LuaCleanupImpl', E, TRUE);
 end;
end;


function  SaveByteCode (L: lua_State; pSrc: Pointer; sz: Integer; ud: Pointer): Integer; cdecl;
var
   lst: TList;
    pd: PInteger;
begin
 result := -1;
 if (pSrc = nil) or (ud = nil) or ( sz <= 0 ) then exit;

 lst := ud;
 // ODS('[~T]. #DBG: dumping bytecode to ~C0A' + fname + '~C07');
 pd := AllocMem ( sz + 4 );
 lst.Add (pd);
 pd^ := sz;

 Move (pSrc^, RelativePtr(pd, 4)^, sz);
 result := 0;
end;


function LuaCompile (L: lua_State): Integer; cdecl;
var
   dbg: Boolean;
     s: String;
     d: String;
    cl: TList;
    pb: PByte;
    pd: PInteger;
    ps: PAnsiChar;
    fs: TFileStream;
    sa: AnsiString;
    zv: DWORD;
     t: Integer;
     n: Integer;
begin
 s := LuaStrArg (L);
 d := '';
 t := lua_gettop (L);
 dbg := FALSE;
 result := 0;
 if t > 1 then d := LuaStrArg (L, 2);
 if t > 2 then dbg := lua_toboolean (L, 3);

 zv := GetModuleHandle ('xrLua.dll');
 if zv > 0 then
  begin
   pb := GetProcAddress (zv, 'lua_dump');
   // ODS('[~T]. #DBG: lua_dump at ' + FormatPtr(pb));
   pb := RelativePtr (pb, $901B );
   UnlockRegion (pb);
   if ( pb^ = $89 ) and ( not dbg ) then
     begin
      pb^ := $31;
      ODS( '[~T]. #DBG: luaU_dump patched to release!' );
     end;
    if ( pb^ = $31 ) and ( dbg ) then
     begin
      pb^ := $89;
      ODS( '[~T]. #DBG: luaU_dump patched to debug!' );
     end;
  end;



 if Pos('--', s) = 0 then
   begin
    // Delete(s, 1, 1); //
    s := FmtStalkerPath (s);
    sa := ReadFileContent (s);
    if d = '' then
       d := AnsiReplaceStr (s, '.script', '.bin');
   end
 else
   sa := AnsiString (s);

 if d = '' then
    d := 'bytecode.bin'
 else
    d := FmtStalkerPath (d);

 if FileExists (d) then DeleteFile (d);

 n := luaL_loadstring (L, PAnsiChar(sa));
 if n <> 0 then
   begin
    d := '?';
    if lua_type(L, -1) = LUA_TSTRING then
       d := LuaStrArg (L, -1);
    PrintError ( 'LuaCompile load code error: ' + d );
    exit;
   end;


 cl := TList.Create;
 try
  lua_dump (L, SaveByteCode, cl);
  asm
   // xor [esp + $2C], ebx
  end;
 except
  on E: Exception do
     OnExceptLog ('LuaCompile->lua_dump', E);
 end;

 fs := TFileStream.Create (d, fmCreate );
 if cl.Count > 0 then
  begin
   for n := 0 to cl.Count - 1 do
    begin
     pd := cl [n];
     ps := RelativePtr(pd, 4);
     // if ( n = 0 ) or ( n = cl.Count - 1 ) then
     if n = 1 then
        PInteger (ps)^ := 5;

     if ( n = 2 ) and ( pd^ > 25 ) then
       begin
        // fs.Write (zv, 4);
        FillChar (ps^, pd^, 0);
        SetStrZ ( ps, '--!!', pd^ ); // ODS("nope! code absent!") --
        pd^ := 5;
       end;

     fs.Write ( ps^, pd^ );
     FreeMem (pd);
    end;
  end
 else
  PrintError ('LuaCompile: No chunks produced, possible error');

 fs.Free;
 cl.Free;

end;

{
  Типичный дамп байт-кода:
    12 байт: заголовок для парсера
    главная функция:
       строка исходника pascal-ansi (size_t before chars) ==> offset + length + 4
       int      linedefined           + 4
       int      lastlinedefined       + 4
       char     nups
       char     numparams
       char     is_vararg
       char     maxstacksize          + 4
    если отл. инфа включена
    ((
       дамп строк:
        int length
        int lines[length]
       дамп локальных переменных:
        int length = f->sizelocvars
        ( string, int, int ) [lenght]
       дамп Upvalues:
        int length
        string [length]
    ))

    дамп констант:
        int length
        (char ttype, nope|char|single|string)
    дамп подфункций:
        int length
        function[length]
}



function LuaLoadBC (L: lua_State): Integer; cdecl;
var
  fname: String;
   buff: TDynByteArray;
begin
 result := 0;
 if lua_gettop (L) < 3 then exit;
 try
  if lua_type (L, 1) = LUA_TSTRING then
    begin
     fname := LuaStrArg (L);
     fname := FmtStalkerPath (fname);
     if RawLoadFile ( fname, buff ) <> '#OK' then exit;
    end;
  LoadByteCode ( L, @buff[0], Length (buff), LuaStrArg (L, 3) );
  // if result < 0 then result := 0;
 finally
  SetLength (buff, 0);
 end;
end;

procedure lua_savectx (Lsrc, L: lua_State);
var
    t: Integer;
begin
 t := lua_gettop(Lsrc);
 lua_pushptr  (Lsrc, L);
 lua_setfield (Lsrc, LUA_REGISTRYINDEX, 'active_vm');
 lua_settop (Lsrc, t);
end;


function ExtractScriptName(const fname: String): String;
var
   ext: String;
begin
  result := ExtractFileName (fname);
  ext := ExtractFileExt (fname);
  if ext <> '' then
     SetLength(result, Length(result) - Length(ext));
end;

function XrExtractPath(const path: String): String;
var
   c: Integer;
   i: Integer;
begin
 result := '';
 c := 0;
 for i := 1 to Length(path) do
  begin
   result := result + path[i];
   if path[i] = '$' then
     begin
      Inc(c);
      if c >= 2 then break;
     end;
  end;
end;


function XrHeader(const name_space, mt: String): AnsiString;
const
   xr_file_header = 'local function script_modified()' +
                    ' return "%s" end ' +
                    'local function script_name()' +
                    ' return "%s" end' +
                    ' local this = {}' +
                    ' %s this %s' +
                    ' setmetatable(this, {__index = _G})' +
                    ' setfenv(1, this) ';
var
   ext,  t, r: String;
begin
 ext := name_space;
 t := StrTok(ext, ['.']);
 if ext = '' then
    r := Format(xr_file_header, [mt, name_space, t + '=', ''])  // name_space = this
 else
    r := Format(xr_file_header, [mt, name_space, t + '= { ' + ext + ' = ',  '}']); // name_space = { ext = this }

 result := AnsiString (r);
end;


function LuaSafeCallImpl (Lsrc: lua_State): Integer; cdecl;




var
   err_idx: Integer;
   errc: String;
   L: lua_State;
   lv: lua_TValue;
   code_valid, cache, err_cb, dbg: Boolean;
   vtype, t, top, i, fidx, args, argc, nret: Integer;
   why, tab, fname, msg,
   stack, fid, r: String;
       in_func_e: PChar;
             ead: Pointer;
             nsn: String;
              sa: AnsiString;
              dt: TDateTime;

begin
 L := Lsrc;

 global_pt.StartOne (37);
 argc := lua_gettop (Lsrc);

 code_valid := TRUE;

 in_func_e := in_func;
 in_func_r := in_func;
 in_func := 'LuaSafeCall#1';

 lsc_dbg := 0;

 fname := LuaStrArg (L, 1);
 msg   := LuaStrArg (L, 2);

 dbg := (Pos('^dbg', msg) > 0);
 cache := not (Pos('^nocache', msg) > 0);
 err_cb := (Pos('^no_err_cb', msg) = 0);

 lsc_dbg := 1;
 result := 0;

 try
  if cache then
    L := GetCachedThread(Lsrc)
  else
   begin
    L := lua_newthread (Lsrc);
    // pd.last_flds [0] := ps.last_flds [0];
    // pd.last_flds [1] := ps.last_flds [1];
   end;

 except
  on E: Exception do
    begin
     PrintError('Exception catched in LuaSafeCall(' + FormatPtr(Lsrc) + ') #1:' + E.Message);
     DebugDumpAll(L);
     gToolThread.AddRequest('BEEP');
     exit;
    end;
 end;

 Assert( Assigned(L), 'thread lua_State not created');


 in_func := 'LuaSafeCall#1.1';
 lsc_dbg := 3;

 lua_savectx(Lsrc, L);

 // if dbg_present then
 if dbg then
    ODS('[~T/' + FormatPtr(Lsrc) + '/' + FormatPtr(L) + ']. #DBG: LuaSafeCall (~C0A"' + fname + '", "' + msg + '"~C07), extra-args = ' + IntToStr (argc - 2) );


 r := 'OK';

 fidx := 0;
 nret := 0;
 why := 'unknown';
 t := lua_gettop (L); // запомнить состояние стека
 err_idx := 0;

 if err_cb then
   try
     lua_getfield (L, LUA_GLOBALSINDEX, 'AtPanicHandler'); // here possible crash :(

      if lua_type(L, -1) = LUA_TFUNCTION then
         fidx := lua_gettop (L) // индекс функции ошибки
      else
        begin
         lua_pop (L, 1);
         lua_pushcfunction (L, AtPanicHandler);
         fidx := lua_gettop (L);
        end;

   except
     on E: Exception do
      PrintError('#EXCEPT: LuaSafeCall#0: ' + E.Message)
     else
      PrintError('#EXCEPT: LuaSafeCall#0 ???')
   end;

 in_func := 'LuaSafeCall#2';
 try
  // ODS ('[~T]. #DBG: LuaSafeCall #args = ' + IntToStr (t) );

  if Pos('#file', msg) = 0 then
    begin
     fname := AnsiReplaceStr (fname, '~LF~', #10);
     fname := AnsiReplaceStr (fname, '~CR~', #13);
    end;


  args := 0;

  lsc_dbg := 4;




  if Pos ('#func', msg) > 0 then
    Begin
      fid := '';
      tab := '';
      // копирование параметров функции из базового луна-стейта, в нить
      if InStr ('.', fname) then
           begin
            tab := StrTok (fname, ['.']);
            if lua_getglobal (L, tab) = LUA_TTABLE then
              begin
               top := lua_gettop(L);
               fname := Trim(fname);
               {
               lua_pushwstr (L, fname);
               lua_gettable (L, top); // table in stack at [1]
               ftype := lua_type (L, -1); // }
               vtype := lua_sk_field (L, fname, top);
               if vtype = LUA_TFUNCTION then
                 begin
                  if dbg then ODS(' #DBG: Вызoв функции ' + fname + ' из namespace ' + tab);
                  fid := 'NS-func';
                 end
               else
                 begin
                  PrintError('Не найдена функция "' + fname + '" в namespace/table ' + tab + ', vtype = ' + IntToStr(vtype) );
                  code_valid := FALSE;
                 end;
              end;

            if fid = '' then
               lua_pop (L, -1);
           end
         else
          if InStr(':', fname) then
           begin
            tab := StrTok (fname, [':']);
            if lua_getglobal (L, tab) in [LUA_TTABLE, LUA_TLIGHTUSERDATA, LUA_TUSERDATA] then
              begin
               // lua_pushwstr (L, fname); lua_gettable (L, -2);
               if lua_sk_field (L, fname, -2) = LUA_TFUNCTION   then
                 begin
                  if dbg then ODS(' #DBG: Вызвов метода ' + fname + ' для объекта ' + tab);
                  fid := 'Obj-method';
                  lua_getglobal (L, tab); // первым параметром идет ссылка на сам объект или таблицу
                 end
               else
                 begin
                  PrintError('Не найдена функция ' + fname + ' в объекте ' + tab);
                  code_valid := FALSE;
                 end;

              end
             else
               begin
                PrintError('Неподходящий тип Lua у переменной ' + tab);
                code_valid := FALSE;
               end;

            if fid = '' then lua_pop (L, -1);
           end;


         if fid = '' then
           begin
            lua_getglobal (L, fname);
            if lua_type (L, -1) = LUA_TNIL then
               begin
                PrintError('LuaSafeCall - Функция не найдена ' + tab + '->' + fname);
                lua_pop (L, 1);
                code_valid := FALSE;
               end;

           end;

         if dbg then
            ODS(' #DBG: function stack item have type = ' + IntToStr(lua_type(L, -1)) );

         lsc_dbg := 5;
         args := (argc - 3 + 1);
         if args > 0 then
           begin
            nret := lua_gettop(Lsrc);
            for i := 3 to argc do
                lua_pushvalue (Lsrc, i);

            lua_xmove(Lsrc, L, args);
            if nret <> lua_gettop(Lsrc) then
               wprintf ('[~T].~C0C #ERROR:~C07 Хюстон, у нас проблемы! LuaSafeCall->lua_xmove, prev_stack = %d, cur_stack = %d',
                        [nret, lua_gettop(Lsrc)]);
           end;


         {
         for i := 3 to argc do
          begin
           lv := lua_toraw( Lsrc, i );
           lua_pushraw ( L, lv );
           Inc (args);
          end; // for
         }
    End // if #func
  else
  if Pos('#file', msg) > 0 then
    begin
     if Pos('$', fname) > 0 then
        XrRescanPathes( XrExtractPath(fname) )
     else
        XrRescanPathes('$game_scripts$');

     fname := FmtStalkerPath (fname);
     result := 1;
     if not FileExists (fname) then
        begin
         lua_pushwstr (Lsrc, 'File not found: ' + fname);
         exit;
        end;
     dt := FileModifiedAt(fname, FALSE);

     why := 'luaL_loadfile';
     sa := ReadFileContent (fname);
     if ( Length(sa) > 0 ) then
        begin
         nsn := ExtractScriptName(fname);

         if sa[1] <> #$1B then
            sa := XrHeader(nsn, FormatDateTime('dd.mm.yyyy hh:nn', dt)) + sa
         else
            begin
             // TODO: set env
             lua_createtable (L, 0, 0);  // this
             lua_setglobal(L, nsn);      // name_space = this
             lua_getglobal(L, nsn);      // name_space to stack
             i := lua_gettop(L);
             lua_createtable (L, 0, 1);
             lua_getglobal(L, '_G');
             lua_setfield (L, -2, '__index');// mt = { __index = _G }
             lua_setmetatable (L, i);        // setmetatble(name_space, mt)
             while lua_gettop(L) >= 1 do
                   lua_pop (L, 1);
            end;


         err_idx := luaL_loadbuffer (L, @sa[1], Length(sa), PAnsiChar(AnsiString(nsn)));
        end
      else
       begin
        lua_pushwstr (Lsrc, 'File is empty: ' + fname);
        exit;
       end;
    end
  else
    begin
     why := 'luaL_loadstring';
     err_idx := luaL_loadstring (L, PAnsiChar ( AnsiString(fname) ));
    end;

  lsc_dbg := 6;
  //  if args > 0 then  ODS(#9#9'pushed args = ' + IntToStr(args) );

  if dbg then
     wprintf(' loadcode err_idx = %d ', [err_idx]);

  in_func := 'LuaSafeCall#3';
  active_L := L;
  if code_valid and ( err_idx = 0 ) then
    try
     why := 'lua_pcall';
     SetActiveLua (L);

     err_idx := lua_pcall (L, args, LUA_MULTRET, fidx);
     if dbg then
        wprintf(' lua_pcall err_idx = %d ', [err_idx]);
    finally
     if ( fidx >= 0 ) and ( lua_iscfunction (L, fidx) ) then
          lua_remove (L, fidx);
    end
  else
     r := 'code not valid';

  active_L := nil;
  nret := 0;
  lsc_dbg := 7;
  in_func := 'LuaSafeCall#4';

  if err_idx = 0 then
     nret := lua_gettop (L) - t; // количество здоровых результатов, которые можно вернуть
 except
  on E: Exception do
   begin
    BrakeState (L);
    lua_settop (L, t);

    upd_timeout := 10000;
    if Pos('EXCEPTION', gLogFileName) = 0 then
       gLogFileRename [0] := AnsiReplaceStr (gLogFilename, '.log', '+EXCEPTION_LSC.log');

    ead := _exception_info.ExceptionRecord.ExceptionAddress;
    r := '~C0C*************************** LuaSafeCall OnException *************************'#13#10;
    r :=  r + '[~T/~i]. Перехвачено исключение в LuaSafeCall, описание:~C0F ' + E.Message +
          #13#10'~C07имя функции/код:~C0F '#13#10 + FormatRight (fname, '  ') +
          #13#10'~C07сообщение = ~C0A"' + msg + '"~C07'#13#10;

    if ead <> nil then
       r := r + 'адрес = ~C0F' + PtrToModPtr (ead) + '~C07'#13#10;

    StalkerODS ('$#DUMP_CONTEXT: Exception', $FF);
    StalkerODS ( r, $FF );

    DebugDumpAll;
    {
    stack := '';
    stack := stack + Format('~C0E thread(L=$%p) ', [L]) + LuaTraceBack (L) + #13#10;        // Вероятно вложенный стек вызовов будет глубже
    stack := stack + '~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~'#13#10;
    stack := stack + Format('~C09 game  (L=$%p) ', [Lsrc]) + LuaTraceBack (Lsrc) + #13#10;

    if stack <> '' then
       r := r + FormatRight (stack, '  ') + '~C07';

    r := r + '-------------------------------------------------------------------------------'#13#10;
    r := r + 'dbg_vars dump: ~C09'#13#10 + FormatRight ( GetDbgVarsDump, '  ') + '~C07'#13#10;
    r := r + '-------------------------------------------------------------------------------'#13#10;
    r := r + CFormat ('in_func = %s', '~C07', [String(in_func)]);

    }


    active_L := nil;
    FlushLog;

    collapse_time := PreciseTime + 30 * DT_ONE_SECOND;

    XrayLogStackTrace ( @last_ex_ptrs );


    if Pos('^mad', msg) > 0 then
       OnExceptLog ('LuaSafeCall("' + fname + '", "' + msg + '", ...)', E, FALSE );
   end;
 end; // try-except

 in_func := 'LuaSafeCall#5';


 if ( err_idx <> 0 ) then
      begin
       errc := '#' + IntToStr(err_idx);
       case err_idx of
           LUA_ERRRUN: errc := 'Runtime Error';
          LUA_ERRFILE: errc := 'Error in file operation';
        LUA_ERRSYNTAX: errc := 'Syntax error';
           LUA_ERRMEM: errc := 'Memory allocation error';
           LUA_ERRERR: errc := 'Error in error-handler';
       end;
         if Pos('ERROR', gLogFileName) = 0 then
            gLogFileRename [0] := AnsiReplaceStr(gLogFilename, '.log', '+ERROR_LSC.log');

       vtype := lua_type (L, -1);

       case vtype of
         LUA_TSTRING:
             errc := errc + ': ' + LuaStrArg (L, -1);
         LUA_TNUMBER:
             errc := errc + ', code = ' + ftow( lua_tonumber(L, -1) );
       end;

       upd_timeout := 15000;
       lua_pop(L, 1);

       errc := '[~T].~C0E Вызов функции/кода: ~C0A'#13#10 + FormatRight (fname, '  ') +
               #13#10'~C0E на этапе ' + why + ' завершился ошибкой: {~C0C ' + errc + '~C07 }';

       stack := '============================================================================='#13#10;
       stack := stack + ' thread ' + LuaTraceBack (L) + #13#10;
       stack := stack + ' caller ' + LuaTraceBack (Lsrc) + #13#10;
       errc  := errc + #13#10'~C0E' + FormatRight (stack, '  ') + '~C07';

       if msg <> '' then  errc := errc + #13#10'~C0Eсообщение:~C0F ' + msg + '~C07';

       errc := errc + 'dbg_vars dump: ~C09'#13#10 + FormatRight ( UnhideText (dbg_vars.Text), '  ') + '~C07'#13#10;

       ODS (errc);

       errc := RemoveColorTags (InfoFmt(errc));
       r := errc;
       FlushLog;
      end;

 result := 1;
 lsc_dbg := 20;

 try
  lua_pushwstr  (Lsrc, r); // result of call
  lsc_dbg := 21;

  if dbg and (nret > 0) then
    ODS(' #DBG(LuaSafeCall): transpassing arguments ' + IntToStr(nret) );



  for i := 1 to nret do
     begin
      lv := lua_toraw (L, i + fidx);
      lua_pushraw (Lsrc, lv);
      Inc (result);
     end;

   lsc_dbg := 22;


   lua_settop (L, t);

   lsc_dbg := 23;


 except
   on E: Exception do
    PrintError('#EXCEPT: LuaSafeCall#3: ' + E.Message)
   else
    PrintError('#EXCEPT: LuaSafeCall#3 ???')
 end;

 if ( lua_gettop (L) = 0 ) and ( dbg ) then
      ODS('[~T]. #DBG(LuaSafeCall), L.stack is clean!');

 lsc_dbg := 50;

 if (not cache) and (L <> Lsrc) then
   begin
    if dbg then ODS('[~T]. #DBG(LuaSafeCall): losted thread lua_State');
   end;
 lsc_dbg := 100;

 in_func := in_func_e;

 global_pt.Stop (37);
 { i := captured.IndexOfObject(lsd);
 if i >= 0 then
    captured.Delete(i); }

end;


function LuaSafeCall (L: lua_State): Integer; cdecl
begin
 try
  result := LuaSafeCallImpl (L);
 finally
  lua_savectx(L, L);
  SetActiveLua (L);
 end;
end;

function FindScript(const name: String): String;
var
   i: Integer;
begin
 result := '';
 if script_dirs = nil then exit;
 for i := 0 to script_dirs.Count - 1 do
   if DirectoryExists(script_dirs[i]) and FileExists( script_dirs[i] + name ) then
      begin
       result := script_dirs[i] + name;
       break;
      end;
end;


var
   ps_cmds: Integer;
   bp_cmds: Integer = 1;
   cmd_nil: Pointer = nil;


function PushCommand (L: lua_State): Integer; cdecl;
begin
 result := 0;
 cmdlist.Lock('ProcessCommands');
 cmdlist.Add ( LuaStrArg(L) );
 cmdlist.Unlock;
end;

function ProcessCommandsImpl (L: lua_State): Integer;
const
  on_rmv_cb = 'on_removed_objects';

var
   s, code, tag, fname: String;
   in_func_e: PChar;
    cmt: CHAR;
   rtmp: TStrMap;
   gobj: TGameObject;

   pfc: Pointer;
   t, i, cnt: Integer;
   cmd_params: String;
begin
 result := 0;
 collapse_time := 0;

 // CheckInstallVEHFlt(TRUE);

 cmd_params := LuaStrArg (L);

 if ps_cmds < 3 then
  begin
   ODS( Format('[~T/~i]. #DBG: ProcessCommands called from $%p', [L]) );
  end;

 // madExcept.ImNotFrozen;

 in_func_e := '<<-lua->>';
 in_func := 'ProcessCommands#1';

 SetGameLuaState (L);

 Inc (ps_cmds);

 if Assigned (removed_list) and (removed_list.Count > 0) then
  try
   // удаление дубликатов
   //  for i := 0 to High (removed_evts) do removed_evts[i] := removed_evts[i] shr 1;

   // вызвать глобальный колбек, с указанием всех погибших нечестной смертью
   lua_getglobal (L, on_rmv_cb);
   if lua_type (L, -1) = LUA_TFUNCTION then
      begin
       // конвертировать в числовую таблицу
       lua_createtable (L, 0, 0);
       cnt := 0;

       for i := 0 to removed_list.Count - 1 do
        begin
         gobj := TGameObject ( removed_list.Objects [i] );
         s := '';

         if ( removed_evts [gobj.id] = 0 ) and gobj.se_destroy then
            begin
             Inc (cnt );
             lua_pushinteger ( L, cnt );
             lua_pushinteger ( L, gobj.id );
             lua_settable ( L, -3 );
             removed_evts [gobj.id] := removed_evts [gobj.id] or 32;
             s := s + IntToStr(gobj.id) + ' ';
            end
          else
          if log_verbose > 7 then
             ODS ( CFormat( '[~T]. #DBG: object %d on_remove events = $%x ', '~C07', [gobj.id,  removed_evts [gobj.id]] ) );
        end;
       if s <> '' then
          wprintf('[~T]. #DBG: executing on_remove ({ %s})', [s]);
       // в стеке идет один аргумент (таблица), за ним функция
       lua_pcall (L, 1, LUA_MULTRET, 0);
      end
   else
     begin
      ODS('[~T].~C0C #WARN:~C07 global callback not found~C0F ' + on_rmv_cb + '~C07');
      lua_pop (L, 1);
     end;

   // объекты-слепки уничтожить
   removed_list.OwnsObjects := TRUE;
   removed_list.Clear;
  except
   on E: Exception do
      OnExceptLog ('ProcessCommands', E);

  end;

 try
  if not Assigned (cmdlist) or (cmdlist.Count = 0) then exit;
 except
  on E: Exception do
     PrintError('WTF? ProcessCommands unexpected exception ');
 end;

 {$IFDEF RELEASE}
 {$IFDEF NLC_GOLD}
 if (cmdlist.Count > 0) and not Assigned (cmd_nil) then
   begin
    PrintError('Invalid command handler = ' + FormatPtr(cmd_nil));
    cmdlist.Clear;
    exit;
   end;
 gToolThread.deviation := 2;
 {$ENDIF}
 {$ENDIF}
 {$IFDEF NLC_GOLD}
 if not dev_comp then exit;
 {$ENDIF}


 if (bp_cmds <> 0) and (cmdlist.Count > 1) and IsDebuggerPresent then
   asm
    int 3
   end;

 cmdlist.Lock('ProcessCommands');
 rtmp := TStrMap.Create();
 try
    in_func := 'ProcessCommands#2';
    if lua_gettop(L) >= 2 then
       cmdlist.Add( LuaStrArg(L, 2) ); // test command

    tag := '#nope';
    try
      while (cmdlist.Count > 0) do
       begin
        code := cmdlist [0];
        // cut spaces
        repeat
         s := code;
         code := AnsiReplaceStr (code, '  ', ' ');
        until s = code;
        code := AnsiReplaceStr (code, ' ', '§');
        cmdlist.Delete(0);
        rtmp.split ('§', code);  // flush !cmd [#arg1 #arg2]
        if (rtmp.Count < 2) or ( Pos ('flush', rtmp [0]) = 0 ) then continue; // add.check
        code := rtmp[1];
        cmt := code [1]; // ~ or !
        if Length (code) < 5 then continue;  // cmd.length check
        Delete (code, 1, 1); // remove '!'

        t := lua_gettop (L);

        lua_getfield (L, LUA_GLOBALSINDEX, 'LuaSafeCall');
        pfc := lua_topointer (L, -1);

        case cmt of
         CHCMD_FUNC:
             begin
              tag := '#code';
              code := code + '(';
              for i := 2 to rtmp.Count - 1 do
               begin
                if i > 2 then code := code + ', ';
                code := code + '"' + rtmp[i] + '"';
               end;
              code := code + ')';
              ODS('[~T]. #DBG: for command ~C0A' + code + '~C07 having ~C0D' + IntToStr(rtmp.Count - 2) + '~C07 args');
             end;
         CHCMD_FILE:
             begin
              if Pos('$', code) > 0 then
                 fname := FmtStalkerPath (code)
              else
                 fname := FindScript (code);

              if (fname <> '') and FileExists (fname) then
                begin
                 StalkerODS('#[~T]. #DBG: executing Lua file~C0A "' + fname + '"~C07', 255);
                 code := fname;
                 tag := '#file';
                end
              else
                begin
                 StalkerODS('!~C0C[~T]. #ERROR: script file not found~C0F "' + code + '"~C07', 255);
                 StalkerODS('#DBG: lookup directories: '#13#10 + script_dirs.Text, 255);
                 code := '';
                 tag := '#file';
                end;
             end;
         else
           begin
            PrintError ('ProcessCommands detect wrong chmcd: "' + cmt + '", allows "!" and "#" only' );
            exit;
           end;
        end;

        if code = '' then continue;

        if Length (code) < 40 then
            ODS(#9' ' + CFormat('LuaSafeCall = $%p, param: {{ %s }}, tag = %s ', '~C07', [ Pointer(pfc), code, tag ]) );

        in_func := 'ProcessCommands#3';

        try
         lua_getfield (L, LUA_GLOBALSINDEX, 'LuaSafeCall');
         lua_pushwstr (L, code ); // #arg1: nested code
         lua_pushwstr (L, tag + ': ProcessCommands ' + cmd_params);  // #arg2: comment and tag
        except
         on E: Exception do PrintError ('ProcessCommands#1: ' + E.Message);
        end;


        in_func := 'ProcessCommands#4';
        lua_pcall (L, 2, 0, 0);

        lsc_dbg := 101;
        result := lua_gettop(L) - t;

        lsc_dbg := 111;
       end;
     except
      on E: Exception do
         OnExceptLog ('ProcessCommands lsc_dbg = ' + IntToStr (lsc_dbg), E);
     end;
 finally
  rtmp.Free;
  cmdlist.Unlock;
  in_func := in_func_e;
  in_func_r := '/-->';
 end;

end; // ProcessCommands

var
   ps_cmds_lock: Integer = 0;

function ProcessCommands (L: lua_State): Integer; cdecl;
var
   locks: Integer;
begin
 result := 0;
 locks := InterlockedIncrement(ps_cmds_lock);
 if locks > 1 then
   begin
    InterlockedDecrement(ps_cmds_lock);
    ODS('[~T].~C0C #WARN:~C07 nested call ProcessCommands detected');
    if locks = 2 then
       ODS('~C0A' + LuaTraceback(L, ' '));
    exit;
   end;

 try
  result := ProcessCommandsImpl (L);
 finally
  InterlockedDecrement(ps_cmds_lock);
 end;
end;

function ProcessKeyboard (L: lua_State): Integer; cdecl;
var
   n: Integer;
   kmap, rmap: TKeyboardState;
   cs, ks, ps: BYTE;
   cb: TKeyboardCallback;
   kc,  argc: Integer;
   psh, p1, p2, p3: Boolean; // pass flags
      latency: Double;
         dbgc: Boolean;

begin
 result := 0;
 collapse_time := 0;

 latency := g_timer.GetTime - th_kupdate;

 if latency > 1000 * DT_ONE_MSEC then
   begin
    ODS('[~T].~C0C #WARN:~C07 keyboard processing latency =~C0F ' + TimeToStrMS ( latency ) + '~C07');
   end;


 argc := lua_gettop (L);

 kmap := th_kstate;

 psh := TRUE;
 dbgc := FALSE;

 if argc >= 0 then
    psh := lua_toboolean (L, 1);


 // GetKeyboardState (kmap);


 for n := 0 to High (kmap) do
   begin
    ks := kmap [n];

    ps := ls_kstate [n]; // previous

    cs := 0;
    kc := InterlockedExchange ( key_clicks [n], 0 );

    if ( ks >  0 )  and ( ps = 0 ) then cs := cs or 1;  // key down
    if ( ks <> ps ) and ( ks = 0 ) then cs := cs or 2;  // key up
    if ( kc <> 0 ) then cs := cs or 4;                  // next

    rmap [n] := ( ks and $80 ) or cs;                   // state was changed = 1


    {
    if ( n = Ord('B') ) and ( cs <> 0 ) then
        begin
         ODS( CFormat('[~T/~B]. #DBG: pressed key #%d, count = %d, ps = $%x, ks = %x,  flags = $%x, psh = %d', '~C07',
                                                        [n, kc, ps, ks, cs, Integer(psh)])  );
         dbgc := TRUE;
        end; // }

    ls_kstate [n] := ks;
   end;

 prv_clicks := key_clicks;

 // loop foreach callback
 if Assigned(kcb_list) and psh then
    for n := 0 to kcb_list.Count - 1 do
      begin
       cb := TKeyboardCallback ( kcb_list [n] );

       if (cb.vk1 = 0) or (cb.func = '') then continue;

       cs := rmap [cb.vk1];

       p1 := ( cs and cb.evt_code ) <> 0;
       p2 := ( cb.vk2 = 0 ) or ( rmap [cb.vk2] and $81 <> 0 ); // for alt / ctrl / shift
       p3 := ( cb.vk3 = 0 ) or ( rmap [cb.vk3] and $81 <> 0 ); // for alt / ctrl / shift

       if dbgc and ( cb.vk1 = Ord('B') ) then
          ODS ( CFormat( '[~T]. #DBG: cs = %x, p1 = %d, evt_code = $%x ', '~C07', [cs, Integer(p1), cb.evt_code] ) );

       if p1 and p2 and p3 then else continue;

       if cb.vk1 = Ord('B') then
          ODS('[~T]. #DBG: trying exec cb-func: ' + cb.func);

       try
         lua_getglobal (L, 'LuaSafeCall'); // outspace execution
         lua_pushwstr (L, cb.func ); // #arg1: nested code
         lua_pushwstr (L, '#func: callback from ProcessKeyboard ');  // #arg2: comment and tag
         lua_pcall (L, 2, 0, 0);
       except
         on E: Exception do
            OnExceptLog ('ProcessKeyboard, func = ' + cb.func,  E);
       end;

       lua_settop (L, argc);
      end;
end;


function DelKbdCallback (L: lua_State): Integer; cdecl;
var
   cb: TObject;
    i: Integer;
begin
 result := 0;

 if lua_gettop (L) < 1 then exit;

 cb := lua_topointer (L, 1);

 i := kcb_list.IndexOf (cb);
 if i >= 0 then
    kcb_list.Delete (i);
end;

function CharToVK ( ch: AnsiChar ): Integer;
begin
 result := 0;
 ch := UpCase (ch);
 if CharInSet ( ch, ['0'..'9', 'A'..'Z'] ) then
   begin
    result := Ord (ch);
    exit;
   end;

end;


function XrayKeyCode (L: lua_State; idx: Integer): Integer;
var
   fini: TIniFile;
     sl: DWORD;
     cc: BYTE;
      s: AnsiString;
     ss: String;
begin
 result := 0;
 case lua_type (L, idx) of
  LUA_TNUMBER:
     result := lua_tointeger (L, idx);
  LUA_TSTRING:
     begin
      s := lua_tolstring ( L, idx, sl );
      ss := String (s);

      cc := Min ( 100, Length (s) );

      if cc = 1 then result := CharToVK ( s[1] ) else
      if ( cc = 2 ) and ( s[1] = 'k' ) then
           result := CharToVK ( s[2] ) else
      // case не приемлимо здесь использовать!
      if ( cc in [3, 4] ) and ( Pos('kF', ss) = 1 ) then
           result := atoi ( Copy ( ss, 3, 2 ) ) + $6F
       else
         begin
          fini := TIniFile.Create ( FmtStalkerPath ( '$fs_root$\keymap.conf' ) );
          ss := fini.ReadString ('vk', ss, '0' );
          result := atoi (ss);
          fini.Free;
         end;

     end;
 end; // case

end;

function SetKbdCallback (L: lua_State): Integer; cdecl;
var
   argc: Integer;
    msg: String;
     cb: TKeyboardCallback;
begin
 result := 1;
 argc := lua_gettop (L);

 if argc <= 1 then
    begin
     lua_pushnil (L);
     exit;
    end;

 cb := TKeyboardCallback.Create;

 cb.func := LuaStrArg (L);
 cb.vk1 := XrayKeyCode (L, 2);
 cb.evt_code := 1;

 if argc > 2 then cb.vk2 := XrayKeyCode (L, 3);
 if argc > 3 then cb.vk3 := XrayKeyCode (L, 4);
 if argc > 4 then cb.evt_code := lua_tointeger (L, 5);
 msg := LuaTraceBack(L);
 if (msg <> '') and (log_verbose >= 7) then
    wprintf('[~T]. #DBG: updated callback for combination %d + %d + %d to %s',
                         [cb.vk1, cb.vk2, cb.vk3, DumpVar(L, 1)]);
 kcb_list.Add (cb);
 lua_pushptr (L, cb);
end;

function SetUpdTimeout (L: lua_State): Integer; cdecl;
var
   nt: Integer;
   in_func_e: PChar;
   on_load: Boolean;
   from: String;
begin
 in_func_e := in_func;
 in_func := 'SetUpdTimeout#1';
 from := 'binder';
 bu_last := PreciseTime;

 nt := lua_tointeger (L, 1);
 if lua_gettop (L) > 1 then
    from := LuaStrArg (L, 2);

 result := 0;
 on_load := (from = 'net_spawn'); // флажок "при загрузке уровня"
 // if nt = 0 then game_L := nil;


 if on_load then
   begin
    SetGameLuaState (L);
    active_L := nil;
    if (rtp_timer = 0) and (rtp_enable) then
        rtp_timer := SetTimer (0, 3029, 50, @RegularTimerProc);
   end
  else
   if (nt > 0) then global_pt.StartOne (7); // разрешение на вызов RegularTimerProc


 if Assigned (global_pt) then
 try

   if nt < 0 then nt := 30000;

   if (upd_timeout = 0) and (not on_load) and (global_pt.Elapsed (4) < 30000) then
   else
      begin
       if upd_timeout = 0 then
          ODS('[~T]. #DBG: Таймер обновления биндера установлен в ~C0D' + IntToStr(nt) + '~C07' );
       upd_timeout := nt;
      end;

   if (nt = 0) then global_pt.StartOne (4);

   if разрчит and (not on_load) then
      ProcessCommands (L);


   global_pt.StartOne (5);

   {$IFOPT D-}
   if (Random (2) = 1) and (global_pt.Elapsed(2) > 180 * 1000) then
      ReplayHard (L);
   {$ENDIF}
 except
  on E: Exception do
     OnExceptLog ('SetUpdTimeout', E);
 end;
 in_func := in_func_e;
end;



procedure SmartCacheFile (sFile: String);
var
   hFile: THandle;

   buff: array [0..65535] of BYTE;
   dwr: DWORD;

begin
 sFile := CorrectFilePath (ExePath + '..\' + sFile ); // относительно корня игры получается

 if (sFile = '') or ( not FileExists (sFile) ) then exit;


 hFile := CreateFile (PChar (sFile), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_ALWAYS, FILE_FLAG_SEQUENTIAL_SCAN, 0);

 if hFile = INVALID_HANDLE_VALUE then exit;

 try
  // цикл чтения
  repeat
   dwr := 0;
   if not  ReadFile (hFile, buff, sizeof(buff), dwr, nil) then break;
   Sleep(50);
  until dwr < sizeof (buff);

 finally
  CloseHandle (hFile);
 end;
end; // SmartCacheFile


function CacheFile(L: lua_State): Integer; cdecl;
var
   sFile: String;

begin
 result := 0; // no results
 sFile := Trim ( LuaStrArg (L) );
 SmartCacheFile (sFile);
end;  // CacheFile

function CacheList (L: lua_State): Integer; cdecl;
var
   sFile: String;
   sltmp: TStrMap;
   n: Integer;

begin
 result := 0;
 sFile := Trim ( LuaStrArg (L) );
 sFile := CorrectFilePath (ExePath + '..\' + sFile );
 if not FileExists (sFile) then exit;

 sltmp := TStrMap.Create;
 try
  sltmp.LoadFromFile(sFile);
  for n := 0 to sltmp.Count - 1 do
   begin
    sFile := Trim (sltmp [n]);
    if sFile = '' then continue;
    SmartCacheFile (sFile);
   end;

 finally
  sltmp.Free;
 end;
end; // CacheList



function lua_dostring(L: lua_State; s, desc: String): String;
var
   a, d: AnsiString;
    err: Integer;
    top: Integer;

begin
 top := lua_gettop(L);
 a := AnsiString(s);
 d := AnsiString(desc);
 // luaL_loadstring(L, PAnsiChar(a));
 err := luaL_loadbuffer (L, @a[1], Length(a), PAnsiChar(d));
 if err = 0 then
    err := lua_pcall(L, 0, LUA_MULTRET, 0);
 if err = 0 then
   result := '#OK'
 else
   result := '#ERROR: ' + LuaStrArg(L, -1);

 lua_settop (L, top);
end;

var
   lua_pcall_orig: function (L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl = nil;
   lua_pcall_next: Pointer;
      lua_pcall_p: Pointer absolute lua_pcall_orig;
      lua_pcall_w: Pointer;
      lua_ns_orig: function (f: lua_Alloc; ud: Pointer): lua_State; cdecl = nil;
      lua_nt_orig: function (f: lua_Alloc; ud: Pointer): lua_State; cdecl = nil;
     luaL_ns_orig: function (): lua_State; cdecl = nil;
    lua_type_orig: function(l: lua_State; idx: Integer): Integer; cdecl = nil;
      lua_ps_orig: procedure(l: lua_State; ps: PAnsiChar); cdecl = nil;          // pushstring
      lua_ls_orig: function(l: lua_State; ps: PAnsiChar): Integer; cdecl = nil;  // loadstring
      lua_lf_orig: function(l: lua_State; ps: PAnsiChar): Integer; cdecl = nil;  // loadfile
      lua_ld_orig: function(l: lua_State; lr, data: Pointer; cName: PAnsiChar): Integer; cdecl = nil; // lua_load


   alloc_list: array of TAllocRec;



function xr_alloc_mem (ud: Pointer; cb: DWORD): Pointer; inline;
begin
 result := orig_hf (ud, nil, 0, cb);
end;

procedure xr_free_mem (ud, p: Pointer; cb: DWORD); inline;
begin
 orig_hf (ud, p, cb, 0);
end;


function GetAlloc ( ud_orig: Pointer ) : TFastMemAllocator;
var n, l: Integer;
    tid: DWORD;
begin
 tid := GetCurrentThreadId;
 l := Length(alloc_list);

 for n := l - 1 downto 0 do
   if alloc_list [n].tid = tid then
     begin
      result := alloc_list [n].fma;
      exit;
     end;

 result := TFastMemAllocator.Create ('luaicp', FALSE);
 result.WorkMM.ud := ud_orig;

 result.WorkMM.alloc_mem := xr_alloc_mem;
 result.WorkMM.free_mem := xr_free_mem;

 // TODO: unsafe code
 SetLength (alloc_list, l + 1);

 alloc_list [l].fma := result;
 alloc_list [l].tid := tid;

end; // GetAlloc


procedure DumpMemStat;
var n, cnt_p, cnt_ab, cnt_fb, freed: Integer;
    cb_p, cb_h: Int64;
    mib_p, mib_h: Double;
    fma: TFastMemAllocator;
    leaked: Integer;
    arqs, frqs: Integer;

    s: String;
begin
 cnt_p := 0;
 freed := 0;
 cb_p := 0;
 cb_h := 0;
 cnt_ab := 0;
 cnt_fb := 0;
 leaked := 0;
 arqs := 0;
 frqs := 0;

 for n := Length(alloc_list) - 1 downto 0 do
  begin
   fma := alloc_list [n].fma;
   if fma = nil then continue;

   Inc (cnt_p, fma.MemPages.Count);
   Inc (cb_p,  fma.Allocated);
   if fma.Heap <> nil then
      Inc (cb_h, fma.Heap.Size);

   Inc (cnt_ab, fma.alloc_blocks);
   Inc (cnt_fb, fma.freed_blocks);

   Inc (freed, fma.FreedSize);

   Inc (arqs, fma.alloc_rqs);
   Inc (frqs, fma.free_rqs);
   Inc (leaked, fma.mfree_leak);

   fma.alloc_rqs := 0;
   fma.free_rqs := 0;
  end;
 mib_p := (cb_p / 1048576.0);
 mib_h := (cb_h / 1048576.0);

 CalcTexMemUsage;

 s := CFormat('[~T]. #OPTF(MEM): Занято %.3f МиБ памяти в %d страницах для FMA, %.3f МиБ в heap-objects. Alloc-blocks = %d, Freed-blocks = %d, FreedSize = %d ' +
              '  AddrSpaceUsed = %d МиБ, Leaked = %d КиБ, Textures = %d КиБ ',
          '~C07',
              [ mib_p, cnt_p, mib_h, cnt_ab, cnt_fb, freed, GetAddressSpaceUsed div 1048576, leaked div 1024, gTexMemUsage div 1024] );
 s := s + #13#10#9 +
      CFormat (' alloc rqs = %fK, free rqs = %fK', '~C07', [arqs / 1000.0, frqs / 1000.0] );

 if s <> last_mem_stat then
   begin
    StalkerODS (s, $FF);
   end;
 last_mem_stat := s;
end;

function TestMM(L: lua_State): Integer; cdecl;
var
  cb_alloc, cur_alloc: Integer;
  mmf: TLuaMM;
  fma: TFastMemAllocator;
  blks: array of TMemBlock;
  pmb: PMemBlock;
  ud: Pointer;
  i, bcnt, bmax: Integer;
  pt: TProfileTimer;


begin
 result := 0;
 cb_alloc := 1024 * 1024;

 fma := nil;
 pt := nil;

 if lua_gettop (L) > 0 then
    cb_alloc := lua_tointeger (L, 1);

 try
  try
     cb_alloc := Max (65536, cb_alloc);
     pt := TProfileTimer.Create;
     fma := TFastMemAllocator.Create('TestMM');
     fma.WorkMM.ud := orig_ud;
     fma.WorkMM.alloc_mem := xr_alloc_mem;
     fma.WorkMM.free_mem := xr_free_mem;


     if g_Config.bInterceptMM then
       begin
        mmf := DMAlloc;
        ud := fma;
       end
     else
       begin
        mmf := orig_hf;
        ud := orig_ud;
       end;

     if not Assigned (mmf) then exit;

     bmax := cb_alloc div 1000;
     SetLength (blks, bmax);

     cur_alloc := 0;

     /// ------------------------------- Benchmark -------------------- ///
     bcnt := 0;

     pt.StartOne;

     // --- allocation and random free
     while ( bcnt < bmax ) and ( cur_alloc < cb_alloc ) do
      begin
       pmb := @blks [bcnt];
       Inc (bcnt);
       pmb.sz := Random (4096) + 1;
       pmb.pp := mmf (ud, nil, 0, pmb.sz);
       Inc ( cur_alloc, pmb.sz );           // counter allocation summ

       if (bcnt < 1000) and (Random (7) <> 5) then continue;
       i := Random (bcnt);
       pmb := @blks [i];
       if pmb.sz = 0 then continue;


       mmf (ud, pmb.pp, pmb.sz, 0);
       pmb.sz := 0;

      end;


     pt.StartOne (2);
     // --- full free stage

     for i := 0 to bcnt - 1 do
      begin
       pmb := @blks [i];
       if pmb.sz = 0 then continue;
       mmf (ud, pmb.pp, pmb.sz, 0);
      end;


     ODS ('------------------------------------------------------------------');
     ODS ( CFormat ( '~C0E[~T]. #OPT: TestMM (int=%d) time = %.1f ms, CPUTime = %.1f ms, close time = %.1f ms, close CPUTime = %.1f ms, bcnt = %d ~C07', '~C0E',
              [Integer(g_Config.bInterceptMM),  pt.Elapsed, pt.CPUElapsed, pt.Elapsed (2), pt.CPUElapsed (2), bcnt] ) );

   except
    on E: Exception do
       OnExceptLog ('TestMM', E);
   end;
  finally
   SetLength (blks, 0);
   fma.Free;
   pt.Free;
  end;
end;


procedure LoadClassMap (L: lua_State; idx: Integer);
var
   n: String;
   c: Integer;
   t: Integer;
begin
 if not lua_istable (L, idx) then exit;
 t := lua_gettop (L);
 class_map.Clear;
 lua_pushnil (L);
 while lua_next (L, idx) <> 0 do
  begin
   c := lua_tointeger (L, -2);
   n := LuaStrArg     (L, -1);      // clsid=class_name
   class_map.IntValues[n] := c;
   lua_pop(L, 1);
  end;
 lua_settop (L, t);
end;

function LuaRandom(L: lua_State): Integer; cdecl;
var
   argc: Integer;
   a, b: Integer;
     rv: Double;
begin
 argc := lua_gettop (L);
 rv := 0;

 case argc of
   0: rv := Random ();
   1: rv := Random ( lua_tointeger (L, 1) ) + 1;
   2: begin
       a := lua_tointeger (L, 1);
       b := lua_tointeger (L, 2);
       rv := Random ( Abs (b - a) ) + a;
      end;
 end;

 lua_pushnumber (L, rv);
 result := 1;
end;

function LuaReplaceMT(L: lua_State): Integer; cdecl;
begin
 result := 0;
 if ( lua_gettop(L) = 2 ) and ( lua_istable (L, 2) ) then
      lua_setmetatable (L, 1)
 else
      PrintError('Bad arguments for ReplaceMT');
end;

function LuaRunCommand(L: lua_State): Integer; cdecl;
var
   cmd: String;
   pls: String;
   dll: DWORD;
     n: Integer;

begin
 result := 0;
 cmd := LuaStrArg (L);
 pls := LuaStrArg (L, 2);
 if cmd = 'PROCESS_COMMANDS' then
   begin
    lua_getglobal(L, 'ProcessCommands');
    lua_call (L, 0, 0);
   end
 else
 if cmd = 'DUMP_CONSOLE_BUFFER' then
    DumpConsoleBuff (nil)
 else
 if cmd = 'LOAD_GLOBAL_MAP' then
    gToolThread.AddRequest (cmd)
 else
 if cmd = 'GAME_CON_LOG_ON' then
    game_con_log := TRUE
 else
 if cmd = 'GAME_CON_LOG_OFF' then
    game_con_log := FALSE
 else
 if cmd =  'SET_LOG_CB' then
    SetLogCallback
 else
 if cmd = 'GET_ENVSTR' then
  begin
   pls := GetEnvironmentVariable ( pls );
   lua_pushwstr (L, pls);
   result := 1;
  end
 else
 if cmd = 'GET_REGSTR' then
  begin
   cmd := LuaStrArg (L, 2); // path
   pls := LuaStrArg (L, 3); // var
   pls := ReadSysReg ( cmd, pls );
   lua_pushwstr (L, pls);
   result := 1;
  end
 else
 if cmd = 'CPU_INFO' then
  begin
   pls := IntToStr (GetCPUCount) + ' @ ' + GetCPUName();
   lua_pushwstr (L, pls);
   result := 1;
  end
 else
 if cmd = 'MM_FREE_BUFF' then
   begin
    DumpMMStat(L);
    {$IFDEF FAST_MM}
    FreeBuffer;
    {$ENDIF}
    DumpMMStat(L);
   end
 else
 if cmd = 'DUMP_MM_STAT' then
   begin
    LogMemoryManagerStateToFile('mm_stat.log');
   end
 else
 if cmd = 'ON_GAME_MM' then
   begin
    XrayMM.mm_list[0].can_used := g_Config.allow_game_mm;
    XrayMM.DumpStats;
   end
 else
 if cmd = 'OFF_GAME_MM' then
   begin
    XrayMM.mm_list[0].can_used := FALSE;
    XrayMM.DumpStats;
   end
 else
 if cmd = 'CALC_MEM_USAGE' then
   begin
    CalcTexMemUsage;
   end
 else
 if cmd = 'MW_ENABLE'  then
   begin
    if mw_flags and 1 = 0 then
       wprintf('[~T]. #DBG: mw_enabled =~C0F true~C07, context =~C0A %s~C07', [pls]);
    mw_flags := mw_flags or 1;
   end
 else
 if cmd = 'MW_DISABLE' then
   begin
    if mw_flags and 1 = 1 then
       wprintf('[~T]. #DBG: mw_enabled =~C0E false~C07, context =~C0A %s~C07', [pls]);
    mw_flags := mw_flags and $FFFFFFFE;
   end
 else
 if cmd = 'MW_DBG_START' then
    mw_flags := mw_flags or 2
 else
 if cmd = 'MW_DBG_END' then
    mw_flags := mw_flags and $FFFFFFFD
 else
 if Pos ('INIT_DMA', cmd) > 0 then
  begin
   if lua_gettop (L) > 1 then
      g_game_build := lua_tointeger (L, 2)
   else
      g_game_build := DetectGameBuild;

   ExportGameBuild (L);
   InitDMA ( cmd = 'REINIT_DMA' );
  end
else
 if cmd = 'INNER_CODE' then
  begin
   pls := '';
   for n := 3 to lua_gettop (L) do
     case lua_type (L, n) of
      LUA_TNUMBER:
         pls := AutoComma(pls) + ftow ( lua_tonumber (L, n) );
      LUA_TSTRING:
         pls := AutoComma(pls) + LuaStrArg (L, n);

     end; // case
   pls := LuaStrArg (L, 2) + '<' + pls;
   inner_cmdl.Lock('add');
   inner_cmdl.Add (pls + '>');
   inner_cmdl.Unlock;
  end
 else
 if cmd = 'SHOW_PULSE' then
    bShowPulse := TRUE
 else
 if cmd = 'HIDE_PULSE' then
    bShowPulse := FALSE
 else
 if cmd = 'START_CRYPTO' then
  begin
   dll := LoadLibrary ('advapi32.dll');
   ODS('[~T]. #DBG: Environment unbalanced. Library loaded at $' + IntToHex(dll, 8) );
  end
 else
 if ( cmd = 'CLEAR_KCB_LIST' ) and Assigned(kcb_list) then
   begin
    wprintf ('[~T].~C0C #WARN:~C07 %d keyboard callbacks removed via command %s ', [kcb_list.Count, cmd]);
    kcb_list.Clear;
   end
 else
 if ( Pos('SIGNAL_', cmd) = 1 ) then
      BroadcastSignal (L, cmd)
 else
 if ( cmd = 'LOAD_CLASS_MAP' ) then
      LoadClassMap (L, 2);

 {$IFDEF NLC_GOLD}
 // утечка ресурсов
 dll := CreateFile ( PChar(XrUpdatePath('$game_config$', 'system.ltx')), GENERIC_READ, FILE_SHARE_WRITE or FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0);
 if dll = INVALID_HANDLE_VALUE then
   begin
    case GetLastError of
      1: ;
      3: ;
      else
        wprintf(' error = $%x ', [GetLastError]);
    end
   end
 else
    AllocMem (dll);

 {$ENDIF}
 {$IFDEF PROFILES}
 if Assigned(SetUserName) and ( Length(profile_name) > 4 ) then
    SetUserName( LPSTR(AnsiString(profile_name)) );
 {$ENDIF}

end;

function ApplyRelocations (L: lua_State): Integer; cdecl;
// local rqs = { reloc_data = cur, code_ptr = CalcPtr(shcopy, code_ofs, '-'), delta = -hmod }
var
   limit: DWORD;
   rdata: PDWORDArray;
    list: PWORDArray;
    code: PByteArray;
    pval: PDWORD;
     cnt: Integer;
     dta: Integer;
     ofs: DWORD;
     rva: DWORD;
      pc: Integer;
       n: Integer;

begin
 result := 1;
 pc := 0;

 rdata := lua_objptr (L, 1);
 code  := lua_objptr (L, 2);
 limit := lua_tointeger (L, 3);
 dta   := lua_tointeger (L, 4);

 rva   :=  rdata[0];
 cnt   := (rdata[1] - 8) div 2;
 list  := @rdata[2];

 for n := 0 to cnt - 1 do
  begin
   ofs := list[n];
   if ofs < $1000 then continue;
   ofs := ofs and $FFF + rva;
   if ofs > limit - 3 then continue;
   pval := @code[ofs];
   Inc (pval^, dta);
   Inc (pc);
  end;
 lua_pushinteger (L, pc);

end;



var cap_cnt: Integer = 0;

procedure CheckCapture (L: lua_State; const sFrom: String; async: Boolean; reg_stuff: Boolean = TRUE );
var
   ls: ^lua_StateRec;
   new_ud: Pointer;
   is_cap: Boolean;
   lsd: TLuaStateDesc;
   top: Integer;

    gs: PLuaGlobalState;
    sf: String;

begin
 if NativeUInt (L) < $10000 then exit;
 if captured = nil then exit;

 if IsDebuggerPresent then
    ODS('[~T]. #DBG: processing CheckCapture for ' + FormatPtr(L) + ' from ' + sFrom );

 g_Config.silent_mode := FALSE;
 top := lua_gettop (L);
 // Захват новой фишки
 gCheckPoints.init (50, 0, 0, 0);
 while not captured.TryLock('CheckCapture') do
     PrintError('Cannot lock object "captured".');

 try

   is_cap := ( nil <> FindStateDesc (L, FALSE) ); // уже захвачено, ага?
   if is_cap then
     begin
      lua_getglobal (L, 'LuaSafeCall');
      if lua_type (L, -1) = LUA_TFUNCTION then
         begin
          ODS('[~T]. #MSG: already captured and stuffed state ' + FormatPtr(L) + '. Exiting...' );
          exit;
         end
        else
          ODS('[~T].~C0C #WARN~C07: already captured but not stuffed state ~C0D' + FormatPtr(L) + '~C07' );

      lua_pop(L, 1);
     end;

   lsd := FindStateDesc (L, TRUE); // Стейт может создаваться многократно с одним указателем, так что лишние заходы нельзя запретить
   Assert ( lsd <> nil, 'Problem with making description' );

   // InstallTopLevelExceptionFilter;
   // безусловная регистрация
   ODS ( Format('[~T/~I/~B]. #DBG: Added L =~C0D $%p~C07, captured in ~C0A %s~C07', [L, sFrom]) );
   ls := L;
   xray_L := nil;

   gCheckPoints.add (1, 1);
   sf := GetPtrInfo ( Addr(lsd.f_realloc) );
   sf := Format('previous frealloc = %s, ud = $%p, ', [sf, lsd.p_realloc]);
   // запоминание функции аллокации
   gs := ls.G;
   Assert ( gs <> nil, 'global_State = nil' );

    if ( Addr (gs.frealloc) <> Addr(lsd.f_realloc) ) or ( gs.ud <> lsd.p_realloc ) then
      begin
       if ( DWORD ( @gs.frealloc) <> $BAADF00D ) then
         begin
          lsd.f_realloc := gs.frealloc;
          lsd.p_realloc := gs.ud;
         end;
       gCheckPoints.add (2, 1);
       ODS ( '[~T].~CF0 #DBG: ' + sf + '~C07' );
       sf := GetPtrInfo ( Addr(gs.frealloc) );
       ODS ( Format(#9#9'~CE0#DBG: updated  frealloc = %s, ud = $%p, ~C07', [sf, gs.ud]) );
      end;

     if (ls.G <> nil) and (g_Config.bInterceptMM) then
       begin
        new_ud := GetAlloc (ls.G.ud);
        ODS ( CFormat('[~T].~C0F #DBG(InterceptMM): Changing frealloc from %p to %p, ud from %p to %p ~C07', '~C0F',
                [ Addr (ls.G.frealloc), Addr (DMAlloc), ls.G.ud, new_ud  ]));
        ls.G.frealloc := @DMAlloc;
        ls.G.ud := new_ud;
        gCheckPoints.add (3, 1);
       end;

   if reg_stuff then
   try
    // Sleep (1);
    // не подключать к левым lua_State

    lua_pushcfunction (L, RegDLLStuff);
    lua_setglobal (L, 'attach_luaicp');
    lua_pushcfunction (L, LuaGetSetGlobalVar);
    lua_setglobal (L, 'GetGlobalVar');
    SetAtPanicHandler (L);

    {

    if lua_cpcalllll ( L, RegDLLStuff, nil ) <> 0 then
      begin
       PrintError ('RegDLLStuff callback failed: ' + LuaStrArg(L, -1) );
       lua_pop (L, 1);
      end; // }

    // RemoveStateDesc (lsd);
   except
    on E: EAccessViolation do
      begin
       ODS( #9#9' CheckPoints dump: ' + gCheckPoints.dump );
       OnExceptLog ('CheckCapture  ' + sf, E, TRUE);
      end;
   end;
 finally
  if top <> lua_gettop(L) then
    begin
     PrintError( Format('captured lua_State stack was modified %d <> %d ', [top, lua_gettop(L)] ) );
     lua_settop (L, top);
    end;
  captured.Unlock;
 end;

 if not is_cap then
    ODS( Format('[~T]. #DBG: Complete CheckCapture for ~C0D$%p~C07', [L]));
end;

procedure ExtCapture (L: lua_State); stdcall;
begin
 try
  CheckCapture (L, 'ExtCapture', FALSE);
 except
  on E: Exception do
     OnExceptLog ('ExtCapture', E);
 end;
end;

function TestXrAPI (L: lua_State): Integer; cdecl;
var
   r: IReader;
   c: PAnsiChar;
begin
 XrRescanPathes;
 r := XrFileOpen('$game_config$', 'system.ltx');
 if r <> nil then
   begin
    wprintf('[~T]. #DBG(TestXrAPI): XrFileOpen returned $%p', [Pointer(r)]);
    Sleep(100);
    c := PAnsiChar (r.data);
    lua_pushboolean (L, AnsiStrings.StrPos(c, '[inventory]') <> nil);
    XrFileClose(r);
    wprintf('   update_path test: %s ', [XrUpdatePath('$game_config$', 'system.ltx')]);
   end
 else
    lua_pushboolean (L, FALSE);


 result := 1;
end;


function RegDLLStuff (L: lua_State): Integer; cdecl;

var
   allow_disp: Boolean;
         disp: Boolean;
        n, gt: Integer;
          udd: TUserDLLDesc;
          ctx: String;





   procedure lua_regfunc (const func: AnsiString; fptr: lua_CFunction; const params: String = '');
   begin
    Assert ( Assigned (fptr), 'very, very bad argument' );

    if Assigned(block_export) and ( block_export.IndexOf ( String(func) ) >= 0 ) then
       begin
        {$IFOPT D+}
         if cap_cnt <= 1 then
            ODS( #9'~C0ERegistering function blocked:    ~C0A' + String(func) + '~C07');
        {$ENDIF}
        exit;
       end;

    LuaRegFunc (L, func, fptr, params, disp and allow_disp);
   end;


begin
 try
  if not gInitialized then
     Main;

  gCheckPoints.init ( 110, 0, 0, 0 );
  luaL_openlibs (L);
  gCheckPoints.add (1, 1);
  luaopen_debug (L);
  gCheckPoints.add (1, 1);

 except
  on E: Exception do
     OnExceptLog ('RegDLLStuff L = ' + FormatPtr (L), E, TRUE);
 end;
 Inc (cap_cnt);
 allow_disp := FALSE;
 disp :=  ( cap_cnt <= 1 ) and ( not InLauncher ) or ( IsDebuggerPresent );

 ctx := 'unknown';

 if lua_gettop(L) > 0 then
  begin
   ctx := LuaStrArg (L);
   allow_disp := lua_toboolean (L, 2);
  end;

 gCheckPoints.add (1, 1);
 lua_getglobal (L, '_G');
 gt := lua_gettop (L);

 wprintf('[~T]. #REG_STUFF: Launcher = $%x, Context = "%s" ', [launcher, ctx]);

 Assert ( lua_type(L, gt) = LUA_TTABLE, 'typeof _G = ' + lua_typeof(L, gt) );

 if dbg_present then
    lua_regfunc ( 'ReplayHard', ReplayHard);

 LuaGetGameLevel (L);

 ExportGameBuild (L);

 LuaTools.ExportAll(L, disp and allow_disp);

 lua_regfunc ( 'AssertIgnore',      LuaAssertIgnore );
 lua_regfunc ( 'SetAtPanicHandler', SetAtPanicHandler);              // TODO: to RunCommand
 lua_regfunc ( 'AtPanicHandler',    AtPanicHandler);
 lua_regfunc ( 'ActivateCheatProtection', ActivateCheatProtection ); // TODO: to RunCommand
 lua_regfunc ( 'ODS',         ODSWrap, '(msg, flags)' );
 lua_regfunc ( 'InfoFmt',     LuaInfoFmt,     '(msg) // format message with tilda tags ' );
 lua_regfunc ( 'PreciseTime', LuaPreciseTime );
 lua_regfunc ( 'CacheFile',   CacheFile );
 lua_regfunc ( 'CacheList',   CacheList );
 lua_regfunc ( 'CalcPtr',     LuaCalcPtr,          '(addr, addr2, opeation) // returned ptr' );
 lua_regfunc ( 'CastToInt',   LuaCastToInt,        '(addr)');
 lua_regfunc ( 'CastToPtr',   LuaCastToPtr,        '(obj) // direct cast *data to lightuserdata ');
 lua_regfunc ( 'CObjectPtr',  LuaCObjectPtr,       '(obj) // get client object pointer ');
 lua_regfunc ( 'SObjectPtr',  LuaSObjectPtr,       '(obj) // get server object pointer ');
 lua_regfunc ( 'CheckPtr',   LuaCheckPtr );
 {$IFDEF NLC}
 lua_regfunc ( 'CheckDir',   LuaCheckDir );
 lua_regfunc ( 'CheckStack', CheckStackOverflow );
 {$ENDIF}
 lua_regfunc ( 'DebugDumpAll', DebugDumpAll );
 lua_regfunc ( 'DumpDir',      LuaDumpDir );
 lua_regfunc ( 'DumpVar',      LuaDumpVar );
 lua_regfunc ( 'DumpMMStat',   DumpMMStat );
 lua_regfunc ( 'ElapsedTime',  LuaElapsed,           '(timer_id)' );
 lua_regfunc ( 'FindSpawnIDS', FindSpawnIDS,        '(name or spawn_id or story_id, s_context)' );
 lua_regfunc ( 'GetDbgVar',    LuaGetSetDbgVar,      '(var_name)' );
 lua_regfunc ( 'SetDbgVar',    LuaGetSetDbgVar,      '(var_name, value, [add_traceback])' );
 lua_regfunc ( 'SetDirectory', LuaSetDirectory );
 lua_regfunc ( 'SetVerbosity', LuaGetSetVerbosity, '(log_verbose_level)' );
 lua_regfunc ( 'GetVerbosity', LuaGetSetVerbosity );
 lua_regfunc ( 'GetGlobalVar', LuaGetSetGlobalVar, '(var_name)' );
 lua_regfunc ( 'SetGlobalVar', LuaGetSetGlobalVar, '(var_name, value)' );

 lua_regfunc ( 'GetThreadAsyncVar', GetThreadAsyncVar );
 lua_regfunc ( 'SetThreadAsyncVar', SetThreadAsyncVar );
 lua_regfunc ( 'ManageThreads', ManageThreads );

 lua_regfunc ( 'ExeParam',    LuaExeParam );
 lua_regfunc ( 'GetLuaState', LuaGetLuaState );
 lua_regfunc ( 'GetProcAddr', LuaGetProcAddr,        '(lib_name, proc_name)        // returned hex-string "$XXXX" or error message ');
 lua_regfunc ( 'GetVarAddr',  LuaGetProcAddr,        '(lib_name, var_name)         // returned hex-string "$XXXX" or error message');
 lua_regfunc ( 'GetVarPtr',   LuaGetVarPtr,          '(lib_name, var_name)         // returned pointer to variable, and error message if fail');
 lua_regfunc ( 'GetVarValue', LuaGetVarValue,        '(lib_name, var_name, [type]) // returned variable, and error message if fail');
 lua_regfunc ( 'LoadFreeLib', LuaLoadFreeLib );
 lua_regfunc ( 'CPUTime',     LuaCPUTime );
 lua_regfunc ( 'DiffPtr',     LuaDiffPtr,             '(addr, addr2) // returned offset = addr2 - addr');
 lua_regfunc ( 'InstallSEH',  LuaInstallSEH );
 lua_regfunc ( 'SetPtrInfo',  LuaSetPtrInfo,          '(addr, description)' );
 lua_regfunc ( 'NearestPtr',  LuaNearestPtr );
 lua_regfunc ( 'CopyFile',    LuaCopyFile );
 lua_regfunc ( 'RenameFile',  LuaRenameFile );
 lua_regfunc ( 'FileExists',  LuaFileExists );
 lua_regfunc ( 'FileLoad',    LuaFileLoad,              '(file_name, rtype = content/lines)' );
 lua_regfunc ( 'FileWrite',   LuaFileWrite,             '(file_name, content, content_size, params_str)');
 lua_regfunc ( 'FileVersion', LuaFileVersion );
 lua_regfunc ( 'FindFiles',   LuaFindFiles );
 lua_regfunc ( 'VerifyFiles', LuaVerifyFiles,           '() // comparing files in gamedata.db? packs and gamedata.open folder' );
 lua_regfunc ( 'flushlog',    LuaFlushLog );


 lua_regfunc ( 'KeyPressed',     LuaKeyPressed );
 lua_regfunc ( 'KeyCombPressed', LuaKeyCombPressed );
 lua_regfunc ( 'GetModuleInfo',  LuaModuleInfo );
 lua_regfunc ( 'GetModuleSize',  LuaModuleSize );
 lua_regfunc ( 'GetMousePos',    LuaMousePos );
 lua_regfunc ( 'LuaCleanup',     LuaCleanup );                         // TODO: to RunCommand
 lua_regfunc ( 'ReplaceMT',      LuaReplaceMT,             '(obj, metatable)');
 lua_regfunc ( 'RunCommand',     LuaRunCommand );


 lua_regfunc ( 'TBHash',     LuaTBHash,                    '() // return traceback hash' );
 lua_regfunc ( 'TracePtr',   LuaTracePtr );
 lua_regfunc ( 'TickMod',    LuaTickMod );

 lua_regfunc ( 'RayTrace',   LuaRayPick,                  '(start, dir, range, target, ignore_obj_id)' );
 lua_regfunc ( 'CopyDMA',    LuaCopyDMA,                  '(dest_addr, src_addr, cb)' );
 lua_regfunc ( 'ReadDMA',    LuaReadDMA,                  '(src_addr, offset, vtype, [dump_size])' );
 lua_regfunc ( 'ReadPtr',    LuaReadPtr,                  '(src_addr, offset) // faster vs ReadDMA ' );
 lua_regfunc ( 'WriteDMA',   LuaWriteDMA,                 '(dest_addr, offset, value, vtype)' );
 lua_regfunc ( 'UnlockDMA',  LuaUnlockDMA,                '(dest_addr, offset)' );
 lua_regfunc ( 'GetTempBuff', LuaGetTempBuff,             '(index) // returns on from 16 buffers, with 1024b size');

 lua_regfunc ( 'ReadLn',     LuaReadLn );
 lua_regfunc ( 'ReplaceStr', LuaReplaceStr );

 lua_regfunc ( 'NotifyReleased', NotifyReleased,        '(id, [context])' );
 lua_regfunc ( 'NotifySpawned',  NotifySpawned,         '(id)');

 lua_regfunc ( 'NearestVertexInfo',  NearestVertexInfo );
 lua_regfunc ( 'LoadVertexList',     LoadVertexList );
 lua_regfunc ( 'SaveVertexInfo',     SaveVertexInfo );
 lua_regfunc ( 'SaveVertexList',     SaveVertexInfo );



 lua_regfunc ( 'Random',          LuaRandom );
 lua_regfunc ( 'RandomHash',      LuaRandomHash );
 lua_regfunc ( 'SetBreakpoint',   LuaSetBreakpoint );
 lua_regfunc ( 'SendAlert',       LuaSendAlert,         '(msg) // send warning message to launcher');
 lua_regfunc ( 'SleepEx',         LuaSleepEx );
 lua_regfunc ( 'SoundMsg',        LuaSoundMsg );

 lua_regfunc ( 'SetGameState', LuaSetGameState );
 lua_regfunc ( 'SetTraceHook', LuaSetHook );
 lua_regfunc ( 'SetThreadAM', LuaSetThreadAM );

 lua_regfunc ( 'FindSubStr', LuaFindSubStr );
 lua_regfunc ( 'StrToVar', StrToVar );
 lua_regfunc ( 'VarToStr', VarToStr );
 lua_regfunc ( 'ReadIni',  ReadIni );
 lua_regfunc ( 'WriteIni', WriteIni );
 lua_regfunc ( 'CheckInList', CheckInList );

 // lua_regfunc ( 'LCSetPath', LCSetPath);
 lua_regfunc ( 'ExpandPath', LuaExpandPath );
 lua_regfunc ( 'ExitProcess', LuaExitProcess );                  // TODO: to RunCommand

 lua_regfunc ( 'PushCommand', PushCommand );
 lua_regfunc ( 'ProcessCommands', ProcessCommands );
 lua_regfunc ( 'ProcessKeyboard', ProcessKeyboard );
 lua_regfunc ( 'SetKbdCallback',  SetKbdCallback );
 lua_regfunc ( 'DelKbdCallback',  DelKbdCallback );

 lua_regfunc ( 'FindConfigStr', XrayFindConfigStr );
 lua_regfunc ( 'LuaSafeCall',   LuaSafeCall );
 lua_regfunc ( 'LuaPopValues',  LuaPopValues );
 lua_regfunc ( 'PtInPolygon',   PtInPolygon );
 lua_regfunc ( 'SetBootMsg',    LuaSetBootMsg );
 lua_regfunc ( 'SetUpdTimeout', SetUpdTimeout );
 lua_regfunc ( 'SetPointerMT',  LuaSetPointerMT );


 lua_regfunc ( 'RegistryFilter',   RegistryFilter,   '(cmd, [list or filter], [value1], [value2]...)' );
 lua_regfunc ( 'InvokeRegistry',   InvokeRegistry,   '(cmd, [id], [ptr])');
 lua_regfunc ( 'ProfileRegistry',  ProfileRegistry,  '(test_count');
 lua_regfunc ( 'UpdateRegistry',   UpdateRegistry,   '(b_fast)');
 lua_regfunc ( 'GameObjectInfo',   LuaObjectInfo );


 lua_regfunc ( 'GetTableType',    LuaTableType, '(table_var) // return: 0 - not table, 1 - array, 2 - map' );
 lua_regfunc ( 'TexCleanTotal',   LuaTexClean );
 lua_regfunc ( 'TexCreate',       LuaTexCreate, '(tex_name)' );
 lua_regfunc ( 'TexFindList',     LuaTexFind,   '(tex_name_mask)' );
 lua_regfunc ( 'TexFile',         LuaTexFile,   '(tex_ptr, tex_name) // WARN: tex_name must be lenght <= original' );
 lua_regfunc ( 'TexLoad',         LuaTexLoad,   '(tex_ptr)' );

 lua_regfunc ( 'IPCQueue',  Push_IPCQueue,  '() // pushing instance of IPC-queue, only game side');
 lua_regfunc ( 'IPCThread', Push_IPCThread, '() // pushing instance of IPC-thread, only launcher side');
 lua_regfunc ( 'CUIStaticEx', _CUIStatic,   '() // make alternate CUIStatic object');
 lua_regfunc ( 'TestXrAPI', TestXrAPI);


 FileWorks.AddGlobals (L);
 XrayExt.AddGlobals     (L, allow_disp);
 EasyECDSA.AddGlobals   (L, FALSE);
 if dev_comp then
   begin
    lua_regfunc ( 'LuaCompile', LuaCompile );
    lua_regfunc ( 'LuaLoadBC', LuaLoadBC );
   end;


 {$IFDEF NLC}
  {$IFOPT D+}
  {$ENDIF}

  lua_regfunc ( 'FastPackStr',  LuaStringPack, '(table) // support only binary/hex translation ' );
  lua_regfunc ( 'FastParseStr', LuaStringUnpack, '(str, at, alg) // default alg = "amk" ' );

  if gt > 0 then
     lua_remove ( L, gt );

  Singularity.AddGlobals (L);
 {$ENDIF}


 LuaWebPacket.ExportAll (L, allow_disp);
 LuaTextFile.ExportAll  (L, allow_disp);

 if not CheckAtPanic (L, AtPanicHandler) then
    SetAtPanicHandler (L);

 {$IFOPT D+}
 if (cap_cnt <= 1) and (usr_dlls.Count > 0) then
     ODS('[~T]. #DBG: Processing user DLLs registration for lua_State...');
 {$ENDIF}
 allow_disp := FALSE;
 lua_regfunc ('ApplyRelocations', ApplyRelocations, '(block, code, delta) // low-level experiment routine');

 LuaTraceback (L);
 gCheckPoints.init ( 120, 0, 0, 0 );

 for n := 0 to usr_dlls.Count - 1 do
  begin
   udd := TUserDLLDesc ( usr_dlls.Objects [n] );
   if udd = nil then continue;
   udd.lua_icpfunc (L);
  end;

 gCheckPoints.init ( 130, 0, 0, 0 );

 Sleep (300);
 CheckInstallVEHFlt (FALSE, LuaVEHandler);


 lua_pushptr ( L, @_first_var );
 // SetPointerMT ( L, -1);
 result := 1;
end; // CheckCapture

// функция пост-вызова. Перед ней был осуществлен вызов оригинального lua_newstate
function on_nsexec(L: lua_State): lua_State; stdcall;
begin
 CheckCapture(L, 'on_nsexec', FALSE);
 result := L;
end; // on_nsexec

function on_ntexec(L: lua_State): lua_State; stdcall;
begin
 CheckCapture(L, 'on_ntexec', FALSE);
 result := L;
end; // on_ntexec



procedure pre_logstring; stdcall;
var
   ftxt: Text;
   sFile: String;
   L: lua_State; ps: PAnsiChar;
begin
 asm
  mov eax, [ebp + 12]  // first param
  mov L, eax
  mov eax, [ebp + 16]  // second param
  mov ps, eax
 end;

 if (ps = nil) or (ps = 'Update') then exit;

 sFile := InfoFmt(ExePath + 'load_~I.log');
 {$I-}
 AssignFile(ftxt, sFile);
 if FileExists(sFile) then Append(ftxt) else ReWrite(ftxt);
 if IOresult = 0 then
  begin
   WriteLn(ftxt, InfoFmt(Format('[~T]. #LL: L = $%p, P = $%p ----------------------- ', [L, Pointer(ps)])) );
   WriteLn(ftxt, ps);
   CloseFile(ftxt);
  end;
end; // pre_loadstring


function wrap_newstate (f: lua_Alloc; ud: Pointer): lua_state ; cdecl;
begin
 result := nil;
 if Assigned(lua_ns_orig) then
   result := lua_ns_orig(f, ud);
 CheckCapture(result, 'lua_newstate', FALSE);
end;


function wrap_newstate_l: lua_State; cdecl;
begin
 result := nil;
 if Assigned(luaL_ns_orig) then
   result := luaL_ns_orig();
 CheckCapture(result, 'luaL_newstate', FALSE);
end;

{function wrap_newthread(l: lua_State): lua_State; cdecl;
begin
 result := lua_nt_orig(l);
 CheckCapture(l, 'lua_newthread#1', FALSE);
 CheckCapture(result, 'lua_newthread#1', FALSE);
end;}

var
   pcall_result: Integer;
function imm_pcall(L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl;
asm
 db 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h // for prolog
 jmp DWORD PTR [lua_pcall_next]
end;

type
  Tpcall_params = record
   eip: Pointer;
     L: lua_State;
    na: Integer;
    nr: Integer;
    ef: Integer;
  end;

  Ppcall_params = ^Tpcall_params;



procedure wrap_pcall( pp: Ppcall_params ); stdcall;

begin
 pcall_L := pp.L;  // просто запомнить
 CheckCapture ( pp.L, 'lua_pcall', FALSE);
 try
  ODS('[~T]. #DBG(wrap_pcall): pfunc type = ' + IntToStr ( lua_type(pp.L,  -1) ) );

  pcall_result := imm_pcall ( pp.L, pp.na, pp.nr, pp.ef);
 except
  on E: Exception do
   OnExceptLog('lua_pcall', E);
 end;
 pcall_L := nil;
end; // wrap_pcall

{  mov  eax, [ebp + 16]       // err_func  = 3
  push eax
  mov  eax, [ebp + 12]       // nresults  = 2
  push eax
  mov  eax, [ebp + 08]       // nargs     = 1
  push eax
  mov  eax, [ebp + 04]       // lua_State = 0
  push eax
  mov  eax, [ebp + 00]        // eip
  push eax
}


function on_pcexec (L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl;
const
     stack_resv = 28; // + ebp + pushad = 64
asm
 pushad
 lea  ebp, [esp + 24h]
 sub  esp, stack_resv
 mov  edi, esp
 mov  ecx, ( stack_resv / 4 )
 xor  eax, eax
 rep  stosd   // make zero stack window
 push ebp
 call wrap_pcall
 add  esp, stack_resv

 popad
 mov  eax, pcall_result
 // db  90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h, 90h

end;



function wrap_type(L: lua_State; idx: Integer): Integer; cdecl;
begin
 result := 0;
 CheckCapture(L, 'lua_type', TRUE);
 if Assigned(lua_type_orig) then
    result := lua_type_orig(L, idx);
end;



var
   allow_repatch: Boolean = FALSE;

type
    TReadBuff = record
     ofs, size: DWORD;
     data: array [0..1023] of AnsiChar;
    end;
    PReadBuff = ^TReadBuff;


function TestReader( L: lua_State; pdata: Pointer; psz: PDWORD): PAnsiChar; cdecl;
var prb: PReadBuff;
begin
 prb := pdata;
 if prb.ofs >= prb.size then
   begin
    result := nil;
    psz^ := 0;
   end
 else
   begin
    result := @prb.data[prb.ofs];
    psz^ := prb.size;
    prb.ofs := prb.size;
   end;
end; // TestReader




function SubAddr (a, b: Pointer): DWORD;
begin
 result := DWORD (a) - DWORD (b);
end;

procedure ExecLuaScript(script_name: String);

 var
    L: lua_State;
    i: Integer;
 begin
  if (script_name = '') then exit;
  if (not FileExists (script_name)) then
    begin
     PrintError('Not found init-script ~C0F' + script_name + '~C07');
     exit;
    end;

  L := luaL_newstate;
  try
   try
    luaL_openlibs (L);
    luaopen_debug (L);
    CheckCapture (L, 'ExecLuaScript', FALSE);
    RegDLLStuff (L);
    // TestXrAPI (L);

    i := luaL_loadfile(L, PAnsiChar ( AnsiString (script_name) ) );
    if i = 0 then
       i := lua_pcall(L, 0, LUA_MULTRET, 0);

    if i <> 0 then
       ODS ('[~T].~C0C #ERROR: Executing ' + script_name + ' with lua_pcall returned error ~C0F' + LuaStrArg (L, -1) + '~C07'  );
   except
    on E: Exception do
       OnExceptLog ('ExceptInitScript(' + script_name + '): ', E);
   end;
  finally
   lua_close (L);
  end;

 end; // ExecLuaScript


 procedure ExecInitScript; stdcall;
 begin
  ODS('[~T]. #DBG: Executing~C0A ' + init_script + '~C07...');
  ExecLuaScript(init_script);
  ODS('[~T]. #DBG: ExecInitScript completed.');
 end;




function FindGameLuaState: lua_State;
var
   L: lua_State;
   v, t, n: Integer;
   res: Boolean;
   lsd: TLuaStateDesc;
begin
 result := nil;
 if Assigned(GameLua) then
    result := GameLua();
 if Assigned(AuxLua) then
    aux_L := AuxLua();

 if result <> nil then exit;

 for n := 0 to captured.Count - 1 do
  begin
   lsd := TLuaStateDesc ( captured.Objects [n] );
   if lsd = nil then continue;
   L := lsd.Lparent;
   if (lsd.ThreadID <> GetCurrentThreadID) or (L = nil) then continue;

   t := lua_gettop (L);
   try

    lua_getglobal (L, 'level'); // push table to stack
    v := lua_type (L, -1);

    if v = LUA_TTABLE then
     try
      lua_getfield (L, -1, 'present');
      if lua_type (L, -1) = LUA_TFUNCTION then
        begin
         res := FALSE;
         if lua_pcall (L, 0, LUA_MULTRET, 0) <> 0 then continue;
         v := lua_type (L, -1);
         case v of
          LUA_TBOOLEAN: res := lua_toboolean (L, -1);
           LUA_TNUMBER: res := ( lua_tointeger (L, -1) <> 0 );
         end; // case
         if res then
                result := L;
        end;

     except
      on E: Exception do
         PrintError('Exception catched in FindGameLuaState: ' + E.Message);
     end;

    finally
     lua_settop (L, t); // correct stack
    end; // try-fin
   if result <> nil then break;

  end; // for
 if result <> nil then
    ODS('[~T]. #DBG: Game lua_State detected = ~C0D ' + Format('$%p', [result]) + '~C07');
end;

var
    loaded: Boolean = TRUE;

procedure CheckBinderHang (const from: String; ExtraTime: Double);
var
     ctx: TContext;

begin
 if IsDebuggerPresent then
    exit;
 if (PreciseTime > bu_last + 5 * DT_ONE_SECOND) and ( global_pt.Elapsed(6) >= 1000 ) then
  begin
    wprintf('[~T].~C0C #HANG_DETECT:~C07 binder update last %s, delay = %.1f ms ',
                [FormatDateTime('hh:nn:ss', bu_last), global_pt.Elapsed(5)]);
    global_pt.StartOne(6);
  end;

 if ( upd_timeout > 0 ) and ( global_pt.Elapsed (5) >= 1.0 * upd_timeout + ExtraTime ) and ( not game_paused ) then
 else exit;

 PlayBeep (1500, 300);
 StalkerODS ('!~C0C[~T]. #WARN(' + from + '): Превышение таймаута ' + Format('(%.0f > %d)', [global_pt.Elapsed (5), upd_timeout]) +
                    ' actor_binder:update, последний вызов SetUpdTimeout ' + FormatDateTime('hh:nn:ss.zzz', bu_last), 255);
 if game_L <> nil then
   begin
    lua_getglobal(game_L, 'last_upd_time');
    if lua_isstring(game_L, -1) then
       wprintf(' scripts _G.last_upd_time = %s', [LuaStrArg(game_L, -1)]);

    lua_pop(game_L, 1);
   end;

 // PlaySnd ('$game_sounds$\msgs\binder_timeout.wav');
 global_pt.StartOne (5);

 DebugDumpAll (nil);
 ctx.ContextFlags := CONTEXT_FULL;
 if ( h_main_thread <> 0 ) and ( GetThreadContext (h_main_thread, ctx) ) then
 with ctx do
  try
   ODS( CFormat( '~C0F[~T/~U]. #DBG(Main thread context): EAX = $%8.x EBX = $%8.x ECX = $%8.x EDX = $%8.x EDI = $%8.x ESI = $%8.x EBP = $%8.x ESP = $%8.x EIP = $%8.x ~C07',
                 '~C0F', [eax, ebx, ecx, edx, edi, esi, ebp, esp, eip] ) );
   ODS ( '[~T]. #DBG:~C0F Unwinding stacks...~C07' );
   ODS ( DumpProcessStack ( GetCurrentProcess, ctx, nil ) );
  except
   on E: Exception do PrintError ('Exception catched in OnTimer: ' + E.Message);
  end;



end;

var rtp_ticks: Integer;
       prv_lt: TDateTime;

procedure TT_regular;
begin
 if parse_log then
    CheckCommandsInLog;

 CheckBinderHang ('TT.OnTimer',  5000);
end;



procedure RegularTimerProc (h: HWND; uMsg, idEvent, dwTime: DWORD); stdcall;
const
   lua_func = 'RegularTimerProc';
var
   // func_exist: Boolean;
   lt: TDateTime;
    e: Double;

begin
 //  ODS( CFormat('[~T/~i]. #DBG: RegularTimer proc called. hWnd = $%x, dwTime = $%x ', '~C07', [h, dwTime]));
 // Sleep (5);
 Inc (not_frozen, 100);

 if game_paused then exit;

 g_perf.StartOne();
 Inc (rtp_ticks);

 e := g_perf.Elapsed();
 if e > 1 then wprintf('[~T].~C0B #PERF_WARN(RegularTimerProc):~C07 elapsed #1 = %.1f ms, rtp_ticks =~C0D %d~C07', [e, rtp_ticks]);


 CheckBinderHang ('RTP', 0);

 e := g_perf.Elapsed();
 if e > 1 then wprintf('[~T].~C0B #PERF_WARN(RegularTimerProc):~C07 elapsed #2 = %.1f ms, rtp_ticks =~C0D %d~C07', [e, rtp_ticks]);

 if rtp_ticks and $FF = 0 then
    begin
     mt_stack_top := GetStackTop;
     mt_stack_btm := GetStackBottom;
     active_wnd := GetActiveWindow ();
     fgrnd_wnd  := WindowFromPoint ( Point (100, 100) );
    end;



 if bShowPulse then
    ODS('[~T/~B]. #PULSE: rtp_ticks = ' + IntToStr(rtp_ticks));

 inner_cmdl.Clear;

 if game_con_log and (rtp_ticks and $3FF = 3) then // 20000 msec period
    DumpConsoleBuff(nil);

 e := g_perf.Elapsed();
 if e > 1 then wprintf('[~T].~C0B #PERF_WARN(RegularTimerProc):~C07 elapsed #3 = %.1f ms, rtp_ticks =~C0D %d~C07', [e, rtp_ticks]);

 if (rtp_ticks and $F <> 11) then exit;

 lt := misc.RealTime (TRUE) ;
 // check one minute pass
 if ( lt - prv_lt > DT_ONE_MINUTE ) then
   try
    prv_lt := lt;
    FlushLog;
   except
    on E: Exception do PrintError ('Exception catched in RegularTimerProc#0:' + E.Message);
   end;

 e := g_perf.Elapsed();
 if e > 1 then wprintf('[~T].~C0B #PERF_WARN(RegularTimerProc):~C07 elapsed #4 = %.1f ms', [e]);
end;

{ TUserDLLDesc }

destructor TUserDLLDesc.Destroy;
begin
 ODS ( Format('[~T]. #DBG: Unloading library ~C0D $%X~C07', [hDLL] ) );
 try
  FreeLibrary (hDLL);
 except
  on E: Exception do
    OnExceptLog (ClassName + '.Destroy', E);
 end;

end;

{ TLUAStateDesc }

function TLUAStateDesc.AllocTLS(lct: lua_State; ThreadID: DWORD): PThreadLuaState;
var n: Integer;
begin
 result := nil;
 for n := 0 to High (tls_list) do
  if ( tls_list [n].TID = 0 ) or ( tls_list [n].TID = ThreadID ) then
   begin
    result := @tls_list [n];
    result.TID := ThreadID;
    result.Lchild := lct;
    ODS('[~T]. #DBG: for parent lua_State ~C0D' + FormatPtr (Lparent) +
        '~C07 associated thread-state ~C0D' + FormatPtr(lct) + '~C07, ThreadId = ~C0D' + IntToStr(ThreadID) + '~C07');
    exit;
   end;
 Assert (result <> nil, 'No more slots for thread lua_State pointers');
end; // AllocTLS

procedure TLUAStateDesc.Clear;
var
   ref: TLUAStateDesc;
     n: Integer;
begin
 for n := 0 to High (tls_list) do
   with tls_list [n] do
    begin
     TID := 0;
     key[0] := #0;
     if Lchild = nil then continue;
     Inc (gLuaThreadRemoved);
     ref := FindStateDesc (Lchild, FALSE); // если пришла звезда, то дочерним уж наверняка тоже
     if (ref <> nil) and (ref <> self) then
       begin
        wprintf('[~T]. #DBG: clearing child desc %s for parent %s ', [ref.FName, FName]);
        ref.Clear;
       end;

     Lchild := nil;
    end;
 Unregister;
end;

constructor TLUAStateDesc.Create(lpt: lua_State);
begin
 FLparent := lpt;
 FThreadID := GetCurrentThreadID;
 captured.AddObject ( Format('$%p', [lpt]), self );
 Register;
end;

destructor TLUAStateDesc.Destroy;
var i: Integer;
begin
 Clear;
 ODS ( Format ('[~T/~i]. #DBG: destroyed lua_State registry $%p for Lparent = ', [Pointer(self)]) + FormatPtr (Lparent));
 inherited;
end;

procedure TLUAStateDesc.Dump;
var n: Integer;
begin
 ODS ('[~T]. #DBG: Dumping lua_State registry [' + Format('$%p', [Pointer(self)]) + '] for Lparent = ' + FParentName);
 for n := 0 to High (tls_list) do
   with tls_list [n] do
     if TID <> 0 then
         wprintf (' tls_list[%d]: TID = %d, Lchild = $%p (%s) ', [n, TID, Lchild, String(key)]);
end;

function TLUAStateDesc.FindTLS (lct: lua_State; ThreadID: DWORD): PThreadLuaState;
var n: Integer;
begin
 result := nil;
 for n := 0 to High (tls_list) do
   // поиск слота, по потоку или по дочернему lua_State
   if (   ( tls_list [n].Lchild = lct ) and ( lct <> nil )   ) or
      (   ( tls_list [n].TID = ThreadID ) and ( ThreadID <> 0 ) ) then
        begin
         result := @tls_list [n];
         exit;
        end;
end; // FindTLS

function _OnLuaStateDescGC(L: lua_State): Integer; cdecl;
var
   lsd: TLUAStateDesc;
begin
 lsd := lua_objptr(L, 1);
 if (lsd <> nil) and (lsd.Lparent = L) and (captured.IndexOfObject (lsd) >= 0) then
    begin
     wprintf('[~T]. #DBG: garbage collection performed for LuaStateDesc object $%p (%s) ', [Pointer(lsd), lsd.FName]);
     RemoveStateDesc(lsd);
    end;

 result := 0;
end;


procedure TLUAStateDesc.Register;
var
    idx: Integer;
begin
 //
 lua_getglobal (Lparent, LSC_THREADS); // она все ещё нужна
 if lua_istable (Lparent, -1) then
    lua_pop(Lparent, 1)
 else
   begin
    ODS('[~T].~C0F #DBG: creating threads registry table. ~C07');
    lua_pop (Lparent, 1);
    lua_createtable (Lparent, 0, 0);
    lua_setglobal (Lparent, LSC_THREADS);
   end;


 FParentName := 'unknown_' + FormatPtr(Lparent);

 if Assigned(get_lvm_name) then
    FParentName := AnsiTrim2W ( get_lvm_name (Lparent) );

 if Pos('unknown_', FParentName) > 0 then
  begin
   if Lparent = FindGameLuaState then
      FParentName := 'game_lua';
   if Lparent = aux_L then
      FParentName := 'aux_lua';
  end;


 FName := 'LSD_' + FParentName + '@' + IntToStr(GetCurrentThreadId);

 lua_getglobal (Lparent, FName);
 if lua_isuserdata(Lparent, -1) then
   begin
    lua_pop(Lparent, 1);
    exit;
   end;
 lua_pop(Lparent, 1); // remove nil

 wprintf('[~T]. #DBG: performing registration %s for lua_State %s ', [FName, FParentName]);
 lua_pushobj(Lparent, self);
 idx := lua_gettop(Lparent);
 lua_createtable   (Lparent, 0, 0);
 lua_pushcfunction (Lparent, _OnLuaStateDescGC);
 lua_setfield      (Lparent, idx + 1, '__gc');
 lua_setmetatable  (Lparent, idx);
 lua_setglobal     (Lparent, FName);
 while lua_gettop(Lparent) >= idx do
       lua_pop           (Lparent, 1);
end;



function TLUAStateDesc.SpawnThread: lua_State;
var
   tls: PThreadLuaState;
     k: String;
     t: Integer;


begin
 result := nil;
 // новый луа-поток под отдельный виндовый поток
 ODS('[~T/~I]. #DBG: For current thread not associated thread(child) lua_State - creating new = ' + FormatPtr(result));
 Dump;
 k := 'L' + IntToStr(InterlockedIncrement (gLuaThreadCounter));

 in_func := 'SpawnThread#1';
 // чтобы сборщик мусора не прибрал новый стейт, надо его сохранить в глобальной табличке
 lua_getglobal (Lparent, LSC_THREADS);
 t := lua_gettop (Lparent);
 Assert (lua_istable(Lparent, t), LSC_THREADS + ' is not table');
 result := lua_newthread (Lparent);
 lua_setfield    (Lparent, t, PAnsiChar(AnsiString(k)));
 lua_pop         (Lparent, 1);

 in_func := 'SpawnThread#2';
 tls := AllocTLS (result, GetCurrentThreadId );
 StrPCopy (tls.key, k);
 if Assigned(set_lvm_name) then
    set_lvm_name(result, AnsiArg(k));


 t := lua_gettop (Lparent) - t + 1;
 if t <> 0 then ODS('[~T]. #WARN: set lsc_threads table item, stack diff = ' + IntToStr(t));
end;

procedure TLuaStateDesc.Unregister;
var
   Lweak: lua_State;
       i: Integer;
begin
 captured.Lock (ClassName + '.Destroy');
 try
  i := captured.IndexOfObject (self);
  if i >= 0 then
    begin
     wprintf ('[~T]. #DBG: Hiding %s object in captured list. Index = %d ', [FName, i]);
     captured.Objects [i] := nil;
     captured[i] := '~' + captured[i];
    end;
 finally
  captured.Unlock;
 end;

 if Lparent <> nil then
 try
  Lweak := Lparent;
  FLparent := nil;
  wprintf('[~T]. #DBG: performing unregister %s for lua_State %s', [FName, FParentName]);
  lua_delglobal(Lweak, LSC_THREADS); // убрать связи с потоками
  lua_delglobal(Lweak, FName);
  // lua_gc (Lweak, LUA_GCCOLLECT, 0);
  // lua_gc (Lweak, LUA_GCCOLLECT, 0);
 except
  on E: Exception do
     PrintError('Exception catched in TLUAStateDesc.Unregister for Lparent = ' + FParentName);
 end;

end;



procedure on_unload (Sender: TModuleDescriptor); stdcall; // ранний колбек  на выгрузку DLL
var
   n: Integer;
begin
  if not loaded then exit;
  loaded := FALSE;
  ODS ('[~T/~i]~C0F Processing LUACAP unload!~C07');

  if Assigned (gIPCThread) then
   begin
    gIPCThread.StopThread ();
    gIPCThread.WaitStop ();
    FreeAndNil (gIPCThread);
   end;

  for n := 0 to Length (alloc_list) - 1 do
     try
      FreeAndNil (alloc_list [n].fma); // TODO: possible process may be crash!
     except
      on E: Exception do
         OnExceptLog ('on_unload#1', E);

     end;

  SetLength (alloc_list, 0);

  // GlobalFinalize;
  try
   if gToolThread <> nil then
    begin
     gToolThread.FreeOnTerminate := TRUE;
     gToolThread.StopThread;
    end;

   ODS ('[~T]. #DBG: Finalizing last stage...');
   Sleep(50);


   n := 0;

   gToolThread := nil;

   if Assigned (captured) then
   try
    captured.Clear;
   except
     on E: Exception do
        OnExceptLog ('on_unload#2', E);

   end;


   if Assigned (auto_free) then
   while (auto_free.Count > 0) do
    try
     ODS ( CFormat ('[~T]. #DBG(%d): Releasing object %s at $%p', '~C07', [n, auto_free [0].ClassName, Pointer (auto_free [0])] ) );
     auto_free.Delete (0);
     Inc (n);
    except
     on E: Exception do
        OnExceptLog ('on_unload#3', E);
    end;

   // для всплытия обращений к удаленным объектам!!!!!
   for n := 0 to auto_nil.Count - 1 do
       PPointer (auto_nil [n])^ := nil;

   FreeAndNil (auto_free);
   FreeAndNil (auto_nil);
   FinalizeModule('~ALL');

  except
   on E: Exception do OutputDebugString (PChar('Exception catched in LibProc: ' + E.Message));
  end;

end; // cb_unload


procedure Test_pcall(L: lua_State);
var

  t: Integer;
begin
 t := lua_gettop(L);
 L := lua_newstate ( DMAlloc, nil );
 luaL_loadstring (L, 'ODS("~CE0 Hello DWORDL!!!~C07")');
 lua_pcall ( L, 0, -1, 0 );
 lua_settop(L, t);
end;

procedure Patch_pcall;
var
   pb: PByteArray;
   ps: Integer;    // PROLOG_SIZE = 6
    s: String;
    n: Integer;
begin
 pb := lua_pcall_p;

 case pb[0] of
  $55: ps := 6;
  $8B: ps := 7;
  else ps := 0;
 end; // case

 if ps = 0 then exit;

 s := '';
 for n := 0 to ps - 1 do
     s := s + ' ' + IntToHex ( pb[n], 2 );

 ODS('[~T]. #DBG: lua_pcall prolog dump ~C0F' + s + '~C07');
 if ( s <> '55 8B EC 83 E4 F8' ) and ( s <> '8B 44 24 10 83 EC 08' ) then exit; // checks for 2947ru and 2945 xrLua.dll

 lua_pcall_next := RelativePtr (lua_pcall_p, ps );
 lua_pcall_w := @on_pcexec;
 UnlockRegion ( lua_pcall_p );
 UnlockRegion ( lua_pcall_w );

 Move ( lua_pcall_p^, Addr(imm_pcall)^, ps ); // save prolog bytes

 // патч оригинальной функции: переход на мой враппер
 Write_PushDWORD ( lua_pcall_p, lua_pcall_w, FALSE );    // push  addr  = 5 bytes
 Move ( op_ret, RelativePtr(lua_pcall_p, 5)^, 1 );       // ret

 // Test_pcall ;
end;


procedure MM_StressTest;
var
     p: array [0..65535] of Pointer;
     i: Integer;


begin
 if true then exit;
 ODS('[~T/~B]. #PERF: performing MM test...');
 for i := 0 to High (p) do
     GetMem (p[i], i + 1);
 ODS('[~T/~B]. #PERF: allocation complete!');
 for i := 0 to High (p) do
     FreeMem (p[i]);
 ODS('[~T/~B]. #PERF: de-allocation complete!');
 ReadLn;
end;

procedure ActivateRuntimeScript;
const
   NAME = '_runtime_';
var
   ht: THelperThread;
begin
 if not FileExists(rtm_script, FALSE) then exit;

 CheckInstallVEHFlt ( FALSE, LuaVEHandler );

 ht := THelperThread ( ht_list.FindObject(NAME) );
 if ht = nil then
   begin
    ht := THelperThread.Create(FALSE, NAME);
    ht.WaitStart();
    ht_list.AddObject(NAME, ht);
   end;

 ht.ScriptLines.LoadFromFile(rtm_script);
 ht.AddRequest('LOAD_SCRIPT');
 ht.AddRequest('EXEC_SCRIPT');
end;


procedure AllocGlobals;
begin
 auto_nil  := TList.Create;
 auto_free := TObjectList.Create (TRUE);

 if g_timer = nil then
    g_timer := TVirtualTimer.Create;
 // g_timer.TestPrecision(500);
 g_panic := AtPanicHandler;

 captured := TStrMap.Create;
 captured.OwnsObjects := FALSE;
 add_garbage (captured);

 global_pt := TProfileTimer.Create;
 add_garbage (global_pt);
 g_perf := TProfileTimer.Create;
 add_garbage (g_perf);

 if gToolThread = nil then
   begin
    gToolThread := TToolThread.Create (TRUE, 'gToolThread', TRUE);
    ODS('[~T]. #DBG: Created ToolThread, ID = ' + IntToStr (gToolThread.ThreadID));

    gToolThread.Start;
    gToolThread.WaitStart(13);
    gToolThread.AddRequest('POST_INIT');
    gToolThread.WaitRequests(120);
    gToolThread.AddRequest('UPDATE_MAPS');
    {$IFDEF NLC}
    Singularity.worker := gToolThread;
    {$ENDIF}
    // gUpdater := TTimeUpdater.Create (FALSE, 'gUpdater');
   end;


 gInitialized := TRUE;

 ptr_infos := TObjectStorage.Create;
 add_garbage (ptr_infos);
 ptr_infos.DefaultSearch := CmpPtrObj;
 ptr_infos.DefaultCompare := CmpPtrInfos;


 ptr_obj := TPtrInfo.Create (nil, 'nil');
 add_garbage (ptr_obj);



 FillChar (key_state_map, sizeof (key_state_map), 0);


 if InXray then
    CloseConsole;

 func_blacklist := TList.Create;
 add_garbage (func_blacklist);

 destr_log := TStrMap.Create();
 add_garbage (destr_log);

 usr_dlls := TStrMap.Create();
 usr_dlls.OwnsObjects := TRUE;
 add_garbage (usr_dlls);

 removed_list := TStrMap.Create;
 add_garbage (removed_list);

 lock_objs := TStrMap.Create(auto_free);
 lock_objs.OwnsObjects := TRUE;
 add_garbage (lock_objs);

 ht_list := TStrMap.Create(auto_free);
 ht_list.OwnsObjects := TRUE;
 add_garbage (ht_list);

 inner_cmdl := TStrMap.Create(auto_free);
 add_garbage (inner_cmdl);

 _conmsgs := TStrMap.Create();
 add_garbage (_conmsgs);

 global_pt.Start ($FFFFFFF);

 upd_timeout := 0;

 block_export := TStrMap.Create;
 block_export.CaseSensitive := FALSE;
 add_garbage (block_export);

 kcb_list := TObjectList.Create (TRUE);
 add_garbage (kcb_list);

 trace_log := TStrMap.Create (auto_free);
 add_garbage (trace_log);

end;

procedure ReInit; stdcall;
var
   flist: TFileList;
    path: String;
       i: Integer;
begin
 FSGame.Free;
 FSGame := ParseFSGame; // possible profile changed
 if script_dirs = nil then
   begin
    script_dirs := TStrMap.Create();
    script_dirs.Duplicates := dupIgnore;
    add_garbage (script_dirs);
   end;


 if (xray_log_path = '') and (FSGame.Count > 1) then
   begin
    xray_log_path := FSGame.Values['$logs$'];
    game_root    := AddSlash ( FSGame.Values['$fs_root$'] );
    game_data    := AddSlash ( FSGame.Values['$game_data$'] );
    game_saves   := AddSlash ( FSGame.Values['$game_saves$'] );
    game_scripts := AddSlash ( FSGame.Values['$game_scripts$']);
    mod_dir      := AddSlash ( FSGame.Values['$mod_dir$']);
   end
 else
   begin
    game_data := game_root + 'gamedata\';
    game_saves := game_root + 'savegames\';
    game_scripts := game_root + 'scripts\';
    mod_dir      := game_root + 'mods\';
   end;

 flist := TFileList.Create();
 try
    script_dirs.Add( AddSlash(game_scripts) );
    script_dirs.Add( AddSlash(mod_dir) );
    flist.FindFiles( AddSlash(game_scripts) + '*.*', faDirectory);
    for i := 0 to flist.Count - 1  do
     begin
      if flist.Items[i].Attr and faDirectory = 0 then continue;
      path := flist[i];
      if (path = '.') or (path = '..') then continue;
      path := AddSlash(game_scripts) + AddSlash(path);
      if script_dirs.IndexOf(path) < 0 then
         script_dirs.Add(path);
     end;

    script_dirs.Sort;
    script_dirs.Sorted := TRUE;

 finally
    flist.Free;
 end;



 if FSGame.Count > 1 then
   begin
    ODS('~C0F[~T]. #DBG: FSGame.ltx dump: ~C0A'#13#10 + UnhideSP ( FSGame.Text ) + '~C07');
   end;
end;

procedure XrGetLocalTime(var st: TSystemTime); cdecl;
begin
 SuperLocalTime (st);
end;


procedure Init (ret_ctx: PContext = nil); stdcall;

var
{$IFNDEF NLC_GOLD}
  li_func: procedure ( pinf: PLibInitInfo ); stdcall;
{$ENDIF}
     hDLL: THandle;
        s: String;
        n: Integer;
      udd: TUserDLLDesc;

      lii: TLibInitInfo;
      act: DWORD;
      msg: AnsiString;
       md: TMD5Digest;
//     fl: TFileList;
       ie: THandle;
       le: THandle;
       ft: FILETIME;
       st: TSystemTime;
       tb: __utimbuf64;
     plst: array [0..16383] of Pointer;

begin
 ie := CreateEvent (nil, TRUE, FALSE, 'Global\luaicp_init');
 if gInitialized then exit;
 h_process := OpenProcess ( PROCESS_ALL_ACCESS, TRUE, GetCurrentProcessID );
 if Assigned(SymInitializeW) and Assigned(SymSetOptions) then
   begin
    SymSetOptions ( SYMOPT_UNDNAME or SYMOPT_LOAD_LINES or SYMOPT_DEFERRED_LOADS or SYMOPT_LOAD_ANYTHING or
                    SymGetOptions ( ) );
    SymInitializeW ( h_process, PChar ( ExePath ), TRUE );
   end
 else
   MessageBox(0, 'Cannot init API from dbghelp.dll', 'Warning', MB_OK);

 FillChar (last_ex_ptrs, sizeof(last_ex_ptrs), 0);
 SaveGetExceptionObject := ExceptObjProc;
 ExceptObjProc := @HookGetExceptionObject;
 // CheckInstallVEHFlt ( FALSE, LuaVEHandler );


 test_pv := @test_v;
 mm_list [1].can_used := TRUE;

 GetLocalTime (st);
 SystemTimeToFileTime (st, ft);
 tb.actime  := _filetime_convert (ft); // now?
 tb.modtime := _get_modtime64(PChar(ExeFileName));
 _wutime64 (PChar(ExeFileName), tb);


 while misc.lmDesc.Status <> MST_INITIALIZED do
    InitializeModule ('misc');

 {$IFDEF FAST_MM}
 BlockMM.log_proc := ODS;
 {$ELSE}
 // XrayMM.log_proc := ODS;
 {$ENDIF}

 s := LowerCase( ExeFileName );
 InLauncher := (ret_ctx = nil) and ( Pos( 'launch.exe', s) > 0 );
 InXray := ( xr_engine <> 0 ) or ( Pos( LowerCase(XR_EXE), s ) > 0 );


 AllocGlobals; // building objects


 init_permut;

 // загрузка конфига
 fini := TIniFile.Create ( FindConfigFile('luaicp.conf') );

 add_garbage (fini);

  with g_Config do
   begin
    bShowConsole := fini.ReadBool ('config', 'ShowConsole', FALSE);
    con_origin.X := fini.ReadInteger ('config', 'ConOrigin.X', con_origin.X);
    con_origin.Y := fini.ReadInteger ('config', 'ConOrigin.Y', con_origin.Y);
    {$IFDEF NLC}
      init_script  := fini.ReadString ('config', 'InitScript', '$mod_dir$/nlc_init.script');
    {$ELSE}
      init_script  := fini.ReadString ('config', 'InitScript', '');
    {$ENDIF}
    rtm_script  := fini.ReadString ('config', 'RuntimeScript', '');

    g_game_build := DetectGameBuild;
    if g_game_build = 0 then
       fini.ReadInteger ('config', 'GameBuild', 2947);
    if g_build_ext = '' then
       g_build_ext := fini.ReadString ('config', 'GameBuildExt', g_build_ext); // debug or release?

    if (g_game_build > 0) then
        ASSERT(g_game_build > 5745, 'Invalid game build!');


    block_export.CommaText := fini.ReadString ('config', 'BlockExport', '');
    s := fini.ReadString('config', 'UserDLLs', '');
    vk_term := fini.ReadInteger('config', 'KeyTerminate', VK_DELETE);
    tool_thread_period := fini.ReadInteger ('config', 'TTPeriod', 20);
    test_keyboard := fini.ReadBool ('config', 'TestKeyboard', TRUE);
    usr_dlls.Split(',', s);

    wprintf('[~T]. #DBG:~C0E UserDLLs = %s ', [usr_dlls.CommaText]);

    // debug section
    md :=  MD5StringA (  AnsiString ( 'uglyhack' + FormatDateTime ('dd.mm.yyyy', Now) ) );

    load_launcher       := fini.ReadBool    ('config', 'LoadLauncher', InXray);

    allow_game_mm       := fini.ReadBool    ('debug', 'AllowGameMM', FALSE);
    bInterceptMM        := fini.ReadBool    ('debug', 'InterceptMM', FALSE);
    MY_SEH_install      := fini.ReadInteger ('debug', 'luaicp.InstallSEH', -1);
    BT_SEH_uninst       := fini.ReadBool    ('debug', 'BugTrap.UninstallSEH', FALSE);
    разрчит             := fini.ReadString  ('debug', 'CheatKey', '') =  MD5DigestToStr (md);
    parse_log           := fini.ReadBool    ('debug', 'ParseLog', FALSE);
    game_con_log        := fini.ReadBool    ('debug', 'Log2GameConsole', FALSE );
    rtp_enable          := fini.ReadBool    ('debug', 'RTPEnable', TRUE);
    show_anticheat      := fini.ReadString  ('debug', 'ShowAntiCheat', '') = IDS_YES;
    inst_logCB          := fini.ReadBool    ('debug', 'InstallLogCB', TRUE);
    map_need_funcs      := fini.ReadBool    ('debug', 'MapNeedFuncs', FALSE);
    game_verbose        := fini.ReadInteger ('debug', 'GameVerbosity', 3);
    beep_enabled        := fini.ReadBool    ('debug', 'BeepEnabled', beep_enabled);
    bc_disabled         := fini.ReadBool    ('debug', 'BinCallsDisabled', FALSE);
    bLoadMaps           := fini.ReadBool    ('debug', 'LoadMaps', not FileExists(ExePath + 'xrGame.pdb', FALSE))
   end;

  if fini.ReadBool('debug', 'SetLogCB', TRUE) then
     SetLogCallback;

  {$IF NOT DEFINED(NLC_GOLD) OR NOT DEFINED(RELEASE)}
  if ( PreciseTime > TIME_EXPIRATION ) then
    begin
     s := 'Внимание, библиотека значительно устарела. Получите обновление во избежание сбоев!';
     ODS('[~T].~C0C #WARN:~C07 ' + s);
     MessageBox (0, PChar(s), 'Предупреждение', MB_OK);
     Windows.Beep(900, 1000);
    end;
  if ( PreciseTime > TIME_EXPIRATION + 1 ) then
     ExitProcess (0);
  {$ENDIF}



  if InXray then
  begin
   if bShowConsole then
     begin
      ShowConsole (SW_SHOWNOACTIVATE);
      SelectConsole (0); //
     end
   else
      ShowConsole (SW_HIDE);
   SetConsoleTitle(PChar ('LUAICP.DLL log console - ' + ExeFileName));
  end;


 ODS('[~T]. #DBG(Startup/initialize log):');
 ODS(startup_log);
 startup_log := '';

 last_path_check := GetTickCount;
 /// ODS( Format ('~C0F #TEST var testv addr =~C0D $%p~C07', [ test_pv ] ));

 // ReadLn;


 game_root := UnhideSP ( game_root );

 ReInit;

 s := FmtStalkerPath('$fs_root$');
 s := AddSlash (s);
 SetCurrentDirectory( PChar(s) ); // }


 if InXray then
    ODS('[~T]. #DBG: Этот релиз перехватчика Lua, приготовлен для версии игры S.T.A.L.K.E.R. '
 {$IFDEF SOC}
     {$IFDEF SOC}
      {$IFDEF NEWEST_BUILD} + '"Тени Чернобыля 1.0007+" ' {$ELSE}+ '"Тени Чернобыля 1.0004+" ' {$ENDIF}
     {$ENDIF}
 {$ENDIF}
 {$IFDEF CSKY}
   + '"Чистое небо" '
 {$ENDIF}
 {$IFDEF SCop}
   + '"Зов Припяти" '
 {$ENDIF}
   + ', версия файла: ' + GetFileVersionStr('') );

 // dmadExcept.GetThreadInfos (
 // DONE: Dump system info via madExcept

 try
  ODS (' IsUserAdmin :     ~C0A' + IfV (IsUserAdmin, 'Yes', 'No') + '~C07');
  ODS (' Windows  ver:     ~C0A' + WStr (GetOSVersionString)  + '~C07');
  s := WStr (GetCPUName) + ' [' + IntToStr (GetCPUCount) + ' cores avail]';
  dev_comp := ( s = 'Intel(R) Core(TM) i7-2600K CPU @ 3.40GHz [8 cores avail]' );
  ODS (' CPU     info:     ~C0A' + s + '~C07');
  ODS (' Memory  info:     ~C0A' + WStr (GetMemoryStatus) + '~C07');
  ODS (' Display mode:     ~C0A' + WStr (GetDisplayModeString) + '~C07');
  dev_comp := dev_comp and ( Pos('32660', WStr (GetMemoryStatus)) > 0 );
  if dev_comp then
     ODS('~CF0 DETECTED DEV COMP!~C07');
 except
  on E: Exception do
     OnExceptLog('Init', E);

 end;
 // }

 FillChar (plst, sizeof(plst), 0);

 if IsDebuggerPresent then
  begin
   DumpMMStat(nil);
   // FreeBuffer;
  end;


   {
   if Assigned (_UnDecorateSymbolName) then
      ODS('[~T]. #DBG: Unname test = ~C9A' + undName ('?pcall@detail@luabind@@YAHPAUlua_State@@HH@Z'));
   }

 kbd_ru := LoadKeyboardLayout ('00000419', 0);
 kbd_en := LoadKeyboardLayout ('00000409', 0);

 user_ltx := TStrMap.Create;
 dbg_vars := TStrMap.Create;
 global_vars := TStrMap.Create;
 add_garbage (user_ltx);
 add_garbage (dbg_vars);
 add_garbage (global_vars);

 ReloadUserLTX;

 init_script := FmtStalkerPath (init_script);
 rtm_script  := FmtStalkerPath (rtm_script);


 cmdlist := TStrMap.Create();
 add_garbage (cmdlist);
 add_garbage (fsgame);

 lii.dwVersion := $01010A;
 lii.ODS_proc := LocalODS;

 // Windows.Beep(3000, 500);
 // ReadLn;
 {$IFDEF NLC_GOLD}
 if InLauncher then
    CreateFile ( PChar( FmtStalkerPath ('$game_config$\system.ltx') ), GENERIC_WRITE,  0, nil, OPEN_EXISTING, 0, 0);
 {$ENDIF}

 // загрузка и регистрация пользовательских DLL
{$IFNDEF NLC_GOLD}
 if IsDebuggerPresent then
    asm
     nop
    end;


 if InLauncher then
    ODS('[~T]. #DBG: extensions not loaded for launcher~C0F ' + usr_dlls.CommaText + '~C07');

 try
  SetCurrentDirectory (PChar(DLLPath));
  for n := usr_dlls.Count - 1 downto 0 do
   begin
    s := DLLPath + Trim ( usr_dlls[n] ) + '.dll';
    SetLastError (0);
    hDLL := LoadLibrary ( PChar(s) );

    if hDLL = 0 then
      begin
       PrintError(' Failed to load ' + s + ': ' + err2str);
       usr_dlls.Delete(n);
       continue;
      end;
    // TODO: обязательно внедрить проверку пользовательских DLL


    udd := TUserDLLDesc.Create;
    udd.hDLL := hDLL;
    udd.lua_icpfunc := GetProcAddress (hDLL, 'CaptureFunc');
    udd.sig_handler := GetProcAddress (hDLL, 'SignalHandler');

    msg := 'Welcome to Xray!';


    SetStrZ ( lii.szMessage, String(msg), 63 );
    usr_dlls.Objects [n] := udd;


    li_func := GetProcAddress (hDLL, 'LibInit');
    if Assigned (li_func) then
      begin
       li_func (@lii);
       if Assigned(udd.lua_icpfunc)  then
          wprintf('[~T]. #DBG: registered API extension from %s', [s]);
      end;


   end;
 except
  on E: Exception do
     OnExceptLog ('luaicp.Init#1', E);
 end;
{$ENDIF}


 // cleaning last log
 s := FindLastXrayLog();
 if (s <> '') then
     DeleteFile (s);



 hDLL := GetModuleHandle (LUA_DLL);
 if hDLL = 0 then
   begin
    PrintError ('LUA_DLL not found in process ' + ExeFileName);
    exit;
   end;


 ODS(CFormat('[~T/~i]. #DBG: LUA_DLL found at $%X, ExeFile = %s ', '~C07', [hDLL, ExeFileName]));

 luaL_ns_orig :=   GetProcAddress(hDLL, 'luaL_newstate');
 lua_pcall_orig := GetProcAddress(hDLL, 'lua_pcall');
 lua_ns_orig :=    GetProcAddress(hDLL, 'lua_newstate');
 lua_nt_orig :=    GetProcAddress(hDLL, 'lua_newthread');
 lua_ls_orig :=    GetProcAddress(hDLL, 'luaL_loadstring');
 lua_lf_orig :=    GetProcAddress(hDLL, 'luaL_loadfile');
 lua_ld_orig :=    GetProcAddress(hDLL, 'lua_load');
 lua_ps_orig :=    GetProcAddress(hDLL, 'lua_pushstring');

 lua_type_orig :=  GetProcAddress(hDLL, 'lua_type');


 LoadGlobalMap;

 {$IFNDEF NEWEST_BUILD}
 CreateCapture  (@lua_ns_orig, @on_nsexec, $53, 6);
 CreateCapture  (@lua_nt_orig, @on_ntexec, $57, 8);
 {$ENDIF}
 if fini.ReadBool ('debug', 'capture_pcall', TRUE) then
    Patch_pcall;

 {$IFDEF NLC_GOLD}
 if CreateFile ( PChar(XrUpdatePath('$game_spawn$', 'all.spawn')), GENERIC_WRITE, FILE_SHARE_DELETE, nil, OPEN_EXISTING, 0, 0) <> INVALID_HANDLE_VALUE then
    begin
     PrintError('Распакованные архивы не поддерживаются в данной сборке');
     ExitProcess( DWORD(-5) );
    end;
 {$ENDIF}

 hDLL := GetModuleHandle ('xrCore.DLL');
 if hDLL <> 0 then
  begin
   ODS(CFormat('[~T/~i]. #DBG: xrCore.DLL found at $%X  ', '~C07', [hDLL]));
   _stklog   :=    GetProcAddress (hDLL, '?Log@@YAXPBD@Z');
   _stkflush :=    GetProcAddress (hDLL, '?FlushLog@@YAXXZ');
   _xdbgInit :=    GetProcAddress (hDLL, '?_initialize@xrDebug@@QAEXAB_N@Z');
   LogStackTrace    := GetProcAddress (hDLL, '?LogStackTrace@@YAXPBD@Z');
   LogStackTraceEx  := GetProcAddress (hDLL, '?LogStackTraceEx@@YAXPAU_EXCEPTION_POINTERS@@@Z');
   DumpContext      := GetProcAddress (hDLL, '?DumpContext@@YAXXZ');
   //                                         ?LogStackTraceEx@@YAXPAU_EXCEPTION_POINTERS@@@Z
   {$IFDEF SOC} // shadow of chernobyl only
   _SetLogCB :=    GetProcAddress (hDLL, '?SetLogCB@@YAXP6AXPBD@Z@Z'); // SetLogCB
   {$ELSE}
   _SetLogCB :=    GetProcAddress (hDLL, '?SetLogCB@@YAP6AXPBD@ZP6AX0@Z@Z'); // SetLogCB
   {$ENDIF}

   _SetTimeGetter := GetProcAddress (hDLL, '?SetTimeGetter@@YAP6AXPAU_SYSTEMTIME@@@ZP6AX0@Z@Z'); // SetTimeGetter

   wprintf ('[~T]. #DBG: xrCore.DLL Log = $%P, FlushLog = $%P, SetLogCB = $%P ', [ Addr(_stklog), Addr(_stkflush), Addr(_SetLogCB) ] );
   if Assigned(_SetTimeGetter) then
       _SetTimeGetter(XrGetLocalTime);

  end;

 // TODO: загрузка конфигурации из файла и дочерних DLL
 asm
  nop
 end;

 SetLogCallback;
 ODS('[~T/~i].~C0E #DBG: Предварительная инициализация закончена.~C07');
 SetEvent (ie);
 allow_repatch := TRUE;
 if BT_SEH_uninst then
    begin
     BT_UninstallSehFilter;
     ODS('~C0F[~T]. #DBG: Обработчик SEH bugtrap.dll отключен.~C07');
    end;

 _GetPtrInfo := GetPtrInfo;

 if MY_SEH_Install >= 0 then
   begin
    CheckInstallVEHFlt(FALSE, LuaVEHandler);
    Patch_XrDebugInit;
   end;


 LCGlobals.ptr_info_func := GetPtrInfo;

 texcap.InitModule (fini);


 if Assigned (SetThreadErrorMode) then
    SetThreadErrorMode (2, nil);

 //    ReadLN;
 // SetTimerResolution ( 5000, TRUE, act );
 // PatchQPF;


 if ( ( xr_engine = 0 ) and fini.ReadBool ('debug', 'ShowMonitor', FALSE) ) then
    TDMThread.Create ( FALSE, 'DMThread.Luaicp' );

 if ( xr_engine = 0 ) and ( InLauncher ) then
   begin
    ODS('[~T]. #DBG: Creating TIPCThread.');
    gIPCThread := TIPCThread.Create ( FALSE, 'IPCThread' )
   end
 else
 if InXray then
   Begin // TODO: cut this large code
    {$IFDEF PROFILES}
    profile_name := ActiveProfileName;
    if Assigned(SetUserName) then
       SetUserName( LPSTR(AnsiString(profile_name)) );
    {$ENDIF}
    {$IFDEF SINGULARITY}
    SetLastError(0);
    n := 0;
    le := 0;
    if g_Config.load_launcher then
     begin
      Repeat
        le := OpenEvent (SYNCHRONIZE, FALSE, 'Global\XRAY_LAUNCHER');
        if le <> 0 then break;
        s := '-attach_to ' + IntToStr ( GetCurrentProcessId );
        if n < 32 then
          begin
           n := ShellExecute (misc.hWndCon, nil, PChar (ExePath + '\launch.exe'), PChar(s), PChar(ExePath),  SW_HIDE);
           wprintf('[~T]. #DBG: ShellExecute returned $%x for "launch.exe" executing', [n]);
          end;
        SleepEx(500, TRUE);

        le := OpenEvent (SYNCHRONIZE, FALSE, 'Global\XRAY_LAUNCHER');
        if n > 32 then Dec(n);
      Until (le <> 0);


      if (le = 0) or ( WaitForSingleObject (le, 25000) = WAIT_TIMEOUT ) then
       begin
        PrintError('Launcher hangs or not responded. Event handle =  $' + IntToHex(le, 4));
        MessageBox(0, 'Не удается запустить дочерний процесс launch.exe', 'Неисправимая ошибка', MB_OK or MB_ICONERROR);
        Sleep(500);
        ExitProcess(51);
       end;

      CloseHandle(le);
     end;
    {$ENDIF}

    act := 0;


    try
       for n := 0 to 100 do
        begin
         act := OpenFileMapping (FILE_MAP_READ or FILE_MAP_WRITE, TRUE, 'WORK_IPC');
         if act <> 0 then break;
         wprintf('~C0C #WARN:~C07 OpenFileMapping failed with error = %s', [err2str]);
         SleepEx(1000, TRUE);
         // ODS ('[~T]. #DBG: waiting WORK_IPC ready...');
        end;

      CloseHandle (act);
      // ReadLn;
      ODS('[~T]. #DBG: Creating TIPCAsyncQueue.');
      gIPCQueue := TIPCAsyncQueue.MakeInstance ('WORK_IPC');

      // if launcher
      if gIPCQueue.IsReceiverSide then
        begin
         FreeAndNil (gIPCQueue);
        end
      else
        begin
         n := GetCurrentProcessId();
         gIPCQueue.PushSimpleRqs ('TEST_IPC', nil);
         if not InLauncher then
           begin
            act := GetCurrentProcessId;
            gIPCQueue.PushDataRqs ('ATTACHE_ME', act, sizeof(act));
           end;
        end;

    except
     on E: Exception do
       OnExceptLog('Init#5', E);
    end;

   End;

 GetSharedBuff;

 if bLoadMaps then
  begin
   ODS('[~T/~B]. #DBG: waiting while base maps loaded...');
   for n := 1 to 100 do
     if gToolThread.base_maps < 2 then Sleep(100);
  end;


 if xr_engine > 0 then
    RootRegistry (TRUE);

 {$IFDEF SOC}
 s := FmtStalkerPath('$mod_dir$\vxdb');
 if Pos('bad_file_name', s) = 0 then
    CheckMakeDir (s);
 {$ENDIF}


 if InXray then
    ExecInitScript;

 if rtm_script <> '' then
    ActivateRuntimeScript;

 // MM_StressTest;

 ODS('[~T]. #DBG: luaicp init complete! ');
 ODS('~C0F========================================================================================================~C07');
end; // Init


 procedure Main;
 begin
  try
   Init;
  except
   on E: Exception do
      OnExceptLog('luaicp.Main ', E, FALSE);
  end;
  XSleep (50);
 end;

function BeginProcessDebug (pid: DWORD): Boolean; stdcall;
begin
 result := FALSE;
 if Assigned (gToolThread) then
    result := gToolThread.AddRequest ('BEGIN_PROCESS_DEBUG', Ptr (pid));
end;

procedure EndProcessDebug; stdcall;
begin
 if Assigned (gToolThread) then
             gToolThread.AddRequest ('END_PROCESS_DEBUG');
end;

procedure SetGameProcess(hp: THandle); stdcall;
begin
 game_process := hp;
end;

procedure EnableVEH(bEnable: Boolean); stdcall;
begin
 MY_SEH_install := IfV (bEnable, 1, -1);
 CheckInstallVEHFlt ( MY_SEH_install < 0, LuaVEHandler );
end;

procedure LibProc(nReason: Integer);
var
   sa: String;
   ms: String;
   id: DWORD;
begin
 try
  case nReason of
   DLL_PROCESS_DETACH: on_unload (nil);
   DLL_THREAD_ATTACH:
      begin
       id := GetCurrentThreadId;
       sa := madExcept.GetThreadName(id);
       if ( Length (sa) < 3 ) or ( Pos('thread ', String(sa)) = 1 ) then
        begin
         sa := InfoFmt('late@~T=~I');
         madExcept.NameThread(id, sa );
        end;

       ms := InfoFmt ('[~T/~I]. #DBG: Thread attached: ') + String (sa);
       OutputDebugString ( PChar (ms) );
       //  ODS('[~T/~I]. #DBG: Thread attached ~C0A' + String(sa) + '~C07');
      end;
  end;

 except
  on E: Exception do
        OnExceptLog ('LibProc', E);
 end;
end; // LibProc

function  SignalDisplayed: Boolean; stdcall;
begin
 result := signal_disp;
end;


function FastMemAlloc (cbSize: SIZE_T): Pointer; stdcall;
begin
  result := FastMM4.FastGetMem(cbSize);
end;

function FastMemReAlloc (p: Pointer; cbSize: SIZE_T): Pointer; stdcall;
begin
  result := FastMM4.FastReallocMem (p, cbSize);
end;

procedure FastMemFree (p: Pointer); stdcall;
begin
  FastMM4.FastFreeMem(p);
end;


var

   ml: TModuleDescList;
   md: TModuleDescriptor;


{
function DllGetClassObject(const CLSID, IID: TGUID; var Obj): HResult; stdcall;
begin

 result := ComServ.DllGetClassObject(CLSID, IID, Obj);
end;}

function DllRegisterServer: HResult; stdcall;
begin
 _SafeLog('executing ComServ.DllRegisterServer');
 result := ComServ.DllRegisterServer;
 if result = S_OK then
 else
   _SafeLog('ComServ.DllRegisterServer failed with result ' + IntToStr(result) + ':' + err2str);
end;


exports
        DllGetClassObject,
        COMSupport.DllCanUnloadNow,
        DllRegisterServer,
        DllUnregisterServer,
        Init              name 'Init',
        ReInit            name 'ReInit',
        ExecInitScript    name 'ExecInitScript',
        BeginProcessDebug name 'BeginProcessDebug',
        EndProcessDebug   name 'EndProcessDebug',
        ExpDumpVar        name 'ExpDumpVar',
        ExtCapture, LocalODS, ShowConsole,
        FastObjectEvent   name  '?FastObjectEvent@@YAXPAU_GAME_OBJECT_EVENT@@@Z',
        InstallTo, DMAlloc, EnableVEH, FSExpandPath, GetModuleDescList, GetGlobalVar, LuaVEHandlerImpl,
        RegDLLStuff, SetGameProcess, SignalDisplayed, UpdateXrayOffset,
        // для отображения функций в профайлере и отладчике VC++

        XrayLogStackTrace,
        ProcessCommands,
        LuaSafeCall, dock_str, RegularTimerProc,
        FastMemAlloc, FastMemReAlloc, FastMemFree,
        InvokeRegistry    name 'profiling_InvokeRegistry',
        UpdateRegistry    name 'profiling_UpdateRegistry',
        _logCB            name 'profiling_LogCallback';

var
   logs_path, exe: String;


{ THelperThread }

procedure THelperThread.LoadScript(L: lua_State);
var
   ierr: Integer;
     sa: AnsiString;
begin
  sa := AnsiString (ScriptLines.Text + #0);
  ierr := luaL_loadstring( L, PAnsiChar(sa) );
  if ierr <> 0 then
   begin
     PrintError (' script load error ' + LuaStrArg(L, -1) );
     ODS(' #DBG(Script dump):~C0F '#13#10 + String(sa) + #13#10'<-------------> ~C07');
     exit;
    end;
  {
  lua_getglobal(result, 'AtPanicHandler');

  if lua_type (result, -1) = LUA_TFUNCTION then
     ierr := lua_gettop (result)
  else
     lua_pop (result, 1);}

  ierr := lua_pcall ( L, 0, LUA_MULTRET, ierr );
  if ierr = LUA_ERRRUN then
    begin
     PrintError ('script pcall error ' + LuaStrArg(L, -1));
     ODS(' #DBG(Script dump):~C0F '#13#10 + String(sa) + #13#10'<-------------> ~C07');
    end;
end;

function THelperThread.MakeState: lua_State;
begin
 result := nil;
 // Assert (Assigned(orig_hf), 'Original heap function not found');
 try
  if Assigned (orig_hf) then
     result := lua_newstate(orig_hf, orig_ud) // WARN: possible routine not safe to be used multithread
  else
     result := lua_newstate (DMAlloc, nil);
  RegDLLStuff (result);

  lua_pushptr (result, self);
  lua_setglobal (result, 'thread_obj'); // needs for globals exchange

  lua_register (result, 'GetThreadAsyncVar', GetThreadAsyncVar);
  lua_register (result, 'SetThreadAsyncVar', SetThreadAsyncVar);

 except
  on E: Exception do
    OnExceptLog('MakeState', E);
 end;
end;

procedure THelperThread.ProcessInit;
begin
 inherited;
 FWorkGlobals := TStrMap.Create(self);
 FScriptLines := TStrMap.Create(self);
 wait_time := 100;
end;

function THelperThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   ierr: Integer;
   ltop: Integer;
begin
 result := inherited ProcessRequest (rqs, rqobj);

 if rqs = 'LOAD_SCRIPT' then
   begin
    ODS('[~T/~I]. #DBG: processing rqs ' + rqs);
    if FLuaState = nil then
       FLuaState := MakeState;
    LoadScript (FLuaState);
   end;

 if rqs = 'EXEC_SCRIPT' then
   begin

    // ODS('[~T/~I]. #DBG: processing rqs ' + rqs);

    if FLuaState = nil then exit;

    // ---------
    ltop := lua_gettop (FLuaState);

    lua_pushboolean(FLuaState, FALSE);
    lua_setglobal(FLuaState, 'next_execute');

    // ---------
    {lua_getglobal(L, 'LuaSafeCall'); // needs 2 args
    lua_pushwstr(L, 'main');
    lua_pushwstr(L, '#func:^mad^');
    lua_call (L, 2, LUA_MULTRET);    // execute safe nested}


    ierr := 0;
    lua_getglobal(FLuaState, 'AtPanicHandler');
    if lua_type (FLuaState, -1) = LUA_TFUNCTION then
       ierr := lua_gettop (FLuaState)
     else
        lua_pop (FLuaState, 1);

    lua_getglobal(FLuaState, 'main'); // main func

    if lua_type(FLuaState, -1) = LUA_TNIL then
       begin
        PrintError('EXEC_SCRIPT - Not found function "main" in script text');
        lua_settop (FLuaState, ltop);
       end
    else
       try
        lua_pcall (FLuaState, 0, LUA_MULTRET, ierr);
       except
        on E: Exception do
           OnExceptLog ('EXEC_SCRIPT request in ' + ThreadName, E);
       end;



    lua_getglobal(FLuaState, 'next_execute');

    if lua_toboolean(FLuaState, -1) then
       AddRequest('EXEC_SCRIPT', rqobj);

    result := 0;
   end;
end;

procedure THelperThread.ProcessThreadStop;
begin
 inherited;

 if FLuaState <> nil then lua_close(FLuaState);
 FLuaState := nil;
 FreeAndNil (FWorkGlobals);
 FreeAndNil (FScriptLines);
end;

procedure PreInit; stdcall;
var
   hExe: DWORD;
      s: String;

begin
 InLauncher := FALSE;
 exe := LowerCase( ModuleFileName (0) );
 hExe := GetModuleHandle ( PChar ( ExtractFileName(exe) ) );
 if hExe > 0 then
    InLauncher := ( nil <> GetProcAddress (hExe, 'IsLauncher') );

 if ( Pos( LowerCase(XR_EXE), LowerCase (exe) ) > 0 ) or (not InLauncher) then
    begin
     con_enabled := TRUE;
     InLauncher := FALSE;
     CloseConsole;
    end;

 SetCurrentDirectory ( PChar( DllPath ) );

 OnInstall := Main; // эта функция запускается хостом, из блока инициации
 Randomize;



 game_root := CorrectFilePath ( DllPath + '..\.');
 game_root := AddSlash (game_root);
 if not FileExists (game_root + 'fsgame.ltx') then
   begin
    game_root := ExePath;
    PrintError ('Cannot locate fsgame.ltx in ' + game_root );
   end;

 fsgame := ParseFSGame;
 logs_path := FmtStalkerPath('$logs$') + '\%DateDir%\';

 if InLauncher then
   begin
    logs_path := logs_path + '\launcher\';
    CheckMakeDir (logs_path);
   end;

 if ( Pos('%', logs_path) = 0 ) and not DirectoryExists (logs_path) then
  begin
   PrintError('Not exists logs directory ~C0F' + logs_path);
   logs_path := CorrectFilePath ( ExePath + '..\logs\');
   PlayBeep (2000, 1000);
  end;


 StartLogging(logs_path);

 s := FmtStalkerPath('$game_textures$');
 if ( s <> '' ) and ( InLauncher ) then
  begin
   s := AddSlash (s) + 'ui\ui_ls61.dds';
   // Assert ( FileExists (s), 'Not exists ' + s );
   // CreateFile ( PChar (s), GENERIC_READ, FILE_SHARE_DELETE, nil, OPEN_ALWAYS, 0, 0 );
  end;



 s := ExtractFileName(ExeFileName);


 // ODS(Format('[~T][~TR][~TX]. #DBG: luatest.dll loaded, hinst = $%.x', [HInstance]));
 try
  h_main_thread := OpenThread ( THREAD_ALL_ACCESS, TRUE, GetCurrentThreadId );
  mt_stack_top := GetStackTop;
  mt_stack_btm := GetStackBottom;

 except
  on E: Exception do PrintError('OpenThread exception: ' + E.Message);
 end;


 ml := MainModuleDescList;

 // ShellExecute(

 md := ml.FindDesc ('misc');
 if md <> nil then
    md.AddCallback ( 'finalize', on_unload );
end;


{ TDumperThread }

function TDumperThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   pctx: PContext;
begin
 result := inherited ProcessRequest (rqs, rqobj);

 if rqs = 'DUMP_STACK' then
   try
    pctx := Pointer (rqobj);

    ODS ( DumpProcessStack ( 0, pctx^, nil ) );
   except
    on E: EAccessViolation do
       PrintError ('AV Catched in DumpProcessStack ' + E.Message);
   end;

end;


begin
 // if misk.dbg_present then

 space_replace := #$A4;
 // asm int 3 end;
 IsMultithread := TRUE;
 // CoInitFlags := COINIT_SPEED_OVER_MEMORY or CoInitFlags;
 // CoInitMT;
 ComServer.UIInteractive := TRUE;
 DllProc := LibProc;
 InitializeModule ('misc');
 PreInit;



 if not gInitialized and (Pos('php.exe', ExeFileName) + Pos('httpd.exe', ExeFileName) > 0) then
   try
    Main;
    // wprintf('[~T]. #DBG: due to detected process %s, just wait ', [ExeFileName]);
    CheckInstallVEHFlt ( TRUE, LuaVEHandler );
    if IsDebuggerPresent then
       asm int 3 end;
   finally
    _SafeLog('Init executed');
   end;
 RegClasses;

 _SafeLog('DLL body execution complete!');
end.
