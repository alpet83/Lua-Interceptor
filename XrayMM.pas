unit XrayMM;

interface
uses Windows, Types;


{.$DEFINE ALLOC_POINTS}
const
   GiB = 1024 * 1024 * 1024;

type
   SIZE_T = LongWord;

   TXrMemory = record
       x_m1: Pointer;   // + $00
       x_m2: Pointer;   // + $04   int m_counter
       x_m3: Pointer;   // + $08   ref xrMemCopy_x86  or xrMemCopy_MMX
       x_m4: Pointer;   // + $0C   ref xrMemFill_x86
       x_m5: Pointer;   // + $10   ref xrMemFill32_x86 or xrMemFill32_MMX
   end;

   PXrMemory = ^TXrMemory;


   PDebugMemoryManager = ^TDebugMemoryManager;


   TDebugMemoryManager = packed record
    cb_alloc: Int64;
    cb_freed: Int64;
    op_count: Int64;
     cb_diff: Int64;

    can_used: Boolean;
     ex_safe: Boolean;
        name: PChar;


        _GetMem: function (Size: NativeInt): Pointer;
       _FreeMem: function(P: Pointer): Integer;
    _ReallocMem: function(P: Pointer; Size: NativeInt): Pointer;

    function    Enabled: Boolean;

    // debug wrappers
    function    GetMem(size: NativeInt): Pointer; inline;
    function    FreeMem(P: Pointer; Size: NativeInt): Integer; inline;
    function    Realloc(P: Pointer; OldSize, NewSize: NativeInt): Pointer; inline;

    procedure   OnAlloc(size: NativeInt);
    procedure   OnFree (size: NativeInt);

   end;


   // ----------------------------------------------------

   TAllocPoint = packed record
      code_ptr: DWORD;   // EIP
      df_alloc: Integer; // diff-value
      op_alloc: Integer;
    op_realloc: Integer;
       op_free: Integer;
   end;

   PAllocPoint = ^TAllocPoint;

   TAllocPointsList = array [0..255] of TAllocPoint;
   PAllocPointsList = ^TAllocPointsList;


   TMemBlockHdr = packed record
    dt: TDateTime;
    sz: DWORD;
    ap: PAllocPoint;                 // alloc point
    mm: PDebugMemoryManager;         // also uses as signature
   end;

   PMemBlockHdr = ^TMemBlockHdr;





{



        375  176 00017680 public: void * __thiscall xrMemory::mem_alloc(unsigned int)
        376  177 00016FA0 public: void __thiscall xrMemory::mem_compact(void)
        377  178 00001530 public: unsigned int __thiscall xrMemory::mem_counter_get(void)
        378  179 00001540 public: void __thiscall xrMemory::mem_counter_set(unsigned int)
        379  17A 00017770 public: void __thiscall xrMemory::mem_free(void *)
        380  17B 000177E0 public: void * __thiscall xrMemory::mem_realloc(void *,unsigned int)
        381  17C 00016DF0 public: unsigned int __thiscall xrMemory::mem_usage(unsigned int *,unsigned int *)

\}

procedure PreloadLog(const msg: WideString; print_flags: DWORD = 255); stdcall;

function  XrTryAlloc(size: NativeInt): Pointer;
function  XrFree(p: Pointer): Integer;

procedure DumpStats;    // via log_proc
procedure DumpMemInfo;  // via wprintf

var
   fake_block: Pointer = nil;
     log_proc: procedure (const msg: WideString; print_flags: DWORD = 255); stdcall = PreloadLog;
      mm_list: array [0..2] of TDebugMemoryManager;
    in_except: Integer = 0;

     a_points: array [0..255] of TAllocPointsList;


threadvar
    inside_mm: Integer;



function InitMM(L: Pointer): Integer; cdecl;
function TestMaxAlloc: NativeUInt;

implementation
uses Math, SysUtils, Classes, StrUtils, Misc, madExcept, FastMM4, LCGlobals;


const
             HDR_SIZE = sizeof (TMemBlockHdr);
         mem_obj_name = '?Memory@@3VxrMemory@@A';
        mem_free_name = '?mem_free@xrMemory@@QAEXPAX@Z';
       mem_alloc_name = '?mem_alloc@xrMemory@@QAEPAXI@Z';
      mem_alloc_name2 = '?mem_alloc@xrMemory@@QAEPAXIPBD@Z';
     mem_realloc_name = '?mem_realloc@xrMemory@@QAEPAXPAXI@Z';
    mem_realloc_name2 = '?mem_realloc@xrMemory@@QAEPAXPAXIPBD@Z';
    NO_HEAP_MM: PChar = '-no_heap_mm';
     MMLogFile: PChar = '_xraymm.log';




var
   xrmm, oldmm: TMemoryManagerEx;
    Memory: PXrMemory = nil;
   ptr_first, ptr_last: DWORD;

     xr_mem_alloc: Pointer;
      xr_mem_free: Pointer;
   xr_mem_realloc: Pointer;
   xr_mem_monitor: Boolean = FALSE;


        alloc_ofs: Integer = 0;
            gHeap: THandle;
            szExe: array [0..MAX_PATH + 1] of CHAR;
         szParams: array [0..1023] of CHAR;


function InterlockedAdd(var Addend: Integer; Increment: Integer): Integer;
asm
      MOV   ECX,EAX
      MOV   EAX,EDX
 LOCK XADD  [ECX],EAX
      ADD   EAX,EDX
end;

procedure FindMemoryObject;
var
   hLib: THandle;
     pp: Pointer;
begin
 hLib := GetModuleHandle ('xrCore.dll');

 if hLib = 0 then
  begin
   log_proc('[~T]. #ERROR: not found xrCore.dll in memory');
   exit;
  end;

 pp := GetProcAddress( hLib, mem_obj_name );

 xr_mem_alloc   := GetProcAddress( hLib, mem_alloc_name );
 xr_mem_free    := GetProcAddress( hLib, mem_free_name );
 xr_mem_realloc := GetProcAddress( hLib, mem_realloc_name );

 if (nil = xr_mem_alloc) then
    begin
     xr_mem_alloc   := GetProcAddress( hLib, mem_alloc_name2 );
     xr_mem_realloc := GetProcAddress( hLib, mem_realloc_name2 );
     xr_mem_monitor := (xr_mem_alloc <> nil);
    end;


 if pp = nil then
   log_proc('#ERROR: cannot get exported symbol ' + mem_obj_name)
 else
   begin
    if pp <> Memory then
       log_proc('#DBG: global xrMemory Memory at $' + IntToHex ( DWORD (pp), 8) );
    Memory := pp;
   end;
end;


procedure PreloadLog;
var
   s: String;
   r: String;
   t: Text;
begin
 s := ParamStr(0);
 s := LowerCase (s);
 if Pos('.exe', s) = 0 then exit;
 s := LowerCase(s);
 r := 'bin\' + XR_EXE;
 s := AnsiReplaceStr (s, LowerCase(r), 'logs\');
 r := '\' + ExtractFileName ( ExeFileName );
 s := AnsiReplaceStr (s, LowerCase(r), '\logs\');
 s := AnsiReplaceStr (s, '\test.exe', '\logs\');

 if not DirectoryExists(s) then
        CreateDirectory(PChar (s), nil);

 s := s + MMLogFile;

 try
  AssignFile(t, s);
  {$I-}
  if FileExists (s) then
     Append (t)
  else
     ReWrite (t);
  s := FormatDateTime ('dd.mm.yy hh:nn:ss.zzz', Now);

  WriteLn (t, '[' + s + ']. ' + msg);

  CloseFile (t);

 except
  on E: Exception do
     OutputDebugString('Exception catched in PreloadLog');
 end; // try-exc


end; //


var
   a_comment: PAnsiChar = 'LUAICP_ALLOC';
   r_comment: PAnsiChar = 'LUAICP_REALLOC';

function XrTryAlloc(size: NativeInt): Pointer;


begin
 result := nil;
 try

   if (xr_mem_alloc = nil) or (Memory = nil) then exit;

   asm
    pushad
    mov  ecx, memory
    test xr_mem_monitor, 1
    jz   @std_alloc
    push a_comment
@std_alloc:
    push size
    mov  eax, xr_mem_alloc
    call eax
    mov  result, eax
    popad
   end;
 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrTryAlloc ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;
end;

function XrRealloc(p: Pointer; newSize: NativeInt): Pointer;
begin
 result := nil;

 if (xr_mem_realloc = nil) or (Memory = nil)  then exit;


 try
  asm
   pushad
   test xr_mem_monitor, 1
   jz   @std_realloc
   push r_comment
@std_realloc:
   mov  ecx, Memory
   mov  eax, xr_mem_realloc
   push newSize
   push p
   call eax
   mov  result, eax
   popad
  end;


 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrRealloc ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;

end;

function XrFree(p: Pointer): Integer;
begin
 result := 1;

 if (xr_mem_free = nil) or (Memory = nil) then exit;


 result := 0;
 try
  asm
   pushad
   mov  ecx, Memory
   mov  eax, xr_mem_free
   push p
   call eax
   popad
  end;


 except
  on E: Exception do
     log_proc('EXCEPTION catched in XrFree ' + E.Message + ', Memory = $' + IntToHex(DWORD(Memory), 8) );
 end;

end;


function HeapAllocMem(Size: NativeInt): Pointer;
begin
 Assert (gHeap <> 0, 'HeapAllocMem: gHeap == NULL');
 result := HeapAlloc ( gHeap, HEAP_GENERATE_EXCEPTIONS or HEAP_ZERO_MEMORY, size );
end;

function HeapReallocMem(P: Pointer; Size: NativeInt): Pointer;
begin
 result := HeapRealloc ( gHeap, HEAP_GENERATE_EXCEPTIONS, p, Size );
end;

function HeapFreeMem (p: Pointer): Integer;
begin
 result := 1;

 if (gHeap <> 0) and HeapFree ( gHeap, HEAP_GENERATE_EXCEPTIONS, p ) then result := 0;

end;


function RegisterExpectedMemoryLeak(p: Pointer): Boolean;
begin
 result := FALSE;
end;

function UnregisterExpectedMemoryLeak (p: Pointer): Boolean;
begin
 result := FALSE;
end;

function CheckMM (mm: Pointer): Boolean; inline;
begin
 result := FALSE;
 if NativeUInt (mm) < $10000 then exit;


 result := ( mm = @mm_list [0] ) or ( mm = @mm_list [1] ) or ( mm = @mm_list [2] );
end;


{ Universal allocators }

function UniGetMem(size: NativeInt): Pointer;
var
   phdr: PMemBlockHdr;
    fsz: NativeInt;
    mgr: PDebugMemoryManager;
    eip: PDWORD;
{$IFDEF ALLOC_POINTS}
    apl: PAllocPointsList;
     ap: PAllocPoint;
{$ENDIF}
      i: Integer;
begin
 {$IFDEF ALLOC_POINTS}
 eip := PDWORD (ebp + $8);
 apl := @a_points[eip^ and $FF];
 {$ENDIF}

 result := nil;

 if size = 0 then exit;

 InterlockedIncrement ( inside_mm );

 try
   // TODO: very small block can allocated via oldmm

   fsz := size + HDR_SIZE;

   for i := 0 to High (mm_list) do
    begin
     mgr := @mm_list [i];
     if not mgr.enabled then continue;
     result := mgr.GetMem (fsz);
     if result <> nil then break;
    end;

   Assert (result <> nil, 'UniAllocMem failed to allocate block with size ' + IntToStr(fsz));


   {$IFDEF ALLOC_POINTS}
   ap := nil;
   for i := 0 to High(apl^) do
    if ( apl[i].code_ptr = 0 ) or ( apl[i].code_ptr = eip^ ) then
      begin
       ap := @apl[i];
       ap.code_ptr := eip^;
       InterlockedAdd (ap.df_alloc, fsz);
       InterlockedIncrement (ap.op_alloc);
       break;
      end;

   phdr.ap := ap;
   {$ENDIF}
   phdr := result;
   phdr.dt := Now;
   phdr.sz := size;
   phdr.mm := mgr;

   Inc ( NativeUInt(result), HDR_SIZE );
 finally
  InterlockedDecrement ( inside_mm );
 end;
end;

function UniAllocMem(size: NativeInt): Pointer;
begin
 result := UniGetMem (size);
 FillChar ( result^, size, 0 );
end;

function UniFreeMem(p: Pointer): Integer;
var
   phdr: PMemBlockHdr;
begin
 result := 0;
 if p = nil then exit;
 phdr := p;


 InterlockedIncrement ( inside_mm );
 try
   try
     Dec ( NativeUInt(phdr), HDR_SIZE );
     if CheckMM (phdr.mm) then
       begin
        {$IFDEF ALLOC_POINTS}
        if phdr.ap <> nil then
          begin
           InterlockedAdd (phdr.ap.df_alloc, -phdr.sz - HDR_SIZE);
           InterlockedIncrement (phdr.ap.op_free);
           if phdr.ap.df_alloc <= 0 then
              phdr.ap.code_ptr := 0;

          end;
        {$ENDIF}

        result := phdr.mm.FreeMem (phdr, phdr.sz + HDR_SIZE ) // WARNING: Is important for block releasing/reallocating
       end
     else
        result := oldmm.FreeMem (p);
   except
    on E: Exception do
       OnExceptLog ('UniFreeMem ' + FormatPtr(p), E);
   end;
 finally
  InterlockedDecrement ( inside_mm );
 end;
end; // UniFreeMem



function UniReallocMem(p: Pointer; newSize: NativeInt): Pointer;
var
   phdr: PMemBlockHdr;
begin

 if p = nil then
   begin
    result := UniAllocMem (newSize);
    exit;
   end;

 phdr := p;
 Dec ( NativeUINt(phdr), HDR_SIZE );

 InterlockedIncrement ( inside_mm );
 try
   if CheckMM (phdr.mm) then
     begin
      {$IFDEF ALLOC_POINTS}
      if phdr.ap <> nil then
        begin
         InterlockedAdd (phdr.ap.df_alloc, newSize - phdr.sz);
         InterlockedIncrement (phdr.ap.op_realloc);
        end;
      {$ENDIF}
      result := phdr.mm.Realloc (phdr, phdr.sz, newSize + HDR_SIZE);
      if result = nil then
        begin
         phdr.mm.can_used := FALSE;
         PrintError('Memory manager ' + phdr.mm.Name + ' Realloc failed! NewSize = ' + IntToStr(newSize));
         result := UniAllocMem (newSize);
         if result <> nil then
            CopyMemory (result, RelativePtr (phdr, HDR_SIZE), phdr.sz - HDR_SIZE);
         exit;
        end;

      phdr := result;
      phdr.dt := Now;
      phdr.sz := newSize;
      Inc ( NativeUInt(result), HDR_SIZE );
     end
   else
      result := oldmm.ReallocMem (p, newSize); // UniAllocMem(newSize);
 finally
  InterlockedDecrement ( inside_mm );
 end;
end; // UniReallocMem


function InitMM;
var
  pHeap: THandle;
begin
 result := 0;
 pHeap := GetProcessHeap;

 if Assigned (log_proc) then
   begin
    log_proc ('[~T]. #DBG: Process Heap  = $' + IntToHex(pHeap, 4) );
   end
 else
   exit;
end;

procedure DumpStats;
var
   i: Integer;
   s: String;
begin
 for i := 0 to High(mm_list) do
 with mm_list[i] do
  begin
   s := Format('mm_list[%d] cb_alloc = %.5f GiB, cb_freed = %.5f GiB, op_count = %d', [i, ( cb_alloc / GiB ), ( cb_freed / GiB ), op_count ] );
   log_proc (s);
  end;
end; // DumpStats


procedure DumpMemInfo;
var
  mmus: TMemoryManagerUsageSummary;
     i: Integer;
     s: String;
begin
 GetMemoryManagerUsageSummary (mmus);
 wprintf ('[~T/~U/~B].~C03 #PERF:~C0F luaicp dynamic memory usage:~C07 allocated = %.3f MiB, overhead = %.3f MiB ',
          [mmus.AllocatedBytes / MEBIBYTE, mmus.OverheadBytes / MEBIBYTE] );


 for i := 0 to High(mm_list) do
 with mm_list[i] do
  if op_count > 0 then
     wprintf (#9#9#9#9#9' mm_list[%d] cb_alloc = %.5f GiB, cb_freed = %.5f GiB, op_count = %.1fM', [i, ( cb_alloc / GiB ), ( cb_freed / GiB ), op_count / 1e6 ] );
end;


function IsDebuggerPresent: Boolean; stdcall; external 'kernel32.dll';


procedure LoadExeParams;
var
   n: Integer;
   s: String;

begin
 FillChar (szExe, sizeof(szExe), 0);
 GetModuleFileName ( $400000, szExe, MAX_PATH );
 StrLower(szExe);

 for n := 1 to ParamCount do
    begin
     if n > 1 then s := s + '|';
     s := s + ParamStr(n);
    end;

 log_proc( PChar('ExeParams: ' + s));

 StrLCopy ( szParams, PChar (s), 1000 );

 s := '';
end;

{ TDebugMemoryManager }

function TDebugMemoryManager.Enabled: Boolean;
begin
 result := can_used and ( ( in_except = 0 ) or ( ex_safe ) );
end;

function TDebugMemoryManager.FreeMem(P: Pointer; Size: NativeInt): Integer;
begin
 result := 0;
 if DWORD(P) < $10000 then exit;

 if not Assigned(_FreeMem) then
   begin
    OutputDebugString('#FATAL: _FreeMem = nil');
    result := -1;
    exit
   end;
 InterlockedIncrement (_exception_quiet);
 try
   try
    result := _FreeMem (p);
   except
    on E: EAccessViolation do
      begin
       OutputDebugString ('#EXCEPTION: inside _FreeMem ');
       result := 0;
      end;
   end;
 finally
  InterlockedDecrement (_exception_quiet);
 end;
 OnFree(size);
end;

function TDebugMemoryManager.GetMem(size: NativeInt): Pointer;
begin
 result := _GetMem(Size);
 OnAlloc(Size);
end;

procedure TDebugMemoryManager.OnAlloc(size: NativeInt);
var
   diff: Int64;
   l, i: Integer;

begin
 Inc(cb_alloc, Size);
 Inc(op_count);
 diff := cb_alloc - cb_freed;
 if diff > cb_diff + 100 * MEBIBYTE then
   begin
    Inc (cb_diff, 100 * MEBIBYTE);
    wprintf ('~C0F #PERF(luaicp)~C07: memory usage raised to %.1f MiB', [diff / MEBIBYTE]);
    for l := 0 to 255 do
     for i := 0 to 255 do
      with a_points[l][i] do
       if df_alloc > MEBIBYTE div 4 then
          wprintf(' [%.2x:%.2x] EIP = $%.8x, df_alloc = %10.1f MiB, a = %9.3fM, r = %9.fM, f = %9.fM ',
                         [l, i, code_ptr, df_alloc / MEBIBYTE, op_alloc / 1e6, op_realloc / 1e6, op_free / 1e6]);
   end;

 if diff < cb_diff - 100 * MEBIBYTE then
   begin
    Dec (cb_diff, 100 * MEBIBYTE);
    wprintf ('~C0A #PERF(luaicp)~C07: memory usage lowered to %.1f MiB', [diff / MEBIBYTE]);
   end;


end;

procedure TDebugMemoryManager.OnFree(size: NativeInt);
begin
 Inc (cb_freed, Size); // TODO: thread unsafe
 Inc(op_count);
end;

function TDebugMemoryManager.Realloc(P: Pointer; OldSize, NewSize: NativeInt): Pointer;
begin
 result := _ReallocMem (P, NewSize);
 OnAlloc (NewSize);
 OnFree (OldSize);
end;

function TestMaxAlloc: NativeUInt;
const
     BLOCK_SZ = 16;
     MEBIBYTE = 1024 * 1024;
var
   tmp: TList;
     s: String;
     p: Pointer;
     h: DWORD;
     n: Integer;
begin
 result := 0;
 tmp := TList.Create;
 try
  h := GetProcessHeap;
  tmp.Capacity := 8192;
  ODS('[~T].~C0E #MEM: TestMaxAlloc pointers:~C0F ');
  s := '';

  for n := 0 to 4095 do
   try
    p := HeapAlloc (h, HEAP_GENERATE_EXCEPTIONS or HEAP_NO_SERIALIZE, BLOCK_SZ * MEBIBYTE);
    if p <> nil then
      begin
       tmp.Add (p);
       s := s + ' ' + FormatPtr(p);
       if n and $F = $F then
         begin
          ODS (s);
          s := '';
         end;
      end;
   except
    on E: EExternalException do
       break;

    on E: Exception do
      begin
       PrintError ( 'TestMaxAlloc, pass #' + IntToStr(n) + ' catched exception ' + E.ClassName + ': ' + E.Message );
       break;
     end;
   end;



  result := tmp.Count * BLOCK_SZ;

  for n := tmp.Count - 1 downto 0 do
   if DWORD (tmp [n]) > $0000000 then
     begin
      HeapFree ( h, HEAP_NO_SERIALIZE, tmp [n] );
      tmp.Delete (n);
     end;
  ODS('~C07... reserved for test =~C0D ' + IntToStr (tmp.Count * BLOCK_SZ) + '~C07' );


 finally
  tmp.Free;
 end;
end;

{$I ..\lib\FastMM4Options.inc}
initialization
 madExcept.NameThread(GetCurrentThreadId, 'main' );

 IsMultiThread := TRUE;

 gHeap := GetProcessHeap;

 GetMemoryManager(oldmm);

 FillChar (mm_list, sizeof(mm_list), 0);
 FillChar (a_points, sizeof(a_points), 0);

 with mm_list [0] do
  begin
   name        := 'Xray';
   can_used    := FALSE;
   _FreeMem    := XrFree;
   _GetMem     := XrTryAlloc;
   _ReallocMem := XrRealloc;
  end;

 with mm_list [1] do
  begin
   name        := 'FastMM4';
   can_used := TRUE;
   ex_safe := TRUE;
   {$IFDEF FullDebugMode}
   _FreeMem    := FastMM4.DebugFreeMem;
   _GetMem     := FastMM4.DebugGetMem;
   _ReallocMem := FastMM4.DebugReallocMem;
   {$ELSE}
   _FreeMem    := FastMM4.FastFreeMem;
   _GetMem     := FastMM4.FastGetMem;
   _ReallocMem := FastMM4.FastReallocMem;
   {$ENDIF}
  end;

 with mm_list [2] do
  begin
   name        := 'SysHeap';
   can_used := TRUE;
   _FreeMem    := HeapFreeMem;
   _GetMem     := HeapAllocMem;
   _ReallocMem := HeapReallocMem;
  end;

 LoadExeParams;

 ptr_first := MAXINT;
 ptr_last  := 0;

 xrmm.GetMem := UniGetMem;
 xrmm.FreeMem := UniFreeMem;
 xrmm.AllocMem := UniAllocMem;
 xrmm.ReallocMem := UniReallocMem;

 xrmm.RegisterExpectedMemoryLeak := RegisterExpectedMemoryLeak;
 xrmm.UnregisterExpectedMemoryLeak := UnRegisterExpectedMemoryLeak;
 FindMemoryObject;


 // if Pos(XR_EXE, LowerCase(sExe) ) > 0 then
 SetMemoryManager(xrmm);
 InitMM (nil);

 log_proc('XrayMM: Initialization completed');
finalization

 DumpStats;
 // SetMemoryManager(oldmm);
end.
