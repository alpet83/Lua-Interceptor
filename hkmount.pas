unit hkmount;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, Messages, TlHelp32, ImageHlp;

{$A-}
{$I x86.inc}


type
    TCPUCmd_1a4 = packed record
     op_code: BYTE;
       value: DWORD;
    end;

    IMAGE_IMPORT_DESCRIPTOR = packed record
      f: record
         case byte of
          0: (Characteristics: DWORD);            // 0 for terminating null import descriptor
          1: (OriginalFirstThunk: DWORD);         // RVA to original unbound IAT (PIMAGE_THUNK_DATA)
         end;

     TimeDateStamp: DWORD;                  // 0 if not bound,
                                            // -1 if bound, and real date\time stamp
                                            //     in IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT (new BIND)
                                            // O.W. date/time stamp of DLL bound to (Old BIND)

     ForwarderChain: DWORD;                 // -1 if no forwarders
     Name: DWORD;
     FirstThunk: DWORD;                     // RVA to IAT (if bound this IAT has actual addresses)
    end; // IID

    PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;

    IMAGE_THUNK_DATA = packed record
    case BYTE of
     0:(ForwarderString: DWORD);      // PBYTE
     1:(func_ofs: DWORD);             // PDWORD
     2:(_ordinal: DWORD);
     3:(AddressOfData: DWORD);        // PIMAGE_IMPORT_BY_NAME
    end; // IMAGE_THUNK_DATA

var
   OnInstall: procedure = nil;

procedure  InstallTo(hWnd, tid: DWORD); cdecl;

procedure  PathIATEntry (hMod: THandle; const fromDLL: String; pfCurr, pfNew: Pointer; const funcName: String = '?');

function   EnumDLLs (pid: DWORD): TList; // получить список hModule для DLL обозначенного процесса

procedure  CreateCapture(pCurr, pNew: PByteArray; chk_byte, prolog_bytes: Byte);
procedure  CreateCapturePRE (pCurr, pNew: PByteArray; prolog_bytes: DWORD; fb: BYTE; const funcName: String = '?');
procedure  Write_PushDWORD ( addr_dest, value: Pointer; safe: Boolean );

implementation


const
    HKINST = $3456789A;
    HKMSG = WM_NULL;


    gEventName = 'Global\hook_mount_evt1243';
type
    PTagMsg = ^tagMSG;

var
      hhk: THandle = 0;
    first: bool = false;
    inst_evt: THandle;


procedure Write_PushDWORD ( addr_dest, value: Pointer; safe: Boolean );
var
   cmd: TCPUCmd_1a4;
    wb: NativeUInt;
begin
 cmd.op_code := op_push_dw;
 cmd.value := DWORD (value);
 if safe then
    WriteProcessMemory ( GetCurrentProcess, addr_dest, @cmd, sizeof(cmd), wb )
 else
    Move ( cmd, addr_dest^, sizeof (cmd) );
end;




function    HkGetMsgProc (code, wp, lp : LongInt ) : LongInt; stdcall;
var
   pmsg: PTagMsg;

begin
 pmsg := Pointer (lp);
 result := 0;
 if (hhk = 0) and (pmsg <> nil) and
    (pmsg.wParam = HKINST) and (pmsg.lParam <> 0) then
    begin
     hhk := pmsg.lparam;
     inst_evt := OpenEvent (EVENT_MODIFY_STATE, TRUE, gEventName);
     if inst_evt <> 0 then
        SetEvent (inst_evt);
     if Assigned (OnInstall) then OnInstall;
     exit;
    end;
 if hhk <> 0 then result := CallNextHookEx (hhk, code, wp, lp);
end;

procedure  InstallTo(hWnd, tid: DWORD); cdecl;
var
    hinst: DWORD;
     proc: Pointer;
    n: Integer;
begin
 ODS(Format('[~T]. Mounting DLL by message-hook method, hWnd = $%x, ThreadID = $%x ', [hWnd, tid]));
 hinst := HINSTANCE;
 proc := @HkGetMsgProc;
 inst_evt := CreateEvent(nil, TRUE, FALSE, gEventName);

 hhk := SetWindowsHookEx (WH_GETMESSAGE, proc, hinst, tid);


 if hhk = 0 then
  begin
   PrintError('Cannot set windows hook, error #' + IntToStr(GetLastError));
   exit;
  end;


 PostMessage (hwnd, HKMSG, HKINST, hhk);

 for n := 0 to 15 do
  if WaitForSingleObject (inst_evt, 1000) = WAIT_OBJECT_0 then
    begin
     ODS ('[~T]. #MSG: Hook installed - press ENTER for auto-unhook...');
     ReadLn;
     break;
    end;

 XSleep(1500);
end;



procedure  PathIATEntry (hMod: THandle; const fromDLL: String; pfCurr, pfNew: Pointer; const funcName: String);
// элементарная замена адреса функции в таблице импорта
type
    TIIDList = packed array [0..1023] of IMAGE_IMPORT_DESCRIPTOR;
    PIIDList = ^TIIDList;
    TThunkList32 = packed array [0..1023] of DWORD;
    PThunkList = ^TThunkList32;

var
   pd: PIIDList; //
   ptl: PThunkList;
   rBytes: SIZE_T;
   ulSize: DWORD;
   paddr: PDWORD;
   pb: Pointer;
   n, i: Integer;
   szModName: PAnsiChar;
   tmp: array [0..261] of CHAR;
   smod, tmod: String;
begin
 ulSize := 0;
 i := -1;
 GetModuleFileName(hMod, tmp, 260);
 tmod := Trim(tmp);
 if ( Pos ('\Windows', tmod) > 0 ) or ( Pos ('syswow64', tmod) > 0 ) or ( Pos('luatest.dll', tmod) > 0 )  then exit;
 pd := ImageDirectoryEntryToData (Ptr(hMod), TRUE, IMAGE_DIRECTORY_ENTRY_IMPORT, ulSize);

 if pd = nil then
  begin
   n := GetLastError;
   PrintError(Format('ImageDirectoryEntryToData returned nil for module~C0F %s~C0C [$%x],  LastError = %d', [tmod, hMod, n]) );
   exit;
  end;

 // ODS(Format('[~T].~C0E #DBG: checking imports for module ~C0A %s~C0E [$%x]~C07', [tmod, hMod]) );
 // ulSize div sizeof (IMAGE_IMPORT_DESCRIPTOR) - 1
 for n := 0 to 1023 do
 if pd[n].Name <> 0 then
  begin
   szModName := Ptr (hMod + pd[n].Name);
   smod := AnsiTrim2W(szModName);
   smod := ExtractFileName(smod);
   // ODS(#9 + smod);
   if UpperCase(smod) <> UpperCase(fromDLL) then continue;
   i := n;
   break;
  end
 else break;
 // в директории нет ссылок на функции модуля fromDLL
 if i < 0 then exit;
 // ODS(Format('[~T]. #DBG: Scaning for function~C0D $%p~C07 imported by module~C0A %s~C07', [pfCurr, tmod] ));

 ptl := Ptr(hMod + pd[i].FirstThunk);
 i := -1;
 for n := 0 to 1023 do
 if (ptl[n] <> 0) then
  begin
   if Pointer(ptl[n]) <> pfCurr then continue;
   i := n;
   break;
  end
 else break;
 // функция уже пропатчена или отсутствует
 if i < 0 then exit;

 paddr := @ptl[i];
 pb := Ptr ( DWORD(paddr) and $FFFFF000 ); // round to page boundary

 if not VirtualProtectEx (GetCurrentProcess, pb, 4096, PAGE_EXECUTE_WRITECOPY, nil) then
    PrintError('VirtualProtectEx returned error ' + Err2Str(GetLastError) );
 ODS(Format('[~T]. #DBG: Patching module ~C0A %20s ~C0D [$%x]~C07, IATEntry at~C0D $%p~C07 (page=$%p) for function ~C0A %s~C07',
                [tmod, hMod, paddr, pb, funcName]));
 // pAddr^ := DWORD (pfNew);
 WriteProcessMemory (GetCurrentProcess, paddr, @pfNew, sizeof(Pointer), rBytes);
 // ReadProcessMemory(GetCurrentProcess,

 if (pAddr^ <> DWORD (pfNew)) or (rBytes < 4) then
    PrintError(Format('Patching failed, curr-ptr = $%x, new-ptr = $%x',  [pAddr^, DWORD (pfNew) ]))
 else
  if Pos('.exe', tmod) = 0 then
    LoadLibrary( PChar(tmod) );

end; // PathIATEntry


function EnumDLLs(pid: DWORD): TList;
var
   h: THandle;
   me: TModuleEntry32;
   s: String;
begin
 result := TList.Create;
 try
   h := CreateToolHelp32Snapshot(TH32CS_SNAPMODULE, pid);
   FillChar (me, sizeof(me), 0);
   me.dwSize := sizeof(me);
   if Module32First(h, me) then
    repeat
     s := Trim(me.szModule);
     //ODS(Format('#DBG: hModule = $%x, ModBaseAddr = $%p, ModName = %s ', [me.hModule, Pointer(me.modBaseAddr), s] ));
     result.Add ( me.ModBaseAddr );
    until ( not Module32Next(h, me) ) or (result.Count > 100);

 except
  on E: Exception do
    PrintError ('Exception in EnumDLLs: ' + E.Message);
 end;
 XSleep(1000);
end; // EnumDLLs




procedure  CreateCapture(pCurr, pNew: PByteArray; chk_byte, prolog_bytes: Byte);
// копирование части кода некоторой функции в отдельную страницу
var
   pg, pcode: PByteArray;
   pgofs: DWORD;
   retp: SIZE_T;

  procedure PutBytes(dwb: DWORD; cnt: BYTE);
  begin
   case cnt of
    1: pg[pgofs] := BYTE (dwb);
    2: PWORD( @pg[pgofs] )^ := WORD (dwb);
    4: PDWORD( @pg[pgofs] )^ := dwb;
   end;
   Inc (pgofs, cnt);
  end;

  procedure PutPushDW ( value: DWORD );
  begin
   PutBytes ( op_push_dw, 1 );
   PutBytes ( value, 4 );
  end;


  { *** механизм перехвата функций, возвращающих lua_State ()
    ---------------------------------------------------------
    Вставка push-jmp переноса на мини-код pcode.
    Миникод вызывает оригинал функции, с помощью push-jmp



  }


begin
 if pCurr[0] <> chk_byte then exit; // wrong prolog
 pcode:= VirtualAlloc (nil, 4096, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
 pg := pcode;
 pgofs := 0;
 retp := $40;
 ODS(Format(' function at~C0D $%p~C07 remapped partially to~C0D $%p~C07', [pCurr, pcode]));
 Sleep(50);
 // сохранить адрес новой точки возвращения
 {
 if dbg_present then
    PutBytes(op_int3, 1)
 else}

 PutBytes(op_nop, 1);

 // на сейчас в стеке хранятся параметры функции (8) и адрес возврата
 // нужно выполнить подмену [esp + 8] = новый адрес возврата
 PutBytes(op_pop_eax, 1);         // извлечение последнего слова из стека -> адрес возврата

 PutBytes(op_mov_ld_eax, 2);
 PutBytes(DWORD(pcode) + $82, 4); // mov [pcode + $82], eax

 PutPushDW ( DWORD(pg) + retp );  // push my-ret-point [+$40]

 // PutBytes($00244489, 4);          // move [esp + $00], eax



   Move(pCurr^, pg[pgofs], prolog_bytes);      // копировать пролог
   Inc(pgofs, prolog_bytes);
   PutPushDW( DWORD(pCurr) + prolog_bytes );    // адрес возврата - продолжение функции
   PutBytes(op_ret, 1);                         // вызвать остаток функции
   pgofs := retp; // точка возврата
   PutBytes(op_push_eax, 1);                    // схоронить результат для передачи своим

     retp := $80;                                // точка возврата в страницу

     PutPushDW ( DWORD(pcode) + retp );            // адрес возврата
     PutPushDW ( DWORD(pNew) );                    // адрес своей функции
     PutBytes(op_ret, 1);                         // вызвать свою функцию

    pgofs := retp;
    PutBytes(op_nop, 1);

    PutPushDW (0);                                // этот адрес будет замещен при вызове функции!
    PutBytes(op_ret, 1);                          // вернуться по оригинальному адресу возврата


 // забить заместо пролога - переход к куску
 pg := Ptr(DWORD(pCurr) and $FFFFF000 );
 VirtualProtect( pg, 4096, PAGE_EXECUTE_READWRITE, nil);
 pg := pcode;
 pgofs := 2048;
 PutPushDW ( DWORD(pcode) );
 PutBytes(op_ret, 1);
 WriteProcessMemory(GetCurrentProcess, pCurr, @pg[2048], 6, retp);
end;

procedure  CreateCapturePRE;
// перехват до вызова функции,



var
   pg, pcode: PByteArray;
   pgofs: DWORD;
   retp: SIZE_T;

  procedure PutBytes(dwb: DWORD; cnt: BYTE);
  begin
   case cnt of
    1: pg[pgofs] := BYTE (dwb);
    2: PWORD( @pg[pgofs] )^ := WORD (dwb);
    4: PDWORD( @pg[pgofs] )^ := dwb;
   end;
   Inc (pgofs, cnt);
  end;
  procedure PutPushDW ( value: DWORD );
  begin
   PutBytes ( op_push_dw, 1 );
   PutBytes ( value, 4 );
  end;


begin

 if pCurr[0] <> fb then
   begin
    PrintError('CreateCapturePRE: first byte of code wrong for func ' + funcName);
    exit; // wrong prolog for luaL_loadstring
   end;

 pcode:= VirtualAlloc (nil, 4096, MEM_COMMIT, PAGE_EXECUTE_READWRITE);
 pg := pcode;
 pgofs := 0;
 retp := $40;
 ODS(Format(' function ~C0A %s~C07 at~C0D $%p~C07 remapped partially to~C0D $%p~C07', [funcName, pCurr, pcode]));
 Sleep(500);

 // call my_func
 PutBytes(op_set_eax, 1);
 PutBytes(DWORD(pNew), 4);
 PutBytes(op_call_eax, 2);


 Move(pCurr^, pg[pgofs], prolog_bytes);      // копировать пролог/первые байтики
 Inc(pgofs, prolog_bytes);
 // jump back to func-rest


 PutBytes(op_set_eax, 1);
 PutBytes(DWORD(pCurr) + prolog_bytes, 4);    // адрес возврата - продолжение функции
 PutBytes(op_jump_eax, 2);

 // забить заместо пролога - переход к куску
 pg := Ptr(DWORD(pCurr) and $FFFFF000 );
 VirtualProtect( pg, 4096, PAGE_EXECUTE_READWRITE, nil);
 pg := pcode;
 pgofs := 2048;
 PutPushDW ( DWORD(pcode) );
 PutBytes ( op_ret, 1 );
 WriteProcessMemory(GetCurrentProcess, pCurr, @pg[2048], 6, retp);
end;


initialization

end.
