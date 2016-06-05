unit XrayImports;

interface
uses Windows, SysUtils, Classes, Misc, LCGlobals, LuaTypes;
{$I stkdef.inc}

type
{
   	char *			        data;
	int				Pos;
	int				Size;
	int				iterpos;
}
  IReaderStruct = packed object
   vftable: Pointer;
 {$IFDEF NEWEST_BUILD}
      desc: Pointer;
       pos: Int64;
      size: Int64;
   iterpos: Int64;
      data: PByteArray;
   nothing: DWORD;         // выравнивание по 8 байтам
 {$ELSE}
      data: PByteArray;
       pos: Integer;
      size: Integer;
   iterpos: Integer;
 {$ENDIF}

   function   Read (var buff; cb: Integer): Integer;
  end;

  IReader = ^IReaderStruct;


var
           xr_core: HMODULE;

        TEXT2CLSID: function ( text: LPSTR ): UInt64; stdcall;

    Factory_Create: function (CLS_ID: UInt64): Pointer; cdecl;
   Factory_Destroy: procedure (obj: Pointer); cdecl;

      LogStackTrace: procedure  (header: LPSTR); cdecl = nil;
    LogStackTraceEx: procedure  (pExPtrs: PEXCEPTION_POINTERS); cdecl = nil;
        SetUserName: procedure  (user_name: LPSTR); cdecl = nil;
    GetUserdataInfo: function (L: lua_State; index: Integer; buffer: LPSTR; buff_size: size_t): Boolean; cdecl = nil;
    GetFunctionInfo: function (p: Pointer; buffer: LPSTR; disp: PDWORD): Boolean; cdecl = nil;
      GetObjectInfo: function (p: Pointer; buffer: LPSTR; disp: PDWORD): Boolean; cdecl = nil;
       SetActiveLua: procedure (L: lua_State); cdecl = nil;
             vminfo: procedure (var _free, _resv, _commitd: SIZE_T); cdecl = nil;

       alife_object: function (id: WORD): Pointer; cdecl = nil;
      engine_object: function (id: WORD): Pointer; cdecl = nil;



   CLocatorAPI: record
            xr_FS: PPointer;
           r_open: function (path, _fname: String): IReader; cdecl; // 2xLPCSTR
          r_close: procedure (var r: IReader); cdecl; // ^IReader
    rescan_pathes: procedure; cdecl;
         get_path: function (path: LPSTR): PFSPath;
      update_path: function (dest, initial, src: PAnsiChar): PAnsiChar; cdecl;
   end;


function  dock_str(const s: String): pstr_value;

procedure InitImports (const sMod: String; hMod: HMODULE);
function  XrFileOpen(const path, file_name: String): IReader;
procedure XrFileClose(var r: IReader);
function  XrGetPath(const path: String): PFSPath;
procedure XrRescanPathes(const path: String = '');
function  XrUpdatePath(const path, src: String): String;

{$IFDEF NEWEST_BUILD}
var
       AuxLua: function: lua_State; cdecl = nil;
      GameLua: function: lua_State; cdecl = nil;
 get_lvm_name: function(L: lua_State): PAnsiChar; cdecl = nil;
 set_lvm_name: procedure(L: lua_State; name: PAnsiChar; add_index: Boolean = TRUE); cdecl = nil;

{$ENDIF}




implementation
Uses Math;

var
     g_pStringContainer: Pointer = nil;
    str_container__dock: function (str: LPCSTR): pstr_value cdecl = nil;




procedure _cap0 (L: lua_State); cdecl;
begin

end;




function GetObjAddress(hMod: HMODULE; const name: String): Pointer;
var
   pp: PPointer;
begin
 result := nil;
 pp := GetProcAddress (hMod, PChar(name));
 if pp <> nil then
    result := pp^;
end;


procedure InitImports (const sMod: String; hMod: HMODULE);
begin
 if UpperCase (sMod) = 'XRCORE.DLL' then
   begin
    xr_core                   := GetModuleHandle ('xrCore.dll');
    g_pStringContainer        := GetProcAddress (hMod, '?g_pStringContainer@@3PAVstr_container@@A');
    TEXT2CLSID                := GetProcAddress (hMod, '?TEXT2CLSID@@YG_KPBD@Z');
    str_container__dock       := GetProcAddress (hMod, '?dock@str_container@@QAEPAUstr_value@@PBD@Z');
    SetUserName               := GetProcAddress (hMod, '?SetUserName@@YAXPBD@Z');
    vminfo                    := GetProcAddress (hMod, '?vminfo@@YAXPAI00@Z');
    CLocatorAPI.xr_FS         := GetProcAddress (hMod, '?xr_FS@@3PAVCLocatorAPI@@A');
    CLocatorAPI.r_open        := GetProcAddress (hMod, '?r_open@CLocatorAPI@@QAEPAVIReader@@PBD0@Z');
    CLocatorAPI.r_close       := GetProcAddress (hMod, '?r_close@CLocatorAPI@@QAEXAAPAVIReader@@@Z');
    CLocatorAPI.get_path      := GetProcAddress (hMod, '?get_path@CLocatorAPI@@QAEPAVFS_Path@@PBD@Z');
    CLocatorAPI.rescan_pathes := GetProcAddress (hMod, '?rescan_pathes@CLocatorAPI@@QAEXXZ');
    CLocatorAPI.update_path   := GetProcAddress (hMod, '?update_path@CLocatorAPI@@QAEPBDAAY0CAI@DPBD1@Z');
    GetFunctionInfo           := GetProcAddress (hMod, '?GetFunctionInfo@@YAHPAXPADPAK@Z');
    GetObjectInfo             := GetProcAddress (hMod, '?GetObjectInfo@@YAHPAXPADPAK@Z');
   end;
 if UpperCase (sMod) = 'XRGAME.DLL' then
   begin
    Factory_Create  := GetProcAddress (hMod, 'xrFactory_Create');
    Factory_Destroy := GetProcAddress (hMod, 'xrFactory_Destroy');
    GetUserdataInfo := GetProcAddress (hMod, '?GetUserdataInfo@@YA_NPAUlua_State@@HPADI@Z');
    SetActiveLua    := GetProcAddress (hMod, '?SetActiveLua@@YAXPAUlua_State@@@Z');
    GameLua         := GetProcAddress (hMod, '?GameLua@@YAPAUlua_State@@XZ');
    AuxLua          := GetProcAddress (hMod, '?AuxLua@@YAPAUlua_State@@XZ');
    alife_object    := GetProcAddress (hMod, '?alife_object_by_id@@YAPAVCSE_ALifeDynamicObject@@G@Z');
    engine_object   := GetProcAddress (hMod, '?client_object_by_id@@YAPAVCObject@@G@Z');
    get_lvm_name    := GetProcAddress (hMod, '?get_lvm_name@@YAPBDPAUlua_State@@@Z');
    set_lvm_name    := GetProcAddress (hMod, '?set_lvm_name@@YAXPAUlua_State@@PBD_N@Z');
   end;
end;


function  dock_str(const s: String): pstr_value;
var
   sa: AnsiString;
begin
 result := nil;
 sa := AnsiString (s);
 asm
  mov  eax, g_pStringContainer
  mov  ecx, [eax]
  test ecx, ecx
  js   @exit
  push sa
  call str_container__dock
  mov  result, eax
@exit:
 end;
end;

function XrFileOpen(const path, file_name: String): IReader;
var
   p, f: AnsiString;
begin
 p := AnsiString(path);
 f := AnsiString(file_name);
 result := nil;

 if (CLocatorAPI.xr_FS = nil) or not Assigned (CLocatorAPI.xr_FS^) or not Assigned(CLocatorAPI.r_open) then exit;

 // wprintf('[~T]. #DBG(XrFileOpen): xr_FS = %p', [CLocatorAPI.xr_FS^]);
 asm
  mov  eax, CLocatorAPI.xr_FS
  mov  ecx, [eax]
//  mov  eax, f
  push f
//  mov  eax, p
  push p
  call CLocatorAPI.r_open
  mov  result, eax
 end;
 // result := CLocatorAPI.r_open(PAnsiChar(p), PAnsiChar(f));

 if Assigned(result) then
   begin
    Assert ( Assigned(result.vftable), 'IReader.vtfable unassigned');
    Assert ( Assigned(result.data),    'IReader.data    unassigned');
    if result.Size > 0 then
       Assert ( result.pos < result.Size, Format('position %d >= size %d', [result.pos, result.size]));
   end;

end;

procedure XrFileClose(var r: IReader);
var
   p: Pointer;
begin
 p := @r;
 if (r = nil) or (CLocatorAPI.xr_FS = nil) or not Assigned (CLocatorAPI.xr_FS^) or not Assigned(CLocatorAPI.r_close) then exit;
 asm
  mov  eax, CLocatorAPI.xr_FS
  mov  ecx, [eax]
  push p
  call CLocatorAPI.r_close
 end;

end;

function  XrGetPath(const path: String): PFSPath;
var
   sa: AnsiString;
begin
 result := nil;
 sa := AnsiString (path);
 if (CLocatorAPI.xr_FS = nil) or not Assigned (CLocatorAPI.xr_FS^) or not Assigned(CLocatorAPI.get_path) then exit;
 asm
  mov  eax, CLocatorAPI.xr_FS
  mov  ecx, [eax]
  push sa
  call CLocatorAPI.get_path
  mov  result, eax
 end;
end;


procedure XrRescanPathes(const path: String);
var
   pp: PFSPath;
begin
 if Length(path) > 3 then
  begin
   pp := XrGetPath(path);
   if pp <> nil then
      pp.m_Flags := pp.m_Flags or flNeedRescan;
  end;


 if (CLocatorAPI.xr_FS = nil) or not Assigned (CLocatorAPI.xr_FS^) or not Assigned(CLocatorAPI.rescan_pathes) then exit;
 asm
  mov  eax, CLocatorAPI.xr_FS
  mov  ecx, [eax]
  call CLocatorAPI.rescan_pathes
 end;

end;

function  XrUpdatePath(const path, src: String): String;
var
   p, s: AnsiString;
   buff: array[0..MAX_PATH * 2] of AnsiChar;
    res: PAnsiChar;
begin
 result := '#ERR_NOT_INITIALIZED';
 if (CLocatorAPI.xr_FS = nil) or not Assigned (CLocatorAPI.xr_FS^) or not Assigned(CLocatorAPI.rescan_pathes) then exit;

 p := AnsiString(path);
 s := AnsiString(src);

 asm
  mov  eax, CLocatorAPI.xr_FS
  mov  ecx, [eax]
  push s
  push p
  lea  eax, buff
  push eax
  call CLocatorAPI.update_path
  mov  res, eax
 end;

 result := AnsiTrim2W (res);
end;



{ IReaderStruct }
function IReaderStruct.Read(var buff; cb: Integer): Integer;
begin
 result := Min (cb, size - pos);
 if result <= 0 then
    begin
     result := 0;
     exit;
    end;
 Move (data[pos], buff, result);
 Inc(pos, cb);
end;




initialization
 SetActiveLua := _cap0;
end.
