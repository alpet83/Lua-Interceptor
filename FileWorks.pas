unit FileWorks;

interface

uses Windows, SysUtils, StrUtils, Classes, Math, ContNrs, IniFiles, StrClasses, misc, IPCUtils,
                LuaTypes, XrayLua, LuaHelp, LCGlobals, LuaTools,  UniArray, DateTimeTools, WThreads, MD5;



procedure AddGlobals(L: lua_State);

implementation


function _CloseFileMapping (L: lua_State): Integer; cdecl;
var
   map: ^TFileMapping;
begin
 map := lua_objptr ( L, 1 );
 result := 0;
 if not Assigned (map) then exit;
 map.Close;
 if map.hFile <> 0 then
    CloseHandle (map.hFile);
 FreeMem (map);
end;

function _FileMappingIndex (L: lua_State): Integer; cdecl;
var
   idx: String;
   map: ^TFileMapping;
begin
 result := 1;
 map := lua_objptr ( L, 1 );
 idx := LuaStrArg (L, 2);

 if ( idx = 'data' ) or ( idx = 'view' ) then
      lua_pushptr ( L, map.pView ) else
 if ( idx = 'file_size' ) then
      lua_pushinteger ( L, map.fSize.QuadPart ) else
 if ( idx = 'free' ) or ( idx = 'close' ) then
      lua_pushcfunction ( L, _CloseFileMapping )
 else
      lua_pushnil (L);
end;


function _LuaMapFile(L: lua_State): Integer; cdecl;
var
   fname: String;
     map: ^TFileMapping;
     ofs: UInt64;


begin
 fname := LuaStrArg (L);
 fname := FmtStalkerPath (fname);

 result := 1;

 map := AllocMem ( sizeof(TFileMapping) );
 map.Init (0);


 ofs := 0;
 if lua_gettop (L) > 2 then
   begin
    ofs := LuaDWORD (L, 2);
    map.nSize := LuaDWORD (L, 3);
   end;

 if not map.Create (fname, '', FALSE) then
  begin
   PrintError ( Format('TFileMapping.Create failed for %s, LastError: %s ', [fname, err2str] ) );
   lua_pushnil (L);
   FreeMem (map);
   exit;
 end;

 map.MapView (ofs);
 AssignMetaIndex ( L, map, _FileMappingIndex, nil, 'FileMappingIndex' );
end;


procedure AddGlobals(L: lua_State);
begin
 lua_register (L, 'FileMapping', _LuaMapFile);
end;

end.
