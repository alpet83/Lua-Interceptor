unit XrayStatic;

interface
uses Windows, SysUtils, Classes, StrUtils, StrClasses, Misc,
     LCGlobals, XrayLua, LuaTypes, LuaTools;



function  _CUIStatic (L: lua_State): Integer; cdecl;

implementation
uses XrayMM;

type
   TGameRect = packed record
        left: Single;
         top: Single;
       right: Single;
      bottom: Single;
   end;
   TStaticFuncList = record
              CUIStatic:record
                   CUIStatic: Pointer; // constructor
                     SetText: Pointer;
                 InitTexture: Pointer;
              end;
              CUISimpleWindow: record
                  SetWndRect: Pointer;
              end;

    // methods
    function  Init: Boolean;
   end;

var
   func_list: TStaticFuncList;


function _Static_InitTexture (L: lua_State): Integer; cdecl;
begin
 result := 0;
end;

function _Static_SetText (L: lua_State): Integer; cdecl;
var
   p, f: Pointer;
   text: Pointer;
begin
 result := 0;
 p := lua_objptr (L, 1);
 f := func_list.CUIStatic.SetText;
 text := lua_objptr (L, 2); // must be shared_str

 wprintf('[~T]. #DBG(CUIStaticEx): trying SetText method at~C0F $%p~C07 for object at~C0E $%p~C07, text =~C0A $%p~C07', [f, p, text]);
 wprintf('[~T]. #DBG: _Static_SetText addr = $%p', [Addr(_Static_SetText)] );
 if ( p = nil ) or ( f = nil ) then exit;
 if IsDebuggerPresent then
    asm
     int 3
    end;

 if text <> nil then
  asm
   pushad
   push  text
   mov   ecx, p
   call  DWORD PTR [f]
   popad
  end;

end;

function _Static_SetWndRect (L: lua_State): Integer; cdecl;
var
   p, f: Pointer;
   rect: packed array [0..3] of Single;
      i: Integer;

begin

 result := 0;
 p := lua_objptr (L, 1);
 f := func_list.CUISimpleWindow.SetWndRect;

 wprintf('[~T]. #DBG(CUIStaticEx): trying SetWndRect method at $%p for object at $%p', [f, p]);
 if ( p = nil ) or ( f = nil ) then exit;
 for i := 0 to 3 do
     rect [i] := lua_tonumber (L, i + 2);

 asm
  pushad
  mov   ecx, p
  lea   ebx, rect
  push  ebx
  call  f
  popad
 end;
end;


function _Static_Index (L: lua_State): Integer; cdecl;
var
   k: String;
   f: lua_CFunction;
begin
 f := nil;
 k := LuaStrArg (L, 2);

 if k = 'InitTexture' then
    f := _Static_InitTexture
 else
 if k = 'SetText' then
    f := _Static_SetText
 else
 if k = 'SetWndRect' then
    f := _Static_SetWndRect;


 if @f <> nil then
    lua_pushcfunction (L, f)
 else
    lua_pushwstr (L, 'CUIStaticEx bad method or prop: ' + k);

 result := 1;
end;



function _CUIStatic (L: lua_State): Integer; cdecl;
var
   p, func: Pointer;
begin
 result := 1;
 if not func_list.Init then
   begin
    PrintError ('CUIStatic failed: Cannot init func_list.');
    lua_pushnil (L);
    exit;
   end;


 p := XrTryAlloc ( $158 );

 if p = nil then
   begin
    PrintError ('CUIStatic failed: Cannot allocate memory via XrMemory');
    lua_pushnil (L);
    exit;
   end;

 ZeroMemory ( p, $158 );

 func := func_list.CUIStatic.CUIStatic;

 if IsDebuggerPresent then
    asm
     int 3
    end;

 asm
  pushad
  mov   ecx, p
  call  DWORD PTR [func]
  popad
 end;

 wprintf('[~T]. #DBG: static allocated at $%p', [p]);

 AssignMetaIndex ( L, p, _Static_Index, nil, 'gmt_Static_Index' );
end;

{ TStaticFuncList }

function GetFuncAddr ( hModule: DWORD; mname: String): Pointer;
var
   s: String;
   i: NativeInt;
begin
 result := nil;
 mname := AnsiReplaceStr (mname, ':', '_');
 i := g_offsets.FindOfs(mname);
 if i = -MAXINT then
   begin
    PrintError ( Format('not found %s in %s', [mname, g_offsets.offset_map.CommaText] ) );
    exit;
   end;
 result := Ptr ( NativeInt (hModule) + i );
end;


function TStaticFuncList.Init: Boolean;
var
   hLib: DWORD;

begin
 result := ( CUIStatic.CUIStatic <> nil );
 if result then exit;

 hLib := GetModuleHandle ('xrGame.dll');
 if hLib = 0 then exit;
 CUISimpleWindow.SetWndRect    := GetFuncAddr (hLib, 'CUISimpleWindow__SetWndRect');

 CUIStatic.CUIStatic            := GetFuncAddr (hLib, 'CUIStatic__CUIStatic');
 CUIStatic.SetText              := GetFuncAddr (hLib, 'CUIStatic__SetText');
 CUIStatic.InitTexture          := GetFuncAddr (hLib, 'CUIStatic__InitTexture');
 result := ( CUIStatic.CUIStatic <> nil );

end;

initialization
 FillChar ( func_list, sizeof(func_list), 0 );

end.
