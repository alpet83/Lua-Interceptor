unit XrayLua;

interface
uses Windows, SysUtils, Classes, LuaTypes;

{$I stkdef.inc}




(*

 static TValue *index2adr (lua_State *L, int idx) {
  if (idx > 0) {
    TValue *o = L->base + (idx - 1);
    api_check(L, idx <= L->ci->top - L->base);
    if (o >= L->top) return cast(TValue *, luaO_nilobject);
    else return o;
  }
  else if (idx > LUA_REGISTRYINDEX) {
    api_check(L, idx != 0 && -idx <= L->top - L->base);
    return L->top + idx;
  }
  else switch (idx) {  /* pseudo-indices */
    case LUA_REGISTRYINDEX: return registry(L);
    case LUA_ENVIRONINDEX: {
      Closure *func = curr_func(L);
      sethvalue(L, &L->env, func->c.env);
      return &L->env;
    }
    case LUA_GLOBALSINDEX: return gt(L);
    default: {
      Closure *func = curr_func(L);
      idx = LUA_GLOBALSINDEX - idx;
      return (idx <= func->c.nupvalues)
                ? &func->c.upvalue[idx-1]
                : cast(TValue *, luaO_nilobject);
    }
  }
}


*)


function  luaL_newstate: Pointer; cdecl;
procedure lua_close (L: lua_State); cdecl;
function  lua_gc(L: lua_State; what, data: Integer): Integer; cdecl;


procedure luaL_openlibs(L: lua_State); cdecl;
function  luaL_loadfile(L: lua_State; filename: PAnsiChar): Integer; cdecl;
function  luaL_loadstring (L: lua_State; S: PAnsiChar): Integer; cdecl;
function  luaL_loadbuffer (L: lua_State; pbuff: Pointer; cb: SIZE_T; name: PAnsiChar): Integer; cdecl;



function lua_newstate(f: lua_Alloc; ud: Pointer) : lua_State; cdecl;
function lua_newthread(L: lua_State): lua_State; cdecl;
// function lua_setallocf(L: lua_State; f: lua_Alloc; ud: Pointer) : lua_State; cdecl;

procedure lua_createtable (L: lua_State; narr, nrec: Integer); cdecl;
function lua_getmetatable (L: lua_State; index: Integer): Integer; cdecl;
function lua_setmetatable (L: lua_State; index: Integer): Integer; cdecl;


function luaopen_base(L: lua_State): lua_Result; cdecl;
function luaopen_io(L: lua_State): lua_Result; cdecl;
function luaopen_jit(L: lua_State): lua_Result; cdecl;
function luaopen_debug(L: lua_State): Integer; cdecl;

function lua_getinfo(L: lua_state; what: PAnsiChar; ar: lua_PDebug): Integer; cdecl;


procedure lua_call(L: lua_State; nargs, nresults: Integer); cdecl;
function  lua_pcall(L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl;
function  lua_cpcall(L: lua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl;
function  lua_dump(L: lua_State; writer: lua_Chunkwriter; data: Pointer): Integer; cdecl;

function lua_error(L: lua_State): Integer; cdecl;


procedure lua_pushcclosure(L: lua_State; f: lua_CFunction; n: Integer); cdecl;
function  lua_gettop(L: lua_State): Integer; cdecl;
procedure lua_settop(L: lua_State; idx: Integer); cdecl;

procedure lua_rawget(L: lua_State; idx: Integer); cdecl;
procedure lua_rawgeti(L: lua_State; idx, n: Integer); cdecl;
function  lua_objlen(L: lua_State; idx: Integer): SIZE_T; cdecl;

function lua_isnumber(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_isstring(L: lua_State; idx: Integer): LongBool; cdecl ;
//function lua_istable(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_iscfunction(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_isfunc (L: lua_State; idx: Integer): Boolean; inline;
function lua_isuserdata(L: lua_State; idx: Integer): LongBool; cdecl;
function lua_newuserdata (L: lua_State; cb: Integer): Pointer; cdecl;
function lua_type(L: lua_State; idx: Integer): Integer; cdecl;
function lua_typeof(L: lua_State; idx: Integer): String;
function lua_typename(L: lua_State; tp: Integer): PAnsiChar; cdecl;



function lua_equal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_rawequal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;
function lua_lessthan(L: lua_State; idx1, idx2: Integer): LongBool; cdecl;

function lua_tointeger(L: lua_State; idx: Integer): lua_Integer; cdecl;
function lua_tonumber(L: lua_State; idx: Integer): lua_Number; cdecl;
function lua_toboolean(L: lua_State; idx: Integer): LongBool; cdecl ;
function lua_tolstring(L: lua_State; idx: Integer; var sl: DWORD): PAnsiChar; cdecl;
function lua_tostring(L: lua_State; idx: Integer): PAnsiChar; inline;
function lua_strlen(L: lua_State; idx: Integer): Cardinal; cdecl;
function lua_tocfunction(L: lua_State; idx: Integer): lua_CFunction; cdecl;
function lua_touserdata(L: lua_State; idx: Integer): Pointer; cdecl;
function lua_tothread(L: lua_State; idx: Integer): lua_State; cdecl;
function lua_topointer(L: lua_State; idx: Integer): Pointer; cdecl;

procedure lua_pushboolean(L: lua_State; b: LongBool); cdecl;
procedure lua_pushvalue(L: lua_State; idx: Integer); cdecl;
procedure lua_pushstring(L: lua_State; s: PAnsiChar); cdecl;
procedure lua_pushnumber(L: lua_State; n: lua_Number); cdecl;
procedure lua_pushinteger(L: lua_State; n: Integer); cdecl;
procedure lua_pushlightuserdata(L: lua_State; p: Pointer); cdecl;
function  lua_pushthread(L: lua_State): Integer; cdecl;

procedure lua_gettable(L: lua_State; idx: Integer); cdecl;
procedure lua_settable(L: lua_State; idx: Integer); cdecl;
function  lua_istable(L: lua_State; n: Integer): Boolean;
function  lua_next(L: lua_State; idx: Integer): Integer; cdecl;



function  lua_setfenv (L: lua_State; idx: Integer): Integer; cdecl;
procedure lua_setfield  (L: lua_State; idx: Integer; k: PAnsiChar); cdecl;
procedure lua_getfield(L: lua_State; idx: Integer; const k: PAnsiChar); cdecl;

function  lua_atpanic(L: lua_State; panicf: lua_CFunction): lua_CFunction; cdecl;

function  lua_sethook (L: lua_State; f: lua_Hook; mask, count: Integer): Integer; cdecl;

procedure lua_pushnil (L: lua_State); cdecl;
procedure lua_pushptr (L: lua_State; p: Pointer; apply_mt: Boolean = FALSE);
procedure lua_pushwstr (L: lua_State; const s: String);

procedure lua_pushcfunction(L: lua_State; f: lua_CFunction);

procedure lua_register(L: lua_state; name: String; f: lua_CFunction);
function  luaL_dostring(L: lua_state; s: PAnsiChar): lua_Result;
procedure lua_pop(L: lua_State; n: Integer);
procedure lua_remove(L: lua_State; n: Integer); cdecl;


procedure lua_pushraw (L: lua_State; const data: lua_TValue);
function  lua_toraw (L: lua_State; idx: Integer): lua_TValue;

function  lua_sk_field (L: lua_State; const key: String; tab_index: Integer ): Integer; // returns type of field

function  lua_getglobal         (L: lua_State; const s: String): Integer;
procedure lua_setglobal         (L: lua_State; const s: String);
procedure lua_delglobal         (L: lua_State; const s: String); // remove global from _G = set nil

procedure lua_setnvalue         (L: lua_State; k: String; v: lua_Number; t_idx: Integer = -3);
procedure lua_setsvalue         (L: lua_State; k, v: String; t_idx: Integer = -3);
function  lua_getglobalfunc     (L: lua_State; fname: String): String;
procedure lua_xmove             (L_from, L_to: lua_State; n: Integer); cdecl;




implementation
uses Misc, LuaTools, LCGlobals;



function  luaL_newstate: Pointer; cdecl; external LUA_DLL;
procedure luaL_openlibs(L: lua_State); cdecl; external LUA_DLL;
function  luaL_loadfile(L: lua_State; filename: PAnsiChar): Integer; cdecl; external LUA_DLL;
function  luaL_loadstring(L: lua_State; S: PAnsiChar): Integer; cdecl; external LUA_DLL;
function  luaL_loadbuffer (L: lua_State; pbuff: Pointer; cb: SIZE_T; name: PAnsiChar): Integer; cdecl; external LUA_DLL;




function lua_newstate(f: lua_Alloc; ud: Pointer) : lua_State; cdecl;                   external LUA_DLL;
procedure lua_close (L: lua_State); cdecl;                                             external LUA_DLL;
function lua_newthread(L: lua_State): lua_State; cdecl;                                external LUA_DLL;
function  lua_gc(L: lua_State; what, data: Integer): Integer; cdecl;                   external LUA_DLL;
// function lua_setallocf(L: lua_State; f: lua_Alloc; ud: Pointer) : lua_State; cdecl;    external LUA_DLL;


procedure lua_createtable (L: lua_State; narr, nrec: Integer); cdecl; external LUA_DLL;
function lua_setmetatable (L: lua_State; index: Integer): Integer; cdecl; external LUA_DLL;
function lua_getmetatable (L: lua_State; index: Integer): Integer; cdecl; external LUA_DLL;

function lua_next(L: lua_State; idx: Integer): Integer; cdecl; external LUA_DLL;

function luaopen_base(L: lua_State): lua_Result; cdecl; external LUA_DLL;
function luaopen_io(L: lua_State): lua_Result; cdecl; external LUA_DLL;
function luaopen_jit(L: lua_State): lua_Result; cdecl; external LUA_DLL;
function luaopen_debug(L: lua_State): Integer; cdecl; external LUA_DLL;



procedure lua_call(L: lua_State; nargs, nresults: Integer); cdecl; external LUA_DLL;
function  lua_pcall(L: lua_State; nargs, nresults, errfunc: Integer): Integer; cdecl; external LUA_DLL;
function  lua_cpcall(L: lua_State; func: lua_CFunction; ud: Pointer): Integer; cdecl; external LUA_DLL;
function  lua_dump(L: lua_State; writer: lua_Chunkwriter; data: Pointer): Integer; cdecl; external LUA_DLL;

function lua_error(L: lua_State): Integer; cdecl; external LUA_DLL;


procedure lua_pushcclosure(L: lua_State; f: lua_CFunction; n: Integer); cdecl; external LUA_DLL;
function  lua_gettop(L: lua_State): Integer; cdecl; external LUA_DLL;
procedure lua_settop(L: lua_State; idx: Integer); cdecl; external LUA_DLL;


procedure lua_rawget(L: lua_State; idx: Integer); cdecl; external LUA_DLL;
procedure lua_rawgeti(L: lua_State; idx, n: Integer); cdecl; external LUA_DLL;
function  lua_objlen(L: lua_State; idx: Integer): SIZE_T; cdecl; external LUA_DLL;



function lua_isnumber(L: lua_State; idx: Integer): LongBool; cdecl; external LUA_DLL;
function lua_isstring(L: lua_State; idx: Integer): LongBool; cdecl; external LUA_DLL;

function lua_iscfunction(L: lua_State; idx: Integer): LongBool; cdecl; external LUA_DLL;

function lua_isuserdata(L: lua_State; idx: Integer): LongBool; cdecl;  external LUA_DLL;
function lua_newuserdata (L: lua_State; cb: Integer): Pointer; cdecl;  external LUA_DLL;

function lua_type(L: lua_State; idx: Integer): Integer; cdecl; external LUA_DLL;
function lua_typename(L: lua_State; tp: Integer): PAnsiChar; cdecl; external LUA_DLL;

function lua_typeof(L: lua_State; idx: Integer): String;
var
   t: Integer;
begin
 t := lua_type(L, idx);
 result := String ( lua_typename (L, t ) );
end;


function lua_equal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL;
function lua_rawequal(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL;
function lua_lessthan(L: lua_State; idx1, idx2: Integer): LongBool; cdecl; external LUA_DLL;

function lua_tointeger(L: lua_State; idx: Integer): lua_Integer; cdecl; external LUA_DLL;
function lua_tonumber(L: lua_State; idx: Integer): lua_Number; cdecl; external LUA_DLL;
function lua_toboolean(L: lua_State; idx: Integer): LongBool; cdecl external LUA_DLL;
function lua_tolstring(L: lua_State; idx: Integer; var sl: DWORD): PAnsiChar; cdecl; external LUA_DLL;
function lua_tostring(L: lua_State; idx: Integer): PAnsiChar; inline;
var rlen: SIZE_T;
begin
 result := lua_tolstring(L, idx, rlen);
end;


function lua_strlen(L: lua_State; idx: Integer): Cardinal; cdecl; external LUA_DLL;
function lua_tocfunction(L: lua_State; idx: Integer): lua_CFunction; cdecl; external LUA_DLL;
function lua_touserdata(L: lua_State; idx: Integer): Pointer; cdecl; external LUA_DLL;
function lua_tothread(L: lua_State; idx: Integer): lua_State; cdecl; external LUA_DLL;
function lua_topointer(L: lua_State; idx: Integer): Pointer; cdecl; external LUA_DLL;

procedure lua_pushnil (L: lua_State); cdecl; external LUA_DLL;
procedure lua_pushboolean(L: lua_State; b: LongBool); cdecl; external LUA_DLL;
procedure lua_pushvalue(L: lua_State; idx: Integer); cdecl; external LUA_DLL;
procedure lua_pushstring(L: lua_State; s: PAnsiChar); cdecl; external LUA_DLL;
procedure lua_pushnumber(L: lua_State; n: lua_Number); cdecl; external LUA_DLL;
procedure lua_pushinteger(L: lua_State; n: Integer); cdecl; external LUA_DLL;
procedure lua_pushlightuserdata(L: lua_State; p: Pointer); cdecl; external LUA_DLL;
function  lua_pushthread(L: lua_State): Integer; cdecl; external LUA_DLL;

procedure lua_remove(L: lua_State; n: Integer); cdecl; external LUA_DLL;


procedure lua_gettable(L: lua_State; idx: Integer); cdecl; external LUA_DLL;
procedure lua_settable(L: lua_State; idx: Integer); cdecl; external LUA_DLL;

function  lua_setfenv (L: lua_State; idx: Integer): Integer; cdecl; external LUA_DLL;
procedure lua_setfield(L: lua_State; idx: Integer; k: PAnsiChar); cdecl; external LUA_DLL;
procedure lua_getfield(L: lua_State; idx: Integer; const k: PAnsiChar); cdecl; external LUA_DLL;

function  lua_getinfo(L: lua_state; what: PAnsiChar; ar: lua_PDebug): Integer; cdecl; external LUA_DLL;
function  lua_atpanic(L: lua_State; panicf: lua_CFunction): lua_CFunction; cdecl; external LUA_DLL;
function  lua_sethook(L: lua_State; f: lua_Hook; mask, count: Integer): Integer; cdecl; external LUA_DLL;
procedure lua_xmove  (L_from, L_to: lua_State; n: Integer); cdecl;external LUA_DLL;


procedure lua_setnvalue (L: lua_State; k: String; v: lua_Number; t_idx: Integer = -3);
begin
 lua_pushwstr (L, k);
 lua_pushnumber (L, v);
 lua_settable (L, t_idx);
end;

procedure lua_setsvalue (L: lua_State; k, v: String; t_idx: Integer = -3);
begin
 lua_pushwstr (L, k);
 lua_pushwstr (L, v);
 lua_settable (L, t_idx);
end;

function  lua_sk_field (L: lua_State; const key: String; tab_index: Integer ): Integer;
begin
 lua_pushwstr ( L, key );
 lua_gettable ( L, tab_index );
 result := lua_type (L, -1);
end;


function lua_getglobalfunc (L: lua_State; fname: String): String;
var
   tab, fid: String;
   top: Integer;

begin
 fid := '';
 if InStr ('.', fname) then
      begin
        tab := StrTok (fname, ['.']);
        lua_getglobal (L, tab);
        top := lua_gettop(L);
        if lua_istable (L, top) then
          begin
           lua_pushwstr (L, fname);
           lua_gettable (L, top); // table in stack at [1]
           if lua_isfunc (L, -1) then
              fid := 'NS-func'
           else
              PrintError('Не найдена функция ' + fname + ' в namespace/table ' + tab);

           lua_remove (L, top);  // удалить таблицу
          end;
        if fid = '' then lua_pop (L, -1);
      end
 else
      begin
       lua_getglobal (L, fname);
       fid := 'func';
      end;

 result := fid;
end; // lua_getglobalfunc

function lua_isfunc (L: lua_State; idx: Integer): Boolean;
begin
 result := (LUA_TFUNCTION = lua_type (L, idx));
end;

function lua_istable(L: lua_State; n: Integer): Boolean;
begin
  Result := (LUA_TTABLE = lua_type(L, n));
end;

procedure lua_pushptr (L: lua_State; p: Pointer; apply_mt: Boolean);
begin
 if p <> nil then
   begin
    lua_pushlightuserdata(L, p);
    if apply_mt then
       SetPointerMT (L, -1);
   end
 else
    lua_pushnil (L);


end;

procedure lua_pushwstr (L: lua_State; const s: String);
var
   sa: AnsiString;
begin
 sa := AnsiString (s);
 lua_pushstring (L, PAnsiChar (sa));
end;

procedure lua_pushcfunction(L: lua_State; f: lua_CFunction);
begin
  lua_pushcclosure(L, f, 0);
end;

function lua_getglobal(L: lua_State; const s: String): Integer;
begin
  lua_getfield (L, LUA_GLOBALSINDEX, PAnsiChar ( AnsiString (s) ) );
  result := lua_type (L, -1);
end;

procedure lua_delglobal         (L: lua_State; const s: String);
begin
 lua_pushnil (L);
 lua_setglobal (L, s);
end;

procedure lua_setglobal(L: lua_State; const s: String);
begin
  lua_setfield(L, LUA_GLOBALSINDEX, PAnsiChar ( AnsiString (s) ));
end;


procedure lua_register(L: lua_state; name: String; f: lua_CFunction);
var
   i: Integer;
   n, tn: AnsiString;
   t: String;
begin
 i := Pos('::', name);
 t := '';
 if i > 0 then
  begin
   t := Copy (name, 1, i - 1);
   name := Copy (name, i + 2, Length (name) - i - 1);
  end;

 n := AnsiString (name);
 tn := AnsiString (t);


 // если регистрация глобальной функции - добавить её в таблицу L->l_gt
 if t = '' then
  begin
   // lua_pushwstr (L, name);
   lua_pushcfunction(L, f);
   lua_setglobal (L, name);
   if lua_gettop(L) > 0 then
      lua_pop (L, -1);
   if lua_gettop(L) > 3 then
      ODS('[~T]. #DBG: Registered function ~C0A ' + name + '~C07, lua_gettop = ' + IntToStr (lua_gettop(L)) );
  end
 else
  begin
   lua_getglobal (L, t);
   if not lua_istable(L, -1) then
      begin
       lua_createtable (L, 0, 0);
       lua_setglobal (L, t);
       lua_getglobal (L, t); // она все ещё нужна
      end;

   lua_pushwstr (L, name);
   lua_pushcfunction(L, f);
   lua_settable (L, -3);    // таблица в стеке.
   lua_pop (L, -1);
   if name = 'foo2500' then
      ODS('[~T]. #DBG: Registered function ~C0A' + t + '.' + name + '~C07, lua_gettop = ' + IntToStr (lua_gettop(L)) );
  end;


end;

function luaL_dostring(L: lua_state; s: PAnsiChar): lua_Result;
begin
 luaL_loadstring(L, s);
 result := lua_pcall(L, 0, LUA_MULTRET, 0);
 if result <> 0 then
   ODS(' lua_pcall returned error #' + IntToStr(result) );
end;


procedure lua_pop(L: lua_State; n: Integer);
begin
  lua_settop(L, -(n) - 1);
end;



procedure lua_pushraw (L: lua_State; const data: lua_TValue);
var
   ptop: StkId;
begin
 ptop := lua_PStateRec (L).top;
 lua_pushlightuserdata (L, data.value.p);
 ptop.value.h := data.value.h;
 ptop.tt := data.tt;
 if L = nil then exit; // for debug
end;

function  lua_toraw (L: lua_State; idx: Integer): lua_TValue;
var
   bvl, tvl: lua_PStack;
begin
 result.value.p := nil;
 result.value.h := Ptr ($FFFFFFFF);
 result.tt := LUA_TNIL;
 result.rsv := 0;

 bvl := lua_PStack ( lua_PStateRec (L).base );
 tvl := lua_PStack ( lua_PStateRec (L).top );


 if (idx > 0) and (idx <= lua_gettop(L)) then
    result := bvl [idx - 1]
 else
 if idx > LUA_REGISTRYINDEX then
    result := tvl [idx]
 else
    case idx of
     LUA_GLOBALSINDEX: result :=  lua_PStateRec (L).l_gt;
    end;

 result.rsv := 0;

 case result.tt of
  LUA_TNIL: FillChar (result, sizeof(result), 0);
  LUA_TBOOLEAN,
  LUA_TLIGHTUSERDATA,
  LUA_TSTRING:
                begin
                 result.rsv := 0;
                 result.value.h := nil;
                end;

 end;
end; // lua_toraw



initialization
finalization

end.

