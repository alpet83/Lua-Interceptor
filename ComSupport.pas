unit COMSupport;

interface

uses Windows, SysUtils, Classes, ActiveX, ComObj, ComServ, luaicp_TLB, Misc, LuaTypes, XrayLua, LuaTools, LCGlobals;

{$R luaicp.tlb}

type
     TLuaScript = class (TAutoObject, ILuaScript)
     private

       FState: lua_State;

     protected
      function  CallFunc        (const bstrFuncName, bstrArgument: WideString): WideString; safecall;
      function  LoadScript      (const bstrFileName: WideString): WideString; safecall;
      function  Get_State: Integer; safecall;


     public

      { C & D }

      destructor  Destroy; override;
      { methods }

      procedure  Initialize; override;

     end;


procedure RegClasses;

function  DllCanUnloadNow: HResult;


implementation
uses ModuleMgr;



function  DllCanUnloadNow: HResult;
var
   s: String;
begin
 // disabled only in apache
 s := LowerCase(ExeFileName);
 wprintf('[~T]. #DBG(DllCanUnloadNow): resident in %s', [s]);
 if Pos('httpd.exe', s) = 0 then
   begin
    result := S_OK;
    FinalizeModule('~ALL');
   end
 else
    result := S_FALSE;
end;

{ TLuaScript }

destructor TLuaScript.Destroy;
begin
 if FState <> nil then
    lua_close (FState);
  inherited;
end;

function TLuaScript.CallFunc(const bstrFuncName, bstrArgument: WideString): WideString;
var
   err, pf, top: Integer;
begin
 top := lua_gettop (FState);
 try
   pf := 0;
   if lua_getglobal (FState, 'AtPanicHandler') = LUA_TFUNCTION then pf := lua_gettop(FState);

   if lua_getglobal (FState, bstrFuncName) = LUA_TFUNCTION then
     try
      lua_pushwstr (FState, bstrArgument);
      err := lua_pcall (FState, 1, LUA_MULTRET, pf);
      if err = 0 then
         result := LuaStrArg (FState, -1)
      else
         result := '#ERROR: lua_pcall returned error ' + IntToStr(err) + ': ' + LuaStrArg(FState, -1);

     except
      on E: Exception do
         OnExceptLog ('TLuaScript.CallFunc', E, TRUE);
     end
   else
     result := '#ERROR: function ' + bstrFuncName + ' is not in _G namespace';

 finally
  lua_settop (FState, top);
 end;
end;

function TLuaScript.LoadScript;
var
   pf, err, top: Integer;
          fname: String;

begin
 fname := FmtStalkerPath (bstrFileName);
 wprintf('[~T]. #DBG: TLuaScript.LoadScript ("%s") ', [fname]);
 if not FileExists(fname) then
    begin
     result := '#ERROR: not exists ' + fname;
     exit;
    end;

 top := lua_gettop (FState);
 try
  pf := 0;
  try
    if lua_getglobal (FState, 'AtPanicHandler') = LUA_TFUNCTION then pf := lua_gettop(FState);

    err := luaL_loadfile(FState, PAnsiChar ( AnsiString (fname) ) );


    if (err = 0) then
      begin
       err := lua_pcall (FState, 0, LUA_MULTRET, pf);
       if err = 0 then
          result := '#OK'
       else
          result := '#ERROR: lua_pcall returned error ' + IntToStr(err) + ': ' + LuaStrArg(FState, -1);
      end
    else
       result := '#ERROR: Problem in ' + bstrFileName + ', luaL_load returned error ' +
                   IntToStr(err) + ': ' + LuaStrArg (FState, -1);
   except
    on E: Exception do
      OnExceptLog ('TLuaScript.LoadScript', E, FALSE);
   end;
 finally
  lua_settop (FState, top);
 end;
end;

function TLuaScript.Get_State: Integer;
begin
 result := Integer (FState);
end;

procedure TLuaScript.Initialize;
var
    rs: function (L: lua_State): Integer; cdecl;
     L: lua_State;



begin
 inherited Initialize;
 L := luaL_newstate;
 wprintf('[~T]. #DBG: TLuaScript.Initialize FState = $%p', [L]);
  try
    luaL_openlibs (L);
    luaopen_debug (L);
    rs := GetProcAddress (Hinstance, 'RegDLLStuff');
    if Assigned (rs) then  rs (L);

  except
   on E: Exception do
      OnExceptLog ('TLuaScript.Initialize', E);
  end;
 FState := L;
end;

procedure DebugTLB;
var
   buff: array [0..260] of WideChar;
  fname: String;

begin
 GetModuleFileNameW (0, @buff, 260);
 fname := LowerCase(buff);
 if Pos('php.exe', fname) +  Pos('regsvr32.exe', fname) > 0 then
   begin
    StartLogging('');
    ShowConsole();
    wprintf ('[~T]. #DBG: %s detected as host. DLL path: %s', [fname, DllPath]);
    Windows.Beep(500, 500);
    // ReadLn;
   end;
end;

procedure RegClasses;
begin
 try
  CoInitializeEx(nil, CoInitFlags or COINIT_SPEED_OVER_MEMORY or COINIT_MULTITHREADED);
  TAutoObjectFactory.Create(ComServer, TLuaScript, Class_LuaScript, ciMultiInstance, tmApartment);
 except
  on E: Exception do
    OnExceptLog('RegClasses', E);
 end;
end;

initialization
  DebugTLB;
  RegClasses;
finalization

end.
