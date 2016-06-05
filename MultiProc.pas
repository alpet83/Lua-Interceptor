unit MultiProc;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, Math, LCGlobals, VxTools, WThreads, LuaTypes, XrayLua,
     ArrayTypes, IPCUtils, LuaTools,  Messages;

type
   TIPCThread = class (TWorkerThread)
   protected
    vx_processed: Integer;

    function                   ProcessRequestEx (prs: PRequestSlot): Integer; override;

   public
    { C & D }
    constructor                Create (CreateSuspended: Boolean; const sName: String; bWindowed: Boolean = FALSE );
   end;


var
     gIPCData: TFileMapping;
    gIPCQueue: TIPCAsyncQueue = nil;
   gIPCThread: TIPCThread = nil;

function GetSharedBuff: Pointer;
function Push_IPCQueue (L: lua_State): Integer; cdecl;
function Push_IPCThread (L: lua_State): Integer; cdecl;

function FindNearestVxAsync(rq: PVxRequest; test: Boolean = FALSE): TVxRequest;



implementation
uses AnsiStrings;

const
    TF_64BIT    = $100;
    TF_INT      = $200;
    LUA_TDOUBLE = LUA_TNUMBER or TF_64BIT;
    LUA_TLONG   = LUA_TNUMBER or TF_INT;
    LUA_TINT64  = LUA_TNUMBER or TF_INT or TF_64BIT;

var
       data_refs: array [0..15] of Pointer;
     shared_buff: Pointer = nil;


function GetSharedBuff: Pointer;
begin
 result := shared_buff;
 if result <> nil then exit;
 gIPCData.Init ( 1024 * 1024 );
 if not gIPCData.Open ('LUAICP_DATA') then
        gIPCData.Create ('LUAICP_DATA', TRUE);
 shared_buff := gIPCData.MapView();
 result := shared_buff;
end;


function FindNearestVxAsync(rq: PVxRequest; test: Boolean = FALSE): TVxRequest;
var
   rr: PVxRequest;
   pe: PAQEvents;

begin
   rr := gIPCQueue.PushDataRqs ('NEAREST_VERTEX', rq^, sizeof(TVxRequest) );
   pe := gIPCQueue.GetEvents; Assert ( rr <> nil, 'Cannot push data request' );
   while ( rr.tick_timeout > 0 ) do
           WaitForSingleObject (pe.FreeEvent, 100);

   result := rr^;

   gIPCQueue.ReleaseData (rr);
end;


function lua_toarray (L: lua_State; t_idx: Integer; pDest: Pointer; tflags: DWORD; max_items: Integer = 65535 ): Integer;
var
   t, i, step: Integer;
          pdv: PDouble;
          pfv: PSingle absolute pdv;
          piv: PInt64  absolute pdv;
          plv: PInteger absolute pdv;
          ppv: PPointer absolute pdv;

   procedure AutoInt;
   begin
    if step = 4 then
       plv^ := LuaDWORD (L, -1)
    else
       piv^ := lua_toint64 (L, -1);
   end;

   procedure AutoFloat;
   begin
    if step = 4 then
       pfv^ := lua_tonumber (L, -1)
    else
       pdv^ := lua_tonumber (L, -1);
   end;


begin
 t := lua_gettop (L);
 step := 4;
 if tflags and TF_64BIT <> 0 then step := 8;
 result := 0;
 for i := 1 to max_items do
  begin
   pdv := pdest;
   lua_pushinteger (L, i);
   lua_gettable (L, t_idx);
   if lua_isnil (L, -1) then
      break;

   case tflags and $F of
    LUA_TNUMBER:
       if tflags and TF_INT <> 0 then AutoInt else AutoFloat;
    LUA_TLUDATA:
       ppv^ := Ptr ( LuaDWORD (L, -1) );
    LUA_TUSERDATA:
       ppv^ := lua_objptr (L, -1);
   end; // case

   Inc (NativeUInt(pDest), step);
   Inc (result);
  end;

 lua_settop (L, t);
end; // lua_toarray


procedure FillRequest (L: lua_State; i_first: Integer; prs: PRequestSlot);
var
  argc: Integer;
  icnt: Integer;
  fcnt: Integer;
  pcnt: Integer;
     a: String;
     i: Integer;
begin
 argc := lua_gettop (L);
 i := i_first;
 fcnt := 0;
 icnt := 0;
 pcnt := 0;
 while (i < argc - 1) do
  begin
   a := LuaStrArg (L, i) + '?';
   case a [1] of
    'd', 'f': begin
          prs.f_params [fcnt and 3] := lua_tonumber (L, i + 1);
          Inc (fcnt);
         end;

    'i', 'u':
         begin
          prs.i_params [icnt and 3] := lua_toint64 (L, i + 1);
          Inc (icnt);
         end;
    'o': begin
          prs.p_params [pcnt and 3] := lua_objptr (L, i + 1);
          Inc (pcnt);
         end;
    'p': begin
          prs.p_params [pcnt and 3] := lua_topointer (L, i + 1);
          Inc (pcnt);
         end;

   end; // case type tag


   Inc (i, 2);
  end;

end; // FillRequest

function RQS_Index (L: lua_State): Integer; cdecl;
var
   prs: PRequestSlot;
     k: String;
     i: Integer;
begin
 result := 1;
 prs := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);
 if k = 'rqs' then
    lua_pushwstr(L, prs.rqs)
 else
 if k = 'obj_ptr' then
    lua_pushptr(L, prs.obj)
 else
 if Pos('_params', k) = 2 then
  begin
   // retrieve all *_params as arrays
   lua_createtable (L, 4, 0);
   for i := 0 to 3 do
    case k[1] of
     'i': lua_setarr_i ( L, i + 1, prs.i_params[i] );
     'f': lua_setarr_f ( L, i + 1, prs.f_params[i] );
     'p': lua_setarr_p ( L, i + 1, prs.p_params[i] );
    end; // for
  end
 else
    lua_pushnil (L);
end;

function RQS_NewIndex (L: lua_State): Integer; cdecl;
var
   prs: PRequestSlot;
     k: String;
begin
 result := 0;
 prs := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);

 if lua_type (L, 3) = LUA_TTABLE then
  begin
   if k = 'f_params' then
      lua_toarray (L, 3, @prs.f_params, LUA_TDOUBLE, 4) else
   if k = 'i_params' then
      lua_toarray (L, 3, @prs.i_params, LUA_TINT64, 4) else
   if k = 'o_params' then
      lua_toarray (L, 3, @prs.p_params, LUA_TUSERDATA, 4) else
   if k = 'p_params' then
      lua_toarray (L, 3, @prs.p_params, LUA_TLUDATA, 4);
  end else
 if k = 'rqs' then
    SetStrWZ ( @prs.rqs, LuaStrArg (L, 3), 23 ) else
 if k = 'obj' then
    prs.obj := lua_objptr (L, 3) else
 if k = 'obj_ptr' then
    prs.obj := Ptr( LuaDWORD ( L, 3 ) );
end;

procedure Push_RequestSlot (L: lua_State; prs: PRequestSlot);
begin
 AssignMetaIndex ( L, prs, RQS_Index, RQS_NewIndex, 'gmt_RequestSlot' );
end;

// ================================ IPC Thread lua wrappers ======================= //
function IPCThread_AddRqs (L: lua_State): Integer; cdecl;
var
   prs: PRequestSlot;
     t: TIPCThread;
     r: String;
begin
 result := 0;
 t := lua_objptr (L, 1);
 if t = nil then exit;
 r := LuaStrArg (L, 2);
 prs := t.AddRequestEx (r);
 FillRequest (L, 3, prs);
 t.SendRequest (prs); // TODO: check is need?
end;


function IPCThread_Index (L: lua_State): Integer; cdecl;
var
   t: TIPCThread;
   k: String;
begin
 result := 1;
 t := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);
 if k = 'add_rqs' then
    lua_pushcfunction (L, IPCThread_AddRqs)
 else

 if k = 'rq_count' then
    lua_pushinteger (L, t.RQCount)
 else
    lua_pushnil (L);
end;


function Push_IPCThread (L: lua_State): Integer; cdecl;
begin
 AssignMetaIndex ( L, gIPCThread, IPCThread_Index, nil, 'gmt_IPCThread_Index');
 result := 1;
end;

// ------------------- IPC Queue lua wrappers ----------------- //

function IPCQueue_AddRqs (L: lua_State): Integer; cdecl;
var
   prs: PRequestSlot;
     r: String;
     q: TIPCAsyncQueue;
begin
 result := 1;
 q := lua_objptr (L, 1);

 prs := nil;

 if q <> nil then
    prs := q.AllocSlot;

 if prs <> nil then
  begin
   r := LuaStrArg (L, 2);
   prs.SetRqs (r);
   FillRequest (L, 3, prs);
   Push_RequestSlot (L, prs);
  end
 else
  lua_pushnil (L);
end;

function IPCQueue_FlushRqs (L: lua_State): Integer; cdecl;
var
   prs: PRequestSlot;
     q: TIPCAsyncQueue;
begin
 result := 0;
 q := lua_objptr (L, 1);
 prs := lua_objptr (L, 2);
 if q <> nil then
    q.Flush (prs);
end;


function IPCQueue_Index (L: lua_State): Integer; cdecl;
var
   q: TIPCAsyncQueue;
   k: String;
begin
 result := 1;
 q := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);
 if k = 'add_rqs' then
    lua_pushcfunction (L, IPCQueue_AddRqs)
 else
 if k = 'flush_rqs' then
    lua_pushcfunction (L, IPCQueue_FlushRqs)
 else
 if k = 'flushed' then
    lua_pushinteger (L, q.Flushed)
 else
    lua_pushnil (L);
end;


function Push_IPCQueue (L: lua_State): Integer; cdecl;
begin
 AssignMetaIndex ( L, gIPCQueue, IPCQueue_Index, nil, 'gmt_IPCQueue_Index');
 result := 1;
end;


{ TIPCThread }

constructor TIPCThread.Create (CreateSuspended: Boolean; const sName: String; bWindowed: Boolean);
var
   lpName: array [0..MAX_PATH] of Char;
     hLib: HMODULE;
     func: PDWORD;
     i, n: Integer;
begin
 MakeIPCQueue ('WORK_IPC');
 inherited Create (CreateSuspended, sName, bWindowed);
 GetModuleFileName (0, lpName, MAX_PATH);
 hLib := GetModuleHandle (lpName);
 if hLib = 0 then exit;
 func := GetProcAddress (hLib, 'IsLauncher');
 if func = nil then  exit;
 for n := 0 to 15 do
   if func^ <> $FFAADDEE then
      Inc ( NativeUInt(func) )
   else
     for i := 0 to High (data_refs) do
      begin
       data_refs [i] := Ptr (func^);
       Inc ( func );
      end;

end;

function TIPCThread.ProcessRequestEx;

var
  rqs: String;
   rr: PVxRequest;
   rt: PDWORDArray;
    p: Pointer;
begin
 result := inherited ProcessRequestEx (prs);

 rqs := prs.rqs;

 if rqs = 'TEST_IPC' then
   begin
    ODS('[~T]. #DBG: TEST_IPC signal received. Connection established!');
    prs.i_params [0] := GetCurrentProcessId;
   end;
 // else wprintf('[~T].~C09 #IPC: received request %s', [rqs]);

 if ( rqs = 'ATTACHE_ME' ) and Assigned ( prs.p_params [0] ) then
   begin
    game_pid :=  PDWORD(prs.p_params[0])^;
    wprintf('[~T]. #DBG: attaching process %d', [game_pid]);
   end;


 rr := prs.p_params [0];
 if rqs = 'PING' then
   begin
    ODS('[~T]. #PING: received by ~C0A' + ThreadName + '~C07');
   end;

 if rqs = 'LOAD_VERTICES' then
   begin
    FindLoadVertices (rr);

    if current_lvl <> nil then
      with rr^, current_lvl^ do
       begin
        // vx_db.AddObject (fn, Pointer(current_lvl) );
        vectors[0].init ( min_x, 0, min_z );
        vectors[1].init ( max_x, 0, max_z );
        i_params [0] := current_lvl.last_lv;
        i_params [1] := current_lvl.count;
       end
      else
       SetStrZ ( rr.rq_error, 'LoadVertices failed async', 32 );

    rr.timeout := 0;

   end; // LoadVertices

 if rqs = 'NEAREST_VERTEX' then
   begin
    if AnsiStrings.StrLen ( rr.src_file ) > 0 then
       if FindLoadVertices ( rr ) <= 0 then
          exit;

    FindNearestVertex (rr);
    rr.rq_error [0] := #0;
    rr.timeout := 0;
    rr.i_params [1] := 200; // OK
    Inc (vx_processed);

    if vx_processed mod 1000 = 0 then
       ODS( CFormat('[~T/~B]. #PERF: IPCThread processed %7d vertices ', '~C07', [vx_processed] ) );

   end;
 if 'SIG_ALERT' = rqs then
   begin
    signal_disp := TRUE;
    Windows.PostMessage (0, WM_ACTIVATEAPP, 1, GetCurrentThreadId());
    MessageBox(0, PChar('Получен сигнал тревоги от игры: ' + PChar(prs.p_params[0])), 'Внимание!',
                                MB_OK or MB_ICONEXCLAMATION or MB_SYSTEMMODAL or MB_SETFOREGROUND);
    signal_disp := FALSE;
   end;

 if 'LOAD_DATA' = rqs then
   begin
    rt := prs.p_params[0];
    if (rt = nil) then
       begin
        PrintError(' p_params[0] = nil ' );
        exit;
       end;
    p := data_refs [ rt[0] ];
    if (p = nil) or (shared_buff = nil) then
       begin
        PrintError( Format (' p = %p, shared_buff = %p ', [p, shared_buff]) );
        exit;
       end;
    rt [2] := DWORD(p);
    CopyMemory (GetSharedBuff, p, rt[1]);
    rt [1] := 0;
   end;
end;

initialization

finalization

end.
