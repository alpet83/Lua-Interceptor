unit XrayRegistry;

interface

uses
  Windows, SysUtils, AnsiStrings, StrUtils, Classes, ArrayTypes, Math, ContNrs, IniFiles, StrClasses, misc, LuaTypes, LuaTools, XrayLua, LuaHelp, LCGlobals,
  UniArray, DateTimeTools, WThreads, WinHeap, Algs, XrayImports, VxTools, MultiProc;

{ Модуль реестра объектов xray, для сведения информации, оперативного доступа, слежения за состоянием (генерацией событий).
}
{$D+}

{$I stkdef.inc}
{$IFDEF DEBUG}

{$ENDIF}
{$IFDEF NLC_GOLD}
 {$O+}
{$ENDIF}


const
   WRONG_IDX = $FFFF;
   EVT_MOVE = $10;
{$IFDEF NEWEST_BUILD}
   SE_CAST_OFFSET       = $58;
{$ELSE}
   SE_CAST_OFFSET       = $60;
{$ENDIF}

   EVT_OBJECT_ADD              = $0001; // создан объект
   EVT_OBJECT_ACTIVATE         = $0002; // обновляемым стал
   EVT_OBJECT_SLEEP            = $0003; // из обновляемых вышел
   EVT_OBJECT_REMOVE           = $0004; // вышел из списков
   EVT_OBJECT_PARENT           = $0005; // изменился владелец объекта
   EVT_OBJECT_REJECT           = $0006; // DROP или что-то в таком духе
   EVT_OBJECT_SPAWN            = $0007; // создан клиентский объект
   EVT_OBJECT_DESTROY          = $0008; // удален объект
   EVT_OBJECT_SERVER           = $1000; // серверный
   EVT_OBJECT_CLIENT           = $2000; // клиентский





type

   TLuaObject = pointer; // объект Lua-bind

   TIndexList64K = packed array [WORD] of WORD;
   TGameIndex = TIndexList64K;


   TSharedStr = packed record
    rfc: DWORD;  // ref-counter
    len: DWORD;
    crc: DWORD;
    txt: array [0..65535] of AnsiChar;
   end;

   PSharedStr = ^TSharedStr;

   ERegistryException = class (Exception)
   end;

   TXrVector = packed record // размер элемента по обыкновению может быть любым, но чаще всего это указатель
    function length(item_size: NativeUInt = 4): NativeUInt;
    case BYTE of
     0:(
    start: Pointer;
     last: Pointer;
     _end: Pointer);
     1:(
    saddr: NativeUInt;
    laddr: NativeUInt;
    eaddr: NativeUInt;)

   end;

   PXrVector = ^TXrVector;


   PSGraphVertex  = ^TSGraphVertex;
   PServerEntityWrapper = ^TServerEntityWrapper;

   // CSGraphVertex
   TSGraphVertex = packed record
    spawn_id: WORD;
     content: array [2..17] of BYTE;
     function  GetWrapper(ofs: DWORD): PServerEntityWrapper; inline;
   end;


   // CServerEntityWrapper
   TServerEntityWrapper = packed record
    e_reader: Pointer; // IReader
    e_writer: Pointer; // IWrite
    m_entity: PByteArray; // prototype
   end;


   TSpawnPos = packed object (T3DVector4)
    gvid, lvid: DWORD;
   end;

   PSpawnPos = ^TSpawnPos;


   TEventInfo = class
   public
    vlst: array [0..7] of Single;
    v3d: T3DVector4;
    ctm: TDateTime; // create time
    src_id: WORD;



    constructor Create (id: WORD; pv: P3DVector4 = nil; v0: Single = 0);
   end;



   PByteBool = ^ByteBool;

   TObjectRegistry = class;
   TRootRegistry = class;

   TVertexPosMap = class
   protected
   public
    data: array of T3DVector4;

    { C & D }
    constructor         Create;
    destructor          Destroy; override;

    { methods }

    procedure    Add (i: Integer; const v: T3DVector4);


   end;


   TGameObject = class
   private
      FChilds: TObjectRegistry;
       FOwner: TRootRegistry;
      FParent: TGameObject;
     FHistory: TStrings;
     FUpdated: Boolean;
     last_pid: WORD;
    FServInst: PByteArray;
    FInstance: PByteArray;
    FObjectName: String;


    procedure CheckChilds;
    procedure LoadClient (bForce: Boolean);
    procedure LoadServer (bForce: Boolean);
    procedure TestUpdateParent (pid: WORD; const ctx: String);
    procedure OnUpdate;
    procedure SetUpdated (const Value: Boolean);
    procedure SetParent(const Value: TGameObject);
    procedure SetInstance(const Value: PByteArray);
    procedure SetServInst(const Value: PByteArray);
    procedure SetObjectName(const Value: String);


   protected

     obj_timers: array [0..7] of TDateTime;

     scan_frame: Integer;
     found_time: TDateTime;
    sc_registry: TObjectRegistry; // section registry
       warn_cnt: Integer;
     client_ref: PPointer;

    function cl_field_w (ofs: Integer): WORD; inline;
    function cl_field_i (ofs: Integer): Integer; inline;


    function se_field_w (ofs: Integer): WORD; inline;
    function se_field_i (ofs: Integer): Integer; inline;




   public
            id: WORD;
      spawn_id: WORD;
        active: Boolean;
      added_by: Integer;
         tflag: WORD;
          cost: Integer;
    inv_weight: Single;
      bad_mark: Integer;
        stable: Integer;

          used: Boolean;      // задействован в реестре, или свободен
     scan_loop: Integer;      // цикл обновления корневого реестра
     upd_loops: Integer;      // кол-во циклов обновления самого объекта

      is_added: Boolean;
      is_alive: Boolean;
     is_exists: Boolean;
     is_mapped: Boolean;
     is_mobile: Boolean;
    _is_online: PByteBool; // from server object field
    gvid, lvid: Integer;
     avg_speed: Single;

      direction,
    cl_position: T3DVector4;  // переменные используются для детектирования смещения (!!!)
    se_position: T3DVector4;
   ang_h, ang_v: Single;


        section: String;    // спавн-секция
         visual: String;

     event_func: String; // обработчик событий
      event_obj: TLuaObject;     // typical == user data
      event_set: String;
      err_count: Integer;

     se_destroy: Boolean;  // флаг для вызова колбека, на уничтожение объекта "совсем"

    { props }


    property    Childs: TObjectRegistry read FChilds; // содержимое инвенторя непися или тайника

    property    Owner: TRootRegistry read FOwner;       // главный регистр обычно
    property    Parent: TGameObject read FParent write SetParent;
    property    Updated: Boolean read FUpdated write SetUpdated;

    property    Name: String read FObjectName write SetObjectName;
    property    pInstance: PByteArray read FInstance write SetInstance; // экземпляр в памяти игры CGameObject*
    property    pServInst: PByteArray read FServInst write SetServInst; // указатель на серверный объект CSE_ALifeDynamicObject


    { C & D }
    constructor Create;
    destructor  Destroy; override;

    { methods }


    procedure   AddChild (gobj: TGameObject);
    procedure   Assign ( gobj: TGameObject );
    function    Dump: String; // debug info format

    procedure   RemoveChild (gobj: TGameObject);

    function    class_id: WORD; inline;

    function    game_vertex_id: DWORD; // inline;

    function    History: TStrings;

    function    is_online: Boolean; inline;


    procedure   Release (const AReason: String);
    procedure   Reset (const AReason: String);
    procedure   TimerStart (nt: Integer);
    function    TimerElapsed (nt: Integer): Double;

    function    obj_flags: DWORD; inline;
    function    parent_id: WORD;
    function    story_id: WORD;   inline;


    procedure   LoadState (bForce: Boolean; flags: BYTE = $FF ); virtual;

    function    LoadSharedStr (pba: PByteArray; ofs: DWORD; is_ptr: Boolean): String;

    procedure   Log (const msg: String);

    function    MemUsage: DWORD;

    function    Position: P3DVector4;

    function    VerifyID ( bClient: Boolean ): Boolean;

    function    VerifyPtrs ( se_ptr, cl_ptr: Pointer ): Boolean;

    procedure   UpdateSpawnID ( spid: WORD );

    // LowLevel
    function    cl_addr (ofs: Integer): Pointer; inline;
    function    se_addr (ofs: Integer): Pointer; inline;


   end; // TGameObject


   TRQFilter = class
   public
    what: String;
    sp_count, ip_count, fp_count: Integer;
    s_params: array [0..7] of String;
    i_params: array [0..7] of Int64;
    f_params: array [0..7] of Double;
   end;


   TGameObjectEvent = packed record
    msg: DWORD; // event code
     id: DWORD; // id of object
     cl: Pointer; // client object pointer
     se: Pointer; // server object pointer
    res: array [0..15] of DWORD; // reserved
   end;


   TObjectRegistry = class (TObjectList)
   private
            FCache: TObjectList;
           FIsRoot: Boolean;
            FOwner: TObjectRegistry;
     FCountUpdated: Boolean;
      FDesignation: String;
        FDispTimer: TProfileTimer;
        FFreezeMax: Double;

    function   GetGameObject(index: Integer): TGameObject; inline;
    function   GetDebugContext: String; inline;
    procedure  SetDebugContext(const Value: String);
   protected

     added_count: Integer;
      chkd_count: Integer;
       heavy_ops: Integer;
     upd_obj_cnt: Integer;
    FDbg_context: array [0..255] of CHAR;
     FLastSorted: Boolean;

   public



    { props }


    property    CountUpdated: Boolean read FCountUpdated write FCountUpdated;
    property    dbg_context: String   read GetDebugContext write SetDebugContext;
    property    FreezeMax: Double read FFreezeMax write FFreezeMax;

    property    IsRoot: Boolean read FIsRoot;
    property    Objects[index: Integer]: TGameObject read GetGameObject; default;
    property    Owner: TObjectRegistry read FOwner;
    property    Designation: String read FDesignation;

    { C & D }
    constructor Create (const ADesignation: String);
    destructor  Destroy; override;

    { Methods }


    function    Add ( gobj: TGameObject ): Integer; virtual;

    procedure   AssignParents;

    procedure   Clear; override;

    procedure   Dump ( flags: DWORD );

    function    Find (id: WORD): TGameObject; virtual;
    function    FindPosition ( id: WORD; pLow, pHigh: PInteger ): Integer; virtual;
    procedure   LoadAll (bForce: Boolean);



    function    MemUsage: DWORD; virtual;

    procedure   Remove ( gobj: TGameObject ); virtual;
    function    StoreIDs ( L: lua_State; flt: Integer ): Integer;
    procedure   StoreObjects ( dst: TObjectRegistry );
   end; // TObjectRegistry

   TScanCallback = function (id: WORD; obj: Pointer): Boolean of object;

   TRootRegistry = class (TObjectRegistry)

   private

       scan_idx: DWORD;   // индекс сканирования 0 - клинтские объекты, 1 - серверные, 2 - спавн
      scan_loop: Integer;
     scan_frame: Integer;
      scan_time: TDateTime;
     scan_found: Integer;
    scan_active: Boolean;
        tmp_reg: TObjectRegistry;
       tmp_reg2: TObjectRegistry;

        FEvents: TStrMap;
        by_name: TStrMap;
      spawn_map: TStrMap;
      spawn_reg: TStringList;
      spawn_pos: TStrMap;
      FSections: TStringList;

      level_map: TStrMap;      // [level_id]       = level_name
    level_index: TGameIndex;   // [game_vertex_id] = level_id
     spawn_objs: array [WORD] of PByteArray;
       ok_count: Integer;

      spawn_index: record
       story_spid: TGameIndex; // [obj.story_id] = spawn_id
        game_spid: TGameIndex; // [obj.id]       = spawn_id
        spid_game: TGameIndex; // [spawn_id]     = obj.id
            names: TGameIndex;
      end;

        scan_cb: TScanCallback;
       FEnabled: Boolean;



    procedure CheckSections;



    function  RegCLObject (id: WORD; obj: Pointer): Boolean;
    function  RegSEObject (id: WORD; obj: Pointer): Boolean;
    function  RegEntity   (id: WORD; obj: Pointer): Boolean;
    function  UnregCLObject (id: WORD; obj: Pointer): Boolean;

    function  Traverse_Tree (node, head: PTreeMapNode): Boolean;
    procedure TestRemoved;
    function  GetMapped(const section: String): TObjectRegistry;
    function  NameIndex(const nm: String): Integer;
    function  SectionIndex(const sc: String): Integer;
    procedure UpdateMap (bFast: Boolean);
    procedure ScanVector(vector: PXrVector);
    procedure SetLevelInfo (const lname: String; level_id, gvid_min, gvid_max: WORD);
    function  SpawnEntityVerify (gobj: TGameObject): Boolean;

    procedure MapEntity(E: PByteArray; spawn_id, level_id: WORD);
    function  GetEntityName (spawn_id: WORD): String;
    function  LevelName(gvid: WORD): String;
    function FindSpawnID(const Name: String; gvid: WORD): WORD;
    procedure LoadSpawnPositions;
    procedure TestSpawnPos(name: String; E: PByteArray);

   protected
        FDirect: array [WORD] of TGameObject;
   //   FRemoved: array [WORD] of BYTE;          // счетчики блокировки регистрации для удаленных объектов

     ext_events: Integer;
     reg_events: Integer;
   destr_events: Integer;
 last_perf_dump: TSystemTime;
         upd_st: TSystemTime;
       upd_loop: Integer;
      exec_time: array [0..3] of Double;
       cpu_time: array [0..3] of Double;
   fast_updates: Integer;
   slow_updates: Integer;

      items_ref: PPointerArray;
      load_mask: WORD;


    function    MakeObject (id: WORD): TGameObject;

   public


    property    Events: TStrMap read FEvents;
    property    Enabled: Boolean read FEnabled write FEnabled;
    property    Sections: TStringList read FSections;
    property    Mapped[const section: String]: TObjectRegistry read GetMapped;


    { C & D }
    constructor Create (const ADesignation: String);
    destructor  Destroy; override;

    { methods }
    function    Add ( gobj: TGameObject ): Integer; override;
    procedure   Clear; override;
    procedure   DebugDump;
    function    Find (id: WORD): TGameObject; override;
    function    FindByName ( sName: String ): TGameObject;
    function    FindBySpawnID( spawn_id: WORD ): TGameObject;
    function    FindObjectByPtr (p: Pointer; back, fwd: DWORD ): String;

    function    GetFiltered ( lst: TStrMap; L: lua_State; tbidx: Integer ): TObjectRegistry; // !
    function    GetFull: TObjectRegistry;
    procedure   GetStoryIDMap (L: lua_State);
    function    IsFiltered ( gobj: TGameObject; flt: TRQFilter ): Boolean; // проверка на отсев по фильтру
    function    JoinSections (sc: String): TObjectRegistry; // !

    procedure   Remove ( gobj: TGameObject ); override;
    procedure   ResetSpawnInfo;

    procedure   MapObject ( gobj: TGameObject; bMap: Boolean = TRUE );


    procedure   MarkObjectBad (id: Integer);
    function    MemUsage: DWORD; override;

    function    ObjectName (id: WORD): String;
    procedure   ProcessEvents (L: lua_State);

    function    RegObject (id: WORD): TGameObject;
    procedure   RemoveBad;
    procedure   ScanObjects ( bFast: Boolean );
    procedure   SwitchParent (id, parent_id: WORD);
    procedure   Update ( bFast: Boolean );
   end; // TRootRegistry

  TBackgroundScanner = class (TWorkerThread)
  protected

    procedure  ProcessInit; override;
  public
    scan_loop: Integer;
    cpu_total: Double;


    procedure  WorkProc; override;
  end;

procedure CleanupDMA;
function  DetectPointers: Integer;
function  EntityName (e: PByteArray): String;
procedure FastObjectEvent(const goe: TGameObjectEvent); cdecl;
function  FindSpawnIDS (L: lua_State): Integer; cdecl;
function  InvokeRegistry (L: lua_State): Integer; cdecl;
function  ProfileRegistry (L: lua_State): Integer; cdecl;
function  UpdateRegistry (L: lua_State): Integer; cdecl;

function  RegistryFilter (L: lua_State): Integer; cdecl;
function  RootRegistry ( bCreateNotExist: Boolean = TRUE ): TRootRegistry; // access method
procedure FinalizeRegistry ( bBeforeExit: Boolean );

function  FindReleased (id: WORD): TGameObject;
procedure OnSpawn ( se, cl: PByteArray );

procedure OnObjectRelease (i: Integer; const context: String; exists_now: Boolean );
procedure TestObjectEvent (s: String);
function  TestObjectPtr(p: Pointer): String;

function  ReadWORD (p: Pointer; ofs: NativeInt): WORD; inline;
procedure VMUpdateRegion(const mbi: TMemoryBasicInformation);
function  VerifyPtr(p: Pointer; sz: DWORD = 4): Boolean;


var
      destr_log: TStrMap = nil;     // !
   active_level: Integer = -1;
   g_level_name: String  = '';
       bad_ptrs: DWORD = 0;


implementation
uses ModuleMgr;

const
   PAGE_RW = PAGE_READWRITE or PAGE_EXECUTE_READWRITE;

type
    TFakeObjectMaker = function ( id: WORD ): PByteArray;

    PFakeTreeContext = ^TFakeTreeContext;

    TFakeTreeContext = record
       last_id: DWORD;
     obj_maker: TFakeObjectMaker
    end;

    TBits32 = 0..31;
    TBitSet = packed set of TBits32;


var

   // ========== DMA vars group ==============
       destroy_queue: PXrVector = nil;
      objects_active: PXrVector = nil;
    objects_sleeping: PXrVector = nil;
      client_objects: PPointerArray = nil;
       alife_objects: PXrMap = nil;
        alife_spawns: PXrMap = nil;
        alife_st_ids: PXrMap = nil;
           map_NETID: PXrMap = nil;

   g_client_map_head: PTreeMapNode = nil;
   g_server_map_head: PTreeMapNode = nil;
   g_spawns_map_head: PTreeMapNode = nil;
   g_st_ids_map_head: PTreeMapNode = nil;
   // ========== DMA vars group ==============
          g_addr_max: UIntPtr = 0;
          g_addr_min: UIntPtr = 0;
          g_vmap_max: UIntPtr = 0;
          g_vmap_min: UIntPtr = 0;
          g_addr_map: array [0..1048576 div 32 - 1] of TBitSet;
          g_sys_info: TSystemInfo;
         g_page_bits: BYTE = 12;
           g_scanner: TBackgroundScanner = nil;

        invoke_count: DWORD = 0;
           art_clsid: WORD = 39;
           g_rootreg: TRootRegistry = nil;
              lreg_L: lua_State = nil;
               sltmp: TStrMap = nil;


function FieldPtr(p: Pointer; ofs: NativeInt): PVariantField; inline;
begin
 Inc (NativeInt(p), ofs);
 result := p;
end;

function dword_field(p: Pointer; ofs: NativeInt): DWORD; inline;
begin
 result := FieldPtr (p, ofs).u32;
end;

function int_field(p: Pointer; ofs: NativeInt): Integer; inline;
begin
 result := FieldPtr (p, ofs).i32;
end;

function word_field(p: Pointer; ofs: NativeInt): WORD; inline;
begin
 result := FieldPtr (p, ofs).u16;
end;



procedure CleanupDMA;
begin
 g_addr_max := 0;
 g_addr_min := 0;
 destroy_queue := nil;
 objects_active := nil;
 objects_sleeping := nil;
 alife_objects := nil;
 alife_spawns := nil;
 map_NETID := nil;
 g_rootreg := nil;
 g_client_map_head := nil;
 g_server_map_head := nil;
 g_spawns_map_head := nil;
 removed_list.Clear;
end;

function CheckPointersValid: Boolean;
begin
 result := Assigned (objects_active) and Assigned (objects_sleeping) and Assigned (alife_objects);
end;

function CheckXrVector(const name: String; p: PXrVector; max_size: Int64 = $FFFF * 2): PXrVector;
var
   diff: Int64;
begin
 result := nil;
 if (p.saddr or p.laddr or p.eaddr = 0) then exit; // not initialized now

 diff := p.laddr - p.saddr;
 if VerifyPtr (p.start) and VerifyPtr(p.last) and VerifyPtr(p._end) and
            (p.saddr <= p.laddr) and (p.laddr <= p.eaddr) and (diff <= max_size) then
    result := p
 else
    wprintf('[~T].~C0C #ERROR(CheckXrVector):~C07 invalid xr_vector "%s" @%p, start = $%p, last = $%p, end = $%p ',
             [name, p, p.start, p.last, p._end]);


end;

procedure SetXrPointer(const name: String; p: Pointer);
var
   target: PPointer;

   procedure set_target(var v; upd_p: Pointer);
   begin
    p := upd_p;
    target := @v;
    if ( upd_p <> nil ) then
       wprintf('[~T]. #DBG: SetXrPointer updating %s from $%p to $%p ', [name, target^, upd_p]);

   end;

begin
 try
  if not VerifyPtr(p, 8) then
     begin
      PrintError( Format('bad pointer %p, cannot assigned to %s', [p, name]) );
      exit;
     end;

 except
  on E: EAccessViolation do
     PrintError( Format('very bad pointer %p, cannot assigned to %s', [p, name]) );
 end;


 target := nil;

 if name = 'client_objects'   then set_target (client_objects, p) else
 if name = 'map_NETID'        then set_target (map_NETID, p) else
 if name = 'alife_objects'    then set_target (alife_objects, p) else
 if name = 'alife_spawns'     then set_target (alife_spawns, p) else
 if name = 'alife_st_ids'     then set_target (alife_st_ids, p) else
 // all heads must be updated before TRootRegistry.ScanObjects
 if name = 'spawn_map.head'   then set_target (g_spawns_map_head, p) else
 if name = 'st_ids.head'      then set_target (g_st_ids_map_head, p) else
 if name = 'client_map.head'  then set_target (g_client_map_head, p) else
 if name = 'server_map.head'  then set_target (g_server_map_head, p) else
 if name = 'destroy_queue'    then set_target (destroy_queue,  CheckXrVector(name, p)) else
 if name = 'objects_active'   then set_target (objects_active, CheckXrVector(name, p)) else
 if name = 'objects_sleeping' then set_target (objects_sleeping, CheckXrVector(name, p)) else
    wprintf(' ~C0C #ERROR~C07 unkown pointer %s ', [name]);

 if target <> nil then
    target^ := p;

end;

function _spawn_entity_index(L: lua_State): Integer; cdecl;
var
  pf: PSingle;
  pd: PDWORD;
  pw: PWORD;

   e: Pointer;
   k: String;
begin
 result := 1;
 e := lua_objptr (L, 1);
 k := LuaStrArg (L, 2);

 pf := nil;
 pd := nil;
 pw := nil;
 try
   with g_offsets do
   if Pos('dir_', k) = 1 then
      begin
       if k = 'dir_x' then
          pf := RelativePtr (e, se_offsets.direction + 0) else
       if k = 'dir_y' then
          pf := RelativePtr (e, se_offsets.direction + 4) else
       if k = 'dir_z' then
          pf := RelativePtr (e, se_offsets.direction + 8)
      end
   else
   if Pos('pos_', k) = 1 then
      begin
       if k = 'pos_x' then
          pf := RelativePtr (e, se_offsets.position + 0) else
       if k = 'pos_y' then
          pf := RelativePtr (e, se_offsets.position + 4) else
       if k = 'pos_z' then
          pf := RelativePtr (e, se_offsets.position + 8)
      end
   else
   if k = 'clsid' then
      pw := RelativePtr (e, se_offsets.clsid)        else
   if k = 'gvid' then
      pw := RelativePtr (e, se_offsets.gvx_id)       else
   if k = 'lvid' then
      pd := RelativePtr (e, se_offsets.lvx_id)       else
   if k = 'spawn_id' then
      pw := RelativePtr (e, se_offsets.spawn_id)     else
   if k = 'story_id' then
      pw := RelativePtr (e, se_offsets.story_id)     else
   if k = 'name' then
      begin
       lua_pushwstr (L, EntityName(e));
       exit;
      end else
      begin
       result := _PtrIndex (L);
       exit;
      end;

   if pd <> nil then
      lua_pushinteger (L, pd^) else
   if pf <> nil then
      lua_pushnumber (L, pf^) else
   if pw <> nil then
      lua_pushinteger (L, pw^) else
      lua_pushnil (L);
 except
  on E: Exception do
    OnExceptLog('_spawn_entity_index', E);

 end;
end;


function cmpGameObjectID (a, b: Pointer): Integer;
var
   goa, gob: TGameObject;
begin
 goa := a;
 gob := b;
 result := (goa.id - gob.id);
end;

function bscmpGameObjectID (iTest: Integer; pData, pValue: Pointer): Integer;
var
   pList: TPointerList;
    test: TGameObject;
      id: WORD;
begin
 pList := pData;
 id := WORD(pValue);
 test := TGameObject ( pList [iTest] );
 result := test.id - id
end;

function DetectPointers: Integer;
var
    alife: PByteArray;
    spwnr: PByteArray; // CALifeSpawnRegistry
    verts: PXrMap;
    stids: PXrMap;
     base: PByteArray;
     list: PXrMap;

      reg: PByteArray; // CALifeObjectRegistry

       pp: PPointer;
        p: Pointer;

begin
 result := 0;
 if not RootRegistry.Enabled then exit;

 if not g_offsets.IsCorrect (TRUE) then exit;

 if ( g_pGameLevel = nil ) then exit;

 with g_offsets do
  if ( level.objects_active > 0 ) and ( level.objects_sleeping > 0 ) then
     begin
      if ( log_verbose >= 1 ) and ( objects_active = nil ) then
         wprintf('[~T]. #DBG: DetectPointers, g_pGameLevel = %p, check offsets $%x, +%d, +%d',
                 [g_pGameLevel^, level.objects_list, level.objects_active, level.objects_sleeping] ); // }
      // p := g_pGameLevel^.calc_ptr (level.objects_list);
      // if map_NETID = nil then  ReadLn;
      // if p <> map_NETID         then SetXrPointer('map_NETID', p);
      p := g_pGameLevel^.calc_ptr (level.destroy_queue);
      if p <> destroy_queue    then SetXrPointer ('destroy_queue', p);
      p := g_pGameLevel^.calc_ptr (level.objects_active);
      if p <> objects_active   then SetXrPointer ('objects_active', p);
      p := g_pGameLevel^.calc_ptr (level.objects_sleeping);
      if p <> objects_sleeping then SetXrPointer ('objects_sleeping', p);

      if level.objects_list > 0 then
        begin
          pp := g_pGameLevel^.calc_ptr (level.client_objects);
          if pp^ <> client_objects then
             SetXrPointer ('client_objects', pp^);
        end;

     end;


 (*


class CALifeSimulatorBase : public IPureDestroyableObject {
protected:
-- 3312
	xrServer									      *m_server;                       00
	CALifeSimulatorHeader						*m_header;                       04
	CALifeTimeManager							  *m_time_manager;                 08
  ?                                                                0C
	CALifeSpawnRegistry							*m_spawns;                       10
	CALifeObjectRegistry						*m_objects;                      14
	CALifeScheduleRegistry					*m_scheduled;                    18
	CALifeStoryRegistry							*m_story_objects;                1C
	CALifeSmartTerrainRegistry			*m_smart_terrains;               20
	CALifeGroupRegistry							*m_groups;                       24
  CALifeRegistryContainer					*m_registry_container;           28


-- 5564
  ? vftable ??
	xrServer									      *m_server;                       00
	CALifeSimulatorHeader						*m_header;                       04
	CALifeTimeManager							  *m_time_manager;                 08
	CALifeSpawnRegistry							*m_spawns;                       0C
	CALifeObjectRegistry						*m_objects;                      10
	CALifeGraphRegistry							*m_graph_objects;
	CALifeScheduleRegistry					*m_scheduled;
	CALifeStoryRegistry							*m_story_objects;
	CALifeSmartTerrainRegistry			*m_smart_terrains;
	CALifeGroupRegistry							*m_groups;
	CALifeRegistryContainer					*m_registry_container;


 *)

 with g_offsets.se_offsets do
 if ( g_ai_space($18) <> nil ) and ( CALifeSimulatorBase > 0 ) then
    try
       reg := nil;
       list := nil;
       verts := nil;
       stids := nil;

       pp := g_ai_space($18);
       alife := pp^;  // CALife
       if alife = nil then exit;

       base := @alife[CALifeSimulatorBase]; // CALifeSimulatorBase at CALifeSimulator + offset
       if Assigned (base) then
        begin
         CopyMemory (@reg,   @base[CALifeObjectRegistry], sizeof(Pointer));
         list := @reg[CALifeObjectReg_list];
         CopyMemory (@spwnr, @base[CALifeSpawnRegistry], sizeof(Pointer));
         if CALifeSpawnReg.vertices > 0 then
            verts := @spwnr[CALifeSpawnReg.vertices]; // CALifeSpawnReg_spawns
         if CALifeSpawnReg.story_ids > 0 then
            stids := @spwnr[CALifeSpawnReg.story_ids];

        end;

       if ( log_verbose >= 1 ) and ( alife_objects = nil ) then
          wprintf('[~T]. g_ai_space[$18] = $%p, alife = $%p, sim_base = $%p, obj_reg = $%p, spawn_reg = $%p ',
                        [pp, alife, base, reg, spwnr] );

       if (list <> alife_objects) then
           SetXrPointer ('alife_objects', list);


       if (verts <> alife_spawns) then
           SetXrPointer ('alife_spawns', verts);

       if (stids <> alife_st_ids) then
           SetXrPointer ('alife_st_ids', stids);

     except
      on E: Exception do;
    end;



 // }

end;


function  FindReleased (id: WORD): TGameObject;
var
   test: TGameObject;
      i: Integer;
begin
 result := nil;
 if not Assigned (removed_list) then exit;

 for i := 0 to removed_list.Count - 1 do
     begin
      test := TGameObject ( removed_list.Objects [i] );
      if test.id <> id then continue;
      result := test;
      break;
     end; // for

end;


procedure SaveReleased ( id: WORD; obj: TGameObject; se_destr: Boolean );
var
    cp: TGameObject;
begin
 if (id = $FFFF) or (not Assigned (removed_list)) then exit;
 // if not se_rmv then exit;

 cp := FindReleased (id);
 if cp = nil then
   begin
    cp := TGameObject.Create;
    removed_list.AddObject ( InfoFmt('~T'), cp );
   end;


 cp.id := id;
 cp.FObjectName := 'disappeared';
 // cp.se_destroy := se_destr;
 cp.se_destroy := cp.se_destroy or se_destr;

 if obj <> nil then
    cp.Assign ( obj );
end;


procedure OnObjectRelease (i: Integer; const context: String; exists_now: Boolean );
var
  cl_destroy: Boolean;
  sv_destroy: Boolean;
     sid, st: String;
        gobj: TGameObject;

begin
 if (i < 0) or (i >= $FFFF) then exit;
 // or ( Pos('entity_Destroy', context) > 0 ) // вероятно удаление прототипа
 sv_destroy := ( Pos(':sv ', context) > 0 ) or ( Pos(':se object', context) > 0 ) or ( Pos('se destr', context) > 0 );
 cl_destroy := ( Pos(':cl ', context) > 0 );

 if not Assigned (g_rootreg) then exit;

   with g_rootreg do
    begin
     gobj := RootRegistry.Find(i);

     st := InfoFmt('~T;');
     sid := 'ID=' + IntToStr(i) + ';';
     Inc (g_rootreg.destr_events);

     if gobj <> nil then
       with gobj do
        begin
         if exists_now then
             LoadState (TRUE);

         if sv_destroy or cl_destroy then
            destr_log.Add ( st + Format('%-9s SE=$%p; CL=$%p; NM=%s; CTX=%s', [sid, pServInst, pInstance, name, context] ) );

         SaveReleased ( i, FDirect [i], sv_destroy );
         if Assigned (pInstance) and ( Pos('destroying', context) > 0 )  then
            wprintf('[~T].~C0C WARN:~C07 late object %-32s event %s , pInstance = $%p ', [Name, context, pInstance]);

         if sv_destroy then pServInst := nil;
         if cl_destroy then UnregCLObject(i, nil);

         if (pInstance = nil) and (pServInst = nil) then
           begin
            MarkObjectBad (i);
            Release (context);
           end;

        end
      else
        begin
         destr_log.Add ( st + sid + 'nil;nil;nil;' + context );
         SaveReleased ( i, nil, sv_destroy );
        end;


    end;
end;

procedure OnSpawn ( se, cl: PByteArray );
var
   p_id: PWORD;
   gobj: TGameObject;
begin
 if ( g_rootreg = nil ) or ( not  CheckPointersValid ) then exit;



 p_id := nil;

 with g_offsets do
 if IsCorrect then
  try
   if se <> nil then
      p_id := @se[se_offsets.id_word];
   if cl <> nil then
      p_id := @cl[id_word];

   if ( p_id <> nil ) then
       begin
        gobj := g_rootreg.RegObject ( p_id^ );
        if se <> nil then
           gobj.pServInst := se;
        if cl <> nil then
           gobj.pInstance := cl;

        gobj.LoadState (TRUE);
       end;

   if DetectPointers > 0 then
        g_rootreg.Update (FALSE);


  except
   on E: Exception do
      OnExceptLog('OnSpawn', E);
  end;
end;


function AngNormalize ( ang: Double ): Double;
begin
 result := ang;
 while result > Pi do
    result := result - 2 * Pi;

 while result < -Pi do
    result := result + 2 * Pi;
end;

function DistancePt (x1, y1, x2, y2: Double): Double; inline;
begin
 x1 := x1 - x2; // distance x
 y1 := y1 - y2; // distance y
 result := Sqrt ( x1 * x1 + y1 * y1 );
end;



function ExtractNum(s: String): Integer;
var
   c: Integer;
begin
 result := -1;
 s := Trim(s);
 c := Pos(']', s);
 if c <= 1 then exit;
 s := Copy (s, 1, c);
 result := atoi(s);
end;

function  FindSpawnIDS (L: lua_State): Integer; cdecl;
var
   fset: array [0..65534] of ByteBool;
     ne: Boolean;
     fs: Boolean;
     ss: String;
     sn: String;
     sv: String;
     ip: WORD;
      i: Integer;
      c: Integer;
      p: WORD;
begin
 result := 2;
 if g_rootreg = nil then
   begin
    lua_pushnil (L);
    lua_pushnil (L);
    exit;
   end;

 ss := '?';

 if lua_gettop(L) > 1 then
    ss := LuaStrArg (L, 2)
 else
   case lua_type(L, 1) of
     LUA_TNUMBER: ss := 'by_id';
     LUA_TSTRING: ss := 'by_name';
   end; // case


 // return object name & proto by id


 if lua_type(L, 1) = LUA_TNUMBER then
 with g_rootreg do
  begin
   ip := lua_tointeger (L, 1); // src as index
   p := WRONG_IDX; // index in spawn init

   if ( ss = 'by_id' ) or ( ss = 'by_spawn_id' ) then
      p := ip;

   if ss = 'by_story_id' then
      p := spawn_index.story_spid [ip];

   i := spawn_index.names [p]; // name index

   if ( i >= 0 ) and ( i < spawn_map.Count ) then
     begin
      ss := spawn_map[i];
      sn := StrTok (ss, [':']);
      if sn <> ss then
         __nop;
      lua_pushwstr ( L, sn );

      if p < WRONG_IDX then
         lua_pushptr ( L, spawn_objs[p] )
      else
         lua_pushnil ( L );
     end
   else
     begin
      PrintError( Format('FindSpawnIDS outbound: ss = %s,  ip = %d, i = %d, max_spawn_id = %d ',
                                [ss, ip, i, spawn_map.Count - 1]));
      lua_pushnil (L);
      lua_pushnil (L);
     end;

   exit;
  end;

 sn := LuaStrArg (L);

 p := Pos('*', sn);

 if ss = 'by_name' then
   with g_rootreg do
    // return all objects by substr
    if p > 0 then
      begin
       fs := lua_toboolean (L, 3);
       ne := lua_toboolean (L, 4);
       FillChar (fset, sizeof(fset), 0);
       result := 1;
       System.Delete (sn, p, 1);
       lua_createtable (L, 0, 0);
       c := 0;
       for i := 0 to spawn_map.Count - 1 do
        begin
         sv := spawn_map [i];
         if log_verbose >= 7 then
            wprintf(' testing "%s" in "%s" ', [sn, sv]);
         if ( sn = '' ) or ( Pos ( sn, sv ) > 0 ) then
           begin
            p := spawn_map.I32Tags [i] and $FFFF;
            if fset[p] or ( fs and ( FindBySpawnID(p) <> nil )) then continue;

            if ne and ( RootRegistry().FindByName(sv) <> nil ) then
              begin
               if IsDebuggerPresent then
                  wprintf('[~T]. #DBG: entity %s was exists - ignoring', [sv]);
               continue;
              end;

            fset[p] := TRUE;
            Inc (c);
            lua_pushinteger (L, c); // key
            lua_pushinteger (L, p); // value
            lua_settable (L, -3);
           end;
        end;
       // lua_pushnil (L);
      end
     else
      begin
       i := spawn_map.IndexOf (sn); // search in
       // single object info
       if i >= 0 then
         begin
          p := spawn_map.I32Tags [i] and $FFFF; // index in spawn
          lua_pushinteger (L, p);
          lua_pushptr (L, spawn_objs[p] )       // proto
         end
       else
         begin
          lua_pushinteger (L, -1);
          lua_pushnil (L);
         end;
      end;
end; // FindSpawnIDS

function  RegistryFilter (L: lua_State): Integer; cdecl;

var
   act: String;
   argc: Integer;
   list: TStringList;
   rflt: TRQFilter;
    idx: Integer;
    obj: TObject;
begin
 result := 1;
 argc := lua_gettop (L);
 act := UpperCase ( LuaStrArg (L) );

 if act = 'NEW_LIST' then
  begin
   list := TStringList.Create;
   list.OwnsObjects := TRUE;
   lua_pushptr (L, list);
   exit;
  end else

 if ( act = 'ADD' ) and ( argc >= 3 ) then
  begin
   list := lua_topointer (L, 2);
   rflt := lua_topointer (L, 3);
   list.AddObject ( rflt.what, rflt);
   lua_pushptr (L, list);
   exit;
  end else
 if ( act = 'NEW' ) and ( argc >= 2 ) then
  begin
   rflt := TRQFilter.Create;
   rflt.what := LuaStrArg (L, 2);
   if argc >= 3 then
    begin
     list := lua_topointer (L, 3);
     list.AddObject ( rflt.what, rflt);
    end;

   lua_pushptr (L, rflt);
   exit;
  end else
 if ( act = 'SETS' ) and ( argc >= 3 ) then
  begin
   rflt := lua_topointer (L, 2);
   rflt.sp_count := 0;
   for idx := 3 to argc do
      begin
       rflt.s_params [idx - 3] := LuaStrArg (L, idx);
       Inc (rflt.sp_count);
      end;

   lua_pushptr (L, rflt);
   exit;
  end else
 if ( act = 'SETN' ) and ( argc >= 3 ) then
  begin
   rflt := lua_topointer (L, 2);
   rflt.fp_count := 0;
   for idx := 3 to argc do
      begin
       rflt.f_params [idx - 3] := lua_tonumber (L, idx);
       Inc (rflt.fp_count);
      end;

   lua_pushptr (L, rflt);
   exit;
  end else
 if ( ( act = 'SETI' ) or ( act = 'SET_IPARAMS' ) ) and ( argc >= 3 ) then
  begin
   rflt := lua_topointer (L, 2);
   rflt.ip_count := 0;
   for idx := 3 to argc do
      begin
       rflt.i_params [rflt.ip_count] := lua_tointeger (L, idx);
       Inc (rflt.ip_count);
      end;
   lua_pushptr (L, rflt);
   exit;
  end else
 if ( act = 'FREE' ) and ( argc >= 2 ) then
  begin
   obj := lua_topointer (L, 2);
   obj.Free;
   lua_pushboolean (L, TRUE);
   exit;
  end;

 PrintError( 'RegistryFilter:  Unknown action ' + act );
 lua_pushwstr (L, 'unknown action ' + act);
end;   // RegistryFilter

procedure TestObjectEvent (s: String);
var
     i, p: Integer;
   sv_own: Boolean;

begin
 if g_rootreg = nil then exit;
 if sltmp = nil then
    sltmp := TStrMap.Create;


 sltmp.Split('[', s);

 if sltmp.Count < 2 then exit;

 s := UnhideSP (sltmp[0]);
 s := Trim (s);

 // ODS('[~T]. #DBG: Split test ' + sltmp.CommaText);

 // cl setDestroy 15@[1302]@20

 if ( Pos('sv destroy', s) > 0 ) or
    ( Pos('se object', s) > 0 ) or
    ( Pos( 'entity_Destroy', s) > 0 ) or
    ( Pos ('cl setDestroy',  s) > 0 ) or ( Pos ('cl destroying', s) > 0 ) then
   begin
    i := ExtractNum ( sltmp[1] );
    if log_verbose >= 7 then
       wprintf ('[~T]. #DBG(Registry): object probably sv/cl destroy  #%d %s ', [i, RootRegistry.ObjectName(i)]);
    if ( i >= 0 ) and ( i < $FFFF ) then
         OnObjectRelease (i, 'log_event:' + s, FALSE );
   end;

 // sv ownership id_parent 1-|7734]2-|inventory_box:mil_cpec_hran] id_entity 3-|223][wpn_pm:wpn_pm0223] [68468]

 sv_own := ( Pos ('sv ownership id_parent', s) > 0 );
 // sv_rjct := ( Pos ('sv reject. id_parent', s) > 0 );
 if sv_own then
  begin
   p := ExtractNum ( sltmp[1] );
   i := ExtractNum ( sltmp[3] );
   // ODS('[~T]. #DBG: p = ' + IntToStr(p) + ', i = ' + IntToStr(i));
   if ( i > 0 ) and ( p >= 0 ) then
       g_rootreg.SwitchParent (i, p);
  end;

end;


function  TestObjectPtr(p: Pointer): String;

begin
 if g_rootreg = nil then exit;

 result := g_rootreg.FindObjectByPtr (p, $100, $1000);
end;

function UpdateRegistry (L: lua_State): Integer; cdecl;
begin
 result := 0;
 active_L := L;
 {$IFDEF NEWEST_BUILD}
 if (g_game_build <= 3312) then
     raise ERegistryException.Create ('For NEWEST_BUILD id = ' + IntToStr(g_game_build));
 {$ENDIF}
 try
  DetectPointers;
  lreg_L := L;
  RootRegistry.Update ( ( lua_gettop(L) > 0 ) and lua_toboolean ( L, 1 ) );
  lreg_L := nil;
 except
  on E: Exception do
     OnExceptLog('UpdateRegistry', E);
 end;
end;

function IsShortPStr (pstr: PAnsiChar; max_len: Integer): Boolean;
var
   i: Integer;
begin
 result := TRUE;
 for i := 0 to max_len - 1 do
   if pstr[i] = #0 then exit;
 result := FALSE;
end;


function EntityName (e: PByteArray): String;
  var
     n: PAnsiChar;
     p: PPointer;
 begin
   result := '';
   if e = nil then exit;
   p := @e[g_offsets.se_offsets.sz_name];
   n := p^;
   if ( not IsShortPStr(n, 100) ) then
     begin
      ODS('[~T].~C0C #ERROR:~C07 to large name for spawn-entity');
      exit;
     end;
   if ( n = nil ) then exit;
   result := AnsiTrim2W (n);
 end;


procedure ClientObjectEvent(const goe: TGameObjectEvent);
var
   rr: TRootRegistry;
   go: TGameObject;
begin
 rr := g_rootreg;
 if rr = nil then exit;

 go := rr.Find(goe.id);

 case goe.msg and $FFF of
  EVT_OBJECT_ADD:
      rr.RegCLObject  (goe.id, goe.cl);

  EVT_OBJECT_DESTROY:
     if go <> nil then rr.Remove(go);

  EVT_OBJECT_PARENT:
      rr.SwitchParent (goe.id, goe.res[0]);

  EVT_OBJECT_REMOVE:
     if go <> nil then go.pInstance := nil;
 end;
end; // case

procedure ServerObjectEvent(const goe: TGameObjectEvent);
var
   rr: TRootRegistry;
begin
 rr := g_rootreg;
 if rr = nil then exit;
 case goe.msg and $FFF of
  EVT_OBJECT_ADD:
      rr.RegSEObject(goe.id, goe.se);

  EVT_OBJECT_PARENT:
      rr.SwitchParent (goe.id, goe.res[0]);

  EVT_OBJECT_REMOVE:
      rr.MarkObjectBad(goe.id);
 end; // case

end;

procedure FastObjectEvent(const goe: TGameObjectEvent); cdecl;
begin
 try
   Inc ( RootRegistry.ext_events );

   if goe.msg and EVT_OBJECT_SERVER <> 0 then
      ServerObjectEvent (goe)
   else
      ClientObjectEvent (goe);
 except
   on E: Exception do
      OnExceptLog ( Format('FastObjectEvent id = %d, cl = $%p, se = $%p, msg = $%x ', [goe.id, goe.cl, goe.se, goe.msg]), E, True );
 end;

end;


function InvokeRegistry (L: lua_State): Integer; cdecl;
var
   argc, i, cnt: Integer;
   gobj: TGameObject;
   s, cmd: String;
   rr: TRootRegistry;
   sr: TObjectRegistry;
   p, p2: Pointer;

begin
 result := 0;
 active_L := L;

 argc := lua_gettop (L);
 if argc <= 0 then
   begin
    PrintError ('No arguments for InvokeRegistry!');
    exit;
   end;

 lreg_L := L;
 rr := RootRegistry;

 cmd := LuaStrArg (L);
 cmd := UpperCase ( Trim (cmd) );

 Inc (invoke_count);
 if invoke_count mod 10000 = 0 then
    wprintf('[~T/~B]. #PERF: InvokeRegistry called %d counts ', [invoke_count]);

 SetStrWZ (curr_action_0, cmd, 64);

 if log_verbose >= 7 then
   ODS('[~T]. #DBG: Processing registry cmd ~C0A' + cmd + '~C07');

 i := -1;
 s := '';

 try
   // обработка запросов мульти-операндных
   if argc >= 2 then
     begin
      i := 0;
      p := nil;
      gobj := nil;


      case lua_type (L, 2) of
        LUA_TSTRING:
         s := LuaStrArg (L, 2);

        LUA_TNUMBER:
          begin
           i := lua_tointeger (L, 2);

           if ( i >= 0 ) and ( i < WRONG_IDX ) then
             begin
              gobj := rr.Find (i);
              {
              if gobj = nil then
               begin
                rr.Update (FALSE);
                gobj := rr.Find (i);
                if gobj <> nil then
                   ODS( CFormat( '[~T].~C0C #PERF_WARN:~C07 object %s (%d) found after forced registry update', '~C07', [gobj.name, gobj.id] ) );
               end; // }

             end;

          end;

        LUA_TLIGHTUSERDATA, LUA_TUSERDATA:
           p := lua_topointer (L, 2);
      end; // case


      if ( i >= 0 ) then
        begin
         if ( cmd = 'DUMP_ONE' ) and ( gobj <> nil ) then
              wprintf(' #DUMP_OBJ: %s', [gobj.Dump]);
         if ( cmd = 'UPDATE_ONE' ) and ( gobj <> nil ) then
            begin
             gobj.LoadState (TRUE);
             exit;
            end;
         if ( cmd = 'UPD_SECTION' ) and ( gobj <> nil ) then
            begin
             rr.MapObject(gobj, FALSE);
             gobj.section := '';
             if gobj.pInstance <> nil then
                gobj.LoadClient(TRUE)
             else
                gobj.LoadServer(TRUE);
             exit;
            end;

         if ( cmd = 'VERIFY_PTRS' ) then
            begin
             p  := lua_topointer(L, 3);
             p2 := lua_topointer(L, 4);
             if ( gobj <> nil ) then
                  gobj.VerifyPtrs ( p, p2 )
             else
              if (p <> nil ) or (p2 <> nil) then
                  wprintf('[~T].~C0C #FAIL:~C07 object %d not found in registry. SE_obj = $%p, CL_obj = $%p ', [i, p, p2]);
             exit;
            end;

        end;

      if ( cmd = 'SPAWN_ENTITY' ) then
        begin
         s := LuaStrArg(L, 2);
         i := lua_tointeger (L, 3); // spawn_id, story_id, game_id
         p := nil;

         if s = 'STORY_ID' then
            i := rr.spawn_index.story_spid [i]; // story_id to spawn_id
         if s = 'GAME_ID' then
            i := rr.spawn_index.game_spid [i];  // obj_id to spawn_id

         if i < WRONG_IDX then
            p := rr.spawn_objs[i]; // spawn_id

         if ( p <> nil ) then
             AssignMetaIndex (L, p, _spawn_entity_index, nil, 'MT_SPAWN_ENTITY')
         else
             lua_pushnil (L);
         result := 1;
         exit;
        end;

      if ( cmd = 'VALIDATE_SPAWN_ID' ) then

       begin
        i := lua_tointeger (L, 2);
        gobj := rr.Find(i);
        if gobj <> nil then
           begin
            i := rr.spawn_map.IndexOf(gobj.Name); // индекс в карте
            if i >= 0 then
               i := rr.spawn_map.I32Tags[i] and $FFFF // to spawn_id
            else
               i := WRONG_IDX;

            if (i <> gobj.spawn_id) or (i <> rr.spawn_index.game_spid[gobj.id]) then
              begin
               wprintf('[~T].~C0C #ERROR:~C07 object spawn_id = %d, spawn_index[%d] = %d, found by name %s = %d  ',
                                   [gobj.spawn_id, gobj.id, rr.spawn_index.game_spid[gobj.id], gobj.Name, i]);

               gobj.UpdateSpawnID(i);
               lua_pushboolean (L, FALSE);
              end
            else
              lua_pushboolean (L, TRUE);

           end
        else
            lua_pushboolean (L, FALSE);

        exit;
       end;

      if ( cmd = 'SET_ACTIVE_LEVEL' ) and ( argc >= 2 ) then
       begin
        g_level_name := LuaStrArg (L, 2);
        active_level := lua_tointeger (L, 3);
       end;


      if ( cmd = 'SET_LEVEL_INFO' ) and ( argc >= 5 ) then
       begin
        // level_name, level_id, gvid_min, gvid_max
        rr.SetLevelInfo (LuaStrArg(L, 2), lua_tointeger(L, 3), lua_tointeger(L, 4), lua_tointeger(L, 5));
        result := 1;
        lua_pushboolean (L, TRUE);
        exit;
       end;


      if ( cmd = 'SET_POINTER' ) and (argc >= 3) then
        begin
          p := lua_topointer (L, 3);
          if lua_isstring(L, 2) then
             SetXrPointer (LuaStrArg(L, 2), p);

          // ODS( CFormat( '[~T].~C0F #DBG: RootRegistry pointer %d updated to $%p ~C07', '~C07', [i, p] ) );
          exit;
        end;

       if (cmd = 'SET_CALLBACK') and Assigned(gobj) and (argc >= 3) then
         begin
          gobj.event_func := LuaStrArg(L, 3);
          if gobj.event_func = '~' then
             gobj.event_func := ''
          else
             ODS('[~T]. #DBG: registered callback ~C0A' + gobj.event_func + '~C07 for~C0D #' + IntToStr(i) + '~C07');
          if argc >= 4 then
             gobj.event_obj := lua_topointer(L, 4);
          if argc >= 5 then
             gobj.event_set := LuaStrArg(L, 5)
          else
             gobj.event_set := '';

          exit;
         end;

      if (cmd = 'CONTENT') or (cmd = 'CHILDS') or (cmd = 'SECTIONS') or (cmd = 'FULL') or (cmd = 'FILTERED') or (cmd = 'SIEVE') then // содержимое непися/монстра/ящика
         begin
          rr.ScanObjects (rr.destr_events = 0);
          rr.RemoveBad;

          lua_createtable (L, 0, 0);
          result := 2; // таблица оставлена в стеке, количество аварийно
          if not rr.Enabled then
             begin
              lua_pushnumber (L, 0);
              exit;
             end;


          sr := nil;

          if ( cmd = 'SIEVE' ) then // отсев id из таблицы. InvokeRegistry('Sieve', filt_list, flags, ids)
             sr := rr.GetFiltered ( p, L, 4 );



          if ( cmd = 'CONTENT' ) or (cmd = 'CHILDS') then
            begin
             if gobj = nil then
                begin
                 PrintError ('Cannot get content. Not found game-object #' + IntToStr (i));
                 lua_pushnumber (L, 0); // count if error
                 exit
                end;
             if cmd = 'CHILDS' then g_rootreg.AssignParents;
             sr := gobj.Childs;
            end;


          if (cmd = 'FILTERED') then
              sr := rr.GetFiltered (p, nil, 0 );

          if (cmd = 'SECTIONS') and (s <> '') then
              sr := rr.JoinSections (s);

          if (cmd = 'FULL') then
              sr := rr;

          if sr = nil then
            begin
             lua_pushnumber (L, 0); // count if error
             exit;
            end;


          i := 15; // all
          if argc >= 3 then
             i := lua_tointeger(L, 3);

          if log_verbose >= 7 then
             ODS( CFormat('[~T]. #DBG: storing %d items from registry %s', '~C07', [sr.Count, sr.Designation]));

          {if cnt > 1000 then
             begin
              lua_pop (L, 1);
              lua_createtable (L, cnt, 0); // preallocated
             end;}

          cnt := sr.StoreIDs (L, i);
          lua_pushnumber (L, cnt);

          if ( cnt <> sr.Count ) and ( log_verbose >= 7 ) then
               ODS( CFormat('[~T]. #DBG: stored objects %d from %d', '~C07', [cnt, sr.Count] ));
          result := 2;

          sr.LoadAll (TRUE);
          exit;
         end;


      if cmd = 'FIND' then
         begin
          gobj := rr.FindByName (s);
          if gobj <> nil then
             lua_pushinteger ( L, gobj.id )
          else
             lua_pushinteger ( L, WRONG_IDX );
          result := 1;
          exit;
         end;


      if cmd = 'DUMP' then rr.Dump ( i  );

      if ( cmd = 'ADD' ) and ( i <> WRONG_IDX ) then
         begin
          if log_verbose > 7 then
             ODS('[~T]. #DBG: Trying find object #' + IntToStr(i));

          p := nil;
          p2 := nil;

          if argc >= 3 then
             p := lua_topointer (L, 3); // lightuserdata
          if argc >= 4 then
             p2 := lua_topointer (L, 4); // lightuserdata

          OnSpawn ( p, p2 );
          // rr.RegObject (i, p, p2);

         end;

      if cmd = 'REMOVE' then
         begin
          if gobj <> nil then
             gobj.Release ('remove from script');
          result := 0;
          exit;
         end;

   end; // if argc >= 2

   if cmd = 'STORY_ID_MAP' then
     begin
      result := 1;
      rr.GetStoryIDMap(L);
     end;


   if cmd = 'PROCESS_EVENTS' then
      rr.ProcessEvents (L);


   if cmd = 'SCAN_OBJS' then
      rr.ScanObjects (FALSE);

   if cmd = 'CLEAR' then
     begin
      rr.dbg_context := 'default';
      if argc >= 2 then
         rr.dbg_context := LuaStrArg(L, 2);
      rr.Clear;
     end;
   if cmd = 'UPDATE' then
      rr.Update ( FALSE );

   if cmd = 'DISABLE_UPDATES' then
      rr.Enabled := TRUE;

   if cmd = 'ENABLE_UPDATES' then
     begin
      rr.Enabled := TRUE;
      FillChar (removed_evts, sizeof(removed_evts), 0);
      if class_map.IndexOfName ('artefact') >= 0 then
         art_clsid := class_map.IntValues['artefact'];
     end;

  lreg_L := nil;
 except
  on E: EAssertionFailed do
     PrintError ('Assertion failed in InvokeRegistry ' + E.Message);
  on E: Exception do
     OnExceptLog ('InvokeRegistry("' + cmd + '" , i = ' + IntToStr(i) + ', s = ' + s + ')', E);
 end;
end;

var
   fake_obj_section: TSharedStr;
                fma: TFastMemAllocator = nil;



function MakeFakeClientObject ( id: WORD ): PByteArray;
var
  pvv: P3DVector4;
   pp: PPointer;
   pw: PWORD;
   ps: PSharedStr;

begin
 result := fma.AllocMem (256);
 pw := @result[g_offsets.id_word];
 pw^ := id;
 pvv := @result[g_offsets.direction];
 pvv.x := ( Random() - 0.5 ) * 10000;
 pvv.y := ( Random() - 0.5 ) * 30;
 pvv.z := ( Random() - 0.5 ) * 10000;
 ps := fma.AllocMem ( 128 );
 SetStrZ ( ps.txt, 'fake_obj_' + IntToStr(id), 128 );
 ps.len := AnsiStrings.StrLen (ps.txt);
 pp := @result[g_offsets.cl_name];
 pp^ := ps;
 pp := @result[g_offsets.section];
 pp^ := @fake_obj_section;
end;

function MakeFakeServerObject ( id: WORD ): PByteArray;
var
   pp: PPointer;
   pw: PWORD;
   pa: PAnsiChar;

begin
 result := fma.AllocMem (256);
 pw := @result[g_offsets.se_offsets.id_word];
 pw^ := id;
 pa := fma.AllocMem (64);
 SetStrZ(pa, 'fake_obj_' + IntToStr(id), 64);
 pp := @result[g_offsets.se_offsets.sz_name];
 pp^ := pa;
 pw := @result[g_offsets.se_offsets.online];
 pw^ := 1;
end;

function MakeFakeTree ( level: Integer; ctx: PFakeTreeContext ): PTreeMapNode;
begin
 result := fma.AllocMem ( sizeof (TTreeMapNode) );

 if level = 0 then
    begin
     result.is_nil := TRUE;
     result.key := WRONG_IDX;
     exit;
    end;

 result.left := MakeFakeTree ( level - 1, ctx );
 result.key := ctx.last_id;
 result.obj := ctx.obj_maker ( result.key );
 Inc ( ctx.last_id );
 result.right := MakeFakeTree ( level - 1, ctx );
end;


function  ProfileRegistry (L: lua_State): Integer; cdecl;
var
  ctx: TFakeTreeContext;
   rr: TRootRegistry;
   tc: Integer;
    n: Integer;

begin
 result := 0;
 fma := TFastMemAllocator.Create ('FakeStorage');
 rr := RootRegistry;

 SetStrZ ( fake_obj_section.txt, 'fake_object', 100 );
 fake_obj_section.len := AnsiStrings.StrLen (fake_obj_section.txt);
 ODS('[~T/~B]. #PERF: creating client object tree...');
 ctx.obj_maker := MakeFakeClientObject;
 ctx.last_id := 0;
 g_client_map_head := MakeFakeTree ( 15, @ctx );
 ctx.obj_maker := MakeFakeServerObject;
 ctx.last_id := 0;
 g_server_map_head := MakeFakeTree ( 15, @ctx );
 tc := 1000;
 if lua_gettop(L) > 0 then
    tc := lua_tointeger (L, 1);

 ODS('[~T/~B]. #PERF: starting tests...');
 for n := 1 to tc do
     rr.Update(FALSE);
 ODS('[~T/~B]. #PERF: complete tests.');

end;


procedure FinalizeRegistry;
var
   gs: TWorkerThread;
begin
 ODS('[~T]. #DBG: Уничтожение главного реестра...');

 FreeAndNil (sltmp);
 if Assigned (g_rootreg) then
  try
   removed_list.Clear;
   g_rootreg.dbg_context := 'FinalizeRegistry';
   g_rootreg.Clear;
   if Assigned (g_scanner) then
     begin
      g_scanner.Priority := tpIdle;
      if bBeforeExit then
       begin
        g_scanner.StopThread();
        g_scanner.WaitStop();
        FreeAndNil (g_scanner);
       end;

     end;
  except
   on E: Exception do
      OnExceptLog ('FinalizeRegistry', E);
  end;

 FreeAndNil (g_rootreg);
end;


function OnModuleRqs (md: TModuleDescriptor; rqs, flags: DWORD): Boolean;
begin
 result := FALSE;
 case rqs of
      MRQ_INITIALIZE:  // ==================================================================================================== //
          begin
           result := (MST_INITIALIZED <> md.Status);
          end;
      MRQ_FINALIZE: // ==================================================================================================== //
          begin
           result := (MST_INITIALIZED = md.Status);
           FinalizeRegistry (TRUE);
          end;
     end; // case
 end; // OnModuleRqs


function RootRegistry( bCreateNotExist: Boolean ): TRootRegistry;

begin
 if ( g_rootreg = nil ) and ( IsDebuggerPresent or bCreateNotExist ) then
      g_rootreg := TRootRegistry.Create ('root_registry');

 result := g_rootreg;
end;



{ TObjectRegistry }

function TObjectRegistry.Add(gobj: TGameObject): Integer;
var
   l, h: Integer;
begin
 result := -1;
 if gobj = nil then exit;
 l := -1;
 h := -1;
 result := FindPosition ( gobj.id, @l, @h );
 if result < 0 then
   begin
    if Count > 0 then
      begin
       Insert ( l + 1, gobj );
       result := l + 1;
      end
    else
       result := inherited Add ( gobj );
    FCountUpdated := TRUE;
   end;

 if IsRoot then // главный регистр?
   begin
     if Count mod 5000 = 0 then
      ODS('[~T/~U/~B]. #DBG: Root registry obj. count =~C0D ' + IntToStr(Count) + '~C07');
   end;

 // Sort ( cmpGameObjectID );


 if Count > 65534 then
    PrintError('Registry ' + self.Designation + ' obj. count = ' + IntToStr(Count));



end;

procedure TObjectRegistry.AssignParents;
var
  gobj: TGameObject;
  pobj: TGameObject;
   pid: WORD;
     n: Integer;

begin
 
 for n := 0 to Count - 1 do
   begin
    gobj := Objects [n];

    pid := gobj.last_pid;
    if pid < WRONG_IDX then
      begin
       pobj := g_rootreg.Find (pid);
       if ( pobj <> nil ) and ( pobj <> gobj.Parent ) then
            pobj.AddChild (gobj)
       else
            gobj.FParent := pobj;

       if gobj.Parent = nil then
         begin
          if gobj.warn_cnt = 0 then
             ODS( CFormat( '[~T].~C0C #WARN:~C07 not found parent instance %d for object %s (%d), cl = %d ', '~C07', [pid, gobj.name, gobj.id, Integer(gobj.is_online)] ));
          Inc (gobj.warn_cnt);
         end
       else
       if gobj.warn_cnt > 0 then
         begin
          ODS( CFormat( '[~T].~C0F #MSG: found parent instance %d for object %s (%d), warn_cnt = %d ', '~C0F', [pid, gobj.name, gobj.id, gobj.warn_cnt] ));
          gobj.warn_cnt := 0;
         end;
      end
     else
      gobj.Parent := nil;

    if gobj.Childs <> nil then
       gobj.CheckChilds;
   end; // for
end;

procedure TObjectRegistry.Clear;
begin
 if (log_verbose > 10) or ( (log_verbose = 10) and (Count > 500)) then
    ODS('[~T]. #DBG: Clearing registry~C0A ' + self.Designation + '~C07 obj. count =~C0D ' + IntToStr(Count) + '~C07, ctx = ' + dbg_context );
 inherited Clear;
end;

constructor TObjectRegistry.Create(const ADesignation: String);
begin
 inherited Create (FALSE);
 dbg_context := 'c'; // create
 FFreezeMax := 2000;
 FDispTimer := TProfileTimer.Create;
 FDesignation := ADesignation;
 if Pos('Content', Designation) > 0 then
    Capacity := 32
 else
    Capacity := 1024;
end;

destructor TObjectRegistry.Destroy;
begin
 if log_verbose > 5 then
    ODS('[~T]. #DBG: destroying object registry ~C0A' + Designation + '~C07');
 dbg_context := 'Destroy:' + dbg_context;
 FreeAndNil(FDispTimer);
 inherited;
 FCache.Free;
end;

procedure TObjectRegistry.Dump;
var
           gobj: TGameObject;
         n, onl: Integer;
              s: String;
    bOnlyOnline: Boolean;
begin

 if IsRoot then
    ODS('[~T/~U/~B].~C0F #DBG: Dumping game root registry, objects count =~C0D ' + IntToStr(Count) +
         '~C07, mem_usage =~C0D ' + IntToStr(MemUsage div 1024) + '~C07 KiB')
 else

    ODS('[~T].~C0F #DBG: Dumping sub-registry for section ~C0A' + Designation + ' ~C07');

 bOnlyOnline := flags and 2 <> 0;

 for n := 0 to Count - 1 do
    begin
     gobj := Objects [n];
     onl := 0;
     if gobj._is_online <> nil then
        onl := BYTE (gobj._is_online^);

     if (bOnlyOnline) and (onl = 0) then continue;

     with gobj do
        s := CFormat ('~C0E id = %5d, name = %30s, section = %30s, pInst = $%p, pServInst = $%p, ci = %5d, pi = %5d, ul = %7d,' +
                             ' gi = %5d, li = %4d, on = %d, pos = %.3f, %.3f ~C07', '~C0E',
                            [id, name, section, pInstance, pServInst, class_id, parent_id, upd_loops, gvid, lvid, onl, position.x, position.z]);
     ODS (s);
    end; // for

end;

function TObjectRegistry.Find (id: WORD): TGameObject;
var
   i: Integer;
begin
 // TODO: source can'be sorted - use binary search
 i := FindPosition (id, nil, nil);
 if i >= 0 then
    result := objects [i]
 else
    result := nil;
end;

function TObjectRegistry.FindPosition(id: WORD; pLow, pHigh: PInteger): Integer;
begin
 result := Algs.BinarySearch ( self.List, Ptr(id), Count, bscmpGameObjectID, pLow, pHigh );
end;

function TObjectRegistry.GetDebugContext: String;
begin
 if DWORD (self) < $10000 then
     raise ERegistryException.Create ('TObjectRegistry.self bad ptr');
 result := Trim ( PChar(@FDbg_context) );
end;

function TObjectRegistry.GetGameObject(index: Integer): TGameObject;
begin
 result := TGameObject ( items[index] );
end;


procedure TObjectRegistry.LoadAll;
var
   gobj: TGameObject;
   n: Integer;
begin
 chkd_count := 0;
 upd_obj_cnt := 0;
 gobj := nil;
 for n := Count - 1 downto 0 do
    try
     gobj := Objects [n];
     if ( gobj = nil ) or ( not gobj.is_exists ) then continue;
     if DWORD(gobj) < $10000 then
       begin
        wprintf('[~T]. #ERROR(LoadAll): invalid pointer gobj = %p ', [gobj]);
        Items [n] := nil;
        continue;
       end;

     gobj.updated := FALSE;
     gobj.LoadState (bForce or gobj.active);
     if gobj.updated then Inc (upd_obj_cnt);
     if ( not bForce ) and ( n and $FFF = $FFF ) and ( FDispTimer.Elapsed (1) > FreezeMax ) then
        begin
         ODS( ftow(FreezeMax, '[~T].~C0C #PERF_WARN(TObjectRegistry.LoadAll):~C07 antifreeze break (%.0f ms)') );
         break;
        end;
    except
     on E: Exception do
       begin
        OnExceptLog ('LoadAll, n = ' + IntToStr(n) + ' gobj = ' + FormatPtr(gobj), E, TRUE );
        gobj.Release('Exception in LoadAll');
        break;
       end;
    end;
end;



function TObjectRegistry.MemUsage: DWORD;
var
   n: Integer;

begin
 result := InstanceSize;
 if not IsRoot then exit;
 for n := 0 to Count - 1 do
     Inc (result, Objects [n].MemUsage);
end;

procedure TObjectRegistry.Remove(gobj: TGameObject);
var
   lcnt: Integer;
begin
 lcnt := Count;
 inherited Remove (gobj);
 FCountUpdated := (Count <> lcnt);
end;

procedure TObjectRegistry.SetDebugContext(const Value: String);
var
   l: Integer;
begin
 l := Min ( High (FDbg_context), Length(value) );
 SetStrWZ ( @FDbg_context, Value, l + 1 );
end;

function TObjectRegistry.StoreIDs(L: lua_State; flt: Integer): Integer;
var
   gobj: TGameObject;
   r, n: Integer;
   _liv: Boolean;
   _itm: Boolean;
   _onl: Boolean;
   _off: Boolean;
begin
 //
 result := 0;

 _liv := ( flt and $01 <> 0 );
 _itm := ( flt and $02 <> 0 );
 _onl := ( flt and $04 <> 0 );
 _off := ( flt and $08 <> 0 );


 if not lua_istable (L, -1) then
   begin
    PrintError('StoreIDs need table in stack[-1]');
    exit;
   end;

 for n := 0 to Count - 1 do
  begin
   gobj := Objects [n];
   r := 0;
   if (gobj <> nil) then
    with gobj do
     begin
      if (id = WRONG_IDX) or (bad_mark <> 0) then r := 2;
      if r = 0 then
      {$IFNDEF OLDVER}
        begin
         if (not is_alive)     and ( not _itm ) then r := 8;
         if gobj.is_alive      and ( not _liv ) then r := 9;
         if (pInstance <> nil) and ( not _onl ) then r := 10; // не требуются онлайн
         if (pInstance = nil)  and ( not _off ) then r := 11; // не требуются оффлайн
        end; // case
      {$ELSE}
        case flt of
         0: if (pInstance = nil) then else r := 10; // offline only
         1: if (pInstance = nil) then r := 11;      // online only
         2: if (pInstance = nil) and is_online then else r := 12;
        end; // case
      {$ENDIF}
     end
   else
     r := 1;

   if r <> 0 then
     begin
      if log_verbose >= 11 then
         wprintf ('[~T]. #DBG: object №%5d #%5d sieved by reason %2d, bad_mark = %d',
                                [n, gobj.id, r, gobj.bad_mark]);
      continue;
     end;
   Inc (result);
   lua_pushnumber (L, result);
   lua_pushnumber (L, gobj.id);
   lua_settable (L, -3);  // default logic!
  end;
end;


procedure TObjectRegistry.StoreObjects(dst: TObjectRegistry);
var
   i: Integer;
begin
 if Count = 0 then exit;
 i := dst.Count;
 dst.Count := i + Count;

 if log_verbose >= 7 then
    ODS( CFormat('[~T]. #DBG: Storing  from section %25s to %25s, %d items, dst.count = %d ', '~C07', [Designation, dst.Designation, Count, dst.Count]));
 CopyMemory ( @dst.List [i], List, Count * sizeof(Pointer) );
end;


{ TGameObject }

procedure TGameObject.AddChild (gobj: TGameObject);
begin
 if Childs = nil then
    FChilds := TObjectRegistry.Create ('Childs#' + section + '@' + IntToStr(id) );

 gobj.FParent := self;

 if Childs.IndexOf (gobj) < 0 then
    Childs.Add (gobj);
end;

procedure TGameObject.Assign(gobj: TGameObject);
begin
 // только абстрактные параметры. Прототип объекта, который возможно исчезнет через секунду
 id := gobj.id;
 Name := gobj.Name;
 section := gobj.section;
 lvid := gobj.lvid;
 gvid := gobj.gvid;
 cl_position := gobj.Position^;
 se_position := cl_position;
end;

procedure TGameObject.CheckChilds;
var
   itm: TGameObject;
   n: Integer;
begin
 // dbg_vars.Values['in_func'] := 'CheckChilds';
 // сверка инвентаря
 itm := nil;
 if Assigned(Childs) then
  try
    for n := Childs.Count - 1 downto 0 do
       begin
        itm := Childs [n];
        if (itm.bad_mark = 0)  and (itm.parent_id <> id) then
           begin
            // PrintError('exclusive child object deletion ' + itm.name + ' from ' + name );
            Childs.Remove (itm);
           end;
       end;
  except
   on E: Exception do
     begin
      if Assigned (itm) then
        begin
         wprintf('[~T].~C0C #ERROR:~C07 invalid object %d pInstance = $%p, bad_mark = %d ', [itm.id, itm.pInstance, itm.bad_mark]);
         RootRegistry.MarkObjectBad(itm.id);
        end;


      OnExceptLog (ClassName + '.CheckChilds', E, TRUE);
      // itm.Release('Exception in CheckChilds');
     end;
  end;
end;

function TGameObject.class_id: WORD;
begin
 result := 0;
 if pServInst <> nil then
    result := se_field_w ( g_offsets.se_offsets.clsid )
 else
 if pInstance <> nil then
    result := cl_field_w ( g_offsets.clsid );

end;

function TGameObject.cl_addr(ofs: Integer): Pointer;
begin
 result := @pInstance[ofs];

end;

function TGameObject.cl_field_i(ofs: Integer): Integer;
begin
 result := PInteger ( cl_addr(ofs) )^;
end;

function TGameObject.cl_field_w(ofs: Integer): WORD;
begin
 result := PWORD ( cl_addr(ofs) )^;
end;

constructor TGameObject.Create;
begin
 // TODO: init

 // FHistory  := TStrMap.Create;

 // Release('iniitial'); // set id to WRONG
 Reset ('Create');
end;

destructor TGameObject.Destroy;
begin
 //Release;
 FreeAndNil (FChilds);
 FreeAndNil (FHistory);


 inherited;
end;


function TGameObject.Dump: String;
begin
 result := Format('id=%5d name=%-38s parent_id=%5d last_pid=%5d clsid = %4d gvid = %5d',
                  [id, Name, parent_id, last_pid, class_id, game_vertex_id]);
end;

function TGameObject.game_vertex_id: DWORD;
var
   pdw: PDWORDArray;
    pp: PPointer;

begin
 result := 0;
 if Assigned(pInstance) and IsBadReadPtr(pInstance, $100) then
    begin
     wprintf('bad pInstance for object %d = $%p ', [self.id, pInstance]);
     pInstance := nil;
    end;

 if ( pInstance <> nil) then
    try
     pp := cl_addr(g_offsets.ai_loc);

     if IsBadReadPtr(pp, 4) then
       begin
        Release('Bad pInstance PTR at [ai_loc]');
        exit;
       end;


     pdw := pp^; // Pointer (cl_field_i(g_offsets.ai_loc));

     if IsBadReadPtr(pdw, 4) then
        Release (Format('Bad pInstance.ai_loc PTR. Cannot read game_vertex_id at %p ', [pdw]))
     else
     if (pdw <> nil) then
       begin
        lvid := pdw^ [1];
        gvid := pdw^ [2] and $FFFF;
       end;

    except
     on E: Exception do
       begin
        wprintf('[~T].~C0C #EXCEPTION:~C07 TGameObject(%s).game_vertex_id accessing pInstance $%p, details: %s %s',
                    [Name, pInstance, E.QualifiedClassName, E.Message]);
        Release ('Bad pInstance.ai_loc PTR');
       end;
    end
 else
 if pServInst <> nil then
   try
    pdw := se_addr (g_offsets.se_offsets.gvx_id);
    if IsBadReadPtr(pdw, 4) then
       Release (Format('Bad pInstance PTR. Cannot read game_vertex_id at %p ', [pdw]))
    else
       gvid := pdw^ [0] and $FFFF;
   except
     on E: Exception do
       begin
        wprintf('[~T].~C0C #EXCEPTION:~C07 TGameObject(%s).game_vertex_id accessing pServInst $%p, details: %s %s',
                    [Name, self.pServInst, E.QualifiedClassName, E.Message]);
        Release ('Bad pServInst PTR. Cannot read game_vertex_id.');
       end;

   end;

 result := gvid;
end;

function TGameObject.History: TStrings;
begin
 if FHistory = nil then
    FHistory := TStringList.Create;
 result := FHistory;
end;

function TGameObject.is_online: Boolean;
begin
 result := (_is_online <> nil) and ( pInstance <> nil ) and (_is_online^);
end;

procedure TGameObject.LoadState;
var
   idx_pass: Boolean;
     se_upd: Boolean;

begin
 if ( bad_mark <> 0 ) or ( scan_loop = owner.upd_loop ) then exit;
 scan_loop := owner.upd_loop;
 updated := False;
 bForce := bForce or ( upd_loops <= 2 );
 idx_pass := ( owner.upd_loop and owner.load_mask = id and owner.load_mask );

 if ( pServInst <> nil ) and ( flags and 1 <> 0 ) then
   begin
    is_exists := TRUE;
    se_upd := ( not is_online );
    if bForce or ( se_upd and idx_pass ) then
                          LoadServer (bForce); // эпизодически
   end;
 if ( pInstance <> nil ) and ( flags and 2 <> 0 ) then
   begin
     is_exists := TRUE;
     if bForce or idx_pass then
                          LoadClient (bForce); // более регулярно
   end;

 // handling
 if (upd_loops <= 2) and (class_id = 33) and (log_verbose > 11) then
     wprintf('[~T]. #DBG(TGameObject.LoadState): id = %3d, name = %-40s, clsid = %d, active = %d, online = %d ', [id, Name, class_id, Ord(active), Ord(is_online)]);

 Inc (upd_loops);
 if bForce or idx_pass then
    TestUpdateParent ( parent_id, 'LoadState' ); // по идее это должно отрабатывать через события
end;



procedure TGameObject.LoadClient;
var
   pvv: P3DVector4;
   pwv: PWORD;
   evt: TEventInfo;
     p: Pointer;

begin
 try
   Inc ( owner.chkd_count );
   if ( client_ref <> nil ) and ( pInstance <> client_ref^ ) then
       begin
        if name = '' then
           name := IntToStr(id);
        wprintf('[~T]. #DBG: client object %s pInstance updated from $%p to $%p, ref = $%p ', [name, pInstance, client_ref^, client_ref]);
        pInstance := client_ref^;
       end;
   if Addr(engine_object) <> nil then
    begin
     p := engine_object(id);
     if p <> pInstance then
       wprintf('[~T].~C0C #ERROR(TGameObject):~C07 cl-pointer changed, old = %p, new = %p, id = %5d, name = %s ', [pInstance, p, id, name]);
    end;


   if (  not Assigned(pInstance) ) or
      ( (upd_loops and $F = 1) and  not VerifyPtr(pInstance, $100) ) then exit;

   // pt.StartOne (1); // last-update delay

   // dbg_vars.Values['in_func'] := ClassName + '.LoadClient';
   // dbg_vars.Values['context'] := Format('id = %d, index = %d, parent_id = %d, clsid = %d, pInst = $%p', [id, index, parent_id, class_id, pInstance]);

   if ( upd_loops <= 1 ) or ( upd_loops and $0F = 0 ) or (bForce) then
     // редкие внутренние проверки
     begin
      Inc ( owner.heavy_ops );
      pwv := cl_addr (g_offsets.id_word);
      if id <> pwv^ then
        begin
         PrintError ( Format('Object #%d has wrong instance = replaced/released in game. Client id_word = %d ', [id, pwv^]) );
         Release('LoadClient id mistmatch');
         exit;
        end;

      name := LoadSharedStr (pInstance, g_offsets.cl_name, TRUE);
      visual := LoadSharedStr (pInstance, g_offsets.visual, TRUE);

      if ( self.section = '' ) or ( self.section = '#reseted' ) then
        begin
         upd_loops := 0;
         section := LoadSharedStr (pInstance, g_offsets.section, TRUE);

         if Assigned (sc_registry) then
            sc_registry.Remove (self);  // remove from old section

         sc_registry := nil;
         Log ( 'section_set from client ' + self.section );
         // Owner.MapObject (self);
         OnUpdate;
        end;
     end;



   if ( event_func = '' )  then
    begin // копирование направления и позици
     pvv := cl_addr ( g_offsets.direction );
     updated := updated or pvv.xz_chg ( direction );
     if VerifyPtr(pvv) then
        direction := pvv^
     else
        PrintError (Format('Invalid pointer calc, g_offsets.fwd_dir = $%x, pInstance = %p ', [g_offsets.direction, pInstance]) );

     pvv := cl_addr ( g_offsets.position );
     updated := updated or pvv.xz_chg ( cl_position );
     cl_position := pvv^;
     exit;
    end;

   // dynamic params
   // редкоотслеживаемые объекты проверять иногда лишь

   if ( TimerElapsed (2) < 100 ) then exit;
   TimerStart (2);

   // TODO: change-dir event detect
   pvv := @pInstance[g_offsets.direction];
   if not direction.Equal (pvv^, 0.001) then
    begin
     direction := pvv^;
     evt := TEventInfo.Create (id, pvv);
     Owner.Events.AddObject ('DIR_CHG', evt);
    end;


   pvv := @pInstance[g_offsets.position];
   if not cl_position.Equal (pvv^, 0.001) then
    begin
     avg_speed := cl_position.distance_xz (pvv^) * 1000 / TimerElapsed (7);
     TimerStart (7);
     cl_position := pvv^;
     evt := TEventInfo.Create (id, pvv, avg_speed);
     evt.vlst [7] := upd_loops;
     Owner.Events.AddObject ('POS_CHG', evt);
    end;

   // dbg_vars.Values['in_func'] := '~~';
 except
   on E: Exception do
      OnExceptLog (ClassName + '.LoadClient, id = #' + IntToStr(id) + ', name = ' + name +
                               ', pInstance = ' + FormatPtr(pInstance) + ', stable = ' + IntToStr(stable), E);
 end;
end;

procedure TGameObject.LoadServer;
var
   pvv: P3DVector4;
   pfv, pfv2: PSingle;
   ppv: PPointer;
   pas: PAnsiChar;
   evt: TEventInfo;
    sc: String;
     s: String;
     p: Pointer;
     n: Integer;

begin
 if (Addr(alife_object) <> nil) then
  begin
   p := alife_object(id);
   if p <> pServInst then
     wprintf('[~T].~C0C #ERROR(TGameObject):~C07 sv-pointer changed, old = %p, new = %p, id = %5d, name = %s ', [pServInst, p, id, name]);
   pServInst := p;
  end;

 if (bad_mark <> 0) or (pServInst = nil) or not VerifyID(FALSE) then exit;
 Inc ( owner.chkd_count );
 s := '?' + IntToStr(self.id) + ':' + self.Name;

 with g_offsets do
  try
   if (se_offsets.released > 0) and (se_field_i(se_offsets.released) > 0) then
     begin
      Release('by-alife!');
      exit;
     end;

   if ( upd_loops <= 1 ) or ( upd_loops and $1F = 0 ) or bForce then
    begin
     Inc ( owner.heavy_ops );
     // VerifyID (FALSE);
     n := se_field_w ( se_offsets.spawn_id );
     if (spawn_id = WRONG_IDX) and ( n < WRONG_IDX ) then
         UpdateSpawnID (n);

     s := '#noname_' + IntToStr (self.id);
     ppv := se_addr(se_offsets.sz_name); // cast pointer
     if not VerifyPtr(ppv, 4) then
        raise ERegistryException.Create(' bad se-object ptr ' + FormatPtr(pServInst));
     pas := ppv^;
     if nil = pas then
       begin
        wprintf('[~T].~C0C #ERROR:~C07 object %d (%s) se-name = nil ', [self.id, name]);
        Release('late-destroyed');
        exit;
       end;

     if not VerifyPtr(pas, 64) then
        raise ERegistryException.Create(
                ' wrong se_offsets.sz_name or destroyed se-object, pString = ' + FormatPtr(pas) +
                ' last_name = ' + Name);


     n := AnsiStrings.StrLen(pas);
     if n > 50 then
       begin
        PrintError( Format(' absurd name length = %d. Object have invalid pointer ServInst', [n]));
        Release('bad_pointers');
        exit;
       end;

     if Assigned(pas) then
        s := String (pas);

     sc := LoadSharedStr(pServInst, se_offsets.section, TRUE);
     if ( self.name = '' )    or ( self.name    = '#reseted' ) or ( self.name <> s ) or
        ( self.section = '' ) or ( self.section <> sc ) or ( self.section = '#reseted' ) then
       begin
        self.name := s;
        self.section := sc;
        if self.bad_mark > 0 then exit;
        Log ( 'section_set from server ' + self.section );
        Owner.MapObject (self);
        self.OnUpdate;
       end;

    end; // episodic check
   {
   }

   _is_online := se_addr(se_offsets.online);
   if not VerifyPtr(_is_online) then
      begin
       wprintf('[~T].~C0C #ERROR:~C07 bad pServInst._is_online = $%p for object %s ', [ Pointer(_is_online), self.name]);
       Release('bad_pointers');
       exit;
      end;

   if ( not _is_online^ ) and ( pInstance <> nil ) then
      begin
       ODS( CFormat('[~T]. #DBG: client instance removed for object %d by online flag', '~C07', [id]) );
       pInstance := nil;
      end;

   if not is_online then
    begin
     gvid := se_field_w (se_offsets.gvx_id);
     lvid := se_field_i (se_offsets.lvx_id);
     pvv := se_addr (se_offsets.position);

     updated := updated or pvv.xz_chg ( self.se_position );

     pfv  := se_addr(se_offsets.angles + 4);
     pfv2 := se_addr(se_offsets.angles + 8);

     if self.event_func <> '' then
       begin
         if not se_position.Equal (pvv^, 0.001) then
          begin
           evt := TEventInfo.Create (id, pvv, 0);
           evt.vlst [7] := upd_loops;
           Owner.Events.AddObject ('SE_POS_CHG', evt);
          end;

         if ( pfv^ <> ang_h ) or ( pfv2^ <> ang_v ) then
           begin
            evt := TEventInfo.Create (id, nil, pfv^);
            evt.vlst [1] := pfv2^;
            evt.vlst [7] := upd_loops;
            Owner.Events.AddObject ('ANG_CHG', evt);
           end;
      end; // if event_func <> ''

     if VerifyPtr(pvv, 12) then se_position := pvv^;
     if VerifyPtr(pfv,  4) then ang_h := pfv^;
     if VerifyPtr(pfv2, 4) then ang_v := pfv2^;
    end // if is_online
  else
     gvid := game_vertex_id;


  except
    on E: ERegistryException do
      begin
       Release('bad_pointers');
       bad_mark := 18;
       wprintf(' context = "%s" ', [s]);
       if lreg_L <> nil then ODS(LuaTraceback (lreg_L,  '    '));
       wprintf('[~T].~C0C #REGISTRY_EXCEPTION:~C0F %s~C07', [E.Message]);
      end;

    on E: Exception do
      begin
       bad_mark := 15;
       wprintf(' context = "%s" ', [s]);
       if lreg_L <> nil then ODS(LuaTraceback (lreg_L,  '  '));
       OnExceptLog (ClassName + '.LoadServer, id = ' + IntToStr(id), E, TRUE);
      end;
  end;

end;

procedure TGameObject.TestUpdateParent;

begin
 if ( last_pid = pid ) then exit;
 warn_cnt := 0;
 updated := TRUE;
 if ( log_verbose >= 3 ) and ( Pos ('repair', name) > 0 ) then
      ODS( CFormat('[~T]. #DBG: for object %s (%d) parent changed from %d to %d, ctx = %s, online = %s ', '~C07',
                                       [name, id, last_pid, pid, ctx, IfV(is_online, 'yes', 'no')]));

 last_pid := pid;
 // parent may be changed any moment
 Parent := nil; // old pointer remove
end;


function TGameObject.TimerElapsed(nt: Integer): Double;
begin
 result := ( g_time_func - obj_timers [nt] ) / DT_ONE_MSEC;
end;

procedure TGameObject.TimerStart(nt: Integer);
begin
 obj_timers [nt] := g_time_func();
end;

procedure TGameObject.UpdateSpawnID(spid: WORD);
var
   pswi: PWORD;
    ref: DWORD;
begin
 if name <> '#reseted' then
  begin
   ref := Owner.FindSpawnID (Name, gvid);
   if ref = 0 then
      ref := WRONG_IDX
   else
      ref := ref and WRONG_IDX;
   if (ref < WRONG_IDX) and (ref <> spid) then
     begin
      // wprintf('[~T].~C0C #WARN:~C07 trying assign wrong spawn_id %d, vs true %d ', [spid, ref]);
      spid := ref;
     end;
  end;

 if spid = 13089 then
    __nop;
 spawn_id := spid;
 pswi := self.se_addr(g_offsets.se_offsets.spawn_id);

 if Assigned (pServInst) then
    pswi^ := spid;

 with self.Owner do // updating index
  begin
   spawn_index.game_spid  [id] := spid;
   spawn_index.spid_game  [spid] := id;
   spawn_index.story_spid [story_id] := spid;
  end;
end;

function TGameObject.VerifyID (bClient: Boolean ): Boolean;
var
   pwv: PWORD;
   piv: PInteger;
   src: PByteArray;
   ofs: Integer;
     n: Integer;
     s: String;
begin
 with g_offsets do
  begin
   result := TRUE;
   if bClient then
     begin
      src := cl_addr (0);
      ofs := id_word;
     end
   else
     begin
      src := se_addr (0);
      ofs := se_offsets.id_word;
     end;
   Assert ( src <> nil, 'src = nil' );
   pwv := @src[ofs];


   if id <> pwv^ then
      begin
       result := FALSE;
       wprintf ('[~T]. #WARN: Object #%d (%s) has wrong format. Found value = %d at ' + IfV (bClient, 'CL_OBJ', 'SE_OBJ') +
                '[$%p + $%x], added_by = %d',
                 [id, self.section, pwv^, src, ofs, added_by]);
       s := '';
       for n := 0 to 20 do
         begin
          if bClient then
             piv := cl_addr(n * 4)
          else
             piv := se_addr(n * 4);

          s := s + ' ' + IntToHex(n * 4, 2) + ':' + IntToHex ( piv^, 8 );
         end;
       ODS(' #DBG: hex id =~C0D ' + IntToHex (id, 4) + '~C07, dump:  ~C0F' + s + '~C07');

       if bClient then
         pInstance := nil
       else
         pServInst := nil;
      end;

  end; // with
end;

function TGameObject.VerifyPtrs(se_ptr, cl_ptr: Pointer): Boolean;
begin
 result := TRUE;
 if ( se_ptr <> pServInst ) and ( RelativePtr (se_ptr, SE_CAST_OFFSET) <> pServInst ) then
   begin
    wprintf('~C0C #FAIL(TGameObject.VerifyPtrs):~C07 object %d "%s" se_instance = $%p, verify = $%p ', [id, Name, pServInst, se_ptr]);
    result := FALSE;
   end;
 if cl_ptr <> pInstance then
   begin
    wprintf('~C0C #FAIL(TGameObject.VerifyPtrs):~C07 object %d "%s" cl_instance = $%p, verify = $%p ', [id, Name, pInstance, cl_ptr]);
    result := FALSE;
   end;


end;

function TGameObject.LoadSharedStr(pba: PByteArray; ofs: DWORD; is_ptr: Boolean): String;
var
   ps: PSharedStr;
   pp: PPointer;
   sa: AnsiString;
begin
 result := '';
 if (pba = nil) or (ofs = 0) then exit;
 sa := '';
 pp := nil;
 try
  pp := @pba [ofs];
  if is_ptr then
     ps := pp^
  else
     ps := Pointer(pp);
  if ps = nil then exit; // possible NULL strings, for visual


  if Assigned (ps) and VerifyPtr(ps, 4) and (ps.len > 0) then
    begin
     if ps.len > 32768 then
        raise ERegistryException.Create ( Format('shared_str at $%p [$%x],  To big length = %d', [pba, ofs, ps.len]));
     SetLength (sa, ps.len);
     Move (ps.txt, sa[1], ps.len);
    end
  else
    begin
     PrintError( Format('Bad shared_str pointer $%p for CSE_instance = $%p, ofs = $%x, id = %d ', [ps, pba, ofs, id]) );
     if id = 0 then
        ReadLn;
    end;

 except
  on E: ERegistryException do
    begin
     PrintError( Format(' Invalid object #%d pointers. Exception catched: %s', [id, E.Message] ) );
     Release ('bad_pointers');
    end;

  on E: Exception do
    OnExceptLog (ClassName + Format('.LoadStr, pData = $%p, ofs = $%x, pp = $%p, is_ptr = %d', [pba, ofs, pp, Ord(is_ptr)]), E);
 end;

 result := String (sa);
end;

procedure TGameObject.Log(const msg: String);
begin
 {$IFOPT D+}
 History.Add (msg);
 {$ENDIF}
end;

function TGameObject.MemUsage: DWORD;
begin
 result := InstanceSize;
 Inc (result, Length(section) * sizeof(Char));
 Inc (result, Length(name) * sizeof(Char));
 Inc (result, Length(visual) * sizeof(Char));

 if Assigned (FChilds) then
    Inc (result, Childs.MemUsage);

end;



function TGameObject.obj_flags: DWORD;
begin
 result := 0;
 if pServInst <> nil then
    result := se_field_i ( g_offsets.se_offsets.flags );
end;

procedure TGameObject.OnUpdate;
begin
 asm
  nop
 end;
end;

function TGameObject.parent_id: WORD;
var
   po: PByteArray;
asm
 push   ebx
 push   ecx
 push   edx
 mov    ecx, self
 mov    eax, WRONG_IDX
 // проверка наличия указателя клиентского объекта
 cmp    [ecx + FInstance], 0
 je     @check_se
 // получение объекта предка
 mov    ebx, [ecx + FInstance]   // ebx =  self.pInstance
 mov    edx, ebx
 add    edx, g_offsets.id_word
 mov    dx, word ptr [edx]
 cmp    word ptr [ecx + id], dx               // test ID = pInstance.id
 jne    @check_se

 add    ebx, g_offsets.parent    // ebx = @self.pInstance[$B4]
 mov    ebx, [ebx]               // ebx =  self.pInstance.parent_obj
 test   ebx, ebx                 // test for nil
 jz     @check_se
 add    ebx, g_offsets.id_word   // ebx = @parent_obj[id_ofs]
 movzx  eax, WORD PTR [ebx]
 jmp    @exit
@check_se:
 // проверка наличия серверного объекта
 mov    ebx, [ecx + FServInst]
 test   ebx, ebx
 jz     @exit
 add    ebx, g_offsets.se_offsets.parent_id
 movzx  eax, WORD PTR [ebx]


@exit:
 pop    edx
 pop    ecx
 pop    ebx
end;

function TGameObject.Position: P3DVector4;
begin
 result := IfV ( is_online, @cl_position, @se_position );
end;

procedure TGameObject.Release;
begin
 if ( ( log_verbose >= 10 ) and ( id > 0 ) ) or
    ( ( log_verbose >= 7 ) and ( FChilds <> nil ) )  then
    ODS( CFormat('[~T]. #DBG(Registry): releasing object %5d, parent %5d, name "%s", reason "%s"', '~C07', [id, parent_id, name, AReason]));

 if Assigned (Owner) then
    Owner.Remove (self);

 try
  Owner.by_name.Remove ( self.name );

  Reset ( 'Release (' + AReason + ')' );
  if Assigned (Childs) then
     FreeAndNil (FChilds);
 except
  on E: EAccessViolation do
     PrintError ('AV catched in TGameObject.Release: ' + E.Message);
 end;

end; // Release

procedure TGameObject.RemoveChild(gobj: TGameObject);
begin
 if Assigned (Childs) then
    Childs.Remove (gobj);
end;



procedure TGameObject.Reset;
begin
 Assert ( self <> nil, 'TGameObject.Reset: self = nil' );
 used := FALSE;
 id := WRONG_IDX;
 is_exists := FALSE;
 client_ref := nil;

 pInstance := nil;
 pServInst := nil;

 _is_online := nil;

 if is_mapped and ( Owner <> nil ) and ( Assigned (owner.by_name) ) then
    owner.by_name.Remove (Name);

 is_mapped := FALSE;

 if sc_registry <> nil then
    sc_registry.Remove (self);

 sc_registry := nil;
 stable := 0;
 bad_mark := 0;
 upd_loops := 0;

 Log ( Format( 'reseted from %s (%d), reason = %s ', [Name, id, AReason] ) );

 section := '#reseted';
 visual := '#reseted';
 name := '#reseted';
 if FChilds <> nil then
   begin
    if Childs.dbg_context = 'c' then
       Childs.dbg_context := 'Reset (' + AReason + ')';
    Childs.Clear;
   end;

 spawn_id := WRONG_IDX;
end;

procedure TGameObject.SetInstance(const Value: PByteArray);
begin
 if Value <> FInstance then self.upd_loops := 0;
 FInstance := Value;
end;

procedure TGameObject.SetObjectName(const Value: String);
begin
 if FObjectName = Value then exit;
 if ( owner <> nil ) and ( owner.by_name <> nil ) then
      owner.by_name.Remove ( FObjectName );

 FObjectName := Value;
 is_mapped := FALSE; // renamed
 Updated := TRUE;
end;

procedure TGameObject.SetParent(const Value: TGameObject);
begin
  if FParent = Value then exit;
  if FParent <> nil then
     FParent.RemoveChild (self);

  FParent := Value;
end;

procedure TGameObject.SetServInst(const Value: PByteArray);
var
   piv: PInteger;
   psv: PByteArray;
begin
 psv := value;

 if psv <> nil then
   begin
    piv := @psv [8];   // detect flag
    if piv^ = $447A0000 then // cast
            psv := @psv [SE_CAST_OFFSET];
   end;

 if psv <> FServInst then
   begin
    upd_loops := 0;
    FServInst := psv;
   end;
end;

procedure TGameObject.SetUpdated(const Value: Boolean);
begin
 if ( not FUpdated ) and Value then OnUpdate;
 FUpdated := Value;
end;

function TGameObject.se_addr(ofs: Integer): Pointer;
begin
 result := @pServInst [ofs];
end;

function TGameObject.se_field_i (ofs: Integer): Integer;
begin
 result := PInteger ( se_addr(ofs) )^;
end;

function TGameObject.se_field_w (ofs: Integer): WORD;
begin
 result := PWORD ( se_addr(ofs) )^;
end;

function TGameObject.story_id: WORD;
begin
 result := 0;
 if pServInst <> nil then
    result := self.se_field_w ( g_offsets.se_offsets.story_id );
end;


{ TEventInfo }

constructor TEventInfo.Create(id: WORD; pv: P3DVector4; v0: Single);
begin
 src_id := id;
 if pv <> nil then v3d := pv^;
 vlst [0] := v0;
 if g_timer = nil then
    ctm := Now
 else
    ctm := g_timer.GetTime;
end;


function MakeObjectsCache(prs: PRequestSlot): Integer;
const
   PRECREATE_OBJECTS = 25000;

var
   rr: TRootRegistry;
   ch: TObjectList;
   pt: TProfileTimer;
   el: Double;
    n: Integer;
begin
 pt := TProfileTimer.Create;
 try
   pt.StartOne (1);
   ch := TObjectList.Create (FALSE);
   ch.Capacity := PRECREATE_OBJECTS;
   ch.Count := PRECREATE_OBJECTS;
   for n := 0 to PRECREATE_OBJECTS - 1 do
       ch [n] := TGameObject.Create;

   rr := TRootRegistry ( prs.obj );
   rr.FCache := ch;
   el := pt.Elapsed (1);

   if el > 50 then
      ODS ( CFormat('[~T].~C0F #PERF_WARN: MakeObjectsCache time = %.3f ms, CPUTime = %.1f ms ~C07', '~C0F', [el, pt.CPUElapsed]) );
 finally
  pt.Free;
 end;
 result := 0;
end;

{ TRootRegistry }

function TRootRegistry.Add(gobj: TGameObject): Integer;
begin
 result := inherited Add (gobj);
 FDirect[gobj.id] := gobj;
 gobj.is_added := TRUE;
 gobj.FOwner := self;
 Inc (added_count);
end;

procedure TRootRegistry.CheckSections;
var
  gobj: TGameObject;
   tmp: TObjectRegistry;
    sc: String;
     n: Integer;
     i: Integer;
begin
 if ( Sections = nil ) or ( Sections.Count = 0 ) then exit;


 for n := 0 to Sections.Count - 1 do
  begin
   sc := Sections [n];
   tmp := TObjectRegistry ( Sections.Objects [n] );
   if tmp = nil then continue;

   for i := tmp.Count - 1 downto 0 do
    begin
     gobj := tmp [i];

     if ( gobj.section <> sc ) then
       begin
        PrintError ( Format (' object "%s" with section "%s" found in section storage "%s" ', [gobj.name, gobj.section, sc] ) );
        ODS ( ' object history dump:~C0F '#13#10 + FormatRight ( gobj.History.Text, #9#9 ) + '~C07' );
        tmp.Remove ( gobj );
       end;
    end;

  end;

end;

procedure TRootRegistry.Clear;
var
   n: Integer;
begin
 if not Assigned(self) then exit;
 ODS('[~T]. #DBG: Global registry clearing, context = ' + dbg_context);
 g_client_map_head := nil;
 g_server_map_head := nil;
 g_spawns_map_head := nil;
 destroy_queue := nil;
 objects_active := nil;
 objects_sleeping := nil;

 if by_name <> nil then by_name.Clear;
 ResetSpawnInfo;

 while Count > 0 do
       TGameObject(Last).Release('RootRegistry.Clear');

 if Assigned(Sections) then
   try
    for n := 0 to Sections.Count - 1 do
        TObjectRegistry ( Sections.Objects [n] ).Clear;

   except
    on E: Exception do
      PrintError('RootRegistry.Clear, exception catched while clearing Map. ');
   end;

 if log_verbose >= 7 then
    ODS('[~T]. #DBG(Clear): RootRegistry.Cache.Count = ' + IntToStr(FCache.Count));
end;

constructor TRootRegistry.Create(const ADesignation: String);
var
   prs: PRequestSlot;
begin
  if log_verbose >= 3 then
    ODS('[~T/~U/~B]. #DBG: RootRegistry initializing...');

 inherited Create (ADesignation);

 FIsRoot := TRUE;

 FCache := nil;


 Assert ( gToolThread <> nil, 'gToolThread = nil');
 prs := gToolThread.AddRequestEx('EXEC_CALLBACK');
 prs.obj := self;
 prs.callback := MakeObjectsCache;
 gToolThread.SendRequest (prs);


 FSections := TStrMap.Create (self);
 Sections.OwnsObjects := TRUE;
 Sections.Sorted := TRUE;

 by_name := TStrMap.Create (self);
 by_name.Sorted := TRUE;

 spawn_map := TStrMap.Create (self);
 spawn_reg := TStringList.Create;
 spawn_pos := TStrMap.Create (self);

 level_map := TStrMap.Create (self);

 FEvents := TStrMap.Create (self);
 FEvents.OwnsObjects := TRUE;

 Capacity := 32768;
 gToolThread.WaitRequests (100, FALSE);

 Enabled := FALSE;
 load_mask := $F;


 if log_verbose >= 3 then
    ODS('[~T/~U/~B]. #DBG: RootRegistry created ');
end;

destructor TRootRegistry.Destroy;
begin
 FCache.OwnsObjects := TRUE;
 FCache.Clear;
 OwnsObjects := TRUE;
 dbg_context := 'RootRegistry.Destroy';
 Clear;

 FreeListData (spawn_pos);

 FreeAndNil (FSections);
 FreeAndNil (FEvents);
 inherited;
 FreeAndNil (tmp_reg);
 FreeAndNil (tmp_reg2);
 FreeAndNil (spawn_map);
 FreeAndNil (spawn_reg);
 FreeAndNil (spawn_pos);
 FreeAndNil (level_map);
 FreeAndNil (by_name);
end;


procedure TRootRegistry.DebugDump;
var
   dump: TStrMap;
    obj: TGameObject;
      s: String;
      n: Integer;
begin
 dump := TStrMap.Create;
 for n := 0 to High (FDirect) do
  begin
   obj := FDirect [n];
   if obj = nil then continue;

   if obj.pInstance <> nil then
    begin
     s := Format( '$%p;cl_obj;%d;%s;%s;%d;%d', [obj.pInstance, n, obj.section, obj.name, obj.lvid, obj.gvid] );
     dump.Add (s);
    end;
   if obj.pServInst <> nil then
    begin
     s := Format( '$%p;se_obj;%d;%s;%s;%d;%d', [obj.pServInst, n, obj.section, obj.name, obj.lvid, obj.gvid] );
     dump.Add (s);
    end;

  end;

 if dump.Count > 0 then
   try
    dump.Sort;
    s := gLogPath + 'game_objects.dump';
    if FileExists (s) then
        DeleteFile (s);
    dump.SaveToFile (s);
   except
    on E: EFCreateError do;
   end;

 dump.Free;
end;

function TRootRegistry.Find(id: WORD): TGameObject;
begin
 result := FDirect [id];
 if (result <> nil) and (result.id = id) then exit;
end;


function TRootRegistry.FindByName(sName: String): TGameObject;
var
   i: Integer;
begin
 i := Pos('*', sName);
 if i = 0 then
   begin
    i := by_name.IndexOf (sName);
    result := nil;
    if i >= 0 then
      begin
       result := TGameObject (  by_name.Objects [i] );
       if result = nil then
          PrintError( Format( 'GameObject %s found in by_name list at %d, but = nil', [sName, i] ) );
      end;
   end
 else
   begin
    result := nil;
    System.Delete ( sName, i, 1);
    i := by_name.FindSub (sName);
    if i >= 0 then
       result := TGameObject ( by_name.Objects[i] );
   end;
end;



function TRootRegistry.FindBySpawnID(spawn_id: WORD): TGameObject;
var
   id: WORD;
begin
 result := nil;
 id := spawn_index.spid_game [spawn_id];
 if id = WRONG_IDX then exit;
 result := Find (id);
end;

function TRootRegistry.FindObjectByPtr(p: Pointer; back, fwd: DWORD): String;
var
  gobj: TGameObject;
  best: Integer;
   ofs: Integer;
   dst: DWORD;
     n: Integer;


  procedure DoTest (const prefix: String; tst: DWORD; var res: String);
  var
     lb, rb: DWORD;
  begin
    lb := tst - back;
    rb := tst + fwd;
    if (lb > dst) or (dst > rb) then exit;

    ofs := Integer (dst) - Integer (tst);

    if Abs (ofs) > best then exit;
    res := prefix + IntToStr(gobj.id) + ':' + gobj.name;
    if ofs > 0 then
       res := res +  ' + $' + IntToHex (ofs, 4);
    if ofs < 0 then
       res := res +  ' - $' + IntToHex (ofs, 4);
  end;

begin
 result := '';

 dst := DWORD (p);  // sample

 for n := 0 to Count - 1 do
  begin
   gobj := Objects [n];
   if gobj.pServInst <> nil then
      DoTest ('SE_OBJ_', DWORD(gobj.pServInst), result);
   if gobj.pInstance <> nil then
      DoTest ('CL_OBJ_', DWORD(gobj.pInstance), result);
  end;

end;

function TRootRegistry.GetEntityName(spawn_id: WORD): String;
begin
 result := '';
 if spawn_objs[spawn_id] <> nil then
    result := EntityName (spawn_objs[spawn_id]);
end;

function TRootRegistry.GetFiltered (lst: TStrMap; L: lua_State; tbidx: Integer ): TObjectRegistry;
var
   nf, n, svc, cnt: Integer;
   gobj: TGameObject;
   flt: TRQFilter;
    id: WORD;
     s: String;
begin
 if tmp_reg = nil then
    tmp_reg := TObjectRegistry.Create ('Temporary');

 if tmp_reg2 = nil then
    tmp_reg2 := TObjectRegistry.Create ('Temporary2');

 result := tmp_reg;

 result.Clear;

 // начальная загрузка списка объектов перед фильтрацией, или из параметра InvokeRegistry .....
 if ( L <> nil ) and lua_istable (L, tbidx) then
  begin
   lua_pushnil (L); // result and first key
   // parsing ids table
   while lua_next (L, tbidx) <> 0 do
     begin
      id := lua_tointeger (L, -1); // -1 value, -2 key
      if ( id < WRONG_IDX ) then
           result.Add ( self.Find (id) );
      lua_pop (L, 1);
     end;

  end
 else
  // ...... или все объекты реестра
   StoreObjects (result);

 if log_verbose >= 5 then
    wprintf('[~T/~B]. #DBG: Using filters %s for %d objects ', [lst.CommaText, result.Count]);


 // отсев не проходящих через фильтры
 for nf := 0 to lst.Count - 1 do
  begin
   tmp_reg2.Clear;
   flt := TRQFilter ( lst.Objects [nf] );

   if log_verbose >= 5 then
   with flt do
    begin
     s := '';
     if sp_count > 0 then s := #9'~C0F s_params =~C0A ';
     for n := 0 to sp_count - 1 do s := s + '<' + s_params [n] + '> ';
     if fp_count > 0 then s := s + #13#10#9'~C0F f_params =~C0D ';
     for n := 0 to fp_count - 1 do s := s + ftow ( f_params [n], '%.3f' ) + ' ';
     if ip_count > 0 then  s := s + #13#10#9'~C0F i_params =~C0D ';
     for n := 0 to ip_count - 1 do s := s + IntToStr ( i_params [n] ) + ' ';
     ODS ('[~T/~B].~C0E #DBG: filter dump for ~C0F' + flt.what + '~C07:'#13#10 + s + '~C07');
    end;

   svc := 0;
   cnt := result.Count;
   for n := cnt - 1 downto 0 do
    begin
     gobj := result.Objects [n];

     if gobj.is_online then
        gobj.LoadClient (FALSE)
     else
        gobj.LoadServer (FALSE);


     if IsFiltered (gobj, flt) then
        Inc (svc)
     else
        tmp_reg2.Add (gobj);
    end; // for

   if log_verbose >= 5 then
     begin
      wprintf ('[~T/~B]. #DBG: by filter %s sieved %d from %d', [flt.what, svc, cnt] );
      if flt.what = 'distance_xz' then
         wprintf(' GameObject.position offset = $%x', [g_offsets.position]);
     end;

   result.Clear;
   tmp_reg2.StoreObjects (result);
  end; // for
end;  // GetFiltered

function TRootRegistry.GetFull: TObjectRegistry;
var
   gobj: TGameObject;
      n: Integer;
begin
 if tmp_reg = nil then
    tmp_reg := TObjectRegistry.Create ('Temporary');
 result := tmp_reg;
 result.Clear;
 for n := 0 to Count - 1 do
  begin
   gobj := Objects[n];
   if gobj.bad_mark = 0 then
      result.Add(gobj)
  end;

end;



function TRootRegistry.GetMapped(const section: String): TObjectRegistry;
var
   i: Integer;
begin
 result := nil;
 if Assigned (Sections) then
   begin
    i := Sections.IndexOf (section);
    if i >= 0 then
       result := TObjectRegistry ( Sections.Objects [i] );
   end;
end;

procedure TRootRegistry.GetStoryIDMap(L: lua_State);
var
   esid: Integer;
      i: Integer;
      e: PByteArray;
      n: PAnsiChar;
begin
 lua_createtable (L, 0, 0);
 for i := 0 to $FFFE do
  begin
   e := spawn_objs [i];
   if e = nil then continue;
   CopyMemory(@n, @e[g_offsets.se_offsets.sz_name], sizeof(Pointer));
   if n = nil then continue;
   if ( AnsiTrim2W (n) = '' )  then continue;
   esid := int_field(e, g_offsets.se_offsets.story_id);
   if esid >= 0 then
    begin
     lua_pushinteger (L, esid);
     lua_setfield (L, -2, n);
    end; // if >= 0
  end; // for
end;

function TRootRegistry.IsFiltered(gobj: TGameObject; flt: TRQFilter): Boolean;
const
  P2D = 180 / Pi;
var
   ang, ref, tresh, dx, dz, dist: Single;
                             tmp: Integer;
                              pv: P3DVector4;

begin
 result := FALSE;

 if not gobj.is_exists then
  begin
   if log_verbose > 8 then
      ODS('[~T].~C0C #WARN:~C07 objects filtered as not existed');
   result := TRUE;
   exit;
  end;


 with flt do
  begin
   if flt.what = 'sc_mask' then
      result := ( Pos ( flt.s_params [0], gobj.section ) = 0 )
   else

   if flt.what = 'section' then
      result := ( flt.s_params [0] <> gobj.section )
   else

   if flt.what = 'distance_xz' then
     begin
      pv := gobj.position;
      dist := DistancePt ( f_params [0], f_params [1], pv.x, pv.z );
      result := ( dist > f_params [2] );
      if ( log_verbose > 8 ) and ( result xor ( log_verbose and 1 = 0) ) then
         wprintf ('[~T]. #DBG: distance_xz from [ %.2f, %.2f ] to %s [ %.2f, %2f ], dist = %.3f ',
                [ f_params[0], f_params [1], gobj.name, pv.x, pv.z, dist] );

     end
   else
   if flt.what = 'sector' then
     begin
      pv := gobj.position;;

      dx := (pv.x - f_params [0]);  // distance x
      dz := (pv.z - f_params [1]);  // distance z

      dist := Sqrt ( dx * dx + dz * dz );

      if dist <= 0 then exit; // in point == not filtered

      dx := dx / dist;
      dz := dz / dist;

      ang := arccos (dz); // who Z is horizontal???


      if dx <= 0 then ang := Abs (ang) else ang := -Abs(ang);

      ang := AngNormalize ( ang ); // revert direction

      ref := AngNormalize ( f_params [2] );

      tresh := f_params [3];

      if log_verbose >= 8 then
         ODS( CFormat ('[~T]. #DBG: dx = %.5f, dz = %.5f, angle = %.1f, ref = %.1f, tresh = %.1f ', '~C07', [dx, dz, ang * P2D, ref * P2D, tresh * P2D]) );

      dz := AngNormalize ( ang - ref );

      result := ( Abs ( dz ) > tresh ); // angle diff below treshold
     end
   else
   if flt.what = 'clsid_range' then
     begin
      if ( Pos('respawn', gobj.Name) > 0 ) then
          __nop;
      tmp := gobj.class_id;


      if tmp = 0 then
         ODS( CFormat( '[~T].~C0C #WARN(TRootRegistry.IsFiltered):~C07 object %5d name %s clsid = 0 ', '~C07', [gobj.id, gobj.name] ) );

      if i_params[0] > i_params[1] then
         begin
          if log_verbose >= 7 then
             wprintf('[~T]. #DBG(TRootRegistry.IsFiltered): swapping range params %d and %d', [i_params[0], i_params[1]]);
          tmp := i_params[1];
          i_params[1] := i_params[0];
          i_params[0] := tmp;
         end;



      result := ( tmp < i_params [0] ) or ( tmp > i_params [1] );

      if ( log_verbose >= 10 ) and result then
         wprintf('[~T]. #DBG(TRootRegistry.IsFiltered): name = %-30s, range = [%d, %d], value = %d, online = %s',
                  [gobj.Name, i_params[0], i_params[1], tmp, IfV(gobj.pInstance = nil, 'no', 'yes')]);

     end
   else
   if flt.what = 'gvid_range' then
     begin
      if ( gobj.id = 0 ) then
           __nop;

      tmp := gobj.game_vertex_id;
      result := ( tmp < i_params [0] ) or ( tmp > i_params [1] );
     end
   else
   if flt.what = 'has_parent' then
      result := ( gobj.parent_id = $FFFF );

  end; // with

end; // IsFiltered



function TRootRegistry.JoinSections(sc: String): TObjectRegistry;
var
   n: Integer;
   r: TObjectRegistry;
begin
 if Sections = nil then
   begin
    PrintError('JoinSections: Sections storage not exists');
    result := nil;
    exit;
   end;

 if log_verbose >= 7 then
    ODS('[~T]. #DBG: Trying join sections for mask~C0A ' + sc + '~C07 from: ~C0F'#13#10#9 + Sections.CommaText + '~C07');

 if Pos('*', sc) = 0 then
  begin
   result := Mapped[sc];
   exit;
  end;

 sc := AnsiReplaceStr(sc, '*', '');

 if tmp_reg = nil then
    tmp_reg := TObjectRegistry.Create ('Temporary');


 result := tmp_reg;
 result.Clear;

 for n := 0 to Sections.Count - 1 do
  if Pos (sc, Sections[n]) > 0 then
    begin
     r := TObjectRegistry ( Sections.Objects [n] );
     r.StoreObjects (result);
    end;
end;

function  TRootRegistry.LevelName ( gvid: WORD ): String;
var
   level_id: WORD;
begin
 result := '?';
 level_id := level_index[gvid];
 if (level_id > 0) and (level_id < level_map.Count) then
     result := level_map[level_id];
end;


function  TRootRegistry.NameIndex( const nm: String ): Integer;
begin
 result := by_name.IndexOf ( nm )
end;

function TRootRegistry.ObjectName(id: WORD): String;
var
   gobj: TGameObject;
begin
 gobj := Find (id);
 if gobj <> nil then
    result := gobj.name
 else
    result := 'not_found';
end;

function  TRootRegistry.SectionIndex( const sc: String ): Integer;
begin
 result := Sections.IndexOf ( sc )
end;

procedure TRootRegistry.SetLevelInfo(const lname: String; level_id, gvid_min, gvid_max: WORD);
var
   gvid: WORD;
begin
 while level_map.Count <= level_id do  level_map.Add('?');
 level_map[level_id] := lname;
 for gvid := gvid_min to gvid_max do
     level_index[gvid] := level_id;
end;



function TRootRegistry.SpawnEntityVerify(gobj: TGameObject): Boolean;
var
  osid: WORD;
  esid: WORD;
  ocid: WORD;
  ecid: WORD;
  pswi: PWORD;
   qnm: String;
     s: String;
     e: PByteArray;

     i: Integer;



begin
 result := FALSE;
 e := spawn_objs [gobj.spawn_id];
 s := EntityName (e);
 if ( s = '' ) then exit;
 if gobj.Name = s then
   begin
    Inc(ok_count); exit;
   end;

 if ( gobj.Name <> s ) and ( gobj.Name <> 'single_player' ) and ( gobj.Name <> 'respawn' ) then
 with g_offsets do
   begin
    i := FindSpawnID (gobj.Name, gobj.gvid);
    pswi := gobj.se_addr (se_offsets.spawn_id);
    if (i > 0) then
      begin
       i := i and WRONG_IDX;
       e := spawn_objs [i];
       Assert ( EntityName(e) = gobj.Name, Format('Found entity name = %s, object name = %s. WTF??? ',
                                [EntityName(e), gobj.Name] ));

       if i mod 100 = 0 then
          wprintf('[~T].~C0C #WARN:~C07 found better entity spawn_id = %5d vs %5d, entity name %s <> object name %s ',
                                   [i, gobj.spawn_id, s, gobj.Name]);


       gobj.UpdateSpawnID(i);
       result := TRUE;
       exit;
      end
    else
      begin
       wprintf ('[~T].~C0C #WARNING:~C07 spawn_entity #%5d name = %35s <> object name = %35s. Probably all.spawn has newer version? Reseting .spawn_entity to $FFFF ',
                                        [gobj.spawn_id, s, gobj.Name]);
       ecid := word_field(e, se_offsets.clsid);
       esid := word_field(e, se_offsets.story_id);

       if Assigned (gobj.pServInst) then
         begin
          ocid := word_field(gobj.pServInst, se_offsets.clsid);
          osid := word_field(gobj.pServInst, se_offsets.story_id);

          wprintf('             entity story_id = %5d, object story_id = %d ', [esid, osid]);
          wprintf('             entity class_id = %5d, object class_id = %d ', [ecid, ocid]);
          wprintf('             entity g_vx_id  = %5d, object g_vx_id  = %d ', [word_field(e, se_offsets.gvx_id), word_field(gobj.pServInst, se_offsets.gvx_id)]);
         end;

       gobj.UpdateSpawnID(WRONG_IDX);
       result := FALSE;
      end;

    collapse_time := PreciseTime + 90 * DT_ONE_SECOND;
    exit;
   end;


 result := TRUE;
end;

procedure TRootRegistry.LoadSpawnPositions;
var
   fini: TIniFile;
   tmpf: String;
   flds: TStrMap;
    tmp: TStrMap;

//   rq: TVxRequest;

     sc: String;
     sv: String;
     wb: Integer;
     sp: PSpawnPos;
     gv: WORD;
      f: File;
      r: IReader;
      c: PAnsiChar;

begin
 XrRescanPathes;
 fini := nil;
 flds := nil;
 r := XrFileOpen('$game_config$', 'spawn_pos.ltx');
 if r <> nil then
   try
    c := PAnsiChar (r.data);
    ODS('[~T/~B]. #PERF: loading spawn points start...');
    // копирование контента в временный файл, парсинг поз иций
    tmpf := ExpandPath ('$mod_dir$\spawn_pos.ltx');
    if FileExists(tmpf) then
       DeleteFile(tmpf);
    {$I-}
    AssignFile (f, tmpf);
    if IOresult = 0 then
       ReWrite (f, 1);
    wb := 0;
    if IOresult = 0 then
      begin
       BlockWrite (f, r.data^, r.size, wb);
       CloseFile (f);
      end;

    fini := nil;
    tmp  := nil;
    if wb = r.size then
      try
       tmp  := TStrMap.Create();
       flds := TStrMap.Create();
       tmp.LoadFromFile(tmpf);


       wprintf('[~T/~B]. #DBG: from~C0F spawn_pos.ltx~C07 loaded %d lines, file size = %d',
                                        [tmp.Count, r.size]);

       Repeat
          flds.Clear;
          sc := tmp.NextSection(FALSE, flds);
          if sc = '' then break;
          // TODO: here may-be section mistake
          gv := flds.IntValues['game_vertex_id'];
          sv := flds.Values['position'];

          if (gv >= 10000) or ( level_index[gv] <> active_level ) or (sv = '') then
            begin
             // wprintf('[~T].~C0C #WARN(LoadSpawnPositions):~C07 skipped section %s', [sc]);
             continue;
            end;

          sp := AllocMem (sizeof(TSpawnPos));
          spawn_pos.AddObject (sc, Pointer(sp));


          sp.init_pos( Trim(sv) );
          sp.gvid := gv;
          sp.lvid := flds.IntValues['level_vertex_id'];
          Assert (sp.gvid < 10000, 'To large game vertex for ' + sc + ' = ' + IntToStr(sp.gvid));
          { // добавить выявление уровня
          rq.init(1000);
          rq.vectors[0].init (sp.x, sp.y, sp.z);
          rq := FindNearestVxAsync ( @rq );
          }
       Until FALSE;

       spawn_pos.Sort;
       spawn_pos.Sorted := TRUE;

       // if sv <> '' then
       wprintf('[~T/~B]. #DBG: after sieve rest %d spawn points', [spawn_pos.Count]);

       // wprintf('[~T]. #DBG: spawn_pos.ltx build timestamp = %s ', [sv]);

      finally
       fini.Free;
       flds.Free;
       tmp.Free;
      end;

    // DeleteFile (tmpf);

    XrFileClose(r);
   except
    on E: Exception do
       PrintError('Exception catched in TRootRegistry.LoadSpawnPositions ' + E.Message);

   end

end;

function TRootRegistry.MakeObject(id: WORD): TGameObject;
begin
 // генерация нового универсального объекта или извлечение из кэша
 result := Find (id);
 if ( result = nil ) and IsRoot and ( FCache <> nil ) and ( FCache.Count > 0 ) then
  begin
   result := TGameObject (FCache.Last);
   FCache.Delete ( FCache.Count - 1 );
  end;
 if result = nil then
    result := TGameObject.Create;
 result.Reset ('MakeObject');
 result.used := TRUE;
 result.id := id;
 result.is_exists := TRUE;
end;



procedure TRootRegistry.MapObject(gobj: TGameObject; bMap: Boolean );
var
  sreg: TObjectRegistry;
   dsg: String;
     i: Integer;
begin
 if gobj.bad_mark > 0 then exit;

 dsg := gobj.name;

 if ( not gobj.is_mapped ) and ( dsg <> '' ) and ( dsg <> '#reseted' ) and ( Pos('#noname_', dsg) = 0 ) and bMap then
   begin
    i := NameIndex (dsg);
    if i < 0 then
       by_name.AddObject (dsg, gobj)
    else
       by_name.Objects [i] := gobj;

    gobj.is_mapped := TRUE;
   end;

 FDirect[gobj.id] := gobj;

 if ( gobj.spawn_id < WRONG_IDX ) and  SpawnEntityVerify (gobj) and bMap  then
      gobj.UpdateSpawnID (gobj.spawn_id);

 if ( Sections = nil ) then
   begin
    PrintError('MapObject failed, Sections = nil');
    exit;
   end;
 dsg := Trim (gobj.section);
 if ( dsg = '' ) or ( dsg = '#reseted' ) then exit;

 if ( gobj.sc_registry <> nil ) and ( gobj.sc_registry.Designation = dsg ) and bMap then exit;

 i := SectionIndex ( dsg ) ;
 if i < 0 then
    begin // регистрация нового суб-реестра
     sreg := TObjectRegistry.Create (dsg);
     Sections.AddObject ( dsg, sreg ); // зарегистрировать секцию

     if log_verbose > 7 then
        ODS('[~T/~U/~B]. #DBG: Created registry ~C0A' + sreg.Designation + '~C07. Initial object ~C0A' + gobj.name + '~C07' );
    end
 else
    sreg := TObjectRegistry ( Sections.Objects [i] );

 if bMap then
   begin
    if sreg.Find ( gobj.id ) = nil then
       sreg.Add ( gobj );
    gobj.sc_registry := sreg;
   end
 else
  if gobj.sc_registry <> nil then
   begin
    gobj.sc_registry.Remove(gobj);
    gobj.sc_registry := nil;
   end;

end;

procedure TRootRegistry.MarkObjectBad(id: Integer);
var
   gobj: TGameObject;
begin
 if ( id >= 0 ) and ( id < $FFFF ) then else exit;

 gobj := self.Find ( WORD(id) );

  if gobj <> nil then
    begin
     if log_verbose >= 7 then
        ODS( CFormat('[~T]. #DBG: marking bad #%d / %s [%s], stable = %d, from thread~C0D #~I~C07', '~C07',
                        [id, gobj.name, gobj.section, gobj.stable] ));

     if Assigned ( gobj.FChilds ) then
                   gobj.FChilds.Clear;

     if Assigned ( gobj.sc_registry ) then
        MapObject (gobj, FALSE);

     // by_name.Remove (gobj.name);
     gobj.Log ('marked bad from ' + gobj.name);
     gobj.bad_mark := 15;
     gobj.is_exists := FALSE;
     gobj.stable := 0;
     gobj.pInstance := nil;
     gobj.pServInst := nil;
    end;

end;

function TRootRegistry.MemUsage: DWORD;
begin
 result := inherited MemUsage + Events.MemUsage;
end;

procedure TRootRegistry.ProcessEvents(L: lua_State);
var
   n, first, i, argc, err: Integer;
   pt: TProfileTimer;
   evt: TEventInfo;
   gobj: TGameObject;
   s: String;


begin

 if Events.Count = 0 then exit;

 pt := TProfileTimer.Create;
 first := lua_gettop (L);

 for n := 0 to Events.Count - 1 do
  begin
   argc := 2; // evt, table of params

   s := Events [n];

   evt := TEventInfo ( Events.Objects  [n] );

   gobj := Find ( evt.src_id );
   if ( gobj = nil ) or ( gobj.event_func = '' ) then continue;

   err := 0;
   if Assigned (g_panic) then
     begin
      lua_pushcfunction (L, g_panic);
      err := lua_gettop(L);
     end;

   // lua_pushwstr (L, event_func);         // idx = 1
   i := lua_gettop(L);

   // lua_getglobal (L, event_func);
   lua_getglobalfunc (L, gobj.event_func);


   if i = lua_gettop(L) then
     begin
      PrintError(ClassName + '.ProcessEvents Not found event_func ' + gobj.event_func);
      lua_settop (L, first);
      break;
     end;




   if Assigned (gobj.event_obj) then
     begin
      lua_pushlightuserdata(L, gobj.event_obj);
      Inc (argc);
     end;


   lua_pushwstr (L, s); // код события

   // составление таблицы параметров-свойств

   lua_createtable (L, 0, 0);
   lua_setnvalue (L, 'id', gobj.id);
   lua_setnvalue (L, 'parent_id', gobj.parent_id);
   lua_setsvalue (L, 'time', FormatDateTime ('dd.mm.yy hh:nn:ss.zzz', evt.ctm));


   for i := 0 to High (evt.vlst) do
       lua_setnvalue (L, 'v' + IntToStr(i), evt.vlst  [i] );

   lua_setnvalue (L, 'x', evt.v3d.x);
   lua_setnvalue (L, 'y', evt.v3d.y);
   lua_setnvalue (L, 'z', evt.v3d.z);

   lua_pcall (L, argc, LUA_MULTRET, err);
   lua_settop (L, first);
  end;


 if ( Events.Count > 1000 ) or ( pt.Elapsed > 25 )  then
   ODS('[~T]. #PROF(ProcessEvents): Events.Count = ' + IntToStr(Events.Count) + ', time = ' + ftow(pt.elapsed, '%.2f ms'));

 Events.Clear;
 pt.Free;

end;

procedure TRootRegistry.Remove(gobj: TGameObject);
var
   i, lcnt: Integer;


begin
 lcnt := Count;
 FDirect [gobj.id] := nil;

 i := FindPosition ( gobj.id, nil, nil );
 if i < 0 then
    inherited Remove ( gobj )
 else
    Delete (i);
 by_name.Remove ( gobj.Name );
 SaveReleased ( gobj.id, gobj, TRUE );

 if ( not OwnsObjects ) and ( FCache.IndexOf (gobj) < 0 ) then
      FCache.Add ( gobj );
 FCountUpdated := (Count <> lcnt);
end;



procedure TRootRegistry.RemoveBad;
var
   n: Integer;
begin
 for n := Count - 1 downto 0 do
 with Objects[n] do
  if ( 1 = bad_mark ) or
     ( pInstance = nil ) and ( pServInst = nil )
       then
            Remove ( Objects [n] );

end;

procedure TRootRegistry.ResetSpawnInfo;
begin
 alife_spawns := nil;
 g_spawns_map_head := nil;

 if Assigned (spawn_map) then
              spawn_map.Clear;
 FillChar ( spawn_objs,  sizeof(spawn_objs),   $00 );
 FillChar ( spawn_index, sizeof (spawn_index), $FF );
end;

function TRootRegistry.RegCLObject (id: WORD; obj: Pointer): Boolean;
var
   gobj: TGameObject;
begin
 result := FALSE;
 if id = WRONG_IDX then
    id := ReadWORD (obj, g_offsets.id_word);

 if removed_evts [id] > 0 then exit;

 gobj := RegObject (id);
 gobj.pInstance := obj;
 gobj.active := scan_active;
 if client_objects <> nil then
    gobj.client_ref := @client_objects[id];

 result := gobj.VerifyID (TRUE);
 if result then
    Inc (scan_found)
 else
    ODS('[~T].~C0C #ERROR:~C07 RegCLObject, VerifyID failed - breaking scan');
 { if Count < 100 then
    wprintf('[~T]. #DBG: To registry added CObject = %p, id = %d ', [obj, id]);}
end;

function TRootRegistry.RegSEObject (id: WORD; obj: Pointer): Boolean;
var
   gobj: TGameObject;
begin
 result := TRUE;
 if (Count = 0) and ( removed_evts [id] > 1 ) then
    begin
     wprintf('[~T].~C0C #WARN:~C07 se-object %5d not added due removed_evts = $%x ', [id, removed_evts[id]]);
     exit;
    end;

 if ( id = WRONG_IDX ) and ( obj <> nil ) then
      id := ReadWORD (obj, g_offsets.se_offsets.id_word);
 ASSERT (id < WRONG_IDX, 'invalid id for se-object ' + FormatPtr(obj));
 Inc (scan_found);
 gobj := RegObject (id);
 gobj.pServInst := obj;

 if gobj.upd_loops and 15 = 0 then
    result := gobj.VerifyID (FALSE);

 if not result then
   begin
    ODS('[~T].~C0C #ERROR:~C07 RegSEObject, VerifyID failed - breaking scan');
    alife_objects := nil;
    g_server_map_head := nil;
    g_offsets.IsCorrect(TRUE); // try reload
   end;

end;


function  TRootRegistry.FindSpawnID(const Name: String; gvid: WORD): WORD;
var
   lvl: WORD;
   qfn: String;
   res: DWORD;
begin
 lvl := level_index[gvid];
 qfn := Name + ':lv' + IntToStr(lvl);
 res := DWORD (spawn_map.FindObject(qfn));
 if res = 0 then
    res := DWORD (spawn_map.FindObject(Name));

 result := WORD(res);
end;

procedure TRootRegistry.TestSpawnPos(name: String; E: PByteArray);
var
   sp: PSpawnPos;
   vf: PVariantField;
   gv: WORD;
   lv: DWORD;

begin
 if name = 'val_koster_21' then
   asm
    nop
   end;

 sp := Pointer ( spawn_pos.FindObject (name) );
 if sp = nil then exit;
 with g_offsets do
  begin
   vf := FieldPtr (E,   se_offsets.position);
   gv := word_field (E, se_offsets.gvx_id);
   lv := dword_field (E, se_offsets.lvx_id);
   if ( sp.distance(vf.v4) > 0.1 ) or ( gv <> sp.gvid ) or ( lv <> sp.lvid ) then
    begin
     wprintf('[~T].~C0C #SPAWN_POS:~C07 %-25s spawn_pos = %8.3f, %8.3f, %8.3f, gvid = %5d, lvid = %d ',
              [name, sp.x, sp.y, sp.z, sp.gvid, sp.lvid]);

     wprintf('[~T].~C0C #SPAWN_POS:~C07 %-25s entity    = %8.3f, %8.3f, %8.3f, gvid = %5d, lvid = %d ',
              [name, vf.v4.x, vf.v4.y, vf.v4.z, gv, lv]);

      vf.v4.x := sp.x;
      vf.v4.y := sp.y;
      vf.v4.z := sp.z;
      FieldPtr (E, se_offsets.gvx_id).u16 := gv;
      FieldPtr (E, se_offsets.lvx_id).u32 := lv;
    end;
  end; //
end;

procedure TRootRegistry.MapEntity (E: PByteArray; spawn_id, level_id: WORD);
var
   n, qfn: String;
      tmp: TGameObject;
begin
  n := EntityName (E);
 //  if spawn_id = 7900 then   wprintf('          spawn_entity %5d = %s ', [spawn_id, n]);

  tmp := nil;
  if n <> '' then
    begin
     qfn := n + ':lv' + IntToStr(level_id);
     if spawn_reg.IndexOf(n) >= 0 then
        PrintError('Already registered spawn_entity  ' + n + ', added level suffix!')
     else
       begin
        spawn_map.AddObject ( n, Pointer (spawn_id + DWORD(level_id) shl 16));
        spawn_reg.Add (n);
       end;

     TestSpawnPos (n, E);
     spawn_map.AddObject ( qfn, Pointer (spawn_id + DWORD(level_id) shl 16));
    end
  else
   try
    tmp := TGameObject.Create;
    tmp.FOwner := self;
    tmp.pServInst := E;
    tmp.LoadServer(TRUE);
    wprintf ('[~T].~C0C #WARN:~C07 spawn entity #%d name == nil, section = %s, gv = %d, lv = %d, pos = %s ',
                        [spawn_id, tmp.section, tmp.gvid, tmp.lvid, tmp.se_position.format]);
   finally
    tmp.Free;
   end;
end;

function TRootRegistry.RegEntity (id: WORD; obj: Pointer): Boolean;
var
        wrp: PServerEntityWrapper;
        // typedef CGraphAbstractSerialize<*,float,ALife::_SPAWN_ID>	SPAWN_GRAPH;

        ptr: PSGraphVertex;
        tmp: WORD;
   spawn_id: WORD;
   level_id: WORD;
begin
 spawn_id := id;
 ptr := obj;
 result := Assigned (ptr);
 if not result then exit;

 wrp := ptr.GetWrapper(g_offsets.se_offsets.CALifeSpawnReg.wrapper);


 if ( wrp <> nil ) then
 with g_offsets, wrp^ do
  begin
   if ( m_entity <> nil ) then
     begin
      tmp := word_field(m_entity, se_offsets.gvx_id);
      level_id := level_index[tmp];
      if level_id = 0 then
         level_id := 1;

      Inc (scan_found);
      // если прототип объекта существует в спавне
      MapEntity ( m_entity, spawn_id, level_id );
      if ( log_verbose > 4) and ( spawn_id < 100 ) then
           wprintf ('sid = %5d, obj = $%p, factory = $%p, p_object = $%p',
                     [spawn_id, ptr, wrp, m_entity]);

      tmp := word_field(m_entity, se_offsets.story_id);
      if tmp < WRONG_IDX then
         spawn_index.story_spid [tmp] := spawn_id;
     end;
   spawn_objs [spawn_id] := m_entity;   // spawn_id <> game_id (!)
  end;

end;



function  TRootRegistry.RegObject;
begin
 // result := nil;
 result := FDirect[id];

 if result = nil then
    begin
     result := MakeObject (id);
     result.added_by := scan_idx;
     result.last_pid := WRONG_IDX;
     Inc (heavy_ops);
     Inc (reg_events);
     Add (result);
    end;

 if ( removed_evts [id] = 1 ) and ( log_verbose >= 7 ) then
      wprintf(' #REG_OBJ: id = %d, re-occupied by object %s ', [id, result.Name]);

 // removed_evts [id] := removed_evts [id] shr 1; // to zero
 result.is_exists := TRUE;
 result.scan_frame := scan_frame;
 result.found_time := scan_time;
 Inc (result.stable);
end;


procedure TRootRegistry.ScanVector (vector: PXrVector);
var
   cnt: Integer;
    sz: DWORD;
     p: PPointer;
begin
 p := vector.start;
 if IsBadReadPtr(p, 4) then exit;
 cnt := 0;
 if NativeUInt(p) < vector.laddr then
 try
  sz := 1 + vector.laddr - NativeUInt(p);
   if (upd_loop < 5) and (log_verbose >= 3) and (sz > 100) then
       wprintf('[~T/~B]. #DBG: ScanVector start = $%p, last = $%p, end = $%p ', [vector.start, vector.last, vector._end]);

   while NativeUInt(p) < vector.laddr do
    begin
     g_addr_max := Max (g_addr_max, DWORD(p^));
     g_addr_min := Min (g_addr_min, DWORD(p^));
     if not scan_cb ($FFFF, p^) then break;
     Inc (cnt);
     Inc (p);
    end;
 except
   on E: Exception do
      OnExceptLog(ClassName + '.ScanVector', E, TRUE);
 end;

 if (upd_loop < 5) and (log_verbose >= 3) and (cnt > 100) then

     wprintf('[~T/~B]. #DBG: ScanVector processed %d objects ', [cnt]);
end;

function TRootRegistry.UnregCLObject (id: WORD; obj: Pointer): Boolean;
var
   gobj: TGameObject;
begin
 if (id = WRONG_IDX) and (obj <> nil) then
    id := ReadWORD (obj, g_offsets.id_word);

 ASSERT (id < WRONG_IDX, 'UnregCLObject: id = WRONG_IDX');

 gobj := self.Find(id);
 if gobj <> nil then
   begin
    gobj.pInstance := nil;
    gobj.client_ref := nil;
    if gobj.pServInst = nil then
       gobj.Release('UnregCLObject');
   end;
 result := TRUE;
end;

{$DEFINE CHECK_PERF}
procedure TRootRegistry.ScanObjects;
var
   gobj: TGameObject;
   diff: Integer;
     ff: Boolean;
      n: Integer;
      i: WORD;
   elps: array [0..7] of Double;
      pt: TProfileTimer;
begin
 // if g_server_map_head = nil then exit;
 if not ( Enabled and CheckPointersValid ) then exit;
 if (not bFast) then
   begin
    g_addr_max := 0;
    g_addr_min := DWORD(-1);
   end;

 pt := FDispTimer;
 pt.StartOne(2);
 pt.StartOne(3);
 for i := 0 to High(elps) do elps[i] := 0;

 try
  Inc (scan_frame);
  scan_time := PreciseTime;

  if map_NETID <> nil then
     g_client_map_head := map_NETID.GetHead ('map_NETID');

  if alife_objects <> nil then
     g_server_map_head := alife_objects.GetHead ('alife_objects');

  if alife_spawns <> nil then
     g_spawns_map_head := alife_spawns.GetHead ('alife_spawns');



  if not bFast then
  for n := 0 to Count - 1 do
     begin
      gobj := Objects [n];
      gobj.is_added  := FALSE;
      gobj.is_exists := FALSE;
     end;

  scan_idx := 2;
  scan_loop := 0;
  for n := 0 to High (removed_evts) do
        removed_evts[n] := removed_evts[n] shr 1;

  {$IFDEF CHECK_PERF}
  elps[0] := pt.Elapsed (2, TRUE);
  {$ENDIF}


  // заполнение спавн-карты
  ff := ( spawn_map.Count = 0 );
  if ( g_spawns_map_head <> nil ) and ff and ( g_offsets.se_offsets.CALifeSpawnReg.wrapper > 0 )  then
    begin
     if spawn_pos.Count = 0 then
        LoadSpawnPositions;


     spawn_map.Sorted := FALSE;
     spawn_reg.Sorted := TRUE;
     FillChar ( spawn_index, sizeof(spawn_index), $FF );

     if ff then  ODS('[~T/~B]. #DBG: updating spawn_map from root = ' + FormatPtr(g_spawns_map_head));


     scan_cb := RegEntity;
     FillChar (spawn_objs, sizeof(spawn_objs), 0);
     Traverse_Tree (g_spawns_map_head.parent, g_spawns_map_head);
     if ff then ODS('[~T/~B]. #DBG: sorting spawn_map. ');
     spawn_map.Sort;
     spawn_map.Sorted := TRUE;
     // обновление индекса имен
     for n := 0 to spawn_map.Count - 1 do
       begin
        i := spawn_map.I32Tags [n];
        spawn_index.names [i] := n;
       end;
     if ff then
        wprintf ('[~T/~B]. #DBG: complete spawn_map. Items = %d', [spawn_map.Count]);
    end;


  if ( g_game_build > 3312 ) and ( sizeof(TTreeMapNode) < 24 ) then
     raise ERegistryException.Create (  Format('sizeof(TTreeMapNode) = %d, for xr_3da build %d', [sizeof(TTreeMapNode), g_game_build]));

  if (scan_frame and $FFF = $F) then
     begin
      if Assigned(destroy_queue)    then wprintf('destroy_queue.length    = %d ', [destroy_queue.length]);
      if Assigned(objects_active)   then wprintf('objects_active.length   = %d ', [objects_active.length]);
      if Assigned(objects_sleeping) then wprintf('objects_sleeping.length = %d ', [objects_sleeping.length]);
     end;


  if ( g_server_map_head <> nil ) and ( not bFast ) then
   begin
    scan_idx := 1;
    scan_loop := 0;
    scan_found := 0;
    scan_cb := RegSEObject;
    Traverse_Tree (g_server_map_head.parent, g_server_map_head);
    {$IFDEF CHECK_PERF}
    elps[1] := pt.Elapsed (2, TRUE);
    {$ENDIF}
    if ( log_verbose >= 7 ) or ( scan_frame < 10 ) or ( elps[1] > 10 ) then
       wprintf('[~T/~U/~B]. #DBG: scan_found server objects = %d ', [scan_found]);

    Assert ( scan_found > 100, Format(' found little server objects < 100 from node $%p ', [g_server_map_head.parent]));
   end;


  scan_idx := 0;
  scan_loop := 0;
  scan_cb := RegCLObject;
  scan_active := TRUE;

  if ( objects_active <> nil ) and ( not bFast ) then
   begin
    // раздельное сканирование позволяет отделить активные объекты от спящих
    scan_found := 0;
    ScanVector (objects_active);
    if log_verbose >= 7 then
       wprintf('[~T/~B]. #DBG: scan_found active = %d ', [scan_found]);
    scan_active := FALSE;
    scan_found := 0;
    {$IFDEF CHECK_PERF}
    elps[2] := pt.Elapsed (2, TRUE);
    {$ENDIF}
    if ( not bFast ) and ( objects_sleeping <> nil )  then
       ScanVector (objects_sleeping);
    if log_verbose >= 7 then
       wprintf('[~T/~B]. #DBG: scan_found sleeping = %d ', [scan_found]);

   end
  else
  if g_client_map_head <> nil then
     Traverse_Tree (g_client_map_head.parent, g_client_map_head);
  {$IFDEF CHECK_PERF}
  elps[3] := pt.Elapsed (2, TRUE);
  {$ENDIF}

  if ( destroy_queue <> nil ) then
   begin
    scan_cb := UnregCLObject;
    ScanVector (destroy_queue);
   end;

  if not bFast then
  for n := Count - 1 downto 0 do
      begin
       gobj := Objects[n];
       diff := scan_frame - gobj.scan_frame;
       if ( diff = 0) then continue;

       i := gobj.id;
       if ( gobj.bad_mark = 0 ) and ( removed_evts [i] = 0 ) then
         begin
           wprintf('[~T].~C0C #WARN:~C07 object %-20s [%5d] not found in last_scan. Last found time %s. Scan_frame = %d < current %d. Releasing.',
                                    [gobj.Name, i, FormatDateTime('hh:nn:ss.zzz', gobj.found_time),
                                          gobj.scan_frame, scan_frame]);
           // MarkObjectBad (i);
         end;
       gobj.Release('Lost after scan');
      end;
  {$IFDEF CHECK_PERF}
  elps[4] := pt.Elapsed (2, TRUE);
  elps[7] := pt.Elapsed (3);
  if elps[7] > 15 then
     wprintf('[~T].~C0C #PERF_WARN(ScanObjects):~C07 %.1f + %.1f + %.1f + %.1f + %.1f = %.1f msec, ok_count = %d ',
                [elps[0], elps[1], elps[2], elps[3], elps[4], elps[7], ok_count]);
  {$ENDIF}

 except
  on E: Exception do
    begin
     PrintError('ScanObjects exception catched: ' + E.Message);
     if lreg_L <> nil then ODS(LuaTraceback (lreg_L,  '    '));
     OnExceptLog ('ScanObjects#0', E, TRUE);
    end;
 end;

end;


procedure TRootRegistry.SwitchParent (id, parent_id: WORD);
var
   gobj: TGameObject;
begin
 gobj := Find (id);
 if gobj = nil then exit;
 gobj.TestUpdateParent ( parent_id, 'SwitchParent' );
 if Pos('belt', gobj.Name) > 0 then
    wprintf('[~T]. #DBG: %s changed owner to %d', [gobj.Name, parent_id]);

end;

procedure TRootRegistry.TestRemoved;
var
   gobj: TGameObject;
      n: Integer;
begin
  for n := Count - 1 downto 0 do
    begin
     gobj := Objects [n];
     if ( gobj.bad_mark > 0 ) then
              Dec(gobj.bad_mark); // возможно ошибко

     if ( not gobj.is_exists ) then
              gobj.Release('ScanObjects/reject');
    end;

end;


function TRootRegistry.Traverse_Tree (node, head: PTreeMapNode): Boolean;
begin
 result := FALSE;

 if (node = head) or ( DWORD(node) < $1000 ) or IsBadReadPtr(node, 22) or node.is_nil then exit;
 Inc (scan_loop);
 if scan_loop >= $10000 then
    begin
     wprintf('[~T].~C0C #WARN:~C07 TRootRegistry.Traverse_Tree  abnormal scan_loop count = %d, breaking ', [scan_loop]);
     exit;
    end;

 // reqursive
 with node^ do
  begin
   Traverse_Tree (left, head);
   if key and WRONG_IDX <> WRONG_IDX then
     begin
      g_addr_max := Max (g_addr_max, DWORD(node.obj));
      g_addr_min := Min (g_addr_min, DWORD(node.obj));
      if not scan_cb (node.key and WRONG_IDX, node.obj) then
         begin
          PrintError( Format(' traversing breaked at node %d ', [node.key]) );
          exit;
         end;
     end;
   Traverse_Tree (right, head);
  end;
  result := TRUE;
end;





procedure TRootRegistry.Update;
var
   e0, e1, e2, e3, e4: Double;
          i, overhead: Integer;
                   pt: TProfileTimer;
                   tr: Double;
begin
 if not g_offsets.IsCorrect(TRUE) then
    begin
     wprintf ('[~T]. ~C0C #WARN(TRootRegistry.Update):~C07 offsets not correct. Config updated = %',
                 [FormatDateTime('dd-mm-yy hh:nn:ss', g_offsets.fupd_time)]);
     exit;
    end;
 if not ( Enabled and CheckPointersValid ) then exit;

 {$IFOPT D+}
 if not bFast and IsKeyPressed(VK_MENU) and IsKeyPressed(VK_CONTROL) and IsKeyPressed(Ord('B')) then
    begin
     ODS('[~T].~C0C #WARN:~C07 registry updates disabled by Ctrl+Alt+B ');
     Enabled := FALSE;
     exit;
    end;
 {$ENDIF}
 //     bFast := FALSE;

 Inc (upd_loop);
 heavy_ops := 0;
 pt := FDispTimer;
 DateTimeToSystemTime ( PreciseTime, upd_st );

 pt.StartOne(1);
 pt.StartOne(42, 3);

 silent_exception := NT_ERROR + $005;
 try
  ScanObjects ( bFast and (destr_events = 0) );
 except
  on E: Exception do
     OnExceptLog ('Update->ScanObjects', E, TRUE);
 end;


 e0 := pt.Elapsed;  // 3 ms
 tr := 500 - e0;

 pt.StartOne(1);
 TestRemoved;

 if (not bFast) or (added_count + reg_events + ext_events > 0) then
         AssignParents;

 overhead := Max ( 0, Events.Count - 1000 );
 for i := 1 to overhead do
     Events.Delete (0);

 e1 := pt.Elapsed;  // 0 ms
 tr := tr - e1;

 pt.StartOne(1);
 if ( pt.Elapsed (43) > 300 ) and ( tr > 0 ) then
   begin
    LoadAll (not bFast);
    pt.StartOne(43);
   end;


 // TODO: add scan several active objects

 silent_exception := 0;

 e2 := pt.Elapsed; // 7 ms
 tr := tr - e2;

 pt.StartOne(1);
 // если изменилось количество, пересчитать индексы

 if tr > 0 then
   begin
    if CountUpdated then
       UpdateMap (bFast);
    CountUpdated := FALSE;
   end;


 if upd_loop > 50 then
    FreezeMax := 25;

 FLastSorted := TRUE;

 if upd_loop and $FF = 0 then
  for i := 0 to Count - 2 do
   if Objects[i].id >= Objects[i + 1].id then
     begin
      PrintError( Format(' unsorted evil at [%d/%d], ids %d >= %d ', [i, Count, Objects[i].id, Objects[i + 1].id] ) );
      FLastSorted := FALSE;
     end;

 if upd_loop and $3FF = $7F then
    CheckSections;

 if Count = 0 then
    ODS('[~T]. ~C0C #WARN(TRootRegistry.Update): nothing is found ~C07');

 e3 := pt.Elapsed;
 e4 := pt.Elapsed(42);

 cpu_time  [0] := cpu_time  [0] + pt.CPUElapsed(42);
 exec_time [0] := exec_time [0] + e4;

 if bFast then
    Inc(fast_updates)
 else
    Inc(slow_updates);

 if e4 > 19 then
   begin
    wprintf ('[~T/~B/~I].~C0C #PERF_WARN(TRootRegistry.Update):~C07  ' +
            'so(%d) = %5.0f, ap = %5.0f, la = %5.0f, um = %5.0f mcs ' +
            'add = %5d, chk = %5d upd = %5d, heavy = %3d, loop = $%x [%.0f ms]',
                 [Ord(bFast), e0 * 1e3, e1 * 1e3, e2 * 1e3, e3 * 1e3,
                  added_count, chkd_count, upd_obj_cnt, heavy_ops, upd_loop,
                  e4]);

    wprintf(' added = %d, reg_events = %d, destr_events = %d ',
                [added_count, reg_events, destr_events]);

    wprintf(' addr range = [$%x .. $%x] ', [g_addr_min, g_addr_max]);
    if ( (not bFast) ) and (e4 > 30) then
      begin
       if lreg_L <> nil then ODS('~C0A' + LuaTraceback (lreg_L,  '   ')) else
       if game_L <> nil then ODS('~C0F' + LuaTraceback (game_L,  '   '));
      end;  // }

    if (e4 > 40) and (load_mask < $FF) then
     begin
      load_mask := (load_mask shl 1) or 1;
      wprintf('~C0C #WARN:~C07 load_mask changed to $%x', [load_mask]);
     end

   end;

 added_count  := 0;
 destr_events := 0;
 ext_events   := 0;
 reg_events   := 0;


 if upd_st.wMinute <> last_perf_dump.wMinute then
   begin
    e0 := 0;
    if last_perf_dump.wYear > 0 then
       e0 := SystemTimeToDateTime (upd_st) - SystemTimeToDateTime (last_perf_dump);
    last_perf_dump := upd_st;
    if (e0 > DT_ONE_SECOND * 30) then
      begin
       e1 := cpu_time[0]  - cpu_time[1];
       e2 := exec_time[0] - exec_time[1];
       for i := High(cpu_time) - 1 downto 0 do
          begin
           cpu_time [i + 1] := cpu_time[i];
           exec_time[i + 1] := exec_time[i];
          end;

       e0 := e0 / DT_ONE_MSEC;

       wprintf('[~T].~CF0 #PERF(TRootRegistry.Update):~C07 cpu_time_total = %.1f sec, cpu_time = %.5f sec, cpu_usage = %.1f %%',
               [ cpu_time[0] / 1000,
                 e1 / 1000,
                 100 * e1 / e0 ]);
       wprintf('\t\t\t exec_time_total = %.1f sec, exec_time = %.5f sec, time_usage = %.1f %%',
               [ exec_time[0] / 1000,
                 e2 / 1000,
                 100 * e1 / e0 ]);
       wprintf('\t\t\\t fast_updates = %d, slow_updates = %d ', [fast_updates, slow_updates]);

      end;

   end;

end;

procedure TRootRegistry.UpdateMap;
var
   n: Integer;
begin
 // обновление карты объектов, с распределением по секционным регистрам
 ok_count := 0;
 for n := 0 to Count - 1 do
   if ( Objects[n].upd_loops < 5 ) then
        MapObject ( Objects [n] );
end; // UpdateMap


{ TVertexPosMap }

procedure TVertexPosMap.Add(i: Integer; const v: T3DVector4);
begin
 if i >= Length (data) then
    SetLength (data, i + 1024);
 data[i] := v;
end;

constructor TVertexPosMap.Create;
begin
 SetLength (data, 64 * 1024);
end;

destructor TVertexPosMap.Destroy;
begin
 SetLength (data, 0);
 inherited;
end;


function  ReadWORD (p: Pointer; ofs: NativeInt): WORD;
var
   pw: PWORD;
begin
 pw := RelativePtr (p, ofs);
 result := pw^;
end;

{ TXrVector }

function TXrVector.length(item_size: NativeUInt): NativeUInt;
begin
 Assert (laddr >= saddr, 'xr_vector.start > xr_vector.last');
 result := (laddr - saddr) div item_size;
end;

{ TSGraphVertex }

function TSGraphVertex.GetWrapper(ofs: DWORD): PServerEntityWrapper;
begin
 // Move (content [ofs], result, sizeof(Pointer));
 result := PPointer (@content[ofs])^;
end;

var
   g_reg_checked: Integer = 0;

procedure VMUpdateRegion(const mbi: TMemoryBasicInformation);
var
    ofs: UIntPtr;
     po: UIntPtr;
     lo: UIntPtr;
     rw: Boolean;
     np: DWORD;
     ns: DWORD;
     sb: set of TBits32;


begin
   if g_page_bits = 0 then
      g_page_bits := 12;

   po := UIntPtr(mbi.BaseAddress);
   ofs := po + mbi.RegionSize;
   rw := FALSE;
   g_addr_min := Min (g_addr_min, po);
   g_addr_max := Max (g_addr_max, ofs);

   if (mbi.State = MEM_COMMIT) and (mbi.Protect and PAGE_RW > 0) then
       rw := TRUE; // весь регион читаемый и записываемый

   while (po < ofs) do
    begin
     np := po shr g_page_bits; // номер страницы
     ns := np shr 5;
     lo := TBits32(np - ns shl 5) and 31; // номер бита модификации
     Assert (ns <= High(g_addr_map),
                Format('set index %d caused overrun map size %d for offset $%x!', [ns, High(g_addr_map) + 1, po]));
     sb := g_addr_map[ns];
     if rw then
        sb := sb + [lo]
     else
        sb := sb - [lo];
     g_addr_map[ns] := sb;
     po := po + g_sys_info.dwPageSize;
     if 0 = g_sys_info.dwPageSize then break;
    end;

   if (g_scanner = nil) or (g_scanner.ThreadID <> GetCurrentThreadId) then
    begin
     Inc (g_reg_checked);
     ofs := 0;
     if Assigned(g_scanner) then
       begin
        g_scanner.Priority := tpNormal;
        ofs := g_scanner.scan_loop;
       end;

     if g_reg_checked mod 1000 = 0 then
        wprintf('[~T/~B]. #PERF: Direct VMScan regions checked %d, g_scanner.scan_loop = %d, bad_ptrs = %d ',
                         [g_reg_checked, ofs, bad_ptrs]);

    end;

end;

function VerifyPtr(p: Pointer; sz: DWORD = 4): Boolean;
var
   ns, np: NativeUInt;
     ofst: NativeUInt;
     bits: TBitSet;
     nbit: TBits32;
       mbi: TMemoryBasicInformation;
begin
 result := FALSE;
 if (UIntPtr(p) < $10000) then exit;
 if g_scanner = nil then
   begin
    ODS('[~T/~U/~B]. #DBG: creating BackgroundScanner thread.');
    g_scanner := TBackgroundScanner.Create(TRUE, 'BackgroundScanner');
    g_scanner.Start;
   end;



 result := TRUE;
 ofst := UIntPtr(P);
 if Assigned (g_scanner) and (g_scanner.scan_loop > 0)  and
    (ofst >= g_vmap_min) and (ofst <= g_vmap_max + 1024) then
  begin
   np := ofst shr g_page_bits; // номер страницы
   ns := np shr 5;
   bits := g_addr_map[ns];
   nbit := np - (ns shl 5); // выбор бита соответствующего странице
   if TBits32(nbit) in bits then exit;
  end;

 FillChar(mbi, sizeof(mbi), 0);
 if VirtualQuery (p, mbi, sizeof(mbi)) = sizeof(mbi) then
    VMUpdateRegion(mbi);
 if mbi.Protect and PAGE_RW = 0 then
   begin
    result := FALSE;
    Inc (bad_ptrs);
   end;
end;

{ TBackgroundScanner }

procedure TBackgroundScanner.ProcessInit;
begin
 inherited;
 self.wait_time := 150;
 self.Priority := tpIdle;
 SetThreadAffinityMask (Handle, $FE);
end;

procedure TBackgroundScanner.WorkProc;


var
     pt: TProfileTimer;
 e0, e1: Double;
    mbi: TMemoryBasicInformation;
    ofs: UIntPtr;
    lim: UIntPtr;
    cnt: DWORD;
     lo: DWORD;
     bb: DWORD;
     sz: NativeUInt;
    vmx: UIntPtr;
    vmn: UIntPtr;


begin
  if (nil = g_scanner) and (not Terminated) then
    begin
     wprintf('[~T]. #DBG: shutdown signal detected in %s', [ThreadName]);
     Terminate();
     exit;
    end;


  if Assigned(g_scanner) and (self <> g_scanner) then
    begin
     wprintf('[~T/~I].~C0C #ERROR:~C07 another BackgroundScanner created & runned with name %s', [g_scanner.ThreadName]);
     Terminate();
     exit;
    end;

  if game_paused then exit;
  if g_addr_max <= g_addr_min then exit;
  if Assigned (xr_active) and not xr_active^ then exit;

  pt := self.ProfTimer;
  pt.StartOne();
  try
    GetSystemInfo(g_sys_info);
    Assert (g_sys_info.dwPageSize >= 4096, 'Неожиданный размер страницы памяти = ' + IntToStr(g_sys_info.dwPageSize));
    lo := 1; // 2  4  8  16 32 64 128 256 512 1024 2048 4096
    bb := 0; // 1  2  3  4   5  6   7   8   9   10   11   12
    while (lo < g_sys_info.dwPageSize) do // Log2 (page_size)
      begin
       lo := lo shl 1;
       Inc (bb);
      end;
    g_page_bits := bb;
    Assert (g_page_bits >= 12, 'Маленький сдвиг для размера страницы ' + IntToStr(g_sys_info.dwPageSize));

    sz := g_addr_max - g_addr_min; // сколько в байтах
    sz := sz div 4096; // сколько страниц
    Inc (scan_loop);

    sz := sz div 32;   // сколько двойных слов

    e0 := pt.Elapsed();
    ofs := g_addr_min;
    lim := g_addr_max + 1024;
    cnt := 0;
    vmx := 0;
    vmn := NativeUInt(-1);

    Repeat
     Inc (cnt);
     FillChar(mbi, sizeof(mbi), 0);
     if VirtualQuery (Pointer(ofs), mbi, sizeof(mbi)) > 0 then
       begin
        ofs := UIntPtr (mbi.BaseAddress) + mbi.RegionSize;
        VMUpdateRegion (mbi);
        vmn := Min (vmn, UIntPtr(mbi.BaseAddress));
        vmx := Max (vmx, ofs);
       end
     else
       Inc(ofs, 4096);

      if cnt and $FF = 0 then
         Sleep(1)
      else
         SwitchToThread;

    Until (ofs > lim);
  except
   on E: Exception do
     PrintError('Exception in TBackgroundScanner.WorkProc: ' + E.Message + ', last_ofs = $' + IntToHex(ofs, 8));
  end;
  // TODO: need interlocked!
  g_vmap_min := vmn;
  g_vmap_max := vmx;

  VerifyPtr( Ptr(g_addr_min) );
  e1 := pt.Elapsed();
  if e1 - e0 > 500 then
     wprintf('[~T].~C0B #PERF_WARN:~C07 ScanVM time = %.1f ms', [e1 - e0]);

  cpu_total := cpu_total + pt.CPUElapsed();
  if scan_loop and $F = 0 then
     Priority := tpLower;

  if (cpu_total >= 5000) and (scan_loop > 1) then
    begin
     wprintf('[~T].~C0F #SCAN_VM:~C07 addr range = $%x..$%x (%d pages by %d), regions scanned %d, median time = %.3f ms, bad_ptrs = %d',
          [g_addr_min, g_addr_max, sz, g_sys_info.dwPageSize, cnt, cpu_total / scan_loop, bad_ptrs]);
     cpu_total := 0;
     scan_loop := 0;
    end;

end;


initialization
 RegModule ('XrayRegistry', 'misc', OnModuleRqs);
 InitializeModule ('XrayRegistry');
finalization
 FreeAndNil (fma);
 FinalizeModule ('XrayRegistry');
end.
