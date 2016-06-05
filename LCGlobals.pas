unit LCGlobals;

interface
uses Windows, SysUtils, Classes, StrUtils, StrClasses, Misc, ContNrs, MD5, IniFiles,
     DateTimeTools, LuaTypes, LuaTools, XrayLua, VxTools, WThreads, PsUtils, FastSync, Debugger, TlHelp32, UniArray,
     madMapFile, IPCUtils, Messages;

const
       MIN_VM_NEED = 100 * MEBIBYTE;
      profile_file = 'player.dat';
      flNeedRescan = 1 shl 2;
{$I stkdef.inc}

{$IFDEF NLC_GOLD}
   TIME_EXPIRATION = 42097 + 3000;
{$ELSE}
   TIME_EXPIRATION = 42097 + 7000;
{$ENDIF}
     PROFILE_TAG: PAnsiChar = 'Пяни';
              ICP_CODE_SIZE = $316000; // размер модуля загруженной DLL







type
   TKeyStateVector = array [0..1023] of DWORD;
   TLogCallback = procedure (msg: LPSTR); cdecl;
   TTimeGetter  = procedure (var st: TSystemTime); cdecl;

   PVFTable = PPointerArray;


   TCppObject = record
      vftable: PVFTable;
         data: packed array [4..255] of BYTE;
    function   calc_ptr(ofs: Integer): Pointer; inline;
    function   read_ptr(ofs: Integer): Pointer; inline;
   end;

   PCppObject = ^TCppObject;

    str_value = packed record
      dwReference: DWORD;
         dwLength: DWORD;
            dwCRC: Integer;
            value: array [0..255] of AnsiChar;
    end;

    pstr_value = ^str_value;

    _shared_str = packed record
     p_: pstr_value;
    end;

    shared_str = ^_shared_str;


  TFSPath = packed object
             m_Path: LPSTR;
             m_Root: LPSTR;
              m_Add: LPSTR;
           m_DefExt: LPSTR;
    m_FilterCaption: LPSTR;
            m_Flags: DWORD;
  end;

  FS_Path = TFSPath;

  PFSPath = ^TFSPath;

  TRenderPaused = function: Integer; stdcall; // ?Paused@CRenderDevice@@QAEHXZ

  TLuaMM = lua_Alloc;
  WStr = string;


   PTreeMapNode = ^TTreeMapNode;

   TTreeMapNode = packed record
       left,                // $00
     parent,                // $04
      right: PTreeMapNode;  // $08

   {$IFDEF NEWEST_BUILD OR CSKY OR SCoP}
     // для современных билдов ключ+значение в конце структуры (24 байта)
      color: BYTE;          // $0C only after build 2947
     is_nil: ByteBool;      // $0D
     adjust: WORD;          // $0E nothing matched
        key: DWORD;         // $10
        obj: Pointer;       // $14
   {$ELSE}
     // для ранних билдов, ключ+значение идут после right. Размер структуры 22 байта
        key: DWORD;         // $0C
        obj: Pointer;       // $10
      color: BYTE;          // $14
     is_nil: ByteBool;      // $15
   {$ENDIF}
    end;

   TXrMap = packed record
    head: PTreeMapNode; // points to most_left, root and most_right nodes
    size: Integer;
    function   GetHead (const name: String): PTreeMapNode;
   end;
   PXrMap = ^TXrMap;


   /// =========================== TABLE STORE =========================== ///
   {    Класс табличного хранилища предназначается для перетаскивания данных между разными lua_State, в том числе выполняемых в разных нитях.  }

   TTableStore = class;

   TValueRecord = record
    _type: WORD;
    flags: WORD;
     strv: String;

    function   Compare(const sample: TValueRecord): Boolean;
    function   FormatStr: String;
    procedure  Import (L: lua_State; idx: Integer);
    procedure  Release;
    procedure  Store (L: lua_State);

    case BYTE of
     0: ( intv: Int64 );
     1: ( dblv: Double );
     2: ( ptrv: Pointer );
     3: ( tabl: TTableStore );
     4: ( bool: Boolean );
     5: ( func: lua_CFunction );
   end; // TValueRecord


   TValuePair = class
   public
    key, value: TValueRecord;

    { C & D }
    constructor Create;
    destructor  Destroy; override;

   end;

   TKeyboardClicks = array [0..255] of Integer;

   TKeyboardCallback = class
   public
    vk1, vk2, vk3: BYTE;
    evt_code: DWORD;
    objp: Pointer;
    func: String;
   end;


   TTableStore = class (TStrMap)

   protected
   public

    constructor  Create(AOwner: TObject = nil);
    { methods }
    procedure    LuaExport (L: lua_State);
    procedure    LuaImport (L: lua_State; idx: Integer);
   end;

   TPtrInfo = class
    private
      FValue: String;
      FPK: Pointer;
    public


     property  PK: Pointer read FPK write FPK;
     property  Value: String read FValue write FValue;

     { C & D }
     constructor  Create (p: Pointer; const initial: String);
   end;


   TToolThread = class (TWorkerThread)
    private
           ec_last: Integer;

          prev_eip: DWORD;
         freez_cnt: Integer;
         cnt_ticks: Integer;
      minute_ticks: Integer;
     reg_check_mem: Boolean;
    launcher_alive: THandle;
     allow_corrupt: Real48;


     procedure                  CheckEnvironment;
     procedure                  CheckModuleLoaded (const sModule: String);
     procedure                  LightCorruption;
     procedure                  TestKeyMap;
     procedure                  UpdateMaps ( scan_libs: Boolean );
     procedure                  SignalAlive;
    protected

      fav_modules: TStrMap;        // association map
       prv_paused: Integer;
        last_free: DWORD;
         mod_list: TModuleList;
         map_list: TModuleMapList;
         sc_timer: TCritSection;
           render: Pointer;
           dbg_ps: TDebuggedProcess;

     // spy_file: Text;

     procedure                  ProcessKeyComb;
     procedure                  ProcessInit; override;
     function                   ProcessRequest(const rqs: String; rqobj: TObject): Integer; override;
     procedure                  ProcessThreadStop; override;  // pre stopping
     procedure                  OnTimer (idTimer: Integer); override;
    public

         main_th: THandle;
        scan_cpu: Boolean;
       base_maps: Integer;
        sc_mlist: TCritSection;
        r_paused: TRenderPaused;
      wnd_active: Boolean;      // активно окно игры
     timer_procs: array [0..3] of TProc;

     property                   Deviation: Real48 read allow_corrupt write allow_corrupt;
     property                   ModuleList: TModuleList read mod_list;

     function                   FindPtrMap (p: Pointer): TModuleMap;
     function                   ModuleByPtr (p: Pointer): TModuleEntry32;
     procedure                  WorkProc; override;
    end; // TPatched

    TCheckPoints = object
    protected
     data: array [0..3] of Integer;
    public

     procedure    add (i, v: integer);
     procedure   exch (i, v: integer);
     procedure   init ( v1, v2, v3, v4: Integer );

     function    dump: String;
    end;

   PDMAOffsets = ^TDMAOffsets;

   TSeOffsets = record
      sz_name: NativeInt;
      id_word: NativeInt;
      section: NativeInt;
    parent_id: NativeInt;
     spawn_id: NativeInt;
     position: NativeInt;
    direction: NativeInt;
        clsid: NativeInt;
       gvx_id: NativeInt;
       lvx_id: NativeInt;
        flags: NativeInt;
     story_id: NativeInt;
    spawn_sid: NativeInt;
       online: NativeInt;
       angles: NativeInt;
     released: NativeInt;

      CALifeSimulatorBase: NativeInt;
      CALifeSpawnRegistry: NativeInt;
           CALifeSpawnReg: record
                   spawns: NativeInt; // offset inside CALifeSpawnsRegistry
                 vertices: NativeInt; // offset inside CALifeSpawnsRegistry
                  wrapper: NativeInt; // offset inside CGraphAbstractSerialize vertex ServerEntityWrapper
                story_ids: NativeInt;
           end;
     CALifeObjectRegistry: NativeInt;
     CALifeObjectReg_list: NativeInt; // offset inside CALifeObjectRegistry

     procedure Load (const sect: String; owner: PDMAOffsets );
   end;

   TLevelOffsets = record
        objects_list: NativeInt;
           map_NETID: NativeInt;
      objects_active: NativeInt;
    objects_sleeping: NativeInt;
       destroy_queue: NativeInt;
      client_objects: NativeInt;
   end;

   TDMAOffsets = object // offsets for basic object
   private
           fini: TIniFile;
          fname: String;
    _is_correct: Boolean;

    procedure   AddDefaultParent;

   public
     fupd_time: TDateTime;  // когда файл с настройками был записан

    g_ai_space: NativeInt;

    trade_flag: NativeInt;
          cost: NativeInt;
    inv_weight: NativeInt;
      position: NativeInt;
       id_word: NativeInt;
       cl_name: NativeInt;
       section: NativeInt;
        visual: NativeInt;
        parent: NativeInt;
         clsid: NativeInt;

      xform_i: NativeInt;
      xform_j: NativeInt;
       direction: NativeInt;
        ai_loc: NativeInt;

         level: TLevelOffsets;


      se_offsets: TSeOffsets;
      offset_map: TStrMap;


      build_sect: String;


    { C & D }
    procedure           Initialize (game_build: Integer; const build_ext: String);

    { methods }
    function            GetIniFile: TIniFile;
    function            IsCorrect (bAutoReload: Boolean = TRUE): Boolean;


    function            ReadOfs (const sect, ident: String; def: NativeInt): NativeInt;

    function            FindOfs (const code: String): NativeInt;

    function            EngineBuildTime: TDateTime;

   end; // TObjOffsets

    TSystemAnchor = array [0..3] of TRandomHash;
    THardwareBind = array [0..2] of TSystemAnchor;


    TGamerProfile = packed record
     ftag: array [0..3] of AnsiChar;            // 04 bytes
     fver: DWORD;         // file version       // 04 bytes = 08
     name: array [0..15] of WideChar;           // 32 bytes = 40
     ctdt: TDateTime;     // created was...        08 bytes = 48
     gpmt: THashVector;   // global permut         32 bytes = 80
     puid: TRandomHash;   // typical random        32 bytes = 112
     hwbf: DWORD;         // bit flags             04 bytes = 116
     hwbd: THardwareBind; //                      384 bytes
     ecsz: BYTE;          // size of signature      1 bytes
     ecsn: array [0..94] of BYTE; // ECDSA + ts     95 bytes
     fcrc: TRandomHash;  // fixed part CRC         32 bytes
     dcnt: DWORD;        // death  counter
     lcnt: WORD;         // levels change count
     ncnt: WORD;         // New Game count
     lvnm: WORD;         // level number/index
     gvid: WORD;         // actor game_vertex  on last update
     lvid: DWORD;        // actor level_vertex on last update

     // non-hashed data:
     pins: TRandomHash;
     pcrc: TRandomHash;  // all of data, exclude self and pins. Used in game-series


     class function  BodySize: Integer; static;
     class function  FixedSize: Integer; static;

     function        BodyCRC (nonce: Int64) : TRandomHash;
     function        FixedCRC (nonce: Int64): TRandomHash;
    end;

    PGamerProfile = ^TGamerProfile;


{$I stkdef.inc}

const
   LUAICP_DLL = 'luaicp.dll';

{$IFDEF SOC}
   {$IFDEF NEWEST_BUILD}
   LUA_DLL = 'lua5.1.dll';
   {$ELSE}
   LUA_DLL = 'xrLua.DLL';
   {$ENDIF}
   XR_EXE  = 'xr_3da.exe';
{$ELSE}
   LUA_DLL = 'lua.JIT.1.1.4.DLL';
   XR_EXE  = 'xrEngine.exe';
{$ENDIF}

  DEF_HIDE_SUFFIX = '';

{$IFDEF NLC}

  DEF_FORM_CAPTION = 'Запуск NLC7 - Я Меченный.';
{$ELSE}
  DEF_FORM_CAPTION = 'Запуск S.T.A.L.K.E.R.';
{$ENDIF}

{$IFDEF SOC}
  DEF_SAVE_EXT = '.sav';
{$ENDIF}

{$IFDEF CSKY}
  DEF_SAVE_EXT = '.sav';
{$ENDIF}

{$IFDEF SCOP}
  DEF_SAVE_EXT = '.scop';
{$ENDIF}

var
      _first_var: Integer = 1;
           g_chs: CHAR = #0;
        dbg_vars: TStrMap;
     global_vars: TStrMap;
    removed_evts: packed array [WORD] of BYTE;
    removed_list: TStrMap;             // ! список удаленных за последний период объектов (id only)
      not_frozen: DWORD = 0;


       g_offsets: TDMAOffsets;         // смещения для технологии DMA
        dev_comp: Boolean = FALSE;     // компьютер разработчика

    auto_free: TObjectList;
     auto_nil: TList = nil;
   planed_dpc: TList = nil;

     game_active: Boolean = FALSE;
    game_verbose: Integer = 3;

    /// >>>>>>>>>>>>>>>>>>>>>>>>> global LUAICP configuration <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< ///
        g_Config: record
         silent_mode: Boolean;
       allow_game_mm: Boolean;
        bInterceptMM: Boolean;
       load_launcher: Boolean;
      end;
   usr_dlls: TStrMap = nil;
    cmdlist: TStrMap = nil; // список команд полученный через консоль (перехват invoke)
   xray_log_file: String = '';
   xray_log_path: String = '';
   xray_log_tail: String = '';
  xray_log_lsize: Integer = 0;
 silent_exception: DWORD = 0;


   last_mem_stat: String;
   FSGame: TStrMap = nil;
   app_data_root, game_root, game_data, game_saves, game_scripts, mod_dir: String;
     script_dirs: TStrMap = nil;
   key_state_map: array [0..255] of TKeyStateVector;


   startup_log: String;
       com_key: String;
       lsc_dbg: Integer;
        test_v: Integer;
       test_pv: PInteger = nil;

       hLuaicp: HMODULE = 0;
  game_process: THandle = 0;
      game_pid: DWORD   = 0;


   MY_SEH_install: Integer = -1;
   BT_SEH_uninst: Boolean = FALSE;
   SEH_pointer: Pointer = nil;
   last_dbg_md5: TMD5Digest;

       h_process: THandle = 0;
   h_main_thread: THandle = 0;
    mt_stack_top: DWORD;
    mt_stack_btm: DWORD;

    gInitialized: Boolean = FALSE;
    bShowConsole: Boolean = FALSE;
       bLoadMaps: Boolean = TRUE;


   block_export: TStrMap = nil;
   user_ltx: TStrMap;
   init_script: String = '';
    rtm_script: String = '';
   fatal_err_cnt: Integer = 0;
   tool_thread_period: Integer = 20;
   log_errors: Integer = 0;
   kbd_ru, kbd_en: HKL;
   global_pt: TProfileTimer;
   show_anticheat: Boolean = FALSE;
      upd_timeout: Integer;
          bu_last: TDateTime = 0; // last binder update

   разрчит: Boolean = FALSE;
   n_tick: Integer = 0;
   in_func: PChar = '/';
   in_func_r: PChar = '/';



   game_except_count: Integer = 0;

            aux_L: lua_State = nil;
         active_L: lua_State = nil;
          pcall_L: lua_State = nil;
           game_L: lua_State = nil;
           xray_L: lua_State = nil; // назначается при проверке захваченных стейтов
    curr_action_0: array [0..63] of Char; // debug hang level 0
    curr_action_1: array [0..63] of Char; // debug hang level 1
    curr_action_2: array [0..63] of Char; // debug hang level 2

    ps_cmds_on_timer: Boolean = FALSE;

   gLuaThreadCounter: Integer = 0;
   gLuaThreadRemoved: Integer = 0;
       n_shifts: Integer = 0;
     InLauncher: Boolean = FALSE;  // executed from host
         InXray: Boolean = FALSE;  // executed from STALKER

  test_keyboard: Boolean = TRUE;
    version_map: TStrMap = nil;
     rtp_enable: Boolean = TRUE;
      rtp_timer: DWORD;

   ccntr: Integer = 0;
   parse_log: Boolean = FALSE;
   game_con_log: Boolean = FALSE;

   active_wnd: HWND;
   fgrnd_wnd: HWND;

   _conmsgs: TStrMap = nil;
   _stklw: Boolean = FALSE;
   _stklog: procedure ( const msg: LPSTR ); cdecl = nil;
   _stkflush: procedure (); cdecl = nil;
   _btfltu: procedure; stdcall = nil; // BT_UninstallSehFilter
   _xdbgInit: Pointer;
   {$IFDEF SOC}
   _SetLogCB: procedure ( _newCB: TLogCallback); cdecl;
   {$ELSE}
   _SetLogCB: function ( _newCB: TLogCallback ): TLogCallback; cdecl;
   {$ENDIF}
   _SetTimeGetter: function ( _newCB: TTimeGetter ): TTimeGetter; cdecl;

      inst_logCB: Boolean = FALSE;
      orig_logCB: TLogCallback = nil;


   game_log: TStrMap;   // cache of game log
    vk_term: WORD = VK_DELETE;
   launcher: HMODULE;
  xr_engine: HMODULE;  // module addr for xr_3da.exe or xr_Engine.exe
    g_sltmp: TObjectList; // rotable temporary string storage


        _RayPick: Pointer; // pointer to func

      current_lvl: PLevelVertexList;
      current_lvf: String = '';
       vertex_map: TVertexMap = nil;
         captured: TStrMap;
     g_game_build: Integer = 0;
      g_build_ext: String = '';


       g_pApplication: ^PCppObject = nil; // pointer into xr*.exe
         g_pGameLevel: ^PCppObject = nil;
    g_pGamePersistent: ^PCppObject = nil;
            g_pRender: ^PCppObject = nil; // pointer into xr*.exe



       map_need_funcs: Boolean = FALSE;
          gToolThread: TToolThread = nil;
            ptr_infos: TObjectStorage;    // !
              ptr_obj: TPtrInfo;  // cache object
           global_uno: PSingle = nil;

            lc_minute: WORD;
             kcb_list: TObjectList = nil; // !
           inner_cmdl: TStrMap = nil;     // !
            trace_log: TStrMap = nil;     // !
            lock_objs: TStrMap = nil;     // !
              ht_list: TStrMap = nil;     // ! helper threads

         gCheckPoints: TCheckPoints;
           key_clicks: TKeyboardClicks;
           prv_clicks: TKeyboardClicks;
            th_kstate: TKeyboardState;
            ls_kstate: TKeyboardState;

           th_kupdate: TDateTime;
        collapse_time: TDateTime = 0; // время после которого приложение должно быть срублено жестко
        ignore_assert: Boolean = FALSE;
             g_in_veh: Integer = 0;
             n_except: Integer = 0;
           bShowPulse: Boolean = FALSE;
        ptr_info_func: function (p: Pointer): String = nil; // pointer description func

       prvExceptionFilter: TFNTopLevelExceptionFilter;

            orig_hf: TLuaMM = nil;    // heap function (memory management)
            orig_ud: Pointer = nil;
           last_src: String = '';
          class_map: TStrMap = nil;   // string to clsid assoc map
             permut: array [0..255] of BYTE;


        signal_disp: Boolean = FALSE;
      gamer_profile: ^TFileMapping = nil;
       profile_name: String = 'nope';
        game_paused: Boolean;
          xr_active: PBoolean = nil;
       beep_enabled: Boolean = TRUE;
             in_ODS: Boolean;
               fini: TIniFile;

       // imported functions, while using in launcher
       FSExpandPath: function (path, pResult: PWideChar): Boolean; stdcall = nil;


function  apply_permut (s: String): String;
function  init_permut: AnsiString;
procedure mix_bytes (bytes: PByteArray; Count: Integer);
procedure add_garbage(var obj);

function  ActiveProfileName: String;
function  ExpandPath (const path: String): String;
function  LuaSleepEx(L: lua_State): Integer; cdecl;
function  LuaSetThreadAM(L: lua_State): Integer; cdecl;
function  LuaSetBootMsg (L: lua_State): Integer; cdecl;
function  LuaGetTempBuff (L: lua_State): Integer; cdecl;
function  LuaSendAlert (L: lua_State): Integer; cdecl;
procedure SetBootMsg(const msg: String);
procedure LoadGlobalMap;


procedure InitDMA (bReloadOffsets: Boolean);
function  HardMD5 (const md: TMD5Digest; use_rand: Boolean): String;
function  DecryptXor (const buff: array of BYTE): String;
function  DecryptXorBin (buff: PByteArray; cbSize: Integer): PByteArray;

function  LoadByteCode (L: lua_State; pbuff: Pointer; cb: Integer; const sNameSpace: String): Integer;
function  Patch_XrDebugInit: NativeUInt;

function  FileCheck (fname: String): Boolean;
function  FmtStalkerPath (const src_path: String): String;
function  GetTmpSL: TStrMap;
procedure RegPtr ( p: Pointer; const info: String; replace: Boolean = FALSE);
procedure PlayBeep(dwFreq, dwDuration: DWORD);

function  KeyPressedEx (vk: BYTE; back: DWORD = 1): Boolean;
function  KeyCombPressed (vk1, vk2: BYTE; vk3: BYTE = $FF; back: DWORD = 1): Boolean;

function  LibFuncRel ( const libname, func: String ): Integer;

function  DetectGameBuild: DWORD;
function  DetectXrGame: String;
procedure ExportGameBuild (L: lua_State);

function  ModuleSize ( const m: String ): Integer;
function  g_ai_space ( offset: NativeInt = 0 ): PPointer;

function  GoodPtr(p: Pointer; size: DWORD = 4): Boolean;
procedure UpdateXrayOffset (key: LPSTR; ofs: DWORD); stdcall;
function  AnsiArg(const s: String): PAnsiChar;
function  Lvm_name(L: lua_State): String;


implementation
uses Math, Singularity, XrayExt, XrayImports, MultiProc, madExcept;

type
     TBuffer1024 = packed array [0..1023] of BYTE;
    TTempBuffers = array [0..255] of TBuffer1024;
      string4096 = packed array [0..4095] of AnsiChar;


const
   HashIndex: String = '0123456789ABCDEFGHIJKLMNOPQRSTUWXYZabcdefghijklmonpqrstuvwxvz ,.!?@#$%^&*()_+-=/{}[]¶§©®±';

var
     xrGame_ver: String;

   CApplication: record
      LoadTitleInt: procedure; stdcall;
   end;



    g_ai_space_ptr: PPointer = nil;
         g_buffers: ^TTempBuffers;
       g_ansi_tmps: array [0..15] of string4096;
       g_ansi_last: Integer = 0;


function AnsiArg(const s: String): PAnsiChar;
var
   i: Integer;
begin
 i := InterlockedIncrement (g_ansi_last) and $F;
 SetStrZ(g_ansi_tmps[i], s, 4095);
 result := g_ansi_tmps[i];
end;


function ActiveProfileName: String;
var
   s: String;
   c: String;

begin
 s := ExpandPath ('$profile_dir$');
 c := ExpandPath ('$profiles$');

 if ( Length (s) >= Length (c) ) and ( Pos(c, s) > 0 ) then
     Delete ( s, 1, Length(c) )
 else
     s := '';

 result := AnsiReplaceStr ( s, '\', '' );
end;

function ExpandPath (const path: String): String;
begin
 SetLength (result, 1024);
 FillChar (result [1], sizeof (Char) * 1024, 0);
 FSExpandPath ( PChar (path), PChar (result) );
 result := Trim (result);
end;

procedure SetBootMsg(const msg: String);
var

   amsg: PAnsiChar;
begin
 if ( g_pApplication = nil ) or ( g_pApplication^ = nil ) or
    ( @CApplication.LoadTitleInt = nil ) then exit;

 amsg := PAnsiChar ( AnsiString (msg) );
 asm
  mov  eax, amsg
  push eax
  mov  ecx, g_pApplication
  mov  ecx, [ecx]
  call [CApplication.LoadTitleInt]
 end;
end;


function LibFuncRel ( const libname, func: String ): Integer;
var
   hLib: Integer;
     pp: Pointer;
begin
 result := -1;
 hLib := GetModuleHandle ( PCHar (libname) );
 if hLib = 0 then exit;

 Dec (result);
 pp := GetProcAddress ( hLib, PChar (func) );
 if pp = nil then exit;

 result := NativeInt (pp) - hLib;
end;

function LVM_name(L: lua_State): String;
begin
 if Assigned(get_lvm_name) then
  result := AnsiTrim2W ( get_lvm_name (L) )
 else
  result := 'unknown_' + FormatPtr(L);
end;


function ModuleSize ( const m: String ): Integer;
var
   c, i: Integer;
begin
 i := -1;
 c := 0;

 if Assigned (gToolThread) and Assigned (gToolThread.ModuleList) then
  with gToolThread do
   repeat
    sc_mlist.Lock('find');
    try
      i := ModuleList.Find (m);
      if i >= 0 then
         i := ModuleList.Items [i].modBaseSize;
    finally
     sc_mlist.Unlock
    end;
    if (i < 0) then
      begin
       // gToolThread.AddRequest('UPDATE_MAPS');
       // Sleep(30);
       result := 0;
       exit;
      end;

   until ( i >= 0 ) or (c > 2);
 result := i;
end;


function g_ai_space ( offset: NativeInt = 0 ): PPointer;
var
   hLib: HMODULE;
begin

 if g_ai_space_ptr = nil then
   begin
     hLib := GetModuleHandle ('xrGame.dll');
     g_ai_space_ptr := GetProcAddress (hLib, '?g_ai_space@@3PAVCAI_Space@@A'); // for newest builds
     if ( g_ai_space_ptr = nil ) then
          g_ai_space_ptr := Ptr ( hLib + HMODULE (g_offsets.g_ai_space) ) // $5602B0
     else
         if log_verbose >= 3 then
           begin
            g_offsets.g_ai_space := NativeInt(g_ai_space_ptr) - NativeInt(hLib);
            wprintf('[~T]. #DBG: g_ai_space exported at xrGame.dll + $%x', [g_offsets.g_ai_space]);
           end;
   end;

 result := Ptr (NativeInt(g_ai_space_ptr^) + offset);
end;

function  LuaGetTempBuff (L: lua_State): Integer; cdecl;
var
   i: Integer;
   p: Pointer;
begin
 i := lua_tointeger (L, 1) and $FF;
 if (i < 16) then
     lua_pushptr ( L, @g_buffers [i] )
 else
   begin
     p := AllocMem ( lua_tointeger (L, 2) );
     lua_pushptr ( L, p );
   end;

 result := 1;
end;

function LuaSetBootMsg (L: lua_State): Integer; cdecl;
begin
 result := 0;
 SetBootMsg ( LuaStrArg (L) );
end;

function LuaSendAlert (L: lua_State): Integer; cdecl;
var
   msg: String;
begin
 msg := LuaStrArg(L);
 if gIPCQueue <> nil then
    gIPCQueue.PushDataRqs( 'SIG_ALERT',  msg[1], sizeof(CHAR) * Length(msg), 15000);
 result := 0;
end;


function StrBegins ( sub, s: String ): Boolean;
begin
 sub := LowerCase (sub);
 s := LowerCase (s);
 result := ( 1 = Pos ( sub, s ) );
end;


function FileCheck (fname: String): Boolean;
begin

 if DirectoryExists (fname) then
    fname := AddSlash (fname);

 result := StrBegins (game_root, fname) or StrBegins (app_data_root, fname);
 if not result then
   PrintError('FileCheck detects filename with restricted root: ' + fname);
end;



function FmtStalkerPath (const src_path: String): String;
var
   k: String;
   n: Integer;
begin
 result := AnsiReplaceStr (src_path, '/', '\');



 if (Pos('\', result) = 0) and (Pos('$', result) = 0) then
     result := '$fs_root$' + result;



 if fsgame <> nil then
    for n := 0 to fsgame.Count - 1 do
     begin
      k := Trim (fsgame.Names [n]);
      if k = '' then continue;

      if Pos(k, result) > 0 then
         result := AnsiReplaceStr (result, k, UnhideSP (fsgame.Values [k]) );
     end;

 result := CorrectFilePath (result);

 if not FileCheck (result) then
    result := 'bad_file_name'
end; // FmtStalkerPath

function  GetTmpSL: TStrMap;
var
  pl: TPointerList;
   h: Integer;

begin
 if g_sltmp = nil then
   begin
    g_sltmp := TObjectList.Create (FALSE);
    while g_sltmp.Count < 32 do
          g_sltmp.Add (nil);
   end;

 pl := g_sltmp.List;

 h := g_sltmp.Count - 1;

 result := pl [0];
 Move ( pl [1], pl [0], sizeof(pl [0]) * h ); // rotate
 if result = nil then
    result := TStrMap.Create
 else
    result.Clear;
 pl [h] := result;

end;


function CharsOnly ( const s: String; const cset: TSysCharSet ): Boolean;
var
   n: Integer;
begin
 result := FALSE;
 for n := 1 to Length (s) do
  if not CharInSet ( s[n], cset ) then exit;
 result := TRUE;
end;



function LoadByteCode (L: lua_State; pbuff: Pointer; cb: Integer; const sNameSpace: String): Integer;

const
   LUA_ID_CHARS = ['0'..'9', '_', 'A'..'Z', 'a'..'z'];

var
   hdr: PLUAC_HEADER;
   szm: DWORD;
   ims: Integer;


  function _LoadString ( var LS: PBYTE ): String;
  var
     l: DWORD;
    sa: AnsiString;
  begin
   sa := '';
   l := PDWORD (LS)^ and szm;
   Inc (LS, hdr.sz_szt);
   if l > 0 then
      SetString (sa, PAnsiChar(LS), l - 1);
   Inc (LS, l);
   result := String(sa);
  end;

  function _LoadInt ( var LS: PBYTE ): Integer;
  begin
   result := PInteger (LS)^ and ims;
   Inc ( LS, hdr.sz_int );
  end;

  function _LoadByte ( var LS: PBYTE ): BYTE;
  begin
   result := LS^;
   Inc ( LS, 1 );
  end;

  function _LoadNumber ( var LS: PBYTE ): Double;
  begin
   result := 0;
   if hdr.sz_number = 4 then
       result := PSingle (LS)^;
   if hdr.sz_number = 8 then
       result := PDouble (LS)^;

   Inc ( LS, hdr.sz_number );
  end;



var
   dbg: AnsiString;
   cur: PBYTE;
   top: Integer;
   tbs: Integer;
   tmt: Integer;
   msg: String;
    pf: Integer;
    fi: Integer;

 procedure  _LoadFunction (rLevel: Integer; const sps: String);
  var
     n: Integer;
     c: Integer;
     t: BYTE;
  begin
   _LoadString ( cur );           // function source
   _LoadInt ( cur );              // line defined
   _LoadInt ( cur );              // last line defined
   Inc ( cur, 4 );                // skip attributes
   c := _LoadInt ( cur );         // lines count
   Inc ( cur, c * hdr.sz_int );   // skip lines
   c := _LoadInt ( cur );         // locals count
   for n := 0 to c - 1 do
    begin
     _LoadString ( cur );         // local var name
     _LoadInt ( cur );            // start pc
     _LoadInt ( cur );            // end pc
    end;
   c := _LoadInt ( cur );         // up values
   for n := 0 to c - 1 do
     _LoadString ( cur );

   c := _LoadInt ( cur );         // constants
   for n := 0 to c - 1 do
    begin
     t := _LoadByte ( cur );
     case t of
           LUA_TNIL:;
       LUA_TBOOLEAN:
          _LoadByte (cur);
        LUA_TNUMBER:
          _LoadNumber (cur);
        LUA_TSTRING:
          _LoadString (cur);
      else
         begin
          PrintError ('bad constant type ' + IntToStr(t));
          break;  // fatal!
         end;
     end; // case
    end; // for
   c := _LoadInt ( cur );         // sub-func count

   for n := 0 to c - 1 do
       _LoadFunction ( rLevel + 1, sps + #9 );
   c := _LoadInt ( cur );         // code size
   Inc ( cur, c * hdr.sz_instr ); // skip instructions
  end; // LoadFunction


begin
 result := -1; // load problem
 {$IFNDEF NLC_GOLD}
 ODS('[~T]. #DBG: loading byte code for ~C0A' + sNameSpace + '~C07');
 {$ENDIF}

 dbg := AnsiString (sNameSpace);

 hdr := pbuff;
 if hdr.signature <> LUAC_SIGNATURE then exit;

 {
 szm := 1 shl (8 * hdr.sz_szt - 1) - 1; // $FF if 1, $FFFF if 2, $FFFFFFFF if 4
 szm := szm shl 1 or 1;
 ims := 1 shl (8 * hdr.sz_int - 1) - 1;
 ims := ims shl 1 or 1;
 ofs := sizeof ( TLUAC_HEADER );

 cur := RelativePtr (pbuff, ofs);
  _LoadFunction (0, ''); // some byte-code checks performing }
 fi := lua_gettop (L);
 ODS( CFormat('#TEST0: gettop = %d', '~C07', [fi] ) );

 lua_getglobal (L, sNameSpace);   // get existing environment

 if lua_type(L, -1) <> LUA_TTABLE then // if table not exists
  begin
   ODS('[~T]. #DBG: namespace not exists - creating...');
   lua_pop (L, 1);
   lua_createtable (L, 0, 0);
   lua_setglobal (L, sNameSpace);
   lua_getglobal (L, sNameSpace);   // get table
  end;
 tbs := lua_gettop (L);           // namespace table index

 result := luaL_loadbuffer (L, pbuff, cb, PAnsiChar(dbg)); // 0 or error code

 if result <> 0 then
   begin
    lua_setfield (L, tbs, 'load_error');
    exit;
   end;

 lua_setfield (L, tbs, 'module_func');
 lua_pushwstr (L, sNameSpace);
 lua_setfield (L, tbs, 'module_name');
 lua_pushwstr (L, '#OK');
 lua_setfield (L, tbs, 'load_error');



 // организация метатаблицы с индексом, для _G
 lua_createtable (L, 0, 0);
 tmt := lua_gettop (L);

 lua_getglobal (L, '_G');
 if lua_istable (L, -1) then
  begin
   lua_setfield (L, tbs, '_gt');     // backref add
   lua_getfield (L, tbs, '_gt');
   lua_setfield (L, tmt, '__index'); // tmt.__index = _G
   lua_setmetatable (L, tbs);        // tbs.metatable = tbt for namespace table
  end
 else
  begin
   lua_settop (L, tbs);
  end;

 // ODS('[~T]. #DBG: chunk ids detected~C0F ' + clst.CommaText + '~C07');
 // load as environment



 pf := 0;
 lua_getglobal (L, 'AtPanicHandler');
 if lua_type(L, -1) = LUA_TFUNCTION then
     pf := lua_gettop (L)
 else
     lua_pop (L, 1);

 lua_getfield (L, tbs, 'module_func'); // reading closure for setfenv
 fi := lua_gettop (L);
 ODS( CFormat('#TEST1: gettop = %d', '~C07', [fi] ) );
 lua_pushvalue(L, tbs); // duplicate table


 if lua_setfenv (L, fi) <> 1 then
    PrintError ('lua_setfenv failed');

 // lua_getfield (L, tbs, 'module_func');

 top := lua_gettop(L);
 ODS( CFormat('#TEST2: gettop = %d', '~C07', [top] ) );

 { ODS( Format('#DUP2: tbs = %d, gettop = %d', [tbs, lua_gettop(L)] ) );
   lua_pushvalue (L, fi); // reading closure for pcall }
 // ожидаемый порядок вещей: table,  panic_func, module_func

 msg := lua_typeof (L, -2 );
 ODS( Format(' in stack at -2 now ~C0F "%s" (%d)~C07', [msg, lua_type(L, -2)] ) );
 msg := lua_typeof (L, -1 );
 ODS( Format(' in stack at -1 now ~C0F "%s" (%d)~C07', [msg, lua_type(L, -1)] ) );

 if lua_type(L, -1) = LUA_TFUNCTION then
   try
    result := lua_pcall(L, 0, LUA_MULTRET, pf);
    if result = 0 then  // need for registering any const/var/func
     begin
      lua_pushwstr (L, '#OK');
      lua_setfield (L, tbs, 'pcall_error');
     end
    else
       lua_setfield (L, tbs, 'pcall_error');
   except
    on E: Exception do
      begin
       lua_pushwstr (L, 'exception catched in lua_pcall ' + E.Message);
       lua_setfield (L, tbs, 'pcall_error');
      end;
   end
 else
   begin
    lua_pushwstr (L, 'in stack value type = ' + String(dbg) );
    lua_setfield (L, tbs, 'pcall_error');
   end;

end;

procedure swap_byte (var a, b: BYTE);
var
   t: Integer;
begin
 t := a;
 a := b;
 b := t;
end;

procedure mix_bytes (bytes: PByteArray; Count: Integer);
var
   a, b: BYTE;
      n: Integer;

begin
 for n := 1 to Count * 256 do
  begin
   a := Random ( MAXINT) mod Count;
   b := Random ( MAXINT ) mod Count;
   if a = b then continue;
   swap_byte ( bytes [a], bytes[b] );
  end

end;


function init_permut: AnsiString;
const
   MASK = High (permut);
   PCNT = MASK + 1;

var
   a, n: Integer;
      s: String;
begin
 Randomize;
 a := Length(HashIndex);
 for n := 0 to MASK do
     permut [n] := n mod a;

 mix_bytes (@permut, MASK + 1);

 s := '';
 for n := 0 to MASK do
     s := s + HashIndex [ permut [n] + 1 ];

 {$IFOPT D+}
 ODS('[~T]. #DBG: randomize test: ' + s);
 {$ENDIF}

 result := AnsiString (s);
end;


function apply_permut (s: String): String;
var
   l, n, i, cnt: Integer;
begin
 result := s;
 cnt := High(permut) + 1;
 if Length (s) > cnt then exit;

 l := Length(s);
 for n := 1 to Length(s) do
  begin
   i := permut[n - 1] mod l + 1;
   result [i] := s [n];  // элементарная перестановка
  end;
end;


function HardMD5 (const md: TMD5Digest; use_rand: Boolean): String;
var
   s: String;
   l: Integer;
begin
 s := MD5DigestToStr (md);
 s := apply_permut(s);
 if use_rand then s := s + Format ('%x', [Random (MaxInt)]);

 result := '';
 while (s <> '') do
  begin
   l := Min (5, Length (s));
   result := result + Copy (s, 1, l);
   Delete (s, 1, l);
   if Length (s) > 3 then result := result + '-';
  end;
 result := '{' + result + '}';
end;


procedure add_garbage(var obj);
begin
 auto_free.Add ( TObject(obj) );
 auto_nil.Add (@obj);
end;


function Patch_XrDebugInit: NativeUInt;
const
    retn4: array[0..2] of BYTE = ($C2, $04, 00);
begin
 if _xdbgInit = nil then exit;
 UnlockRegion (_xdbgInit);
 WriteProcessMemory ( GetCurrentProcess, _xdbgInit, @retn4, 3, result );

 if result = 3 then
    ODS('[~T]. #DBG: Patch xrDebug::__initialize successfull!');
end;


function DecryptXorBin (buff: PByteArray; cbSize: Integer): PByteArray;
var
   n: Integer;
begin
 result := AllocMem ( cbSize );
 for n := 0 to cbSize - 1 do
     result[n] := buff [n] xor (n + $BC);
end;

function DecryptXor (const buff: array of BYTE): String;
var
   ps: PAnsiChar;
begin
 result := '';
 ps := PAnsiChar ( DecryptXorBin ( @buff, Length (buff) ) );
 result := String (ps);
 FreeMem (ps);
end;


function LuaSleepEx(L: lua_State): Integer; cdecl;
var
   argc: Integer;
   ms: DWORD;
   ba: Boolean;
begin
 ms := 500;
 argc := lua_gettop(L);
 ba := FALSE;

 if argc > 0 then  ms := lua_tointeger(L, 1);
 if argc > 1 then  ba := lua_toboolean(L, 2);

 SleepEx (ms, ba);
 result := 0;
end;


//
function LuaSetThreadAM(L: lua_State): Integer; cdecl;
var
   msk: DWORD;
begin
 msk := $F;
 if lua_gettop(L) > 0 then
    msk := lua_tointeger (L, 1);
 SetThreadAffinityMask ( GetCurrentThread(), msk );
 result := 0;
end;


{ TValueRecord }

function TValueRecord.Compare(const sample: TValueRecord): Boolean;
begin
 result := FALSE;

 if (_type <> sample._type) then exit; // different (may be convertable?)

 case (_type) of
  LUA_TNUMBER: result := ( dblv = sample.dblv);
  LUA_TSTRING: result := ( strv = sample.strv);
  else
      result := (ptrv = sample.ptrv);
 end; // case
end;

function TValueRecord.FormatStr: String;
begin
 result := 'nil';
 case _type of
  LUA_TBOOLEAN: result := IfV (bool, 'TRUE', 'FALSE');
   LUA_TNUMBER: result := FormatFloat ('0.###', dblv);
   LUA_TSTRING: result := strv;
    LUA_TTABLE,
    LUA_TLIGHTUSERDATA,
    LUA_TUSERDATA,
    LUA_TFUNCTION: result := FormatPtr (ptrv);
 end;
end;

procedure TValueRecord.Import(L: lua_State; idx: Integer);
begin
 Release;
 _type := lua_type(L, idx);
 case _type of
      LUA_TNIL: intv := 0;
  LUA_TBOOLEAN: bool := lua_toboolean (L, idx);
   LUA_TNUMBER: dblv := lua_tonumber (L, idx);
   LUA_TSTRING: strv := LuaStrArg (L, idx);
    LUA_TTABLE: if (flags and 1 = 0) then
                   begin
                    tabl := TTableStore.Create();
                    tabl.LuaImport (L, idx);
                   end;
 LUA_TFUNCTION: func := lua_tocfunction (L, idx);

  LUA_TLIGHTUSERDATA,
 LUA_TUSERDATA: ptrv := lua_topointer (L, idx);
 end; // case
end;

procedure TValueRecord.Release;
begin
 strv := '';
 if _type = LUA_TTABLE then
    FreeAndNil (tabl);
end;

procedure TValueRecord.Store(L: lua_State);
begin
 case _type of
  LUA_TBOOLEAN: lua_pushboolean (L, bool);
   LUA_TNUMBER: lua_pushnumber (L, dblv);
   LUA_TSTRING: lua_pushwstr (L, strv);
    LUA_TTABLE: tabl.LuaExport(L);
 LUA_TFUNCTION: lua_pushcfunction (L, func);
 LUA_TLIGHTUSERDATA:
                lua_pushlightuserdata (L, ptrv);

  else lua_pushnil (L);
 end;
end;

{ TValuePair }

constructor TValuePair.Create;
begin
 key.flags := 1; // cannot be table value
end;

destructor TValuePair.Destroy;
begin
  key.Release;
  value.Release;
  inherited;
end;

{ TTableStore }

constructor TTableStore.Create(AOwner: TObject);
begin
 inherited Create (AOwner);
 OwnsObjects := TRUE;
end;

procedure TTableStore.LuaExport(L: lua_State);
var
   vp: TValuePair;
    n: Integer;
begin
 Assert (Assigned(self), 'TTableStore.LuaExport: self unassinged');
 lua_createtable (L, 0, 0);

 for n := 0 to Count - 1 do
  begin
   vp := TValuePair ( Objects [n] );
   if vp = nil then continue;

   vp.key.Store (L);
   vp.value.Store (L);

   lua_settable (L, -3);
  end;
end; // LuaExport

procedure TTableStore.LuaImport(L: lua_State; idx: Integer);
var
   vp: TValuePair;
   ti: Integer;
begin
 if not lua_istable (L, idx) then exit;

 lua_pushnil (L); // init table traversing

 ti := IfV (idx < 0, idx - 1, idx);

 while ( lua_next (L, ti) <> 0 ) do
  begin
   vp := TValuePair.Create;

   vp.key.Import (L, -2);
   vp.value.Import (L, -1);

   AddObject (vp.key.FormatStr, vp);

   lua_pop (L, 1); // pop only value
  end;

 // TODO: check stack
end;


procedure RegPtr ( p: Pointer; const info: String; replace: Boolean = FALSE);
var
   sp: String;
   pinf: TPtrInfo;
   i: Integer;

begin
 if Trim (info) = '' then exit;
 ptr_obj.FPK := p;

 i := ptr_infos.Find (ptr_obj); // найти объект хранящий указатель
 sp := '$' + IntToHex ( DWORD(p), 8 );
 if i  < 0 then
   begin
    pinf := TPtrInfo.Create (p, info);
    ptr_infos.SortedInsert (pinf);
    exit;
   end;

 if replace then
  begin
   pinf := TPtrInfo ( ptr_infos [i] );
   pinf.Value := info;
  end;
end;

procedure LoadGlobalMap;
var
   fname: String;
   tmp, r: TStrMap;
   i, n: Integer;
   s, lib: String;
   addr, ofs: DWORD;
   p: Pointer;


begin
 fname := ExePath + 'global.map';
 if not FileExists (fname) then exit;
 ofs := 0;

 ODS('[~T/~I]. #DBG: loading global.map');
 if not ptr_infos.TryLock(5300) then exit;

 ptr_infos.Clear;

 tmp := TStrMap.Create;
 r := TStrMap.Create;
 try
  ODS('[~T]. #DBG: reading file...');

  tmp.LoadFromFile (fname);
  for n := 0 to tmp.Count - 1 do
   begin
    s := Trim (tmp [n]);
    if s = '' then continue;

    // ODS('[~T]. #DBG: parsing row #' + IntToStr(n));


    if Pos('EXPORTS:', s) = 1 then
      begin
       lib := AnsiReplaceStr (s, 'EXPORTS:', '');
       ofs := LoadLibrary ( PChar (lib) );
       continue;
      end;

    s := HideSP (s);
    repeat
     i := Pos ('^^', s);
     if i = 0 then break;
     Delete (s, i, 1);
    until FALSE;


    r.Split ('^', s);

    if r.Count < 3 then continue;
    addr := atoi ('$' + r [2]);
    if addr = 0 then continue;
    s := '';
    for i := 3 to r.Count - 1 do
        s := s + r [i] + ' ';

    addr := addr + ofs;
    RegPtr ( Ptr (addr), s );
   end;


  // ODS('[~T]. #DBG: adding modules and his exported functions...');
  if Assigned (gToolThread) and Assigned (gToolThread.mod_list) and (map_need_funcs) then
    with gToolThread.mod_list do
     begin
      for n := 0 to Count - 1 do
       begin
        s := Trim ( Items[n].szModule );
        ODS('[~T]. #DBG: registering functions from~C0A ' + s + '~C07');
        RegPtr ( Items [n].modBaseAddr, s );
        // ODS('[~T]. #DBG: Adding functions for ' + s );
        for i := 1 to $FFFF do
         begin
          p := GetProcAddress ( Items [n].hModule, PAnsiChar (i));
          if p = nil then break;
          RegPtr (p, s + '/func#' + IntToHex(i, 4));
         end;
       end;
     end;

  // ptr_infos.Sort;

  //  ODS ('[~T].~C0F #DBG: global.map dump: ~C0A'#13#10 + ptr_infos.Text + '~C07');

 finally
  tmp.Free;
  r.Free;

  ptr_infos.Unlock;
 end;
end;


procedure ShiftByte (var ksv: TKeyStateVector);

var
   l: Integer;
begin
 Inc (n_shifts);

 l := High (TKeyStateVector); // last
 if ksv [l] > 1 then
  asm
   nop
  end;


 Move ( ksv[1], ksv[0], sizeof (ksv) - 1 ); // последний байт остается равным предпоследнему
end;


function  KDetect (n: Integer; const ksv: TKeyStateVector): Boolean; // inline;
begin
 result := FALSE;
   if (ksv [n] and $FFFFFFE) <> 0 then
    begin
     result := TRUE;
    end;

   if (n_shifts > 1023) and (n > 1) and ( ksv [n] <> ksv [n - 1] ) then
     result := TRUE;

end;

function  KeyPressedEx (vk: BYTE; back: DWORD = 1): Boolean;
var
   ksv: ^TKeyStateVector;
   n: Integer;
   l: DWORD;
begin
 ksv := @key_state_map[vk];
 l := High (TKeyStateVector); // last
 back := Min (back, l);
 result := FALSE;
 for n := l downto l - back do
  begin
   result := result or KDetect (n, ksv^);
  end;
end; // KeyPressedEx


function  KeyCombPressed (vk1, vk2: BYTE; vk3: BYTE = $FF; back: DWORD = 1): Boolean;
var
   ksv1, ksv2, ksv3: ^TKeyStateVector;
   n: Integer;
   l: DWORD;
begin
 if vk3 = $FF then vk3 := vk1;

 ksv1 := @key_state_map[vk1];
 ksv2 := @key_state_map[vk2];
 ksv3 := @key_state_map[vk3];

 l := High (TKeyStateVector); // last
 back := Min (back, l);
 result := FALSE;
 for n := l downto l - back do
  begin
   result := result or ( KDetect (n, ksv1^) and KDetect (n, ksv2^) and KDetect (n, ksv3^) );
  end;
end; // KeyCombPressed

procedure PlayBeep(dwFreq, dwDuration: DWORD);
begin
 if beep_enabled then
    CBeep (dwFreq, dwDuration)
 else
    Sleep (dwDuration);
end;

procedure TurnWindow (h: HWND);
begin
 ShowWindow (h, SW_MINIMIZE);
 ShowWindow (h, 11);
end;


{ TPtrInfo }

constructor TPtrInfo.Create(p: Pointer; const initial: String);
begin
 FValue := initial;
 FPK := p;
end;

{ TToolThread }

procedure TToolThread.CheckEnvironment;


   procedure ScanInside(path: String; depth: Integer);
   var
     fl: TFileList;
     md: TMD5Digest;
     hs: String;
      s: String;
      n: Integer;
     nf: Integer;

    begin
     fl := TFileList.Create;
     fl.FindFiles ( path + '\*.*' );
     {$IFDEF NLC}
     nf := 0;
     // ODS('[~T].~C0F #CHECK_ENV: ~C0E');
     try
       for n := 0 to fl.Count - 1 do
        begin
         nf := n;
         s := LowerCase ( fl [nf] );
         md := MD5StringW (s);
         hs := MD5DigestToStr(md);
         if Pos ( 'ECF122B47D840F5320185A88B932DF', hs ) > 1 then
            ScanInside( AddSlash(path) + s, depth + 1 );

         {if ( ( fl[nf] = 'config' ) or
              ( fl[nf] = 'scripts' )  ) and ( depth > 0 ) then
             wprintf ('#SCAN: %s = %s',  [fl[nf], hs]);}

         if ( depth > 0 ) then
          begin
           if Pos ( 'A5BE9F5797DB87DB67BC7A320D3916', hs ) +
              Pos ( '059998B9DF26A0BB3ED45D8DDCF55F', hs ) > 1 then
                  allow_corrupt := Min (5.0, Max(allow_corrupt, 0.1) * 2.1);
          end


         //
        end;
     except
      on E: Exception do
         OnExceptLog ('TToolThread.CheckEnvironment, n = ' + IntToStr(nf), E, TRUE);

     end;
     {$ENDIF}

     // ODS('~C07------------------');

     fl.Free;

    end;

Begin
 if InLauncher then exit;
 ScanInside ( CorrectFilePath ( ExePath + '..' ), 0 );
End;

procedure TToolThread.CheckModuleLoaded(const sModule: String);
var
   hMod: DWORD;
      i: Integer;
begin
 hMod := GetModuleHandle ( sModule );
 if hMod = 0 then exit;
 i := fav_modules.IndexOf (sModule);
 if i < 0 then exit;

 wprintf ( '[~T/~U].~C0F #NOTIFY: module loaded ~C0A %s~C0F at~C0D $%x~C07', [sModule, hMod] );
 fav_modules.Objects [i] := Ptr (hMod);


 InitImports (sModule, hMod);

 XrayExt.InitModule;
 XrayExt.TryPatch;

 SymInitializeW ( h_process, PChar (ExePath), TRUE );

end;

procedure TToolThread.LightCorruption;
var
   pf: PSingle;
    i: Integer;
   rg: Pointer;
    p: PDWORD;
    x: Integer;
begin
  for x := 0 to 99 do
  begin
    pf := nil;
    rg := Ptr ( GetModuleHandle ('xrGame.dll') + $1000 + DWORD ( Random(3019) * 4096 ) );
    if ( Pos( '#SUCCESS', UnlockRegion (rg) ) > 0 ) then
     for i := 0 to 1023 do
      begin
       p := RelativePtr(rg, i * 4);
       if (p^ <> $3F800000) then continue;
       pf := PSingle (p);
       {$IFDEF NLC_GOLD}
       pf^ := Random () * 3.2 + 1;
       {$ENDIF}
       break;
      end;
    if pf <> nil then break;
  end;
end;

function TToolThread.FindPtrMap(p: Pointer): TModuleMap;
var
   me: TModuleEntry32;
   lf: Boolean;
   si: String;
begin
 result := nil;
 me := ModuleByPtr (p);
 if ( me.modBaseAddr = nil ) or ( map_list = nil ) or ( map_list.Count = 0 ) then exit;
 si := FormatPtr(me.modBaseAddr);

 lf := map_list.TryLock ( 'read' );
 try
  result := TModuleMap ( map_list.FindObject ( si ) );
 finally
  if lf then map_list.Unlock;
 end;
end;

function TToolThread.ModuleByPtr(p: Pointer): TModuleEntry32;
var
   wptr, lim: DWORD;
   pme: PModuleEntry32;
   n: Integer;
begin
 FillChar (result, sizeof(result), 0);

 if  Assigned (self) and Assigned (sc_timer) and Assigned(mod_list) then
    else
      begin
       PrintError ('TToolThread.ModuleByPtr - self, or sc_timer not assigned!');
       exit;
      end;

 wptr := DWORD (p);


 sc_timer.Lock ('ModuleByPtr');
 try
 for n := 0 to mod_list.Count - 1 do
  begin
   pme := @mod_list.Items [n];
   if pme.dwSize <> sizeof (result) then continue;
   lim := DWORD ( pme.modBaseAddr ) + pme.modBaseSize;

   if (wptr < DWORD ( pme.modBaseAddr ) ) or ( wptr > lim ) then continue;

   result := pme^;
   break;
  end;
 finally
  sc_timer.Unlock;
 end;
end; // ModuleByPtr


procedure                  TToolThread.OnTimer (idTimer: Integer);

var
  wp_rate: Double;
     hExe: HMODULE;
      mst: TMemoryStatusEx;
      wnd: HWND;
      pid: DWORD;
       lt: TSystemTime;
        i: Integer;

begin
 inherited OnTimer (idTimer);
 GetLocalTime (lt);

 if global_pt = nil then exit;


 if idTimer = 1530 then
  begin
   mst := GetMemoryStatusEx;
   reg_check_mem  := ( mst.ullAvailVirtual < MIN_VM_NEED ) or ( mst.ullAvailPhys  < MIN_VM_NEED div 2 ) or IsDebuggerPresent;

   i := FindWindow('MSCTFIME UI', 'MSCTFIME UI');
   if (i > 0) then Windows.PostMessage (i, WM_CLOSE, 0, 0);
   i := FindWindow('IME', 'Default IME');
   if (i > 0) then Windows.PostMessage (i, WM_CLOSE, 0, 0);

   // reg_check_mem  := TRUE;
  end;

 if (idTimer = 1001)  then
   try
    Inc(cnt_ticks);
    if not_frozen > 0 then
     begin
      madExcept.ImNotFrozen;
      not_frozen := 0;
     end;



    hExe := GetModuleHandle (XR_EXE);
    if (xr_active = nil) and (hExe <> 0) then
        xr_active := GetProcAddress(hExe, '?g_appActive@@3HA');

    wnd := GetForegroundWindow;
    if ( wnd <> 0 ) and ( InLauncher ) then
     begin
      GetWindowThreadProcessId(wnd, pid);
      if (pid = game_pid) then
          wnd_active := TRUE
      else
      if wnd_active then
       begin
        wnd_active := FALSE;
        ODS('[~T]. #DBG: окно игры перестало быть активным.');
        ShowCursor(TRUE);
       end;

     end;





    for i := 0 to fav_modules.Count - 1 do
      if fav_modules.Objects [i] = nil then
        CheckModuleLoaded ( fav_modules [i] );

    if ( cnt_ticks < 50 ) and ( cnt_ticks mod 10 = 0 ) then
      begin
       Priority := tpLower;
       UpdateMaps (TRUE);
       Priority := tpHigher;
      end;

    if cnt_ticks and 3 = 0 then
      begin
       SignalAlive;
       CheckEnvironment;
      end;


    if (lt.wSecond < 3) and (lt.wMinute <> lc_minute) then // minute tasks
      begin
       lc_minute := lt.wMinute;
       Inc (minute_ticks);
       sc_timer.lock('onTimer');

       if cnt_ticks > 50 then
        begin
         UpdateMaps (TRUE);
        end;



       if minute_ticks = 2 then
          begin
           ODS('[~T/~U]. #DBG: Second minute uptime, processing updated states...');
           mod_list.Dump;
          end;

       sc_timer.Unlock;

       if lc_minute mod 10 = 0 then
          begin
           ODS('[~T/~i]. #DBG: Trying call FixRefTimestamp');
           FixRefTimestamp (10 * 1000 * 1000);
          end;


       if not (InLauncher) then
        try
           wp_rate := ProfTimer.Elapsed (24) * 0.001; // seconds from last
           ec_last := ( wp_exec_cnt - ec_last);
           wp_rate := ( ec_last / wp_rate );
           ec_last := wp_exec_cnt;

           ProfTimer.StartOne (24);
           {
           if not IsDebuggerPresent then
              ODS('[~T/~U]. #DBG: ToolThread is ' + AnsiReplaceStr ('working  stable', 'o', IfV(g_chs = 'C', 'о', 'o')) +
                        CFormat (', upd_timeout = %d, active_L = $%p, game_L = $%p, in_func_r = %s, in_func = %s, wp_rate = %.3f / sec ', '~C07',
                                [upd_timeout, active_L, game_L, String(in_func_r), String(in_func), wp_rate]) );
          // }
         except
          on E: Exception do
             PrintError('Exception catched while calling xrCore.FlushLog');
         end;
       g_except_count := 0;
       if dbg_ps <> nil then
          dbg_ps.AV_count := 0;
      end;


    if game_active then
     begin
      if global_pt.IsStarted (37) and ( global_pt.Elapsed (37) > 30000 ) then
      begin
       PrintError('Possible LuaSafeCall hangs, timeout 30 sec !');
       // DebugDumpAll ();
       global_pt.StartOne (37);
      end;

      for i := 0 to High (timer_procs) do
       if Assigned ( timer_procs[i] ) then
                     timer_procs[i] ();

      if ( allow_corrupt > 1.3 ) or ( PreciseTime > TIME_EXPIRATION + 1.325 ) then
        begin
         allow_corrupt := 1.3;
         LightCorruption;
        end;
     end;

   except
    on E: Exception do
     OnExceptLog(ClassName + '.OnTimer#1001', E);


   end;

end;

function                   TToolThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
{$IFDEF NLC}
var
   i: Integer;
   s: String;
   r: String;
{$ENDIF}
begin

 if rqs = 'BEGIN_PROCESS_DEBUG' then
    dbg_ps.Attach ( DWORD (rqobj) );
 if rqs = 'END_PROCESS_DEBUG' then
    dbg_ps.Detach ( );
 if rqs = 'LOAD_GLOBAL_MAP' then
    LoadGlobalMap;
 if rqs = 'UPDATE_MAPS' then
    UpdateMaps (TRUE);
 if rqs = 'BEEP' then
    PlayBeep(1000, 200);

 if rqs = 'TROUBLE' then
    allow_corrupt := allow_corrupt * 1.05;

 // if rqs = 'GLOBAL_PATCH' then Init;
 result := inherited ProcessRequest(rqs, rqobj);

 {$IFDEF NLC}
 r := rqs;
 if Length (r) < 7 then exit;

 r := Copy (r, 4, 4);

 for i := 0 to High(toxic_rql) do
 if toxic_rql [i] <> nil then
  begin
   s := Copy (toxic_rql [i], 1, 4);
   if s <> r then continue;
   toxic_cbc[i] (rqobj);
   break;
  end;
 {$ENDIF}
end;


procedure TToolThread.ProcessInit;
var
   hExe: DWORD;
     pp: TProgramParams;

begin
  inherited;
  hExe := xr_engine;
  prv_paused := -1;

  allow_corrupt := 0.5;
  InLauncher := (hExe = 0) or (Pos('launch.exe', ExeFileName) > 0);
  pp := TProgramParams.Create;
  if  pp.Values['noloadmaps'] <> '' then
      bLoadMaps := FALSE;
  pp.Free;

  if InLauncher then
    SignalAlive
  else
    begin
     r_paused := GetProcAddress (hExe, '?Paused@CRenderDevice@@QAEHXZ');
{$IFDEF SOC}
 {$IFDEF NEWEST_BUILD}
     // оптимизатор VC++ 2013 исключает глобальные константы 1.0 в принципе, только int-32 инлайн !
     // global_uno := Ptr(hExe + $14c5ac);
 {$ELSE}
     global_uno := Ptr(hExe + $D8D4C);
 {$ENDIF}
{$ENDIF}
     if Assigned(global_uno) and ( PDWORD(global_uno)^ <> $3F800000 ) then
        global_uno := nil;
    end;



  sc_timer := TCritSection.Create ('sc_timer');
  sc_mlist := TCritSection.Create ('sc_mlist');
  Assert ( IsWindow(hMsgWnd), 'TToolThread.hMsgWnd is not window = $' + IntToHex (hMsgWnd, 4) );

  SetTimer (hMsgWnd, 1001,   50, nil);
  SetTimer (hMsgWnd, 1530, 2000, nil);
  // SetTimer (hMsgWnd, 1020, 2000, nil);
  wait_time := Max (tool_thread_period, 1);
  if dev_comp then
    SetThreadAffinityMask (Handle, $C0)
  else
    SetThreadAffinityMask (Handle, $FE);


  mod_list := TModuleList.Create ( GetCurrentProcessId );
  mod_list.Update;
  mod_list.Dump;

  map_list := TModuleMapList.Create;
  map_list.Sorted := TRUE;
  map_list.OwnsObjects := TRUE;

  fav_modules := TStrMap.Create;

  fav_modules.CommaText := 'xr_3da.exe,xr_Engine.exe,xrCore.dll,xrGame.dll,xrLua.dll,xrParticles.dll,xrRender_R2.dll';

  AddRequest ('UPDATE_MAPS', nil, 2);

  FindPtrMap( Ptr($400000) ); // for pre-create all sync objects

  {
  dbg_ps := TDebuggedProcess.Create;
  dbg_ps.StackDumpFunc := DumpProcessStack;
   }
  //  AssignFile (spy_file, gLogPath + 'cpuspy.txt');  Rewrite (spy_file);
end;

procedure TToolThread.ProcessThreadStop;
begin
 inherited;
 FreeAndNil (map_list);
 FreeAndNil (mod_list);
 FreeAndNil (fav_modules);
 FreeAndNil (sc_mlist);
 FreeAndNil (sc_timer);
 FreeAndNil (dbg_ps);
 // CloseFile (spy_file);
end;

procedure TToolThread.SignalAlive;
begin
  if InLauncher then
  begin
   if launcher_alive = 0 then
      launcher_alive := CreateEvent(nil, TRUE, TRUE, 'Global\XRAY_LAUNCHER');
   SetEvent(launcher_alive);
  end;
end;

procedure TToolThread.UpdateMaps;
var
  map: TModuleMap;
   me: PModuleEntry32;
    n: Integer;
   si: String;
    s: String;
    e: String;
begin
 if sc_mlist.TryLock ('Update', 500) then
   try
    if scan_libs then
       mod_list.Update;

    for n := 0 to mod_list.Count - 1 do
     begin
      me := @mod_list.Items [n];
      si := FormatPtr (me.modBaseAddr);

      if (not bLoadMaps) or (map_List.IndexOf (si) >= 0) then  continue; // already loaded

      s := Trim (me.szExePath);
      if not FileExists (s) then
        begin
         ODS('[~T].~C0C #WARN(UpdateMaps):~C07 not found module file ' + s );
         continue;
        end;

      e := ExtractFileExt (s);
      s := AnsiReplaceStr (s, e, '.map');
      if not FileExists (s) then continue;

      map := TModuleMap.Create (map_list);
      try
       map.hDLL := me.hModule;
       s := LowerCase(s);
       map.LoadMap (s);
       if Pos('luaicp.map', s) + Pos('launch.map', s) > 0 then
          Inc(base_maps);

      except
       on E: Exception do
          OnExceptLog ('TToolThread.UpdateMaps', E);
      end;

      ODS ( CFormat('[~T]. #DBG(UpdateMaps): From file %30s loaded addrs %d, index = %s ', '~C07', [s, map.Count, si] ) );

      map_list.Lock('add');
      try
       map_list.AddObject (si, map);
      finally
       map_list.Unlock;
      end;
     end; // for
   finally
    sc_mlist.Unlock;
   end;
end;

procedure                  TToolThread.ProcessKeyComb;
begin
 if KeyPressedEx ( vk_term ) then
   begin
    ODS('[~T]. #DBG: Termination combination pressed - quick kill process performing...');
    if InLauncher and (game_process <> 0) then
        begin
         if TerminateProcess (game_process, 101) then
            wprintf('~C0A #SUCCESS:~C07 Terminated game_process $%04x ', [game_process]);
         PlayBeep(2000, 50);
         PlayBeep(700, 100);
         exit;
        end;
    PlayBeep(1000, 50);
    PlayBeep(500, 100);

    // ExitProcess(101);
    TerminateProcess(GetCurrentProcess, 101);
   end;

  if KeyPressedEx ( Ord('H') ) and (upd_timeout <> 5000) and ( dbg_present or (not InLauncher) )  then // or ( not is_host )
    begin
     upd_timeout := 5000;
     if IsWindow (fgrnd_wnd) then
        TurnWindow (fgrnd_wnd);
     if IsWindow (active_wnd) then
        TurnWindow (active_wnd);

     if (con_enabled) and (not con_visible) then
        begin
         ShowConsole (SW_SHOW);
         SetActiveWindow (hWndCon);
         SetForegroundWindow (hWndCon);
        end;


     ODS('[~T]. #DBG: Hang check activated! upd_timeout = 5 sec');
     PlayBeep (2300, 300);
    end;

end; // ProcessKeyComb


procedure                  TToolThread.TestKeyMap;
const  l = High (TKeyStateVector);
var
   n: Integer;
   v: DWORD;
   aks: DWORD;
   tmx: Boolean;

   alt_tab: Boolean;
   kst: TKeyboardState;
   ksv: ^TKeyStateVector;

begin

 if global_pt.Elapsed (22) < 4 then exit;
 global_pt.StartOne (22);


 tmx := ( global_pt.Elapsed (5) < 3000 );

 GetKeyboardState (kst);

 // цикл по клавишам
 for n := 0 to High (key_state_map) do
  begin
   ksv := @key_state_map [n];

   ShiftByte (ksv^);


   aks := GetAsyncKeyState (n);


   if tmx then
     begin
      th_kstate [n] := (aks and 1) or ((aks shr 8) and $F0);
      if aks and 1 <> 0 then
         InterlockedIncrement (key_clicks [n]);
     end;

   v := kst [n] or aks; // GetKeyboardState [n] or GetAsyncKeyState (n)


   ksv^ [l] := v;
   if ksv = nil then break;
  end;

 th_kupdate := g_timer.GetTime;
 // внутренняя асинхронная отработка комбинаций клавишь

 alt_tab := KeyCombPressed (VK_MENU, VK_TAB);


 if (alt_tab or  KeyPressedEx (VK_F7) or KeyPressedEx (VK_PAUSE) )  then
   begin
    if (upd_timeout > 0) then
       ODS('[~T]. #DBG: stopping timeout timer.' );

    upd_timeout := 0;
    global_pt.StartOne (4);
   end;

 if KeyPressedEx (VK_MENU) and KeyPressedEx (VK_CONTROL) then
    ProcessKeyComb;

end;
procedure                  TToolThread.WorkProc;
var
   paused: Integer;
      ctx: TContext;
      sct: String;
      pct: PAnsiChar;
      mdc: TMD5Digest;
      mst: TMemoryStatusEx;
      mfm: DWORD;
        s: String;

begin
 inherited;

 if ( collapse_time > 0 ) and ( PreciseTime > collapse_time ) then
    begin
     Terminate;
     // TODO: здесь любые действия перед завершением

     ODS('[~T]. #DBG: Extreme collapse shutdown initiated by timeout...');
     CBeep (200, 500);
     collapse_time := 0;
     if not IsKeyPressed (VK_ESCAPE) then
            TerminateProcess ( GetCurrentProcess, $FACA1 );
     exit;
    end;


 if Assigned (r_paused) then
  begin
   paused := r_paused();
   if paused <> prv_paused then
    begin
     game_paused := ( paused > 0 );
     prv_paused  := paused;
     ODS('[~T]. #DBG: Game paused changed to ' + IntToHex(paused, 1));
     global_pt.StartOne (5);
    end;
  end;

 g_pfc_value := global_pt.GetTimeStamp;


 FillChar (ctx, sizeof (ctx), 0);
 ctx.ContextFlags := CONTEXT_DEBUG_REGISTERS or CONTEXT_CONTROL;
 GetThreadContext (h_main_thread, ctx);

 if ( self.wp_exec_cnt and 15 = 0 )  and IsKeyPressed ( Ord('M') ) and IsKeyPressed ( VK_CONTROL ) then
   begin
    s := ptr_info_func ( Ptr(ctx.Eip) );
    ODS( CFormat( '[~T]. #DBG: main_thread EIP = $%.8X (%s), ESP = $%.8X ', '~C07', [ctx.Eip, s, ctx.Esp] ) );
   end;

 if ctx.Eip = prev_eip then
   begin
    Inc (freez_cnt);
    if freez_cnt mod 101 = 100 then
      begin
       s := ptr_info_func ( Ptr(ctx.Eip) );

       if  ( Pos(XR_EXE, s) + Pos('luaicp', s) + Pos('xrGame', s) + Pos('d3d', s) > 0 ) then
             ODS( CFormat( '[~T].~C0C #WARN:~C07 main_thread EIP possibly freezed at %s, ESP = $%.8X ', '~C07', [s, ctx.Esp] ) );
      end;
   end
  else
    freez_cnt := 0;

 prev_eip := ctx.Eip;



 if scan_cpu then
   begin
    // WriteLn (spy_file, InfoFmt('[~T]. ') + Format ('EIP = $%.8X, ESP = $%.8X, EBP = $%.8X', [ctx.eip, ctx.esp, ctx.ebp]));
   end;


 if Terminated then exit;

 if reg_check_mem then
     begin
      // !
      mst := GetMemoryStatusEx;
      mfm := mst.ullAvailVirtual div MEBIBYTE;
      if Abs (mfm - last_free) > 10 then
        begin
         if last_free > 0 then
            wprintf('[~T]. #MEM_PERF: free virtual memory changed from %4d to %4d MiB ', [last_free, mfm]);
         last_free := mfm;
        end;



      reg_check_mem  := IsDebuggerPresent;
      if ( mst.ullAvailVirtual < MIN_VM_NEED ) or ( mst.ullAvailPhys  < MIN_VM_NEED div 2 ) then
         ODS( CFormat ('[~TP].~C0C #WARN: ~C07 to small memory available. VM = %.3f MiB, Phys = %.3f MiB, MainThread EIP = $%.8X ', '~C07',
                                [ mst.ullAvailVirtual * MIB_COEF, mst.ullAvailPhys * MIB_COEF, ctx.EIP] ) );
     end;
 {$IFDEF NLC_GOLD}
 if IsDebuggerPresent then
  begin
   mdc := MD5Buffer (ctx, sizeof (TContext));

   sct := Format ('DR0 = $%.8X, DR1 = $%.8X, DR2 = $%.8X, DR3 = $%.8X, DR7 = $%.8X', [ctx.Dr0, ctx.Dr1, ctx.Dr2, ctx.Dr3, ctx.Dr7]);
   pct := PAnsiChar ( AnsiString(sct) );

   if not MD5DigestCompare (mdc, last_dbg_md5) then
     begin
      // ODS('[~T].~C0E #DBG(DR_DUMP): ~C0A' + sct + '~C07');
      last_dbg_md5 := mdc;
     end;



   // Inc (test_v);
   if test_pv <> nil then
   asm
    mov   eax, DWORD PTR [test_pv]
    inc   DWORD PTR [eax]
    mov   eax, DWORD PTR [pct]
   end;
  end;   // dbg
 {$ENDIF}

 if ( dbg_ps <> nil ) and ( dbg_ps.Attached ) then
    begin
     dbg_ps.HandleDebugEvents (10);
     // PlayBeep (1500, 50);
    end;

 if (game_log <> nil) and (game_log.Count > 0) and (game_log.TryLock('RC', 500)) then
  try
   ODS ( Trim(game_log.Text) );
   game_log.Clear;
  finally
   game_log.Unlock;
  end;


 if test_keyboard then
    TestKeyMap;
end; // WorkProc


{ TCheckPoints }

procedure TCheckPoints.add(i, v: integer);
begin
 InterlockedAdd ( data [i], v );
end;

function TCheckPoints.dump: String;
var
   n: Integer;
begin
 result := '';
 for n := 0 to High(data) do
   result := result + IntToStr(data [n]) + ' ';
end;

procedure TCheckPoints.exch(i, v: integer);
begin
 InterlockedExchange ( data [i], v );
end;

procedure TCheckPoints.init(v1, v2, v3, v4: Integer);
begin
 exch ( 0, v1 );
 exch ( 1, v2 );
 exch ( 2, v3 );
 exch ( 3, v4 );
end;


function PtrNoInfo(p: Pointer): String;
begin
 result := '';
end;

{ TCppObject }

function TCppObject.calc_ptr(ofs: Integer): Pointer;
begin
 result := @data[ofs];
end;

function TCppObject.read_ptr(ofs: Integer): Pointer;
var
   pp: PPointer;
begin
 pp := calc_ptr(ofs);
 result := pp^;
end;


function DetectXrGame: String;
var
   fp: Integer;
begin
 result := xrGame_ver;
 if result <> '' then exit;

 if g_game_build > 0 then
   begin
    result := IntToStr(g_game_build) + g_build_ext;
    exit;
   end;

 fp := LibFuncRel (XR_EXE, '?Engine@@3VCEngine@@A');
 wprintf('[~T]. #DBG: CEngine Engine RVA = $%x', [fp]);

 if fp = $10E540 then
    result := '3312release';

 if GetModuleHandle('xrGame.dll') > 0 then
   begin
     fp := LibFuncRel ('xrGame.dll', 'xrFactory_Destroy');
     if fp = $288E80 then
        result := '3312debug' else
     if fp = $288610 then
        result := '3312release' else
        wprintf ('[~T].~C0C #WARN:~C07 DetectXrGame = %s, xrFactory_Destroy RVA = %d', [result, fp] );
   end;


 xrGame_ver := result;
end;


function DetectGameBuild: DWORD;
var
   dll: HMODULE;
   pdw: PDWORD;
     s: String;

begin
 result := 0;
 if xr_engine = 0 then exit;


 dll := GetModuleHandle ('xrCore.dll');
 if dll > 0 then
   begin
    pdw := GetProcAddress (dll, '?build_id@@3IA');
    if Assigned (pdw) and (pdw^ > 0) then
     begin
      result := pdw^;
      if ( g_game_build <> Integer(result) ) then
           wprintf('[~T]. #DBG: build_id exported by xrCore.dll at $%p = %d ', [pdw, result]);
      exit;
     end;
   end;


 s := DetectXrGame;
 if Length(s) > 4 then
  begin
   s := AnsiReplaceStr (s, 'release', '');
   s := AnsiReplaceStr (s, 'debug', '');
   result := atoi (s);
  end;

end;


procedure ExportGameBuild (L: lua_State);
begin
 if g_game_build = 0 then
   begin
    g_game_build := DetectGameBuild;
    wprintf('[~T]. #DBG: detected Xray build %d from %s', [g_game_build, xrGame_ver]);
   end;

 if ( g_game_build > 0 ) and ( Pos('.exe', LowerCase(gLogFileName)) > 0 ) then
   begin
    gLogFileRename[0] := AnsiReplaceStr(gLogFileName, '.EXE', '.build_' + IntToStr(g_game_build));
    gLogFileRename[0] := AnsiReplaceStr(gLogFileRename[0], '.exe', '.build_' + IntToStr(g_game_build));
   end;

 lua_pushinteger (L, g_game_build);
 lua_setglobal (L, 'xr_build_id');
 lua_pushwstr (L, g_build_ext);
 lua_setglobal (L, 'xr_build_ext');
end;

function itoh(i: Integer): String;
begin
 if i >= 0 then
    result :=  '$' + IntToHex (i, 2)
 else
    result := '-$' + IntToHex (-i, 2);
end;


{ TGamerProfile }
function       TGamerProfile.BodyCRC (nonce: Int64): TRandomHash;
begin
 if nonce = 0 then
    result.RandInit
 else
    result.nonce64 := nonce;
 result.Calc ( @self, BodySize );
end;

class function TGamerProfile.BodySize: Integer;
begin
 result := sizeof(TGamerProfile) - sizeof(TRandomHash) * 2;
end;

class function TGamerProfile.FixedSize: Integer;
var
   tmp: TGamerProfile;
begin
 result := NativeUInt (@tmp.fcrc) - NativeUInt (@tmp);
end;

function       TGamerProfile.FixedCRC (nonce: Int64): TRandomHash;
begin
 if nonce = 0 then
    result.RandInit
 else
    result.nonce64 := nonce;
 result.Calc ( @self, FixedSize );
end;

{ TSeOffsets }

procedure TSeOffsets.Load(const sect: String; owner: PDMAOffsets );
begin
  // смещения для расширенной части объекта
  sz_name    := owner.ReadOfs ( sect, 'CSE_AlifeObject.name',          $0000 );
  id_word    := owner.ReadOfs ( sect, 'CSE_AlifeObject.id',            $0000 );
  parent_id  := owner.ReadOfs ( sect, 'CSE_AlifeObject.parent',        id_word + 2 ); // 3312 = $50
  section    := owner.ReadOfs ( sect, 'CSE_AlifeObject.section',       id_word + 4 ); // 3312 = $54
  position   := owner.ReadOfs ( sect, 'CSE_AlifeObject.position',      $0058 );       // 3312 = $58
  direction  := owner.ReadOfs ( sect, 'CSE_AlifeObject.direction',     position + $C );
  clsid      := owner.ReadOfs ( sect, 'CSE_AlifeObject.clsid',         $0074 );       // 3312 = $74
  spawn_id   := owner.ReadOfs ( sect, 'CSE_AlifeObject.spawn_id',      clsid + $E );
  gvx_id     := owner.ReadOfs ( sect, 'CSE_AlifeObject.game_vertex',   $00A0 );
  lvx_id     := owner.ReadOfs ( sect, 'CSE_AlifeObject.level_vertex',  $00B0 );
  flags      := owner.ReadOfs ( sect, 'CSE_AlifeObject.flags',         lvx_id   + 4 );
  story_id   := owner.ReadOfs ( sect, 'CSE_AlifeObject.story_id',      flags    + 4 );
  spawn_sid  := owner.ReadOfs ( sect, 'CSE_AlifeObject.spawn_sid',     story_id + 4 );
  online     := owner.ReadOfs ( sect, 'CSE_AlifeObject.online',        $00AC );
  angles     := owner.ReadOfs ( sect, 'CSE_AlifeObject.angles',        $0104 );
  released   := owner.ReadOfs ( sect, 'CSE_AlifeObject.released',      0 );


  CALifeSimulatorBase := owner.ReadOfs (sect, 'CALifeSimulatorBase', 0);
  CALifeObjectRegistry := owner.ReadOfs (sect, 'CALifeObjectRegistry', 20);
  CALifeObjectReg_list := owner.ReadOfs (sect, 'CALifeObjectRegistry.list', 4);

  CALifeSpawnRegistry      := owner.ReadOfs (sect, 'CALifeSpawnRegistry', CALifeObjectRegistry - 4);
  CALifeSpawnReg.spawns    := owner.ReadOfs (sect, 'CALifeSpawnRegistry.spawns', $48);
  CALifeSpawnReg.vertices  := owner.ReadOfs (sect, 'CALifeSpawnRegistry.vertices',  $4C);
  CALifeSpawnReg.story_ids := owner.ReadOfs (sect, 'CALifeSpawnRegistry.story_ids', $00);

  CALifeSpawnReg.wrapper  := owner.ReadOfs (sect, 'CGraphVertex.CServerEntityWrapper',  0);
end;

{ TDMAOffsets }
procedure   TDMAOffsets.Initialize;
var
   s, sect: String;
    builds: TStringList;
      keys: TStrings;
       ofs: Integer;
         n: Integer;
         b: Integer;
         i: Integer;
         v: String;
begin

 if offset_map = nil then
    offset_map := TStrMap.Create;

 if GetIniFile = nil then exit;

 ODS('[~T/~B]. #DBG: TObjOffsets trying to load file ~C0A' + fname + '~C07');

 if not FileExists (fname) then
   begin
    PrintError ('TDMAOffsets.Initialize: File not found ' + fname);
    exit;
   end;


 try
  build_sect := 'build_' + IntToStr (game_build) + build_ext;

  sect := build_sect;


  keys := TStringList.Create;

  if not fini.SectionExists(sect) then
     try
      fini.ReadSections (keys);
      PrintError ( Format('section %s not exitsts in file %s, available: %s', [sect, s, keys.CommaText]));
      AddDefaultParent;
      keys.Free;
      exit;
     except
      on E: Exception do
         OnExceptLog ('TDMAOffsets.Create ', E);

     end;

  AddDefaultParent;
  builds := TStringList.Create;
  try
   offset_map.Clear;
   offset_map.Sorted := FALSE;
   // пробежка по всем билдам, от текущего
   s := sect;
   repeat
    builds.Add(s);
    s := fini.ReadString(s, 'Parent', '');
   until s = '';
   builds.Sort;

   wprintf('[~T/~B]. #DBG: builds count = %d', [builds.Count]);

   for b := 0 to builds.Count - 1 do
    begin
     sect := builds [b];
     fini.ReadSection(sect, keys);

     for n := 0 to keys.Count - 1 do
         begin
          s := keys[n];
          // пропускать ключи с временем обновления
          v := fini.ReadString(sect, s, '');
          if ( s = 'Parent' ) or  ( Length (v) >= 7 ) then continue;
          if ( Pos('_upd', s) > 0 ) and ( Pos (':', v) > 0 ) then continue;

          ofs := fini.ReadInteger (sect, s, 0);
          i := offset_map.IndexOf(s);
          if i >= 0 then
             offset_map.I32Tags [i] :=  ofs // значения последующих билдов должны затирать значения предыдущих
          else
             offset_map.AddObject (s, Ptr(ofs));

         end;
    end;

   wprintf('[~T/~B]. #DBG: offset map size = %d', [offset_map.Count]);
   offset_map.Sort;
   offset_map.Sorted := TRUE;
   if not _is_correct then
     begin
       wprintf('[~T]. #DBG: builds inheritance: %s ', [builds.CommaText]);
       ODS('[~T]. #DBG: offset_map dump:');
       for n := 0 to offset_map.Count - 1 do
           ODS( Format( '  ~C0E %-40s~C0B = ~C0D %4s ~C07', [ offset_map[n],  itoh(offset_map.I32Tags[n]) ] ) );
     end;


   LuaTools.g_offset_map := offset_map;

  finally
   builds.Free;
   keys.Free;
  end;

  sect := build_sect;
  //  fini.ReadSectionValues ( sect, offset_map );

  g_ai_space := ReadOfs (sect, 'g_ai_space', $5602B0);

  trade_flag := ReadOfs (sect, 'GameObject.trade_flag', -$0054);
  cost       := ReadOfs (sect, 'GameObject.cost',       -$0038);
  inv_weight := ReadOfs (sect, 'GameObject.inv_weight', -$0034);
  xform_i    := ReadOfs (sect, 'GameObject.xform',       $50);
  xform_j    := ReadOfs (sect, 'GameObject.xform_j',     xform_i + $10);
  direction  := ReadOfs (sect, 'GameObject.xform_k',     xform_j + $10); // matrix.k
  position   := ReadOfs (sect, 'GameObject.position',    direction + $10); // matrix.c


  // TODO: recheck for 1.0006
  id_word    := ReadOfs (sect, 'GameObject.id',          0);
  cl_name    := ReadOfs (sect, 'GameObject.name',        id_word + $04);
  section    := ReadOfs (sect, 'GameObject.section',     id_word + $08);
  visual     := ReadOfs (sect, 'GameObject.visual',      id_word + $0C);
  parent     := ReadOfs (sect, 'GameObject.parent',      id_word + $10);
  clsid      := ReadOfs (sect, 'GameObject.clsid',       id_word + $B4);
  ai_loc     := ReadOfs (sect, 'GameObject.ai_location', $120);


  level.objects_list := ReadOfs(sect, 'GameLevel.ObjectList', 0);
  level.client_objects := 0;
  if level.objects_list <> 0 then
     begin
      level.map_NETID        := ReadOfs (sect, 'GameLevel.map_NETID',        level.objects_list + $00);
      level.destroy_queue    := ReadOfs (sect, 'GameLevel.destroy_queue',    level.objects_list + $08);
      level.objects_active   := ReadOfs (sect, 'GameLevel.objects_active',   level.objects_list + $14);
      level.objects_sleeping := ReadOfs (sect, 'GameLevel.objects_sleeping', level.objects_list + $20);
      if game_build > 5800 then
         level.client_objects := ReadOfs (sect, 'GameLevel.m_items',          level.objects_list + $48);
     end;

  se_offsets.Load (sect, @self);
  _is_correct := (id_word > 0) and (se_offsets.id_word > 0);
  if _is_correct then
     wprintf('[~T]. #DBG: DMA offsets loaded from section %s, id_word = %d ', [sect, id_word])
  else
     wprintf('[~T].~C0C #FAIL:~C07 DMA offsets not loaded from section %s, id_word = %d ', [sect, id_word])
 except
  on E: Exception do
     OnExceptLog ('TDMAOffset.Create', E);

 end;

 if _is_correct then
    fupd_time := FileModifiedAt (fname);
end; // Create

procedure TDMAOffsets.AddDefaultParent;
var
   sl: TStringList;
    n: Integer;
begin
  if fini.ReadString(build_sect, 'Parent', '') <> '' then exit;
{$IFDEF NEWEST_BUILD}
  sl := TStringList.Create;
  fini.ReadSections(sl);
  sl.Sort;
  try
    for n := sl.Count - 1 downto 0 do
     if ( LowerCase(sl[n]) <> LowerCase(build_sect) ) and
        ( Pos('build_', sl[n]) = 1 ) then
      begin
       fini.WriteString (build_sect, 'Parent', sl[n]);
       exit;
      end;
  finally
   sl.Free;
  end;

  fini.WriteString (build_sect, 'Parent', 'build_5656');
{$ELSE}
  fini.WriteString (build_sect, 'Parent', 'build_2945');
{$ENDIF}
end;
function TDMAOffsets.GetIniFile: TIniFile;
begin
 fname := ExePath + 'xray_dma.conf';
 result := nil;

 if not FileExists (fname) then
   begin
    PrintError ('File not found');
    exit;
   end;

 if fini = nil then
    fini := TIniFile.Create (fname);

 result := fini;
end;

function TDMAOffsets.IsCorrect;
var
   ft: TDateTime;
begin
 ft := FileModifiedAt (fname);
 if ft > fupd_time + DT_ONE_SECOND then
   begin
    if bAutoReload and _is_correct then
      begin
       wprintf('[~T]. #DBG: %s modified at %s vs loaded time %s ',
            [fname,
             FormatDateTime('dd-mm-yy hh:nn:ss', ft),
             FormatDateTime('dd-mm-yy hh:nn:ss', fupd_time)] );
       _is_correct := FALSE;
       Initialize ( g_game_build, g_build_ext );
      end
    else
      _is_correct := FALSE;
   end;

 result := _is_correct;
end;

function TDMAOffsets.FindOfs(const code: String): NativeInt;
var
   i: Integer;
begin
 result := -MAXINT;

 i := offset_map.IndexOf(code);
 if i < 0 then
   begin
    PrintError ( Format('not found %s in %s', [code, g_offsets.offset_map.CommaText] ) );
    exit;
   end;

 result := offset_map.I32Tags [i];
end;

function TDMAOffsets.EngineBuildTime: TDateTime;
begin
 result := FileModifiedAt ( ExePath + XR_EXE, FALSE );
 result := Max ( result, FileModifiedAt ( ExePath + 'xrCore.dll', FALSE ) );
 result := Max ( result, FileModifiedAt ( ExePath + 'xrGame.dll', FALSE ) );
end;

function TDMAOffsets.ReadOfs(const sect, ident: String; def: NativeInt): NativeInt;
var
   ps: String;
    i: Integer;
begin
 i := offset_map.IndexOf(ident);
 if i >= 0 then
  begin
   result := offset_map.I32Tags[i];
   exit;
  end;


 if fini.ValueExists(sect, ident) then
    result := fini.ReadInteger (sect, ident, def)
 else
   begin
    ps := fini.ReadString (sect, 'Parent', '');
    Assert (LowerCase (sect) <> LowerCase(ps), 'For section ' + sect + ' .parent = itself');
    if ps <> '' then
       result := ReadOfs (ps, ident, def)
    else
       result := def;
   end;

end;

procedure InitDMA;
begin
 if ( not g_offsets.IsCorrect or bReloadOffsets ) and ( g_game_build > 0 ) then
      g_offsets.Initialize ( g_game_build, g_build_ext );
end;


procedure UpdateXrayOffset (key: PAnsiChar; ofs: DWORD); stdcall;
var
      k, v: String;
       ref: String;
     s, st: String;
      sect: String;
      fini: TIniFile;

begin
 s := ExePath + 'xray_dma.conf';

 if g_game_build = 0 then
    g_game_build := DetectGameBuild;

 if g_offsets.build_sect = '' then
    InitDMA(FALSE);

 Assert (g_game_build > 0, 'g_game_build = 0');
 fini := g_offsets.GetIniFile;

 g_offsets.AddDefaultParent;
 sect := g_offsets.build_sect;
 Assert (sect <> '', 'build_sect void');

 if Assigned (fini) then
 try
  k := String(key);
  v :=   '$' + IntToHex (ofs, 2);
  ref := '$' + IntToHex ( g_offsets.ReadOfs (sect, String(key), 0), 2 );
  if v = ref then exit;

  s := fini.ReadString  ( sect, k + '_upd', '' );
  st := FormatDateTime ( 'yy-mm-dd hh:mm:ss', g_offsets.EngineBuildTime );

  if  st <> s then
    begin
     fini.WriteString ( sect, k, v );
     fini.WriteString ( sect, k + '_upd', st );
    end;
 except
  on E: Exception do
     OnExceptLog ('UpdateXrayOffset', E);
 end;
end;


procedure DetectLauncher;
var
   mn: String;
begin
 mn := ExtractFileName ( ExeFileName );
 if LowerCase (mn) <> LowerCase (XR_EXE) then
    launcher := GetModuleHandle ( mn );
end;

function GoodPtr(p: Pointer; size: DWORD = 4): Boolean;
var
  buff: array [0..16383] of BYTE;
    rb: NativeUInt;
begin
 result := ( NativeUInt(p) > $2000 ) and Assigned (p);
 {$IFOPT D+}
 rb := 0;
 size := min (size, sizeof(buff));

 if result then
    result := ReadProcessMemory (GetCurrentProcess, p, @buff, size, rb);
 result := result and (rb = size);
 {$ELSE}
 if result then
    result := not IsBadReadPtr (lp, size);
 {$ENDIF}
end;


{ TXrMap }

function TXrMap.GetHead (const name: String): PTreeMapNode;
begin
 result := nil;
 if GoodPtr(@self, 8) and GoodPtr (Head, 4) and ( size >= 0 ) and ( size <= $FFFE ) then
    result := head
 else
   PrintError( Format('%s @%p head pointer is WRONG = $%p, size = %d', [name, @self, head, size]) );
end;


initialization
 ptr_info_func := PtrNoInfo;

 hLuaicp := GetModuleHandle (LUAICP_DLL);
 if hLuaicp = 0 then
    hLuaicp := LoadLibrary ( PChar ( ExePath + LUAICP_DLL ) );
 if hLuaicp > 0 then
    FSExpandPath := GetProcAddress ( hLuaicp, 'FSExpandPath' );

 xr_engine := GetModuleHandle (XR_EXE);
 if xr_engine <> 0 then
  begin
   g_pApplication    := GetProcAddress ( xr_engine, '?pApp@@3PAVCApplication@@A' );
   g_pGamePersistent := GetProcAddress ( xr_engine, '?g_pGamePersistent@@3PAVIGame_Persistent@@A' );
   g_pGameLevel      := GetProcAddress ( xr_engine, '?g_pGameLevel@@3PAVIGame_Level@@A' );
   g_pRender := GetProcAddress ( xr_engine, '?Render@@3PAVIRender_interface@@A' );
   CApplication.LoadTitleInt := GetProcAddress ( xr_engine, '?LoadTitleInt@CApplication@@QAEXPBD@Z' );

   if GetProcAddress ( xr_engine, '?bDebug@@3HA') <> nil then
      g_build_ext := '_debug';

   _RayPick := GetProcAddress ( xr_engine, '?_RayPick@CObjectSpace@@AAEHABU?$_vector3@M@@0MW4rq_target@collide@@AAUrq_result@4@PAVCObject@@@Z' );

   InitImports ('XRCORE', LoadLibrary('xrCore.dll'));
  end;


 InitDMA (TRUE);



 DetectLauncher;

 g_buffers := VirtualAlloc (nil, sizeof (TTempBuffers), MEM_COMMIT, PAGE_EXECUTE_READWRITE);

 class_map := TStrMap.Create();

finalization
 if gamer_profile <> nil then
   begin
    gamer_profile.Close;
    FreeMem (gamer_profile);
   end;

 FreeAndNil (g_offsets.fini);
 FreeAndNil (g_offsets.offset_map);
 VirtualFree (g_buffers, sizeof(TTempBuffers), MEM_RELEASE);
 FreeAndNil (game_log);
 FreeAndNil (class_map);
end.

