unit texcap;

interface
uses Windows, SysUtils, Classes, Misc, LCGlobals, LuaTypes, LuaTools, XrayLua, StrClasses, DateTimeTools, WThreads, Math, IniFiles;


{$I stkdef.inc}
var
         gTexList: TList = nil;
        gTexIndex: TStrMap = nil;
    gTexBlackList: TStrMap = nil; // не рекомендованные к отложенной загрузке
     gTexLoadList: TStrMap = nil; // —писок загружаемых текстур
     gTexMemUsage: Int64 = 0;
    textures_path: String;




function  LuaTexCreate (L: lua_State): Integer; cdecl;
function  LuaTexClean (L: lua_State): Integer; cdecl;
function  LuaTexFile (L: lua_State): Integer; cdecl;
function  LuaTexFind (L: lua_State): Integer; cdecl;
function  LuaTexLoad (L: lua_State): Integer; cdecl;

procedure CalcTexMemUsage;

procedure InitModule ( fini: TIniFile );
procedure CleanupModule (L: lua_State);


procedure LoadTexture (t: Pointer);
procedure UnloadTexture (t: Pointer);

implementation
uses XrayImports;

type
    TAsyncTexLoader = class (TWorkerThread)

    protected

     sum_time: Double;

     function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;

    end;


    TFarJump = packed record
     c25FF: WORD;
     pDest: Pointer;
     procedure   Init (pt: Pointer); overload;
     procedure   Init (va: NativeUInt); overload;
    end;


    TDDSurface = packed record
      wrap0: array [0..31] of BYTE;
      widht: Integer;             // 36
     height: Integer;             // 40
      wrap1: array [0..99] of BYTE;
       size: Integer;
    end;

    PDDSurface = ^TDDSurface;

    TTexture = packed record
      vftable: Pointer; //  0  4
     refCount: Integer; //  4  8
        wrap0: DWORD;   //  8  12
      srcFile: pstr_value; // 12 16
        wrap1: array [0..11] of BYTE;
     old_surf: Pointer;
      function surface: PDDSurface; //
    end;

    PTexture = ^TTexture;


    TCppMethod = procedure; stdcall; // (thiscall)
    TTextureMethod = function (pThis: PTexture): Integer; stdcall;
    PTextureMethod = ^TTextureMethod;

var
       _TextureLoadCB: PTextureMethod;
     _TextureUnloadCB: PTextureMethod;
    DeferredUploadRet: DWORD = 0;
 CRender_texture_load: function ( tex_name: PAnsiChar; pSize: PInteger ): Pointer; stdcall = nil;

             CTexture: record
                           LoadNext: Pointer;
                         UnloadNext: Pointer;
                               Load: TCppMethod;
                           LoadImpl: TCppMethod;
                            Preload: TCppMethod;
                           PostLoad: TCppMethod;
                            SetName: TCppMethod; // (name: PAnsiChar)
                        surface_set: procedure (p: Pointer); stdcall;
                             Unload: TCppMethod;
                           pSurface: TCppMethod;
                       end;

     CResourceManager: record
                           pResManager: PPointer;
                           get_manager: function: Pointer; stdcall;
                          get_textures: function(pm: Pointer): Pointer; stdcall;
                        _CreatePS_Next: Pointer;    // wayback to function
                        _CreateTexture: TCppMethod;  // function ( tex_name: PAnsiChar ): PTexture
                             _CreatePS: TCppMethod;
                        DeferredUpload: TCppMethod;

                       end;



     txload_esp_dec: DWORD = $314;
        main_thread: DWORD = 0;
        mt_tex_load: Boolean = TRUE;
        unload_orig: DWORD = 0;
        ignore_load: DWORD = 0;
          crps_orig: DWORD = 0; // _createPS
         caller_esp: DWORD = 0;

         pf_texLoad: Pointer;
         pf_texUnld: Pointer;
        pf_createPS: Pointer;

          prv_tlist: Pointer = nil;
           prefetch: TStrMap = nil;
            prefcnt: Integer = 0;
           sum_time: Double;
          gFileList: TFileList = nil;
            pRender: PCppObject;
             hEvent: THANDLE = 0; // дл€ согласовани€ с движком загрузки текстур
          hEventExt: THandle = 0; // дл€ согласовани€ с движком загрузки текстур
          rest_load: Integer = 0;


// memory info
              _free: SIZE_T;
              _resv: SIZE_T;
              _comt: SIZE_T;



procedure CleanupModule (L: lua_State);
var
   sn: String;
    i: Integer;
    n: Integer;
begin
 FreeAndNil (gFileList);
 sum_time := 0;
 if Assigned(prefetch) then
  begin
   prefetch.Clear;
   prefetch.LoadFromFile ( ExePath + 'tex_prefetch.lst' );
  end;

 n := -2;

 if Assigned (gTexLoadList) then
  try

   sn := 'Textures load dump ';

   lua_getglobal (L, 'level');

   case lua_type (L, -1) of
    LUA_TTABLE, LUA_TLIGHTUSERDATA:
     begin
      lua_getfield (L, -1, 'name');

      ODS ('[~T]. #DBG: level.name type = ' + lua_typeof(L, -1) );

      if lua_type(L, -1) = LUA_TFUNCTION then
        begin
         lua_pcall (L, 0, 1, 0);
         sn := sn + ' for level "' + LuaStrArg (L, -1) + '":'
        end;

      lua_pop (L, 1);
     end;
    else
     begin
      ODS ('[~T].~C0C #WARN:~C07 _G.level type = ' + lua_typeof(L, -1) );
      lua_pop (L, 1);
     end;
   end; // case



   gTexLoadList.Lock('dump-clear');
   gTexLoadList.Insert (0, sn);
   gTexLoadList.AppendToFile ( gLogPath + '\tex_load.log' );

   sn := '';

   n := 0;
   for i := gTexLoadList.Count - 1 downto 0 do
     begin
      n := i;
      gTexLoadList.Delete (i);
     end;

   gTexLoadList.Unlock;

   Assert ( n <= 0, 'loop not complete');
   gTexIndex.Clear;

  except
   on E: EAccessViolation do
     begin
      PrintError( 'AV catched in texcap.CleanupModule, last_n = ' + IntToStr(n) + ': ' + E.Message );
      gTexLoadList := nil;
     end;
  end;

end;


function gResManager: Pointer; {$IFOPT D-} inline;{$ENDIF}
var
   hLib: HMODULE;
begin
 result := nil;

 if not Assigned (CResourceManager.get_manager) then
   begin
    hLib := GetModuleHandle('xrGame.dll');
    CResourceManager.get_manager  := GetProcAddress (hLib, '?get_resource_manager@@YAPAVCResourceManager@@XZ' );
    CResourceManager.get_textures := GetProcAddress (hLib, '?get_textures_map@@YAPAXPAVCResourceManager@@@Z');
   end;
 if Assigned (CResourceManager.get_manager) then
    result := CResourceManager.get_manager();
 if CResourceManager.pResManager <> nil then
    result := CResourceManager.pResManager^;

end;

function SmallTexList ( const path: String ): TList;
begin
 result := TList  (gTexIndex.FindObject(path, TRUE, TList));
end;

function Traverse_textures (node, head: PTreeMapNode): Boolean;
var
   path: String;
begin
 result := FALSE;

 if (node = head) or ( DWORD(node) < $1000 ) or IsBadReadPtr(node, 22) or node.is_nil then exit;
 // reqursive
 with node^ do
  begin
   Traverse_textures (left, head);
   if key <> 0 then // LPCSTR
     begin
      gTexList.Add(obj);
      path := String ( PAnsiChar(key) );
      path := ExtractFilePath(path);
      if path <> '' then
         SmallTexList (path).Add(obj);
     end;

   Traverse_textures (right, head);
  end;
  result := TRUE;
end;

procedure UpdateTexList;
var
   tex_map: PXrMap;
         n: Integer;
begin
 {$IFDEF NEWEST_BUILD}
 if (gResManager <> nil) and Assigned(CResourceManager.get_textures) then
   begin
    tex_map := CResourceManager.get_textures (gResManager);
    if (tex_map =  nil) or (tex_map.head = nil)
    {$IFOPT D-}
       or (tex_map.size = gTexList.Count) // TODO: check optimization effect!
    {$ENDIF}  then exit;
    gTexList.Clear;
    for n := 0 to gTexIndex.Count - 1  do
        TList (gTexIndex.Objects[n]).Clear;
    Traverse_textures (tex_map.head.parent, tex_map.head);
   end;

 {$ELSE}


 {$ENDIF}
end;

procedure FindRender;
var
   vmt: PPointerArray;

begin
 if xr_engine = 0 then exit;

 {
.text:004A6DA9                 mov     ecx, Render
.text:004A6DAF                 mov     edx, [ecx]      ; render vftable
.text:004A6DB1                 lea     esi, [esp+324h+var_310]
.text:004A6DB5                 push    esi
.text:004A6DB6                 push    eax
.text:004A6DB7                 mov     eax, [edx+1Ch]  ; texture load method, typicaly CRender::texture_load
.

 }

 if pRender = nil then
    pRender := g_pRender^;                                                       // mov ecx, [Render]

 if pRender <> nil then
   begin
    vmt := pRender.vftable;                                                           // mov edx, [ecx]

    if vmt[7] = nil then exit;

    CRender_texture_load := vmt [7];
    ODS (  CFormat( '[~T]. #DBG: pRender = $%p, CRender::texture_load = $%p ', '~C07', [pRender, vmt[7]] ) );
   end;

end;




function IsDDSTextureSize ( sample: String ): Int64;

var

    fl: TFileList;
     p: String;
     s: String;

   n: Integer;

begin

 result := 0;
 if gFileList = nil then
    gFileList := TFileList.Create;

 fl := gFileList;

 p := AddSlash ( textures_path ) + sample;
 p := ExtractFilePath ( p ) + '*.*';
 if fl.LastPath <> p then
    fl.FindFiles ( p );


 sample := ExtractFileName (sample); // name without extension

 for n := 0 to fl.Count - 1 do
   begin
    s := fl [n];
    if Length (s) <> Length (sample) + 4 then continue;
    if Pos (sample, s) <> 1 then continue;



    s := ExtractFileExt ( s );
    s := LowerCase (s);

    // найдена плоха€ альтернатива (!)
    if ( s = '.avi' ) or ( s = '.seq' ) or ( s = '.ogm' ) then
      begin
       result := 0;
       exit;
      end;

    if s = '.dds' then
       result := fl.Items [n].FileSize;
    // if fs > min_size * 5 then   ODS( CFormat( '[~T]. #DBG: File %-100s size = %8d ', '~C07', [tfile, fs] ) );
   end;


end;


{
 CTexture::Load

 .text:004A68C0                 sub     esp, 314h
 .text:004A68C6                 push    ebx
 .text:004A68C7                 push    ebp
 .text:004A68C8                 push    esi

}

function ThisCall (this: Pointer; const proc: TCppMethod): Pointer; stdcall; overload;
asm
 push ecx
 mov  ecx, this
 call dword ptr [proc]
 pop  ecx
end;

function ThisCall (this: Pointer; const proc: TCppMethod; arg: Pointer ): Pointer; stdcall; overload;
asm
 push ecx
 mov  ecx, this
 mov  eax, arg
 push eax
 call dword ptr [proc]
 pop  ecx
end;


function TexCreate(const tname: String): PTexture;
var
   pTexName: PAnsiChar;
begin
 pTexName := PAnsiChar ( AnsiString(tname) );
 result := nil;
 if gResManager <> nil then
    result :=  ThisCall (gResManager, CResourceManager._CreateTexture, pTexName);

 if result <> nil then
    Inc (result.refCount);
end;


function  TexFileRef (t: PTexture): pstr_value; inline;
var
   pp: PPointer;

begin
 pp := RelativePtr (t, 12);
 result := pp^;
end;

function  TexSurface (t: PTexture): PDDSurface; inline;
var
   pp: PPointer;
begin
 pp := RelativePtr (t, 28);
 result := pp^;
end;


function TexMemUsage (s: PDDSurface): Integer; inline;
var
   pv: PInteger;
begin
 result := 0;
 if s = nil then exit;
 pv := RelativePtr (s, 140);
 result := pv^;
end;

function TexDim (s: PDDSurface): PPoint; inline;
begin
 result := RelativePtr (s, 32);
end;


function  TexFile (t: PTexture): String;
var
   ps: pstr_value;
   sa: AnsiString;
begin
 ps := TexFileRef (t);
 try
   if ( ps <> nil ) and not IsBadReadPtr(ps, sizeof(TTexture)) and ( ps.dwLength > 0 ) then
         begin
          SetLength ( sa, ps.dwLength );
          // Move ( ps.sd[0], sa[1], ps.strl );
          StrLCopy ( PAnsiChar(sa), @ps.value, ps.dwLength );
          result := AnsiTrim2W (sa);
         end
    else
          result := '?';
 except
   on E: Exception do
      OnExceptLog ('TexFile', E, TRUE);
 end;

end;


procedure DirectLoadTexture (t: PTexture; ldr: TAsyncTexLoader );
var
   tfile: String;
   tname: PAnsiChar;
      dt: TDateTime;
      tt: Double;
       v: Integer;

begin
 dt := PreciseTime ();
 tfile := TexFile (t);


 {$IFDEF NEWEST_BUILD}
 LoadTexture (t);
 {$ELSE}
 tname := PAnsiChar ( AnsiString (tfile) );
 try
   asm
    push eax
    push ecx
    lea  eax, v
    push eax
    mov  eax, tname
    push eax
    call CRender_texture_load
    push eax
    mov  ecx, t
    call CTexture.surface_set
    pop  ecx
    pop  eax
   end;
 except
  on E: Exception do
     PrintError('Sorry, but multithread texture load caused exception ' + E.Message);
 end;
 ThisCall ( t, CTexture.PostLoad );
{$ENDIF}

 dt := ( PreciseTime - dt ) / DT_ONE_MSEC;

 sum_time := sum_time + dt;

 if ldr <> nil then
   begin
    ldr.sum_time := ldr.sum_time + dt;
    tt := ldr.sum_time;
   end
 else
   tt := 0;


 if ( t.surface <> nil ) then
   begin
    Inc (t.refCount);
    InterlockedAdd ( Integer(_free), -t.surface.size);
    if dt > 80 then
       ODS( CFormat( '[~T/~B]. #PREFETCH: CTexture::PostLoad  %-30s size = %2.3f MiB ref_count = %5d, load_time = %5.1f ms (%s), total_time = %5.1f ms, thread_time = %5.1f ms ',
                   '~C07', [tfile, ( t.surface.size / 1048576 ), t.refCount, dt, IfV(dt < 100, 'light', 'heavy'), sum_time, tt] ) );
   end
 else
   ODS('[~T].~C0C #WARN: ~C07 DirectLoadTexture has no effect for ' + tfile );

end;


var
        tex_reg: Integer = 0;
        loaders: array [0..7] of TAsyncTexLoader;
       n_loader: Integer = 0;
      last_load: TDateTime = 0;
   previous_tex: Pointer;

function GetAsyncTexLoader: TAsyncTexLoader;
begin
 if loaders [n_loader] = nil then
   begin
    loaders [n_loader] := TAsyncTexLoader.Create (FALSE, 'AsyncTexLoader#' + IntToStr(n_loader));
    loaders [n_loader].WaitStart;
   end;
 result := loaders [n_loader];

 n_loader := ( n_loader + 1) and High (loaders);
end;

procedure AsyncLoadTexture ( t: PTexture );
var
   ldr: TAsyncTexLoader;
begin
 if t.surface <> nil then exit;
 ThisCall ( t, CTexture.Preload);
 // ODS('[~T/~B]. #PREFETCH: CRender::texture_load  ' + tfile);
 ldr := GetAsyncTexLoader;
 InterlockedIncrement(rest_load);
 ldr.AddRequest ('TEXTURE_LOAD', Pointer(t));
end;



function TexRegisterImpl (t: PTexture): Integer;
var
   cstack: String;
    tfile: String;
    stack: PDWORD;
     elps: Double;
      evt: THandle;
      tid: DWORD;
       dt: TDateTime;
       tt: PTexture;
       wt: TWorkerThread;
        s: String;
        n: Integer;

begin
  //

  result := 0;
  tfile := '';
  if gTexList = nil then exit;


  dt := PreciseTime;

  try
   stack := Pointer (caller_esp);
   s := '$' + IntToHex (stack^, 4);

{$IFNDEF NEWEST_BUILD}
   if log_verbose > 4 then
    begin
     s := ptr_info_func( Ptr(stack^) );
     ODS ( CFormat( '[~T]. #DBG: CTexture = $%p, caller  ESP = $%.8X, EIP = %s, tex_reg = %d', '~C07', [t, caller_esp,  s, tex_reg] ) );
    end;
{$ENDIF}

   if ( t = previous_tex ) and ( t.refCount > 0 ) and ( t.surface <> nil ) then
     begin
      result := 1;
      if Pos('$user$', TexFile (t)) = 0 then
         wprintf ('[~T].~C0C #WARN:~C07 Trying (re)load previous texture %-30s, refCount = %3d, surface = $%p ',
               [ TexFile (t), t.refCount, t.surface ] );

      if t.refCount < 3 then
         Inc (t.refCount)
      else
         result := 0;

      previous_tex := nil;
      exit;
     end;   previous_tex := t;

   tfile := TexFile (t);


   if last_load = 0 then last_load := dt;
   last_load := dt;

   if gTexList.Count = 0 then
      begin
       FindRender;
       // r_text_load := GetProcAddr ( 'xrRender_R2.dll', '' );
       // ReadLn;
      end;

   // Assert ( caller_esp <> $0018FD0C, ' caller_esp catched ');
   if gTexList.IndexOf (t) >= 0 then
     begin
      if (t.surface = nil) and (log_verbose > 7) then
          wprintf('[~T].~C0C #WARN:~C07 texture %30s registered, but not loaded previously. Forcing load... ', [TexFile(t)]);

      if t.surface <> nil then
         result := 1;
      exit;
     end;


   gTexList.Add (t);
   if gTexList.List <> prv_tlist then
      begin
       prv_tlist := gTexList.List;
       ODS('[~T]. #DBG: gTexList.List relocated to ' + FormatPtr(prv_tlist));
      end;
   result := 0;

   if ( t.refCount < 2 ) then
        if ( Pos('crosshair', tfile) + Pos( 'map\', tfile ) + Pos('amk_ui_base_textur', tfile)  > 0 ) then Inc ( t.refCount );

   tid := GetCurrentThreadID;

   n := prefetch.IndexOf (tfile);
   if ( n >= 0 ) then
     begin
      prefetch.Objects [n] := Pointer(t);
      if tid = main_thread then
         Inc (prefcnt);
     end;

   //

   if stack^ = DeferredUploadRet then
      begin
       // Inc (prefcnt);
       s := 'CODE:.CResourceManager::DeferredUpload(void)';

       if ( gTexList.Count >= 1500 ) and ( gTexList.Count mod 100 = 0 ) then
            SetBootMsg ( Format('«агружено %d текстур ', [gTexList.Count] ) );

       if Pos ('map\', tfile) > 0 then
         asm
          nop
         end;


       if ( Pos('ui_ls', tfile) = 0 ) and ( Pos('intro', tfile) = 0 ) then
          // allowed load
       else
           begin
            ODS('[~T].~C9F #DBG: prevented load texture ~C9E' + tfile + '~C07');
            result := 1;
           end;
      end
    else
      begin
       {
       // TODO: сложности с планированием загрузки текстур: движок может затребовать surface по возврату
       tsize := IsDDSTextureSize (tfile);

       if Assigned ( CRender_texture_load ) and ( Pos('sun', tfile) = 0 ) and ( tsize > 1024 * 1024 ) then
            begin
             if ( t.surface = nil ) and mt_tex_load then
               begin
                AsyncLoadTexture ( t );
                result := 1;  // this texture planned for load
                exit;
               end;
            end; // }

      end;

   if Assigned (gTexLoadList) then
     begin
      elps := ( PreciseTime - last_load ) / DT_ONE_MSEC;
      gTexLoadList.Add ( InfoFmt('~U') + Format ( '; %-30s;%.3f;', [ tfile, elps ] ) + s );
     end;


    if (prefcnt = 0) and (hEventExt = 0) then
      begin
       hEventExt := OpenEvent (EVENT_ALL_ACCESS, FALSE, 'MT_TEXURES_PREFETCH_0');
       if (hEventExt <> 0) and (0 = hEvent) then
          begin
           hEvent := CreateEvent(nil, TRUE, FALSE, 'MT_TEXURES_PREFETCH_1'); // создать заранее, чтобы движку не пропустить мои действи€
           ODS('[~T/~I].~C0F #DBG: catched event MT_TEXURES_PREFETCH_0, so new engine detected ! ~C07');
          end;
      end;


    if (tid = main_thread) and ((prefcnt = 1) or (hEventExt <> 0)) then //
      begin
       if 0 = hEvent then
          hEvent := CreateEvent(nil, TRUE, FALSE, 'MT_TEXURES_PREFETCH_1');

        _free := 1024 * 1024 * 1024;
        if Assigned (vminfo) then
           vminfo(_free, _resv, _comt);


       if (hEventExt <> 0) then
        begin
         WaitForSingleObject(hEventExt, 100);
         CloseHandle (hEventExt);
         hEventExt := 0;  // чтобы не реагировать на него в дальнейшем
        end;



       if _free < 768 * 1024 * 1024 then
          ODS('[~T]. #PERF: prefetch textures breaked due free VM < 768MIB')
       else
       for n := 0 to prefetch.Count - 1 do
        if prefetch.Objects [n] = nil then
           begin
            Inc (prefcnt);
            s := Trim ( prefetch [n] );
            s := UnhideSP (s);
            if s = '' then continue;
            ODS('[~T/~I].~CF0 #PREFETCH: forced textures loading ~CF2' + s + '~C07' );
            tt := TexCreate (s);
            prefetch.Objects [n] := Pointer (tt);

            if tt = nil then
               begin
                PrintError('Texture not created ' + s);
                continue;
               end;

            if tt.surface = nil then
              begin
                if mt_tex_load then
                   AsyncLoadTexture (tt)   // только свежесозданные текстуры можно загружать асинхронно
                else
                   LoadTexture (tt);
              end;
            // Inc (tt.refCount);
           end; // if -> for
      end;

   result := Ifv (t.surface = nil, 0, 1);
   if (prefcnt > 1) and (rest_load = 0) and (hEvent <> 0) then
     begin
      ODS('[~T].~CF0: #PREFETCH: detected no more textures to load, all threads completed!');
      SetEvent(hEvent);
      CloseHandle(hEvent);
      hEvent := 0;
      for n := 0 to High(loaders) do
           try
            wt := loaders[n];
            loaders[n] := nil;
            if not Assigned(wt) then continue;
            wt.StopThread();
            wt.FreeOnTerminate := TRUE;
           except
            on E: Exception do
               OnExceptLog('TexRegisterImpl', E, TRUE);
           end;
     end;



   if ( log_verbose >= 7 ) and ( stack <> nil ) then
     begin
      cstack := IntToHex ( stack^, 0 );

      ODS ( CFormat('[~T/~U/~B]. #DBG: registered texture %p index %7d, caller = $%s, file %24s', '~C07', [t, gTexList.Count - 1, cstack, tfile]) );
     end;
   // if gTexList.Count < 10 then Sleep (50);
  except
   on E: Exception do
      OnExceptLog ('TexRegister ' + FormatPtr(t) + ', tfile = ' + tfile, E);
  end;
end;


function TexRegister (t: PTexture): Integer; stdcall;
begin
 result := 0;
 if tex_reg > 0 then
     exit; // ignore_load = 0

 if caller_esp = 0 then
    asm
      mov caller_esp, ebp
      add caller_esp, 4
    end;


 InterlockedIncrement (tex_reg);
 try
  result := TexRegisterImpl (t);
  ignore_load := result;
 finally
  InterlockedDecrement (tex_reg);
 end;
end;


function TexUnregister (t: PTexture): Integer; stdcall;
var
   d: PPoint;
   i: Integer;
   s: Pointer;
   u: Single;

begin
 result := 0;
 if gTexList = nil then exit;
 i := gTexList.IndexOf (t);
 if i >= 0 then
   begin
    s := TexSurface ( gTexList [i] );
    if ( s <> nil ) and ( log_verbose >= 7 ) then
     begin
      u := TexMemUsage (s) / 1048576;
      d := TexDim (s);
      ODS( CFormat('[~T/~U/~B]. #DBG: unregistered texture $%p index %7d file %24s size %2.1f MiB widht %4d height %4d',
                                        '~C07', [t, i, TexFile(t), u, d.X, d.Y]) );
     end;

    gTexList.Delete (i);
   end
  else
   if log_verbose > 8 then
    ODS( CFormat('[~T/~U/~B].~C0C #WARN~C07: cannot unregister texture $%p file %s ', '~C07', [t, TexFile(t)]) );

end;


procedure LoadTexture (t: Pointer);
begin
 if Assigned (CTexture.LoadImpl) then
    Thiscall (t, CTexture.LoadImpl, Ptr(1));
end;


procedure UnloadTexture (t: Pointer);
begin
 Thiscall (t, CTexture.Unload);
end;


{$IFDEF NEWEST_BUILD}


procedure _ImmTexLoad;
begin
 CTexture.LoadImpl;
end;

{$ELSE}

procedure _ImmTexLoad; stdcall;
asm
 sub    esp, 211h
 nop
 jmp [CTexture.LoadNext]
end;


procedure _TexLoad; stdcall; // stub for texture-load
asm
 mov    caller_esp, esp
 pushad
 push   ecx             // thiscall, ecx = this
 // 3 ofs
 call   TexRegister
 // 8 ofs
 popad
 // 9 ofs
 cmp    ignore_load, 0
 jna    _ImmTexLoad
 ret
end;


procedure _TexUnload; stdcall;
asm
 pushad
 push   ecx
 call   TexUnregister
 popad
 cmp    unload_orig, 83EC8B55h
 jne    @test_2945
 // 2947-3312 restore
 push   ebp
 mov    ebp, esp
 and    esp, 0FFFFFFF8h
 jmp [CTexture.UnloadNext]

@test_2945:
 cmp    unload_orig, 8B565551h
 jne    @fatal_unknown
 // 2945 restore
 push   ecx
 push   ebp
 push   esi
 mov    esi, ecx
 xor    ebp, ebp
 jmp [CTexture.UnloadNext]

@fatal_unknown:
 int    3
 int    3
end;
{$ENDIF}

procedure Pre_CreatePS ( rm: Pointer ); stdcall;
type
  TAnsiStrArray = array [0..31] of PAnsiChar;
  PAnsiStrArray = ^TAnsiStrArray;

var
  ps: Pointer;
   p: PAnsiStrArray;
   s: String;
   n: Integer;
begin
 asm
  mov eax, ebp
  mov ps, eax
  add eax, 24h
  mov p, eax
 end;
 if p = nil then exit;

 s := 'EBP = ' + FormatPtr(ps) + '// ';

 for n := 1 to 1 do
    begin
     if DWORD ( p^[n] ) < $190000 then continue;

     s := s + IntToHex(n, 2) + ':' + String ( AnsiTrim ( p^[n] ) ) + '|';
    end;

 // if IsDebuggerPresent then ODS('[~T]. #_CreatePS ' + s);
end;


procedure _CreatePS; stdcall;
asm
 pushad
 push   ecx      // ecx = CResourceManager
 call   Pre_CreatePS
 popad
 push   ebp
 mov    ebp, esp
 and    esp, 0FFFFFFF8h
 jmp    [CResourceManager._CreatePS_Next];
 int    3
end;

procedure CalcTexMemUsage;
var
   n: Integer;
begin
 gTexMemUsage := 0;
 if Assigned (gTexList) then
    for n := 0 to gTexList.Count - 1 do
        gTexMemUsage := gTexMemUsage + TexMemUsage ( TexSurface ( gTexList [n] ) ) or $FF + 1;
end;



procedure InitModule;
var
   hm: HMODULE;
   fn: String;
   hp: THandle;
   pa: PPointer;
   pp: PByteArray;
   pv: PDWORD;
   jc: BYTE;
begin
 {$IFNDEF SOC}
 exit;
 {$ENDIF}

 if ( xr_engine = 0 ) then
    begin
     // PrintError('texcap.InitModule failed. xr_engine = 0');
     exit;
    end;



 mt_tex_load := fini.ReadBool ('opts', 'mt_tex_load', TRUE) and ( xr_engine > 0 );

 if mt_tex_load then
    for jc := 0 to High(loaders) do
        GetAsyncTexLoader();


 if fini.ReadBool ('debug', 'LogTexturesLoad', FALSE) then
    gTexLoadList := TStrMap.Create;


 textures_path := FmtStalkerPath ( '$game_textures$' );

 gTexList := TList.Create;
 gTexList.Capacity := 8192;
 gTexIndex := TStrMap.Create;
 gTexIndex.OwnsObjects := TRUE;
 gTexIndex.Sorted      := TRUE;

 gTexBlackList := TStrMap.Create;

 prefetch := TStrMap.Create;

 fn := ExePath + 'tex_prefetch.lst';
 if FileExists(fn) then
    prefetch.LoadFromFile ( fn );


 CResourceManager._CreateTexture := GetProcAddress (xr_engine, '?_CreateTexture@CResourceManager@@QAEPAVCTexture@@PBD@Z');
 CResourceManager._CreatePS      := GetProcAddress (xr_engine, '?_CreatePS@CResourceManager@@QAEPAUSPS@@PBD@Z');
 CResourceManager.DeferredUpload := GetProcAddress (xr_engine, '?DeferredUpload@CResourceManager@@QAEXXZ');

 pp := @CResourceManager.DeferredUpload;

 DeferredUploadRet := DWORD (pp) + $1E;
 ODS('[~T]. #DBG: CResourceManager::DeferredUpload return from CTexture::Load = $' + IntToHex (DeferredUploadRet, 5));
 hm := GetModuleHandle ('xrTextures.dll');
 if hm = 0 then
    hm := xr_engine;

 CTexture.Load        := GetProcAddress (hm, '?Load@CTexture@@QAEXXZ');
 CTexture.LoadImpl    := GetProcAddress (hm, '?LoadImpl@CTexture@@QAEXXZ');
 CTexture.Preload     := GetProcAddress (hm, '?Preload@CTexture@@QAEXXZ');
 CTexture.PostLoad    := GetProcAddress (hm, '?PostLoad@CTexture@@QAEXXZ');
 CTexture.SetName     := GetProcAddress (hm, '?SetName@CTexture@@QAEXPBD@Z');
 CTexture.surface_set := GetProcAddress (hm, '?surface_set@CTexture@@QAEXPAUIDirect3DBaseTexture9@@@Z');
 CTexture.Unload      := GetProcAddress (hm, '?Unload@CTexture@@QAEXXZ');
 CTexture.pSurface    := GetProcAddress (hm, '?pSurface@CTexture@@QBEPAUIDirect3DBaseTexture9@@XZ');

 _TextureLoadCB       := GetProcAddress (hm, '?TextureLoadCapture@@3P6GHPAX@ZA');
 _TextureUnloadCB     := GetProcAddress (hm, '?TextureUnloadCapture@@3P6GHPAX@ZA');
 if Assigned(_TextureLoadCB) then
    _TextureLoadCB^ := TexRegister;

 if Assigned(_TextureUnloadCB) then
    _TextureUnloadCB^ := TexUnregister;


 {$IFDEF NEWEST_BUILD OR CSKY OR SCoP}
 // nothing to patch
 exit;
 {$ELSE}
 pp := @CTexture.Preload;
 if pp [5] = $8B then
   begin
    Move ( pp [7], pa, sizeof(Pointer) );
    if not InRange ( NativeUInt(pa), $4F0000, $5F0000 ) then
      begin
       ODS('[~T].~C0C #WARN:~C07 By smart detection gResourceManager at ' + FormatPtr(pa) );
       pa := nil;
      end;
   end;

  if pa = nil then
     pa := Ptr ( xr_engine + $10BE5C );

  CResourceManager.pResManager := pa;
  ODS( CFormat('[~T]. #DBG: gResourceManager at [$%p] = $%p ', '~C07', [pa, gResManager] ) );



 pp := @CTexture.Load;
 if pp = nil then
   begin
    ODS ('[~T]. #DBG: Cannot locate method CTexture::Load');
    exit;
   end
 else
   begin
    CTexture.LoadNext := @pp [6];
    ODS ('[~T]. #DBG: method CTexture::Load at ' + FormatPtr(pp) + ' moved to ' + FormatPtr(CTexture.LoadNext) );
   end;


 ReadProcessMemory ( hp, pp, @fj, 6, wb );
 if wb < 6 then exit;
 if fj.c25FF <> $EC81 then exit; // sub esp, const32
 txload_esp_dec := DWORD (fj.pDest);  // value
 pf_texLoad := @_TexLoad;

 WriteProcessMemory ( hp, @_ImmTexLoad, @fj, 6, wb );

 fj.Init ( @pf_texLoad );  // jump far [addr]
 WriteProcessMemory ( hp, pp, @fj, 6, wb ); // патч перехода

 if wb < 6 then exit;
 ODS ('[~T]. #DBG: CTexture::Load was patched successfully! Detected sub esp, ' + IntToHex (txload_esp_dec, 3) + 'h' );
 // while not IsKeyPressed(VK_RETURN) do Sleep (100);

 // ThisCall (nil, _TexLoad); // test

 // =================================================================== CTexture::Unload patching =============

 pp := @CTexture.Unload;
 if pp = nil then
   begin
    ODS (#9#9' #DBG: Cannot locate method CTexture::Unload');
    exit;
   end
 else
   begin
    CTexture.UnloadNext := RelativePtr (pp, 6);
    ODS (#9#9' #DBG: method CTexture::Unload at ' + FormatPtr(pp) + ' moved to ' + FormatPtr(CTexture.UnloadNext) );
   end;

 ReadProcessMemory ( hp, pp, @unload_orig, 4, wb );
 if wb < 4 then exit;
 if unload_orig = $8B565551 then
   begin
    ODS(#9#9' #DBG: release 2945 detected, incremented jump destinantion');
    Inc ( NativeUInt (CTexture.UnloadNext) );
   end;

 pf_texUnld := @_TexUnload;
 fj.Init ( @pf_texUnld );

 WriteProcessMemory ( hp, pp, @fj, 6, wb );
 if wb < 6 then exit;
 ODS (#9#9' #DBG: CTexture::Unload was patched successfully! ');

 pf_createPS := @_CreatePS;
 pp := Addr ( CResourceManager._CreatePS );
 if pp <> nil then
  begin
   CResourceManager._CreatePS_Next := RelativePtr (pp, 6);
   ODS (#9#9' #DBG: method CResourceManager::_CreatePS at ' + FormatPtr(pp) + ' moved to ' + FormatPtr(CResourceManager._CreatePS_Next) );
   ReadProcessMemory ( hp, pp, @crps_orig, 4, wb );
   if wb < 4 then exit;
   if crps_orig <> $83EC8B55 then exit; // 2947-3312 std
   wb := 0;
   fj.Init ( @pf_createPS );
   WriteProcessMemory ( hp, pp, @fj, 6, wb );
   if wb = 6 then
      ODS (#9#9' #DBG: CResourceManager::_CreatePS was patched successfully! ');

   // ReadLn;
  end;


 {$ENDIF}

 {
 pv := RelativePtr ( Ptr(hm), $10BD98 );
 if pv^ <> 0 then
   begin
    ODS( '[~T]. #DBG: Allow_DefferedUpload located at ' + FormatPtr (pv) + ' = $' + IntToHex(pv^, 4) );
    pv^ := 0;
   end; // }

end;

function  LuaTexClean(L: lua_State): Integer; cdecl;
var
  ms: Integer;
   i: Integer;
   s: Pointer;
   t: Pointer;
begin
 result := 0;
 ms := 0;

 if lua_gettop (L) > 0 then
    ms := lua_tointeger (L, 1);

 try
  for i := gTexList.Count - 1 downto 0 do
    begin
     t := gTexList [i];
     s := TexSurface (t);

     if ( ms > 0 ) and ( s <> nil ) and ( TexMemUsage (s) < ms ) then continue;

     {$IFDEF NEWEST_BUILD}
     UnloadTexture (t);
     {$ELSE}
     ThisCall (t, _TexUnload);
     {$ENDIF}
    end;

 except
  on  E: Exception do
    OnExceptLog ('LuaTexClean', E);
 end;

end;

function  LuaTexCreate (L: lua_State): Integer; cdecl;

var
   t: PTexture;
begin
 result := 1;
 t := TexCreate ( LuaStrArg(L) );
 if t <> nil then
    lua_pushptr (L, t)
 else
    lua_pushnil (L);
end;

function  LuaTexLoad(L: lua_State): Integer; cdecl;
var
   load: Boolean;
    tex: Pointer;
begin
 result := 0;

 tex := lua_topointer (L, 1);

 if DWORD(tex) < $10000 then exit;

 load := TRUE;

 if lua_gettop(L) > 1 then
    load := lua_toboolean (L, 2);


 {$IFDEF NEWEST_BUILD}
 if load then
   begin
    LoadTexture (tex);
    wprintf(' after load texture name = %s ', [TexFile(tex)]);
   end
 else
    UnloadTexture (tex);

 {$ELSE}
 if load then
    ThisCall (tex, _TexLoad)
 else
    ThisCall (tex, _TexUnload);
 {$ENDIF}

end;


function  LuaTexFile(L: lua_State): Integer; cdecl;
var
   ps: pstr_value;
   sa: AnsiString;
   ue: Boolean;
    t: Pointer;
    s: String;
    o: String;

begin
 result := 1;
 t := Ptr ( LuaDWORD (L, 1) );
 try

   if ( t = nil ) or not GoodPtr(t, 16) then
    begin
     lua_pushwstr(L, 'invalid texture pointer ' + FormatPtr(t));
     exit;
    end;

   ps := TexFileRef (t);
   if not GoodPtr (ps, 16) then
     begin
      lua_pushwstr(L, 'Bad ptr!');
      exit;
     end;

 except
  on E: Exception do
    begin
     lua_pushwstr (L, 'bad texture pointer!');
     PrintError('Exception catched in LuaTexFile ' + E.Message);
     exit;
    end;
 end;

 ue := TRUE;
 // установка или получение файла по указателю текстуры
 if lua_gettop(L) > 1 then
   begin
    if lua_gettop(L) > 2 then ue := lua_toboolean (L, 3);

    s := LuaStrArg (L, 2);
    s := UnhideSP (s);
    // TexRegisterImpl (t);
    o := TexFile (t);

    if Assigned (CTexture.SetName) and ue then
      begin
       sa := AnsiString (s);
       ThisCall (t, CTexture.SetName, Pointer(PAnsiChar(sa)));
       wprintf(' CTexture.SetName(%s), old = %s ', [s, o]);
       exit;
      end;


    ps.dwLength := Length (s);
    SetStrZ ( @ps.value[0], s, ps.dwLength + 1 );
   end;

 lua_pushwstr (L, TexFile(t));
end;

var
   tex_find_calls: DWORD = 0;

function  LuaTexFind(L: lua_State): Integer; cdecl;
var
  tl: TList;
   c: Integer;
   i: Integer;
   t: Integer;
   e: String;
   s: String;
   p: String;


begin
 result := 1;
 lua_createtable (L, 0, 0); // нова€ таблица в стек
 t := lua_gettop (L);
 e := LuaStrArg (L);
 p := LuaStrArg (L, 2);
 if gTexList = nil then exit;
 if p <> '' then
    p := AddSlash(p); // all patches ends \

 try
    if log_verbose > 5 then
       ODS('[~T/~B]. #PERF: updating textures list');
   UpdateTexList;
   if log_verbose > 5 then
      ODS('[~T/~B]. #PERF: scaning textures list');

   c := 0;
   if e = '' then
   for i := 0 to gTexList.Count - 1 do
    begin
     Inc (c);
     lua_pushptr( L, gTexList [i] );
     lua_setfield(L, t, PAnsiChar(AnsiString(s)) );
    end
   else
    begin
     if ( Pos('\', e) > 0 ) and ( p = '' ) then
          p := ExtractFilePath (e);

     tl := TList ( gTexIndex.FindObject(p) );
     if tl = nil then
        tl := gTexList;

     for i := 0 to tl.Count - 1 do
        begin
         Inc (c);
         s := TexFile ( tl[i] );
         if ( Pos(e, s) = 0 ) then continue;
         lua_pushptr( L, tl [i] );
         lua_setfield(L, t, PAnsiChar(AnsiString(s)) );
        end

    end;

   if log_verbose > 5 then
      wprintf ('[~T/~B]. #PERF: complete scan, results = %d', [c]);

   Inc (tex_find_calls);
   if (tex_find_calls mod 100 = 0) then
       wprintf('[~T/~B]. #PERF: tex_find_calls = %d, current from: %s', [tex_find_calls, LuaTraceback(L, ' ')]);
 except
  on E: Exception do
     OnExceptLog('LuaTexFind', E);

 end;

end;


{ TFarJump }

procedure TFarJump.Init(pt: Pointer);
begin
 self.c25FF := $25FF;
 self.pDest := pt;
end;

procedure TFarJump.Init(va: NativeUInt);
begin
 Init ( Ptr(va) );
end;

{ TAsyncTexLoader }

function TAsyncTexLoader.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   pt: PTexture;

begin
 result := Inherited ProcessRequest (rqs, rqobj);
 if rqs = 'TEXTURE_LOAD' then
  begin
   pt := Pointer (rqobj);
   ResetEvent (hEvent);
   DirectLoadTexture (pt, self);
   if InterlockedDecrement (rest_load) <= 0 then
      SetEvent (hEvent);
  end;

end;

{ TTexture }

function TTexture.surface;
begin
 if Assigned (CTexture.pSurface) then
    result := ThisCall (@self, CTexture.pSurface)
 else
    result := old_surf;
end;


initialization

 // ThisCall (nil, _TexLoad);

 OutputDebugString('texcap module initialized');
 main_thread := GetCurrentThreadID;

finalization
 gTexList.Free;
 gTexBlackList.Free;
 gTexLoadList.Free;
 FreeAndNil (gTexIndex);
 prefetch.Free;
end.
