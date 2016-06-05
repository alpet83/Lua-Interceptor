unit Singularity;

interface

uses
  Windows, SysUtils, StrUtils, Classes, Math, ContNrs, IniFiles, StrClasses, misc, LuaTypes, XrayLua, LuaHelp, LCGlobals, LuaTools,
  UniArray, DateTimeTools, WThreads, MD5, IPCUtils;

type
   TSadCallback = procedure (p: Pointer);

   TGameAnchor = packed record

   end;

   PGameAnchor = ^TGameAnchor;
const
   OFS_DELAY = 347;
   OFS_ENVID = 399;
   OFS_POWER = 413;
   OFS_DATA  = 504;


{
  Защита мода выделенным ключом.
   1. Ключ формируется в начале новой игры, как продукт преобразования унифицирующих данных (хэш-функция).
   2. Ключ проверяется на соответствие компьютеру при каждой инициализации, и сравнивается с записанным в сохраненку.
   3. Для работы игры на дополнительных локациях, ключу нужна активация - добавление цифровой подписи через сайт.



  Реализация механизмов ключа.
   * Выборка данных унификации компьютера. Реестр + дата папки Windows + вендор процессора.
   * Связывающей сущностью для серии сохраненок, является профиль игрока (структура TGamerProfile).
   * Подпись профиля сохраняется в отдельном файле, присутствие которого в соответствующей папке необходимо для работы
       большинства локаций.

  Подзадачи механизма защиты:
   * Регулярная проверка целостности файлов мода (хэши архивов и отдельных папок).
   * Проверка соответствия текущей сохраненки профилю игрока на скриптовом уровне.
   * Проверка соответствия цифровой подписи, профилю игрока на дополнительных локациях.

  Анти-хакерские решения:
   * Дублирование механизмов с небольшими различиями.
   * Использование связки процессов launcher + xr_3da для распределенных вычислений.



  Реализация примитивной подписи:
     На стороне сервера производится обратимое преобразование хэша статической части профиля игрока. После загрузки уровня производится
   восстановление хэша и его вычитание с рассчетным (в скрипте). Разность сохраняется в распределенной среде.

   профиль игрока  = источник хэша для подписи сервером. Валидация при 100% совпадении.
   профиль системы = источник хэшей для связывания сохраненок с одним компьютером. Валидация при 67% совпадении.
   подпись сервера = преобразование хэша профиля игрока. Валидация при 100% совпадении.


  TODO: Модификация uno в новом движке

}




{$I stkdef.inc}


{$IFDEF SINGULARITY}

{$I signatures.inc}

var
      deploy: TStrMap = nil;
      worker: TWorkerThread = nil;

   toxic_rql: array [0..7] of PWideChar;
   toxic_cbc: array [0..7] of TSadCallback;


procedure AddGlobals(L: lua_State);
function  CheckStackOverflow (L: lua_State): Integer; cdecl;
procedure DoCleanup;

{$ENDIF}

implementation
uses madExcept, XrayRegistry, ArrayTypes, ByteCode, PsUtils;


{$IFDEF RELEASE}
{$D-}
{$ENDIF}
{$IFDEF DEBUG}
{$D+}
{$ENDIF}


const
   IMPL_STAT: String = 'impl_Stat';
   INF_RQSPX: String = 'infl_srq@';
   _Operator: String = 'InterlockedIncrement';

   SINGULARITY_SIZE = 3172;


{$IFDEF SINGULARITY}
const
  bin_sizes: array [0..15] of Integer = ( sizeof (ByteCode.savemgmt), sizeof(ByteCode.hidden), sizeof(ByteCode.profiles), 0,
                                          0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 );

  bin_stuff: array [0..15] of Pointer = ( @ByteCode.savemgmt, @ByteCode.hidden, @ByteCode.profiles, nil,
                                           nil,      nil, nil, nil,
                                           nil,      nil, nil, nil,
                                           nil,      nil, nil, nil );
  bin_names: array [0..15] of String = ( 'savemgmt', 'hidden',   'profiles',  '',
                                         '',         '', '', '',
                                         '',         '', '', '',
                                         '',         '', '', '' );


var
   gSingularity: PByteArray = nil;

procedure SinError(const msg: String); inline;
begin
{$IFOPT D+}
  PrintError(msg);
{$ENDIF}
end;


function Internal_LoadProfile(fn, map_name: String): PFileMapping;
var
   crc: TRandomHash;
   pgp: PGamerProfile;
   wcp: String;
   hdr: array [0..3] of AnsiChar;
    hf: THandle;
    rb: DWORD;
begin
 result := nil;
 try
   fn := FmtStalkerPath  (fn);
   if not FileExists(fn) then
     begin
      PrintError(' not found file ' + fn);
      exit;
     end;

   wcp := AnsiReplaceStr (fn, 'player.dat', 'tmp.player.wcp');

   AllowReadWrite (fn);
   AllowReadWrite (wcp);

   CopyFile ( PChar(fn), PChar('last.' + fn + '.bak'), FALSE );
   CopyFile ( PChar(fn), PChar(wcp), FALSE );

   hf := CreateFile ( PChar(wcp), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_ALWAYS, 0, 0);
   if hf = INVALID_HANDLE_VALUE then
     begin
      PrintError(' cannot open file ' + fn);
      exit;
     end;

   ReadFile(hf, hdr, 4, rb, nil);
   SetFilePointer (hf, 0, nil, FILE_BEGIN);
   if DWORD(hdr) <> PDWORD(PROFILE_TAG)^ then
     begin
      PrintError(' invalid header bytes for ' + fn + ' = $' + IntToHex(DWORD(hdr), 8));
      CloseHandle (hf);
      exit;
     end;


   result := AllocMem ( sizeof(TFileMapping) );
   result.Init (0);
   result.Create (hf, map_name, TRUE);
   SetStrWZ (result.szFile, wcp, MAX_PATH);


   Assert (result.nSize >= sizeof(TGamerProfile), fn + '  format is old/invalid, size = ' + IntToStr(result.nSize));

   if result.hMapping > 0 then
      begin
        result.MapView(0);
        if result.pView <> nil then
          begin
           pgp := result.pView;
           hash_permut := pgp.gpmt;
           pgp.pcrc.CheckPermut;
           crc := pgp.BodyCRC( pgp.pcrc.nonce64 );
           if pgp.pcrc.Compare (crc) then exit;
           SinError(' invalid profile crc ' + fn);
          end
        else
          SinError(' cannot create file view ' + fn);
      end
   else
       SinError(' cannot map file ' + fn);
   result.Close;
   FreeMem (result);
   result := nil;
   if (hf <> INVALID_HANDLE_VALUE) then  CloseHandle (hf);
 except
  on E: Exception do
   begin
{$IFOPT D+}
    OnExceptLog('Internal_LoadProfile', E);
{$ELSE}
    result := nil;
{$ENDIF}
   end;
 end;

end;

function CheckStackOverflow (L: lua_State): Integer; cdecl;
var
   top: Integer;
   map: TStrMap;
begin
 result := 1;
 top := lua_gettop(L);
 if top > 10000 then
    SinError('Lua stack overflow detected, top = ' + IntToStr(top));

 lua_pushnumber (L, top);

 if Assigned(worker) then
  begin
   map := TStrMap.Create;
   map.Text := LuaTraceBack(L);
   worker.AddRequest(IMPL_STAT, map);
  end;

end;


function GetProc(const sLib, sProc: String): Pointer;
var
   hDLL: DWORD;
begin
 result := nil;
 hDLL := GetModuleHandle ( PChar (sLib) );
 if hDLL = 0 then
   begin
    ODS('[~T].~C0C #WARN:~C07 not loaded library ' + sLib);
    exit;
   end;

 result := GetProcAddress ( hDLL, PChar (sProc) );
 if result = nil then
   begin
    // ODS('[~T].~C0C #WARN:~C07 not found procedure ' + sProc + ' in ' + sLib );
    result := Ptr ($22);
    exit;
   end;

end;

function TestHashes(const name: String; const a, b: TRandomHash): Boolean;
{$IFNDEF NLC_GOLD}
var
   n: Integer;
   s: String;
{$ENDIF}
begin
 result := a.Compare(b);
 if result then exit;
 if a.Format <> b.Format then
    SinError ( Format('Bad profile data, hash~C0F %s~C07 mistmatch:~C0A "%s"~C0B <>~C0E "%s".~C07', [name, a.Format, b.Format] ) )
 else
    SinError ('critical comparator error hash:~C07' + a.Format);

 if a.FormatRaw <> b.FormatRaw then
    SinError ( Format(' raw hashes: "%s" <> "%s" ', [a.FormatRaw, b.FormatRaw]))
 else
    SinError ('critical comparator error, raw hash:~C07 ' + a.FormatRaw);
 result := a.Compare(b);
{$IFNDEF NLC_GOLD}
s := '  hash_permut:~C0F ';
for n := 0 to High (hash_permut) do
    s := s + ' ' + IntToHex(hash_permut[n], 2);
ODS('~C0E' + s + '~C07');
{$ENDIF}
end;

function LoadProfileKey (ref_nonce: Int64): TRandomHash;
var
   fs: TFileStream;
   fn: String;
   wc: String;
   gp: TGamerProfile;
   rh: TRandomHash;
begin
 FillChar (result, sizeof(result), 0);
 fn := ExpandPath('$profile_dir$\' + profile_file);

 if Pos ('bad_file_name', fn) > 0 then exit;
 wc := AnsiReplaceStr (fn, 'player.dat', 'tmp.player.wcp');
 if FileExists(wc) then  fn := wc;
 if not FileExists ( fn ) then exit;

 try
  fs := TFileStream.Create ( fn, fmOpenReadWrite or fmShareDenyNone );
 except
  on E: Exception do
     fs := nil;
 end;

 if Assigned (fs) then
   try
    if fs.Read ( gp, sizeof (gp) ) < sizeof (gp) then exit;

    if DWORD (gp.ftag) <> PDWORD(PROFILE_TAG)^ then
      begin
       wprintf ('[~T].~C0C #FATAL~C07: Profile data in %s invalid/damaged = {%s} (%x <> %x). You must restore this file from backup!',
            [fn, DWORD(gp.ftag), PDWORD(PROFILE_TAG)^]);
       MessageBox (0, PChar(
                        Format('Данные профиля в файле %s неверные/повреждены. Восстановите файл из бэкапа или создайте новый профиль', [fn])
                           ), 'Неисправимая проблема!', MB_OK);


       ExitProcess( DWORD(-220));
       exit;
      end;

    hash_permut := gp.gpmt;
    rh := gp.BodyCRC (gp.pcrc.Nonce64);
    if not TestHashes ('pcrc@' + IntToHex(rh.Nonce64, 8), gp.pcrc, rh) then exit;

    if ref_nonce = 0 then
       rh.Nonce64 := gp.puid.Nonce64
    else
       rh.Nonce64 := ref_nonce;
    rh.Calc (@gp.name, sizeof(gp.name) + sizeof(gp.ctdt));
    if not TestHashes ('puid@' + IntToHex(rh.Nonce64, 8), gp.puid, rh) then exit;
    result := rh;
    // retrieving need crc
    gp.fcrc := gp.FixedCRC(gp.fcrc.Nonce64);
    gp.pcrc := gp.BodyCRC (gp.pcrc.Nonce64);
    // update crc
    fs.Position := 0;
    fs.Write ( gp, sizeof (gp) );
   finally
    fs.Free;
   end;
end;

function LuaGetProfileKey ( L: lua_State ): Integer; cdecl;
var
   sin: Pointer;
    sv: String;
    rh: TRandomHash;

begin
 sin := lua_objptr (L, 1);
 Assert (sin <> nil, 'singularity not assigned');
 sv := '';
 try
  sv := LuaStrArg (L, 2);
  if sv = '' then sv := '0';
  rh := LoadProfileKey ( atoi(sv) );
  sv := rh.format();
  if Pos('00000', sv) = 1 then
     sv := '#INVALID_KEY';

 except
  on E: Exception do
     OnExceptLog('LuaGetProfileKey', E);
 end;
 lua_pushwstr ( L, sv );
 sin := lua_newuserdata (L, sizeof(TRandomHash));
 Move (rh, sin^, sizeof(rh));
 SetPointerMT (L, -1);
 Move (sin^, rh, sizeof(rh));
 if sv <> rh.Format then
    PrintError('critical error occured');


 result := 2;
end;

function LuaGetSystemSign ( L: lua_State ): Integer; cdecl;
var
   tab: Pointer;
   tnb: TLargeInteger;
   fba: TLargeInteger;
    rh: TRandomHash;
    th: TRandomHash;
    nv: LARGE_INTEGER;
     s: String;

  procedure  store_hash;
  var
     p: Pointer;
  begin
   p := lua_newuserdata (L, sizeof(TRandomHash));
   Move (rh, p^, sizeof(rh));
   SetPointerMT (L, -1);
   lua_set_field (L, -2, rh.Format);
  end;

begin
 tab := lua_objptr (L, 1);

 if lua_gettop (L) >= 2 then
   begin
    s := LuaStrArg (L, 2);
    if lua_isstring (L, 2) and ( Length(s) > 16 ) then
       rh.Nonce64 := atoi (s)
    else
      begin
       rh.SetNonceLow  ( LuaDWORD (L, 2) );
       rh.SetNonceHigh ( LuaDWORD (L, 3) );
      end;
   end
 else
    rh.RandInit;

 result := 1;
 Assert (tab <> nil, 'singularity not assigned');

 th.nonce64 := -1;
 nv := rh.nonce;
 th.Calc(@nv, sizeof(nv)); // multiply nonce


 lua_createtable (L, 0, 0);
 s := GetEnvironmentVariable('USERPROFILE');
 rh.Calc(s);                             // юзер
 store_hash;
 s := GetCPUName;
 rh.nonce64 := th.md.L1;
 rh.Calc(s);                             // процессор
 store_hash;

 s := GetOSVersionString;
 rh.nonce64 := th.md.L2;
 rh.Calc(s);                             // версия ОС
 store_hash;

 GetDiskFreeSpaceEx ( PChar(ExePath), fba, tnb, nil );
 rh.Nonce64 := th.MD.L1 xor th.MD.L2;
 rh.Calc (@tnb, sizeof(tnb));           // размер диска
 store_hash;
end;


procedure set_mtf (L: lua_State; cf: lua_CFunction; tab: Integer; const key: String );
const
   NI: String = 'newindex';
   CI: String = 'index';

var
   s: String;

begin
 s := '__';
 lua_pushcfunction (L, cf);
 if key = NI then s := s + CI
 else
  if key = CI then s := s + NI;
 lua_setfield (L, tab, PAnsiChar ( AnsiString(s) ) );
end;


function _Index (L: lua_State): Integer; cdecl;
var
   key: String;
   tab: Pointer;
   pop: PWideChar;
    md: TMD5Digest;
    pf: PSingle;
    pv: PDWORD;
    ch: CHAR;
     t: Integer;
begin
 result := 1;
 t := lua_gettop(L);

 tab := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);

 if key = 'alert' then
   begin
    lua_pushcfunction (L, LuaSendAlert);
    exit;
   end else
 if key = 'delay' then
   begin
    pf := RelativePtr (tab, OFS_DELAY);
    lua_pushnumber (L, pf^);
   end else
{$IFDEF NLC_GOLD}
 if key = 'gold' then
    lua_pushboolean (L, TRUE)
 else
{$ENDIF}
 if key = 'power' then
   begin
    pf := RelativePtr (tab, OFS_POWER);
    lua_pushnumber (L, pf^);
   end else
 if key = 'hyperinflation' then
   try
    ODS('[~T]. #DBG: Hyperinflation begins...');
    ch := 'x';
    pop := @_Operator[1];
    pop[12] := ch;

    if Assigned (worker) then
      begin
       worker.AddRequest( INF_RQSPX + IntToHex(HInstance, 4), tab);
       lua_pushnumber (L, 1e100 + worker.RQCount);
      end
    else
       lua_pushnumber (L, 1);

    ch := 'E';
    Move ( ch, pop [11], 2 );
    ODS('[~T]. #DBG: Hyperinflation complete');
   except
    on E: Exception do
      begin
       PrintError('Hyperinflation failed! ' +E.Message);
       {$IFOPT D+}
       OnExceptLog('_index', E);
       {$ENDIF}
      end;
   end;

 pv := RelativePtr (tab, OFS_ENVID);
 if key = 'flesh' then
    lua_pushinteger ( L, pv^ )
 else
 if key = 'data' then
    lua_pushptr ( L, RelativePtr (tab, OFS_DATA ) )
 else
 if key = 'unlock' then
    begin
     pv^ := Random (100);
     lua_pushboolean ( L, TRUE );
    end else
 if key = 'lock'   then
    begin
     pv^ := $FAB0EDA1 xor $DECAB001;
     lua_pushboolean ( L, TRUE );
    end else
 if pv^ xor $FAB0EDA1 = $DECAB001 then
   begin
    if key = 'profile_size' then
      begin
       lua_pushinteger (L, TGamerProfile.BodySize);
      end
    else
    if key = 'get_profile_key' then
       lua_pushcfunction ( L, LuaGetProfileKey );
   end;

 // if key = 'sys_sign' then
 __nop;
 md  := MD5.MD5StringW(key);
 key := MD5.MD5DigestToStr(md);
 if Pos('F5BA9FBC71FB3967A636BC93E01A', key) > 0 then
    lua_pushcfunction ( L, LuaGetSystemSign );

 if GetProc ( 'kernel32.dll', _Operator ) = nil then exit;
 if t = lua_gettop(L) then
    lua_pushnil (L);
end;



function _NewIndex (L: lua_State): Integer; cdecl;
var
   key: String;
   tab: Pointer;
    vp: Double;
    pf: PSingle;


begin
 result := 0;
 tab := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);
 key := LowerCase (key);
 vp := lua_tonumber (L, 3);
 pf := RelativePtr (tab, 329);


 if key = 'delay' then
    pf := RelativePtr (tab, OFS_DELAY) else
 if key = 'power' then else
    pf := RelativePtr (tab, OFS_POWER);
 if ( key = 'uno' ) then
    pf := global_uno;

 if pf <> nil then pf^ := vp;
end;


function NewSingularity (L: lua_State): Integer; cdecl;


var
   pData: PPointer;
   pBase: Pointer;
     cnt: Integer;
       n: Integer;
begin
 result := 1;
 if gSingularity <> nil then
   pBase := gSingularity
 else
   pBase := AllocMem (SINGULARITY_SIZE);

 cnt := SINGULARITY_SIZE div 4;

 pData := pBase;

 for n := 0 to cnt - 1 do
  begin
   pData^ := RelativePtr ( pBase, Random(cnt) * 4 ); // labirinth
   Inc (pData);

   Assert ( NativeUInt(pData) <= NativeUInt(pBase) + SINGULARITY_SIZE, 'Extreme buffer overrun!' );

  end;

 gSingularity := pBase;

 pData := RelativePtr ( pBase, 120 );
 pData^ := TProfileTimer.Create;

 SetStrWZ ( pBase, 'Iridium Balls inactive...', 128 );

 {$IFOPT D-}
 if (not IsDebuggerPresent) or (dev_comp) then
 {$ENDIF}
    UnlockRegion(@_Operator[1]);


 pData := RelativePtr ( pBase, 217 );
 // Move ( bin_staff, pData^, sizeof (bin_staff) );
 // Dec (pData, 4);
 PDWORD( pData )^ := sizeof(bin_stuff) div 4;
 pData := RelativePtr ( pBase, OFS_DATA );
 Move ( sig_list, pData^, sizeof (sig_list) ); // 64 bytes save

 ReallocMem ( gSingularity, SINGULARITY_SIZE + 16 );
 lua_pushobj ( L, gSingularity );

 lua_createtable (L, 1, 2);
 n := lua_gettop (L);
 set_mtf (L, _NewIndex, n, 'index');
 set_mtf (L, _Index, n, 'newindex');
 lua_setmetatable (L, n - 1);

 ODS ('[~T]. #DBG: xrGame.DLL detection = ' + DetectXrGame );
 lua_setglobal (L, 'last_singularity');
 lua_getglobal (L, 'last_singularity');
end;

function ExplodeSingularity (L: lua_State): Integer; cdecl;
var
   pstr: PWideChar;
   pcrc: PDWORD;
   ssrc: PIntegerArray;
   dsrc: PPointerArray;
   buff: Pointer;
   name: String;
      n: Integer;
      t: Integer;
begin
 // тупая и безрадостная подготовка к взрыву.
 pstr := lua_objptr (L, 1);
 if Assigned (pstr) then
    SetStrWZ ( pstr, 'Iridium Balls is active )', 128 );

 pcrc := RelativePtr ( pstr, 129 );
 pcrc^ := CalcCRC32 ( pstr, 128 );

 worker.WaitRequests;

 ssrc := RelativePtr (pstr, 217);
 dsrc := RelativePtr (ssrc, 16 * 4);

 t := lua_gettop (L);

 for n := 0 to High(bin_stuff) do
 if ( ssrc [n] > 0 ) and ( ssrc [n] < $1000000 ) and ( dsrc [n] <> nil ) then
  begin
   buff := dsrc [n];
   name :=  bin_names [n];

   if LoadByteCode ( L, buff, ssrc [n], name ) <> 0 then
    begin
    {$IFNDEF NLC_GOLD}
     PrintError( Format('Failed load byte code %d for namespace %s, buff = $%p', [n, name, buff]) );
    {$ENDIF}
    end;
   {

   if luaL_loadbuffer (L, buff, ssrc [n], PAnsiChar (name) ) = 0 then
     begin
      if lua_pcall (L, 0, 0, 0) <> 0 then
         SinError ( IntToStr(n) + ' pcall error: ' + LuaStrArg(L, -1) );
     end
   else
      SinError ( IntToStr(n) + ' loadbuffer error: ' + LuaStrArg(L, -1) ); // }

   lua_settop (L, t); // restore stack   x

   FreeMem ( buff );
   dsrc [n] := nil;

  end;


 result := 0;
end;


procedure Progression_0 (p: Pointer);
var
  flog: String;
   map: TStrMap;
    dd: TMD5Digest;
     t: Text;
begin
 map := p;
 try
  dd := MD5.MD5StringW (map.Text);
  {$IFOPT D+}
  flog := gLogPath + 'mercator.stat';
  {$I-}
  AssignFile (t, flog);
  if FileExists(flog) then
     Append (t)
  else
     ReWrite (t);
  WriteLn ( t, InfoFmt('[~d ~T]. #ADD_STAT') );
  WriteLn  ( t, InfoFmt ( HardMD5 (dd, TRUE) + ':' + map.Text ) );
  CloseFile (t);

  {$ELSE}
  {$ENDIF}
 finally
  map.Free;
 end;
end;

procedure Progression_1 (p: Pointer);
var
  buff: Pointer;
  size: Integer;
  dest: PIntegerArray;
     n: Integer;
begin
 if gSingularity = nil then exit;
 dest := RelativePtr (gSingularity, 217);

 for n := 0 to High (bin_stuff) do
  try
   dest [n] := 0;
   size := bin_sizes [n];
   if size = 0 then continue;

   buff := LCGlobals.DecryptXorBin ( bin_stuff [n], size );

   if buff = nil then continue;
   dest [n] := size;
   dest [n + 16] := Integer (buff);
   // if IsDebuggerPresent then break;
  except
   on E: Exception do
      OnExceptLog('source_' + IntToStr(n), E);
  end;
end;

function _profile_flush (L: lua_State): Integer; cdecl;
var
   fmp: ^TFileMapping;
   pgp: PGamerProfile;
    rr: TRootRegistry;
    gg: TGameObject;
    wc: String;
    fn: String;

begin
 result := 0;
 fmp := lua_objptr (L, 1);
 pgp := fmp.pView;
 if pgp = nil then exit;

 pgp.fcrc := pgp.FixedCRC (pgp.fcrc.Nonce64 + 1);
 rr := RootRegistry (FALSE);
 if rr <> nil then
   begin
     gg := rr.Find(0);
     if gg <> nil then
       begin
        rr.LoadAll (TRUE);
        pgp.gvid := gg.gvid;
        pgp.lvid := gg.lvid;
       end;
  end;
 pgp.pcrc := pgp.BodyCRC (pgp.pcrc.Nonce64 + 1);
 wc := fmp.szFile;
 fn := AnsiReplaceStr(wc, 'tmp.player.wcp', 'player.dat');
 if (wc <> fn) and FileExists(wc) then
     CopyFile (PChar(wc), PChar(fn), FALSE);
end;


function _profile_unload (L: lua_State): Integer; cdecl;
var
   fmp: ^TFileMapping;
begin
 fmp := lua_objptr (L, 1);
 if Assigned(fmp) then
  try
   fmp.Close (TRUE);
   fmp.hFile := 0;
   if fmp = gamer_profile then
      gamer_profile := nil;

   FreeMem (fmp);
  except
   on E: Exception do
      OnExceptLog('profile_unload', E);
  end;
 result := 0;
end;


function _profile_index (L: lua_State): Integer; cdecl;
var
   fmp: ^TFileMapping;
   pgp: PGamerProfile;
   key: String;
     i: Integer;
begin
 result := 1;
 fmp := lua_objptr (L, 1);
 key := LuaStrArg (L, 2);

 if key = 'unload' then // всегда доступная функция выгрузки
   begin
    lua_pushcfunction (L, _profile_unload);
    exit;
   end;

 pgp := fmp.pView;
 if pgp = nil then
   begin
    PrintError('lost pView for ' + fmp.szFile);
    lua_pushnil(L);
    exit;
   end;

 if key = 'name' then
    lua_pushwstr (L, pgp.name) else
 if key = 'date' then
    lua_pushwstr (L, FormatDateTime('dd.mm.yy hh:nn:ss', pgp.ctdt)) else
 if key = 'fixed_sz' then
    lua_pushinteger (L, pgp.FixedSize) else
 if key = 'flush' then
    lua_pushcfunction (L, _profile_flush) else
 if key = 'header_sz' then
    lua_pushinteger (L, pgp.FixedSize - sizeof(pgp.ecsn) - 1) else
 if key = 'size' then
    lua_pushinteger (L, fmp.nSize) else
 // pointers
 if key = 'addr' then
    lua_pushptr (L, pgp, TRUE) else
 if key = 'puid' then
    lua_pushptr (L, @pgp.puid, TRUE) else
 if key = 'ecsz' then
    lua_pushptr (L, @pgp.ecsz, TRUE) else
 if key = 'ecsn' then
    lua_pushptr (L, @pgp.ecsn, TRUE) else
 if key = 'fcrc' then
    lua_pushptr (L, @pgp.fcrc, TRUE) else
 if key = 'pcrc' then
    lua_pushptr (L, @pgp.pcrc, TRUE) else
 if key = 'dcnt' then
    lua_pushptr (L, @pgp.dcnt, TRUE) else
 if key = 'lcnt' then
    lua_pushptr (L, @pgp.lcnt, TRUE) else
 if key = 'ncnt' then
    lua_pushptr (L, @pgp.lcnt, TRUE) else
 if key = 'lvnm' then
    lua_pushptr (L, @pgp.lvnm, TRUE) else
 if key = 'gvid' then
    lua_pushptr (L, @pgp.gvid, TRUE) else


 if key = 'hwbf' then
    lua_pushptr (L, @pgp.hwbf, TRUE) else
 if key = 'hwbd' then
   begin
    lua_createtable (L, 0, High(pgp.hwbd) + 1);
    for i := 0 to High(pgp.hwbd) do
       begin
        lua_pushinteger (L, i + 1);
        lua_pushptr (L, @pgp.hwbd[i], TRUE);
        lua_settable (L, -3);
       end;
   end

 else
    lua_pushnil (L);
end;


function _profile_load (L: lua_State): Integer; cdecl;
var
   fmp: ^TFileMapping;
   glb: Boolean;
    fn: String;

begin
 result := 1;

 try
   fn := LuaStrArg (L, 1);
   if fn = '' then
      fn := '$profile_dir$\' + profile_file;

   glb := Pos ( '$profile_dir$\\player.', fn ) > 0;

   if glb and ( gamer_profile <> nil ) then
        begin
         // already loaded
         AssignMetaIndex (L, gamer_profile, _profile_index, nil, 'GMT_PLAYER_PROFILE');
         exit;
        end;

   fmp := Internal_LoadProfile(fn, LuaStrArg(L, 2));

   if (fmp <> nil) and (fmp.pView <> nil) then
     begin
      AssignMetaIndex (L, fmp, _profile_index, nil, 'GMT_PLAYER_PROFILE');
      if glb then gamer_profile := fmp;
     end
   else
     begin
      lua_pushnil (L);
      fmp.close;
      FreeMem (fmp);
     end;
 except
  on E: Exception do
     OnExceptLog('_profile_load', E);
 end;
end;



procedure AddGlobals(L: lua_State);
begin
 {$IFDEF NLC}
 lua_register (L, 'NewSingularity', NewSingularity );
 lua_register (L, 'ExplodeSingularity', ExplodeSingularity );
 lua_register (L, 'player_dat',     _profile_load);
 {$ENDIF}

 toxic_rql [0] := @IMPL_STAT[4];
 toxic_cbc [0] := Progression_0;
 toxic_rql [1] := @INF_RQSPX[4];
 toxic_cbc [1] := Progression_1;

end;


procedure DoCleanup;
begin
 try
  FreeMem (gSingularity);
  FreeAndNil (deploy);

  gSingularity := nil;
 except
  on E: Exception do
     SinError('Exception catched in DoCleanup: ' + E.Message );
 end;
end;

{$ENDIF}

initialization

finalization

end.

