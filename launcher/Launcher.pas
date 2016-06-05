unit Launcher;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StrUtils, Types, Buttons, Math, IniFiles, ShellAPI, ShlObj, LGlobal, DateTimeTools,
  Dialogs, Misc, StrClasses, NLCPack, Zlib, MD5, TlHelp32, ImageHlp, StdCtrls, ExtCtrls, PicButton, JPEG, Patcher, GameConfig, CountBack, LuaTools;

{$I stkdef.inc}
{$A4}

type
    SIZE_T = DWORD;
    lua_State = Pointer;
    lua_Alloc = function (ud, p: Pointer; osize, nsize: SIZE_T): Pointer;  cdecl;


type
  TTransparentListBox = class (TListBox)
  private
    last_up_item: Integer;
    FBackground: TImage;
   procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure SetBackground(const Value: TImage);
  protected
   procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
  public

  published
   property       Background: TImage read FBackground write SetBackground;
  end; //



  TPostInitProc = procedure;

  TLForm = class(TForm)
    imgBackground: TImage;
    updTimer: TTimer;
    lbStatus: TLabel;
    pbExit: TPicButton;
    pbGoMainMenu: TPicButton;
    pbLoadGame: TPicButton;
    pbConfig: TPicButton;
    lbProfileInfo: TLabel;
    procedure btnGoMainMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnLoadGameClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure updTimerTimer(Sender: TObject);
    procedure sbExitClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure pbConfigClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbProfileInfoDblClick(Sender: TObject);
  protected


  private
    { Private declarations }
    n_ticks: Integer;
          have_sign: Boolean;
    Factive_profile: String;
        fsg_profile: String;


    procedure CheckProfile;
    procedure LoadConfig;
    procedure LoadProfiles;
    procedure SetBtnImage(btn: TSpeedButton; const fname, def_caption: String);
    procedure ProcessTimer;
    procedure MoveBtn(btn: TControl; x, y, n: Integer);
    procedure DecryptImage(idx: Integer);
    procedure SetActiveProfile ( const s: String );

  public
    { Public declarations }


           lbSaves: TTransparentListBox;

    property    active_profile: String read Factive_profile write SetActiveProfile;

    { methods }

    procedure   DumpSaves;

    procedure   HideExit;

    procedure   PatchFSGame;
  end;

var
  LForm: TLForm;

  GameProcessAffinity: DWORD = 0;
   form_caption: String;

   sv_hide_ext,
   sv_hide_suffix,
   sv_scan_mask: String;

   svbox_font_nm: String = 'Fixedsys';
   svbox_font_sz: Integer = 10;
   svbox_org_x: Integer = 20;

   btns_org_x: Integer = 265;
   btns_org_y: Integer = 40;
   btns_width: Integer = 208;
   btns_height: Integer = 44;
   btns_offset: Integer = 44;
   form_on_top: Boolean = TRUE;
   max_show_saves: Integer = 15;





// function  BeginProcessDebug (pid: DWORD): Boolean; stdcall;
// function  EndProcessDebug : Boolean; stdcall;
procedure AttachTarget;
procedure ExecTarget;
procedure InitModule;
procedure EnableVEH(bEnable: Boolean);  stdcall;
procedure FindLastSave(dump: TStrings = nil);
function  InfiltrateToExe(const sExeFile, sParams, sWorkDir: String): DWORD;
procedure InfiltrateToRuned(const sWnd: String);

implementation
uses LCGlobals, GamerProfile, madExcept;

{$R *.dfm}

var
   xr_lock: THandle = INVALID_HANDLE_VALUE;

 //  external LUAICP_DLL;
procedure VERIFY(condition: Boolean; msg: String);
begin
 if not condition then
    Application.MessageBox (PChar(msg), PChar('Assertion made'), MB_OK or MB_ICONERROR);

 Assert (condition, msg);
end;


procedure InitModule;
begin
 ODS('[~d ~T/~B/~U]. #PERF: launcher initializing...');
 AttachInterceptor;
 ODS('[~d ~T/~B/~U]. #DBG: InitModule complete. hDLL = $' + IntToHex (hLib, 8) );
end;


procedure EnableVEH(bEnable: Boolean);  stdcall; external LUAICP_DLL;
procedure SetGameProcess (hp: THandle); stdcall; external LUAICP_DLL;
function  SignalDisplayed: Boolean;     stdcall; external LUAICP_DLL;


// function lua_newstate(f: lua_Alloc; ud: Pointer) : lua_State; cdecl;    external LUA_DLL;

// procedure Init; stdcall; external LUA_CAPDLL;


// procedure InstallTo(hWnd, tid: DWORD); cdecl; external LUA_CAPDLL;
// function  DMAlloc(ud, p: Pointer; osize, nsize: SIZE_T): Pointer; cdecl; external LUA_CAPDLL;



procedure PackLastLogs;
var
  log_list: TStrMap;
  log_path: String;
  fl: TFileList;
  s: String;
begin
 log_list := TStrMap.Create;
 fl := TFileList.Create;
 try
  log_path := AddSlash( ExpandPath( '$logs$' ) );
  s := CorrectFilePath( ExePath + 'bugreport.txt' );
  if (s <> '') and FileExists (s) then
      log_list.Add (s);


  if DirectoryExists (log_path) then
   begin
    fl.SortBy := SortByUpdateDate;
    fl.FindFiles (log_path + 'xray*.log');
    if fl.Count > 0 then  log_list.Add (log_path + fl [fl.Count - 1]);
    fl.FindFiles (log_path + 'xr_*.log');
    if fl.Count > 0 then  log_list.Add (log_path + fl [fl.Count - 1]);
    fl.FindFiles (log_path + 'launch*.log');
    if fl.Count > 0 then  log_list.Add (log_path + fl [fl.Count - 1]);
   end;
  ODS('[~T]. #DBG: Founded logs to archive: ~C0A'#13#10 + log_list.Text + '~C07');

  ArchiveFiles ( log_list, log_path + 'logs_pack-' + FormatDateTime ('yyyy.mm.dd@hh-nn', Now)  + '.nlcp', False, False); // не добавлять (заместить архив), не удалять исходные файлы

 finally
  log_list.Free;
  fl.Free;
 end;
end;


procedure CheckSaveAutobackup;
var
   fl: TFileList;
   sl: TStrMap;
   dt: TDateTime;
   dir: string;
   n: Integer;

begin
 fl := TFileList.Create;
 sl := TStrMap.Create;
 try
  fl.SortBy := SortByUpdateDate;
  fl.FindFiles (saves_dir + sv_scan_mask);
  if fl.Count = 0 then exit;
  n := fl.Count - 1;
  dt := fl.Items [n].lwTime;

  if dt <= last_sv_date then exit;

  last_sv_date := dt;

  sl.Add ( saves_dir + fl [n] );

  dir := CorrectFilePath (ExePath + '..\backup\');
  CheckMakeDir (dir);
  ArchiveFiles (sl, dir + 'svbackup-' + FormatDateTime ('yyyy.mm.dd@hh-nn', Now)  + '.nlcp', False, False);
 finally
  sl.Free;
  fl.Free;
 end;
end; // CheckSaveAutoback


{$A-}
type
    TConfigDataBlock = packed record
     cbSize: UInt16;
     hLib: HModule;
     DLLName: array [0..MAX_PATH + 1] of AnsiChar;
    ProcName: array [0..31] of AnsiChar;
     evtName: array [0..31] of AnsiChar;
     loadLib: function (lib: PAnsiChar): HModule; stdcall;  // LoadLibrary
      gpaddr: function (hLib: HModule; proc: PAnsiChar): Pointer; stdcall; // GetProcAddress
       _init: procedure (ret_ctx: PContext); stdcall;
      _sleep: procedure (dwMsec: DWORD); stdcall;
     ret_ctx: TContext;
    end; // TConfigDataBlock

    PConfigDataBlock = ^TConfigDataBlock;

const
     CDB_SIZE = sizeof (TConfigDataBlock);



// данная функция бинарно копируется в адресное пространство процесса-жертвы. Никакие строкововые константы из этой функции недоступны!
procedure RemoteLoadDLL(pBlock: PConfigDataBlock); stdcall;
begin
 if pBlock = nil then exit;

 if pBlock.cbSize <> CDB_SIZE  then exit;

 with pBlock^ do
  begin
   if (@loadLib = nil) or (@gpaddr = nil) or (@_sleep = nil) then asm int 3 end;
   hLib := loadLib(DLLName);
   _init := gpaddr(hLib, ProcName);
   if @_init <> nil then _init(@ret_ctx);
   _sleep(1000);   // wait stand-by
   SetEvent(ready_event);
  end;
end; // RemoteLoadDLL


type
   TPushRec32 = packed record
    bcmd: BYTE; // $68
    dw: UInt32;  // data
   end;

procedure WaitInit;
var
   n: Integer;
   h: THandle;
begin
  h := 0;
  for n := 1 to 30 do
   begin
    h := OpenEvent (SYNCHRONIZE, FALSE, 'Global\luaicp_init');
    if h <> 0 then break;
    XSleep(100);
   end;
  if h = 0 then exit;
  WaitForSingleObject (h, 5000);
  SetEvent(ready_event);
end;

function WaitTargetExit(hProcess: THandle): DWORD;
var
   msg: tagMSG;
      i: Integer;
begin
   i := 0;
   while WaitForSingleObject(hProcess, 250) = WAIT_TIMEOUT do
    begin
     Inc (i);
     ImNotFrozen;

     if ( i and 15 = 0 ) and ( sv_autobackup ) then
         CheckSaveAutoBackup;

     if PeekMessage (msg, 0, 0, 0, PM_REMOVE) then
      begin
       TranslateMessage (msg);
       DispatchMessage (msg);
      end;

     if IsKeyPressed (VK_CONTROL) and IsKeyPressed (VK_MENU) then
       begin
        if IsKeyPressed ( VK_PAUSE ) then
           TerminateProcess (hProcess, $200030);
        // if IsKeyPressed ( Ord ('D') ) then BeginProcessDebug ( pinf.dwProcessId );
       end;
     SleepEx(20, TRUE);
    end;



   {$IFOPT D-}
   PackLastLogs;
   {$ENDIF}
   GetExitCodeProcess (hProcess, result);
   ODS ('[~T].~C07 #MSG: Process completed with exit code~C0D $' + IntToHex (result, 8) + '~C07');

   while SignalDisplayed do
         SleepEx(500, TRUE);
end;


procedure AttachTarget;
var
   hp: THandle;
begin
 hp := OpenProcess (SYNCHRONIZE or PROCESS_TERMINATE, FALSE, target_pid);
 SetGameProcess (hp);
 game_process := hp;
 if hp = 0 then
   begin
    wprintf('[~T].~C0C #FATAL:~C07 failed to attach to process $%x, error = %s ', [target_pid, err2str]);
    exit;
   end;

 WaitTargetExit(hp);
 CloseHandle (hp);
 SetGameProcess (0);
 game_process := 0;
end;

function InfiltrateToExe(const sExeFile, sParams, sWorkDir: String): DWORD;
label x0;

var
   sPath: PChar;

   sinf: TStartupInfo;
   pinf: TProcessInformation;

   ctx_save, ctx_new: TContext;
   pBlock: PByteArray;
   lData: TConfigDataBlock;
   cofs, start, fstart: Integer;
   wb: NativeUInt;
   prc: TPushRec32;
   hLib: NativeUInt;
   buff: packed array[0..16383] of BYTE;
   s: String;
   i: Integer;

  procedure PutByte(b: BYTE);
  begin
   buff[cofs] := b;
   Inc (cofs);
  end;

  procedure StoreBytes(const src; bcnt: WORD);
  begin
   Move(src, buff[cofs], bcnt);
   Inc(cofs, bcnt);
  end;



begin
 result := 0;
 // s := ExtractFilePath(sExeFile);
 sPath := IfV (sWorkDir <> '', PChar(sWorkDir), nil);
 // if s <> '' then sPath := PChar(s);
 FillChar (sinf, sizeof(sinf), 0);
 FillChar (pinf, sizeof(pinf), 0);

 sinf.cb := sizeof(sinf);
 sinf.wShowWindow := SW_HIDE;

 ODS ('[~T]. #DBG: Запуск процесса ~C0A' + sExeFile + '~C07, с параметрами {~C0A ' + sParams +
           '~C07 } , рабочая папка =~C0A ' + sWorkDir + '~C07, в приостановленном состоянии...');
 Sleep(50);

 if not CreateProcess (nil, PChar(sExeFile + ' ' + sParams), nil, nil, FALSE, CREATE_SUSPENDED,  nil, sPath, sinf, pinf) then
    begin
     ODS('[~T].~C0C #ERROR: CreateProcess returned error: ~C0F ' + Err2Str(GetLastError) + '~C07');
     exit;
    end;

 if pinf.hThread = 0 then
    begin
     ODS('[~T].~COC #ERROR: CreateProcess error, pinf.hThread = 0~C07');
     exit;
    end;

 if GameProcessAffinity <> 0 then
    SetProcessAffinityMask (pinf.hProcess, GameProcessAffinity);
 // OpenThread (THREAD_ALL_ACCESS, TRUE, tid);

 {$IFDEF NEWEST_BUILD}
 WaitInit();
 {$ELSE}
 FillChar(ctx_save, sizeof(ctx_save), 0);


 ctx_Save.ContextFlags := CONTEXT_FULL;

 if not GetThreadContext (pinf.hThread, ctx_save) then
   begin
    ODS('~C0C GetThreadContext returned error: ~C0F ' + Err2Str(GetLastError) + '~C07');
    exit;
   end;

 ctx_new := ctx_Save;

 ODS(Format('[~T]. #DBG: Main thread $%x stopped at ~C0D $%X~C07, esp =~C0D $%X~C07', [pinf.hThread, ctx_save.Eip, ctx_save.esp]));

 FillChar(buff, 16384, $90); // fill with nop's

 start := 0;
 fstart := 0;
 pBlock := nil;
 // Инициализация блока данных, передаваемого удаленной функции
 try
   FillChar(lData, CDB_SIZE, 0);
   ldata.cbSize := CDB_SIZE;
   StrPCopy (ldata.DLLName, LUAICP_DLL);
   StrPCopy (ldata.ProcName, 'Init');
   StrPCopy (ldata.evtName, '\\global\syncXCapture');
   hLib := LoadLibrary('kernel32.dll');
   ldata.loadLib := GetProcAddress(hLib, 'LoadLibraryA');
   ldata.gpaddr := GetProcAddress(hLib,  'GetProcAddress');
   ldata._sleep := GetProcAddress(hLib,  'Sleep');
   ldata.ret_ctx := ctx_Save;
   Move(ldata, buff, CDB_SIZE );
   start := Max ( 2048, (1 + CDB_SIZE div 16) * 16 );
   // aligned start of code
   cofs := start;
   fstart := 8192;                     // функция начинается здесь
   // выделение блока в адресном пространстве "жертвы"
   pBlock := VirtualAllocEx ( pinf.hProcess, nil, 16384, MEM_COMMIT, PAGE_EXECUTE_READWRITE);



   prc.bcmd := $68;
   prc.dw := ctx_Save.Eip;             // return addr
   StoreBytes(prc, sizeof(prc));       // push eip_orig for return to orig.
   PutByte($60);                       // pushad
   prc.dw := DWORD(pBlock) + DWORD(fstart);   // addr to func
   StoreBytes(prc, sizeof(prc));       // push eip_orig for return to orig.
   PutByte($58);                       // pop eax
   prc.dw := DWORD (pBlock);
   StoreBytes(prc, sizeof(prc));       // push pBlock ptr
   PutByte($FF); PutByte($D0);         // call eax -> my func
   PutByte($61);                       // popad
   PutByte($C3);                       // ret
   Move( Addr(RemoteLoadDLL)^, buff[fstart], 7000); // скопировать с запасом
   // RemoteLoadDLL (nil);
   ctx_new.ContextFlags := CONTEXT_CONTROL;
   ctx_new.Eip := ( DWORD(pBlock) + DWORD(start));
 except
  on E: Exception do
   OnExceptLog ('Prepare inception', E);
 end;

 ODS( CFormat('[~T]. #DBG: Remote memory block allocated at $%P, target EIP = %X, back EIP = %X, RemoteLoadDLL_func = %X ', '~C07',
                         [pBlock, ctx_new.eip, ctx_save.Eip, DWORD(pBlock) + fstart]) );
 wb := 0;

 if WriteProcessMemory (pinf.hProcess, pBlock, @buff[0], 16384, wb) then
    begin
     s := '';
     for i := 0 to 15 do
         s := s + IntToHex ( buff [i + start], 2 ) + '  ';
     ODS( Format('[~T]. #DBG: WriteProcessMemory success. Copyed~C0D %.3f~C07 KiB from 16K. Buffer test dump '#13#10#9'~C0F %s ~C07',
                [wb / 1024, s]) );
    end
 else
    begin
     PrintError('WriteProcessMemory  failed. LastError = ' + Err2Str(GetLastError));
     TerminateProcess (pinf.hProcess, 0);
     exit;
    end;

 if SetThreadContext(pinf.hThread, ctx_new) then
    ODS( Format(' Process now paused at~C0D $%X~C07, ~C0C sleeping 0.5 seconds ~C07', [ctx_new.eip])); //}

 Sleep (500);

 if ( dbg_present or pause_run ) and ( show_con ) then
    try
     ODS ('[~T]. #DBG: For continue press ENTER..');
     ReadLn;
    except
     on E: Exception do PrintError ('Exception catched while calling ReadLn: ' + E.Message);
    end;
 {$ENDIF}
 ResumeThread(pinf.hThread);
 SetGameProcess (pinf.hProcess);
 game_process := pinf.hProcess;
 game_pid     := pinf.dwProcessId;

 if debug_mode then
  begin
   ODS('[~T]. #DBG: Подключение отладчика к запущенному процессу... ');
   XSleep (2000); // TODO: replace for init-wait
   BeginProcessDebug ( pinf.dwProcessId );
  end;

 if service_mode then
  begin
   ODS('[~T]. #MSG: Process ' + sExeFile + ' waiting complete or breaked...');
   result := WaitTargetExit (pinf.hProcess);
  end;

 SetGameProcess (0);
 game_process := 0;
 CloseHandle(pinf.hProcess);
end; // InfiltrateToExe

procedure InfiltrateToRuned(const sWnd: String);
var
   hWnd, tid: DWORD;
begin
 hWnd := FindWindow (nil, PWideChar(sWnd));
 if hWnd = 0 then
   begin
    ODS('Window "' + sWnd + '" not found. Hooking aborted' );
    exit;
   end;


 ODS ('Trying hook window "' + sWnd + '"');
 tid := GetWindowThreadProcessId(hWnd);
 if tid = 0 then exit;
 // if (tid <> 0) then  InstallTo (hWnd, tid);
end; // InfiltrateToRunded


var
   xrp: String;

procedure ExecTarget; // HLL entry <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
var
   dmp: TStrMap;

begin

 target_exe := AnsiReplaceStr (target_exe, '%20', ' ');

 ODS( '[~T]. #DBG: target_exe = ~C0A' + target_exe + '~C07' );



 if FileExists(target_exe) then
  try
   xrp := xray_params;

   repeat
     // адаптация параметров игры для загрузки сохранений

     if load_save and ( Pos('-start', xray_params) = 0 ) then
       begin
        if load_sv = '' then
          begin
           dmp := TStrMap.Create;
           FindLastSave (dmp);
           FreeAndNil (dmp);

           load_sv :=  DelSubStr (last_savegame, sv_hide_ext);
          end;

        if (load_sv <> '') then
            xrp := xray_params + ' -start server(' + load_sv + '/single/alife/load) client(localhost)';
       end;



     if InfiltrateToExe (target_exe, xrp, work_dir) = 0 then
       begin
        ODS('[~T]. #DBG: Normal exit from game detected.');
        break;
       end;

     // ODS('[~T]. #DBG: Trying rerun game...');
     ODS('[~T]. #DBG: abnormal game exit...');

     {
     BreakoutForm := TBreakoutForm.Create (Application);
     if BreakoutForm.ShowModal = mrAbort then
       begin
        ODS('[~T]. #DBG: User break accepted');
        BreakoutForm.Free;
        break;
       end;
     BreakoutForm.Free;
      }
     load_save := TRUE;
     last_savegame := '';
     load_sv := '';

     break;
   until FALSE;

   SetLength (xrp, 0);


  except
   on E: Exception do
      OnExceptLog ('ExecTarget ' + target_exe, E);
  end
 else
    InfiltrateToRuned (target_exe);

end;

procedure FindLastSave;
var
   fl: TFileList;
   sv, svl: String;
    n: Integer;
    p: Integer;
begin
 if not DirectoryExists (saves_dir) then
    begin
     PrintError ('Directory saves_dir not exists: ~C0F {' + saves_dir + '}');
     exit;
    end;

 fl := TFileList.Create;
 try
  fl.SortBy := SortByUpdateDate;
  sv := saves_dir + '*' + DEF_SAVE_EXT; // sv_scan_mask
  ODS('[~T]. #DBG: Searching saves with mask ~C0A' + sv + '~C07');

  fl.FindFiles (sv);


  svl := '';

  if dump <> nil then
    for n := fl.Count - 1 downto 0 do
      begin
       sv := fl [n];
       if sv_hide_suffix = '' then
          p := 1
       else
          p := Pos(sv_hide_suffix, sv);


       // extended filtration (override)
       if ( p = 0 ) and ( Pos ('_autosave.', sv) = 0 ) then
           begin
            ODS(#9#9'#DBG: rejected file ~C0A' + sv + '~C07');
            fl.Delete (n);
            continue;
           end;


       if sv_hide_suffix <> '' then
          sv := AnsiReplaceStr ( sv, sv_hide_suffix, '' );

       sv := AnsiReplaceStr ( sv, DEF_SAVE_EXT, '' );

       if dump.Count < max_show_saves then
          dump.Add (sv);

       svl := svl + #9 + FormatDateTime ( 'yy.mm.dd hh:nn:ss', fl.Items [n].lwTime ) + #9#9 + sv + #13#10;
      end;


  if fl.Count > 0 then
    begin
     ODS(#9#9'#DBG: saves by last-write-time: ~C0A'#13#10 + svl + '~C07');
     last_savegame := fl [fl.Count - 1]; // самый новый, с наибольшей датой изменения
    end;

 finally
  fl.Free;
 end;
end;


procedure TLForm.btnGoMainMenuClick(Sender: TObject);
begin
 go_exit := FALSE;
 Hide;
 try
  ExecTarget;
  Close;
 except
  on E: Exception do
     OnExceptLog('GoMainMenuClick', E);
 end;

end;

procedure TLForm.btnLoadGameClick(Sender: TObject);
begin
 load_sv := Trim ( lbSaves.Items[lbSaves.ItemIndex] ) + sv_hide_suffix;
 load_save := TRUE;
 go_exit := FALSE;
 Hide;
 try
  ExecTarget;
  Close;
 except
  on E: Exception do
     OnExceptLog('GoMainMenuClick', E);
 end;
end;

procedure TLForm.CheckProfile;
var
    f: TFileStream;
    b: array [0..47] of AnsiChar;


begin
 have_sign := FALSE;
 player_dat := ExpandPath('$profile_dir$\player.dat');

 if FileSizeEx(player_dat) = 708  then
  try
   f := TFileStream.Create(player_dat, fmOpenRead);
   f.Position := $1F4;
   f.Read(b, 42);
   if StrPos (b, '****') = nil then
      have_sign := TRUE;
   FillChar (b, sizeof(b), 0);
   f.Seek(1, TSeekOrigin.soCurrent);
   f.Read(b, 19);
  finally
   f.Free;
  end;

 lbProfileInfo.Caption := IfV(have_sign, 'Профиль подписан!', 'Профиль не подписан!');
 lbProfileInfo.Visible := TRUE;
 lbProfileInfo.Hint := IfV(have_sign, 'Время подписи: ' + String ( PAnsiChar(@b) ), 'Двойной щелчок, чтобы найти файл');
end;

procedure TLForm.DecryptImage(idx: Integer);
type
   WREC = packed record
    case BYTE of
     0: (ab, bb: BYTE);
     2: (ww: WORD);
   end;


var
   rss: TResourceStream;
   mst: TMemoryStream;
   jim: TJPEGImage;
   data: array of WREC;
   resd: array of AnsiChar;
   mask: BYTE;
   n: Integer;


begin

 mst := TMemoryStream.Create;
 jim := TJPEGImage.Create;

 rss := TResourceStream.Create (HInstance, 'RLIST' + IntToStr(idx), RT_RCDATA);
 try
  if rss.Size <= 0 then exit;

  SetLength (data, rss.Size div sizeof(WREC));
  SetLength (resd, rss.Size div sizeof(WREC));
  rss.Read (data[0], rss.Size);

  for n := 0 to Length (data) - 1 do
  with data[n] do
   begin
    mask := 1 shl (n and 7);
    resd [n] := AnsiChar ( ab xor bb xor mask );
   end;

  mst.Write (resd[0], Length(resd));
  mst.Position := 0;
  jim.LoadFromStream (mst);

  jim.DIBNeeded;


  imgBackground.Picture.Bitmap.Assign (jim);
 finally
  rss.Free;
  jim.Free;
  mst.Free;
  SetLength (resd, 0);
  SetLength (data, 0);
 end;
end;

procedure TLForm.DumpSaves;
begin

 VERIFY ( Pos('\', saves_dir) > 0, 'Некорректная папка saves_dir: ' + saves_dir);

 lbSaves.Items.Clear;
 FindLastSave ( lbSaves.Items );
 if lbSaves.Items.Count > 0 then
   begin
    lbSaves.ItemIndex := 0;
    lbSaves.Visible := TRUE;
    pbLoadGame.Enabled := (xr_lock = INVALID_HANDLE_VALUE);
   end;
end;

// DecrypteImage

procedure TLForm.SetActiveProfile(const s: String);
var
   fn: String;
   pc: Integer;
   gp: TGamerProfile;
   rf: TRandomHash;
    f: TFileStream;
begin
 Caption := form_caption + ' - ' + s;

 if s = Factive_profile then exit;
 Factive_profile := s;
 if s <> fsg_profile then PatchFSGame;
 wprintf('[~T]. #DBG: SetActiveProfile("%s")', [s]);

 if saves_dir = '' then
    begin
     ODS('[~T].~C0C #WARN:~C07 saves_dir undefined, trying re-detect');
     saves_dir := ExpandPath('$game_saves$');
    end;

 DumpSaves;

 Assert ( Length(s) < 16, 'To large profile name' );


 fn := ExpandPath('$profile_dir$');
 CheckMakeDir (fn);
 fn := fn + '\' + profile_file;


 if FileExists (fn) and ( FileSizeEx (fn) = sizeof (gp) ) then
   begin
    f := TFileStream.Create (fn, fmOpenRead);
    f.Read (gp, sizeof(gp));
    // TODO: checking binary file profile
    f.Free;
    if DWORD (gp.ftag) = PDWORD (PROFILE_TAG)^ then
     begin
      hash_permut := gp.gpmt;
      rf := gp.BodyCRC (gp.pcrc.Nonce64);
      if rf.Compare(gp.pcrc) then exit;
     end;
    ODS ('[~T].~CCF #ERROR:~CCE profile data invalid, recreating player.dat !!!~C07');
   end;

  Randomize;
  f := TFileStream.Create (fn, fmCreate);
  mix_bytes (@hash_permut[1], 31);
  // creating new binary profile
  FillChar (gp, sizeof(gp), 0);
  Move (PROFILE_TAG^,gp.ftag, 4);
  gp.fver := $10007;
  gp.gpmt := hash_permut;
  gp.pcrc.CheckPermut;
  gp.ctdt := PreciseTime ();
  StrPCopy (gp.name, s);
  gp.puid.RandInit();
  gp.puid.Calc (@gp.name, sizeof(gp.name) + sizeof(gp.ctdt));  // profile key all-time fixed
  pc := atoi ( NewProfileDialog.edtPIN.Text );
  Move ( init_permut()[1], gp.hwbd, Min (256, sizeof (gp.hwbd)) );
  gp.ecsz := 42;
  FillChar (gp.ecsn, sizeof(gp.ecsn), '*');
  gp.gvid := $FFFF;
  gp.lvid := $AAAA;
  gp.lvnm := $0001; // escape
  gp.pins.RandInit;
  gp.pins.Calc ( @pc, 4 );
  gp.fcrc := gp.FixedCRC (0);
  gp.pcrc := gp.BodyCRC (0);
  rf := gp.FixedCRC(gp.fcrc.Nonce64);
  ASSERT ( gp.fcrc.Compare(rf), Format('Hash algorithm failed:\n %s <> %s ', [gp.fcrc.Format, rf.Format]) );
  f.Write ( gp, sizeof (gp) );
  f.Free;
  CopyFile ( PChar(fn), PChar(AnsiReplaceStr(fn, '.dat', '.new')), FALSE);
end;

procedure TLForm.SetBtnImage(btn: TSpeedButton; const fname, def_caption: String);
var
   dst, src: TRect;
   cx, cy: Integer;

   procedure OfsCopy(n: Integer);
   begin
    SetRect (dst, cx * n, 0, cx * (n + 1), cy);
    btn.Glyph.Canvas.CopyRect (dst, imgBackground.Canvas, src);
   end;


begin
 if not FileExists(ExePath + fname) then
  begin
   btn.Caption := def_caption;
   exit;
  end;


 btn.Glyph.LoadFromFile (ExePath + fname);
 btn.NumGlyphs := 4;

 exit;

{ btnTemp.Glyph.Transparent := TRUE;
 btnTemp.Glyph.LoadFromFile (ExePath + fname);

 cx := btn.Width;
 cy := btn.Height;

 SetRect (src, btn.Left, btn.Top, btn.Left + cx - 1, btn.Top + cy - 1);
 OfsCopy (0);
 OfsCopy (1);
 OfsCopy (2);
 OfsCopy (3);



 TransparentBlt ( btn.Glyph.Canvas.Handle,
                  0, 0, cx * 4 - 1, cy - 1,
                  btnTemp.Glyph.Canvas.Handle,
                  0, 0, cx * 4 - 1, cy - 1,
                  RGB(0, 0, $FF) );
                                                   }
end;


procedure TLForm.ProcessTimer;
begin
 Inc (n_ticks);
 try
   if n_ticks = 2 then
     begin
      PostInit (); // PostInit procedure
      {$IFDEF PROFILES}
      LoadProfiles ();
      {$ENDIF}

      DumpSaves;
      pbGoMainMenu.Enabled := (xr_lock = INVALID_HANDLE_VALUE);
      pbExit.Enabled := TRUE;
      pbExit.OnClick := sbExitClick;
     end;
   if n_ticks = 3 then
    begin
     if pbGoMainMenu.Enabled then
        begin
         pbLoadGame.OnClick := btnLoadGameClick;
         pbGoMainMenu.OnClick := btnGoMainMenuClick;
         lbStatus.Caption := 'Готов к запуску.';
        end
       else
         lbStatus.Caption := 'Запуск невозможен, проверьте правильность установки мода!';
     updTimer.Interval := 300;
     CheckProfile;
    end;
 except
  on E: Exception do
   OnExceptLog (className + '.ProcessTimer', E);
 end;
 if n_ticks in [2..5] then
    ODS('[~T]. #DBG: LForm.n_ticks = ' + IntToStr(n_ticks));

end;

procedure TLForm.sbExitClick(Sender: TObject);
begin
 go_exit := TRUE;
 HideExit;
end;

procedure TLForm.updTimerTimer(Sender: TObject);
begin
 updTimer.Enabled := FALSE;
 ProcessTimer;
 updTimer.Enabled := TRUE;
end;

procedure TLForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
 CanClose := (n_ticks > 0);
 if CanClose then Hide;
end;

procedure TLForm.MoveBtn (btn: TControl; x, y, n: Integer);
begin
 btn.Left := btns_org_x + x;
 btn.Top  := btns_org_y + y + n * btns_offset;
 btn.Width := btns_width;
 btn.Height := btns_height;
end;

procedure TLForm.PatchFSGame;
var
   sm: TStrMap;
   fn: String;
    s: String;
    n: Integer;
    i: Integer;
begin
 if active_profile = '' then
   begin
    PrintError('active_profile not selected. PatchFSGame breaked!');
    exit;
   end;
 fn := ExpandPath ('$profiles$');
 if fn = 'bad_file_name' then
   begin
    PrintError('FATAL: cannot retrieve path $profiles$ ');
    exit;
   end;
 CheckMakeDir(fn + '\' + active_profile);

 fn := ExpandPath ('$fs_root$') + '\fsgame.ltx';
 if Pos( 'bad_file_name', fn ) > 0 then
    begin
     PrintError('FATAL: cannot retrieve path $fs_root$ ');
     exit;
    end;

 if not FileExists (fn) then exit;

 sm := TStrMap.Create;
 try
  sm.LoadFromFile (fn);
  i := sm.FindSub ('$profile_dir$'); // find sub string
  s := '$profile_dir$    = false| true| $profiles$|    ' + active_profile;


  if i < 0 then
     sm.Insert(1, s)
  else
     sm [i] := s;


  sm.SaveToFile (fn);
 finally
  sm.Free;
 end;

 if Assigned (_ReInit) then
   begin
    _ReInit;
    saves_dir := ExpandPath ('$game_saves$');
    wprintf('[~T]. #DBG: retrieved saves_dir = %s', [saves_dir]);
    CheckMakeDir (saves_dir);
    CheckProfile;
   end;
end;

procedure TLForm.pbConfigClick(Sender: TObject);
var
   dlg: TConfigDialog;
begin
 Hide;
 //  dlg := TConfigDialog.Create ( Application );
 with ConfigDialog.lbxProfiles do
  begin
   Items.Assign ( all_profiles );
   ItemIndex := Items.IndexOf ( active_profile );
  end;

 ConfigDialog.ShowModal;
 Show;
end;

procedure TLForm.FormCreate(Sender: TObject);
var
   bg_image: String; //
   sp: String;
   ap: String;
   up: String;
   fl: TFileList;
    i: Integer;
    c: Integer;
begin
 Randomize;
 {$IFDEF NLC_GOLD}
 xr_lock := CreateFile ( PChar( ExpandPath ('$game_config$\game.ltx') ), GENERIC_WRITE,  0, nil, OPEN_EXISTING, 0, 0);
 {$ENDIF}
 // EnableVEH (TRUE);
 MoveBtn (pbGoMainMenu, 0, 0, 0);
 MoveBtn (pbLoadGame,   0, 0, 1);
 MoveBtn (pbExit,       0, 0, 2);
 MoveBtn (pbConfig,     0, 0, 3);

 Caption := form_caption;

 lbSaves := TTransparentListBox.Create (imgBackground);
 lbSaves.Visible := FALSE;

 lbSaves.Top :=  btns_org_y;
 lbSaves.Left := svbox_org_x;
 lbSaves.Width := btns_org_x - lbSaves.Left - 40;
 lbSaves.Height := lbStatus.Top - lbSaves.Top - 15;
 lbSaves.SetParent (self);
 lbSaves.ControlStyle := [csParentBackground];
 lbSaves.BorderStyle := bsNone;
 lbSaves.Brush.Style := bsClear;
 lbSaves.Color := clGray;
 lbSaves.Font.Color := clWhite;
 lbSaves.Font.Name := svbox_font_nm;
 lbSaves.Font.Size  := svbox_font_sz;
 lbSaves.Style := lbOwnerDrawFixed;

 SetClassLong (lbSaves.Handle, GCL_HBRBACKGROUND, 0);


 bg_image := 'nlc_background.bmp';

 fl := SearchFiles (ExePath + 'nlc_background*.bmp');
 i := fl.Count;

 if i > 0 then
    bg_image := fl [ Random (i) ];

 fl.Free;


 // imgBackground.Picture.Graphic.LoadFromStream ();

 DecryptImage ( 1 + Random(3) );

 {$IFOPT D+}
 if FileExists(ExePath + bg_image) then
    imgBackground.Picture.LoadFromFile (ExePath + bg_image);
 {$ENDIF}


 lbSaves.Background := imgBackground;

 SetBtnImage(pbGoMainMenu, 'btn_main_menu.bmp', 'Главное меню');
 SetBtnImage(pbLoadGame,   'btn_load_game.bmp', 'Загрузить игру');
 SetBtnImage(pbExit,       'btn_exit.bmp',      'Выход');


 if not form_on_top then
   FormStyle := fsNormal;
 //
 LoadConfig;

 ODS('[~T]. #DBG: Form create complete');
 c := 0;
 up := '';

 ap := UpperCase ( GetEnvironmentVariable('ProgramData') );
 sp := UpperCase ( GetEnvironmentVariable('APPDATA') );
 if Pos(ap, UpperCase(ExePath)) > 0 then up := ap;
 if Pos(sp, UpperCase(ExePath)) > 0 then up := sp;
 if up <> '' then
    Application.MessageBox ( PChar('Модификация установлена в нерекомендуемую папку ' + up),
                             'Предупреждение', MB_OK or MB_ICONWARNING );



 for i := 1 to Length(ExePath) do
  if UpCase (ExePath[i]) in [' ', '0'..'9', ':', '+', '-', '.', 'A'..'Z', '\', '_'] then
  else c := i;
 if c > 0 then
    Application.MessageBox (PChar('In path "' + ExePath + '" detected untypical characters at ' + IntToStr(c) +
                             #13#10' We recommend use only base ASCII chars.'),
                            'X-Ray warning', MB_OK or MB_ICONWARNING);

end;


procedure TLForm.FormDestroy(Sender: TObject);
begin
 ODS('[~T]. #DBG: FormDestroy... ');
end;

procedure TLForm.FormPaint(Sender: TObject);
begin
 {imgBackground.Repaint;
 lbSaves.Repaint;
 pbExit.Repaint;}
end;

procedure TLForm.HideExit;
begin
 Hide;
 Close;
end;

procedure TLForm.lbProfileInfoDblClick(Sender: TObject);
var
   pidl: Pointer;
begin
 // if have_sign then exit;

 while not FileExists(player_dat) do
    LoadProfiles;

 pidl := ILCreateFromPathW ( PChar(player_dat) );
 SHOpenFolderAndSelectItems ( pidl, 0, nil, 0 );
end;

procedure TLForm.LoadConfig;
var
   fini: TIniFile;
      s: String;
      i: Integer;
begin
 if not FileExists ( app_config ) then exit;

 fini := TIniFile.Create ( app_config );
 try
 finally
  fini.Free;
 end;

end;

procedure TLForm.LoadProfiles;
var
  fl: TFileList;
  fn: String;
   s: String;
   i: Integer;
   n: Integer;

begin
 // TODO: load all profiles

 profiles_dir := ExpandPath ('$profiles$');


 if profiles_dir = 'bad_file_name' then exit; // incompatible fsgame.ltx
 CheckMakeDir (profiles_dir); // directory must be made

 all_profiles.Clear;

 fl := TFileList.Create;
 try
  fl.FindFiles (profiles_dir + '\*', faDirectory);
  for n := 0 to fl.Count - 1 do
   begin
    s := fl [n];
    if ( s = '.' ) or ( s = '..' ) then continue;

    fn := CorrectFilePath ( profiles_dir + '\' + s + '\' +  profile_file );

    if not FileExists (fn) then continue;
    all_profiles.Add (s);
   end;



 finally
  fl.Free;
 end;



 // detecting active profile
 s := ActiveProfileName;

 fsg_profile := s;  // active in fsgame.ltx

 if ( s = '' ) or ( Pos('?', s) > 0 ) then
   begin
    self.Hide;
    if all_profiles.Count = 0 then
       s := ConfigDialog.MakeNewProfile
    else
      begin
       ConfigDialog.sbMain.Panels[0].Text := 'Не выбран профиль игрока в fsgames.ltx!';
       pbConfigClick (self);
      end;
    self.Show;
   end;

 if s <> '' then
    active_profile := s;

 CheckProfile;
end;

{ TTransparentListBox }

procedure TTransparentListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  csrc: TCanvas;
  cx, cy: Integer;
  Data: String;
  Flags: DWORD;
begin
 cx := self.ItemAtPos ( Point(5, 5), TRUE );
 if ( Count > 0 ) and ( cx <> last_up_item ) then
      Invalidate;
 last_up_item := cx;

 if Background = nil then
   begin
    inherited;
    exit;
   end;
 csrc := Background.Canvas;
 Canvas.Brush.Style := bsClear;
 cx := Rect.Right - Rect.Left;
 cy := Rect.Bottom - Rect.Top;

 if (Index >= 0) and (odSelected in State) then
    Canvas.Font.Color := clYellow
 else
    Canvas.Font.Color := Font.Color;



 BitBlt ( Canvas.Handle, Rect.Left, Rect.Top, cx, cy,
          csrc.Handle, Rect.Left + Left, Rect.Top + Top, SRCCOPY );

  Flags := DrawTextBiDiModeFlags(DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  if not UseRightToLeftAlignment then
    Inc(Rect.Left, 2)
  else
    Dec(Rect.Right, 2);

  Data := '';
  if (Style in [lbVirtual, lbVirtualOwnerDraw]) then
    Data := DoGetData(Index)
  else
    Data := Items[Index];
  DrawText(Canvas.Handle, Data, Length(Data), Rect, Flags);
  Canvas.Pen.Color := clAqua;
end;

procedure TTransparentListBox.SetBackground(const Value: TImage);
begin
  FBackground := Value;
  Invalidate;
end;

procedure TTransparentListBox.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
   r: TRect;
   csrc: TCanvas;
   cx, cy: Integer;
begin
 // nothing
 message.Result := 0;
 if Background = nil then exit;
 cx := Width;
 cy := Height;

 SetRect (r, Left, Top, Left + cx - 1, Top + cy - 1);

 csrc := Background.Canvas;
 if ( cx = 0 ) or ( csrc = nil ) then exit;

 BitBlt ( Message.DC, 0, 0, cx, cy,  csrc.Handle, Left, Top, SRCCOPY );
 Message.Result := 1;
end;


end.
