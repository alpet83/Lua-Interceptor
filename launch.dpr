program launch;

{$R *.dres}

uses
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Zlib,
  MD5,
  Forms,
  Dialogs,
  Controls,
  Windows,
  SysUtils,
  StrUtils,
  Classes,
  StrClasses,
  ImageHlp,
  TlHelp32,
  ModuleMgr,
  Misc,
  IniFiles,
  Math,
  NLCPack in 'NLCPack.pas',
  PicButton in '..\lib\components\PicButton.pas',
  LCGlobals in 'LCGlobals.pas',
  Patcher in 'Patcher.pas',
  CountBack in 'launcher\CountBack.pas' {BreakoutForm},
  Launcher in 'launcher\Launcher.pas' {LForm},
  GameConfig in 'launcher\GameConfig.pas' {ConfigDialog},
  WThreads in '..\lib\WThreads.pas',
  GamerProfile in 'launcher\GamerProfile.pas' {NewProfileDialog},
  LGlobal in 'launcher\LGlobal.pas',
  ProofCheck in 'launcher\ProofCheck.pas',
  EasyECDSA in '..\lib\EasyECDSA.pas',
  LData in 'launcher\LData.pas';

{-$APPTYPE CONSOLE}

{$A4}
{$R *.res}
{$SetPEFlags IMAGE_FILE_LARGE_ADDRESS_AWARE}

function IsLauncher: Boolean;
begin
 result := TRUE;
 exit;
 asm
  ret
  dd $FFAADDEE
  dd offset Data_1
  dd 0, 0, 0, 0
 end;
end;

procedure PreInit;
var
   dbg: TStrings;
begin
 dbg := TStringList.Create;
 dbg.Add('PID=' + IntToStr(GetCurrentProcessId));
 dbg.Add('START=' + FormatDateTime('dd-mm-yy hh:nn:ss.zzz', Now));
 dbg.SaveToFile('..\launcher.pid');
 dbg.Free;
end;

exports
      IsLauncher;


var
      s: String;
   fini: TIniFile;
    app: TObject;
     mm: TMemoryManagerEx;
     pa: PPointer;

begin
 GetMemoryManager(mm);
 {$IFOPT D+}
 PreInit;
 {$ENDIF}
 try
  if not IsLauncher then exit;
 except
  on E: Exception do
     OnExceptLog('init', E, TRUE);
 end;





 hLib := 0;
 IsMultithread := TRUE;

 SetLastError(0);
 ready_event := CreateEvent (nil, TRUE, FALSE, 'Global\XRAY_LAUNCHER');
 if GetLastError = ERROR_ALREADY_EXISTS then
  begin
   MessageBox (0, PChar ('Xray Launcher already runed! Kill ' + ExtractFileName (ExeFileName) + ' from taskmanager.'),
                  'Already runed!', MB_OK or MB_ICONERROR);
   exit;
  end;


 ShowConsole(SW_HIDE);
 StartLogging ('');
 EnableVEH(TRUE);
 wprintf('[~T]. #DBG: Console CP = %d ', [GetConsoleOutputCP]);

 if GetACP <> 1251 then
    PrintError( Format('System code page is %d, russian messages will be present as ????', [GetACP]) );


 Launcher.InitModule;
 EasyECDSA.AddGlobals(nil, FALSE);

 s := AnsiReplaceStr (ExeFileName, '.exe', '.conf');

 fini := TIniFile.Create ( FindConfigFile (s) );
 try
  target_exe := fini.ReadString('config', 'AttachTo', '');
  target_exe := Trim ( AnsiReplaceStr (target_exe, '$ExePath$', ExePath) );          // templating path
  target_exe := AnsiReplaceStr (target_exe, '\\', '\');


  xray_params := fini.ReadString('config', 'xray_params', '');

  work_dir    := fini.ReadString('config', 'WorkDirectory', GetCurrentDir);
  no_launcher := fini.ReadBool  ('config', 'NoLauncher', FALSE);


  form_caption := fini.ReadString ('config', 'FormCaption', DEF_FORM_CAPTION);
  form_caption := UnhideSP (form_caption);
  pause_run := fini.ReadBool ('config', 'PauseRun', False);


  work_dir := Trim ( AnsiReplaceStr (work_dir, '$ExePath$', ExePath) );
  work_dir := CorrectFilePath ( work_dir );


  show_con :=       fini.ReadBool ('config', 'ShowConsole', FALSE);
  sv_hide_suffix := fini.ReadString ('config', 'SVHideSuffix', DEF_HIDE_SUFFIX);
  sv_hide_ext  := fini.ReadString ('config', 'SVHideExt', DEF_SAVE_EXT);
  sv_scan_mask := fini.ReadString ('config', 'SVScanMask', '*' + sv_hide_suffix + sv_hide_ext);
  max_show_saves := fini.ReadInteger ('config', 'MaxShowSaves', 15);
  svbox_font_nm := fini.ReadString  ('config', 'SVLFontName', 'Arial Cyr');
  svbox_font_nm := UnhideSP (svbox_font_nm);
  svbox_font_sz := fini.ReadInteger ('config', 'SVLFontSize', 10);
  svbox_org_x := fini.ReadInteger('config', 'SVL.Left', 10);
  btns_org_x :=  fini.ReadInteger('config', 'btns.Left', 265);
  btns_org_y :=  fini.ReadInteger('config', 'btns.Top', 40);
  btns_width  := fini.ReadInteger('config', 'btns.Width', 208);
  btns_height := fini.ReadInteger('config', 'btns.Height', 44);
  btns_offset := fini.ReadInteger('config', 'btns.Offset', 44);

  form_on_top := fini.ReadBool ('config', 'FormOnTop', TRUE);


  GameProcessAffinity := fini.ReadInteger('debug', 'AffinityMask', 0);

  if no_launcher then
     ODS('[~T]. #DBG: Запуск ланчера отключен через настройки...');

 finally
  fini.Free;
 end;

 madExcept.NameThread( GetCurrentThreadId, 'Launcher.Main');
 saves_dir := '';

 if ParamCount > 0 then
    begin
     pp := TProgramParams.Create;
     if pp.values ['e'] <> '' then
        target_exe := pp.Values['e'];
     load_save :=      ( pp.IntValues ['last_save'] <> 0 );
     sv_autobackup :=  ( pp.IntValues ['sv_autobackup'] <> 0 );
     debug_mode :=     ( pp.IntValues ['debugger'] <> 0 );
     target_pid :=     ( pp.IntValues ['attach_to'] );
     service_mode :=   ( pp.IntValues ['immexit'] = 0 ) or ( pp.IntValues ['service'] <> 0 ) or ( target_pid > 0 );

     {$IFOPT D+}
     s := pp.Values['nlcunpack'];
     if ( s <> '' ) and FileExists ( s ) then
        begin
         ODS('[~T]. #DBG: Trying unpack ' + s);
         NLCUnpack ( s, ExePath );
         exit;
        end;
     {$ENDIF}
     FreeAndNil (pp);
    end;

 if service_mode then
    ODS('[~T]. #DBG: service mode activated.');

 Data_1;
 pa := @IsLauncher;
 Inc ( NativeUint(pa), $0D );
 // wprintf('Data_1 at $%p, ref = $%p/$%p ', [@Data_1, pa, pa^]);

 if no_launcher or ( target_pid > 0 ) then
   begin
    PostInit;
    go_exit := FALSE;
   end
 else
  try
   Application.Initialize;
   Application.MainFormOnTaskbar := True;
   Application.Title := 'Xray launcher';
   Application.CreateForm(TLForm, LForm);
   Application.CreateForm(TConfigDialog, ConfigDialog);
   Application.CreateForm(TNewProfileDialog, NewProfileDialog);
   Application.Run;
 except
   on E: Exception do
    OnExceptLog('Launcher run', E);
  end;


 // ===============================================
 // Init;
 if target_pid > 0 then  AttachTarget;

 // ================== Обработка запуска ==================== //




 try
  ODS('[~T]. #DBG: Finalization begins...');
  XSleep(20);
  FreeLibrary (hLib);
  XSleep(50);
  SetMemoryManager(mm);
  ODS('[~T]. #DBG: DLL unloaded, relax... ');
  // FinalizeModule('~ALL');

 except
  on E: Exception do
     onExceptLog('Exception while DLL unloading', E);
 end;


 {
 if Assigned(Application) then
   try
     Application.Destroy;
     Application := nil;
     ODS('[~T]. #DBG: Application object destroyed... ');
   except
    on E: Exception do
      begin
       Application := nil;
       E.Free;
      end;
   end;
  }
 ModuleMgr.FinalizeModule('~ALL');
 XSleep(100);
 TerminateProcess ( GetCurrentProcess, 0 );
end.