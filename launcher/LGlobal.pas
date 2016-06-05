unit LGlobal;

interface
uses Windows, SysUtils, Classes, StrUtils, StrClasses, Misc, LCGlobals;

{ Глобальные определения исключительно для ланчера.

}

{$I stkdef.inc}
var
   hLib: THandle;
   pp: TProgramParams;

   ready_event: THandle = 0;
   no_launcher: Boolean;


   // luacap procedures
            _Init: procedure (ret_ctx: PContext); stdcall = nil;
          _ReInit: procedure; stdcall = nil;
  _ExecInitScript: procedure; stdcall = nil;
        _LocalODS: procedure (msg: PWideChar; print_flags: DWORD = 255); stdcall = nil;
     _ShowConsole: function (sw_code: Integer): THandle; stdcall = nil;
BeginProcessDebug: function (pid: DWORD): Boolean; stdcall = nil;
  EndProcessDebug: function  : Boolean; stdcall = nil;



      debug_mode: Boolean = FALSE;
       pause_run: Boolean = FALSE;
       load_save: Boolean = FALSE;
         load_sv: String = '';
   last_savegame: String = '';
    last_sv_date: TDateTime = 0;

      target_pid: DWORD = 0;
      target_exe: String;
        work_dir: String;

      app_config: String;
       saves_dir: String = '';
         go_exit: Boolean = TRUE;
     xray_params: String = '';
        show_con: Boolean = TRUE;
   sv_autobackup: Boolean = FALSE;
    service_mode: Boolean = TRUE;

      player_dat: String;        // full path-filename to player.dat
    profiles_dir: String;
    all_profiles: TStrMap = nil; // gamer profiles list



procedure AttachInterceptor;
procedure ODS (const msg: WideString);
procedure PostInit;


implementation

procedure ODS (const msg: WideString);
begin
 if Assigned (_LocalODS) then
    _LocalODS ( PWideChar(msg));
end;


procedure AttachInterceptor;
var
   s: String;
begin
 SetLastError (0);
 s := ExePath + LUAICP_DLL;
 Assert ( FileExists (s), 'cannot find ' + s);
 hLib := LoadLibrary ( PChar (s) );
 Assert (hLib <> 0, ' cannot load ' + s );


 _Init := GetProcAddress (hLib, 'Init');
 _ReInit := GetProcAddress (hLib, 'ReInit');
 _ExecInitScript := GetProcAddress (hLib, 'ExecInitScript');
 _LocalODS := GetProcAddress (hLib, 'LocalODS');
 _ShowConsole := GetProcAddress (hLib, 'ShowConsole');

 FSExpandPath       := GetProcAddress ( hLib, 'FSExpandPath' );
 BeginProcessDebug  := GetProcAddress ( hLib, 'BeginProcessDebug' );
 EndProcessDebug    := GetProcAddress ( hLib, 'EndProcessDebug' );
end;

procedure PostInit;
var
   s: String;
begin
 try


  if show_con then
    begin
     // ShowConsole (SW_SHOW);
     _ShowConsole (SW_SHOWNOACTIVATE);
     SetConsoleTitle ( PChar ('Log console - ' + ExeFileName) );
     {AssignFile (Input, '');
     Reset (Input);}
    end
  else
     _ShowConsole (SW_HIDE);





  if Assigned (_Init) then
     try
      _Init(nil);
      ODS_Proc := _LocalODS;
      ODS('[~T]. #DBG: Этот релиз ' + ExtractFileName(ExeFileName) +
                     ' приготовлен для версии игры S.T.A.L.K.E.R. '
                 {$IFDEF SOC}
                     {$IFDEF SOC}
                      {$IFDEF NEWEST_BUILD} + '"Тени Чернобыля 1.0007+" ' {$ELSE}+ '"Тени Чернобыля 1.0004+" ' {$ENDIF}
                     {$ENDIF}
                 {$ENDIF}
                 {$IFDEF CSKY}
                   + '"Чистое небо" '
                 {$ENDIF}
                 {$IFDEF SCop}
                   + '"Зов Припяти" '
                 {$ENDIF});
      // ReadLn;
     except
      on E: Exception do
         _SafeLog ('Exception catched in #1 ' + E.Message);
     end
   else
     begin
      CBeep (500, 2000);
      ExitProcess ($33FF);
     end;

   if ( ParamCount = 0 ) and ( target_exe = '' ) and ( target_pid = 0 ) then
      begin
       WriteLn ('xray_params: [-service] [-last_save] -e <exe_file> of sample process. This window will closed in 5 seconds...');
       XSleep (5000);
       exit;
      end;

  saves_dir := AddSlash ( ExpandPath ('$game_saves$') );

  ODS('fsgame.$game_saves$ = ' + saves_dir);

 except
  on E: Exception do
     _SafeLog ('Exception catched in #0 ' + E.Message);
 end;

 if Assigned (_ExecInitScript) then
              _ExecInitScript;
end;

initialization
 app_config := ExtractFileName( ExeFileName );
 app_config := AnsiReplaceStr( app_config, '.exe', '.conf' );
 app_config := FindConfigFile( app_config );

 all_profiles := TStrMap.Create;
finalization
 FreeAndNil ( all_profiles );

end.
