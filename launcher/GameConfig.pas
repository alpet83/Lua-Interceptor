unit GameConfig;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Patcher, Vcl.StdCtrls, Vcl.CheckLst, Misc, StrClasses, IniFiles, StrUtils, Vcl.ComCtrls, LGlobal;

{$I stkdef.inc}
type
  TConfigDialog = class(TForm)
    lbPatches: TLabel;
    clbPatches: TCheckListBox;
    cbxResolution: TComboBox;
    Label2: TLabel;
    btnSave: TButton;
    btnCancel: TButton;
    chxFullScreen: TCheckBox;
    sbMain: TStatusBar;
    gbProfiles: TGroupBox;
    lbxProfiles: TListBox;
    btnAddProfile: TButton;
    btnDelProfile: TButton;
    procedure FormCreate(Sender: TObject);
    procedure clbPatchesClickCheck(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddProfileClick(Sender: TObject);
    procedure btnDelProfileClick(Sender: TObject);
    procedure lbxProfilesClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }



       user_ltx: String;
      data_root: String;

    procedure  FillResolutions;
    function   LoadUserLTX (upd_ui: Boolean): TStrMap;
    procedure  SaveUserLTX;
    procedure  UpdatePatches;
    procedure  UpdChecked(n: Integer);
  public
    { Public declarations }


    { methods }

    function    GetPatcher (n: Integer): TPatcher; inline;
    procedure   LoadPatchers;

    procedure   DeleteProfile ( const name: String );
    function    MakeNewProfile: String;
  end;

var
  ConfigDialog: TConfigDialog;

implementation
uses Launcher, LCGlobals, GamerProfile;

{$R *.dfm}

procedure TConfigDialog.FormCreate(Sender: TObject);
begin
 FillResolutions;
 data_root := ExpandPath ( '$app_data_root$' );

 user_ltx := AddSlash (data_root) + 'user.ltx';

 if FileExists (user_ltx) then
    LoadUserLTX (TRUE).Free
 else
   cbxResolution.Enabled := FALSE;

 {$IFDEF NEWEST_BUILD}
 lbPatches.Hide;
 clbPatches.Hide;
 gbProfiles.Width := Width - 20;
 {$ELSE}
 clbPatches.AddItem ( 'Поддержка 3Gb', T3GbPatcher.Create ( ExePath + XR_EXE ) );
 {$ENDIF}

 LoadPatchers;
 UpdatePatches;
end;

procedure TConfigDialog.FormDestroy(Sender: TObject);
var
   sl: TStrings;
begin
 sl := clbPatches.Items;
 StrClasses.FreeListObjects (sl);
end;

procedure TConfigDialog.FormShow(Sender: TObject);
begin
 btnSave.Enabled := ( lbxProfiles.ItemIndex >= 0 );
 btnCancel.Enabled := ( LForm.active_profile <> '' );
end;

function TConfigDialog.GetPatcher(n: Integer): TPatcher;
begin
 result := TPatcher ( clbPatches.Items.Objects [n] );
end;

procedure TConfigDialog.lbxProfilesClick(Sender: TObject);
begin
 btnSave.Enabled := TRUE;
end;

function TConfigDialog.LoadUserLTX (upd_ui: Boolean): TStrMap;
var
   lines: TStrMap;
       s: String;
       n: Integer;
begin
 AllowReadWrite (user_ltx);
 lines := TStrMap.Create (self);
 lines.LoadFromFile (user_ltx);

 result := lines;

 if not upd_ui then exit;


 n := lines.FindSub( 'vid_mode' );
 if n >= 0 then
   begin
    s := lines [n];
    Delete (s, 1, 8);
    s := Trim (s);

    with cbxResolution do
         ItemIndex := Items.IndexOf (s);
   end;

 n := lines.FindSub( 'fullscreen' );

 if n >= 0 then
   begin
    s := lines [n];
    chxFullscreen.Checked := ( Pos (' on', s) > 0 );
   end;

end;

procedure TConfigDialog.LoadPatchers;
var
{$IFNDEF NEWEST_BUILD}
   fini: TIniFile;

   list: TStrMap;
   sect: String;
   pobj: TPatcher;
    tgt: String;
      t: String;
      n: Integer;
{$ENDIF}
   conf: String;

begin

 conf := app_config;

 if not FileExists (conf) then exit;
 {$IFNDEF NEWEST_BUILD}

 list := TStrMap.Create;
 fini := TIniFile.Create (conf);

 list.CommaText := fini.ReadString( 'config', 'Patches', '' );

 for n := 0 to list.Count - 1 do
  begin
   sect := list [n];
   if sect = '' then break;

   t := fini.ReadString( sect, 'Class', '?' );
   t := LowerCase (t);
   pobj := nil;


   tgt := fini.ReadString( sect, 'Target', '' );

   if t = 'diff' then
      pobj := TDiffPatcher.Create( tgt );

   if pobj = nil then continue;

   pobj.Caption := fini.ReadString( sect, 'Caption', 'Noname for ' + tgt );
   pobj.LoadConfig( fini, sect );

   clbPatches.AddItem ( pobj.Caption, pobj );
  end;


 fini.Free;
 list.Free;
 {$ENDIF}

end;

function TConfigDialog.MakeNewProfile: String;
var
   sl: TStrings;
    i: Integer;
begin
 result := '';

 if NewProfileDialog.ShowModal <> mrOK then exit;

 result := NewProfileDialog.edtName.Text;

 // registering in lists
 i := all_profiles.IndexOf (result);
 if i < 0 then
    i := all_profiles.Add (result);

 all_profiles.Sort;

 sl := lbxProfiles.Items;
 sl.Assign (all_profiles);
 lbxProfiles.ItemIndex := sl.IndexOf (result);
end;

procedure TConfigDialog.SaveUserLTX;
var
   lines: TStrMap;
       n: Integer;
begin
 lines := LoadUserLTX (FALSE);

 n := lines.FindSub( 'vid_mode' );
 if n >= 0 then
    lines [n] := 'vid_mode ' + cbxResolution.Text;

 n := lines.FindSub( 'fullscreen' );
 if n >= 0 then
    lines [n] := 'rs_fullscreen ' + IfV(chxFullscreen.Checked, 'on', 'off');


 lines.SaveToFile (user_ltx);
 lines.Free;
end;

procedure TConfigDialog.UpdatePatches;
var
   n: Integer;

begin
 for n := 0 to clbPatches.Count - 1 do
     UpdChecked (n);
end;


procedure TConfigDialog.UpdChecked (n: Integer);
var
   p: TPatcher;
   s: String;
begin
 p := GetPatcher (n);
 if p = nil then exit;
 s := p.Patch ('');
 clbPatches.Checked [n] := ( s = 'patched' );

 if Pos( 'patched', s ) = 0 then
   begin
    s := ' patcher {' + p.Caption + '}: ' + p.LastError;
    PrintError ( s );
    sbMain.Panels[0].Text := s;
   end;
end;

procedure TConfigDialog.btnSaveClick(Sender: TObject);
begin
 SaveUserLTX;
 sbMain.Panels[0].Text := 'Настройки сохранены.';

 with lbxProfiles do
  if ItemIndex >= 0 then
     LForm.active_profile := Items [ItemIndex];
 modalResult := mrOk;
end;

procedure TConfigDialog.btnAddProfileClick(Sender: TObject);
begin
 MakeNewProfile;
end;

procedure TConfigDialog.btnCancelClick(Sender: TObject);
begin
 modalResult := mrCancel;
end;

procedure TConfigDialog.btnDelProfileClick(Sender: TObject);
var
   s: String;
begin
 if lbxProfiles.ItemIndex < 0 then exit;

 with lbxProfiles do
   s := Items [ItemIndex];

 if Application.MessageBox (  PChar('Вы точно хотите удалить профиль ' + s), 'Подтверждение', MB_YESNO ) <> IDYES then exit;

 DeleteProfile (s);
end;

procedure TConfigDialog.clbPatchesClickCheck(Sender: TObject);
var
   a: String;
   r: String;
   p: TPatcher;
   i: Integer;
begin
 i := clbPatches.ItemIndex;

 if i < 0 then exit;

 p := GetPatcher (i);

 a := IfV ( clbPatches.Checked [i], 'patch', 'unpatch' );

 r := p.Patch (a);

 clbPatches.Checked [i] := ( r = 'patched' );

 a := 'patcher {' + p.Caption + '}: ';
 if r = ERR_FAIL then
    sbMain.Panels[0].Text := a + p.LastError
 else
    sbMain.Panels[0].Text := a + r;
end;

procedure TConfigDialog.DeleteProfile(const name: String);
var
   sl: TStrings;
    i: Integer;
begin
 sl := lbxProfiles.Items;
 i := sl.IndexOf (name);
 if i < 0 then exit;
 sl.Delete (i);
 // TODO: remove profile files
end;

procedure TConfigDialog.FillResolutions;
const
   MAX_MODES = 16384;

var
   modes: array of DEVMODE;
  cnt, n: Integer;
       s: String;
begin
 //
 cbxResolution.Clear;

 SetLength (modes, MAX_MODES);
 cnt := 0;
 FillChar ( modes[0], sizeof (modes), 0 );

 while ( cnt < MAX_MODES ) do
   begin
    modes [cnt].dmSize := sizeof ( DEVMODE );
    if not EnumDisplaySettings ( nil, cnt, modes [cnt] ) then break;
    Inc (cnt);
   end;

 for n := 0 to cnt - 1 do
   with modes [n], cbxResolution do
   if dmBitsPerPel > 16 then
    begin
     s := IntToStr ( dmPelsWidth ) + 'x' + IntToStr ( dmPelsHeight );

     if Items.IndexOf (s) < 0 then Items.Add (s);
    end;

 SetLength (modes, 0);
end; // FillResolutions

end.
