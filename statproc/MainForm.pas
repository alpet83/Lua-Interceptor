unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Vcl.StdCtrls,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, ExStringGrid, Misc, WThreads, StrClasses, ContNrs, Vcl.Samples.Gauges,
  StrUtils;

type
  TMForm = class(TForm)
    sgFiles: TStringGridEx;
    btnAnalyse: TButton;
    Label1: TLabel;
    edtSrcPath: TEdit;
    edtVolume: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    ggProgress: TGauge;
    btnExit: TButton;
    OpenDialog1: TOpenDialog;
    lbxRoot: TListBox;
    Label4: TLabel;
    btnAddRoot: TButton;
    procedure btnAnalyseClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure edtSrcPathDblClick(Sender: TObject);
    procedure btnAddRootClick(Sender: TObject);
  private
    function ExtractRoot(const fn: String): String;
    { Private declarations }

  public
    { Public declarations }
  end;

var
  MForm: TMForm;

implementation

{$R *.dfm}

type
   TFileLoadStat = class

   public
     r_name: String;
    n_loads: Integer;
     n_stat: Integer;
     f_size: UInt64;
     weight: Single;


   end; // TFileLoadStat


function   GetFileSizeByName (const sFileName: String): Int64;
var
   hFile: THandle;
      li: LARGE_INTEGER;

begin
 hFile := CreateFile ( PChar (sFileName), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0 );

 li.QuadPart := 0;

 if hFile <> INVALID_HANDLE_VALUE then
  begin
   li.LowPart :=  GetFileSize(hFile, @li.HighPart);
   CloseHandle (hFile);
  end;

 result := li.QuadPart;

end;


function cmpFileWeight (a, b: Pointer): Integer;
var
   aw, bw: Single;
begin
 aw := TFileLoadStat (a).weight;
 bw := TFileLoadStat (b).weight;
 result := 0;

 if aw > bw then result := -1;
 if aw < bw then result := +1;

end;


function TMForm.ExtractRoot(const fn: String): String;
var
   n: Integer;
begin
 result := '';
 n := PosLowerCase ('\gamedata\', fn);
 if n = 0 then exit;
 result := Copy (fn, 1, n + 8);

 with lbxRoot.Items do
   if ( result <> '' ) and ( IndexOf(result) < 0 ) then Add (result);


end;


procedure TMForm.btnAnalyseClick(Sender: TObject);
var
     slrows: TStrMap;
     slcols: TStrMap;
     slstat: TStrMap;
     sl_all: TStrMap;
      flist: TFileList;
   src_path: String;
   root, rr: String;
     n_file: Integer;
      n_row: Integer;
      r_pos: Single;
      n_col: Integer;
      fname: String;
      rname: String;
     weight: Single;
     volume: Int64;
      stobj: TFileLoadStat;
      total: Int64;
      fixed: Int64;

       srtl: TObjectList;
         sr: TStrings;
          i: Integer;

begin

 btnExit.Enabled := FALSE;
 btnAnalyse.Enabled := FALSE;
 volume := atoi ( edtVolume.Text ) * 1048576;

 srtl := TObjectList.Create (FALSE);
 flist := TFileList.Create(self);
 slrows := TStrMap.Create(self);
 slcols := TStrMap.Create(self);
 slstat := TStrMap.Create(self);
 sl_all := TStrMap.Create(self);


 rr := '';

 with lbxRoot do
  if ItemIndex >= 0 then rr := Items [ItemIndex];

 slstat.Sorted := TRUE;
 slstat.OwnsObjects := TRUE;
 try
  src_path := edtSrcPath.Text;
  if src_path = '' then
     src_path := ExePath;

  src_path := CorrectFilePath(src_path);
  src_path := AddSlash (src_path);



  flist.FindFiles ( src_path + '*.csv' );

  ggProgress.MinValue := 0;
  ggProgress.MaxValue := 1000;




  for n_file := 0 to flist.Count - 1 do
   begin
    slrows.LoadFromFile( src_path + flist [n_file] );

    root := '';
    n_col := 0;

    for n_row := 0 to slrows.Count - 1 do
     begin

      r_pos := (n_row / slrows.Count);

      weight := 1.0 + ( (1 - r_pos) / 1000);

      slcols.Split(',', slrows[n_row]);

      if n_row = 0 then
       begin
        n_col := slcols.IndexOf('Path');
        if n_col <= 0 then n_col := 0;
        continue;
       end;


      if slcols.Count < 7 then continue;

      fname := slcols [n_col];

      fname := AnsiReplaceStr(fname, '\\', '\');


      if root = '' then root := ExtractRoot (fname);

      rname := fname;

      if PosLowerCase(root, rname) = 1 then
         Delete (rname, 1, Length(root));

      // rname := Trim(rname);


      stobj := TFileLoadStat ( slstat.FindObject( LowerCase(rname) ) );
      if stobj = nil then
        begin
         stobj := TFileLoadStat.Create;
         stobj.n_loads := 0;
         stobj.weight := weight;
         if rr = '' then rr := root;

         stobj.f_size := GetFileSizeByName ( rr + rname);
         stobj.r_name := rname;
         stobj.n_stat := n_file;

         srtl.Add (stobj);
         slstat.AddObject( LowerCase(rname), stobj);
        end
      else
      if n_file > stobj.n_stat then
        begin
         stobj.n_stat := n_file;
         stobj.weight := stobj.weight + weight  + 0.0001;
         Inc ( stobj.n_loads );
        end;


      if n_row and 255 = 0 then
        begin
         Label1.Caption := Format ( '{%s} processed %d rows from %d', [flist [n_file], n_row, slrows.Count]);

         ggProgress.Progress := Round ( 1000 * n_row / slrows.Count );

         Application.ProcessMessages;
        end;


     end; // for n_row



   end; // for n_file

  srtl.Sort(cmpFileWeight);
  sgFiles.RowCount := srtl.Count + 2;

  sl_all.LoadFromFile('all_files.lst');


  // rest generation
  root := '';

  for n_row := 0 to sl_all.Count - 1 do
   begin
    fname := sl_all [n_row];
    rname := fname;


    if root = '' then root := ExtractRoot (fname);






    if PosLowerCase(root, rname) = 1 then
           Delete (rname, 1, Length(root));

    stobj := TFileLoadStat ( slstat.FindObject( LowerCase(rname) ) );
    if stobj = nil then
        begin
         stobj := TFileLoadStat.Create;
         stobj.n_loads := 0;
         stobj.weight := 0;
         stobj.f_size := GetFileSizeByName (fname);
         stobj.r_name := rname;
         stobj.n_stat := 0;

         srtl.Add (stobj);
         slstat.AddObject( LowerCase(rname), stobj);
        end;

    if n_row and 255 = 0 then
     begin
      Label1.Caption := Format ('{all_files.lst} processed %d rows from %d', [n_row, slrows.Count]);

      ggProgress.Progress := Round ( 1000 * n_row / sl_all.Count );

      Application.ProcessMessages;
     end;

   end;



  total := 0;
  fixed := 0;

  n_file := 0;

  if lbxRoot.Items.Count > 0 then
   begin
    i := lbxRoot.ItemIndex;
    if i > 0 then
       root := lbxRoot.Items [i]
    else
       root := lbxRoot.Items [0];
   end;

  slrows.Clear;

  for n_row := 0 to srtl.Count - 1 do
   begin
    stobj := TFileLoadStat (srtl [n_row]);
    sr := sgFiles.Rows [n_row  + 1];
    sr [0] := ftow ( stobj.weight, '%.7f' );
    sr [1] := stobj.r_name;
    sr [2] := ftow ( stobj.f_size / MIB_BYTES, '%.3f MiB' );
    Inc ( total, stobj.f_size );

    slrows.Add ( root + stobj.r_name );

    if total > fixed + volume then
     begin
      fixed := total;
      slrows.SaveToFile('optimal' + IntToStr(n_file) + '.lst');
      slrows.Clear;
      Inc (n_file);
     end;

   end;





  Label1.Caption := ftow ( total / MIB_BYTES, '%.3f MiB total' );

  slrows.SaveToFile('optimal' + IntToStr(n_file) + '.lst');

  ggProgress.Progress := ggProgress.MaxValue;

 finally
  flist.Free;
  slrows.Free;
  slcols.Free;
  slstat.Free;
  sl_all.Free;
  srtl.Free;
  btnExit.Enabled := TRUE;
  btnAnalyse.Enabled := TRUE;
 end;


end;

procedure TMForm.btnExitClick(Sender: TObject);
begin
 Close;
end;

procedure TMForm.edtSrcPathDblClick(Sender: TObject);
begin

 if not OpenDialog1.Execute(Handle) then exit;

 edtSrcPath.Text := ExtractFilePath ( OpenDialog1.FileName );

end;

procedure TMForm.FormCreate(Sender: TObject);
var
   r: TStrings;
begin
 edtSrcPath.Text := ExePath;
 sgFiles.DefaultAttr.font_attr.size := 7;
 r := sgFiles.Rows[0];
 r [0] := 'Weight';
 r [1] := 'FileName';
 r [2] := 'FileSize';
 OpenDialog1.InitialDir := ExePath;
 OpenDialog1.Title := 'Select any CSV file';
end;

procedure TMForm.btnAddRootClick(Sender: TObject);
var
   sr: String;
begin
 sr := InputBox('Adding ROOT', 'Put full path to "gamedata\"', 'C:\GAMES\STK\gamedata');
 sr := Trim(sr);
 sr := AddSlash (sr);
 if (sr = '') or ( not DirectoryExists (sr) ) then
   begin
    Application.MessageBox( PChar('Directory not exists ' + sr) , 'Error', MB_OK or MB_ICONERROR);
    exit;
   end;

 with lbxRoot do
  begin
   if Items.IndexOf(sr) < 0 then
      ItemIndex := Items.Add(sr);
  end;

end;


end.
