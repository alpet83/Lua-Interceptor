unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, PngImage, Misc, StrClasses, IniFiles;

type
  TISPMainForm = class(TForm)
    imgSource: TImage;
    imgDest: TImage;
    btnLoad: TButton;
    btnSplit: TButton;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSplitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    pimgSource: TPNGImage;
  public
    { Public declarations }
  end;

var
  ISPMainForm: TISPMainForm;

implementation

{$R *.dfm}

procedure TISPMainForm.btnLoadClick(Sender: TObject);
var
   r: TRect;
begin
 try
  pimgSource.LoadFromFile('ui_icon_equipment.png');
  // pimgSource.CreateAlpha;
  r := TRect.Empty;

  imgSource.Width := pimgSource.Width;
  imgSource.Height := pimgSource.Height;
  if pimgSource.Canvas = nil then exit;
  r.Size := imgSource.ClientRect.Size;
  // imgSource.Canvas.Brush.Color := clWhite;
  // imgSource.Canvas.Brush.Style := bsSolid;
  // imgSource.Canvas.FillRect(r);
  imgSource.Picture.Assign(pimgSource);
  imgDest.Left := imgSource.Left + imgSource.Width + 20;
  Width := imgDest.Left + 300;
  //
  // pimg.Free;
 except
  on E: Exception do
    OnExceptLog('Load image', E);
 end;
end;

procedure TISPMainForm.btnSplitClick(Sender: TObject);
var
   pimg: TPNGImage;
    scl: TStringList;
    map: TStrMap;
    cnv: TCanvas;
     sc: String;
      s: String;
      n: Integer;
      i: Integer;
     rd: TRect;
     rs: TRect;

begin
 // pimg := TPNGImage.CreateBlank (COLOR_RGBALPHA, 8, 100, 100);
 pimg := TPNGImage.Create;
 pimg.TransparentColor := clWhite;
 // pimg.Header.Assign (pimgSource.Header);

 // pimg.TransparentColor := pimgSource.TransparentColor;
 scl := TStringList.Create;
 map := TStrMap.Create;

 btnSplit.Enabled := FALSE;
 imgDest.Canvas.Brush.Style := bsClear;
 // imgDest.Transparent := TRUE;
 imgDest.Canvas.Brush.Color := clWhite;

 try

  cnv := pimgSource.Canvas;

  rd.Left := 0;
  rd.Top  := 0;


  scl.LoadFromFile('equipment.ltx');
  for i := 0 to 9 do map.Add('nope');

  for n := 0 to scl.Count - 1 do
   begin
    s := scl[n];
    if Pos('[', s) = 0 then continue;
    sc := Copy(s, 2, Length(s) - 2);
    for i := 0 to 4 do
        map[i] := scl[n + i + 1];

    rs.Left := map.IntValues['icon_x'];
    rs.Top  := map.IntValues['icon_y'];
    rs.Width  := map.IntValues['icon_w'];
    rs.Height := map.IntValues['icon_h'];

    rd.Width := rs.Width;
    rd.Height := rs.Height;

    if (rs.Width > 0) and (rs.Height > 0) then
     begin
      if rs.Width <> 100 then
          asm nop end;
      imgDest.Width := rs.Width;
      imgDest.Height := rs.Height;
      imgDest.Picture.Bitmap.SetSize(rs.Width, rs.Height);
      imgDest.Canvas.FillRect(rd);
      imgDest.Picture.Graphic.Transparent := TRUE;
      imgDest.Canvas.Draw(-rs.Left, -rs.Top, imgSource.Picture.Graphic); //  CopyRect(rd, cnv, rs);
      // imgDest.Canvas.DrawFocusRect();
      pimg.Assign(imgDest.Picture.Bitmap);

      {
      pimg.Canvas.Brush.Style := bsSolid;
      pimg.Canvas.Brush.Color := clWhite;
      pimg.Canvas.FillRect(rd);

      pimg.Canvas.CopyRect(rd, cnv, rs);}

      pimg.CompressionLevel := 7;
      pimg.TransparentColor := clWhite;


      pimg.SaveToFile( 'out\' + sc + '.png' );
     end;

   end;



 finally
  btnSplit.Enabled := TRUE;
  pimg.Free;
  map.Free;
  scl.Free;
 end;
end;

procedure TISPMainForm.FormCreate(Sender: TObject);
begin
 pimgSource := TPNGImage.Create;
end;

end.
