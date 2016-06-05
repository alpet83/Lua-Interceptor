unit LCDebug;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, Math,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, WThreads, LCGlobals, VxTools, Misc;

type
  TDMThread = class (TWorkerThread)
  protected
   procedure  ProcessInit; override;

  public

  end;


  TDebugMonitor = class(TForm)
    pgctl: TPageControl;
    tsMap: TTabSheet;
    imgMap: TImage;
    btnDrawMap: TButton;
    updTimer: TTimer;
    edtTest: TEdit;
    chxVertexDump: TCheckBox;
    lbInfo: TLabel;
    procedure updTimerTimer(Sender: TObject);
    procedure btnDrawMapClick(Sender: TObject);
    procedure imgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
        br, rr: TRect;
    last_found: Integer;

  public
    { Public declarations }

    function  x_ofs: Integer; inline;
    function  y_ofs: Integer; inline;

    function   xzpt ( x, z: Single ): TPoint;
    function   v3pt ( pt: PVector3F ): TPoint;

  end;

var
  DebugMonitor: TDebugMonitor;
     gDMThread: TDMThread;

implementation

{$R *.dfm}

{ TDMThread }

procedure TDMThread.ProcessInit;
begin
 inherited;
 gDMThread := self;
 DebugMonitor := TDebugMonitor.Create (nil);
 DebugMonitor.ShowModal;

end;

function AngleLimit (v: Single): Single;
begin
 if v < 0   then v := 0;
 if v > 180 then v := 180;
 result := v;
end;

function Gradient ( zone: Byte; value, range: Single ): TColor;

var
   rc, bc, gc: Single;

begin
 if value < 0 then
  asm
   nop
  end;

 // value always ++ from 0 to 255

 value := value * 255 / range;
 rc := 0;
 bc := 0;
 gc := 0;

 case zone of
  0: bc := value;                      // blue  fade in   (->blue)
  1: begin bc := 255; gc := 000 + value; end; // green fade in   (->aqua)
  2: begin gc := 255; bc := 255 - value; end; // blue  fade out  (->green)
  3: begin gc := 255; rc := 000 + value; end; // red   fade in   (->yellow)
  4: begin rc := 255; gc := 255 - value; end; // green fade out  (->red)
  5: begin rc := 255 - value; end;            // red   fade out  (->black)
  7: begin rc := value; bc := value; gc := value; end;
  8: begin
      rc := 255 - value;
      bc := 255 - value;
    end;

 end;

 result := RGB ( Trunc(rc), Trunc(gc), Trunc(bc) );
end;


procedure TDebugMonitor.btnDrawMapClick(Sender: TObject);


var
  cnv: TCanvas;
  pms: PVxMapSector;
  vxl: PLevelVertexList;
  pvx: PLevelVertex;
  ext: Integer;
   cl: TColor;
   fy: Single;
   rg: Integer;
    y: Integer;

    p: TPoint;
    r: TRect;
    n: Integer;
    s: String;
begin
 if current_lvl = nil then exit;

 if ( vertex_map = nil ) then
      vertex_map := TVertexMap.Create;

 if current_lvl <> vertex_map.VxList then
      vertex_map.MakeSectors ( current_lvl );


 br := vertex_map.BoundsRect;

 ext := vertex_map.Step * 2 + 10;

 SetRect (rr, 0, 0, br.Width + ext, br.Height + ext );

 imgMap.Picture.Bitmap.Width := rr.Width;
 imgMap.Picture.Bitmap.Height := rr.Height;
 cnv := imgMap.Canvas;
 cnv.Brush.Color := clBlack;
 cnv.Brush.Style := bsSolid;
 cnv.Pen.Color := clWhite;

 cnv.Rectangle ( rr );

 // cnv.Brush.Style := bsClear;

 cnv.Font.Name := 'Small Fonts';
 cnv.Font.Size := 6;
 cnv.Font.Color := clBlue;

 for n := 0 to vertex_map.SectorsCount - 1 do
  begin
   pms := vertex_map.SectorsList [n];
   r := pms.bounds_rect;
   if ( r.Width <= 1 ) or ( r.Height <= 1 ) then continue;



   if pms.index_count > 0 then
     begin
      cnv.Pen.Color := IfV ( pms.sec_flags and SCF_SCANNED <> 0, clLime, clWhite );
      cnv.Brush.Color := clGray;
     end
   else
     begin
      cnv.Pen.Color := clGray;
      cnv.Brush.Color := clBlack;
     end;
   r.Offset ( x_ofs, y_ofs );
   cnv.Rectangle (r);
  end;

 vxl := vertex_map.VxList;


 cnv.Pen.Color := clBlack;
 cnv.Brush.Style := bsClear;

 if chxVertexDump.Checked then

 for n := 0 to vxl.count - 1 do
  begin
   pvx := @vxl.list [n];
   fy := pvx.pos.y;
   y := Trunc ( fy );
   rg := 20;
   if ( y < -200 ) then cl := clBlack else
    // от черного к €рко-синему
    if InRange ( y, -rg * 4, -rg * 3 ) then
       cl := Gradient ( 0, fy + rg * 4, rg ) else
    // to aqua
    if InRange ( y, -rg * 3, -rg * 2 ) then
       cl := Gradient ( 1, fy + rg * 3, rg ) else
    // to green
    if InRange ( y, -rg * 2, -rg * 1 ) then
       cl := Gradient ( 2, fy + rg * 2, rg ) else
    // to yellow
    if InRange ( y, -rg * 1, -rg * 0 ) then
       cl := Gradient ( 3, fy + rg * 1, rg ) else
    // to red
    if InRange ( y, +rg * 0, rg * 1 ) then
       cl := Gradient ( 4, fy - 000, rg ) else
    // if InRange ( y, +rg * 1, +rg * 2 ) then
       cl := Gradient ( 5, fy - rg * 1, rg * 3 ); // else
    // if InRange ( y, +rg * 5, +rg * 2 ) then
    // cl := Gradient ( 7, fy - rg * 2, rg * 5 );
    // else  cl := clPurple;
   p := v3pt ( @pvx.pos );

   if n <= vxl.last_lv then
      cnv.Pixels [ p.X, p.Y ] := cl
   else
      cnv.Ellipse ( p.X - 10, p.Y - 10, p.X + 10, p.Y + 10 );
  end;


 cnv.Pen.Color := RGB ( 0, 255, 255 );

 for n := 0 to vertex_map.SectorsCount - 1 do
  begin
   pms := vertex_map.SectorsList [n];

   r := pms.bounds_rect;
   r.Offset ( x_ofs, y_ofs );

   s := Format ( '%d ', [pms.index_count] );
   cnv.TextOut ( r.Left + 3, r.Top + 5, s );

   if pms.next <> nil then
     begin
      cnv.MoveTo ( r.CenterPoint.X, r.CenterPoint.Y );
      r := pms.next.bounds_rect;
      r.Offset ( x_ofs, y_ofs );
      cnv.LineTo ( r.CenterPoint.X, r.CenterPoint.Y );
     end;

  end;

 if last_found > 0 then
  begin
   pvx := @vxl.list [last_found];

   cnv.Pen.Color := clWhite;
   p := v3pt ( @pvx.pos );

   cnv.Ellipse ( p.X - 10, p.Y - 10, p.X + 10, p.Y + 10 );
   cnv.Pixels [p.X, p.Y] := clWhite;
  end;

end;

procedure TDebugMonitor.imgMapMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
   pv: TVector3F;
   rq: TVxRequest;
   vl: PLevelVertexList;


begin
 pv.x := x - x_ofs;
 pv.y := 0;
 pv.z := y - y_ofs;

 lbInfo.Caption := Format (' %.3f x %.3f ', [pv.x, pv.z] );

 if ( vertex_map = nil ) or ( vertex_map.VxList = nil ) or not ( ssCtrl in Shift ) then exit;
 vl := vertex_map.VxList;
 FillChar (rq, sizeof(rq), 0);
 rq.vectors [0] := pv;
 rq.i_params [3] := 1;
 FindNearestVertex ( @rq );
 last_found := rq.lv_ids [0];
 // vertex_map.NearestVertex ( @pv );
 if last_found >= 0 then
   edtTest.Text := Format( 'pos = %.1f,%.1f,%.1f  lvid = %d, gvid = %d, tvid = %d ', [pv.x, pv.y, pv.z, last_found, vl.list[last_found].gvid, rq.lv_ids[1] ] )
 else
   edtTest.Text := 'not found';


end;

procedure TDebugMonitor.updTimerTimer(Sender: TObject);
begin
 if Assigned (gDMThread) then
    gDMThread.NotifyAlive;

 btnDrawMap.Enabled := ( current_lvl <> nil );
end;

function TDebugMonitor.v3pt(pt: PVector3F): TPoint;
begin
 result := xzpt ( pt.x, pt.z );
end;

function TDebugMonitor.xzpt(x, z: Single): TPoint;
begin
 result.X := Trunc ( x ) + x_ofs;
 result.Y := Trunc ( z ) + y_ofs;
end;

function TDebugMonitor.x_ofs: Integer;
begin
 result := - br.Left + 5;
end;

function TDebugMonitor.y_ofs: Integer;
begin
 result := - br.Top + 5
end;

end.
