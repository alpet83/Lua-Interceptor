{ VERTEX TOoLS }

unit VxTools;

interface
uses Windows, SysUtils, Classes, Misc, StrClasses, Math, DateTimeTools, LuaTypes, XrayLua, ArrayTypes;

{
  Алгоритм быстрого поиска ближайшего к точке вертекса. Предполагается неравномерное распределение вертексов в XZ, при количестве 0.5-0.9 млн.

  Двухмерное пространство разбивается на строки (координата Z) и столбцы (координата X), например 200 * 200. Это дает конечное число секторов (40000),
  каждый из которых собирает в массив индексов вертексы (точки), принадлежащие самому сектору и его 8 соседям.
  Предполагается использование 2-байтных дельта-индексов и базового 4-байтного индекса для экономии памяти.

  Алгоритм поиска огрубляет координаты входящей точки, до координат центра сектора, после чего поиск ведется только с его вертексами и с вертексами его соседей.
  В случае если координаты точки за пределами карты, проводится исследование одной из её сторон для поиска ближайшего сектора (бинарный поиск возможен по дистанции).

  Если сектор для ожидаемой точки не заполнен вертексами, производится поиск ближайшего сектора к точке. Алгоритм поиска сектора должен оптимизироваться отдельно: возможно
  приемлемо предварительное сведение ближайших секторов по 32 направлениям в специальный список.

  Идеальное распределение 800 тысяч вертексов по 40 тысячам секторам * 9, дает 180 вертексов принадлежащих одному сектору в среднем.
  Поисковое линейное сокращение на данном этапе = 3 порядка (в 4000 раз).





}


const
   SCF_SCANNED = $8000;

type
   PVector3F = ^TVector3F;
   TVector3F = packed record
    x, y, z: Single;


    function    check_bound ( minv, maxv: Single ): Boolean;
    procedure   init ( ax, ay, az: Single );
    procedure   import ( L: lua_State; idx: Integer );
    function    distance_to ( const pv: TVector3F ): Single;
    function    point_xz: TPoint;

   end;




   TVertexMap = class;

   TLevelVertex = packed record
    gvid: WORD;
    lv16: WORD;  // only first 16 bits
     pos: TVector3F;
   end;

   PLevelVertex = ^TLevelVertex;

   PLevelVertexList = ^TLevelVertexList;


   TLevelVertexList = packed record
      cb_size: Integer;
      mb_size: Integer;
        count: Integer;
      last_lv: Integer;
        svctx: Pointer;
        min_x: Single;
        min_z: Single;
        max_x: Single;
        max_z: Single;

       reserv: array [0..15] of DWORD;
         list: array [0..2 * 1048576 - 1] of TLevelVertex;


     function  CalcSize: Integer;
     procedure Flush;
   end;

   TVxIndex = DWORD;

   TVxIndexList = packed array [0..32767] of TVxIndex;

   PVxIndexList = ^TVxIndexList;
   PVxMapSector = ^TVxMapSector;

   TScanContext = record
           pt: PVector3F;
           xz: TPoint;
        ideal: Single;
      vx_list: PLevelVertexList;
     min_dist: Single;
    best_lvid: Integer;
      checked: Integer;
       cmpcnt: Integer;
     previous: PVxMapSector;
   end;

   PScanContext = ^TScanContext;



   TVxMapSector = packed record
      sec_flags: DWORD;
    index_count: Integer;
           scol: SmallInt;
           srow: SmallInt;
    index_first: DWORD;
    bounds_rect: TRect;
      best_dist: Single;

      neighbors: array [0..7] of PVxMapSector;  // начиная с левого верхнего, по часовой стрелке обход
     index_list: PVxIndexList;
           next: PVxMapSector;

    function    Center: TVector3F;
    function    MinDistanceTo ( pt: PVector3F ): Single;
    function    Nearest ( pctx: PScanContext; recursion: Integer ): Integer;
    procedure   Release;
    procedure   ResizeIndexList (newSize: Integer);
   end;

   TSectorList = packed array [0..1048575] of TVxMapSector;

   PSectorList = ^TSectorList;




   TVertexMap = class
   private

     FFilled: TList;
    FSecList: array of TVxMapSector;
     FVxList: PLevelVertexList;

    procedure  ResizeList (newSize: Integer);
    function GetSectorByIndex(index: Integer): PVxMapSector;
    function TraceSectors(col, row, dx, dy: Integer): PVxMapSector;
    function NearestSector ( pt: PVector3F; ignore: PVxMapSector = nil; lsv: TList = nil ): PVxMapSector;

   protected
    brk_count: Integer;
     brk_summ: Integer;
     lsv_summ: Integer;



    YMin, YMax: Single;
    ZDim, XDim: Integer; // размерности матрицы секторов
    ZOfs, XOfs: Integer; // смещения для получения индексов
    FStep: Integer;       // шаг сетки (1/3 стороны сектора)
    KStep: Single;
    FRect: TRect;        // пределы входящих координат вертексов в карту

     FTimer: TProfileTimer;
    FCenter: TPoint;

    FCellBounds: TRect;  // пределы индексов ячеек.



    function            GetSector(ACol, ARow: Integer): PVxMapSector; inline;

    function            calc_col ( pt: PVector3F ): Integer; inline;
    function            calc_row ( pt: PVector3F ): Integer; inline;

   public



    property            BoundsRect: TRect read FRect;
    property            Sectors [ACol, ARow: Integer]: PVxMapSector read GetSector; { ACol, ARow must be offseted >= 0 }
    property            SectorsList [index: Integer]: PVxMapSector read GetSectorByIndex;
    property            Step: Integer read FStep;
    property            Timer: TProfileTimer read FTimer;
    property            VxList: PLevelVertexList read FVxList;
    { C & D }
    constructor         Create;
    destructor          Destroy; override;

    { methods }


    function            NearestVertex ( pt: PVector3F ): Integer;

    procedure           MakeSectors (src: PLevelVertexList; AStep: Integer = 25); // разбивка карты на сектора с индексированием

    function            SectorsCount: Integer;

    function            VertDiff: Single;


   end;

   PVxRequest = ^TVxRequest;

   TVxRequest = record
       timeout: Integer;
      src_file: array [0..263] of AnsiChar;
      rq_error: array [0..31] of AnsiChar;
       vectors: array [0..3] of TVector3F;
        lv_ids: array [0..3] of Integer;
        gv_ids: array [0..3] of WORD;
      i_params: array [0..3] of Int64;
      f_params: array [0..3] of Double;

      procedure init (tout: Integer);
      function  tick_timeout: Integer;
   end;


var
   vx_db: TStrMap = nil;

function FindLoadVertices ( rq: PVxRequest ): Integer;
function FindNearestVertex ( rq: PVxRequest ): Integer;
function LoadVertices (fn: String): Boolean;



implementation
uses LCGlobals;

const
   NB_UP_LEFT  = 0;
   NB_UP_CENTR = 1;
   NB_UP_RIGHT = 2;
   NB_MD_RIGHT = 3; // middle right
   NB_DN_RIGHT = 4;
   NB_DN_CENTR = 5;
   NB_DN_LEFT  = 6;
   NB_MD_LEFT  = 7;


function FindLoadVertices ( rq: PVxRequest ): Integer;
var
  prv: Pointer;
  ffn: String;
   fn: String;
    i: Integer;

begin
  rq.rq_error [0] := #0;
  ffn := String(rq.src_file);
  fn := ExtractFileName ( ffn );

  Assert ( Assigned(vx_db), 'vertices database not initialized' );
  i := vx_db.IndexOf (fn);
  if i < 0 then
    begin
     ODS ('[~T/~B].~C0F #DBG: Async loading vertices from ~C0A' + ffn + '~C07');
     current_lvl := nil; // prevent releasing (!)
     LoadVertices ( ffn );
     if current_lvl <> nil then
       begin
        ODS ('[~T/~B].~C0F #DBG: Load vertices successful~C07');
        vx_db.AddObject ( fn, Pointer ( current_lvl ) );
       end;
    end
  else
    begin
     prv := current_lvl;
     current_lvl := Pointer ( vx_db.Objects [i] );
     if prv <> current_lvl then
        ODS('[~T]. #DBG: vertices already in memory ~C0F' + fn + '~C07, switching current_lvl = ' + FormatPtr(current_lvl));
    end;
 if current_lvl <> nil then
    result := current_lvl.count
 else
   begin
    result := 0;
    SetStrZ (  rq.rq_error, '#FATAL: load vertices failed', 23 );
   end;

end;
function FindNearestVertex ( rq: PVxRequest ): Integer;
var
  pft: TProfileTimer;
   pt: TVector3F;
   rp: PVector3F;
   tt: Boolean;
   md: Single;
   dt: Double;
   df: Double;
   lv: Integer;

    d: Single;
    i: Integer;
    g: Integer;
    b: Integer;
begin
 result := -1;

 if current_lvl = nil then exit;

 pt := rq.vectors [0];
 tt := rq.i_params [3] <> 0;

 if vertex_map = nil then
   begin
    vertex_map := TVertexMap.Create;
    add_garbage (vertex_map);
   end;

 if current_lvl <> vertex_map.VxList then
    vertex_map.MakeSectors ( current_lvl );


 pft := vertex_map.Timer;
 pft.StartPFC (1);
 lv := vertex_map.NearestVertex ( @pt );
 df := pft.Elapsed (1);
 rq.lv_ids [0] := lv;

 result := lv;

 if lv >= 0 then
   with current_lvl^ do
    begin
     g := list [lv].gvid;
     if lv > last_lv then g := g or $8000;        // error marker
     rq.gv_ids [0] := g;
     if ( not tt ) then exit;
    end;

 // debug compare test (linear and very slow)
 b := -1;
 md := 50000;
 pft.StartPFC (1);
 for i := 0 to current_lvl.last_lv  do
  begin
   rp := @current_lvl.list [i].pos;
   d := rp.distance_to ( pt );
   if rp.y > pt.y + 1 then
         d := d + 20; // изменить предпочтительность для "надлежащих" вертексов

   if d < md then
    begin
     md := d;
     b := i;
    end;
  end;

 dt := pft.Elapsed (1);

 if dt > 22 then
    ODS ( Format ('[~T]. #PERF: debug search time =~C0D %.3f~C07 ms vs fast search time~C0F %.3f~C07 ms ', [dt, df] ) );

 if b <> lv then
  begin
   d := current_lvl.list [lv].pos.distance_to (pt);
   ODS ( CFormat ('[~T].C0F #DBG: optimized search dist = %.3f, linear search dist = %.3f ~C07', '~C0F', [d, md] ) );
  end;

 if b >= 0 then
   with current_lvl^ do
    begin
     g := list [b].gvid;
     if b > last_lv then
        g := g or $8000;
     rq.gv_ids [0] := g;
    end;

 if lv < 0 then
    rq.lv_ids [0] := b;
 rq.lv_ids [1] := b;


end;


function LoadVertices (fn: String): Boolean;
var
   fobj: TFileStream;
begin
result := FALSE;

 fobj := TFileStream.Create ( fn, fmOpenRead or fmShareDenyWrite );
 if fobj.Size = 0 then
   begin
    PrintError (' File size = 0 for ' + fn );
    fobj.Free;
    DeleteFile (fn);
    exit;
   end;

 try
  current_lvf := fn;

  if current_lvl <> nil then
     FreeMem (current_lvl);

  GetMem ( current_lvl, fobj.Size );
  fobj.Read ( current_lvl^, fobj.Size );

  with current_lvl^ do
   begin
    Assert ( ( count > 0 ) and ( count < 5000000 ), ' strange vertex count = ' + IntToStr(count) );

    svctx := nil;
    if last_lv = 0 then
       last_lv := count - 1;


    // lua_pushwstr ( L, '#OK');
    ODS ( CFormat('[~T]. #DBG: level vertex map loaded. Level count = %d, total count = %d. Bound rect XZ = { %.2f, %.2f, %.2f, %.2f } ',
                                '~C07', [last_lv + 1, count,  min_x, min_z, max_x, max_z] ) );
   end;

  result := TRUE;
 finally
  fobj.Free;
 end;

end;

function cmpSectorDist (a, b: Pointer): Integer;
var
   sa, sb: PVxMapSector;
begin
 sa := a;
 sb := b;
 result := 0;
 // сектора с большой удаленностью, задвинуть в конец
 if sa.best_dist > sb.best_dist then result := +1;
 if sa.best_dist < sb.best_dist then result := -1;
end;

function v3f ( x, y, z: Single ): TVector3F;
begin
 result.x := x;
 result.y := y;
 result.z := z;
end;


{ TLevelVertexList }

function TLevelVertexList.CalcSize: Integer;
begin
 result := sizeof (self) - sizeof (List) + sizeof(TLevelVertex) * count;
end;

procedure TLevelVertexList.Flush;
var
   fobj: TFileStream;
   acnt: Integer;
   ucnt: Integer;
    pgv: PLevelVertex;
    plv: PLevelVertex;
    fgv: Integer;
     gv: Integer;
     lv: Integer;
     md: Single;
      d: Single;

begin
 if count = 0 then exit;
 cb_size := CalcSize;
 wprintf('[~T/~B]. #DBG: Saving %d level vertices, %d total records, size = %d ', [last_lv + 1, count, cb_size] );

 acnt := 0;
 ucnt := 0;
 fgv := last_lv + 1;

 // цикл тяжелого матричного перебора вертексов, для ассоциации уровневых и глобальных игровых по дистанции
 for lv := 0 to last_lv do
  begin
   plv := @list [lv];
   if plv.gvid < $8000 then continue;

   md := 1e10;

   for gv := fgv to count - 1 do
    begin
     pgv := @list [gv];
     d := pgv.pos.distance_to ( plv.pos );
     if d < md then
       begin
        md := d;
        plv.gvid := pgv.gvid;
       end;
    end;

   if plv.gvid < $8000 then
      Inc (acnt)
   else
      Inc (ucnt);
  end;

 fobj := svctx;
 if fobj <> nil then
    fobj.Write(self, cb_size)
 else
    PrintError('Flush: FileStream object is not assigned!');

 wprintf('[~T/~B]. #DBG: associatied gvid-to-lvid %d, non-associated %d ', [acnt, ucnt]);

 count := 0;
end;


{ TVertexMap }

function TVertexMap.calc_col (pt: PVector3F): Integer;
// var xx: Single;
begin
 // xx := ( pt.x - FRect.Left );
 result := Round ( pt.x / FStep - 0.5 );
end;

function TVertexMap.calc_row (pt: PVector3F): Integer;
// var zz: Single;
begin
 // zz := ( pt.z - FRect.Top );
 result := Round ( pt.z / FStep - 0.5 );
end;

constructor TVertexMap.Create;
begin
 FFilled := TList.Create;
 FTimer := TProfileTimer.Create;
end;

destructor TVertexMap.Destroy;
begin
 ResizeList (0);
 FFilled.Free;
 FTimer.Free;
 inherited;
end;

function TVertexMap.GetSector(ACol, ARow: Integer): PVxMapSector;
var
   index: Integer;
begin
 ACol := ACol - FCellBounds.Left;
 ARow := ARow - FCellBounds.Top;
 index := ARow * XDim + ACol;

 Assert ( ( index >= 0 ) and ( index < Length(FSecList) ), Format(' index outbound sector list %d vs count %d ', [index, Length(FSecList)] ) );

 result := @FSecList [index];
end;

function TVertexMap.GetSectorByIndex(index: Integer): PVxMapSector;
begin
 result := @FSecList [index];
end;

procedure TVertexMap.MakeSectors(src: PLevelVertexList; AStep: Integer);


var
   i, nv, ns, row, col: Integer;

   //  xx, zz: Single;
   pv: PLevelVertex;
   ps: PVxMapSector;
   xx: Integer;
   zz: Integer;
   ri: DWORD;



begin
 ResizeList(0); // clean old

 FVxList := src;

 FStep := Max (10, AStep);

 KStep := 1.0 / FStep;

 if src.count = 0 then exit;

 ODS( CFormat('[~T/~B]. #DBG: MakeSectors processing %d records...', '~C07', [src.count] ) );

 // WARN: Z from - to + = from UP to DOWN
 // WARN: индексы привязываются к массиву вертексов src

 { debug sample:
    step = 100
    src = ( min_x = -10000,  min_z = -10000, max_x = 10000, max_z = 10000 )

   rounding with expand:
    FRect = ( Left = - 11000, Top = - 11000, Right = 11000, Bottom = 11000 )

   XDim = 22000 / 100 + 1 = 221 = ZDim


  }

 // округление координат до тысячей
 FRect.Left :=   Round ( src.min_x * KStep ) * FStep;
 FRect.Right :=  Trunc ( src.max_x * KStep ) * FStep;
 FRect.Top    := Round ( src.min_z * KStep ) * FStep; // upper side
 FRect.Bottom := Trunc ( src.max_z * KStep ) * FStep;


 while FRect.Left > src.min_x do
       FRect.Left  :=  FRect.Left - FStep;

 while FRect.Right < src.max_x do
       FRect.Right :=  FRect.Right + FStep;

 while FRect.Top > src.min_z do
       FRect.Top  :=   FRect.Top - FStep;

 while FRect.Bottom < src.max_z do
       FRect.Bottom := FRect.Bottom + FStep;

 // определение размерности сетки
 FCellBounds.Left   := Trunc ( FRect.Left / FStep );
 FCellBounds.Top    := Trunc ( FRect.Top  / FStep );
 FCellBounds.Right  := Trunc ( FRect.Right  / FStep + 1 );
 FCellBounds.Bottom := Trunc ( FRect.Bottom / FStep + 1 );

 XDim := FCellBounds.Width  + 1;
 ZDim := FCellBounds.Height + 1;
 FCenter.X := ( FCellBounds.Left + FCellBounds.Right ) div 2;
 FCenter.Y := ( FCellBounds.Top  + FCellBounds.Bottom ) div 2;
 ResizeList( ZDim * XDim ); //
 ZeroMemory ( @FSecList [0], sizeof (FSeclist[0]) * ZDim * XDim );
 // распределение всех вертексов по секторам, автоматически дает географическую матричную сортировку

 YMin := +1e10;
 YMax := -1e10;


 for nv := 0 to src.last_lv do
  begin
   pv := @src.list [nv];
   // relative coord transform
   col := calc_col ( @pv.pos );
   row := calc_row ( @pv.pos );

   if not PtInRect ( FCellBounds, Point (col, row) ) then continue; // TODO: outbound vertex must be saved as exception

   YMin := Min (YMin, pv.pos.y);
   YMax := Max (YMax, pv.pos.y);

   ps := Sectors [col, row]; //

   // reallocation adjust
   if ps.index_count and $0F = 0 then
     begin
      if ps.index_count = 0 then
        begin
         ps.index_first := nv;
         xx := col * FStep;
         zz := row * FStep;
         SetRect ( ps.bounds_rect, xx, zz, xx + FStep, zz + FStep );
         // SetRect ( ps.bounds_rect, 10000, 10000, -10000, -10000 );
        end;

      ps.ResizeIndexList ( ps.index_count + $10 );
     end;

   {
   with ps.bounds_rect do
    begin
     xx := Trunc ( pv.pos.x );
     zz := Trunc ( pv.pos.z );

     Left := Min ( Left, xx - 1 );
     Top  := Min ( Top,  zz - 1 );
     Right := Max ( Right, xx + 1 );
     Bottom := Max ( Bottom, zz + 1 );
    end; // }


   ps.index_list [ps.index_count] := nv;
   Inc (ps.index_count);

  end;


 // распределение соседей секторов (взаимные ссылки ячеек матрицы).


 // 0-UL 1-UC 2-UR 3-CR 4-DR 5-DC 6-DL 7-CL
 FFilled.Clear;

 with FCellBounds do
 for row := Top to Bottom do
  for col := Left to Right do
   begin
    ps := Sectors [col, row];
    ps.scol := col;
    ps.srow := row;

    if ( ps.index_count = 0 ) then
      begin
       xx := Trunc ( col * FStep );
       zz := Trunc ( row * FStep );
       ps.bounds_rect.Create ( xx + 2, zz + 2, xx + FStep - 2, zz + FStep - 2 );
      end
     else
      FFilled.Add (ps);


    // col in left..right
    // row in top..bottom

    // сектора правее левой границы сетки
    ps.neighbors [NB_MD_LEFT] := TraceSectors ( col, row, -1,  0 );
    ps.neighbors [NB_UP_LEFT] := TraceSectors ( col, row, -1, +1 );
    ps.neighbors [NB_DN_LEFT] := TraceSectors ( col, row, -1, -1 );

    // сектора левее правой границы сетки
    ps.neighbors [NB_MD_RIGHT] := TraceSectors ( col, row, +1,  0 );
    ps.neighbors [NB_UP_RIGHT] := TraceSectors ( col, row, +1,  1 );
    ps.neighbors [NB_DN_RIGHT] := TraceSectors ( col, row, +1, -1 );

    ps.neighbors [NB_UP_CENTR] := TraceSectors ( col, row, 0,  +1 );
    ps.neighbors [NB_DN_CENTR] := TraceSectors ( col, row, 0,  -1 );

   end;

 ODS( CFormat('[~T/~B]. #DBG: MakeSectors total sectors = %d, filled = %d ', '~C07', [SectorsCount, FFilled.Count] ) );
end;

function TVertexMap.TraceSectors ( col, row, dx, dy: Integer ): PVxMapSector;
var
   t: PVxMapSector;
   v: TVector3F;
   p: TPoint;

begin
 result := nil;

 if PtInRect ( FCellBounds, Point (col + dx, row + dy) ) then
  begin
   t := Sectors [col + dx, row + dy];
   result := t;
   if t.index_count > 0 then exit;
   result := nil;
   exit;
  end
 else
   exit;


 t := Sectors [col, row];

 p := t.bounds_rect.CenterPoint;
 v.X := p.X + FStep * dx * 0.9;
 v.y := 0;
 v.Z := p.Y + FStep * dy * 0.9;

 result := NearestSector ( @v ); // ближайший сектор с дичью

end;

function TVertexMap.VertDiff: Single;
begin
 result := YMax - YMin;
end;

function TVertexMap.NearestSector (pt: PVector3F; ignore: PVxMapSector; lsv: TList ): PVxMapSector;

const
   DEG_TO_RAD = PI / 180;

var
   md: Single;
   mo: Single;
    d: Single;
    n: Integer;
    s: PVxMapSector;


begin
 md := 1e10;
 mo := Step * 3;
 result := nil;
 // if FALSE then
 for n := 0 to FFilled.Count - 1 do
  begin
   s := FFilled [n];
   if ( s = ignore )  then continue;
   d := s.MinDistanceTo ( pt );
   s.best_dist := d;
   if d < md + mo then
      lsv.Add (s);
   if ( d < md ) then
    begin
     md := d;
     result := s;
    end;
  end; // for

end;

function TVertexMap.NearestVertex (pt: PVector3F): Integer;
var
        sector: PVxMapSector;
         overd: Single;
           ctx: TScanContext;
           lsv: TList;

             n: Integer;


begin
 // if not PtInRect ( FCellBounds, Point (col, row) ) then exit;

  for n := 0to Length (FSecList) - 1 do
    with FSecList [n] do
      begin
       sec_flags := sec_flags and ( not SCF_SCANNED );
       next := nil;
      end;

 // sector := self.Sectors [col, row];
 // Assert ( sector <> nil, Format( 'sectors[%d, %d] = nil', [col, row] ) );
 lsv := TList.Create;
 //  if sector.index_count = 0 then
 NearestSector ( pt, nil, lsv );
 lsv.Sort ( cmpSectorDist );

 ctx.pt := pt;
 ctx.xz := pt.point_xz;
 ctx.ideal := 0.01;
 ctx.min_dist := 20000;
 ctx.cmpcnt := 0;
 ctx.best_lvid := -1;
 ctx.vx_list := VxList;
 ctx.checked := 0;
 ctx.previous := nil;

 overd := Sqrt ( Sqr(Step) + Sqr(VertDiff) ); // максимальное перекрытие сектора в 3D

 for n := 0 to Min (30, lsv.Count - 1) do
  begin
   sector := lsv [n];
   if ctx.min_dist + overd < sector.best_dist then
     begin
      Inc ( brk_count );
      Inc ( brk_summ, n );
      Inc ( lsv_summ, lsv.Count );
      if brk_count mod 1000 = 0 then
         ODS( CFormat ('[~T]. #PERF: median break loop = %.1f / %.1f ', '~C07', [ brk_summ / brk_count, lsv_summ / brk_count ] ) );

      break;
     end;
   // if ctx.min_dist <= sector.best_dist * 0.1 then break;
   sector.Nearest ( @ctx, 0 );
   // оставшееся число секторов для проверки, не должно превышать предельное число секторов по окружности для best_dist

  end;
 result := ctx.best_lvid;
 if ctx.cmpcnt > 140000 then
    ODS ('[~T]. #PERF: fast search comparision count =~C0D ' + IntToStr(ctx.cmpcnt) + '~C07' );
 lsv.Free;
end;

procedure TVertexMap.ResizeList(newSize: Integer);
var
   n, oldSize: Integer;

begin
 oldSize := Length (FSecList);

 for n := newSize to oldSize - 1 do
          FSecList[n].Release;

 SetLength (FSecList, newSize);
end;

function TVertexMap.SectorsCount: Integer;
begin
 result := Length ( FSecList );
end;

{ TVxMapSector }

function TVxMapSector.Center: TVector3F;
var
   p: TPoint;
begin
 p := bounds_rect.CenterPoint;
 result.x := p.x;
 result.y := 0;
 result.z := p.Y;
end;

function TVxMapSector.MinDistanceTo(pt: PVector3F): Single;
var
   d0, d1, d2, d3: Single;
begin
 with bounds_rect do
  begin
   d0 := pt.distance_to ( v3f ( Left,  0, Top ) );
   d1 := pt.distance_to ( v3f ( Right, 0, Top ) );
   d2 := pt.distance_to ( v3f ( Right, 0, Bottom ) );
   d3 := pt.distance_to ( v3f ( Left,  0, Bottom ) );

   d0 := Min (d0, d1);
   d2 := Min (d2, d3);
   result := Min (d0, d2);
  end;

end;

function TVxMapSector.Nearest( pctx: PScanContext; recursion: Integer  ): Integer;
var
   ns: set of BYTE;
   cp: TPoint;
    n: Integer;
    i: TVxIndex;
    t: PLevelVertex;
    d: Single;


begin
 result := -1;
 if @self = nil then exit;

 if ( sec_flags and SCF_SCANNED <> 0 ) or ( recursion > 100 ) then exit;

 Inc ( pctx.checked );

 sec_flags := sec_flags or SCF_SCANNED;

 if pctx.checked > 50 then
   asm
    nop
   end;


 with pctx^ do
  for n := 0 to Integer (index_count) - 1 do
    begin
     i := index_list^ [n];
     Assert ( Integer(i) < vx_list.count, Format('index outbound in vx_list %d vs count %d ', [i, vx_list.count]) );
     t := @vx_list.list[i];
     d := t.pos.distance_to ( pt^ );
     if t.pos.y > pt.y + 1 then
         d := d + 20; // изменить предпочтительность для "надлежащих" вертексов

     Inc ( cmpcnt );
     if d < min_dist then
       begin
        min_dist := d;
        best_lvid := i;
        if pctx.previous <> nil then
           pctx.previous.next := @self;

       end;
    end;

 // ( pctx.best_lvid > 0 ) and ( min_dist > ideal ) or
 with pctx^ do
 if ( min_dist > 1000 ) then
    begin

     if ( recursion = 0 ) and ( pctx.best_lvid = 1 ) then
       // проход по всем соседям на первый раз
        ns := [NB_UP_LEFT..NB_MD_LEFT]
     else
       begin
        ns := [];
        // движение по секторной карте как у шахматной ладьи, на одну клеточку
        cp := bounds_rect.CenterPoint;

        if xz.X >= cp.x then ns := ns + [NB_MD_RIGHT];
        if xz.X <= cp.x then ns := ns + [NB_MD_LEFT];

        if xz.Y >= cp.y then ns := ns + [NB_DN_CENTR];
        if xz.Y <= cp.y then ns := ns + [NB_UP_CENTR];
       end;

     pctx.previous := @self;

     for n := 0 to High (neighbors) do
       if BYTE(n) in ns then
         begin
          neighbors [n].Nearest ( pctx, recursion + 1 );
          if min_dist < ideal then break;
         end
    end;

 result := pctx.best_lvid;
end;

procedure TVxMapSector.Release;
begin
 index_count := 0;
 if index_list = nil then exit;
 FreeMem (index_list);
 index_list := nil;
end;

procedure TVxMapSector.ResizeIndexList(newSize: Integer);
begin
 ReallocMem ( index_list, newSize * sizeof(TVxIndex) );
end;

{ TVector3F }

function TVector3F.check_bound(minv, maxv: Single): Boolean;
begin
 result := ( x > minv ) and ( x < maxv ) and
           ( y > minv ) and ( y < maxv ) and
           ( z > minv ) and ( z < maxv );
end;

function TVector3F.distance_to( const pv: TVector3F ): Single;
var
   xd, yd, zd: Single;
begin
 xd := pv.x - x;
 yd := pv.y - y;
 zd := pv.z - z;

 result := xd * xd + yd * yd + zd * zd;
 result := Sqrt ( result );
end;

procedure TVector3F.import(L: lua_State; idx: Integer);
var
   pso: PPointerArray;
   pvo: PSingleArray;
begin
 if lua_type (L, idx) in LUA_ANY_USERDATA then else exit;
 pso := lua_topointer (L, idx); // CScriptObject
 if pso = nil then exit;
 pvo := pso^[0];                // _vector3
 if pvo = nil then exit;
 x := pvo [0];
 y := pvo [1];
 z := pvo [2];
end;

procedure TVector3F.init(ax, ay, az: Single);
begin
 x := ax;
 y := ay;
 z := az;
end;

function TVector3F.point_xz: TPoint;
begin
 result.X := Trunc (x);
 result.Y := Trunc (z);
end;

{ TVxRequest }

procedure TVxRequest.init(tout: Integer);
begin
 FillChar (self, sizeof(self), 0);
 SetStrZ (rq_error, 'request timeout', 32);
 timeout := tout;
end;

function TVxRequest.tick_timeout: Integer;
begin
 Dec (timeout);
 result := timeout;
end;


initialization
 vx_db := TStrMap.Create;
finalization
 vx_db.FreeData;
 FreeAndNil (vx_db);
end.
