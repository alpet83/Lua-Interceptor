// This great code ripped out from various sources.
// Enjoy!
//
// ok.
// i've used this:
// Greate D3D10/D3D10/Delphi source of code: http://labo.xo-ox.net/
//
// With love from Georgy V. Moshkin (tmtlib@narod.ru)

unit tinyD3D;

interface

uses Direct3D9;

type TDXVertex=record
                pos,
                normal : TD3DVector;
                color  : TD3DColor;
                u,v    : single;
               end;

function CoTan(const X: Extended): Extended;
function D3DXVector3(x,y,z:single):TD3DVector;
function PerspectiveFovLH(fovy, aspect, zn, zf: Single): TD3DMatrix;
function VectorNormalize(var Vec: TD3DVector): Single;
function VectorCross(const V1, V2: TD3DVector): TD3DVector;
function VectorDot(const V1, V2: TD3DVector): Single;
function LookAtLH(const Eye, At, Up: TD3DVector):  TD3DMatrix;
function MakeEasyVertex(x,y,z,u,v:single):TDXVertex;
procedure MatrixIdentity(var mat: TD3DMatrix);
procedure MatrixTranslation(var mat:TD3DMatrix;x,y,z:single);
procedure MatrixMultiply(var mat:TD3DMatrix; mat1,mat2:TD3DMatrix);
procedure MatrixRotationX(var mat:TD3DMatrix;angle:single);
procedure MatrixRotationY(var mat:TD3DMatrix;angle:single);
procedure MatrixRotationZ(var mat:TD3DMatrix;angle:single);
function rotateAxis(angle: Single;axisx,axisy,axisz:single): TD3DMatrix;

implementation

function CoTan(const X: Extended): Extended;
{ CoTan := Cos(X) / Sin(X) = 1 / Tan(X) }
asm
FLD X
FPTAN
FDIVRP
FWAIT
end;

function D3DXVector3(x,y,z:single):TD3DVector;
begin
result.x:=x;
result.y:=y;
result.z:=z;
end;

function PerspectiveFovLH(fovy, aspect, zn, zf: Single): TD3DMatrix;
var
ysc,xsc,f1:Extended;
begin
(*
ySc = cot(fovY/2)
xSc = yScale / aspect ratio

xSc 0 0 0
0 ySc 0 0
0 0 zf/(zf-zn) 1
0 0 -zn*zf/(zf-zn) 0
//*)
ysc := cotan(fovy *0.5);
xsc := ysc / aspect;

result._11:=xsc;
result._12:=0;
result._13:=0;
result._14:=0;

result._21:= 0;
result._22:= ysc;
result._23:= 0;
result._24:=0;

result._31:=0;
result._32:=0;
f1:=zf/(zn-zf);
result._33:=-f1;
result._34:= 1;

result._41:=0;
result._42:=0;
result._43:= zn*f1;
result._44:=0;


end;


function VectorNormalize(var Vec: TD3DVector): Single;
var f:single;
begin
with Vec do begin
Result := Sqrt(Sqr(x) + Sqr(y) + Sqr(z));
f:=1/Result;
x := x *f;
y := y *f;
z := z *f;
end;
end;

function VectorCross(const V1, V2: TD3DVector): TD3DVector;
begin
Result.x := V1.y * V2.z-V1.z * V2.y;
Result.y := V1.z * V2.x-V1.x * V2.z;
Result.z := V1.x * V2.y-V1.y * V2.x;
(*
Vector(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y-a.y*b.x);
//*)
end;

function VectorDot(const V1, V2: TD3DVector): Single;
begin
result := V1.x*V2.x + V1.y*V2.y + V1.z*V2.z;
end;


function LookAtLH(const Eye, At, Up: TD3DVector):  TD3DMatrix;
var
zv,xv,yv:TD3DVector;

begin
//(*
zv.x:=At.x - Eye.x;
zv.y:=At.y - Eye.y;
zv.z:=At.z - Eye.Z;
VectorNormalize(zv);
xv:=VectorCross(Up,zv);
VectorNormalize(xv);
yv:=VectorCross(zv,xv);

result._11:=xv.x;
result._12:=yv.x;
result._13:=zv.x;
result._14:=0;

result._21:= xv.y;
result._22:= yv.y;
result._23:= zv.y;
result._24:=0;

result._31:=xv.z;
result._32:=yv.z;
result._33:=zv.z;
result._34:= 0;

result._41:=-VectorDot(xv, eye);
result._42:=-VectorDot(yv, eye);
result._43:= -VectorDot(zv, eye);
result._44:=1;
(*
zv = normal(At - Eye)
xv = normal(cross(Up, zv))
yv = cross(zv, xv)
xv.x yv.x zv.x 0
xv.y yv.y zv.y 0
xv.z yv.z zv.z 0
-dot(xv, eye) -dot(yv, eye) -dot(zv, eye) 1
//*)
end;

function MakeEasyVertex(x,y,z,u,v:single):TDXVertex;
begin
 result.pos.x:=x;
 result.pos.y:=y;
 result.pos.z:=z;
 result.u:=u;
 result.v:=v;
// result.pos.rhw:=sin(gettickcount/1000)+u*v;
 result.color:= D3DCOLOR_XRGB(255,255,255);

end;

procedure MatrixIdentity(var mat: TD3DMatrix);
begin
  FillChar(mat, SizeOf(mat), 0);
  mat._11:= 1; mat._22:= 1; mat._33:= 1; mat._44:= 1;
end;

function MatE(a00,a01,a02,a03,
              a10,a11,a12,a13,
              a20,a21,a22,a23,
              a30,a31,a32,a33: single):TD3DMatrix;
begin

result._11:=a00;
result._12:=a01;
result._13:=a02;
result._14:=a03;
result._21:=a10;
result._22:=a11;
result._23:=a12;
result._24:=a13;
result._31:=a20;
result._32:=a21;
result._33:=a22;
result._34:=a23;
result._41:=a30;
result._42:=a31;
result._43:=a32;
result._44:=a33;
end;


procedure MatrixTranslation(var mat:TD3DMatrix;x,y,z:single);
begin
// FillChar(mat, SizeOf(mat), 0);
MatrixIdentity(mat);
mat._14:=x;
mat._24:=y;
mat._34:=z;

end;

procedure MatrixMultiply(var mat:TD3DMatrix; mat1,mat2:TD3DMatrix);
Var row,clm: integer;
qwe:TD3DMatrix;
begin

 for clm := 0 to 3 do
  for row := 0 to 3 do
    qwe.m[clm,row] := mat1.m[0,clm] * mat2.m[row,0] +
                      mat1.m[1,clm] * mat2.m[row,1] +
                      mat1.m[2,clm] * mat2.m[row,2] +
                      mat1.m[3,clm] * mat2.m[row,3];

mat:=qwe;

end;


procedure MatrixRotationX(var mat:TD3DMatrix;angle:single);
var cosA,sinA:single;
begin
  MatrixIdentity(mat);

 	cosA:= cos(angle); sinA:= sin(angle);

 mat:=matE(
		1, 0,     0,    0,
		0, cosA, -sinA, 0,
		0, sinA,  cosA, 0,
		0, 0,     0,    1);
end;

procedure MatrixRotationY(var mat:TD3DMatrix;angle:single);
var cosA,sinA:single;
begin
  MatrixIdentity(mat);

 	cosA:= cos(angle); sinA:= sin(angle);

	mat:=matE(
		cosA, 0, -sinA, 0,
		0,    1,  0,    0,
		sinA, 0,  cosA, 0,
		0,    0,  0,    1);

end;

procedure MatrixRotationZ(var mat:TD3DMatrix;angle:single);
var cosA,sinA:single;
begin
  MatrixIdentity(mat);

 	cosA:= cos(angle); sinA:= sin(angle);

	mat:= matE(
		cosA, -sinA, 0, 0,
		sinA,  cosA, 0, 0,
		0,     0,    1, 0,
		0,     0,    0, 1);
end;

function rotateAxis(angle: Single;axisx,axisy,axisz:single): TD3DMatrix;
var
  m: TD3DMatrix;
  cosA, sinA: Single;
begin

  // Return a rotation matrix for an arbitrary axis through the origin.
  MatrixIdentity(m);
  cosA := cos(angle);
  sinA := sin(angle);

  m._11 := cosA + (1 - cosA)*axisx*axisx;
  m._21 := (1 - cosA)*axisx*axisy - axisz*sinA;
  m._31 := (1 - cosA)*axisx*axisz + axisy*sinA;
  m._12 := (1 - cosA)*axisx*axisz + axisz*sinA;
  m._22 := cosA + (1 - cosA)*axisy*axisy;
  m._32 := (1 - cosA)*axisy*axisz - axisx*sinA;
  m._13 := (1 - cosA)*axisx*axisz - axisy*sinA;
  m._23 := (1 - cosA)*axisy*axisz + axisx*sinA;
  m._33 := cosA + (1 - cosA)*axisz*axisz;

  Result := m;

end;


end.
