program xrpack;
{$APPTYPE CONSOLE}
{$SetPEFlags $20}
uses
  Forms,
  Windows,
  Classes,
  SysUtils,
  Misc,
  ZLib,
  DateTimeTools in '..\lib\DateTimeTools.pas';

{
procedure DeflatePart(const sFileName: String; ofs: DWORD);
var
   buff: array [0..16383] of BYTE;
   ds: TDecompressionStream;
   ms: TMemoryStream;
   fs: TFileStream;
   rb: Integer;

begin

 fs := TFileStream.Create(sFileName, fmOpenRead);
 ms := TMemoryStream.Create;
 ds := nil;

 try
  fs.Position := ofs;
  rb := fs.Size - fs.Position;
  fs.Read(buff, rb);
  ms.Write(buff, rb);


  ds := TDecompressionStream.Create(ms);
  rb := ds.Read(buff, 16);
 finally
  ms.free;
  fs.Free;
  ds.Free;
 end;

 if rb <= 0 then
  begin
   ODS('[~T]. #DBG: Decompressed no data. ');
   exit;
  end;

 fs := TFileStream.Create('output.bin', fmCreate);
 fs.Write(buff, rb);

 fs.Free;

end;
}

procedure ProgressCB (cntFiles, busyOps, cntBytes: Integer; fProgress: Single); stdcall;
begin
 wprintf ( '[~T]. #DBG: Completed %3.1f %%, processed %7d files, busy %5d, loaded/saved = %.3f MiB             ~\'#13,
                         [fProgress, cntFiles, busyOps, cntBytes / MEBIBYTE ] );
end;

procedure MessageCB(szMsg: PWideChar; const pTime: PSystemTime); stdcall;
var
   dt: TDateTime;
    s: String;
begin
 if pTime <> nil then
    dt := SystemTimeToDateTime (pTime^)
 else
    dt := Now;

 s := FormatDateTime('[hh:nn:ss.zzz]', dt) + '. ' + szMsg;

 ODS (s);
end;

function        XrDB_StartProcess(szAction, szSource, szTarget, szOptions: PWideChar): Pointer; stdcall; external 'xrdb.dll';
procedure       XrDB_SetCallback (nCB: Integer; pFunc: Pointer); stdcall; external 'xrdb.dll';
function        XrDB_WaitComplete(xrp: Pointer; dwMSec: DWORD): DWORD; stdcall; external 'xrdb.dll';


procedure  ProcessList(fn: String);
var
       a, s, opts: String;
                p: Pointer;
                n: Integer;

begin
  if not FileExists(fn) then exit;
  a := ParamStr(1);
  s := ParamStr(3);
  opts := ParamStr(4);

  if (s = '') then
     begin
      s := 'L:\NLC\gamedata.db';
      n := Pos('.lst', fn);
      Dec(n);
      if ( n > 1 ) and CharInSet (fn[n], ['0'..'f']) then
          s := s + fn [n];
     end;


  wprintf('[~T]. #DBG: target = %s ', [s]);

  p := XrDB_StartProcess (PChar(a), PChar(fn), PChar(s), PChar(opts));

  while  XrDB_WaitComplete (p, 256000) <> WAIT_OBJECT_0 do
        __nop;

  // DeflatePart (fn, atoi(s));

end;

var
   n: Integer;
   f: TSearchRec;
  sl: TStringList;
  fn: String;

begin
 StartLogging('');
 ShowConsole();

 fn := ParamStr(2);
 XrDB_SetCallback (0, @MessageCB);
 XrDB_SetCallback (1, @ProgressCB);


 if (fn <> '') then
    ProcessList(fn)
 else
 if ParamStr(1) = 'PACK' then
  begin
   sl := TStringList.Create;
   if FindFirst (ExePath + 'optimal*.lst', faNormal, f) = 0 then
     Repeat
      sl.Add(f.name);

     Until FindNext (f) <> 0;
   FindClose (f);

   sl.Sort;
   for n := 0 to sl.Count - 1 do
     begin
      wprintf('[~T/~B]. #DBG: processing source list %s', [sl[n]]);
      ProcessList (ExePath + sl [n]);
     end;

   sl.Free;
  end;


 // if IsDebuggerPresent then readln;
 Windows.Beep(1000, 100);
 ExitProcess(0);
end.
