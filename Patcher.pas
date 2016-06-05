unit Patcher;

interface
uses Windows, Messages, SysUtils, Variants, Classes, Misc, StrClasses, IniFiles, StrUtils, LGlobal;


{$I stkdef.inc}

const
   ERR_FAIL = '#FAIL';

type

  TPatcher = class
  private
   FLastError: String;
  protected
      FCaption: String;
   FFileTarget: String;
       FHandle: THandle; // file descriptor

   function       OpenTarget: String;
   function       Problem ( bTest: Boolean; const error: String ): Boolean;

   function       Seek ( ofs: UInt64 ): Boolean;

  public


   property        Caption: String read FCaption write FCaption;
   property      LastError: String read FLastError;
   { C & D }
   constructor    Create ( const AFileTarget: String );
   destructor     Destroy; override;

   { methods }


   procedure      LoadConfig ( ini: TIniFile; const sect: String ); virtual;
   function       Patch ( const action: String ): String; virtual;
  end;



  {$IFNDEF NEWEST_BUILD}
  T3GBPatcher = class (TPatcher)
  public
   constructor    Create ( const AFileTarget: String );
   // methods
   function       Patch ( const action: String ): String; override;
  end;


  TDiffPatcher = class (TPatcher)
  private

   FDiffSource: String;

  public

   property DiffSource: String read FDiffSource write FDiffSource;


   { methods }
   procedure      LoadConfig ( ini: TIniFile; const sect: String ); override;
   function       Patch ( const action: String ): String; override;

  end;
  {$ENDIF}

implementation
uses Launcher, LCGlobals;

{ TPatcher }

constructor TPatcher.Create(const AFileTarget: String);
begin
 FFileTarget := AnsiReplaceStr( AFileTarget, '$ExePath$', ExePath );
 if Pos ('$', FFileTarget) > 0 then
    FFileTarget := ExpandPath( FFileTarget );
 FCaption := 'Noname for ' + FFileTarget;
end;

destructor TPatcher.Destroy;
begin
 if FHandle <> INVALID_HANDLE_VALUE then
                        CloseHandle (FHandle);
 inherited;
end;

procedure TPatcher.LoadConfig(ini: TIniFile; const sect: String);
begin
 // nope
end;

function TPatcher.Problem;
begin
 result := bTest;
 if bTest then
    FLastError := error;
end;

function TPatcher.Seek(ofs: UInt64): Boolean;
var
   li: LARGE_INTEGER;
begin
 li.QuadPart := ofs;
 result := ( FHandle > 0 ) and ( FHandle <> INVALID_HANDLE_VALUE );
 if result then
    result := SetFilePointer ( FHandle, li.LowPart, @li.HighPart, FILE_BEGIN ) <> INVALID_SET_FILE_POINTER;
end;

function TPatcher.OpenTarget: String;
begin
 result := 'file not exists ';;
 if not FileExists (FFileTarget) then exit;

 SetLastError (0);
 if FHandle = 0 then
    FHandle := CreateFile ( PChar (FFileTarget), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0 );

 if FHandle = INVALID_HANDLE_VALUE then
    result := 'CreateFile error ' + Err2Str()
 else
    result := '#OK';
end;

function TPatcher.Patch(const action: String): String;
begin
 result := OpenTarget;

 if Problem ( result <> '#OK', '#ERROR: cannot open file ' + FFileTarget + ', details: ' + result ) then
  begin
   result := ERR_FAIL;
   exit;
  end;

 result := '';
end;

{$IFNDEF NEWEST_BUILD}
{ T3GBPatcher }

constructor T3GBPatcher.Create(const AFileTarget: String);
begin
 inherited Create (AFileTarget);
 FCaption := 'Память 3GB';
end;

function T3GBPatcher.Patch(const action: String): String;
const
   LAA = $0020;  // LARGEADDRESSAWARE

var
  flags: BYTE;
   buff: array [0..2047] of BYTE;
    pdw: PDWORD;
    ofs: DWORD;
     rb: DWORD;
      r: Integer;


begin
 result := inherited Patch (action);
 if result <> '' then exit;

 try
   ReadFile ( FHandle, buff, sizeof (buff), rb, nil );

   if Problem ( rb < 2000, '#ERROR: readed bytes ' + IntToStr(rb) ) then exit;

   pdw := @buff [$03C];
   ofs := pdw^;  // NT header

   result := ERR_FAIL;
   if Problem ( ofs >= High (buff), ' #ERROR: wrong offset' ) then exit;

   pdw := @buff [ofs];

   if Problem ( pdw^ and $FFFF <> $4550, '#ERROR: not PE' )  then exit; // check for PE

   Inc (ofs, 22);

   flags := buff [ofs];

   if action <> '' then // patching
    begin
     if action = 'patch' then
        flags := flags or LAA
     else
        flags := flags and (not LAA);

     if Problem ( not Seek ( ofs ), '#ERROR: cannot seek file ' ) then exit;

     r := 0;

     WriteFile ( FHandle, flags, 1, rb, nil );

     if Problem ( rb <> 1, '#ERROR: cannot write one byte' ) then
        exit;

     Seek ( ofs );
     ReadFile ( FHandle, flags, 1, rb, nil ); // probe
    end;

   if ( flags and LAA <> 0 ) then
       result := 'patched'
   else
       result := 'not patched';

 finally
  CloseHandle ( FHandle );
  FHandle := 0;
 end;

end;


{ TDiffPatcher }

procedure TDiffPatcher.LoadConfig(ini: TIniFile; const sect: String);
begin
 inherited;
 DiffSource := ini.ReadString (sect, 'DiffSource', '');
 DiffSource := ExpandPath( DiffSource );

end;

function TDiffPatcher.Patch(const action: String): String;
var
   diff: TStrMap;
   cols: TStrMap;
   size: DWORD;
    ofs: DWORD;
     rb: DWORD;
     vo: BYTE;
     vp: BYTE;
     vc: BYTE;
     lc: Integer;
     ds: String;
      n: Integer;

begin
 result := inherited Patch (action);
 if result <> '' then exit;


 FLastError := '';

 diff := TStrMap.Create;
 cols := TStrMap.Create;
 try
  result := ERR_FAIL;

  if Problem ( not FileExists (DiffSource), '#ERROR: not found diff-source ' + DiffSource )  then
     exit;

  size := GetFileSize ( FHandle, nil );

  diff.LoadFromFile (DiffSource);

  if Problem ( diff.Count = 0, 'diff file is void' ) then exit;

  lc := 0;

  // foreach lines in diff file
  for n := 0 to diff.Count - 1 do
   begin
    ds := AnsiReplaceStr( diff [n], ':', '' );
    cols.Split( '^', HideSP(ds) );
    if cols.Count < 3 then continue;

    ofs := atoi ( '$' + cols [0] );
    vo := atoi ( '$' + cols [1] );
    vp := atoi ( '$' + cols [2] );

    if Problem ( ofs >= size, 'offset >= target size ' ) then break;

    Seek ( ofs );
    vc := 0;

    ReadFile ( FHandle, vc, 1, rb, nil );

    if Problem ( rb <> 1, 'Cannot read byte at $' + IntToHex(ofs, 4) + ': ' + err2str ) then break;

    if ( vc <> vo ) and ( vc <> vp ) then break; // BAD file version

    Inc (lc);

    if action <> '' then
      begin
       Seek (ofs);
       if action = 'patch' then
          WriteFile ( FHandle, vp, 1, rb, nil )
       else
          WriteFile ( FHandle, vo, 1, rb, nil );

       if Problem ( rb <> 1, 'cannot write byte at $' + IntToHex(ofs, 4) + ': ' + err2str ) then break;  // cannot write


       if Seek (ofs) then
          ReadFile ( FHandle, vc, 1, rb, nil ); // check
      end; // if

    if ( n = 0 ) then
         result := IfV( vc = vp, 'patched', 'not patched' );
   end; // for

 finally
  CloseHandle (FHandle);
  FHandle := 0;
  diff.Free;
  cols.Free;
 end;
end;

{$ENDIF}

end.
