unit NLCPack;

interface
uses Windows, SysUtils, Classes, StrClasses, Misc, Math, MD5, Zlib;


const
   NLCPACK_VERSION = $10001;

type
    TNLCPackHeader = packed record
     signature: array [0..7] of AnsiChar;
     version: DWORD;
     dir_size: Integer;
     dir_hash: TMD5Digest;
     file_hash: TMD5Digest;
     resv: array [0..63] of BYTE;
    end; // TNLCPack

    TNLCDirEntry = packed record
     cbEntrySize: WORD;
     fileName: array [0..127] of AnsiChar;
     fileSize: Integer;
     block_start: Integer;
     block_size: Integer;
     block_hash: TMD5Digest;
     resv: array [0..31] of BYTE;
    end;



procedure ArchiveFiles ( src: TStrMap; const dst: String; bAdd, bRemovewSrc: Boolean);
procedure NLCUnpack (const src, dst_path: String);


implementation


procedure ArchiveFiles ( src: TStrMap; const dst: String; bAdd, bRemovewSrc: Boolean);
var
   n, dsz: Integer;
   sa: AnsiString;
   fs: TFileStream;
   cs: TCompressionStream;
   ms: TMemoryStream;
   fh: THandle;
   hdr: TNLCPackHeader;
   dir: array of TNLCDirEntry;
   buff: array [0..65535] of BYTE;
   fsz, ofs, rb, wb: DWORD;



begin
 if FileExists (dst) then DeleteFile (dst);

 if src.Count = 0 then exit;

 SetLength (dir, src.Count);
 dsz := sizeof (TNLCDirEntry) * Length (dir);
 FillChar (dir[0], dsz, 0); // fillzero
 FillChar (hdr, sizeof (hdr), 0);

 ODS('[~T]. #DBG: Inflating files to archive ~C0A' + dst + '~C07');

 ms := TMemoryStream.Create;
 fs := TFileStream.Create ( dst, fmCreate or fmShareDenyWrite );
 try
  StrPCopy (hdr.signature, 'NLCPACK');
  hdr.version := NLCPACK_VERSION;
  hdr.dir_size := Length (dir);

  fs.Write (hdr, sizeof (hdr));                // резервирование места

  for n := 0 to Length (dir) - 1 do
   begin
    dir [n].cbEntrySize := sizeof (TNLCDirEntry);
    sa := AnsiString ( ExtractFileName ( src [n] ) );
    StrLCopy ( dir [n].fileName, PAnsiChar ( sa ), High ( dir [n].fileName ) );

    fs.Write ( dir [n], sizeof (TNLCDirEntry)); // резервирование места
   end;

  // ------------------- packing with zip ------------------ //
  for n := 0 to src.Count - 1 do
   begin
    // -- метод бережного открытия и получения контента -- //
    fh := CreateFile ( PChar ( src [n] ), GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, 0, 0);
    if fh = INVALID_HANDLE_VALUE then
      begin
       PrintError ( 'Cannot open file for read: ' + src [n] );
       continue;
      end;
    ofs := 0;
    dir [n].fileSize := 0;
    dir [n].block_start := fs.Position;

    fsz := GetFileSize (fh, nil);

    cs := TCompressionStream.Create (clFastest, fs);
    try

    // -- цикл считывания бинарного содержимого, и запаковки
     while ( ofs < fsz ) do
      begin
        rb := Min ( sizeof (buff), fsz - ofs );
        ReadFile (fh, buff, rb, rb, nil);
        if rb = 0 then break;
        ofs := ofs + rb;
        wb := cs.Write (buff, rb);
        Inc ( dir [n].fileSize, wb );
      {ODS( Format ('[~T]. #DBG(Inflate %s): ofs = $%x, rb = %d, wb = %d, fs.pos = %d, fs.size = $%x ',
                        [src [n], ofs, rb, wb, fs.Position, fs.Size]));}
      end;
     finally
      FreeAndNil (cs);
     end;


    dir [n].block_size := fs.Size -  dir [n].block_start;
   end;

   fsz := fs.Size;

   if fsz > DWORD ( dir [0].block_start ) then
   try
    rb := fsz - DWORD ( dir [0].block_start ); // сколько упакованных байтов вписалось

    fs.Seek ( dir [0].block_start, soFromBeginning );

    ms.Clear;
    if  ms.CopyFrom (fs, rb) = rb then
      begin
       hdr.file_hash := MD5Stream (ms);
      end;

    for n := 0 to Length (dir) - 1 do
      begin
       ms.Clear;
       fs.Seek ( dir [n].block_start, soFromBeginning );
       rb := ms.CopyFrom ( fs, dir [n].block_size );
       if Integer(rb) < dir [n].block_size then continue;
       dir [n].block_hash := MD5Stream (ms);
      end;

    hdr.dir_hash := MD5Buffer ( dir[0], dsz );

    fs.Seek ( 0, soFromBeginning );
    fs.Write ( hdr, sizeof (hdr) );
    fs.Write ( dir[0], dsz );
    fs.Position := fsz;

   except
    on E: Exception do
       PrintError ('Exception catched in ArchiveFiles: ' + E.Message);
   end;


 finally

  fs.Free;
  ms.Free;
  SetLength (dir, 0);
 end;
 XSleep (10);
end;


procedure NLCUnpack (const src, dst_path: String);
var
   fs, nfs: TFileStream;
   ds: TDecompressionStream;
   ms: TMemoryStream;
   hdr: TNLCPackHeader;
   dir: array of TNLCDirEntry;
   n, dsz: Integer;
   ref: TMD5Digest;
   fn: String;

begin
 if not FileExists (src) then exit;
 if not DirectoryExists (dst_path) then exit;

 nfs := nil;
 fs := TFileStream.Create (src, fmOpenRead or fmShareDenyWrite );

 ms := TMemoryStream.Create;
 try
  fs.Read ( hdr, sizeof (hdr) );
  hdr.signature [7] := #0;

  if PAnsiChar (@hdr.signature) <> 'NLCPACK' then
    begin
     PrintError('NLCUnpack - wrong signature ');
     exit;
    end;

  if hdr.version <> NLCPACK_VERSION then
     ODS('[~T].~C0C #WARN:~C07 Archive version different =~C0D $' + IntToHex (hdr.version, 8) + '~C07' );

  dsz := hdr.dir_size * sizeof (TNLCDirEntry);

  SetLength (dir, hdr.dir_size);

  fs.Read (dir[0], dsz);

  ref := MD5Buffer (dir[0], dsz);

  if not MD5DigestCompare (hdr.dir_hash, ref) then
    begin
     PrintError('NLCUnpack - directory hash wrong!');
     exit;
    end;


  for n := 0 to Length (dir) - 1 do
   begin
    fn := AnsiTrim2W ( dir [n].fileName ); // TODO: check bad name!
    ODS('[~T]. #DBG(Inflate): Unpacking file ~C0A' + String(fn) + '~C07');


    if dir [n].cbEntrySize <> sizeof ( TNLCDirEntry ) then
      begin
       PrintError('NLCUnpack - DirEntry #' + IntToStr(n) + ' wrong cbEntrySIze!');
       continue;
      end;
    fs.Position := dir [n].block_start;

    ms.Clear;
    ms.CopyFrom ( fs, dir [n].block_size );
    ref := MD5Stream (ms);

    if not MD5DigestCompare (dir [n].block_hash, ref) then
      begin
       PrintError('NLCUnpack - block #' + IntToStr(n) + ' hash wrong!');
       continue;
      end;


    ms.Position := 0;

    nfs := TFileStream.Create ( AddSlash (dst_path) + fn, fmCreate or fmShareDenyWrite );
    try
     repeat
      ds := TDecompressionStream.Create (ms);
      try
       if ds.Size > 0 then
          nfs.CopyFrom ( ds, ds.size )
       else
          break;
      finally
       FreeAndNil (ds);
      end;
      ODS ('[~T]. #DBG: ms.pos = ' + IntToStr(ms.Position) + ', ms.size = ' + IntToStr (ms.Size) );

     until ( TRUE );

     ODS ( CFormat ('[~T]. #DBG: unpacked %d bytes from %d ', '~C07', [nfs.Size, dir [n].fileSize]) );

    finally
     FreeAndNil (nfs);
     ms.Clear;
    end;
   end;



 finally

  SetLength (dir, 0);
  fs.Free;
  ms.Free;
 end;
end; // NLCUnpack

end.

