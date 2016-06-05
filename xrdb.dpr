{$IFDEF CPUX64}
 library xrdb64;
{$ELSE}
 library xrdb;
{$ENDIF}

{ Библиотека для быстрой отработки распаковки/архивации xray database файлов. Написано по мотивам конвертора от уважаемого Bardak'a.

  Взаимодействие с вызывающей программой заключается в запуске асинхронных процессов (запаковка/распаковка).
  Предварительно можно выставить callback для эпизодического вызова, чтобы отражать прогресс операции.

}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMM4Messages in '..\lib\FastMM4Messages.pas',
  Windows,
  System.SysUtils,
  System.Classes,
  StrUtils,
  misc in '..\lib\misc.pas',
  ModuleMgr in '..\lib\ModuleMgr.pas',
  WThreads,
  Zlib,
  Math, MD5, IniFiles,
  ContNrs, LuaTypes, XrayLua, LuaTools,
  Scrambler in 'Scrambler.pas',
  StrClasses in '..\lib\StrClasses.pas',
  DateTimeTools in '..\lib\DateTimeTools.pas',
  FastSync in '..\lib\FastSync.pas',
  ArrayTypes in '..\lib\ArrayTypes.pas',
  UniArray in '..\lib\UniArray.pas',
  MemStat in '..\lib\MemStat.pas',
  WinHeap in '..\lib\WinHeap.pas',
  msvcrt in '..\lib\msvcrt.pas',
  PsUtils in '..\lib\PsUtils.pas',
  Classes64 in '..\lib\Classes64.pas';

{$R xrdb.res}
{$I stkdef.inc}


const
     DB_CHUNK_INDEX     = $00000001;
     DB_CHUNK_DATA      = $00000002;
     DB_CHUNK_USERDATA	= $00000279;
     DB_CHUNK_TAGGED    = $01000000;
     CHUNK_COMPRESSED	= $80000000;


     DB_VERSION_AUTO		= $00;
     DB_VERSION_1114		= $01;
     DB_VERSION_2215		= $02;
     DB_VERSION_2945		= $04;
     DB_VERSION_2947RU	        = $08;
     DB_VERSION_2947WW	        = $10;
     DB_VERSION_XDB		= $20;


     SIGNATURE_TAGV0            = $FA00DB47;

     PF_DIRECTORY               = $0001;
     PF_FILE                    = $0002;
     PF_HAVE_TAG                = $0010;
     PF_OFFSET_FIRST            = $0020;
     PF_OFFSET_64               = $0040;
     PF_OFFSET_ZERO             = $0080;
     PF_OFFSET_CHUNK            = $1000;

     LZO_E_OK                    = 0;
     LZO_E_ERROR                 = -1;
     LZO_E_OUT_OF_MEMORY         = -2;    // [not used right now] */
     LZO_E_NOT_COMPRESSIBLE      = -3;    // [not used right now] */
     LZO_E_INPUT_OVERRUN         = -4;
     LZO_E_OUTPUT_OVERRUN        = -5;
     LZO_E_LOOKBEHIND_OVERRUN    = -6;
     LZO_E_EOF_NOT_FOUND         = -7;
     LZO_E_INPUT_NOT_CONSUMED    = -8;
     LZO_E_NOT_YET_IMPLEMENTED   =- 9;    // [not used right now] */

     HASH_MAP_MASK = $FFF;

     ZERO_VECTOR: array [0..3] of UInt64 = ( 0, 0, 0, 0 );

{$IFDEF CPUX64}
  LZ4_DLL = 'LZ4_x64.dll';
{$ELSE}
{$SetPEFlags $20}
  LZ4_DLL = 'LZ4.dll';
{$ENDIF}


type
  TProgressCallback = procedure (cntOps, busyOps, cntBytes: Integer; fProgress: Single); stdcall;
  TMessageCallback  = procedure (szMsg: PWideChar; const pTime: PSystemTime); stdcall;

  TArchiveChunk   = class;
  TXrDB_Processor = class;

  TFileTagV0 = packed record
           crTime: FILETIME; // 8
       laTimeDiff: DWORD;    // 12
       lwTimeDiff: DWORD;    // 16
      dwFileAttrs: DWORD;    // 20
      dwSignature: DWORD;    // 24
         Reserver: array [0..1] of DWORD;
  end; // 32 bytes struct

  PFileTagV0 = ^TFileTagV0;

  TLZMemManager = packed record
     ext_alloc: function (uBytes: SIZE_T): Pointer; stdcall;
   ext_realloc: function (pOld: Pointer; uBytes: SIZE_T): Pointer; stdcall;
      ext_free: procedure (pBuff: Pointer); stdcall;
  end; // TLZMemManager

  PLZMemManager = ^TLZMemManager;


  TChunkHeader = packed record
   id, size: DWORD;
  end; // TChunk

  PChunkHeader = ^TChunkHeader;


  TChunkRef = record
   id, size: DWORD;
   offset: LARGE_INTEGER; // typically only to first chunk
   pData: Pointer;
  end;

  PChunkRef = ^TChunkRef;

  TIndexEntryHdr = packed record
   cbHdrSize: WORD;                // +02 bytes
       flags: WORD;                // +02
      cbReal: DWORD;
      cbComp: DWORD;
         crc: DWORD;
    upd_time: UInt64;
  end;

  PIndexEntryHdr = ^TIndexEntryHdr;


  TAsyncFile = class
  private
   ovpd: OVERLAPPED;
   hRead: THandle;    // source read
   hWrite: THandle;    // dest to write
   FOwner: TXrDB_Processor;



  public


   sz_real, cb_readed: UInt64;
    sz_compressed: DWORD;
        crc, rcrc: DWORD;
    source_offset: UInt64;
    target_offset: UInt64;
      next_offset: UInt64; // рассчетно-проверочное значение

      cloned_from: TAsyncFile;
           f_name: String;
           f_opts: String;
            f_tag: TFileTagV0;

         is_clone: Boolean;
          is_last: Boolean;
            chunk: TArchiveChunk;

             data: array of BYTE;
            udata: array of BYTE;
     udata_writes: Integer;

        pack_ratio: Single; // минимальный коэффициент сжатия, при котором запаковка допускается
          b_packed: Boolean;
       save_packed: Boolean;
          upd_time: UInt64;
            op_log: String;
        auto_write: Boolean;
         tag_bytes: BYTE;
        tail_bytes: BYTE;      // сколько байт в конце для выравнивания
              busy: CHAR;


   property Owner: TXrDB_Processor read FOwner;


   { C & D }
   constructor  Create;
   destructor   Destroy; override;


   { methods }

   procedure    AsyncDeflate;
   procedure    BeginRead(extra: DWORD = 0);
   procedure    BeginWrite(bAsync: Boolean);
   procedure    CloseRead;
   function     Compressible: Boolean;
   procedure    FreeData ( flt: String = 'PDH' );

   procedure    OpenRead ();
   procedure    OnWriteComplete();
   procedure    Reset ();
   procedure    SendEvent ();
   function     Tagged: Boolean;
   procedure    TryDeflate (temp, big_temp: Pointer; big_temp_size: DWORD); // попытаться сжать

   procedure    PutData(dst: CHAR; const buff; uBytes: DWORD);

   procedure    UpdateFromTag;

  end; // TAsyncFile



  //--------------------------------------------------------------------------------------------------------------------------------------------//
  // thread for async pack/unpack

  TPackerThread = class (TWorkerThread)
  protected

   temp_buff: array [0..128 * 16384 - 1]    of BYTE;
    big_buff: array [0..30 * MIB_BYTES - 1] of BYTE;


   function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;

  public

  end; // TPackerThread

  // один чанк сохраняемый в архив
  TArchiveChunk = class
  private
         m_files: TStrMap;
      m_hash_map: array [0..HASH_MAP_MASK] of TObjectList;
       m_folders: TStrMap;
   m_curr_offset: UInt64;
   m_align_bytes: DWORD;
    m_index_pass: Integer;
        m_header: TChunkHeader;
         m_index: TMemoryStream;
          m_data: TMemoryStream64;

   function         AddAlignChunk(from, target: UInt64; bWrite: Boolean = TRUE): DWORD;

   function         StoreOffset(af: TAsyncFile; af_index, osize: Integer; b_write: Boolean): UInt64;

  public
   m_base_offset: UInt64;

      Compressed: Boolean;
      IntOffsets: Boolean;

           Owner: TXrDB_Processor;

   property         ChunkID: DWORD   read m_header.id write m_header.id;
   property         Files: TStrMap read m_files;
   property         Folders: TStrMap read m_folders;

   { C & D }
   constructor      Create;
   destructor       Destroy; override;
   { methods }

   function         IndexFiles (base_offset: UInt64): UInt64;
   procedure        IndexFolders ();
   procedure        SaveData   (hDest: THandle);
   function         StoreIndex (bTest: Boolean; target_size: DWORD): DWORD;
  end;


  TXrDB_Processor = class (TWorkerThread)
  private
   root_path: String;

   FChunkList: TObjectList;


   function                   FindAddChunk(id: DWORD): TArchiveChunk;

   function                   AsyncLoadFiles(src: TStrMap; fill_tag: Boolean): UInt64;
   procedure                  LogMsg(const s: String);

   procedure                  PackDB;
   procedure                  UnpackDB;
   // function                   IndexFiles(fmap: TStrMap; sindex: TStream; const root: String): UInt64;
   procedure                  SetAllUnpacked (fmap: TStrMap);
   function                   ParseIndex(buff: PByteArray; cbSize: Integer; idx: TStrMap): UInt64;
   function                   VerifyFileName(fname: String): Boolean;
   function                   HandleWriteEvents: Integer;
   function                   HandleReadEvents(): Integer;
   function                   WriteFileSync(hFile: THandle; duOffset: Int64; const Buffer; uBytes: SIZE_T; var ovpd: OVERLAPPED): DWORD;
   function                   GetChunk(index: Integer): TArchiveChunk;
   function                   AllocateFile(chunk: TArchiveChunk): TAsyncFile;
  protected

        m_index: TMemoryStream;

     never_pack: TStrMap;

      min_ratio: Single;
   loaded_files: Integer;
   loaded_bytes: UInt64;
    saved_files: Integer;
    saved_space: Int64;
    total_bytes: UInt64;
    align_bytes: UInt64;
    clone_bytes: UInt64;
     tags_bytes: UInt64;
   packed_bytes: UInt64;
   writed_bytes: UInt64;
    target_size: UInt64;
        packers: array [0..15] of TPackerThread;
    last_packer: Integer;
        c_chunk: Boolean; // compressed chunk


     work_stage: CHAR;
      ovp_event: THandle;
             pt: TProfileTimer;



   procedure                  ProcessInit; override;
   function                   ProcessRequest (const rqs: String; rqobj: TObject): Integer; override;
   procedure                  ProcessThreadStop; override;

   procedure                  ProcessNewEvents;


  public
   Options, Source, Target: String;
               event_queue: TAsyncQueue;


   { methods }
   property                    Chunks[index: Integer]: TArchiveChunk read GetChunk;

   function                    AllocPacker: TPackerThread;


  end; // TXrDB_Processor


  TAllocMemProc = function (num, size: SIZE_T): Pointer; cdecl;
  TFreeMemProc  = procedure  (p: Pointer); cdecl;


  const
       HEADER_SIZE = sizeof (TIndexEntryHdr); // aligned to DWORD
   var
        files_pool: TObjectList = nil;

function LZ4_AllocMem (num, size: SIZE_T): Pointer; cdecl;
begin
 result := AllocMem (num * size);
end;

procedure LZ4_FreeMem (p: Pointer); cdecl;
begin
 FreeMem (p);
end;

procedure Write;
begin
 Assert(FALSE, 'Is trap');
end;

procedure UpdateVersion(const f_name: String);
var
   fini: TIniFile;

begin
 if Pos('db_version.ltx', f_name) > 0 then
  begin
   fini := TIniFile.Create(f_name);
   fini.WriteString('version', 'timestamp', FormatDateTime('dd.mm.yy-hh:nn', Now));
   fini.Free;
  end;
end;


function FileTimeDiff (const a, b: FILETIME): Int64;
var
   av: Int64 absolute a;
   bv: Int64 absolute b;
begin
 result := av - bv;
end;

function FileTimeRel (const a: FILETIME; diff: Int64): FILETIME;
var
   av: Int64 absolute a;
   rv: Int64 absolute result;
begin
 rv := av + diff;
end;

function  SetFilePtr (hDest: THandle; pos: UInt64 ): Boolean;
var
   new_pos: LARGE_INTEGER;
begin
 result := SetFilePointerEx (hDest, pos, @new_pos, FILE_BEGIN) and ( new_pos.QuadPart = pos );
end;


procedure FreeAndNil(var param);
var
   zero: Pointer;
    obj: TObject;
begin
 zero := nil;
 Move (param, obj,  sizeof(Pointer));
 Move (zero, param, sizeof(Pointer));
 try
  obj.Free;
 except
  on  E: Exception do
      OnExceptLog ('FreeAndNil', E);
 end;

end;


function RelPath (const path, root: String): String;
begin

 result := path;

 if result = '' then exit;

 if Pos( LowerCase(root), LowerCase(result) ) = 1 then
    Delete (result, 1, Length(root));

 while (Length (result) > 0) and ( CharInSet ( result[1], ['\', ':', '/', ','] ) ) do
        Delete (result, 1, 1);

end;

procedure DefMsgCB(szMsg: PWideChar; const pTime: PSystemTime); stdcall;
var
   dt: TDateTime;
    s: String;
begin
 if pTime <> nil then
    dt := SystemTimeToDateTime (pTime^)
 else
    dt := Now;

 s := FormatDateTime('[hh:nn:ss.zzz]', dt) + '. ' + szMsg;
 ODS(s);

end; // DefMsgCB

procedure DefPrgCB (cntFiles, busyFiles, cntBytes: Integer; fProgress: Single); stdcall;
begin


end;

procedure Seek (var ovpd: OVERLAPPED; position: UInt64); inline;
var
   li: LARGE_INTEGER;
begin
 li.QuadPart := position;
 ovpd.Offset := li.LowPart;
 ovpd.OffsetHigh := li.HighPart;
end;


{$IFNDEF CPUX64}

procedure LZHuff_inflate(var dst: PByteArray; var dstSize: DWORD; src: PByteArray; srcSize: DWORD); stdcall; external 'lzhuff.dll' name '?LZHuff_inflate@@YGXAAPAEAAIQAEI@Z';
procedure LZHuff_deflate(var dst: PByteArray; var dstSize: DWORD; src: PByteArray; srcSize: DWORD); stdcall; external 'lzhuff.dll' name '?LZHuff_deflate@@YGXAAPAEAAIQAEI@Z';

function  LZO_inflate(dst: PByteArray; var dstSize: DWORD; src: PByteArray; srcSize: DWORD; pTemp: Pointer): Integer; stdcall; external 'lzhuff.dll' name '?LZO_inflate@@YGHQAEPAK0IPAX@Z';
function  LZO_deflate(dst: PByteArray; var dstSize: DWORD; src: PByteArray; srcSize: DWORD; pTemp: Pointer): Integer; stdcall; external 'lzhuff.dll' name '?LZO_deflate@@YGHQAEPAK0IPAX@Z';


procedure LZHuff_ReleaseBuffer(buff: PByteArray); stdcall; external 'lzhuff.dll' name '?ReleaseBuffer@@YGXPAE@Z';
procedure LZHuff_SetMM (mm: PLZMemManager); stdcall; external 'lzhuff.dll' name '?SetMemoryManager@@YGXPAPAX@Z';
{$ENDIF}

// int LZ4_compress        (const char* source, char* dest, int sourceSize);
// int LZ4_decompress_safe (const char* source, char* dest, int compressedSize, int maxDecompressedSize);
function LZ4_compress        (const source: LPSTR; dest: LPSTR; sourceSize: Integer): Integer;                  cdecl; external LZ4_DLL;
function LZ4_decompress_safe (const source: LPSTR; dest: LPSTR; sourceSize, maxDecompSize: Integer): Integer;   cdecl; external LZ4_DLL;
function LZ4_compressBound    (size: Integer): Integer; cdecl; external LZ4_DLL;
procedure LZ4_SetMemCallbacks  (alloc_cb: TAllocMemProc; free_cb: TFreeMemProc); cdecl; external LZ4_DLL;


procedure TestCompression (src, ref: LPSTR; src_size, dst_size: DWORD);
var
  equal: Integer;
   temp: LPSTR;
    res: Integer;
      I: integer;
begin
 temp := AllocMem (src_size * 10);
 if StrLComp(src, 'LZ4F', 4) <> 0 then
    Assert(FALSE, ' wrong archive tag ' + src);
 Assert ( PDWORD(@src[4])^ = dst_size, ' wrong target size in header ');



 res := LZ4_decompress_safe ( @src[9], temp, src_size - 9, dst_size);

 if (res < src_size) then
   begin
    equal := 0;
    for i := 0 to dst_size - 1 do
     if temp[i] = ref[i] then
        Inc (equal);

    Assert (FALSE, Format('decomression failed with code %d, equal bytes = %d / %d ', [res, equal, dst_size]));
   end;
 FreeMem (temp);

end;

function LZ4_deflate (dst: PByteArray; var dstSize: DWORD; src: PByteArray; srcSize: SIZE_T): Integer;
begin
 SetStrZ ( LPSTR(dst), 'LZ4F', 5 );
 Move ( srcSize, dst[4], 4 );
 dst := @dst [9];

 result := LZ4_compress ( LPSTR(src), LPSTR(dst), srcSize );

 if result > 0 then
   begin
    dstSize := result + 9;
    result := 0;
   end
 else
    dstSize := 0;
end;



var
   gMsgCB: TMessageCallback = DefMsgCB;
   gPrgCB: TProgressCallback = DefPrgCB;
   gAlloc: Integer = 0;
    gLZMM: TLZMemManager;

function        XrDB_StartProcess(szAction, szSource, szTarget, szOptions: PWideChar): Pointer; stdcall;
const
   ALLOWED_ACTIONS: array [0..1] of String = ('PACK', 'UNPACK');
var
    xrp: TXrDB_Processor;
     sa: String;
      n: Integer;

begin
 sa := '';

 for n := 0 to High (ALLOWED_ACTIONS) do
     if Pos(ALLOWED_ACTIONS[n], szAction) > 0 then
        sa := ALLOWED_ACTIONS [n];

 result := nil;

 if sa = '' then exit;

 // creating thread

 xrp := TXrDB_Processor.Create(FALSE, 'XrUnpacker');
 xrp.FreeOnTerminate := TRUE;
 xrp.WaitStart();
 xrp.Source := szSource;
 xrp.Target := szTarget;
 xrp.Options := szOptions;

 xrp.AddRequest(sa);


 result := xrp;
end; // StartDecryption

function  XrDB_WaitComplete(xrp: TXrDB_Processor; dwMSec: DWORD): DWORD; stdcall;
begin
 result := xrp.WaitRequests(dwMsec);
 // WaitForSingleObject (xrp.Handle,
end;

function  XrDB_BreakProcess (xrp: TXrDB_Processor; bForce: Boolean): DWORD; stdcall;
var
   h: THandle;
begin
 h := xrp.Handle;
 xrp.StopThread();
 xrp.Terminate;
 result := xrp.WaitStop();
 if bForce and ( WAIT_TIMEOUT = WaitForSingleObject(h, 50) ) then TerminateThread (h, 0);

end;



procedure XrDB_SetCallback (nCB: Integer; pFunc: Pointer); stdcall;
begin
 case nCB of
  0: gMsgCB := pFunc;
  1: gPrgCB := pFunc;
 end
end; // SetInfoCallback

function TXrDB_Processor.VerifyFileName(fname: String): Boolean;
var
   i: Integer;
begin
 fname := RelPath (fname, root_path);
 result := TRUE;
 for i := 1 to Length(fname) do
    if BYTE(fname[i]) > $80 then
       begin
        result := FALSE;
        LogMsg (Format('[~T].~C0C #WARN:~C07 опознаны не латинские символы в файле %s, он будет проигнорирован', [fname]));
        break;
       end;
end;


function TXrDB_Processor.ParseIndex (buff: PByteArray; cbSize: Integer; idx: TStrMap): UInt64;

var
  ptr_ofs: DWORD;
 ptr_size: DWORD;
      cbn: DWORD;

       pp,
       pl: PByteArray;
       ps: PAnsiChar;
       sa: String;
       af: TAsyncFile;
       bf: TAsyncFile;
       ih: PIndexEntryHdr;
        n: Integer;
begin

 result := 0;

 pp := @buff[0];
 pl := @buff[cbSize - 18];

 try
   while ( NativeUInt(pp) < NativeUInt (pl) ) do
    begin
     af := TAsyncFile.Create;
     ih := PIndexEntryHdr (pp);

     cbn := ih.cbHdrSize - HEADER_SIZE + 2;           // 2 bytes

     if idx.Count > 25 then
      __nop;

     Assert( cbn < MAX_PATH, 'Strange name length ' + IntToStr(cbn) +
                             ', offset = ' + IntToStr (NativeUInt(pl) - NativeUInt(pp)) +
                             ', size = ' + IntToStr(cbSize) +
                             ', parsed = ' + IntToStr (idx.Count) );

     Assert (ih.flags <> 0, 'flags must be set');
     af.sz_real       := ih.cbReal;         // 4 bytes
     af.sz_compressed := ih.cbComp;
     af.crc           := ih.crc;
     af.upd_time      := ih.upd_time;

     pp := @pp [sizeof (TIndexEntryHdr)]; // после заголовка записи

     ptr_size := IfV(ih.flags and PF_OFFSET_64 = 0, 4, 8);

     if ih.flags and PF_OFFSET_FIRST = 0 then
       begin
         ps := LPSTR(pp);
         ptr_ofs := cbn;                    // после строки имени
       end
     else
       begin
         ptr_ofs := 0;
         ps := @pp [ptr_size];              // после смещения
       end;

     SetString (sa, ps, cbn);
     af.f_name := sa;

     if ptr_size = 4 then
        af.source_offset := PDWORD (@pp[ptr_ofs])^
     else
        af.source_offset := PUInt64(@pp[ptr_ofs])^;

     pp := @pp [cbn + ptr_size];             // пропуск имени и смещения

     af.b_packed := (af.sz_compressed < af.sz_real);
     if (af.source_offset > 0) and (af.sz_compressed > 0) and (af.source_offset < result) then
       for n := idx.Count - 1 downto 0 do
        begin
         bf := TAsyncFile ( idx.Objects [n] );
         if bf.source_offset < af.source_offset then break;

         if (bf.source_offset = af.source_offset) and (bf.crc <> af.crc) then
           begin
            af.Owner.LogMsg( Format('#WARN: CRC_DIFF = $%x, but same data offset for files:  '#13#10#9, [af.crc xor bf.crc]) + af.f_name + #13#10#9 + bf.f_name);
            break;
           end;
        end;
     // }

     idx.AddObject (af.f_name, af);
     Inc (result, af.sz_compressed);

     {
     if idx.IndexOf(af.f_name) < 0 then
       begin
       end
     else
       af.Free;
     }
    end; // while
 except
  on E: Exception do
    OnExceptLog ('ParseIndex', E);
 end;

end; // ParseIndex


function cmpFileOffset(a, b: Pointer): Integer;
var
   af, bf: TAsyncFile;
begin
 af := a;
 bf := b;
 result := 0;
 if af.target_offset = bf.target_offset then exit;
 result := IfV ( af.target_offset > bf.target_offset, -1, +1 );
end;

function XrPackProcess (L: lua_State): Integer; cdecl;
var
   act, src, dst: String;
            opts: String;
          pEvent: Pointer;
begin
 act := 'PACK';
 src := LuaStrArg(L, 1);
 dst := LuaStrArg(L, 2);
 opts := LuaStrArg(L, 3);
 pEvent := XrDB_StartProcess ( PChar(act), PChar(src), PChar(dst), PChar(opts));
 while (pEvent <> nil) and ( XrDB_WaitComplete (pEvent, 256000) <> WAIT_OBJECT_0 ) do
     asm
      nop
     end;

end;

function CaptureFunc (L: lua_State): Integer; cdecl;
begin
 // lua_register ( L, 'OpenXMLDoc',  OpenXMLDoc);
 LuaRegFunc (L, 'XrPackProcess', XrPackProcess, '(source_list, dest_file, [options]); // pack files into archive ', TRUE);
 result := 0;
end; // CaptureFunc

procedure LibInit ( pinf: PLibInitInfo ); stdcall;
begin
 // TODO: init procs
 Misc.LibInit (pinf);
end;

exports
    XrDB_StartProcess, XrDB_BreakProcess, XrDB_SetCallback, XrDB_WaitComplete, CaptureFunc, LibInit;


{ TXrDB_Processor }
function TXrDB_Processor.HandleReadEvents(): Integer;
var
   prs: TRequestSlot;
    af: TAsyncFile;
begin
 result := 0;
 while event_queue.Flushed > 0 do
   begin
    prs := event_queue.ReadRelease;
    af := TAsyncFile (prs.obj);
    if af = nil then continue;

    Assert ( Assigned(af), ' unassigned async-file!');
    // af := TAsyncFile ( fmap.Objects [n] );

    case af.busy of
     'U': begin
           if af.cb_readed <> af.sz_real then
              af.busy := 'E'                  // продолжение загрузки
           else
             if (af.sz_real = 0) then
               begin
                af.busy := 'C';
                af.crc := 0;
                Inc (result);
               end
             else
               begin
                if (af.sz_real >= 512) and (not af.Chunk.Compressed) and af.Compressible  then
                   af.AsyncDeflate ()
                else
                  begin
                   af.sz_compressed := af.sz_real;
                   af.busy := 'C';
                   af.crc := CRC32 ( DWORD(-1), @af.data[0], af.cb_readed );
                   af.CloseRead;
                   Inc (packed_bytes, af.cb_readed);
                   Inc (loaded_bytes, af.cb_readed);
                   Inc (loaded_files);
                  end;

               end;

          end;

      'c': begin // after pack
            Inc (loaded_bytes, af.cb_readed);
            Inc (packed_bytes, af.sz_compressed);
            Inc (loaded_files);
            af.busy := 'C';
            af.CloseRead;
           end;

      else
           begin
             LogMsg ('~C0C #WARN:~C07 unexpected busy state = ' + af.busy);
             af.busy := 'C';
           end;

    end; // case;

   end; // while

end;

function TXrDB_Processor.AsyncLoadFiles ( src: TStrMap; fill_tag: Boolean ): UInt64;
var

   szr, cnt, lcnt, n: Integer;
               flist: TStrMap;
                finf: BY_HANDLE_FILE_INFORMATION;
                  af: TAsyncFile;



begin
 result := 0;
 if src.Count = 0 then exit;
 work_stage := 'R';
 loaded_files := 0;
 loaded_bytes := 0;

 flist := TStrMap.Create(self);
 try
   flist.Assign(src);
   flist.Sort; // для ускорения загрузки в несколько раз (при адекватной дефрагментации диска)
   pt.StartOne(33);
   for n := 0 to flist.Count - 1 do
    begin
     af := TAsyncFile (flist.Objects[n]);
     af.pack_ratio := min_ratio;
     af.f_name := flist.AutoLine (n);
     af.FOwner := self;
     af.OpenRead(  );

     if fill_tag and GetFileInformationByHandle (af.hRead, finf) then
     with af.f_tag do
      begin
       crTime := finf.ftCreationTime;
       laTimeDiff := FileTimeDiff (finf.ftLastAccessTime, crTime);
       lwTimeDiff := FileTimeDiff (finf.ftLastWriteTime,  crTime);
       dwFileAttrs := finf.dwFileAttributes;
       dwSignature := SIGNATURE_TAGV0;
       Inc ( result, sizeof (TFileTagV0) );
      end;

     af.f_opts := flist.ValueFromIndex [n];

     af.source_offset := 0;
     af.op_log := af.op_log + 'BeginRead()'#13#10;
     af.BeginRead;
     if (n mod 1000 = 0) then
       begin
        event_queue.WaitForPush(1);
        lcnt := HandleReadEvents();
        gPrgCB (lcnt, loaded_files, loaded_bytes, 100 * loaded_files / flist.Count );
       end;
    end;


   szr := 0;

   repeat
    event_queue.WaitForPush(100);
    lcnt := HandleReadEvents();
    gPrgCB (lcnt, loaded_files, loaded_bytes, 100 * loaded_files / flist.Count );

    cnt := 0;
    for n := flist.Count - 1 downto 0 do
        begin
         af := TAsyncFile (flist.Objects[n]);
         if af.busy <> 'C' then
            Inc (cnt);
        end;

     if cnt = 0 then break;
   until FALSE; // waiting loop


   for n := flist.Count - 1 downto 0 do
      begin
       af := TAsyncFile (flist.Objects[n]);
       flist[n] := Format('%-200s %.3f', [flist[n], af.sz_real / af.sz_compressed]);
      end;

 finally
  flist.SaveToFile('loaded_files.lst');
  flist.Free;
 end;


end;


function TXrDB_Processor.GetChunk(index: Integer): TArchiveChunk;
begin
 result := TArchiveChunk (FChunkList[index]);
end;




procedure SaveRaw (const sFileName: String; pbuff: Pointer; uBytes: SIZE_T);
var
   fdst: THandle;
     wb: DWORD;
begin
 fdst := CreateFile ( PChar (sFileName), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, 0, 0 );
 WriteFile (fdst, pbuff^, uBytes, wb, nil);
 CloseHandle (fdst);
end;


function TXrDB_Processor.WriteFileSync (hFile: THandle; duOffset: Int64; const Buffer; uBytes: SIZE_T; var ovpd: OVERLAPPED): DWORD;
var
   wb: DWORD;
   li: LARGE_INTEGER;
   sv: THandle;
begin
 // ---
 result := 0;
 if duOffset < 0 then
   li.LowPart :=  GetFileSize (hFile, @li.HighPart)
 else
   li.QuadPart := duOffset;

 ovpd.Offset := li.LowPart;
 ovpd.OffsetHigh := li.HighPart;

 sv := ovpd.hEvent;
 if sv > $10000 then
    ovpd.hEvent := ovp_event;
 WriteFile (hFile, Buffer, uBytes, wb, @ovpd);
 if GetOverlappedResult (hFile, ovpd, wb, TRUE) then
    result := wb;
 ovpd.hEvent := sv;
end;


procedure TXrDB_Processor.ProcessInit;
begin
  event_queue := TAsyncQueue.Create('loaded');
  FChunkList := TObjectList.Create (TRUE);
  pt := TProfileTimer.Create;
  inherited;

end;

procedure TXrDB_Processor.ProcessNewEvents;
begin
 case work_stage of
  'R': HandleReadEvents;
  'W': HandleWriteEvents;
 end;

end;

function TXrDB_Processor.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
begin
 if rqs = 'PACK' then PackDB;
 if rqs = 'UNPACK' then UnpackDB;
 result := inherited ProcessRequest (rqs, rqobj);
end;


procedure TXrDB_Processor.ProcessThreadStop;
var
   n: Integer;
begin
  // command for stop
  for n := 0 to High (packers) do
    if packers [n] <> nil then
       packers [n].StopThread()
    else
       break;

  // wait and release
  for n := 0 to High (packers) do
    if packers [n] <> nil then
      begin
       packers [n].WaitStop();
       FreeAndNil (packers [n]);
      end
    else
       break;

  FreeAndNil (event_queue);
  FreeAndNil (pt);

  inherited;
end;

procedure TXrDB_Processor.SetAllUnpacked(fmap: TStrMap);
var
    af: TAsyncFile;
     n: Integer;
begin
 for n := 0 to fmap.Count - 1 do
  begin
   af := TAsyncFile ( fmap.Objects [n] );
   af.save_packed := FALSE;
   af.sz_compressed := af.cb_readed;
   af.target_offset := MAXINT;
   af.FreeData('P');
  end;
end;

function TXrDB_Processor.FindAddChunk(id: DWORD): TArchiveChunk;
var
   i: Integer;
begin
 for i := FChunkList.Count -1 downto 0 do
  if Chunks[i].ChunkID and $FFFF = id and $FFFF then
    begin
     result := Chunks[i];
     result.ChunkID := id; // update flags
      result.Compressed := (id and CHUNK_COMPRESSED <> 0);
     exit;
    end;

 result := TArchiveChunk.Create;
 result.ChunkID := id;
 result.Owner := self;
 result.m_index := m_index;
 FChunkList.Add(result);
end;

function TXrDB_Processor.AllocPacker: TPackerThread;
var
   can_add, i: Integer;
        count: Integer;
          ptw: TPackerThread;
begin
 result := nil;
 count := Min (GetCPUCount, High(packers) + 1);
 can_add := 0;

 for i := 0 to count - 1 do
   if packers [i] <> nil then
     begin
      ptw := packers[i];
      if result = nil then
         result := ptw
      else
       if result.RQCount > ptw.RQCount then
          result := ptw;

     end
   else
     Inc(can_add);

 if (result = nil) or ((result.RQCount > 0) and (can_add > 0)) then
  for i := 0 to Count - 1 do
  if packers[i] = nil then
   begin
    result := TPackerThread.Create(FALSE, 'packer#' + IntToStr(last_packer));
    result.WaitStart();
    packers[i] := result;
    break;
   end;
end;

procedure TXrDB_Processor.LogMsg(const s: String);
var
   st: TSystemTime;
begin
 GetLocalTime (st);
 gMsgCB( @s[1], @st );
end;

function TXrDB_Processor.HandleWriteEvents;
var
   prs: TRequestSlot;
    af: TAsyncFile;
     n: Integer;
begin
  result := 0;

  while event_queue.Flushed > 0 do
  begin
   prs := event_queue.ReadRelease;
   if (prs.obj = nil) then continue;
   af := TAsyncFile ( prs.obj );
   Assert ( UpperCase(af.busy) <> 'C' );
   if not CharInSet ( af.busy, ['D', 'E'] ) then continue;
   Inc ( result );
  end;

end;

const
    DUMP_DELTA = 64 * MEBIBYTE;

function  TXrDB_Processor.AllocateFile(chunk: TArchiveChunk): TAsyncFile;
begin
 if files_pool.Count > 0 then
    result := PopValue (files_pool) as TAsyncFile
 else
    result := TAsyncFile.Create();

 result.Chunk := chunk;
end;

procedure TXrDB_Processor.PackDB;
var
   optlst: TStrMap;

   poffset: PNativeUInt;
   idx_adr: NativeUInt;
   ofs_ref: DWORD;
     hDest: THandle;
      size: DWORD;
       wcb: DWORD;

  indx_pos: Integer;
  fn, path: String;

   i_chunk: TArchiveChunk;
   d_chunk: TArchiveChunk;
    mfiles: TStrMap;
    scramb: TXrScrambler;
      buff: PByteArray;



   tm_tags: Boolean;
    is_dir: Boolean;
      serr: String;
      icnt: Integer;
      wcnt: Integer;
      bcnt: Integer;

       ovpd: OVERLAPPED;
       chkh: TChunkHeader;
         af: TAsyncFile;
         bf: TAsyncFile;
         li: LARGE_INTEGER;
      entry: TIndexEntryHdr;

   al_index: DWORD; // index align

   data_ofs: UInt64;
   cn, i, n: Integer;

   // fd: TFileStream;

begin
 // loading file list from text file
 if not FileExists(Source) then
  begin
   LogMsg('#FATAL_ERROR: File not found ' + Source);
   exit;
  end;

 {
   Вариации упаковки файлов в базу данных:
    * Заданный порядок и сортировка в индексе (распаковка сторонними решениями будет идти "долго").
    * Заданный порядок и соответствие в индексе
    * Сохранение индекса в первом чанке
    * Сохранение индекса в последнем чанке
    * Шифрование заголовка или прямой текст.

 }
 Priority := tpHigher;

 m_index := TMemoryStream.Create();

 FChunkList.Create;
 i_chunk := FindAddChunk (DB_CHUNK_INDEX);
 d_chunk := FindAddChunk (DB_CHUNK_DATA);


 never_pack := TStrMap.Create(self);
 never_pack.Add('level.geom');
 mfiles := TStrMap.Create();
 optlst := TStrMap.Create();

 scramb := TXrScrambler.Create();;
 hDest := CreateFile ( PChar (Target), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_FLAG_SEQUENTIAL_SCAN, 0 );
 FillChar (ovpd, sizeof(ovpd), 0);
 // dump := TFileStream.Create ( Target, fmCreate, fmShareDenyWrite );
 ovp_event := CreateEvent (nil, FALSE, FALSE, nil);
 ovpd.hEvent := ovp_event;

 // fd := TFileStream.Create(Target, fmCreate);

 try
  optlst.Split(',', Options);
  LogMsg('#DBG: Loading files list from ' + Source + '/~B/'#13#10);
  mfiles.Clear;
  mfiles.Duplicates := dupIgnore;
  mfiles.LoadFromFile(Source);
  for i := mfiles.Count - 1 downto 0 do
    if Trim(mfiles[i]) = '' then mfiles.Delete(i);

  if mfiles.count = 0 then
    begin
     LogMsg('#ERROR: no lines in file ' + Source);
     exit;
    end;

  mfiles.SaveToFile('source_nodup.lst');
  mfiles.Sort;
  mfiles.SaveToFile('source_sorted.lst');

  root_path := UnhideSP (optlst.Values ['root']);

  indx_pos  := optlst.IntValues['index_pos'];

  min_ratio := optlst.FloatValues['min_pack_ratio'];

  if min_ratio <= 1 then
     min_ratio := 1.75;


  tm_tags := ( optlst.IntValues['no_tags'] = 0 ); // добавлять временные теги к каждому телу файла

  if root_path = '' then
   for cn := mfiles.Count - 1 downto 0 do
    begin
     fn := mfiles.AutoLine(cn);
     fn := UnhideSP(fn);
     n := Pos ('gamedata\', LowerCase(fn));
     if n = 0 then continue;
     root_path := Copy (fn, 1, n + 8);
     break;
    end;


  root_path := LowerCase (root_path);



  cn := DB_CHUNK_DATA;

  LogMsg( Format('#DBG: %d entries found '#13#10, [mfiles.Count] ) );
  LogMsg('#DBG: Splitting folders and files to chunks /~B/'#13#10);
  // получение списка каталогов и сепарация файлов по чанкам(!)
  mfiles.Reverse; // для ускоренного удаления
  icnt := 0;

  for n := mfiles.Count - 1 downto 0 do
   begin
    if n mod 1000 = 0 then
       LogMsg(Format('#DBG: Rest for check %5d entries                                  .'#13, [n]));

    fn := mfiles.AutoLine(n);
    if (fn = '') or ( Pos('.svn', fn) > 0 ) then
      begin
       Inc (icnt);
       mfiles.Delete(n);
       continue;
      end;

    fn := UnhideSP(fn);


    if fn = 'CHUNK_ID' then
      begin
       cn := mfiles.IntValues['CHUNK_ID'];
       d_chunk := FindAddChunk(cn);
       Inc (icnt);
       mfiles.Delete (n);
       continue;
      end;



    is_dir := DirectoryExists (fn);
    if ( not is_dir ) and ( not FileExists (fn) ) then
       begin
        Inc (icnt);
        LogMsg(#10'~C0C #ERROR:~C07 not existed ' + fn);
        mfiles.Delete(n);
        continue;
       end;

    fn := LowerCase(fn);
    path := ExtractFilePath (fn);


    if ( Pos(root_path, fn) = 1 ) {and VerifyFileName(fn)} then
      begin
       path := RelPath (path, root_path);
       if ( path <> '' ) and ( d_chunk.Folders.IndexOf (path) < 0 ) then
            d_chunk.Folders.Add (path);
       if is_dir then
          mfiles.Delete(n) // not a file
       else
         begin
          af := AllocateFile (d_chunk);
          mfiles.Objects [n] := af;
          d_chunk.Files.AddObject (mfiles[n], af);
         end;

      end
    else
     begin
      Inc (icnt);
      LogMsg('~C0C #WARN:~C07 restricted path ' + path);
      mfiles.Delete(n); // outside root
     end;

   end; // for

  // TODO: save times tags
  LogMsg (#10'#DBG: Ignored entries count = ' + IntToStr(icnt));

  LogMsg (#10'#DBG: Packing stage 1. Loading contents... /~B/'#10 );
  packed_bytes := 0;
  AsyncLoadFiles ( mfiles, tm_tags );
  LogMsg( Format(#10'#DBG: loaded %3.f MiB in %d files ', [loaded_bytes / MEBIBYTE, mfiles.Count]) );

  // теперь осталось запаковать, зашифровать и выбросить...
  // scramb.encrypt(pindex, pindex, p_size); // хорошенько перемещать
  // Sleep(500);

  pt.StartOne(33);
  work_stage := 'W';
  data_ofs := 0;
  ofs_ref := 0;
  n := 0;
  LogMsg ('#DBG: Packing stage 2. Indexing files /~B/ ===============================================                           '#10 );

  al_index := (mfiles.Count div 500) * 32 + 32;
  if mfiles.Count < 10 then
     al_index := al_index + 2048;
  Repeat
    if ofs_ref <= al_index then
      begin
       Inc (data_ofs, al_index); // флуктуация упакованного размера индекса растет при большом числе файлов
       while data_ofs and $F <> 8 do Dec(data_ofs);
       ofs_ref := data_ofs; // целевой размер заголовка архива
      end
    else
       data_ofs := ofs_ref;

    m_index.Clear;
    // индексирование файлов собирает индекс в один блок
    for cn := 1 to FChunkList.Count - 1 do
    begin
     d_chunk := Chunks [cn];
     // после индексации чанк выдает свой ожидаемый размер
     d_chunk.IndexFolders;
     Inc (data_ofs, d_chunk.IndexFiles (data_ofs));
    end;

    Assert (m_index.Size > 8, 'Index is void');
    target_size := data_ofs;
    data_ofs := i_chunk.StoreIndex (TRUE, ofs_ref); // оценить размер сжатого индекса
    if data_ofs and $F <> 8 then
       __nop;

    Inc (n);
  Until (data_ofs = ofs_ref) or (n > 4); // продолжать вплоть до максимального приближения )

  Assert (n < 5, 'Cannot glue chunks');

  i_chunk.StoreIndex(FALSE, ofs_ref);         // сохранить сжатый индекс в потоке

  LogMsg ('#DBG: Packing stage 3. Writing contents... /~B/ ===============================================                           '#10 );


  for cn := 0 to FChunkList.Count - 1 do
  begin
   d_chunk := Chunks[cn];
   LogMsg(#10' #DBG: dumping data chunk $' + IntToHex(d_chunk.ChunkID, 8) + '          '#10);
   d_chunk.SaveData(hDest);
  end;


 finally

  CloseHandle (hDest);
  hDest := 0;

  LogMsg('~CF0 #OK: packing complete -------------------------------- <<< ---------------------------- /~B/ saved_space =~CFD ' +
                                        ftow(saved_space / 1048576, '%.3f MiB ~C07' ));

  optlst.Free;
  mfiles.Free;

  FreeAndNil (m_index);
  scramb.Free;
 end;

end;

procedure TXrDB_Processor.UnpackDB;
// unpack on file to directory
var
  tm_tags: Boolean;
   chunks: TList;
   mfiles: TStrMap;
   scramb: TXrScrambler;

    fsize: LARGE_INTEGER;
    usize: DWORD;
    ubuff: PByteArray;
     fpos: LARGE_INTEGER;
     chkh: TChunkHeader;
     hdrx: PChunkRef;
     chkr: PChunkRef;
     fsrc: THandle;
     tail: DWORD;
     buff: PByteArray;
     ovpd: OVERLAPPED;
       af: TAsyncFile;
       rb: DWORD;
     cnt, i, n: Integer;


begin
 if not FileExists(Source) then
   begin
    LogMsg('#FATAL_ERROR: File not found ' + Source);
    exit;
   end;

 chunks := TList.Create;
 mfiles := TStrMap.Create;
 scramb := TXrScrambler.Create(); // TODO: from options detect format


 fsrc := INVALID_HANDLE_VALUE;

 tm_tags := FALSE;

 Priority := tpHigher;

 try
  fsrc := CreateFile ( PChar(Source), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_FLAG_OVERLAPPED, 0 );
  if (fsrc = 0) or (fsrc = INVALID_HANDLE_VALUE) then
    begin
     LogMsg('#SYSTEM_ERROR: Cannot open file for read ' + Source + '. Details: ' + Err2Str(GetLastError) );
     exit;
    end;

  fsize.LowPart := GetFileSize(fsrc, @fsize.HighPart);
  fpos.QuadPart := 0;


  LogMsg('[~B]. #DBG: Stage 1. Opening database ========================================================= ');


  FillChar (ovpd, sizeof(ovpd), 0);

  ovpd.hEvent := CreateEvent (nil, FALSE, FALSE, nil);

  hdrx := nil;
  // parsing the chunks
  repeat
   ovpd.Offset := fpos.LowPart;
   ovpd.OffsetHigh := fpos.HighPart;
   ReadFile (fsrc, chkh, sizeof(TChunkHeader), rb, @ovpd); // sync read small portion - only chunk header

   // WaitForSingleObject (ovpd.hEvent, 30000) = WAIT_TIMEOUT
   if not GetOverlappedResult (fsrc, ovpd, rb, TRUE) then
     begin
      LogMsg('#SYSTEM_ERROR: overlapped file read failed. Details: ' + Err2Str(GetLastError) );
      break;
     end;

   if rb = sizeof(chkh) then
     begin
      GetMem (chkr, sizeof(TChunkRef));
      chunks.Add(chkr);

      chkr.id := chkh.id;
      chkr.size := chkh.size;

      Inc (fpos.QuadPart, sizeof(TChunkHeader));

      chkr.offset := fpos;
      chkr.pData := nil;

      if chkr.id and DB_CHUNK_TAGGED <> 0 then
         tm_tags := TRUE;


      Inc (fpos.QuadPart, chkh.size); // goto next chunk

      if ( chkr.id and DB_CHUNK_INDEX <> 0 ) then hdrx := chkr;

     end
   else
     break; // if

  until ( fpos.QuadPart >= fsize.QuadPart );

  // last chunk must be header (default)
  chkr := hdrx;

  if ( chkr <> nil ) and ( chkr.size > 0 ) then
    begin
     // loading and decrypting the header
     GetMem (buff, chkr.size);

     // SetFilePointer (fsrc, fpos.LowPart, @fpos.HighPart, FILE_BEGIN);
     ovpd.Offset := chkr.offset.LowPart;
     ovpd.OffsetHigh := chkr.offset.HighPart;

     ReadFile (fsrc, buff^, chkr.size, rb, @ovpd);

     if not GetOverlappedResult (fsrc, ovpd, rb, TRUE) then
       begin
        LogMsg('#SYSTEM_ERROR: overlapped file read failed. Details: ' + Err2Str(GetLastError) );
        exit;
       end;


     if rb <> chkr.size then
       begin
        LogMsg('#SYSTEM_ERROR: Cannot read header chunk. Details: ' + Err2Str(GetLastError) );
        exit;
       end;


     scramb.decrypt(buff, buff, rb);
     // unpack
     usize := 0;
     ubuff := nil;

     // n := PDWORD(buff)^;
     // LogMsg('#DBG: Unpacking ' + IntToStr( n ) + ' bytes from ' + IntToStr(rb) + ' byte stream');
     // data, real_size, code, compressed_size
     if chkr.id and CHUNK_COMPRESSED <> 0 then
       begin
         {$IFDEF LZO_PACK}
         LZHuff_inflate (ubuff, usize, buff, rb);
         {$ENDIF}


         if usize <= 0 then
           begin
            LogMsg('#LIB_ERROR: LZHuff_inflate failed.');
            exit;
           end;


         SaveRaw ( Source + '.idx', ubuff, usize );
         // parsing the index
         total_bytes := ParseIndex (ubuff, usize, mfiles);
         {$IFDEF LZO_PACK}
         LZHuff_ReleaseBuffer(ubuff);
         {$ENDIF}
        end
      else
         total_bytes := ParseIndex (buff, rb, mfiles);

     // reading database and saving the files
     if Pos(':', Target) = 0 then
        Target := ExePath + Target;

     Target := CorrectFilePath (Target);
     Target := AddSlash (Target);

     pt.StartOne (33);



     saved_files := 0;
     writed_bytes := 0;

     LogMsg('[~B]. #DBG: Stage 2. Dumping files ========================================================= ');

     tail := IfV (tm_tags, sizeof(TFileTagV0), 0);

     if total_bytes > 0 then
       for n := 0 to mfiles.Count - 1 do
        begin
         af := TAsyncFile (mfiles.Objects[n]);
         af.FOwner := self;
         af.f_name := Target + af.f_name;
         af.hRead := fsrc;
         af.auto_write := TRUE;


         if Terminated then break;

         if af.source_offset > 0 then
            af.BeginRead (tail)
         else
            CheckMakeDir (af.f_name);


         while (gAlloc > 64 * MIB_BYTES) do
           begin
             // SleepEx (100, TRUE);
             if pt.Elapsed(33) > 250 then
               begin

                cnt := 0;
                for i := 0 to n do
                  begin
                   af := TAsyncFile (mfiles.Objects[n]);
                   if af.data <> nil then Inc (cnt);
                  end;

                gPrgCB (saved_files, cnt, writed_bytes, writed_bytes * 100.0 / total_bytes ); // TODO: progress calc
                pt.StartOne(33);
               end;
           end; // while

        end;


     /// waiting for complete

     mfiles.OwnsObjects := TRUE;
     repeat

      for n := mfiles.Count - 1 downto 0 do
        begin
         af := TAsyncFile (mfiles.Objects[n]);
         if (af.data = nil) and (af.busy <> 'W') then
            mfiles.Delete (n);
        end;

      cnt := mfiles.Count;

      if cnt > 0 then SleepEx (250, TRUE);

      gPrgCB (saved_files, cnt, writed_bytes, writed_bytes * 100.0 / total_bytes); // TODO: progress calc


     until (cnt = 0) or (Terminated);

     gPrgCB (saved_files, 0, writed_bytes, 100.001); // TODO: progress calc

     LogMsg('[~B]. #DBG: Expand complete ---------------------------------- <<<<<<< -------------');

     {
   }

    end;



 finally

  if (fsrc <> INVALID_HANDLE_VALUE) then CloseHandle (fsrc);
  scramb.Free;
  chunks.Free;
  mfiles.Free;

  if ovp_event <> 0 then CloseHandle (ovp_event);

  Priority := tpNormal;
 end;

end;




procedure OnFileReadComplete(dwError, dwBytes: DWORD; lpOverlapped: POVERLAPPED); stdcall;
var
   pf: TAsyncFile;
begin
 pf := Pointer (lpOverlapped.hEvent); // this member ignored by ReadFileEx


 if dwBytes > 0 then
   begin
    pf.busy := 'U';
    pf.cb_readed := dwBytes;
    pf.rcrc := CRC32( DWORD(-1), PByte(pf.data), IfV (pf.b_packed, pf.sz_compressed, pf.sz_real) );

    if Pos ('lua_help.script', pf.f_name) > 0 then
       __nop;


    if pf.auto_write then pf.BeginWrite (TRUE);
    pf.SendEvent;
   end
 else
   begin
    pf.Owner.LogMsg('#SYSTEM_ERROR: IOCompletion read failed. Details: ' + Err2Str(dwError));
    pf.busy := 'E';
    pf.SendEvent;
   end;
end;

procedure OnFileWriteComplete(dwError, dwBytes: DWORD; lpOverlapped: POVERLAPPED); stdcall;
var
  _crc: DWORD;
   szf: DWORD;
    pf: TAsyncFile;
    pd: PByteArray;



begin
 if lpOverlapped = nil then exit;


 pf := Pointer (lpOverlapped.hEvent); // this member ignored by ReadFileEx

 if (dwBytes > 0) and ( lpOverlapped.Offset = pf.target_offset ) then
   begin
    Inc (pf.Owner.writed_bytes, dwBytes);
    // если идет обработка записи единичного файла (вывод из архива)
    if pf.target_offset = 0 then
      begin
       Inc (pf.Owner.saved_files, 1);

       szf := IfV (pf.b_packed, pf.sz_compressed, pf.sz_real); // size
       pd  := @pf.data[0]; // why loaded from source

       if pf.rcrc <> pf.crc then
           pf.Owner.LogMsg( Format('#WARN_CRC: CRC = $%x (estimated $%x), packed = %d, for file %s ', [pf.rcrc, pf.crc, Ord(pf.b_packed), pf.f_name] ) );


       if ( pf.cb_readed > szf ) then
         begin
          // Move ( pd[szf], pf.f_tag, sizeof(TFileTagV0) );
          // pf.UpdateFromTag;
         end;
      end;

    pf.FreeData;
    pf.OnWriteComplete;
   end
 else
   begin
    pf.Owner.LogMsg('#SYSTEM_ERROR: IOCompletion write failed. Details: ' + Err2Str(dwError));
    pf.busy := 'E';
    pf.BeginWrite(TRUE);
    pf.SendEvent;
   end;
end;


{ TPackedFile }

procedure TAsyncFile.AsyncDeflate;
var
   pkr: TPackerThread;
begin
 busy := 'P';
 pkr := Owner.AllocPacker;
 pkr.AddRequest ('TRY_DEFLATE', self);
 // if pkr.RQCount > 50 then pkr.WaitRequests(900);
end;

procedure TAsyncFile.BeginRead(extra: DWORD);
var
   szr: SIZE_T;
   err: DWORD;
begin


 if (hRead = 0) or (hRead = INVALID_HANDLE_VALUE) then
    begin
     busy := 'E';
     SendEvent;
     exit;
    end;

 szr := IfV (b_packed, sz_compressed, sz_real) + extra;

 if szr = 0 then
   begin
    busy := 'U';
    SendEvent;
   end;


 if (szr > 0) and (busy <> 'R') then
   begin
    SetLength (data, szr);
    FillChar  (data [0], szr, 0);
    InterlockedAdd (gAlloc, szr);
    FillChar (ovpd, sizeof(ovpd), 0);
    ovpd.hEvent := DWORD (self);
    ovpd.Offset := source_offset;
    SetLastError(0);
    if ReadFileEx (hRead, data, szr, @ovpd, @OnFileReadComplete) then
       busy := 'R'
    else
     begin
      busy := 'E';
      err := GetLastError();
      Owner.LogMsg('#SYSTEM_ERROR: ReadFileEx returned ' + Err2Str(err));
     end;

   end
else
   if auto_write then BeginWrite (TRUE);   // save void file sync

end;

procedure TAsyncFile.BeginWrite(bAsync: Boolean);
var
     path: String;
    proxy: PByteArray;
    extra: DWORD;
    wsize: DWORD;
     temp: Pointer;
     _crc: DWORD;
      res: Integer;
      wbc: DWORD;
begin
 if busy = 'W' then exit;

 busy := 'W';

 path := ExtractFilePath (f_name);
 CheckMakeDir (path);

 if hWrite = 0 then // save single file ?
    hWrite := CreateFile ( PChar (f_name), GENERIC_WRITE, FILE_SHARE_READ, nil, CREATE_ALWAYS, FILE_FLAG_OVERLAPPED, 0);

 if hWrite = INVALID_HANDLE_VALUE then
   begin
    Owner.LogMsg('#SYSTEM_ERROR: Cannot create file ' + f_name + '. Details:' + Err2Str (GetLastError) );
    FreeData;
    exit;
   end;

 if sz_compressed = 0 then
   begin
    InterlockedAdd (Owner.saved_files, 1);
    FreeData;
    exit;
   end;

 ovpd.hEvent := DWORD (self);
 ovpd.Offset      := target_offset;
 ovpd.OffsetHigh  := target_offset shr 32;

 // Assert ( sz_compressed = cbSize, Format('Read only %d from %d bytes', [cbSize, sz_compressed]) );

 if (sz_real <> sz_compressed) and (not save_packed) and ( target_offset = 0 ) then // only if saving to file
  begin
   // предварительно распаковать
   SetLength (udata, sz_real or $FF + 1);

   // GetMem (temp, $10000 * 32);
   temp := nil;
   res := 0;
   {$IFDEF LZO_PACK}
   res := LZO_inflate (udata, sz_real, data, sz_compressed, temp);
   {$ENDIF}
   Inc (udata_writes);

   if ( sz_real > 0 ) and ( res = LZO_E_OK ) then
     begin
      InterlockedAdd (gAlloc, sz_real);
      WriteFileEx ( hWrite, udata, sz_real, ovpd, @OnFileWriteComplete );
     end
    else
      busy := 'E'; // if

  end
 else
  begin
   proxy := IfV (save_packed, udata, data);
   wsize := IfV (save_packed, sz_compressed, sz_real);

   extra := 0;

   Assert (save_packed = b_packed, 'save_packed <> b_packed');




   _crc := CRC32 ( DWORD(-1), PByte(proxy), wsize );

   if target_offset > 0 then // only if saving to database
    begin
     extra := IfV (Tagged, sizeof(TFileTagV0), 0);
     Move (f_tag, proxy[wsize], extra); // DANGEROUS if mem-block limited
    end;



   if (_crc <> crc) then
       Owner.LogMsg ('~C0C #ERROR:~C07 Wrong data CRC for ' + f_name +
                        Format (#13#10#9#9'  $%x <> $%x, size = %d, clone = %d, writes = %d, packed = %d', [_crc, crc, wsize, Ord(is_clone), udata_writes, Ord(b_packed)]));


   if bAsync then
      WriteFileEx ( hWrite, proxy, wsize + extra, ovpd, @OnFileWriteComplete )
   else
     begin
      wbc := 0;
      wbc := owner.WriteFileSync ( hWrite, target_offset, proxy^, wsize + extra, ovpd );
      if wbc > 0 then
         OnFileWriteComplete (0, wbc, @ovpd)
      else
         OnFileWriteComplete ( GetLastError, wbc, @ovpd );

     end;
  end;



 // if Tagged and ( target_offset > 0 ) then



end;

procedure TAsyncFile.CloseRead;
begin
 CloseHandle (hRead);
 hRead := 0;
end;

function TAsyncFile.Compressible: Boolean;
var
   ext: String;
begin
 result := TRUE;
 ext := ExtractFileExt (f_name);
 if ( Pos('.ogg', ext) + Pos('.ogm', ext) > 0 ) or
    ( Owner.never_pack.IndexOf( ExtractFileName(f_name) ) >= 0 ) then
      result := FALSE;

end;

constructor TAsyncFile.Create;
begin
 busy := 'I'; // idle mode
 pack_ratio := 2.5;
end;

destructor TAsyncFile.Destroy;
begin
 Assert ( busy <> 'W', ' deleting async-file while write operation ');

 FreeData;
 inherited;
end;

procedure TAsyncFile.FreeData;
begin
 if ( data <> nil ) and ( Pos ('D', flt) > 0 ) then
  begin
   op_log := op_log + 'Release(data)'#13#10;
   InterlockedAdd (gAlloc, -Length(data));
   SetLength (data, 0);
  end;

 if ( udata <> nil ) and ( Pos ('D', flt) > 0 ) then
  begin
   op_log := op_log + 'Release(udata)'#13#10;
   InterlockedAdd (gAlloc, -Length(udata));
   SetLength (udata, 0);
  end;

 if ( target_offset = 0 ) and ( Pos ('H', flt) > 0 ) then
  begin
   op_log := op_log + 'Release(hWrite)'#13#10;
   CloseHandle (hWrite);
   hWrite := 0;
  end;
end; // FreeData

procedure TAsyncFile.OnWriteComplete;
var
   n: Integer;
begin
 busy := 'D';
end;


procedure TAsyncFile.OpenRead();
var
     li: LARGE_INTEGER;
begin
 UpdateVersion(f_name);

 hRead := CreateFile ( PChar(f_name), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_ALWAYS, FILE_FLAG_OVERLAPPED, 0 );

 if hRead <> INVALID_HANDLE_VALUE then
   begin
    li.LowPart := GetFileSize (hRead, @li.HighPart);
    sz_real := li.QuadPart;
    sz_compressed := sz_real;
   end;
end; // OpenRead

procedure TAsyncFile.PutData(dst: CHAR; const buff; uBytes: DWORD);
begin
 case dst of
  'U': begin
        if udata = nil then
          begin
           udata_writes := 0;
           SetLength (udata, uBytes or $FF + 1);
           Inc (gAlloc, Length(udata));
          end;

        Move (buff, udata[0], uBytes);
        Inc (udata_writes);
       end;
  else
       begin
        Assert (Length (data) >= Integer(uBytes));
        Move (buff, data, uBytes);
       end;
 end;
end;




procedure TAsyncFile.Reset;
begin
 FreeData;
 sz_compressed := 0;
 sz_real       := 0;
 is_clone := FALSE;
 cloned_from := nil;
end;

procedure TAsyncFile.SendEvent;
begin
 with Owner do
  begin
   if ( event_queue.Flushed >= 1000 ) then
        ProcessNewEvents();

   event_queue.PushSimpleRqs('EVENT_' + self.busy, self);
  end;
end;

function TAsyncFile.Tagged: Boolean;
begin
 result := ( f_tag.dwSignature = SIGNATURE_TAGV0 );
end;

procedure TAsyncFile.TryDeflate;
var
   rpack: Single;

     res: Integer;
begin
 //  exit;
 Assert (Assigned(data), 'data unassigned!');

 if (sz_real < 1024) or (sz_real > big_temp_size) or (data = nil) or (pack_ratio >= 1000) then exit;


 sz_compressed := 0;

 try
  sz_compressed := sz_real;

  FillChar (big_temp^, sz_real, 0);

{$IFDEF LZO_PACK}

  af.op_log := af.op_log + 'LZO_deflate()'#13#10;
  res := LZO_deflate (big_temp, sz_compressed, data, sz_real, temp);

  if res <> LZO_E_OK then
   begin
    Owner.LogMsg ('#LZO_ERROR: deflate file ' + f_name + ' failed with error ' + IntToStr (res));
    udata_size := 0;
    FreeData('P');
    sz_compressed  := sz_real;
   end;
{$ELSE}
  op_log := op_log + 'LZ4_deflate()'#13#10;
  res := LZ4_deflate (big_temp, sz_compressed, @data[0], sz_real);
  if res <> 0 then
   begin
    Owner.LogMsg ('#LZ4_ERROR: deflate file ' + f_name + ' failed with error ' + IntToStr (res));
    FreeData ('P');
    sz_compressed  := sz_real;
   end;

{$ENDIF}



 except
  on E: Exception do
     Owner.LogMsg( Format('#EXCEPTION: LZHuff_Deflate for %d bytes data of %s. Details: %s', [sz_real, f_name, E.Message] ) );
 end;

 if sz_compressed = 0 then
   begin
    sz_compressed := sz_real;
    exit;
   end;

 rpack := sz_real / sz_compressed;

 if rpack >= pack_ratio then
  try
   Inc (Owner.saved_space, (sz_real - sz_compressed) );
   FreeData ('D');
   PutData ('U', big_temp^, sz_compressed );
   crc := CRC32 ( DWORD(-1), PByte(udata), sz_compressed);
   pack_ratio := rpack;
   if rpack > 10 then
      Owner.LogMsg ( Format(#10'#GOOD: pk ratio~C0A %9.2f~C07, saved_space =~C0D %8d~C07, addr = $%04p, writes = %7d, CRC = $%8X, %s'#10,
                                [rpack, Owner.saved_space, udata, udata_writes, crc, f_name] ));
   b_packed := TRUE;
   save_packed := TRUE;
  except
   on E: Exception do
      Owner.LogMsg( Format('#EXCEPTION: LZHuff_Deflate#2 for %d bytes data of %s. Details: %s', [sz_real, f_name, E.Message] ) );
  end
 else
  try
   b_packed := FALSE;
   save_packed := FALSE;
   sz_compressed := sz_real;
   FreeData('P');
  except
   on E: Exception do
      Owner.LogMsg( Format('#EXCEPTION: LZHuff_Deflate#3 for %d bytes data of %s. Details: %s', [sz_real, f_name, E.Message] ) );
  end;




end;





procedure TAsyncFile.UpdateFromTag;
{var
   finf: BY_HANDLE_FILE_INFORMATION;}
var
   laTime: FILETIME;
   lwTIme: FILETIME;
begin
 if f_tag.dwSignature <> SIGNATURE_TAGV0 then exit;

 {
 finf.dwFileAttributes := f_tag.dwFileAttrs;
 finf.ftCreationTime   := f_tag.crTime;
 finf.ftLastAccessTime := f_tag.laTime;
 finf.ftLastWriteTime  := f_tag.lwTime;
 }


 with f_tag do
  begin
   laTime := FileTimeRel (crTime, laTimeDiff);
   lwTime := FileTimeRel (crTime, lwTimeDiff);
   SetFileTime (hWrite, @crTime, @laTime, @lwTime);
  end;
 SetFileAttributes ( PChar (f_name), f_tag.dwFileAttrs );
end;

{ TPackerThread }

function TPackerThread.ProcessRequest(const rqs: String; rqobj: TObject): Integer;
var
   af: TAsyncFile;
begin
 if (rqs = 'TRY_DEFLATE') and (Assigned(rqobj)) then
   begin
    Priority := tpNormal;
    af := TAsyncFile (rqobj);
    Assert (af.busy = 'P', 'async_file.busy = ' + af.busy);

    FillChar (temp_buff, sizeof(temp_buff), 0);

    try
     af.TryDeflate (@temp_buff, @big_buff, sizeof(big_buff));
    except
     on E: Exception do
       OnExceptLog ('TryDeflate file = ' + af.f_name + ', size = ' + IntToStr(af.sz_real), E);
    end;

    if not af.b_packed then
       af.crc := CRC32 ( DWORD(-1), @af.data[0], af.sz_real);

    af.busy := 'c';
    af.SendEvent;
    // Priority := tpNormal;
    result := 0;
    exit;
   end;
 result := inherited ProcessRequest (rqs, rqobj);
end;


function LZMM_Alloc(uBytes: SIZE_T): Pointer; stdcall;
begin
 GetMem(result, uBytes);
end;

function LZMM_ReAlloc(pOld: Pointer; uBytes: SIZE_T): Pointer; stdcall;
begin
 result := pOld;
 ReallocMem (result, uBytes);
end;

procedure LZMM_Free(p: Pointer); stdcall;
begin
 FreeMem (p);
end;

procedure LibProc (reason: Integer);
begin
 try
  case reason of
   DLL_PROCESS_DETACH:
      begin
       FreeAndNil (files_pool);
       FinalizeModule ('~ALL');
      end;
  end;

 except
  on E: Exception do
     OnExceptLog ('LibProc', E);

 end;

end;


{ TArchiveChunk }

procedure TArchiveChunk.SaveData(hDest: THandle);
var
  prev_ofs: UInt64;
  file_ofs: UInt64;
  data_pos: DWORD;
   hdr_add: UInt64;
    dumped: UInt64;
    ch_pos: UInt64;

      buff: Pointer;
      i, n: Integer;
      diff: DWORD;
      tail: DWORD;
       wcb: DWORD;
        af: TAsyncFile;
begin
 if m_files.Count = 0 then
   begin
    WriteFile (hDest, m_data.Memory^, m_data.Size, wcb, nil);
    Inc ( Owner.writed_bytes, wcb );
    exit;
   end;


  // =============================================== DUMPING PART ==================================
 // write [ no compression ].
 // ?? необходимо перекинуть файловые объекты в TObjectList, чтобы провести сортировку по смещению (файловому указателю)
 Files.Reverse; // первый файл сделать последним в списке
 Files.OwnsObjects := FALSE;

 // wfiles.Sort(cmpFileOffset);
 prev_ofs := 0;
 m_data.Clear;
 m_data.Write(m_header, sizeof(m_header));
 hdr_add := m_data.Size;
 dumped := 0; // из этого чанка ещё ничего не сохранено

 SetFilePointerEx (hDest, dumped, @file_ofs, FILE_CURRENT); // что там уже есть в файле


 tail := 0;

 for n := 0 to Files.Count - 1 do
  begin
   af := TAsyncFile ( Files.Objects [n] );
   if af.is_clone then continue;
   af.is_last := TRUE;
   break;
  end;

 for n := Files.Count - 1 downto 0 do
 with Owner do
   begin
    af := TAsyncFile ( Files.Objects [n] );
    // Assert (af.is_clone xor af.is_last, ' last file is clone! ');

    Assert (Assigned(af), 'File object not assigned #' + IntToStr(n));

    if (af.is_clone) or (af.sz_compressed = 0) then
      begin
       af.busy := 'D';
       af.FreeData;
       files_pool.Add (af);
       files.Delete(n); // not need any dumping
       continue;
      end;

    ch_pos := af.target_offset - m_base_offset; // позиция в пределах данных чанка
    diff := ch_pos - prev_ofs;
    if diff <> af.tag_bytes + tail then
       __nop;

    tail := af.tail_bytes;

    Assert (ch_pos >= prev_ofs, ' unsorted file catched ' + af.f_name);

    Assert (ch_pos >= dumped,   Format(' unsorted file catched 2 %s < %s, prev ends = %s ',
                                                [IntToStr(af.target_offset), IntToStr(dumped),
                                                 IntToStr(prev_ofs)]));

    data_pos := hdr_add + ch_pos - dumped;
    if af.Tagged and (not af.is_clone) then
      begin
       m_data.Position := data_pos - sizeof(af.f_tag);
       m_data.Write(af.f_tag, sizeof(af.f_tag));
      end;

    m_data.Position := data_pos;

    prev_ofs := ch_pos + af.sz_compressed;



    if af.save_packed then
      begin
       Assert ( Length(af.udata) >= af.sz_compressed, 'Length(udata) < af.sz_compressed for ' + af.f_name);
       m_data.Write (af.udata[0], af.sz_compressed)
      end
    else
      try
       Assert ( Length(af.data) >= af.sz_real, 'Length(data) < af.sz_real for ' + af.f_name);
       Assert ( NativeUInt(@af.data[0]) > $10000,
             Format('data unassigned = %s %d bytes!\n %s' , [ FormatPtr(@af.data[0]), af.sz_real, af.op_log]));
       m_data.Write ( af.data[0], af.sz_real);
      except
       on E: Exception do
          OnExceptLog(Format('write data %s %d bytes\n %s' , [ FormatPtr(@af.data[0]), af.sz_real, af.op_log]), E, TRUE);
      end;

    m_data.Write(ZERO_VECTOR, af.tail_bytes);
    if af.is_last then
       m_data.Write (ZERO_VECTOR, m_align_bytes); // дополнить окончание чанка


    //      TestCompression ( RelativePtr(dump.Memory, sizeof(chkh) + 9 ), sindex.Memory, p_size - 9, sindex.Size );

    if (af.is_last or (m_data.Size > DUMP_DELTA)) and not Compressed then
      begin
       // data_ofs := 0;
       if m_data.Size and 3 <> 0 then
          __nop;

       if self.IntOffsets then
          SetFilePtr (hDest, m_base_offset + dumped + file_ofs)
       else
          SetFilePtr (hDest, m_base_offset + dumped - hdr_add);


       if WriteFile (hDest, m_data.Memory^, m_data.Size, wcb, nil) then
         begin
          Inc (dumped, m_data.Size - hdr_add); // в сохраненых байтах заголовок чанка не учитывается!
          m_data.Clear;
          Inc (writed_bytes, wcb);
          hdr_add := 0;
          gPrgCB ( 0, 1, writed_bytes, 100.0 * writed_bytes / target_size );
         end
       else
          PrintError('WriteFile returned error ' + err2str);

     end;

    af.FreeData; // освобождаем память вместо занятой потоком
    files_pool.Add (af);
    Files.Delete (n);
   end; // for FILE

 if Files.Count > 0 then
    __nop;
 SetFilePtr (hDest, m_base_offset + dumped);
 Inc (Owner.align_bytes, m_align_bytes);
 if Compressed then  // допускается упаковка только всего чанка полностью!
   begin
    Assert (m_data.Size = m_header.Size + sizeof(m_header),
             Format('Mistmatch sizes, stored %.3f KiB, expected %.3f KiB ',
                    [m_data.Size / 1024, (m_header.Size + sizeof(m_header)) / 1024]));

    wcb := m_header.Size;                       // сколько записано байтов в область данных
    GetMem ( buff, LZ4_compressBound (wcb) );
    LZ4_deflate ( buff, m_header.size, RelativePtr (m_data.Memory, sizeof(m_header)), wcb );
    Assert ( m_header.size > 0, ' LZ4_compress failed ');
    Inc (Owner.saved_space, wcb - m_header.size);
    m_header.id   := m_header.id or CHUNK_COMPRESSED;
    m_data.Clear;
    m_data.Write (m_header, sizeof(m_header));   // перезапись заголовка чанка
    m_data.Write (buff^, m_header.Size);         // добавление сжатых данных
    SetFilePtr (hDest, file_ofs);                // чанк добавляется в конец файла
    if not WriteFile (hDest, m_data.Memory^, m_data.Size, wcb, nil) then
       Owner.LogMsg('~C0C #ERROR:~C07 WriteFile failed for 1-st chunk ' + err2str);
   end
 else
   begin
     Assert (m_data.Size = 0, 'Not all data stored to file');
     Assert (dumped and $7 = 0, 'Not aligned chunk size produced = ' + IntToStr(dumped and $F));
     Assert (dumped = m_header.Size,
             Format('Mistmatch sizes, stored %.3f KiB, declared %.3f KiB ',
                    [dumped / 1024, m_header.Size / 1024]));
   end;

 m_data.Clear;
end;

function TArchiveChunk.StoreIndex;
var
   comp_index: PByteArray;
    comp_size: DWORD;
         serr: String;
            n: Integer;
begin
  comp_index := nil;
  try
   m_header.id := DB_CHUNK_INDEX;

  {$IFDEF LZO_PACK}
   LZHuff_deflate (comp_index, comp_size, m_index.Memory, m_index.Size);
  {$ELSE}
   comp_size := LZ4_compressBound (m_index.Size);
   GetMem (comp_index, comp_size);
   n := LZ4_deflate (comp_index, comp_size, m_index.Memory, m_index.Size);
   Assert ( n = 0, 'Deflate index failed');
   TestCompression ( LPSTR(comp_index),     m_index.Memory, comp_size, m_index.Size);
  {$ENDIF}

  except
   on E: Exception do
    begin
     serr := Format('Exception catched in StoreIndex dest = $%p, dest_size = %d, source = $%p, source_size = %d ',
                    [comp_index, comp_size, m_index.Memory, m_index.Position] );
     // PrintError ( serr );
     Owner.LogMsg('#ERROR: ' + serr);
     OnExceptLog ( ClassName + '.PackDB/LZHuff_deflate #1', E);
    end;
  end;


 m_header.size := comp_size;

 if (not bTest) and (m_index.Size > comp_size) then
  begin
    m_header.id := m_header.id or CHUNK_COMPRESSED;
    m_data.Clear;
    m_data.Write(m_header, sizeof(m_header));
    m_data.Write (comp_index^, comp_size);
  end;

 {$IFDEF LZO_PACK}
  LZHuff_ReleaseBuffer(comp_index);
  {$ELSE}
  FreeMem (comp_index);
  {$ENDIF}
  result := AddAlignChunk (sizeof(m_header) + m_header.size, target_size, not bTest);
end;

function TArchiveChunk.StoreOffset(af: TAsyncFile; af_index, osize: Integer; b_write: Boolean): UInt64;
var
   bf: TAsyncFile;
    i: Integer;
 begin
   bf := nil;
   result := 0;

   bf := af.cloned_from;

   if bf <> nil then
    begin
     af.target_offset := bf.target_offset;
     af.next_offset   := bf.next_offset;
     af.is_clone := TRUE;
     af.f_tag.dwSignature := 0; // хранить тег для пустышки нет желания
     if b_write then // ie.flags and PF_OFFSET_ZERO = 0
        m_index.Write(af.target_offset,  osize);    // по этому смещению будет записано тело файла.
    end
   else
   with Owner do
    begin
     af.is_clone := FALSE;
     af.tag_bytes := 0;

     if af.tagged then
       begin
        while (m_curr_offset + sizeof(TFileTagV0)) and $F <> 0 do
         begin
          Inc (af.tag_bytes);
          Inc (m_curr_offset);
         end;

        Inc (m_curr_offset, sizeof(TFileTagV0));
        Inc (af.tag_bytes,  sizeof(TFileTagV0));
       end;

     Inc (tags_bytes, af.tag_bytes);
     Inc (result,     af.tag_bytes);
     af.target_offset := m_curr_offset;

     if b_write then
        m_index.Write (m_curr_offset, osize);       // по этому смещению будет записано тело файла.


     // в тело архива врезает тело файлика
     Inc (m_curr_offset, af.sz_compressed);
     Inc (result,        af.sz_compressed);

     af.tail_bytes := 0;

     // подгонка выравнивания для следующего файла
     if not af.is_last then
     while (m_curr_offset and $F <> 0) do
       begin
        Inc (m_curr_offset); // align
        Inc (af.tail_bytes);
        Inc (align_bytes);
       end;

     Inc (result, af.tail_bytes);

     af.next_offset := af.target_offset + af.sz_compressed + af.tail_bytes; // без учета тега

     if af.sz_real < af.sz_compressed  then
        Inc ( saved_space, af.sz_real - af.sz_compressed );

    end;

 end;

function  TArchiveChunk.AddAlignChunk;
var
   chkh: TChunkHeader;
begin
 result := from;
 ASSERT (target < 50 *MEBIBYTE, 'To large target');
 if target < $100 then
    target := target + result;

 if target and $F < 8 then
    target := 8 + target - (target and $7);
 if target and $F > 8 then
    target := target or $7 + 9;

 if result + sizeof(chkh) <= target then
    begin
     // Assert ( result <= target - sizeof(chkh), 'no space for aligment chunk header');
     chkh.id   := DB_CHUNK_USERDATA;
     chkh.size := target - result - sizeof(chkh);

     if bWrite then
       begin
        m_data.Write (chkh, sizeof(chkh));
        m_data.Write (PAnsiChar('USERDATA:ALIGN_CHUNK')^, chkh.size);
       end;
     Inc (result, sizeof(chkh) + chkh.size);
    end;

end;

constructor TArchiveChunk.Create;
begin
 inherited Create;
 m_data        := TMemoryStream64.Create;
 m_files       := TStrMap.Create(self);
 m_folders     := TStrMap.Create(self);
 m_folders.Sorted := TRUE;
end;

destructor TArchiveChunk.Destroy;
var
   n: Integer;
begin
 for n := 0 to High(m_hash_map) do
     FreeAndNil ( m_hash_map[n] );


 m_data.Free;
 m_files.Free;
 m_folders.Free;
 inherited;
end;

function TArchiveChunk.IndexFiles(base_offset: UInt64): UInt64;
var
    prvofs: UInt64;
    ofirst: Boolean;
    icount: Integer;
    n_last: Integer;
     osize: DWORD;
     fsize: UInt64;
     chunk: DWORD;
      list: TObjectList;
        af: TAsyncFile;
        bf: TAsyncFile;
        ie: TIndexEntryHdr;
        sa: AnsiString;
        fn: String;
         n: Integer;
         i: Integer;
begin
 // сохранение информации о файлах в индекс
 Inc (m_index_pass);
 IntOffsets := IntOffsets or Compressed;
 if IntOffsets then
    m_base_offset := 0
 else
    m_base_offset := base_offset + sizeof(m_header);

 m_curr_offset := m_base_offset;
 result := 0;
 prvofs := 0;
 chunk  := 0;
 // root_path := root;
 // saved_space := 0;
 FillChar (ie, sizeof(ie), 0);
 ofirst := TRUE;
 icount := 0;
 n_last := m_files.Count - 1;

 if 1 = m_index_pass then
 for n := 0 to n_last do
  begin
   af := TAsyncFile ( m_files.Objects [n] ); // existed file object
   i := af.crc and HASH_MAP_MASK;
   if m_hash_map [i] = nil then
      m_hash_map [i] := TObjectList.Create (FALSE);
   list := m_hash_map [i];
   list.Add (af);
  end;

 

 for n := 0 to n_last do
  begin
   af := TAsyncFile ( m_files.Objects [n] ); // existed file object
   af.is_last := ( n = n_last );
   fn := af.f_name;

   fn := RelPath (fn, Owner.root_path);
   sa := AnsiString(fn);
   ie.flags     := PF_FILE;
   ie.cbHdrSize := Length (sa) + HEADER_SIZE - 2;
   if Pos('stalker_custom_data.ltx', fn) > 0 then
      __nop;


   osize := 4;
   if ofirst then
      ie.flags := ie.flags or PF_OFFSET_FIRST; // 1.0006 not compat!
   if IntOffsets then
      ie.flags := ie.flags or PF_OFFSET_CHUNK;

   ie.cbReal   := af.sz_real;
   ie.cbComp   := af.sz_compressed;
   ie.crc      := af.crc;
   ie.upd_time := _get_modtime64 (PChar(af.f_name));
   if af.sz_compressed = 0 then
      af.f_tag.dwSignature := 0;

   if ( m_index_pass = 1 ) then;
      begin
       i := af.crc and HASH_MAP_MASK;
       list := m_hash_map [i];
       for i := 0 to list.Count - 1 do
         begin
          bf := TAsyncFile (list [i]);
          if af = bf then break; // если дальше будут клоны, их надо оставить
          if ( bf.crc = af.crc ) and ( bf.sz_compressed = af.sz_compressed ) then
           begin
            af.is_clone := TRUE;
            af.cloned_from := bf;
            af.f_tag.dwSignature := 0;
            break;
           end; // if
         end; // for list
      end; // if


   if af.Tagged then
      ie.flags := ie.flags or PF_HAVE_TAG;

   // if (sindex.Size > 4000) and (sindex.Size < 5000) then
   {if Pos('script.ltx', af.f_name) + Pos('prefetch.ltx', af.f_name)  > 0 then
       wprintf(#10'[~T]. #DBG: storing file entry %s at offset %d, header_size = %d, file_size = %d '#10,
                [af.f_name, sindex.Size, ie.cbHdrSize, af.sz_real]);}

   // saving info
   m_index.Write(ie, sizeof(ie));


   if IntOffsets then
     begin
      if Compressed then
         ChunkID := ChunkID or CHUNK_COMPRESSED;

      m_index.Write (ChunkID, sizeof(ChunkID));
     end;

    if ofirst then
      begin
       fsize := StoreOffset(af, n, osize, ie.flags and PF_OFFSET_ZERO = 0);
       m_index.Write(sa[1], Length(sa)); // если имя идет за файловым смещением
      end
    else
      begin
       m_index.Write(sa[1], Length(sa));
       fsize := StoreOffset(af, n, osize, ie.flags and PF_OFFSET_ZERO = 0);
      end;


   if fsize = 0 then continue;
   ASSERT (fsize > 0, 'fsize < 0???');
   Inc (icount);
   // search copies
   if af.is_clone then
      Inc (Owner.clone_bytes, fsize)
   else
      result := af.next_offset - m_base_offset;  // конечный размер данных в чанке, без учета выравнивания

  end; // for
 m_files.SaveToFile('indexed_files.lst');
 Owner.LogMsg(Format('#PERF: indexed %d files, median size with align = %.3f KiB' ,[icount, result / (1024 * icount)]));
 // в чанках с данными можно простое выравнивание использовать
 // at 'base_offset' inserted bytes[result], then next chunk header

 m_align_bytes := 0;
 while (result + base_offset) and $F <> 8 do
  begin
   Inc (result);
   Inc (m_align_bytes);
  end;
 m_header.Size := result;
end;

procedure TArchiveChunk.IndexFolders;
var
     ie: TIndexEntryHdr;
      o: DWORD;
      s: AnsiString;
      n: Integer;
begin
 FillChar (ie, sizeof(ie), 0);
 o := 0;
 Folders.Sort;
 for n := 0 to Folders.Count - 1 do
  begin
   s := AnsiString (Folders [n]);
   ie.flags := PF_DIRECTORY or PF_OFFSET_ZERO;
   ie.cbHdrSize := Length(s) + HEADER_SIZE - 2;  // не учитывается cbHdrSize как элемент заголовка
   ie.upd_time  := _get_modtime64 (PChar( Owner.root_path + Folders [n]));
   m_index.Write(ie,   HEADER_SIZE);             // hdr
   m_index.Write(s[1], Length(s));               // name
  end;
  Folders.SaveToFile('indexed_dirs.lst');
end;

begin
 ShowConsole();
 StartLogging('');
 wprintf(' sizeof(Integer)              = %d ', [sizeof(Integer)]);
 wprintf(' sizeof(DWORD)                = %d ', [sizeof(DWORD)]);
 wprintf(' sizeof(Pointer)              = %d ', [sizeof(Pointer)]);
 wprintf(' sizeof(Entry)                = %d ', [sizeof(TIndexEntryHdr)]);
 wprintf(' sizeof(FileTag)              = %d ', [sizeof(TFileTagV0)]);

 LZ4_SetMemCallbacks (LZ4_AllocMem, LZ4_FreeMem);
 gLZMM.ext_alloc   := LZMM_Alloc;
 gLZMM.ext_realloc := LZMM_ReAlloc;
 gLZMM.ext_free    := LZMM_Free;
 files_pool        := TObjectList.Create (FALSE);
 DllProc           := LibProc;

 {$IFNDEF CPUX64}
 LZHuff_SetMM (@gLZMM);
 {$ENDIF}
end.
