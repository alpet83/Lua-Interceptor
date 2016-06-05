unit TablePack;

interface
uses Windows, SysUtils, Classes, StrClasses, Misc, LuaTypes, XrayLua, LuaTools, LCGlobals, Algs;


function  DecodeBinTable (L: lua_State; buff: PByteArray; ridx: Integer): Integer;
function  EncodeTableBin (L: lua_State; idx: Integer; buff: PByteArray; widx: Integer): Integer;


function  LuaStringPack (L: lua_State): Integer; cdecl;
function  LuaStringUnpack (L: lua_State): Integer; cdecl;
function  LuaTableType (L: lua_State): Integer; cdecl;

implementation


const
    AMK_TAG_NUMBER = #01;
    AMK_TAG_STRING = #02;
    AMK_TAG_BOOL   = #03;
    AMK_TAG_TABLE  = #04;
    AMK_TAG_EOTAB  = #05;
    AMK_TAG_ARRAY  = #06;
    AMK_TAG_HEXDEC = #07;
    AMK_TAGS: set of AnsiChar = [AMK_TAG_NUMBER .. AMK_TAG_HEXDEC];

var
     stat_pack_count: Integer = 0;
   stat_unpack_count: Integer = 0;


function ParseAMKtoken (const sa: AnsiString; from, tab: Integer): Integer;
var
    i: Integer;
   ch: AnsiChar;
begin
 result := Length(sa); // предположить можно, что токен до конца строки

 for i := from to Length (sa) do
   begin
    ch := sa [i];
    if not ( ch in AMK_TAGS ) then continue;

    if ch = AMK_TAG_TABLE then
       Inc(tab); // кол-во вложенных таблиц увеличилось
    if ( ch = AMK_TAG_EOTAB ) and ( tab > 0 ) then
       Dec(tab);

    if ( i > from ) and ( 0 = tab ) then  // обнаружен следующий тег
     begin
      result := i - 1;
      break;
     end;
   end;

end;

function PreParseAMK (const sa: AnsiString; i: Integer): TStrMap; // first pass parse
var
   ch: AnsiChar;
   tk: String;
    e: Integer;
    l: Integer;
begin
 result := GetTmpSL;
 l := Length (sa);
 while (i <= l) do
  begin
   ch := sa[i];
   e := i;
   // *5*33*52
   if ch = AMK_TAG_EOTAB then
     begin
      Inc (i);
      continue; // in original alg need return due recursive
     end;

   if ch in AMK_TAGS then
      e := ParseAMKtoken (sa, i, 0);

   if e > i then
     begin
      tk := String ( Copy ( sa, i, e - i + 1 ) );
      result.Add (tk);
      i := e + 1;
     end
   else
     begin
      wprintf('[~T]. #WARN(PreParseAMK): char ignored #%d at %d for {%s} ', [ BYTE(ch), i, sa]);
      Inc (i);
     end;

  end;


end;

procedure ParseAMK( L: lua_State; sa: AnsiString; from: Integer );
var
   sl: TStrMap;
   tk: String;
   ch: AnsiChar;
   tt: Integer;
   af: Boolean; // array flag
    i: Integer;
begin
 if sa = '' then
   begin
    lua_createtable (L, 0, 0);
    exit;
   end;
 ch := sa[1];
 af := FALSE;

 if ch = AMK_TAG_ARRAY then
    begin
     af := TRUE;
     Delete (sa, 1, 1);
    end;


 sl := PreParseAMK (sa, from);
 if sl.Count > 10 then
    wprintf('[~T]. #DBG(ParseAMK): %d tokens detected ', [sl.Count] );


 lua_createtable (L, sl.Count, 0);
 tt := lua_gettop (L);
 i := 0;

 while ( i < sl.Count ) do
  begin
   tk := sl [i];

   if af then
      lua_pushinteger (L, i + 1); // key for list


   if tk = '' then
      lua_pushnil (L)
   else
     begin
      // if array, every token is value, else only odd token is value
      ch := AnsiChar ( tk [1] );
      Delete (tk, 1, 1);

      case ch of
       AMK_TAG_NUMBER:
          if Pos('#IND', tk) > 0 then
             lua_pushnumber ( L, 0 )
          else
             lua_pushnumber ( L, atof(tk) );

       AMK_TAG_STRING:
          lua_pushwstr (L, tk);
       AMK_TAG_BOOL:
          lua_pushboolean ( L, ( tk = '1' ) or ( LowerCase(tk) = 'true' ) );
       AMK_TAG_HEXDEC:
          lua_pushnumber ( L, atoi ( '$' + tk ) );
       AMK_TAG_TABLE:
          begin
           if dbg_present then
              wprintf('[~T]. #DBG(ParseAMK): Processing inner table for %s, token = %s ', [ ReplaceChar ( String(sa), #7, '?' ), '~C0F' + tk]);
           ParseAMK ( L, AnsiString (tk), 1 ); // TODO: debug multiply tables include
          end;

      else
          wprintf('[~T].~C0c #WARN(ParseAMK):~C07 unexpected tag %d for %s ', [Byte(ch), tk] );
      end; // case

     end; // not void token handle

   if af or ( i and 1 <> 0 ) then
      lua_settable (L, tt);

   Inc (i);
  end;


end;


function Buff2Hex( buff: PByteArray; cb: Integer ): String;
const
   HEX_STR: PWideChar = '0123456789ABCDEF'#0#0;

var
   p, i: Integer;
      b: BYTE;
begin
 SetLength (result, cb * 2);
 p := 1;
 for i := 0 to cb - 1 do
  begin
   b := buff[i];
   result [p] := HEX_STR [ b shr 4 ];
   Inc (p);
   result [p] := HEX_STR [ b and 15];
   Inc (p);
  end;
end;


function HexHalfByte ( c: AnsiChar ): BYTE;
begin
 if c <= '9' then
    result := BYTE (c) - $30
 else
 if c < 'a' then
    result := BYTE (c) - $41 + 10
 else
    result := BYTE (c) - $61 + 10;

end;

function HexToByte ( h, l: AnsiCHAR ): BYTE; inline;
begin
 result := HexHalfByte ( h) shl 4 + HexHalfByte (l);
end;

function Hex2Buff ( const s: AnsiString; buff: PByteArray; size: Integer ): Integer;
var
   i: Integer;
   p: Integer;
begin
 result := Length (s) div 2;
 p := 1;
 for i := 0 to result - 1 do
  begin
   if p >= size then break;
   buff [i] := HexToByte ( s[p], s[p + 1] );
   Inc (p, 2);

  end;

end;


function EasyPack ( src, dest: PByteArray; cb: Integer ): Integer;
var
   cur: BYTE;
     n: Integer;
begin
 result := 0;
 dest [0] := 0;
 dest [1] := 0;
 for n := 0 to cb - 1 do
  begin
   cur := src [n];
   if ( cur = dest [result + 0] ) and ( dest [result + 1] < 255 ) then
      Inc ( dest [result + 1] )
   else
     begin
      Inc ( result, 2 );
      dest [result + 0] := cur;
      dest [result + 1] := 1;
     end;
  end;

 if ( dest [result + 1] > 0 ) then
      Inc (result, 2);
end;


type
   TTaggedValue = packed record
     tag: AnsiChar;
     rsv: BYTE;
    size: WORD;
    case BYTE of
     0: (iv: Int64 );
     1: (dv: Double );
     2: (bv: LongBool );
     3: (fv: lua_CFunction );
     4: (pv: Pointer );
     5: (ss: array [0..7] of AnsiChar);
   end;

   PTaggedValue = ^TTaggedValue;

function EncodeValueBin ( L: lua_State; idx: Integer; dest: PByteArray; didx: Integer; key: Boolean = FALSE ): Integer;

var
    lt: Integer;
    tv: PTaggedValue;
    sa: AnsiString;
    sl: DWORD;


begin
 lt := lua_type (L, idx);
 tv := @dest[didx];
 Inc ( didx, sizeof (TTaggedValue) );

 FillChar ( tv^, sizeof (tv), 0 );

 tv.tag := AnsiChar (lt);
 if key then
    tv.rsv := $10;

 case lt of
  LUA_TBOOLEAN:
      tv.bv := lua_toboolean (L, idx);
  LUA_TNUMBER:
      tv.dv := lua_tonumber (L, idx);
  LUA_TSTRING:
     begin
      sa := lua_tolstring (L, idx, sl);
      Assert ( sl < 65536, 'Heavy string cannot be packed');
      Assert ( sl + DWORD(didx) < 65535, '64Kv buffer overrun detected');
      tv.size := sl;
      tv.iv := 0;
      CopyMemory ( @tv.ss, @sa[1], sl );  // allowed overrun this value buffer!
      if sl > 8 then
         Inc ( didx, sl - 8 );            // expanded buffer

      while ( didx and 3 <> 0 ) do
         begin
          dest [didx] := $FF; // alt space
          Inc ( didx );
         end;
     end;
  LUA_TTHREAD:
      tv.pv := lua_tothread (L, idx);
  LUA_TFUNCTION:
      tv.fv := lua_tocfunction (L, idx);
  LUA_TUSERDATA:
      tv.pv := lua_touserdata (L, idx);
  LUA_TLIGHTUSERDATA:
      tv.pv := lua_topointer (L, idx);
 end;
 result := didx;
end;

function  EncodeTableBin (L: lua_State; idx: Integer; buff: PByteArray; widx: Integer): Integer;
var
     cnt: Integer;
      tv: PTaggedValue;
      st: Integer;
      tt: Integer;

begin
 cnt := 0;
 result := widx;

 try
   tv := @buff[widx];

   tt := lua_table_type (L, idx, @cnt);
   if tt = 0 then exit;

   Inc (widx, sizeof (TTaggedValue));
   tv.tag := AnsiChar (LUA_TTABLE);
   tv.rsv := tt;
   tv.size := cnt;

   lua_pushnil (L);
   while lua_next (L, idx) <> 0 do
    begin
     st := lua_gettop (L);

     if tt = 2 then
        widx := EncodeValueBin ( L, st - 1, buff, widx, TRUE ); // put also key

     if lua_type (L, st) = LUA_TTABLE then
        widx := EncodeTableBin ( L, st, buff, widx )
     else
        widx := EncodeValueBin ( L, st, buff, widx );

     lua_pop (L, 1);
    end;

   //  pksz := EasyPack ( @buff, @pack, widx );
   tv.iv := widx;
   result := widx;

  except
   on E: Exception do
      PrintError('Exception cached in PackTableHex: ' + E.QualifiedClassName + ' ' + E.Message);
  end;
end;




function DecodeValueBin (L: lua_State; buff: PByteArray; ridx: Integer): Integer;
var
    tv: PTaggedValue;
    sa: AnsiString;

begin
 tv := @buff [ridx];

 result := ridx + sizeof (TTaggedValue);

 case BYTE(tv.tag) of
  LUA_TBOOLEAN:
      lua_pushboolean (L, tv.bv);
  LUA_TNUMBER:
      lua_pushnumber (L, tv.dv);
  LUA_TLUDATA,
  LUA_TUSERDATA:
      lua_pushlightuserdata (L, tv.pv);

  LUA_TSTRING:
     begin
      SetString (sa, tv.ss, tv.size);
      lua_pushstring (L, PAnsiChar(sa) );
      if tv.size > 8 then
         Inc ( result, tv.size - 8 );
      while ( result and 3 <> 0 ) do Inc (result); // align 4
     end;

  LUA_TFUNCTION:
      lua_pushcfunction ( L, tv.fv );

  LUA_TTABLE:
      result := DecodeBinTable (L, buff, ridx);
  else
      lua_pushnil (L);

 end; // case
end;


function DecodeBinTable (L: lua_State; buff: PByteArray; ridx: Integer): Integer;
var
   cnt: Integer;
   lim: Integer;
   map: Boolean;
    tv: PTaggedValue;
    ti: Integer;
     n: Integer;

begin
 tv := @buff[ridx];
 result := ridx;
 if BYTE(tv.tag) <> LUA_TTABLE then exit;
 map := ( tv.rsv = 2 ); // table has keys
 cnt := tv.size;
 Inc (ridx, sizeof(TTaggedValue));
 lim := tv.iv;

 if map then
    lua_createtable (L, 0, cnt)
 else
    lua_createtable (L, cnt, 0);

 ti := lua_gettop(L);

 for n := 0 to cnt - 1 do
  begin
   if map then
     begin
      tv := @buff[ridx];
      Assert ( tv.rsv = $10, Format('Key marker not assotiated for value, tag = %d', [Byte(tv.tag)] ) );
      ridx := DecodeValueBin (L, buff, ridx); // decode key
     end
   else
      lua_pushinteger (L, n + 1);

   ridx := DecodeValueBin (L, buff, ridx);
   lua_settable (L, ti);
   if ridx >= lim then
      break;

  end; // for
  result := ridx;
end;


function StrZeroPack ( s: String ): String;

var
   l, i, c: Integer;
        zf: Boolean;
begin
 c := 0;
 l := Length(s);
 SetLength ( result, l );
 result := '';
 for i := 1 to l do
   begin
    zf := ( s [i] = '0' );
    // если встречается символ отличный от нуля, добавить предшествующие нули как счетчик
    if not zf or ( c >= 8 ) then
     begin
      if ( c >= 1 ) then
           result := result + Char(c);
      c := 0;
    end;

    if zf then
       Inc (c)
    else
      begin
       c := 0;
       result := result + s [i];
      end;
   end;

 // probably end of string have zeros
 if ( c >= 1 ) then
      result := result + Char(c);
end;

function StrZeroUnpack ( s: String ): String;

var
   i: Integer;
   c: SmallInt;
   z: Integer;
begin
 result := '';
 for i := 1 to Length (s) do
   begin
    c := SmallInt (s [i]);
    // TODO: check is method fastest
    case c of
      1: result := result + '0';
      2: result := result + '00';
      3: result := result + '000';
      4: result := result + '0000';
      5: result := result + '00000';
      6: result := result + '000000';
      7: result := result + '0000000';
      8: result := result + '00000000';
    else
         result := result + s [i];
    end; // case
   end; // for

end;


function LuaStringPack (L: lua_State): Integer; cdecl;
var
    buff: array [0..65535] of BYTE;
    size: Integer;
      rs: String;
      zs: String;
begin
 Inc ( stat_pack_count );
 if stat_pack_count mod 1000 = 0 then
    wprintf('[~T]. #STAT: LuaStringPack was called %d counts ', [stat_pack_count] );


 FillChar (buff, sizeof(buff), $FE);
 size := EncodeTableBin ( L, 1, @buff[0], 0 );
 zs := #$A7 + Buff2Hex ( @buff, size );
 rs := StrZeroPack (zs);
 if zs <> StrZeroUnpack (rs) then
    wprintf('[~T].~C0C #ERROR:~C07 decoded result %s <> source %s ', [StrZeroUnpack(rs), zs] );

 lua_pushwstr ( L, rs );
 result := 1;
end;

function LuaStringUnpack (L: lua_State): Integer; cdecl;
var
  buff: array [0..65535] of BYTE;
   alg: String;
    sa: AnsiString;
    ch: AnsiChar;
    sl: TStrMap;
    ac: Integer;
    at: Integer;
     s: String;
begin
 Inc ( stat_unpack_count );
 if stat_unpack_count mod 1000 = 0 then
    wprintf('[~T]. #STAT: LuaStringUnpack was called %d counts ', [stat_unpack_count] );

 alg := 'amk';
 s := LuaStrArg(L);
 at := 1;
 ac := lua_gettop(L);
 if ( s <> '' ) and ( s [1] = #$A7 ) then alg := 'bin'; // auto-detect

 if ac > 1 then at := lua_tointeger (L, 2);
 if ac > 2 then alg := LuaStrArg (L, 3);

 result := 1;
 // parsing typical
 if alg = 'amk' then
   begin
    sa := AnsiString (s);
    ParseAMK ( L, sa, at );
   end;

 if alg = 'bin' then
   begin
    s := StrZeroUnpack ( s );
    sa := AnsiString (s);
    if ( sa <> '' ) and ( sa [1] = #$A7 ) then
         Delete ( sa, 1, 1 )
    else
         PrintError ('LuaStringUnpack: string without prefix');
    Hex2Buff ( sa, @buff, sizeof(buff) );
    DecodeBinTable ( L, @buff, 0 );
   end;

 if ac + 1 < lua_gettop (L) then
    wprintf('[~T]. #WARN(LuaStringUnpack): added values count = %d ', [lua_gettop(L) - ac] );

 at := lua_type (L, -1);
 if at <> LUA_TTABLE then
    wprintf('[~T]. #WARN(LuaStringUnpack): last pushed value type = %d', [at]);

end;


function LuaTableType (L: lua_State): Integer; cdecl;
begin
 result := 1;
 lua_pushinteger ( L, lua_table_type (L, 1) );
end;


end.
