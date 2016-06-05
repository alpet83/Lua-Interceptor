unit Scrambler;

interface
uses
  Windows,
  System.SysUtils,
  System.Classes,
  misc;


const
  SBOX_SIZE = 256;

type
  TSBOX = packed array [0..SBOX_SIZE - 1] of BYTE;
  PSBOX = ^TSBOX;


  TXrScrambler = class
  private
   enc_sbox: TSBOX;
   dec_sbox: TSBOX;
   m_seed: Integer;

   procedure            init_sboxex(_seed, init_seed, size_mult: Integer);

  public
   { C & D }
   constructor          Create (cfg: CHAR = 'R');

   { methods }

   procedure            decrypt (pSrc, pDst: PByteArray; cbSize: Integer);
   procedure            encrypt (pSrc, pDst: PByteArray; cbSize: Integer);


  end;

implementation

const
     SEED_MULT          = $8088405;
     SEED_RU            = $131a9d3;
     SEED0_RU           = $1329436;
     SIZE_MULT_RU       = 8;
     SEED_WW            = $16eb2eb;
     SEED0_WW           = $05bbc4b;
     SIZE_MULT_WW       = 4;


procedure SwapBytes(var a, b: Byte); inline;
var
   t: Byte;
begin
 t := a;
 a := b;
 b := t;
end;


{ TXrScrambler }

constructor TXrScrambler.Create(cfg: CHAR);
begin
 if cfg = 'R' then
   init_sboxex(SEED_RU, SEED0_RU, SIZE_MULT_RU)
 else
   init_sboxex(SEED_WW, SEED0_WW, SIZE_MULT_WW);

end;

procedure TXrScrambler.decrypt(pSrc, pDst: PByteArray; cbSize: Integer);
var
   i, seed: Integer;
   bb, key: BYTE;

begin
 seed := m_seed;

 for i := 0 to cbSize - 1 do
   begin
    seed := 1 + seed * SEED_MULT;
    key := (seed shr 24) and $FF;
    bb := dec_sbox [pSrc[i] xor key];
    pDst [i] := bb;
   end; // for

end; // decrypt

procedure TXrScrambler.encrypt(pSrc, pDst: PByteArray; cbSize: Integer);
var
   i, seed: Integer;
   bb, key: BYTE;

begin
 seed := m_seed;

 for i := 0 to cbSize - 1 do
   begin
    seed := 1 + seed * SEED_MULT;
    key := (seed shr 24) and $FF;
    bb := enc_sbox [pSrc[i]] xor key;
    pDst [i] := bb;
   end; // for
end; // encrypt


procedure TXrScrambler.init_sboxex(_seed, init_seed, size_mult: Integer);
var
   a, b, i: Integer;
begin
 m_seed := _seed;

 for i := SBOX_SIZE - 1 downto 0 do enc_sbox[i] := i;

 // mixing table of 256 downscending bytes
 for i := size_mult * SBOX_SIZE downto 1 do
  begin
   init_seed := 1 + init_seed * SEED_MULT;
   a := (init_seed shr 24) and $FF;

   repeat
    init_seed := 1 + init_seed * SEED_MULT;
    b := (init_seed shr 24) and $FF;
   until (a <> b);

   SwapBytes ( enc_sbox[a], enc_sbox[b] );
  end; // for

 for i := 0 to SBOX_SIZE - 1 do
  begin
   a := enc_sbox [i];
   dec_sbox[a] := i;
  end;

end; // init_sboxes

end.
