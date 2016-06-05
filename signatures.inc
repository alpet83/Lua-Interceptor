{
 const
   sig_a00: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $BE, $18, $74, $A6, $A5, $51, $80, $CD, $FA, $87, $71, $D9, $D2, $0F, $22, $7D ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $A8, $04, $F8, $A2, $E2, $91, $A9, $6F, $15, $CE, $8F, $62, $7D, $AD, $D5, $1E ) );
   sig_a01: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $81, $EC, $E3, $AD, $2D, $91, $49, $E3, $C9, $FD, $7A, $48, $23, $CB, $FE, $BB ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $1F, $19, $D4, $B2, $DC, $EA, $D2, $6A, $A3, $A0, $37, $78, $89, $60, $B4, $23 ) );
   sig_a02: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $C9, $3B, $8B, $5C, $2B, $55, $70, $74, $C4, $2D, $E4, $F0, $3F, $9F, $FC, $89 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $81, $6F, $0A, $BB, $9E, $7B, $30, $03, $24, $28, $1D, $A5, $59, $C8, $C8, $19 ) );
   sig_a03: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $FA, $1E, $B5, $EF, $AB, $13, $36, $5D, $89, $3F, $41, $12, $65, $8D, $BA, $3E ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $46, $7B, $1A, $2E, $63, $EE, $E1, $FA, $2D, $93, $09, $17, $BA, $2B, $9A, $07 ) );
   sig_a04: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $96, $15, $75, $13, $B1, $12, $DE, $5B, $DC, $6C, $0C, $EE, $8F, $D0, $0D, $F0 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $FF, $67, $C0, $0B, $C7, $40, $77, $C6, $DC, $9E, $B2, $03, $70, $96, $96, $75 ) );
   sig_a05: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $32, $AD, $FA, $E8, $39, $2D, $3E, $DE, $52, $E0, $4A, $0F, $EB, $4C, $23, $C1 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $98, $AB, $F5, $35, $9D, $68, $81, $51, $32, $07, $28, $93, $50, $D6, $01, $EC ) );
   sig_a06: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $BA, $99, $D7, $A9, $1D, $92, $FB, $81, $1E, $D5, $8B, $6C, $E8, $90, $9E, $E5 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $FB, $D7, $F9, $E7, $B1, $13, $DB, $72, $26, $24, $D2, $09, $DB, $03, $A4, $2A ) );
   sig_a07: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $BF, $2C, $98, $6A, $C0, $45, $5B, $B6, $2A, $DC, $28, $8E, $C5, $FB, $A7, $5E ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $23, $F2, $BF, $8A, $FA, $AC, $72, $C8, $BD, $FC, $6F, $46, $66, $9E, $36, $63 ) );
   sig_a08: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $4F, $43, $2F, $EA, $1D, $95, $6F, $08, $D3, $5C, $B0, $59, $06, $09, $A6, $D3 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $0D, $E2, $26, $B7, $7F, $E6, $03, $2A, $74, $FF, $0D, $C7, $05, $51, $A3, $DB ) );
   sig_a09: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $AF, $C8, $A5, $AE, $15, $E5, $E3, $A7, $7E, $6B, $34, $BC, $3D, $A7, $78, $62 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $D8, $84, $A7, $DB, $9A, $E8, $06, $F2, $E4, $CE, $3F, $B1, $1B, $6B, $5F, $9D ) );
   sig_a10: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $73, $21, $42, $7C, $3D, $5E, $E6, $A6, $4C, $7B, $C5, $33, $76, $5E, $3C, $64 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $41, $C2, $E8, $F2, $7C, $57, $D6, $F7, $98, $35, $36, $E1, $35, $D7, $75, $89 ) );
   sig_a11: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $9B, $D0, $A8, $BE, $B0, $14, $0D, $8D, $D4, $D8, $AC, $9C, $CC, $DC, $D1, $1E ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $A1, $79, $C2, $61, $FC, $73, $BC, $86, $86, $91, $4E, $2B, $F8, $A6, $99, $1B ) );
   sig_a12: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $2C, $80, $3E, $79, $53, $E7, $B7, $D4, $E7, $13, $EB, $BE, $02, $2F, $C5, $89 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $DD, $0E, $4A, $14, $C5, $76, $06, $D3, $26, $FE, $3A, $C5, $6F, $8F, $62, $FB ) );
   sig_a13: array [0..1] of TRHashVector =
          ( ( $A1, $CE, $DD, $39, $A8, $0C, $F2, $BF, $E7, $30, $CC, $E0, $A3, $76, $A4, $67, $38, $6C, $AB, $09, $8D, $29, $15, $19 ),
            ( $3C, $A2, $B0, $FE, $77, $32, $D1, $4D, $61, $AB, $A0, $21, $23, $DF, $7A, $1D, $9A, $69, $6D, $F7, $A5, $02, $34, $CA ) );

   sig_list: array [0..15] of Pointer = ( @sig_a00, @sig_a01, @sig_a02, @sig_a03,
                                          @sig_a04, @sig_a05, @sig_a06, @sig_a07,
                                          @sig_a08, @sig_a09, @sig_a10, @sig_a11,
                                          @sig_a12, @sig_a13, nil, nil );

  }
  const
   sig_list: array [0..15] of Pointer = ( nil, nil, nil, nil,
                                          nil, nil, nil, nil,
                                          nil, nil, nil, nil,
                                          nil, nil, nil, nil );
