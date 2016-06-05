const
           op_nop: BYTE = $90;
           op_ret: BYTE = $C3;
          op_int3: BYTE = $CC;

       op_pop_eax: BYTE = $58;
       op_push_dw: BYTE = $68;
      op_push_eax: BYTE = $50;
       op_set_eax: BYTE = $B8;
      op_call_eax: WORD = $D0FF;
      op_jump_eax: WORD = $E0FF;
      op_jump_dwf: WORD = $25FF;
    op_mov_ld_eax: WORD = $0589; // 89 05 ?? ?? ?? ?? = mov [???????], eax


