c := 0;

     if ReadConsoleOutput (hBuff, @con_data, sz, zp, rr) then
      for y := rr.Top to rr.Bottom do
       begin
        l := 0;
        for x := rr.Right downto l do
         if not CharInSet (con_data[y, x], [' ', #0]) then
            begin
             l := x;
             break;
            end;

        for x := rr.Left to l do
           begin
            rtext [c] :=  con_data[y, x];
            Inc (c);
           end;

        rtext [c] := #13;
        Inc (c, 2);
       end; // }
