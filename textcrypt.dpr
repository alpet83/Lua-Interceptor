
{$APPTYPE CONSOLE}
uses Windows, SysUtils, Classes, StrUtils, StrClasses, Misc;


procedure convert (filename: String);
var
      f: File;
     sz: Int64;
   buff: array of BYTE;
   dest: TStrMap;
   x, y, n: Integer;
   s, r: String;

begin
 if fileName = '' then
    begin
     PrintError('FileName must be specified in command line!');
     exit;
    end;

 AssignFile (f, filename);
 {$I-}
 Reset (f, 1);
 if IOresult <> 0 then exit;

 sz := FileSize (f);
 dest := TStrMap.Create;

 if (sz > 0) and (sz < 1048576) then
  begin
   SetLength (buff, sz);
   BlockRead (f, buff[0], sz);

   s := ExtractFileName (filename);
   r := s;
   if Pos('.', s) > 0 then
      s := StrTok (r, ['.']);



   dest.Add( 'const ' + s + ': array[0..' + IntToStr(sz - 1) + '] of BYTE = (' );


   for y := 0 to ((sz - 1) div 16) do
    begin
     r := '';
     for x := 0 to 15 do
      begin
       n := y * 16 + x;
       if n >= sz then break;

       r := r + '$' + IntToHex ( buff [n] xor BYTE(n + $BC), 2 );

       if n < sz - 1 then
          r := r + ', '
       else
          r := r + ' );';
      end;
     dest.Add (#9 + Trim (r));
    end;

   // ODS ('       );');

   SetLength (buff, 0);
  end;

 CloseFile (f);

 ODS ('~C0F' + dest.Text + '~C07');

 s := ExtractFileExt (filename);
 filename := AnsiReplaceStr (filename, s, '.inc');
 dest.SaveToFile (filename);
 dest.Free;
 Sleep(500);
end;


begin
 StartLogging('');
 ShowConsole (SW_MINIMIZE);
 convert(ParamStr(1));
 Windows.Beep(1000, 100);
end.