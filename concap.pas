procedure RunDosInMemo(DosApp:String;AMemo:TMemo) ;
  const
     ReadBuffer = 2400;
  var
   Security : TSecurityAttributes;
   ReadPipe,WritePipe : THandle;
   start : TStartUpInfo;
   ProcessInfo : TProcessInformation;
   Buffer : Pchar;
   BytesRead : DWord;
   Apprunning : DWord;
  begin
   With Security do
     begin
      nlength := SizeOf(TSecurityAttributes) ;
      binherithandle := true;
      lpsecuritydescriptor := nil;
     end;
   if CreatePipe (ReadPipe, WritePipe,
                  @Security, 0) then

     begin
      Buffer := AllocMem(ReadBuffer + 1) ;
      FillChar(Start,Sizeof(Start),#0) ;
      start.cb := SizeOf(start) ;
      start.hStdOutput := WritePipe;
      start.hStdInput := ReadPipe;
      start.dwFlags := STARTF_USESTDHANDLES +
                         STARTF_USESHOWWINDOW;
      start.wShowWindow := SW_HIDE;

      if CreateProcess(nil,
            PChar(DosApp),
           @Security,
           @Security,
           true,
           NORMAL_PRIORITY_CLASS,
           nil,
           nil,
           start,
           ProcessInfo) then
     begin
       repeat
        Apprunning := WaitForSingleObject
                   (ProcessInfo.hProcess,100) ;
        Application.ProcessMessages;
        until (Apprunning <> WAIT_TIMEOUT) ;
        Repeat
         BytesRead := 0;
         ReadFile(ReadPipe, Buffer[0], ReadBuffer,BytesRead,nil) ;
         Buffer[BytesRead]:= #0;
         OemToAnsi(Buffer,Buffer) ;
         AMemo.Text := AMemo.text + String(Buffer) ;
        until (BytesRead < ReadBuffer) ;
     end;
    FreeMem(Buffer) ;
    CloseHandle(ProcessInfo.hProcess) ;
    CloseHandle(ProcessInfo.hThread) ;
    CloseHandle(ReadPipe) ;
    CloseHandle(WritePipe) ;
   end;
end;