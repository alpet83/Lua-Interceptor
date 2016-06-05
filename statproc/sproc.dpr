program sproc;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {MForm},
  ExStringGrid in '..\..\lib\components\ExStringGrid.pas';

{$R *.res}


var
   f: Single;
   d: Cardinal absolute f;


begin
  d := $4f800000;
  if f > 0 then
    asm
     mov [ecx + $FC], eax
     add [ecx + $FC], eax


     cmp eax, ebx
     je @test
     jmp @nope
     nop
@test:
     inc eax
@nope:


    end;

  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMForm, MForm);
  Application.Run;
end.
