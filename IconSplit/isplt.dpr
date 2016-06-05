program isplt;

uses
  FastMM4, FastMM4Messages,
  Windows, Misc, Vcl.Forms,
  MainForm in 'MainForm.pas' {ISPMainForm};

{$R *.res}

begin
  StartLogging('');
  ShowConsole();
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TISPMainForm, ISPMainForm);
  Application.Run;
end.
