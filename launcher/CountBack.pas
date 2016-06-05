unit CountBack;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TBreakoutForm = class(TForm)
    btnCancel: TButton;
    lbInfo: TLabel;
    tmrDownCount: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure tmrDownCountTimer(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    { Private declarations }

  public
    { Public declarations }

    nTimeout: Integer;

  end;

var
  BreakoutForm: TBreakoutForm;

implementation
uses Misc;

resourcestring
   restart_label = 'Игра будет перезапущена через %d сек ';


{$R *.dfm}

procedure TBreakoutForm.btnCancelClick(Sender: TObject);
begin
 ODS('[~T]. #DBG: btnCancel pressed');
 ModalResult := mrAbort;
end;

procedure TBreakoutForm.FormCreate(Sender: TObject);
begin
 nTimeout := 3;
end;

procedure TBreakoutForm.tmrDownCountTimer(Sender: TObject);
begin
 Dec (nTimeout);

 lbInfo.Caption := Format( restart_label, [nTimeout] );

 if nTimeout <= 0 then
    ModalResult := mrOk;
end;

end.
