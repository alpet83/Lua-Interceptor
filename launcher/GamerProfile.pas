unit GamerProfile;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Mask, Misc;

type
  TNewProfileDialog = class(TForm)
    Label1: TLabel;
    edtName: TMaskEdit;
    edtPIN: TMaskEdit;
    Label2: TLabel;
    btnOK: TButton;
    Button2: TButton;
    procedure edtNameChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  NewProfileDialog: TNewProfileDialog;

implementation

{$R *.dfm}

procedure TNewProfileDialog.edtNameChange (Sender: TObject);
var
   s: String;
begin
 s := edtName.Text;
 btnOK.Enabled := ( Length (s) > 4 );

 ReplaceChars ( s, [#0..#31, '\', '/', '<', '>', '?', '*', ':', '|'], ' ' );

 s := Copy (s, 1, 10);

 if s <> edtName.Text then
    edtName.Text := s;
end;

end.
