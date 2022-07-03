program UscoKamera;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form3},
  uDmod in 'uDmod.pas' {frmDmod: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TfrmDmod, frmDmod);
  Application.Run;
end.
