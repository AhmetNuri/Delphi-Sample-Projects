program AESFMX;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form2},
  Prism.Crypto.AES in 'Prism.Crypto.AES.pas',
  DCPbase64 in 'DCPbase64.pas',
  DCPblockciphers in 'DCPblockciphers.pas',
  DCPconst in 'DCPconst.pas',
  DCPcrypt2 in 'DCPcrypt2.pas',
  DCPrijndael in 'DCPrijndael.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
