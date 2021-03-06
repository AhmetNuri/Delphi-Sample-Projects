unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.OleCtrls, SHDocVw,
  Vcl.ToolWin, Vcl.ComCtrls;

type
  TForm2 = class(TForm)
    WebBrowser1: TWebBrowser;
    Memo1: TMemo;
    ToolBar1: TToolBar;
    Button1: TButton;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

uses UWebBrowserWrapper;

procedure TForm2.Button1Click(Sender: TObject);
var
Wb : TWebBrowserWrapper;
begin
  wb := TWebBrowserWrapper.Create(WebBrowser1) ;
  try
   wb.NavigateToURL(Edit1.Text)  ;

  finally
  Memo1.Lines.Text :=   wb.SaveToString;
  end;
 //  wb.CopyAll;
end;

end.
