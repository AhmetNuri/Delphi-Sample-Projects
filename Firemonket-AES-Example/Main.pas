// this project use https://github.com/SnakeDoctor/DCPcrypt librarry
// this is test on Delphi 10.4 Windows and Android
// This is example for AES 256 cBC (Base64 Encode) with pm PKCS7
// it is working on Firemonkey Windows and Android. Alose it should work on IOS and MacOS
// these  is compatible with online tools



unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,


  Prism.Crypto.AES, System.NetEncoding, FMX.TabControl, FMX.Edit, FMX.ListBox,
  FMX.Layouts;

type
  TForm2 = class(TForm)
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    Memo1: TMemo;
    Panel1: TPanel;
    btnEncode: TButton;
    BtnDecode: TButton;
    ListBox1: TListBox;
    ListBoxGroupHeader1: TListBoxGroupHeader;
    ListBoxItem1: TListBoxItem;
    ListBoxGroupHeader2: TListBoxGroupHeader;
    ListBoxItem2: TListBoxItem;
    ListBoxGroupHeader3: TListBoxGroupHeader;
    ListBoxItem3: TListBoxItem;
    EditStr: TEdit;
    EditKey: TEdit;
    EditVI: TEdit;
    ListBoxHeader1: TListBoxHeader;
    procedure btnEncodeClick(Sender: TObject);
    procedure BtnDecodeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.BtnDecodeClick(Sender: TObject);
 var
  OriginalText, Key, IV, EncryptedText: TBytes;
  Txt : string;
begin
 // OriginalText := TEncoding.ANSI.GetBytes('+GJaNsm7QtR+egJmfER9foaJYOpcvraHCcRhAXmgL0k=');
  OriginalText := TNetEncoding.Base64.DecodeStringToBytes(EditStr.Text) ;
  Key := TEncoding.ANSI.GetBytes(EditKey.Text); // 256 bits-32 bytes
  IV := TEncoding.ANSI.GetBytes(EditVI.Text); // 16 bytes
  EncryptedText := TAES.Decrypt(OriginalText, Key, 256, IV, cmCBC, pmPKCS7);
//    txt := TNetEncoding.Base64.DecodeStringToBytes(EncryptedText);
    Memo1.Text :=   TEncoding.UTF8.GetString( EncryptedText) ; //TNetEncoding.Base64.DecEncodeBytesToString(EncryptedText);
  // Output: L/5zwPlqWDSWPy6LbQASgmZF2/cD33ecs/hHeDTUSu0=

end;

procedure TForm2.btnEncodeClick(Sender: TObject);
var
  OriginalText, Key, IV, EncryptedText: TBytes;
begin

  OriginalText := TEncoding.UTF8.GetBytes( EditStr.Text);
//  OriginalText := TEncoding.ANSI.GetBytes( EditStr.Text);
  Key := TEncoding.ANSI.GetBytes(EditKey.Text); // 256 bits-32 bytes
  IV := TEncoding.ANSI.GetBytes(EditVI.Text); // 16 bytes

  EncryptedText := TAES.Encrypt(OriginalText, Key, 256, IV, cmCBC, pmPKCS7);
  Memo1.Text := TNetEncoding.Base64.EncodeBytesToString(EncryptedText);
  // Output: L/5zwPlqWDSWPy6LbQASgmZF2/cD33ecs/hHeDTUSu0=

end;

end.
