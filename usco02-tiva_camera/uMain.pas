unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.StdCtrls, System.StrUtils,
  CPort, Data.DB, Vcl.Grids, Vcl.DBGrids, Vcl.DBCtrls, Vcl.DBCGrids, Vcl.Menus;

type
  TForm3 = class(TForm)
    Panel1: TPanel;
    PageControl1: TPageControl;
    tiSenaryo: TTabSheet;
    EditKomut: TEdit;
    btnKomutGoner: TButton;
    Timer1: TTimer;
    DBGrid1: TDBGrid;
    DBGrid2: TDBGrid;
    Panel2: TPanel;
    DBNavigator1: TDBNavigator;
    DBNavigator2: TDBNavigator;
    btnBaglan: TButton;
    btnRun: TButton;
    tiCihaz: TTabSheet;
    DBGrid3: TDBGrid;
    DBNavigator3: TDBNavigator;
    tiLog: TTabSheet;
    MemoLog: TMemo;
    TabSheet1: TTabSheet;
    MemoCOM: TMemo;
    PopupMenu: TPopupMenu;
    ReceteyiKopyala1: TMenuItem;
    PopupMenuIO: TPopupMenu;
    IOOlutur1: TMenuItem;
    BtnIOEkle1: TMenuItem;
    ComPort1: TComPort;
    btnComportATA: TButton;
    procedure btnKomutGonerClick(Sender: TObject);
    procedure btnBaglanClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ComDataPacket2Packet(Sender: TObject; const Str: string);
    procedure btnComportATAClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ReceteyiKopyala1Click(Sender: TObject);
    procedure IOOlutur1Click(Sender: TObject);
    procedure BtnIOEkle1Click(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    function KomutOlustur(fPin, fstatu: SmallInt): String;
    function KomutGonder(komut: string): Boolean;
    function ComPort(l_port: String): TCommConfig;
    function Run: Boolean;
  end;

var
  Form3: TForm3;
  Sure: Cardinal;
  Pin: SmallInt;
  Statu: SmallInt;
  fComFileName: String;
  pBekle: Boolean;
  pkomut: AnsiString;
  pResetKomut: AnsiString;

implementation

{$R *.dfm}

uses uDmod;

procedure TForm3.btnBaglanClick(Sender: TObject);
begin
  with frmDmod do
  begin
    var
    p := ExtractFileDir(Application.ExeName) + PathDelim + 'CameraControl.db';

    FDConnection1.Params.Database := p;
    try
      FDConnection1.Open;

      qCreateTables.ExecSQL;

    finally

      qSENARYO.Open();
      qDATAS.Open();
      qIO.Open();
    end;

  end;
  EditKomut.Enabled := True;
  btnKomutGoner.Enabled := True;
  btnRun.Enabled := True;
  if FileExists(fComFileName) then
    MemoCOM.Lines.LoadFromFile(fComFileName);
end;

procedure TForm3.BtnIOEkle1Click(Sender: TObject);
begin
  with frmDmod do
  begin
    qIO.First;
    for var I := 0 to qIO.RecordCount - 1 do
    begin
      qDATAS.Insert;
      qDATASAD.Value := qIOIO_AD.Value;
      qDATAS.Post;
      qio.Next;

    end;
  end;

end;

procedure TForm3.btnKomutGonerClick(Sender: TObject);
VAR
  Str: STRING;
begin
  ComPort1.Open;
  Str := 'do 1 0' + #13#10 + ' do 2 0' + #13#10;
  // ComPort1.WriteStr(STR);
  ComPort1.WriteStr(EditKomut.Text + #13#10);
end;

procedure TForm3.btnRunClick(Sender: TObject);
begin
  with frmDmod do
  begin
    qSENARYO.First;
    while not qSENARYO.Eof do
    begin
     if qSENARYOAKTIF.value then
     begin
      if run  then  
          qSENARYO.Next
         else
         begin
          qSENARYO.last; 
         end;
     end  
     else
      qSENARYO.Next;
     end;
  end;
end;

procedure TForm3.btnComportATAClick(Sender: TObject);
begin
  try
    if ComPort1.Connected then
      ComPort1.Close;
    ComPort1.Port := (MemoCOM.Text);

  finally
    ComPort1.Open;
    MemoCOM.Lines.SaveToFile(fComFileName);
  end;
end;

procedure TForm3.ComDataPacket2Packet(Sender: TObject; const Str: string);
begin
  MemoLog.Lines.Add(Str);
end;

function TForm3.ComPort(l_port: String): TCommConfig;
{ Gets the comm port settings (use '\\.\' for com 10..99) }
var
  size: Cardinal;
  CommConfig: TCommConfig;
begin
  FillChar(Result, SizeOf(TCommConfig), 0);

  // strip trailing : as it does not work with it
  if (RightStr(l_port, 1) = ':') then
    l_port := LeftStr(l_port, Length(l_port) - 1);
  try
    FillChar(CommConfig, SizeOf(TCommConfig), 0);
    CommConfig.dwSize := SizeOf(TCommConfig);
    size := SizeOf(TCommConfig);
    if (GetDefaultCommConfig(PChar(l_port), CommConfig, size)) then
    begin
      Result := CommConfig;
    end
    // if port is not found add unc path and check again
    else if (GetDefaultCommConfig(PChar('\\.\' + l_port), CommConfig, size))
    then
    begin
      Result := CommConfig;
    end
  except
    Showmessage('Unable to open port ' + l_port);
  end;
end;

procedure TForm3.FormShow(Sender: TObject);
begin
  fComFileName := 'comsettings.ini';
end;

procedure TForm3.IOOlutur1Click(Sender: TObject);
begin
  with frmDmod do
  begin
    qIO.Open();
    for var I := 0 to 76 do
    begin
      qIO.Insert;
      qIOIO_AD.Value := i.ToString;
      qIOIO_NO.Value := i;
      qIO.Post;
    end;
    qIO.Insert;
    qIOIO_AD.Value := 'DELAY100';
    qIOIO_NO.Value := 100;
    qIO.Post;
    qIO.Insert;
    qIOIO_AD.Value := 'DELAY10';
    qIOIO_NO.Value := 10;
    qIO.Insert;
    qIOIO_AD.Value := 'DELAY50';
    qIOIO_NO.Value := 50;
    qIO.Post;
    qIO.Insert;
    qIOIO_AD.Value := 'DELAY1000';
    qIOIO_NO.Value := 1000;
    qIO.Post;

  end;
end;

function TForm3.KomutGonder(komut: string): Boolean;
begin
  // komut gönder
  try
    if not ComPort1.Connected then
      ComPort1.Open;
    ComPort1.WriteStr((komut));

  finally
    // ComPort1.Close;
    Result := True;
  end;
end;

function TForm3.KomutOlustur(fPin, fstatu: SmallInt): String;
begin
  // Süre
  Result := (' do ' + fPin.ToString + ' ' + fstatu.ToString + #13#10);

end;

procedure TForm3.ReceteyiKopyala1Click(Sender: TObject);
begin

  with frmDmod do
  begin
    memDatas.Open;
    qDATAS.First;
    while not qDATAS.Eof do
    begin
      memDatas.Insert;
      memDatasAD.Value := qDATASAD.Value;
      memDatas.Post;
      qDATAS.Next;
    end;
    var
    seneryoad := qSENARYOAD.Value;
    qSENARYO.Insert;
    qSENARYOAD.Value := 'Copy ' + seneryoad;
    qSENARYO.Post;
    memDatas.First;
    while not memDatas.Eof do
    begin
      qDATAS.Insert;
      qDATASAD.Value := memDatasAD.Value;
      qDATAS.Post;
      memDatas.Next;
    end;

  end;
end;

function TForm3.Run: Boolean;
var
  I: integer;
begin
  try
    Result := False;
    btnRun.Enabled := False;
    pkomut := '';
    pResetKomut := '';
    ComPort1.Open;
    PageControl1.ActivePage := tiLog;
    with frmDmod do
    begin
      qDATAS.DisableControls;
      qDATAS.Last;
      qDATAS.First;
      for I := 0 to qDATAS.RecordCount do
      begin
        VAR
        S := qDATASCIHAZ.AsString;
        if i = qDATAS.RecordCount then
          S := 'DELAY';
        if ((S.StartsWith('DELAY')) OR (S.StartsWith('delay')) OR
          (S.StartsWith('Delay'))) then
        begin
          // Komut Gonder;
          MemoLog.Lines.Add(pkomut);
          KomutGonder(pkomut);
          // MemoLog.Lines.Add(' Delay ' + S + ' ' + TimeToStr(Time));
          Form3.Caption := 'Delay';
          pBekle := False;
          VAR
          DELAYINT := qDATASPIN.Value;
          if DELAYINT < 1 then
            DELAYINT := 1;
          Timer1.Interval := DELAYINT;
          Timer1.Enabled := True;
          pkomut := '';
          // KomutGonder('RESET' + #13#10);
          while not pBekle do
          begin
            Application.ProcessMessages;
          end;

          MemoLog.Lines.Add(pResetKomut);
          KomutGonder(pResetKomut);
          pResetKomut := '';
          // MemoLog.Lines.Add(' Delay Bitiþ ' + S + ' ' + TimeToStr(Time));
        end
        else
        begin
          Form3.Caption := 'Set';
          pkomut := pkomut + KomutOlustur(qDATASPIN.Value, 1);
          pResetKomut := pResetKomut + KomutOlustur(qDATASPIN.Value, 0);
        end;
        qDATAS.Next;
      end;
      qDATAS.EnableControls;
    end;
    btnRun.Enabled := True;
    Result := True;
  except
    on E: Exception do
    begin
      Result := False;
      MemoLog.Lines.Add(E.Message);
    end;
  end;

end;

procedure TForm3.Timer1Timer(Sender: TObject);

begin
  Timer1.Enabled := False;
  pBekle := True;

end;

end.
