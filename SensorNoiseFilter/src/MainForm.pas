{ ============================================================================
  MainForm.pas - View Katmanı
  
  Kullanıcı arayüzü: CSV yükleme, filtre seçimi, parametre ayarları,
  TPaintBox ile grafik çizimi ve gürültü noktalarının renkli gösterimi.
  
  MVC Mimarisi: VIEW
  ============================================================================ }
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
  SensorDataModel, SignalFilters, FilterController;

type
  TFormMain = class(TForm)
    { Üst Panel - Kontroller }
    PanelTop: TPanel;
    LabelFile: TLabel;
    LabelFilter: TLabel;
    LabelAxis: TLabel;
    LabelParam1: TLabel;
    LabelParam2: TLabel;
    LabelSigma: TLabel;
    EditFilePath: TEdit;
    BtnLoadCSV: TButton;
    ComboFilter: TComboBox;
    ComboAxis: TComboBox;
    EditParam1: TEdit;
    EditParam2: TEdit;
    BtnApplyFilter: TButton;
    EditSigma: TEdit;
    BtnDetectNoise: TButton;
    BtnExportFiltered: TButton;
    CheckShowRaw: TCheckBox;
    CheckShowFiltered: TCheckBox;

    { Grafik Paneli }
    PanelGraph: TPanel;
    PaintBoxGraph: TPaintBox;

    { Alt Panel - İstatistikler }
    PanelBottom: TPanel;
    PanelStats: TPanel;
    LabelStatsTitle: TLabel;
    LabelRawStats: TLabel;
    LabelFilteredStats: TLabel;
    LabelNoiseInfo: TLabel;
    LabelLegend: TLabel;
    StatusBar: TStatusBar;

    { Diyaloglar }
    OpenDialogCSV: TOpenDialog;
    SaveDialogCSV: TSaveDialog;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure BtnLoadCSVClick(Sender: TObject);
    procedure BtnApplyFilterClick(Sender: TObject);
    procedure BtnDetectNoiseClick(Sender: TObject);
    procedure BtnExportFilteredClick(Sender: TObject);
    procedure ComboFilterChange(Sender: TObject);
    procedure ComboAxisChange(Sender: TObject);
    procedure PaintBoxGraphPaint(Sender: TObject);
    procedure PaintBoxGraphMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxGraphMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxGraphMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckShowRawClick(Sender: TObject);
    procedure CheckShowFilteredClick(Sender: TObject);
  private
    FController: TFilterController;

    { Grafik çizim değişkenleri }
    FGraphMarginLeft: Integer;
    FGraphMarginRight: Integer;
    FGraphMarginTop: Integer;
    FGraphMarginBottom: Integer;
    FIsPanning: Boolean;
    FPanStartX: Integer;
    FOffsetX: Integer;
    FZoomFactor: Double;
    FShowRawData: Boolean;
    FShowFilteredData: Boolean;

    { Controller olay işleyicileri }
    procedure OnDataLoaded(Sender: TObject; ARecordCount: Integer);
    procedure OnFilterApplied(Sender: TObject; AFilterName: string);
    procedure OnNoiseDetected(Sender: TObject; ANoiseCountX, ANoiseCountY, ANoiseCountZ: Integer);
    procedure OnStatusMessage(Sender: TObject; const AMessage: string);
    procedure OnStatisticsUpdated(Sender: TObject;
      const ARawStats, AFilteredStats: TStatistics; AAxis: TAxisType);

    { Yardımcı metodlar }
    procedure InitializeFilterCombo;
    procedure UpdateParamLabels;
    function ReadFilterParams: TFilterParams;
    procedure DrawGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);
    procedure DrawAxisLines(ACanvas: TCanvas; AWidth, AHeight: Integer;
      AMinVal, AMaxVal: Double);
    procedure DrawDataSeries(ACanvas: TCanvas; const AData: TArray<Double>;
      AColor: TColor; AWidth, AHeight: Integer; AMinVal, AMaxVal: Double;
      ADrawPoints: Boolean);
    procedure DrawNoiseMarkers(ACanvas: TCanvas; const AData: TArray<Double>;
      AWidth, AHeight: Integer; AMinVal, AMaxVal: Double);
    function DataXToScreenX(AIndex, ADataCount, AWidth: Integer): Integer;
    function DataYToScreenY(AValue, AMinVal, AMaxVal: Double; AHeight: Integer): Integer;
    procedure ExportFilteredCSV(const AFileName: string);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

{ TFormMain }

procedure TFormMain.FormCreate(Sender: TObject);
begin
  { Grafik kenar boşlukları }
  FGraphMarginLeft := 70;
  FGraphMarginRight := 20;
  FGraphMarginTop := 20;
  FGraphMarginBottom := 30;
  FOffsetX := 0;
  FZoomFactor := 1.0;
  FIsPanning := False;
  FShowRawData := True;
  FShowFilteredData := True;

  { Controller oluştur ve olayları bağla }
  FController := TFilterController.Create;
  FController.OnDataLoaded := OnDataLoaded;
  FController.OnFilterApplied := OnFilterApplied;
  FController.OnNoiseDetected := OnNoiseDetected;
  FController.OnStatusMessage := OnStatusMessage;
  FController.OnStatisticsUpdated := OnStatisticsUpdated;

  { Filtre combobox'ını doldur }
  InitializeFilterCombo;

  { Durum çubuğu başlat }
  StatusBar.Panels[0].Text := 'Hazır - CSV dosyası yükleyin.';
  StatusBar.Panels[1].Text := 'Kayıt: 0';
  StatusBar.Panels[2].Text := '';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FController.Free;
end;

procedure TFormMain.InitializeFilterCombo;
var
  FT: TFilterType;
begin
  ComboFilter.Items.Clear;
  for FT := Low(TFilterType) to High(TFilterType) do
    ComboFilter.Items.Add(TFilterFactory.GetFilterName(FT));
  ComboFilter.ItemIndex := 0;
  UpdateParamLabels;
end;

procedure TFormMain.UpdateParamLabels;
begin
  case TFilterType(ComboFilter.ItemIndex) of
    ftMovingAverage:
    begin
      LabelParam1.Caption := 'Pencere Boyutu:';
      LabelParam2.Caption := '(kullanılmıyor)';
      EditParam1.Text := '5';
      EditParam2.Text := '';
      EditParam2.Enabled := False;
    end;
    ftMedian:
    begin
      LabelParam1.Caption := 'Pencere Boyutu:';
      LabelParam2.Caption := '(kullanılmıyor)';
      EditParam1.Text := '5';
      EditParam2.Text := '';
      EditParam2.Enabled := False;
    end;
    ftLowPass:
    begin
      LabelParam1.Caption := 'Alpha (0-1):';
      LabelParam2.Caption := '(kullanılmıyor)';
      EditParam1.Text := '0.1';
      EditParam2.Text := '';
      EditParam2.Enabled := False;
    end;
    ftHighPass:
    begin
      LabelParam1.Caption := 'Alpha (0-1):';
      LabelParam2.Caption := '(kullanılmıyor)';
      EditParam1.Text := '0.1';
      EditParam2.Text := '';
      EditParam2.Enabled := False;
    end;
    ftBandPass:
    begin
      LabelParam1.Caption := 'Alt Alpha:';
      LabelParam2.Caption := 'Üst Alpha:';
      EditParam1.Text := '0.05';
      EditParam2.Text := '0.3';
      EditParam2.Enabled := True;
    end;
    ftKalman:
    begin
      LabelParam1.Caption := 'Q (Süreç):';
      LabelParam2.Caption := 'R (Ölçüm):';
      EditParam1.Text := '0.01';
      EditParam2.Text := '0.1';
      EditParam2.Enabled := True;
    end;
    ftExponentialSmoothing:
    begin
      LabelParam1.Caption := 'Alpha (0-1):';
      LabelParam2.Caption := '(kullanılmıyor)';
      EditParam1.Text := '0.3';
      EditParam2.Text := '';
      EditParam2.Enabled := False;
    end;
  end;
end;

function TFormMain.ReadFilterParams: TFilterParams;
var
  FS: TFormatSettings;
begin
  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';

  Result := TFilterParams.Default;

  case TFilterType(ComboFilter.ItemIndex) of
    ftMovingAverage, ftMedian:
      Result.WindowSize := StrToIntDef(EditParam1.Text, 5);
    ftLowPass:
      Result.AlphaLow := StrToFloatDef(EditParam1.Text, 0.1, FS);
    ftHighPass:
      Result.AlphaHigh := StrToFloatDef(EditParam1.Text, 0.1, FS);
    ftBandPass:
    begin
      Result.AlphaLow := StrToFloatDef(EditParam1.Text, 0.05, FS);
      Result.AlphaHigh := StrToFloatDef(EditParam2.Text, 0.3, FS);
    end;
    ftKalman:
    begin
      Result.KalmanQ := StrToFloatDef(EditParam1.Text, 0.01, FS);
      Result.KalmanR := StrToFloatDef(EditParam2.Text, 0.1, FS);
    end;
    ftExponentialSmoothing:
      Result.ExponentialAlpha := StrToFloatDef(EditParam1.Text, 0.3, FS);
  end;
end;

{ =================== Buton Olayları =================== }

procedure TFormMain.BtnLoadCSVClick(Sender: TObject);
begin
  if OpenDialogCSV.Execute then
  begin
    EditFilePath.Text := OpenDialogCSV.FileName;
    try
      FController.LoadCSV(OpenDialogCSV.FileName);
    except
      on E: Exception do
        ShowMessage('CSV yükleme hatası: ' + E.Message);
    end;
  end;
end;

procedure TFormMain.BtnApplyFilterClick(Sender: TObject);
begin
  FController.SetFilterParams(ReadFilterParams);
  FController.SelectFilter(TFilterType(ComboFilter.ItemIndex));
  FController.ApplyFilter;
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.BtnDetectNoiseClick(Sender: TObject);
var
  FS: TFormatSettings;
  Sigma: Double;
begin
  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';
  Sigma := StrToFloatDef(EditSigma.Text, 2.0, FS);
  FController.DetectNoise(Sigma);
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.BtnExportFilteredClick(Sender: TObject);
begin
  if FController.GetModel.Count = 0 then
  begin
    ShowMessage('Dışa aktarılacak veri yok.');
    Exit;
  end;

  if SaveDialogCSV.Execute then
    ExportFilteredCSV(SaveDialogCSV.FileName);
end;

procedure TFormMain.ComboFilterChange(Sender: TObject);
begin
  UpdateParamLabels;
end;

procedure TFormMain.ComboAxisChange(Sender: TObject);
begin
  case ComboAxis.ItemIndex of
    0: FController.SelectAxis(atX);
    1: FController.SelectAxis(atY);
    2: FController.SelectAxis(atZ);
  end;
  FController.UpdateStatistics;
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.CheckShowRawClick(Sender: TObject);
begin
  FShowRawData := CheckShowRaw.Checked;
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.CheckShowFilteredClick(Sender: TObject);
begin
  FShowFilteredData := CheckShowFiltered.Checked;
  PaintBoxGraph.Invalidate;
end;

{ =================== Controller Olay İşleyicileri =================== }

procedure TFormMain.OnDataLoaded(Sender: TObject; ARecordCount: Integer);
begin
  StatusBar.Panels[1].Text := Format('Kayıt: %d', [ARecordCount]);
  FOffsetX := 0;
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.OnFilterApplied(Sender: TObject; AFilterName: string);
begin
  StatusBar.Panels[2].Text := AFilterName;
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.OnNoiseDetected(Sender: TObject; ANoiseCountX, ANoiseCountY, ANoiseCountZ: Integer);
begin
  LabelNoiseInfo.Caption := Format(
    'Gürültü Noktaları: X=%d, Y=%d, Z=%d (Sigma x%.1f)',
    [ANoiseCountX, ANoiseCountY, ANoiseCountZ,
     FController.GetModel.NoiseSigmaMultiplier]);
  PaintBoxGraph.Invalidate;
end;

procedure TFormMain.OnStatusMessage(Sender: TObject; const AMessage: string);
begin
  StatusBar.Panels[0].Text := AMessage;
end;

procedure TFormMain.OnStatisticsUpdated(Sender: TObject;
  const ARawStats, AFilteredStats: TStatistics; AAxis: TAxisType);
var
  AxisName: string;
begin
  case AAxis of
    atX: AxisName := 'X';
    atY: AxisName := 'Y';
    atZ: AxisName := 'Z';
  end;

  LabelRawStats.Caption := Format(
    'Ham Veri [%s]: Ort=%.4f | StdSap=%.4f | Min=%.4f | Maks=%.4f | N=%d',
    [AxisName, ARawStats.Mean, ARawStats.StdDev, ARawStats.Min, ARawStats.Max, ARawStats.Count]);

  if AFilteredStats.Count > 0 then
    LabelFilteredStats.Caption := Format(
      'Filtreli [%s]: Ort=%.4f | StdSap=%.4f | Min=%.4f | Maks=%.4f | N=%d',
      [AxisName, AFilteredStats.Mean, AFilteredStats.StdDev,
       AFilteredStats.Min, AFilteredStats.Max, AFilteredStats.Count])
  else
    LabelFilteredStats.Caption := 'Filtreli Veri: Henüz filtre uygulanmadı.';
end;

{ =================== Grafik Çizimi =================== }

procedure TFormMain.PaintBoxGraphPaint(Sender: TObject);
begin
  DrawGraph(PaintBoxGraph.Canvas, PaintBoxGraph.Width, PaintBoxGraph.Height);
end;

procedure TFormMain.DrawGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);
var
  RawData, FilteredData: TArray<Double>;
  MinVal, MaxVal, Range: Double;
  I: Integer;
  Model: TSensorDataModel;
  Rec: TSensorRecord;
  IsNoise: Boolean;
begin
  { Arka plan }
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(Rect(0, 0, AWidth, AHeight));

  Model := FController.GetModel;
  if Model.Count = 0 then
  begin
    ACanvas.Font.Size := 12;
    ACanvas.Font.Color := clGray;
    ACanvas.TextOut(AWidth div 2 - 120, AHeight div 2 - 10,
      'CSV dosyası yükleyin...');
    Exit;
  end;

  RawData := FController.GetRawData;
  FilteredData := FController.GetFilteredData;

  { Min/Max hesapla }
  MinVal := RawData[0];
  MaxVal := RawData[0];
  for I := 0 to Length(RawData) - 1 do
  begin
    if RawData[I] < MinVal then MinVal := RawData[I];
    if RawData[I] > MaxVal then MaxVal := RawData[I];
  end;

  if Length(FilteredData) > 0 then
  begin
    for I := 0 to Length(FilteredData) - 1 do
    begin
      if FilteredData[I] < MinVal then MinVal := FilteredData[I];
      if FilteredData[I] > MaxVal then MaxVal := FilteredData[I];
    end;
  end;

  { Biraz boşluk ekle }
  Range := MaxVal - MinVal;
  if Range < 0.001 then
    Range := 1.0;
  MinVal := MinVal - Range * 0.1;
  MaxVal := MaxVal + Range * 0.1;

  { Eksen çizgileri }
  DrawAxisLines(ACanvas, AWidth, AHeight, MinVal, MaxVal);

  { Filtrelenmiş veri - yeşil (önce çiz, ham verinin altında kalsın) }
  if FShowFilteredData and (Length(FilteredData) > 0) then
    DrawDataSeries(ACanvas, FilteredData, clGreen, AWidth, AHeight, MinVal, MaxVal, False);

  { Ham veri - mavi }
  if FShowRawData then
    DrawDataSeries(ACanvas, RawData, clBlue, AWidth, AHeight, MinVal, MaxVal, True);

  { Gürültü noktaları - kırmızı }
  DrawNoiseMarkers(ACanvas, RawData, AWidth, AHeight, MinVal, MaxVal);

  { Başlık }
  ACanvas.Font.Size := 9;
  ACanvas.Font.Color := clBlack;
  ACanvas.Font.Style := [fsBold];
  ACanvas.TextOut(FGraphMarginLeft + 5, 3, Format('Sensör Verisi Grafigi - %d Nokta',
    [Length(RawData)]));
  ACanvas.Font.Style := [];
end;

procedure TFormMain.DrawAxisLines(ACanvas: TCanvas; AWidth, AHeight: Integer;
  AMinVal, AMaxVal: Double);
var
  GraphLeft, GraphRight, GraphTop, GraphBottom: Integer;
  I, NumTicks: Integer;
  Val: Double;
  YPos: Integer;
begin
  GraphLeft := FGraphMarginLeft;
  GraphRight := AWidth - FGraphMarginRight;
  GraphTop := FGraphMarginTop;
  GraphBottom := AHeight - FGraphMarginBottom;

  { Çerçeve }
  ACanvas.Pen.Color := clSilver;
  ACanvas.Pen.Width := 1;
  ACanvas.Rectangle(GraphLeft, GraphTop, GraphRight, GraphBottom);

  { Y ekseni etiketleri }
  ACanvas.Font.Size := 7;
  ACanvas.Font.Color := clGray;
  NumTicks := 8;

  for I := 0 to NumTicks do
  begin
    Val := AMinVal + (AMaxVal - AMinVal) * I / NumTicks;
    YPos := DataYToScreenY(Val, AMinVal, AMaxVal, AHeight);

    { Yatay kılavuz çizgisi }
    ACanvas.Pen.Color := $00F0F0F0;
    ACanvas.Pen.Style := psDot;
    ACanvas.MoveTo(GraphLeft, YPos);
    ACanvas.LineTo(GraphRight, YPos);
    ACanvas.Pen.Style := psSolid;

    { Etiket }
    ACanvas.TextOut(2, YPos - 6, Format('%.2f', [Val]));
  end;
end;

procedure TFormMain.DrawDataSeries(ACanvas: TCanvas; const AData: TArray<Double>;
  AColor: TColor; AWidth, AHeight: Integer; AMinVal, AMaxVal: Double;
  ADrawPoints: Boolean);
var
  I, N: Integer;
  X1, Y1, X2, Y2: Integer;
begin
  N := Length(AData);
  if N < 2 then
    Exit;

  { Çizgi }
  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 1;

  X1 := DataXToScreenX(0, N, AWidth);
  Y1 := DataYToScreenY(AData[0], AMinVal, AMaxVal, AHeight);
  ACanvas.MoveTo(X1, Y1);

  for I := 1 to N - 1 do
  begin
    X2 := DataXToScreenX(I, N, AWidth);
    Y2 := DataYToScreenY(AData[I], AMinVal, AMaxVal, AHeight);
    ACanvas.LineTo(X2, Y2);
  end;

  { Noktalar }
  if ADrawPoints then
  begin
    ACanvas.Brush.Color := AColor;
    ACanvas.Pen.Color := AColor;
    for I := 0 to N - 1 do
    begin
      X1 := DataXToScreenX(I, N, AWidth);
      Y1 := DataYToScreenY(AData[I], AMinVal, AMaxVal, AHeight);
      ACanvas.Ellipse(X1 - 3, Y1 - 3, X1 + 3, Y1 + 3);
    end;
  end;
end;

procedure TFormMain.DrawNoiseMarkers(ACanvas: TCanvas; const AData: TArray<Double>;
  AWidth, AHeight: Integer; AMinVal, AMaxVal: Double);
var
  I, N: Integer;
  Model: TSensorDataModel;
  Rec: TSensorRecord;
  IsNoise: Boolean;
  X1, Y1: Integer;
  SelAxis: TAxisType;
begin
  Model := FController.GetModel;
  N := Model.Count;
  SelAxis := FController.GetSelectedAxis;

  ACanvas.Brush.Color := clRed;
  ACanvas.Pen.Color := clMaroon;
  ACanvas.Pen.Width := 2;

  for I := 0 to N - 1 do
  begin
    Rec := Model.GetRecord(I);

    case SelAxis of
      atX: IsNoise := Rec.IsNoiseX;
      atY: IsNoise := Rec.IsNoiseY;
      atZ: IsNoise := Rec.IsNoiseZ;
    else
      IsNoise := False;
    end;

    if IsNoise then
    begin
      X1 := DataXToScreenX(I, N, AWidth);
      Y1 := DataYToScreenY(AData[I], AMinVal, AMaxVal, AHeight);

      { Kırmızı büyük daire ile gürültü noktası }
      ACanvas.Ellipse(X1 - 6, Y1 - 6, X1 + 6, Y1 + 6);

      { Çarpı işareti }
      ACanvas.MoveTo(X1 - 5, Y1 - 5);
      ACanvas.LineTo(X1 + 5, Y1 + 5);
      ACanvas.MoveTo(X1 + 5, Y1 - 5);
      ACanvas.LineTo(X1 - 5, Y1 + 5);
    end;
  end;

  ACanvas.Pen.Width := 1;
end;

function TFormMain.DataXToScreenX(AIndex, ADataCount, AWidth: Integer): Integer;
var
  GraphWidth: Integer;
begin
  GraphWidth := AWidth - FGraphMarginLeft - FGraphMarginRight;
  if ADataCount <= 1 then
    Result := FGraphMarginLeft + GraphWidth div 2
  else
    Result := FGraphMarginLeft + FOffsetX +
      Round((AIndex / (ADataCount - 1)) * GraphWidth * FZoomFactor);
end;

function TFormMain.DataYToScreenY(AValue, AMinVal, AMaxVal: Double;
  AHeight: Integer): Integer;
var
  GraphHeight: Integer;
  Ratio: Double;
begin
  GraphHeight := AHeight - FGraphMarginTop - FGraphMarginBottom;
  if Abs(AMaxVal - AMinVal) < 1E-10 then
    Ratio := 0.5
  else
    Ratio := (AValue - AMinVal) / (AMaxVal - AMinVal);
  { Y ekseni ters (ekranın üstü 0) }
  Result := FGraphMarginTop + GraphHeight - Round(Ratio * GraphHeight);
end;

{ =================== Fare Etkileşimi (Kaydırma) =================== }

procedure TFormMain.PaintBoxGraphMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FIsPanning := True;
    FPanStartX := X - FOffsetX;
    PaintBoxGraph.Cursor := crHandPoint;
  end;
end;

procedure TFormMain.PaintBoxGraphMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if FIsPanning then
  begin
    FOffsetX := X - FPanStartX;
    PaintBoxGraph.Invalidate;
  end;
end;

procedure TFormMain.PaintBoxGraphMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FIsPanning := False;
    PaintBoxGraph.Cursor := crDefault;
  end;
end;

{ =================== Dışa Aktarma =================== }

procedure TFormMain.ExportFilteredCSV(const AFileName: string);
var
  SL: TStringList;
  I: Integer;
  Model: TSensorDataModel;
  Rec: TSensorRecord;
  FX, FY, FZ: TArray<Double>;
  FS: TFormatSettings;
begin
  Model := FController.GetModel;
  FX := Model.GetFilteredData(atX);
  FY := Model.GetFilteredData(atY);
  FZ := Model.GetFilteredData(atZ);

  if (Length(FX) = 0) or (Length(FY) = 0) or (Length(FZ) = 0) then
  begin
    ShowMessage('Önce filtre uygulayın.');
    Exit;
  end;

  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';

  SL := TStringList.Create;
  try
    SL.Add('"serialNumber","x_raw","y_raw","z_raw","x_filtered","y_filtered","z_filtered","isNoiseX","isNoiseY","isNoiseZ"');

    for I := 0 to Model.Count - 1 do
    begin
      Rec := Model.GetRecord(I);
      SL.Add(Format('"%s","%.5f","%.5f","%.5f","%.5f","%.5f","%.5f","%s","%s","%s"',
        [Rec.SerialNumber,
         Rec.X, Rec.Y, Rec.Z,
         FX[I], FY[I], FZ[I],
         BoolToStr(Rec.IsNoiseX, True),
         BoolToStr(Rec.IsNoiseY, True),
         BoolToStr(Rec.IsNoiseZ, True)], FS));
    end;

    SL.SaveToFile(AFileName, TEncoding.UTF8);
    ShowMessage(Format('Filtreli veri kaydedildi: %s (%d kayıt)', [AFileName, Model.Count]));
  finally
    SL.Free;
  end;
end;

end.
