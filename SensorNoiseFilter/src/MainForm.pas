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
  System.Threading,
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
    CheckDrawGraph: TCheckBox;

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
    procedure CheckDrawGraphClick(Sender: TObject);
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

    { Arka plan işleme }
    FProcessingTask : ITask;
    FProgressBar    : TProgressBar;
    FProcessing     : Boolean;

    { Görüntüleme önbelleği — LTTB ile küçültülmüş veri (hızlı repaint için) }
    FDisplayRawData      : TArray<Double>;
    FDisplayFilteredData : TArray<Double>;
    FDisplayRawIndices   : TArray<Integer>;
    FDisplayNoiseFlags   : TArray<Boolean>;
    FDisplayMinVal       : Double;
    FDisplayMaxVal       : Double;
    FLastPaintWidth      : Integer;

    { Fare kaydırma kısıtlayıcısı }
    FLastPanMs : Cardinal;

    { Controller olay işleyicileri }
    procedure OnDataLoaded(Sender: TObject; ARecordCount: Integer);
    procedure OnFilterApplied(Sender: TObject; AFilterName: string);
    procedure OnNoiseDetected(Sender: TObject; ANoiseCountX, ANoiseCountY, ANoiseCountZ: Integer);
    procedure OnStatusMessage(Sender: TObject; const AMessage: string);
    procedure OnStatisticsUpdated(Sender: TObject;
      const ARawStats, AFilteredStats: TStatistics; AAxis: TAxisType);
    procedure OnProgress(Sender: TObject; APercent: Integer; const AMsg: string);

    { Yardımcı metodlar }
    procedure InitializeFilterCombo;
    procedure UpdateParamLabels;
    function ReadFilterParams: TFilterParams;
    procedure SetControlsEnabled(AEnabled: Boolean);
    function DownsampleLTTBWithIndices(const AData: TArray<Double>;
      MaxPoints: Integer; out Indices: TArray<Integer>): TArray<Double>;
    procedure PrepareDisplayData;
    procedure DrawGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);
    procedure DrawAxisLines(ACanvas: TCanvas; AWidth, AHeight: Integer;
      AMinVal, AMaxVal: Double);
    procedure DrawDataSeries(ACanvas: TCanvas; const AData: TArray<Double>;
      AColor: TColor; AWidth, AHeight: Integer; AMinVal, AMaxVal: Double;
      ADrawPoints: Boolean);
    procedure DrawNoiseMarkers(ACanvas: TCanvas;
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

const
  { With LTTB downsampling the graph handles any size; keep a very high
    threshold so the auto-disable almost never triggers. }
  LargeDataThreshold = 10000000;

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

  FProcessing    := False;
  FLastPaintWidth := 0;
  FLastPanMs      := 0;

  { Controller oluştur ve olayları bağla }
  FController := TFilterController.Create;
  FController.OnDataLoaded       := OnDataLoaded;
  FController.OnFilterApplied    := OnFilterApplied;
  FController.OnNoiseDetected    := OnNoiseDetected;
  FController.OnStatusMessage    := OnStatusMessage;
  FController.OnStatisticsUpdated := OnStatisticsUpdated;
  FController.OnProgress         := OnProgress;

  { Filtre combobox'ını doldur }
  InitializeFilterCombo;

  { İlerleme çubuğu — dinamik oluşturma (DFM değişikliği gerektirmez) }
  FProgressBar := TProgressBar.Create(PanelTop);
  FProgressBar.Parent := PanelTop;
  FProgressBar.SetBounds(534, 12, 170, 16);
  FProgressBar.Min := 0;
  FProgressBar.Max := 100;
  FProgressBar.Position := 0;
  FProgressBar.Visible := False;

  { Durum çubuğu başlat }
  StatusBar.Panels[0].Text := 'Hazır - CSV dosyası yükleyin.';
  StatusBar.Panels[1].Text := 'Kayıt: 0';
  StatusBar.Panels[2].Text := '';
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  { Cancel any background task before freeing the controller }
  if Assigned(FProcessingTask) then
    FProcessingTask.Cancel;
  FProcessingTask := nil;
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
var
  FileName: string;
begin
  if not OpenDialogCSV.Execute then
    Exit;

  FileName := OpenDialogCSV.FileName;
  EditFilePath.Text := FileName;

  SetControlsEnabled(False);
  FProcessing := True;
  FProgressBar.Position := 0;
  FProgressBar.Visible  := True;

  FProcessingTask := TTask.Run(procedure
  begin
    try
      FController.LoadCSV(FileName);
    except
      on E: Exception do
        TThread.Queue(nil, procedure
        begin
          ShowMessage('CSV yükleme hatası: ' + E.Message);
        end);
    end;

    TThread.Queue(nil, procedure
    begin
      if Application.Terminated then Exit;
      FProcessing := False;
      FProgressBar.Visible := False;
      SetControlsEnabled(True);
      PrepareDisplayData;
      PaintBoxGraph.Invalidate;
    end);
  end);
end;

procedure TFormMain.BtnApplyFilterClick(Sender: TObject);
var
  Params: TFilterParams;
  FilterType: TFilterType;
begin
  Params     := ReadFilterParams;
  FilterType := TFilterType(ComboFilter.ItemIndex);

  SetControlsEnabled(False);
  FProcessing := True;
  FProgressBar.Position := 0;
  FProgressBar.Visible  := True;

  FProcessingTask := TTask.Run(procedure
  begin
    try
      FController.SetFilterParams(Params);
      FController.SelectFilter(FilterType);
      FController.ApplyFilter;
    except
      on E: Exception do
        TThread.Queue(nil, procedure
        begin
          ShowMessage('Filtre hatası: ' + E.Message);
        end);
    end;

    TThread.Queue(nil, procedure
    begin
      if Application.Terminated then Exit;
      FProcessing := False;
      FProgressBar.Visible := False;
      SetControlsEnabled(True);
      if CheckDrawGraph.Checked then
      begin
        PrepareDisplayData;
        PaintBoxGraph.Invalidate;
      end;
    end);
  end);
end;

procedure TFormMain.BtnDetectNoiseClick(Sender: TObject);
var
  FS: TFormatSettings;
  Sigma: Double;
begin
  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';
  Sigma := StrToFloatDef(EditSigma.Text, 2.0, FS);

  SetControlsEnabled(False);
  FProcessing := True;
  FProgressBar.Position := 0;
  FProgressBar.Visible  := True;

  FProcessingTask := TTask.Run(procedure
  begin
    try
      FController.DetectNoise(Sigma);
    except
      on E: Exception do
        TThread.Queue(nil, procedure
        begin
          ShowMessage('Gürültü tespit hatası: ' + E.Message);
        end);
    end;

    TThread.Queue(nil, procedure
    begin
      if Application.Terminated then Exit;
      FProcessing := False;
      FProgressBar.Visible := False;
      SetControlsEnabled(True);
      if CheckDrawGraph.Checked then
      begin
        PrepareDisplayData;
        PaintBoxGraph.Invalidate;
      end;
    end);
  end);
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
  PrepareDisplayData;
  if CheckDrawGraph.Checked then
    PaintBoxGraph.Invalidate;
end;

procedure TFormMain.CheckShowRawClick(Sender: TObject);
begin
  FShowRawData := CheckShowRaw.Checked;
  if CheckDrawGraph.Checked then
    PaintBoxGraph.Invalidate;
end;

procedure TFormMain.CheckShowFilteredClick(Sender: TObject);
begin
  FShowFilteredData := CheckShowFiltered.Checked;
  if CheckDrawGraph.Checked then
    PaintBoxGraph.Invalidate;
end;

procedure TFormMain.CheckDrawGraphClick(Sender: TObject);
begin
  { Kullanıcı grafik çizimini açtı/kapattı; durumu hemen yansıt }
  PaintBoxGraph.Invalidate;
end;

{ =================== Controller Olay İşleyicileri =================== }

procedure TFormMain.OnDataLoaded(Sender: TObject; ARecordCount: Integer);
begin
  { Thread-safe: arka plan thread'inden çağrılabilir }
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnDataLoaded(Sender, ARecordCount); end);
    Exit;
  end;

  StatusBar.Panels[1].Text := Format('Kayıt: %d', [ARecordCount]);
  FOffsetX := 0;

  { Büyük veri setlerinde grafik çizimini otomatik kapat }
  if ARecordCount >= LargeDataThreshold then
  begin
    CheckDrawGraph.Checked := False;
    StatusBar.Panels[0].Text :=
      Format('Yüklendi: %d kayıt (büyük veri seti — grafik otomatik kapatıldı)', [ARecordCount]);
  end
  else
    StatusBar.Panels[0].Text := Format('Toplam %d kayıt yüklendi.', [ARecordCount]);
end;

procedure TFormMain.OnFilterApplied(Sender: TObject; AFilterName: string);
begin
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnFilterApplied(Sender, AFilterName); end);
    Exit;
  end;
  StatusBar.Panels[2].Text := AFilterName;
end;

procedure TFormMain.OnNoiseDetected(Sender: TObject; ANoiseCountX, ANoiseCountY, ANoiseCountZ: Integer);
begin
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnNoiseDetected(Sender, ANoiseCountX, ANoiseCountY, ANoiseCountZ); end);
    Exit;
  end;
  LabelNoiseInfo.Caption := Format(
    'Gürültü Noktaları: X=%d, Y=%d, Z=%d (Sigma x%.1f)',
    [ANoiseCountX, ANoiseCountY, ANoiseCountZ,
     FController.GetModel.NoiseSigmaMultiplier]);
end;

procedure TFormMain.OnStatusMessage(Sender: TObject; const AMessage: string);
begin
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnStatusMessage(Sender, AMessage); end);
    Exit;
  end;
  StatusBar.Panels[0].Text := AMessage;
end;

procedure TFormMain.OnStatisticsUpdated(Sender: TObject;
  const ARawStats, AFilteredStats: TStatistics; AAxis: TAxisType);
var
  AxisName: string;
begin
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnStatisticsUpdated(Sender, ARawStats, AFilteredStats, AAxis); end);
    Exit;
  end;

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

procedure TFormMain.OnProgress(Sender: TObject; APercent: Integer; const AMsg: string);
begin
  if GetCurrentThreadID <> MainThreadID then
  begin
    TThread.Queue(nil, procedure begin OnProgress(Sender, APercent, AMsg); end);
    Exit;
  end;
  FProgressBar.Position := APercent;
  if AMsg <> '' then
    StatusBar.Panels[0].Text := AMsg;
end;

{ =================== Grafik Çizimi =================== }

procedure TFormMain.SetControlsEnabled(AEnabled: Boolean);
begin
  BtnLoadCSV.Enabled       := AEnabled;
  BtnApplyFilter.Enabled   := AEnabled;
  BtnDetectNoise.Enabled   := AEnabled;
  BtnExportFiltered.Enabled := AEnabled;
  ComboFilter.Enabled      := AEnabled;
  ComboAxis.Enabled        := AEnabled;
end;

{ Largest-Triangle-Three-Buckets (LTTB) downsampling.
  Reduces AData to MaxPoints while preserving visual fidelity.
  Returns downsampled values; Indices maps each output slot → original index. }
function TFormMain.DownsampleLTTBWithIndices(const AData: TArray<Double>;
  MaxPoints: Integer; out Indices: TArray<Integer>): TArray<Double>;
var
  DataLen, i, j, a, nextA, rangeOffs, rangeTo: Integer;
  Every, MaxArea, Area, AvgX, AvgY: Double;
  Values_: TArray<Double>;
begin
  DataLen := Length(AData);

  { Nothing to reduce — return as-is with identity mapping }
  if (DataLen <= MaxPoints) or (MaxPoints <= 2) then
  begin
    Result := Copy(AData);
    SetLength(Indices, DataLen);
    for i := 0 to DataLen - 1 do
      Indices[i] := i;
    Exit;
  end;

  SetLength(Values_, MaxPoints);
  SetLength(Indices, MaxPoints);

  Every := (DataLen - 2) / (MaxPoints - 2);

  Values_[0] := AData[0];
  Indices[0] := 0;
  a := 0;

  for i := 0 to MaxPoints - 3 do
  begin
    { Compute average of the NEXT bucket (anchor for area calculation) }
    rangeOffs := Trunc((i + 1) * Every) + 1;
    rangeTo   := Min(Trunc((i + 2) * Every) + 1, DataLen);

    AvgX := 0; AvgY := 0;
    if rangeTo > rangeOffs then
    begin
      for j := rangeOffs to rangeTo - 1 do
      begin
        AvgX := AvgX + j;
        AvgY := AvgY + AData[j];
      end;
      AvgX := AvgX / (rangeTo - rangeOffs);
      AvgY := AvgY / (rangeTo - rangeOffs);
    end;

    { Find the point in the CURRENT bucket with the largest triangle area }
    rangeOffs := Trunc(i * Every) + 1;
    rangeTo   := Min(Trunc((i + 1) * Every) + 1, DataLen);

    MaxArea := -1;
    nextA   := Min(rangeOffs, DataLen - 1);
    for j := rangeOffs to rangeTo - 1 do
    begin
      Area := Abs((a - AvgX) * (AData[j] - AData[a]) -
                  (a - j)    * (AvgY      - AData[a])) * 0.5;
      if Area > MaxArea then
      begin
        MaxArea := Area;
        nextA   := j;
      end;
    end;

    Values_[i + 1] := AData[nextA];
    Indices[i + 1] := nextA;
    a := nextA;
  end;

  Values_[MaxPoints - 1] := AData[DataLen - 1];
  Indices[MaxPoints - 1] := DataLen - 1;
  Result := Values_;
end;

{ Rebuild the downsampled display cache.
  Must be called on the main thread after data changes or axis switches. }
procedure TFormMain.PrepareDisplayData;
var
  RawData, FilteredData: TArray<Double>;
  NoiseData: TArray<Boolean>;
  DummyIndices: TArray<Integer>;
  MaxDisplayPts: Integer;
  I: Integer;
  MinVal, MaxVal, Range: Double;
begin
  if FController.GetModel.Count = 0 then
  begin
    SetLength(FDisplayRawData, 0);
    SetLength(FDisplayFilteredData, 0);
    SetLength(FDisplayRawIndices, 0);
    SetLength(FDisplayNoiseFlags, 0);
    FDisplayMinVal  := 0;
    FDisplayMaxVal  := 1;
    FLastPaintWidth := 0;
    Exit;
  end;

  MaxDisplayPts   := Max(500, PaintBoxGraph.Width * 2);
  FLastPaintWidth := PaintBoxGraph.Width;

  { Downsample raw data — keep original indices for noise flag lookup }
  RawData         := FController.GetRawData;
  FDisplayRawData := DownsampleLTTBWithIndices(RawData, MaxDisplayPts, FDisplayRawIndices);

  { Downsample filtered data (indices not needed for noise lookup) }
  FilteredData := FController.GetFilteredData;
  if Length(FilteredData) > 0 then
    FDisplayFilteredData := DownsampleLTTBWithIndices(FilteredData, MaxDisplayPts, DummyIndices)
  else
    SetLength(FDisplayFilteredData, 0);

  { Compute min/max from FULL dataset so the Y axis is always correct }
  MinVal := RawData[0]; MaxVal := RawData[0];
  for I := 1 to High(RawData) do
  begin
    if RawData[I] < MinVal then MinVal := RawData[I];
    if RawData[I] > MaxVal then MaxVal := RawData[I];
  end;
  for I := 0 to High(FilteredData) do
  begin
    if FilteredData[I] < MinVal then MinVal := FilteredData[I];
    if FilteredData[I] > MaxVal then MaxVal := FilteredData[I];
  end;
  Range := MaxVal - MinVal;
  if Range < 0.001 then Range := 1.0;
  FDisplayMinVal := MinVal - Range * 0.1;
  FDisplayMaxVal := MaxVal + Range * 0.1;

  { Build noise-flag array aligned to the downsampled raw-data points }
  NoiseData := FController.GetModel.GetNoiseData(FController.GetSelectedAxis);
  SetLength(FDisplayNoiseFlags, Length(FDisplayRawIndices));
  for I := 0 to High(FDisplayRawIndices) do
  begin
    if FDisplayRawIndices[I] < Length(NoiseData) then
      FDisplayNoiseFlags[I] := NoiseData[FDisplayRawIndices[I]]
    else
      FDisplayNoiseFlags[I] := False;
  end;
end;

procedure TFormMain.PaintBoxGraphPaint(Sender: TObject);
begin
  DrawGraph(PaintBoxGraph.Canvas, PaintBoxGraph.Width, PaintBoxGraph.Height);
end;

procedure TFormMain.DrawGraph(ACanvas: TCanvas; AWidth, AHeight: Integer);
begin
  { Arka plan }
  ACanvas.Brush.Color := clWhite;
  ACanvas.FillRect(Rect(0, 0, AWidth, AHeight));

  if FController.GetModel.Count = 0 then
  begin
    ACanvas.Font.Size  := 12;
    ACanvas.Font.Color := clGray;
    ACanvas.TextOut(AWidth div 2 - 120, AHeight div 2 - 10,
      'CSV dosyası yükleyin...');
    Exit;
  end;

  { Grafik çizimi kapalıysa bilgi mesajı göster }
  if not CheckDrawGraph.Checked then
  begin
    ACanvas.Font.Size  := 11;
    ACanvas.Font.Color := clGray;
    ACanvas.TextOut(AWidth div 2 - 210, AHeight div 2 - 10,
      'Grafik çizimi kapalı — "Grafik Çiz" seçeneğini işaretleyin.');
    Exit;
  end;

  { Ekran genişliği değiştiyse (pencere boyutu) önbelleği yenile.
    İşlem sürerken yenileme yapma — eski veriyle çizmeye devam et. }
  if (AWidth <> FLastPaintWidth) and not FProcessing then
    PrepareDisplayData;

  if Length(FDisplayRawData) = 0 then
    Exit;

  { Eksen çizgileri (önbellekten min/max değerleri kullan) }
  DrawAxisLines(ACanvas, AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal);

  { Filtrelenmiş veri — yeşil }
  if FShowFilteredData and (Length(FDisplayFilteredData) > 0) then
    DrawDataSeries(ACanvas, FDisplayFilteredData, clGreen, AWidth, AHeight,
      FDisplayMinVal, FDisplayMaxVal, False);

  { Ham veri — mavi; küçük setlerde nokta da çiz }
  if FShowRawData then
    DrawDataSeries(ACanvas, FDisplayRawData, clBlue, AWidth, AHeight,
      FDisplayMinVal, FDisplayMaxVal, Length(FDisplayRawData) <= 500);

  { Gürültü noktaları — kırmızı (önbellekten bayrak dizisi) }
  DrawNoiseMarkers(ACanvas, AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal);

  { Başlık }
  ACanvas.Font.Size  := 9;
  ACanvas.Font.Color := clBlack;
  ACanvas.Font.Style := [fsBold];
  ACanvas.TextOut(FGraphMarginLeft + 5, 3,
    Format('Sensör Verisi Grafiği — %d nokta (%d görüntüleniyor)',
      [FController.GetModel.Count, Length(FDisplayRawData)]));
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
  Points: array of TPoint;
begin
  N := Length(AData);
  if N < 2 then
    Exit;

  { Pre-build the full point array, then issue a single Polyline GDI call.
    This is orders of magnitude faster than N individual LineTo calls. }
  SetLength(Points, N);
  for I := 0 to N - 1 do
  begin
    Points[I].X := DataXToScreenX(I, N, AWidth);
    Points[I].Y := DataYToScreenY(AData[I], AMinVal, AMaxVal, AHeight);
  end;

  ACanvas.Pen.Color := AColor;
  ACanvas.Pen.Width := 1;
  Winapi.Windows.Polyline(ACanvas.Handle, Points[0], N);

  { Draw individual data-point markers only for small datasets }
  if ADrawPoints then
  begin
    ACanvas.Brush.Color := AColor;
    ACanvas.Pen.Color   := AColor;
    for I := 0 to N - 1 do
      ACanvas.Ellipse(Points[I].X - 3, Points[I].Y - 3,
                      Points[I].X + 3, Points[I].Y + 3);
  end;
end;

procedure TFormMain.DrawNoiseMarkers(ACanvas: TCanvas;
  AWidth, AHeight: Integer; AMinVal, AMaxVal: Double);
var
  I, N, X1, Y1: Integer;
begin
  N := Length(FDisplayNoiseFlags);
  if (N = 0) or (Length(FDisplayRawData) < N) then
    Exit;

  ACanvas.Brush.Color := clRed;
  ACanvas.Pen.Color   := clMaroon;
  ACanvas.Pen.Width   := 2;

  { FDisplayNoiseFlags is already aligned to FDisplayRawData (LTTB-sampled).
    No Model.GetRecord call in the loop — zero O(N) overhead here. }
  for I := 0 to N - 1 do
  begin
    if FDisplayNoiseFlags[I] then
    begin
      X1 := DataXToScreenX(I, N, AWidth);
      Y1 := DataYToScreenY(FDisplayRawData[I], AMinVal, AMaxVal, AHeight);

      ACanvas.Ellipse(X1 - 6, Y1 - 6, X1 + 6, Y1 + 6);
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
const
  PAN_THROTTLE_MS = 16; { ~60 FPS }
var
  Now: Cardinal;
begin
  if FIsPanning then
  begin
    FOffsetX := X - FPanStartX;
    Now := GetTickCount;
    if Now - FLastPanMs >= PAN_THROTTLE_MS then
    begin
      FLastPanMs := Now;
      PaintBoxGraph.Invalidate;
    end;
  end;
end;

procedure TFormMain.PaintBoxGraphMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    FIsPanning := False;
    PaintBoxGraph.Cursor := crDefault;
    PaintBoxGraph.Invalidate; { Final repaint after pan }
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
