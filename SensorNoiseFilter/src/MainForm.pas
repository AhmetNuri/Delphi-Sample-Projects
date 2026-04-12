{ ============================================================================
  MainForm.pas - View Katmanı
  
  Kullanıcı arayüzü: CSV yükleme, filtre seçimi, parametre ayarları,
  TSkPaintBox ile Skia tabanlı grafik çizimi, gürültü noktalarının renkli
  gösterimi, çok eksenli görüntüleme ve MAD tabanlı otomatik gürültü tahmini.
  
  MVC Mimarisi: VIEW
  ============================================================================ }
unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Math,
  System.Types, System.UITypes, System.Threading,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls,
      vcl.Skia,
  SensorDataModel, SignalFilters, FilterController, System.Skia;

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
    BtnAutoEstimateNoise: TButton;
    CheckShowAllAxes: TCheckBox;

    { Grafik Paneli }
    PanelGraph: TPanel;
    PaintBoxGraph: TSkPaintBox;

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
    procedure PaintBoxGraphDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure PaintBoxGraphMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxGraphMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure PaintBoxGraphMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CheckShowRawClick(Sender: TObject);
    procedure CheckShowFilteredClick(Sender: TObject);
    procedure CheckDrawGraphClick(Sender: TObject);
    procedure BtnAutoEstimateNoiseClick(Sender: TObject);
    procedure CheckShowAllAxesClick(Sender: TObject);
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
    FShowAllAxes: Boolean;

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

    { Çok eksenli görüntüleme önbelleği }
    FDisplayRawDataY    : TArray<Double>;
    FDisplayRawDataZ    : TArray<Double>;
    FDisplayMinValAll   : Double;
    FDisplayMaxValAll   : Double;

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
    function TextBaseline(AFont: ISkFont; ATopY: Single): Single;
    procedure DrawGraph(ACanvas: ISkCanvas; AWidth, AHeight: Integer);
    procedure DrawAxisLines(ACanvas: ISkCanvas; AWidth, AHeight: Integer;
      AMinVal, AMaxVal: Double);
    procedure DrawDataSeries(ACanvas: ISkCanvas; const AData: TArray<Double>;
      AColor: TAlphaColor; AWidth, AHeight: Integer; AMinVal, AMaxVal: Double;
      ADrawPoints: Boolean);
    procedure DrawNoiseMarkers(ACanvas: ISkCanvas;
      AWidth, AHeight: Integer; AMinVal, AMaxVal: Double);
    procedure DrawMultiAxisGraph(ACanvas: ISkCanvas; AWidth, AHeight: Integer);
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

  { Grafik renk sabitleri }
  COLOR_FILTERED_GREEN : TAlphaColor = $FF008000;  { Koyu yeşil - filtreli veri }
  COLOR_AXIS_ORANGE    : TAlphaColor = $FFFF8C00;  { Turuncu - Y ekseni }
  COLOR_AXIS_PURPLE    : TAlphaColor = $FF8B008B;  { Mor - Z ekseni }

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
  FShowAllAxes := False;

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

procedure TFormMain.BtnAutoEstimateNoiseClick(Sender: TObject);
{ MAD tabanlı gürültü sigma tahmini: seçili eksenin gürültü seviyesini
  otomatik hesaplar ve EditSigma alanını önerilen çarpanla doldurur. }
var
  MADSigma, SuggestedMultiplier: Double;
  AxisName: string;
  FS: TFormatSettings;
begin
  if FController.GetModel.Count = 0 then
  begin
    ShowMessage('Önce bir CSV dosyası yükleyin.');
    Exit;
  end;

  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';

  MADSigma := FController.EstimateNoiseSigmaMAD(FController.GetSelectedAxis);
  SuggestedMultiplier := FController.GetSuggestedSigmaMultiplier(FController.GetSelectedAxis);

  case FController.GetSelectedAxis of
    atX: AxisName := 'X';
    atY: AxisName := 'Y';
    atZ: AxisName := 'Z';
  end;

  EditSigma.Text := Format('%.2f', [SuggestedMultiplier], FS);
  StatusBar.Panels[0].Text := Format(
    '%s ekseni gürültü tahmini: MAD-Sigma=%.4f | Önerilen Çarpan=%.2f',
    [AxisName, MADSigma, SuggestedMultiplier]);
end;

procedure TFormMain.CheckShowAllAxesClick(Sender: TObject);
begin
  FShowAllAxes := CheckShowAllAxes.Checked;

  if FShowAllAxes then
    LabelLegend.Caption :=
      'Renk: Mavi = X Ekseni | Turuncu = Y Ekseni | Mor = Z Ekseni'
  else
    LabelLegend.Caption :=
      'Renk: Mavi = Ham Veri | Yeşil = Filtreli | Kırmızı = Gürültü Noktası';

  if CheckDrawGraph.Checked then
  begin
    PrepareDisplayData;
    PaintBoxGraph.Invalidate;
  end;
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
  RawDataY, RawDataZ: TArray<Double>;
  NoiseData: TArray<Boolean>;
  DummyIndices: TArray<Integer>;
  MaxDisplayPts: Integer;
  I: Integer;
  MinVal, MaxVal, AllMin, AllMax, Range: Double;
begin
  if FController.GetModel.Count = 0 then
  begin
    SetLength(FDisplayRawData, 0);
    SetLength(FDisplayFilteredData, 0);
    SetLength(FDisplayRawIndices, 0);
    SetLength(FDisplayNoiseFlags, 0);
    SetLength(FDisplayRawDataY, 0);
    SetLength(FDisplayRawDataZ, 0);
    FDisplayMinVal    := 0;
    FDisplayMaxVal    := 1;
    FDisplayMinValAll := 0;
    FDisplayMaxValAll := 1;
    FLastPaintWidth   := 0;
    Exit;
  end;

  MaxDisplayPts   := Max(500, PaintBoxGraph.Width * 2);
  FLastPaintWidth := PaintBoxGraph.Width;

  { Seçili eksen ham veriyi örnekle — orijinal indeksler gürültü bayrağı için saklanır }
  RawData         := FController.GetRawData;
  FDisplayRawData := DownsampleLTTBWithIndices(RawData, MaxDisplayPts, FDisplayRawIndices);

  { Filtreli veriyi örnekle }
  FilteredData := FController.GetFilteredData;
  if Length(FilteredData) > 0 then
    FDisplayFilteredData := DownsampleLTTBWithIndices(FilteredData, MaxDisplayPts, DummyIndices)
  else
    SetLength(FDisplayFilteredData, 0);

  { Y ekseni sınırlarını tüm veri setinden hesapla }
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

  { Seçili eksen için gürültü bayrakları }
  NoiseData := FController.GetModel.GetNoiseData(FController.GetSelectedAxis);
  SetLength(FDisplayNoiseFlags, Length(FDisplayRawIndices));
  for I := 0 to High(FDisplayRawIndices) do
  begin
    if FDisplayRawIndices[I] < Length(NoiseData) then
      FDisplayNoiseFlags[I] := NoiseData[FDisplayRawIndices[I]]
    else
      FDisplayNoiseFlags[I] := False;
  end;

  { Çok eksenli mod: Y ve Z eksenlerini de hazırla }
  if FShowAllAxes then
  begin
    RawDataY := FController.GetModel.GetAxisData(atY);
    RawDataZ := FController.GetModel.GetAxisData(atZ);

    FDisplayRawDataY := DownsampleLTTBWithIndices(RawDataY, MaxDisplayPts, DummyIndices);
    FDisplayRawDataZ := DownsampleLTTBWithIndices(RawDataZ, MaxDisplayPts, DummyIndices);

    { Tüm eksenler için birleşik min/max hesapla }
    AllMin := MinVal; AllMax := MaxVal;
    for I := 0 to High(RawDataY) do
    begin
      if RawDataY[I] < AllMin then AllMin := RawDataY[I];
      if RawDataY[I] > AllMax then AllMax := RawDataY[I];
    end;
    for I := 0 to High(RawDataZ) do
    begin
      if RawDataZ[I] < AllMin then AllMin := RawDataZ[I];
      if RawDataZ[I] > AllMax then AllMax := RawDataZ[I];
    end;
    Range := AllMax - AllMin;
    if Range < 0.001 then Range := 1.0;
    FDisplayMinValAll := AllMin - Range * 0.1;
    FDisplayMaxValAll := AllMax + Range * 0.1;
  end
  else
  begin
    SetLength(FDisplayRawDataY, 0);
    SetLength(FDisplayRawDataZ, 0);
    FDisplayMinValAll := FDisplayMinVal;
    FDisplayMaxValAll := FDisplayMaxVal;
  end;
end;

procedure TFormMain.PaintBoxGraphDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
begin
  DrawGraph(ACanvas, Round(ADest.Width), Round(ADest.Height));
end;

{ Skia text için: VCL TextOut'un üst-sol Y koordinatını Skia taban çizgisine dönüştür }
function TFormMain.TextBaseline(AFont: ISkFont; ATopY: Single): Single;
var
  Metrics: TSkFontMetrics;
begin
  AFont.GetMetrics(Metrics);
  { Skia'da fAscent, taban çizgisinin ÜSTÜNDEKI mesafeyi negatif sayı olarak verir
    (örn. -7.5). Üst-sol Y'den taban Y'ye dönüşüm: taban = üst + (-fAscent) = üst - fAscent }
  Result := ATopY - Metrics.Ascent;
end;

procedure TFormMain.DrawGraph(ACanvas: ISkCanvas; AWidth, AHeight: Integer);
var
  LPaint: ISkPaint;
  LFont: ISkFont;
  LTitleText: string;
begin
  { Arka plan temizle }
  ACanvas.Clear(TAlphaColors.White);

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Style := TSkPaintStyle.Fill;

  if FController.GetModel.Count = 0 then
  begin
    LFont := TSkFont.Create(nil, 12);
    LPaint.Color := TAlphaColors.Gray;
    ACanvas.DrawSimpleText('CSV dosyası yükleyin...',
      AWidth div 2 - 120, TextBaseline(LFont, AHeight div 2 - 10), LFont, LPaint);
    Exit;
  end;

  if not CheckDrawGraph.Checked then
  begin
    LFont := TSkFont.Create(nil, 11);
    LPaint.Color := TAlphaColors.Gray;
    ACanvas.DrawSimpleText(
      'Grafik çizimi kapalı — "Grafik Çiz" seçeneğini işaretleyin.',
      AWidth div 2 - 210, TextBaseline(LFont, AHeight div 2 - 10), LFont, LPaint);
    Exit;
  end;

  if (AWidth <> FLastPaintWidth) and not FProcessing then
    PrepareDisplayData;

  if Length(FDisplayRawData) = 0 then
    Exit;

  if FShowAllAxes then
    DrawMultiAxisGraph(ACanvas, AWidth, AHeight)
  else
  begin
    DrawAxisLines(ACanvas, AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal);

    { Filtreli veri — koyu yeşil }
    if FShowFilteredData and (Length(FDisplayFilteredData) > 0) then
      DrawDataSeries(ACanvas, FDisplayFilteredData, COLOR_FILTERED_GREEN,
        AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal, False);

    { Ham veri — mavi; küçük setlerde nokta da çiz }
    if FShowRawData then
      DrawDataSeries(ACanvas, FDisplayRawData, TAlphaColors.Blue,
        AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal,
        Length(FDisplayRawData) <= 500);

    { Gürültü noktaları — kırmızı işaretler }
    DrawNoiseMarkers(ACanvas, AWidth, AHeight, FDisplayMinVal, FDisplayMaxVal);
  end;

  { Başlık }
  LFont := TSkFont.Create(nil, 9);
  LFont.Embolden := True;
  LPaint.Color := TAlphaColors.Black;
  LTitleText := Format('Sensör Verisi Grafiği — %d nokta (%d görüntüleniyor)',
    [FController.GetModel.Count, Length(FDisplayRawData)]);
  ACanvas.DrawSimpleText(LTitleText,
    FGraphMarginLeft + 5, TextBaseline(LFont, 3), LFont, LPaint);
end;

procedure TFormMain.DrawAxisLines(ACanvas: ISkCanvas; AWidth, AHeight: Integer;
  AMinVal, AMaxVal: Double);
var
  GraphLeft, GraphRight, GraphTop, GraphBottom: Integer;
  I, NumTicks, YPos: Integer;
  Val: Double;
  LStrokePaint, LDashPaint, LTextPaint: ISkPaint;
  LFont: ISkFont;
  DashIntervals: TArray<Single>;
begin
  GraphLeft   := FGraphMarginLeft;
  GraphRight  := AWidth - FGraphMarginRight;
  GraphTop    := FGraphMarginTop;
  GraphBottom := AHeight - FGraphMarginBottom;

  LFont := TSkFont.Create(nil, 7);

  { Çerçeve }
  LStrokePaint := TSkPaint.Create;
  LStrokePaint.Style := TSkPaintStyle.Stroke;
  LStrokePaint.Color := TAlphaColors.Silver;
  LStrokePaint.StrokeWidth := 1;
  ACanvas.DrawRect(TRectF.Create(GraphLeft, GraphTop, GraphRight, GraphBottom), LStrokePaint);

  { Kesik çizgili kılavuz çizgileri }
  SetLength(DashIntervals, 2);
  DashIntervals[0] := 4;
  DashIntervals[1] := 4;
  LDashPaint := TSkPaint.Create;
  LDashPaint.Style := TSkPaintStyle.Stroke;
  LDashPaint.Color := $FFF0F0F0;
  LDashPaint.StrokeWidth := 1;
  LDashPaint.PathEffect := TSkPathEffect.MakeDash(DashIntervals, 0);

  { Etiket boya }
  LTextPaint := TSkPaint.Create;
  LTextPaint.AntiAlias := True;
  LTextPaint.Style := TSkPaintStyle.Fill;
  LTextPaint.Color := TAlphaColors.Gray;

  NumTicks := 8;
  for I := 0 to NumTicks do
  begin
    Val  := AMinVal + (AMaxVal - AMinVal) * I / NumTicks;
    YPos := DataYToScreenY(Val, AMinVal, AMaxVal, AHeight);

    { Yatay kılavuz çizgisi }
    ACanvas.DrawLine(GraphLeft, YPos, GraphRight, YPos, LDashPaint);

    { Y ekseni etiketi }
    ACanvas.DrawSimpleText(Format('%.2f', [Val]),
      2, TextBaseline(LFont, YPos - 6), LFont, LTextPaint);
  end;
end;

procedure TFormMain.DrawDataSeries(ACanvas: ISkCanvas; const AData: TArray<Double>;
  AColor: TAlphaColor; AWidth, AHeight: Integer; AMinVal, AMaxVal: Double;
  ADrawPoints: Boolean);
var
  I, N: Integer;
  Pts: TArray<TPointF>;
  LPaint: ISkPaint;
  LBuilder: ISkPathBuilder;
  LPath: ISkPath;
begin
  N := Length(AData);
  if N < 2 then
    Exit;

  { Skia path olarak poliyhat: tek DrawPath çağrısıyla çizilir }
  SetLength(Pts, N);
  for I := 0 to N - 1 do
  begin
    Pts[I].X := DataXToScreenX(I, N, AWidth);
    Pts[I].Y := DataYToScreenY(AData[I], AMinVal, AMaxVal, AHeight);
  end;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.Color := AColor;
  LPaint.Style := TSkPaintStyle.Stroke;
  LPaint.StrokeWidth := 1;

  LBuilder := TSkPathBuilder.Create;
  LBuilder.MoveTo(Pts[0].X, Pts[0].Y);
  for I := 1 to N - 1 do
    LBuilder.LineTo(Pts[I].X, Pts[I].Y);
  LPath := LBuilder.Detach;

  ACanvas.DrawPath(LPath, LPaint);

  if ADrawPoints then
  begin
    LPaint.Style := TSkPaintStyle.Fill;
    for I := 0 to N - 1 do
      ACanvas.DrawCircle(Pts[I].X, Pts[I].Y, 3, LPaint);
  end;
end;

procedure TFormMain.DrawNoiseMarkers(ACanvas: ISkCanvas;
  AWidth, AHeight: Integer; AMinVal, AMaxVal: Double);
var
  I, N, X1, Y1: Integer;
  LPaint: ISkPaint;
begin
  N := Length(FDisplayNoiseFlags);
  if (N = 0) or (Length(FDisplayRawData) < N) then
    Exit;

  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;
  LPaint.StrokeWidth := 2;

  { FDisplayNoiseFlags, LTTB örneklenmiş FDisplayRawData ile hizalı.
    Model.GetRecord döngüsü yok — O(1) sabit maliyet. }
  for I := 0 to N - 1 do
  begin
    if FDisplayNoiseFlags[I] then
    begin
      X1 := DataXToScreenX(I, N, AWidth);
      Y1 := DataYToScreenY(FDisplayRawData[I], AMinVal, AMaxVal, AHeight);

      { Çember çiz }
      LPaint.Style := TSkPaintStyle.Stroke;
      LPaint.Color := TAlphaColors.Maroon;
      ACanvas.DrawCircle(X1, Y1, 6, LPaint);

      { Çarpı işareti }
      ACanvas.DrawLine(X1 - 5, Y1 - 5, X1 + 5, Y1 + 5, LPaint);
      ACanvas.DrawLine(X1 + 5, Y1 - 5, X1 - 5, Y1 + 5, LPaint);
    end;
  end;
end;

procedure TFormMain.DrawMultiAxisGraph(ACanvas: ISkCanvas; AWidth, AHeight: Integer);
{ Tüm 3 ekseni farklı renklerde aynı grafikte çizer.
  X = Mavi | Y = Turuncu ($FFFF8C00) | Z = Mor ($FF8B008B) }
var
  LTextPaint: ISkPaint;
  LFont: ISkFont;
  LegendX: Single;
begin
  DrawAxisLines(ACanvas, AWidth, AHeight, FDisplayMinValAll, FDisplayMaxValAll);

  { Z ekseni — mor (arka planda) }
  if Length(FDisplayRawDataZ) > 0 then
    DrawDataSeries(ACanvas, FDisplayRawDataZ, COLOR_AXIS_PURPLE,
      AWidth, AHeight, FDisplayMinValAll, FDisplayMaxValAll, False);

  { Y ekseni — turuncu }
  if Length(FDisplayRawDataY) > 0 then
    DrawDataSeries(ACanvas, FDisplayRawDataY, COLOR_AXIS_ORANGE,
      AWidth, AHeight, FDisplayMinValAll, FDisplayMaxValAll, False);

  { X ekseni — mavi (ön planda) }
  if Length(FDisplayRawData) > 0 then
    DrawDataSeries(ACanvas, FDisplayRawData, TAlphaColors.Blue,
      AWidth, AHeight, FDisplayMinValAll, FDisplayMaxValAll, False);

  { Köşe açıklaması }
  LFont := TSkFont.Create(nil, 9);
  LTextPaint := TSkPaint.Create;
  LTextPaint.AntiAlias := True;
  LTextPaint.Style := TSkPaintStyle.Fill;

  LegendX := AWidth - 115;

  LTextPaint.Color := TAlphaColors.Blue;
  ACanvas.DrawSimpleText('— X Ekseni',
    LegendX, TextBaseline(LFont, FGraphMarginTop + 2), LFont, LTextPaint);

  LTextPaint.Color := COLOR_AXIS_ORANGE;
  ACanvas.DrawSimpleText('— Y Ekseni',
    LegendX, TextBaseline(LFont, FGraphMarginTop + 16), LFont, LTextPaint);

  LTextPaint.Color := COLOR_AXIS_PURPLE;
  ACanvas.DrawSimpleText('— Z Ekseni',
    LegendX, TextBaseline(LFont, FGraphMarginTop + 30), LFont, LTextPaint);
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
{ Sadece filtre edilmiş veriyi .csv formatında kaydeder.
  Sütunlar: serialNumber, x_filtered, y_filtered, z_filtered }
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
    SL.Add('"serialNumber","x_filtered","y_filtered","z_filtered"');

    for I := 0 to Model.Count - 1 do
    begin
      Rec := Model.GetRecord(I);
      SL.Add(Format('"%s","%.5f","%.5f","%.5f"',
        [Rec.SerialNumber, FX[I], FY[I], FZ[I]], FS));
    end;

    SL.SaveToFile(AFileName, TEncoding.UTF8);
    ShowMessage(Format('Filtreli veri kaydedildi: %s (%d kayıt)', [AFileName, Model.Count]));
  finally
    SL.Free;
  end;
end;

end.
