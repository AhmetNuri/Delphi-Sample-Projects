{ ============================================================================
  FilterController.pas - Controller Katmanı
  
  Model ve View arasındaki koordinasyonu sağlar. CSV yükleme, filtre uygulama,
  gürültü tespiti ve görselleştirme güncellemelerini yönetir.
  
  MVC Mimarisi: CONTROLLER
  ============================================================================ }
unit FilterController;

interface

uses
  System.SysUtils, System.Classes, System.Math,
  System.Generics.Collections, System.Generics.Defaults,
  SensorDataModel, SignalFilters;

type
  { View'a bildirim göndermek için olay tipleri }
  TDataLoadedEvent = procedure(Sender: TObject; ARecordCount: Integer) of object;
  TFilterAppliedEvent = procedure(Sender: TObject; AFilterName: string) of object;
  TNoiseDetectedEvent = procedure(Sender: TObject; ANoiseCountX, ANoiseCountY, ANoiseCountZ: Integer) of object;
  TStatusMessageEvent = procedure(Sender: TObject; const AMessage: string) of object;
  TStatisticsUpdatedEvent = procedure(Sender: TObject;
    const ARawStats, AFilteredStats: TStatistics; AAxis: TAxisType) of object;
  TProgressEvent = procedure(Sender: TObject; APercent: Integer; const AMessage: string) of object;

  { Ana controller sınıfı }
  TFilterController = class
  private
    FModel: TSensorDataModel;
    FCurrentFilter: ISignalFilter;
    FCurrentFilterType: TFilterType;
    FFilterParams: TFilterParams;
    FSelectedAxis: TAxisType;

    { Olaylar }
    FOnDataLoaded: TDataLoadedEvent;
    FOnFilterApplied: TFilterAppliedEvent;
    FOnNoiseDetected: TNoiseDetectedEvent;
    FOnStatusMessage: TStatusMessageEvent;
    FOnStatisticsUpdated: TStatisticsUpdatedEvent;
    FOnProgress: TProgressEvent;

    procedure DoStatusMessage(const AMsg: string);
    procedure DoProgress(APercent: Integer; const AMsg: string);
  public
    constructor Create;
    destructor Destroy; override;

    { CSV dosyasını yükle }
    procedure LoadCSV(const AFileName: string);

    { Filtre parametrelerini ayarla }
    procedure SetFilterParams(const AParams: TFilterParams);
    function GetFilterParams: TFilterParams;

    { Filtre türünü seç ve uygula }
    procedure SelectFilter(AType: TFilterType);
    procedure ApplyFilter;

    { Gürültü tespiti }
    procedure DetectNoise(ASigmaMultiplier: Double);

    { Eksen seçimi }
    procedure SelectAxis(AAxis: TAxisType);

    { Veri erişimi }
    function GetModel: TSensorDataModel;
    function GetSelectedAxis: TAxisType;
    function GetCurrentFilterType: TFilterType;
    function GetRawData: TArray<Double>;
    function GetFilteredData: TArray<Double>;

    { İstatistik hesapla }
    procedure UpdateStatistics;

    { Gürültü seviyesi otomatik tahmini - MAD (Medyan Mutlak Sapma) tabanlı }
    function EstimateNoiseSigmaMAD(AAxis: TAxisType): Double;

    { Seçili eksen için önerilen sigma çarpanını hesapla }
    function GetSuggestedSigmaMultiplier(AAxis: TAxisType): Double;

    { Olaylar }
    property OnDataLoaded: TDataLoadedEvent read FOnDataLoaded write FOnDataLoaded;
    property OnFilterApplied: TFilterAppliedEvent read FOnFilterApplied write FOnFilterApplied;
    property OnNoiseDetected: TNoiseDetectedEvent read FOnNoiseDetected write FOnNoiseDetected;
    property OnStatusMessage: TStatusMessageEvent read FOnStatusMessage write FOnStatusMessage;
    property OnStatisticsUpdated: TStatisticsUpdatedEvent read FOnStatisticsUpdated write FOnStatisticsUpdated;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

const
  { MAD → sigma dönüşüm katsayısı: normal dağılım için tutarlı tahmin sağlar.
    İspat: sigma = k * MAD, k = 1 / (sqrt(2) * erfinv(0.5)) ≈ 1.4826 }
  MAD_TO_SIGMA_SCALE_FACTOR = 1.4826;

  { Önerilen sigma çarpanı sınırları }
  MIN_SIGMA_MULTIPLIER = 0.5;
  MAX_SIGMA_MULTIPLIER = 5.0;

  { Normal dağılımın %95 güven aralığı z-değeri }
  NORMAL_DIST_95_PERCENTILE = 1.96;

{ TFilterController }

constructor TFilterController.Create;
begin
  inherited Create;
  FModel := TSensorDataModel.Create;
  FFilterParams := TFilterParams.Default;
  FCurrentFilterType := ftMovingAverage;
  FSelectedAxis := atX;
end;

destructor TFilterController.Destroy;
begin
  FCurrentFilter := nil;
  FModel.Free;
  inherited;
end;

procedure TFilterController.DoStatusMessage(const AMsg: string);
begin
  if Assigned(FOnStatusMessage) then
    FOnStatusMessage(Self, AMsg);
end;

procedure TFilterController.DoProgress(APercent: Integer; const AMsg: string);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, APercent, AMsg);
end;

procedure TFilterController.LoadCSV(const AFileName: string);
begin
  try
    DoProgress(0, 'CSV yükleniyor...');
    DoStatusMessage('CSV dosyası yükleniyor: ' + ExtractFileName(AFileName));
    FModel.LoadFromCSV(AFileName);
    DoProgress(70, Format('%d kayıt yüklendi.', [FModel.Count]));
    DoStatusMessage(Format('Toplam %d kayıt yüklendi.', [FModel.Count]));

    if Assigned(FOnDataLoaded) then
      FOnDataLoaded(Self, FModel.Count);

    { Otomatik gürültü tespiti }
    DoProgress(80, 'Gürültü tespiti...');
    DetectNoise(FModel.NoiseSigmaMultiplier);
    DoProgress(100, 'Hazır.');

  except
    on E: Exception do
    begin
      DoStatusMessage('Hata: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TFilterController.SetFilterParams(const AParams: TFilterParams);
begin
  FFilterParams := AParams;
end;

function TFilterController.GetFilterParams: TFilterParams;
begin
  Result := FFilterParams;
end;

procedure TFilterController.SelectFilter(AType: TFilterType);
begin
  FCurrentFilterType := AType;
  FCurrentFilter := TFilterFactory.CreateFilter(AType, FFilterParams);
  DoStatusMessage('Filtre seçildi: ' + TFilterFactory.GetFilterName(AType));
end;

procedure TFilterController.ApplyFilter;
var
  RawData, Filtered: TArray<Double>;
  FilterName: string;
  Axis: TAxisType;
  AxisIdx: Integer;
begin
  if FModel.Count = 0 then
  begin
    DoStatusMessage('Önce bir CSV dosyası yükleyin.');
    Exit;
  end;

  { Filtre oluştur }
  FCurrentFilter := TFilterFactory.CreateFilter(FCurrentFilterType, FFilterParams);
  FilterName := FCurrentFilter.GetName;

  DoStatusMessage('Filtre uygulanıyor: ' + FilterName);
  DoProgress(0, 'Filtre uygulanıyor...');

  { Tüm eksenlere filtre uygula — progress per axis }
  AxisIdx := 0;
  for Axis := Low(TAxisType) to High(TAxisType) do
  begin
    RawData  := FModel.GetAxisData(Axis);
    Filtered := FCurrentFilter.Apply(RawData);
    FModel.SetFilteredData(Axis, Filtered);
    Inc(AxisIdx);
    DoProgress(Round(AxisIdx / 3 * 100),
      Format('Filtre: %s (%d/3 eksen)', [FilterName, AxisIdx]));
  end;

  DoStatusMessage('Filtre uygulandı: ' + FilterName);

  if Assigned(FOnFilterApplied) then
    FOnFilterApplied(Self, FilterName);

  UpdateStatistics;
end;

procedure TFilterController.DetectNoise(ASigmaMultiplier: Double);
var
  I, NoiseX, NoiseY, NoiseZ: Integer;
  Rec: TSensorRecord;
begin
  if FModel.Count = 0 then
    Exit;

  DoProgress(0, 'Gürültü tespiti yapılıyor...');
  FModel.DetectNoise(ASigmaMultiplier);

  NoiseX := 0;
  NoiseY := 0;
  NoiseZ := 0;

  for I := 0 to FModel.Count - 1 do
  begin
    Rec := FModel.GetRecord(I);
    if Rec.IsNoiseX then Inc(NoiseX);
    if Rec.IsNoiseY then Inc(NoiseY);
    if Rec.IsNoiseZ then Inc(NoiseZ);
  end;

  DoStatusMessage(Format('Gürültü tespiti: X=%d, Y=%d, Z=%d nokta', [NoiseX, NoiseY, NoiseZ]));
  DoProgress(100, 'Gürültü tespiti tamamlandı.');

  if Assigned(FOnNoiseDetected) then
    FOnNoiseDetected(Self, NoiseX, NoiseY, NoiseZ);
end;

procedure TFilterController.SelectAxis(AAxis: TAxisType);
begin
  FSelectedAxis := AAxis;
  case AAxis of
    atX: DoStatusMessage('X ekseni seçildi');
    atY: DoStatusMessage('Y ekseni seçildi');
    atZ: DoStatusMessage('Z ekseni seçildi');
  end;
end;

function TFilterController.GetModel: TSensorDataModel;
begin
  Result := FModel;
end;

function TFilterController.GetSelectedAxis: TAxisType;
begin
  Result := FSelectedAxis;
end;

function TFilterController.GetCurrentFilterType: TFilterType;
begin
  Result := FCurrentFilterType;
end;

function TFilterController.GetRawData: TArray<Double>;
begin
  Result := FModel.GetAxisData(FSelectedAxis);
end;

function TFilterController.GetFilteredData: TArray<Double>;
begin
  Result := FModel.GetFilteredData(FSelectedAxis);
end;

procedure TFilterController.UpdateStatistics;
var
  RawData, FilteredData: TArray<Double>;
  RawStats, FilteredStats: TStatistics;
begin
  RawData := GetRawData;
  FilteredData := GetFilteredData;

  RawStats := FModel.CalculateStatistics(RawData);

  if Length(FilteredData) > 0 then
    FilteredStats := FModel.CalculateStatistics(FilteredData)
  else
    FilteredStats := Default(TStatistics);

  if Assigned(FOnStatisticsUpdated) then
    FOnStatisticsUpdated(Self, RawStats, FilteredStats, FSelectedAxis);
end;

function TFilterController.EstimateNoiseSigmaMAD(AAxis: TAxisType): Double;
{ MAD (Median Absolute Deviation) tabanlı gürültü sigma tahmini.
  Formül: sigma_hat = 1.4826 * median(|xi - median(x)|)
  1.4826 katsayısı normal dağılım için tutarlı bir tahmin sağlar. }
var
  RawData, SortedData, MADValues: TArray<Double>;
  I, N: Integer;
  MedianVal, MAD: Double;
begin
  Result := 0;
  RawData := FModel.GetAxisData(AAxis);
  N := Length(RawData);
  if N < 2 then
    Exit;

  { Adım 1: Medyanı bul }
  SortedData := Copy(RawData);
  TArray.Sort<Double>(SortedData, TComparer<Double>.Default);
  if N mod 2 = 1 then
    MedianVal := SortedData[N div 2]
  else
    MedianVal := (SortedData[(N div 2) - 1] + SortedData[N div 2]) / 2.0;

  { Adım 2: Her nokta için |xi - median| hesapla }
  SetLength(MADValues, N);
  for I := 0 to N - 1 do
    MADValues[I] := Abs(RawData[I] - MedianVal);

  { Adım 3: Mutlak sapmaların medyanını bul (MAD) }
  TArray.Sort<Double>(MADValues, TComparer<Double>.Default);
  if N mod 2 = 1 then
    MAD := MADValues[N div 2]
  else
    MAD := (MADValues[(N div 2) - 1] + MADValues[N div 2]) / 2.0;

  { Normal dağılım için tutarlı tahmin: sigma = MAD_TO_SIGMA_SCALE_FACTOR * MAD }
  Result := MAD_TO_SIGMA_SCALE_FACTOR * MAD;
end;

function TFilterController.GetSuggestedSigmaMultiplier(AAxis: TAxisType): Double;
{ Veri karakteristiğine göre önerilen sigma çarpanını hesaplar.
  MAD tahmini ve örnek standart sapması oranına dayanır:
  - Veri temizse (az aykırı değer): MAD_sigma ≈ StdDev → multiplier ≈ 1.96
  - Çok aykırı değer varsa: MAD_sigma << StdDev → multiplier düşer
  Sonuç [0.5, 5.0] aralığına sınırlandırılır. }
var
  RawData: TArray<Double>;
  MADSigma, SampleStdDev: Double;
  Stats: TStatistics;
begin
  Result := 2.0;
  RawData := FModel.GetAxisData(AAxis);
  if Length(RawData) < 2 then
    Exit;

  MADSigma := EstimateNoiseSigmaMAD(AAxis);
  Stats := FModel.CalculateStatistics(RawData);
  SampleStdDev := Stats.StdDev;

  if (SampleStdDev < 1E-10) or (MADSigma < 1E-10) then
    Exit;

  { 1.96 * (MAD_sigma / sample_stddev) — normal dağılım için %95 eşiği }
  Result := EnsureRange(
    NORMAL_DIST_95_PERCENTILE * MADSigma / SampleStdDev,
    MIN_SIGMA_MULTIPLIER,
    MAX_SIGMA_MULTIPLIER);
end;

end.
