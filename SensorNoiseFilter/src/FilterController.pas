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
  { Kapsamlı gürültü analizi sonuç kaydı.

    Üç tamamlayıcı yöntem kullanılır:
      1. MAD-Sigma   : Aykırı değerlere dayanıklı, tüm gürültü tahmini.
      2. RMS-Noise   : Birinci fark yöntemi ile yüksek-frekanslı gürültü.
      3. SNR          : Sinyal güç / gürültü güç oranı (dB).

    Bunların yanı sıra, hesaplanan değerlerden otomatik Kalman Q/R ve sigma
    çarpanı önerileri türetilir. }
  TNoiseAnalysisResult = record
    MADSigma         : Double;  { MAD-tabanlı gürültü sigma tahmini }
    RMSNoise         : Double;  { Birinci-fark RMS gürültüsü: std(Δx)/√2 }
    SignalRMS        : Double;  { Sinyalin RMS büyüklüğü (ortalama çıkarılmış) }
    SNR_dB           : Double;  { Sinyal-Gürültü Oranı (dB): 20·log10(SignalRMS/RMSNoise) }
    NoiseVariance    : Double;  { RMSNoise² — Kalman R başlangıç değeri }
    SuggestedSigma   : Double;  { Önerilen sigma çarpanı (gürültü tespiti için) }
    SuggestedKalmanR : Double;  { Önerilen Kalman ölçüm gürültüsü kovaryansı }
    SuggestedKalmanQ : Double;  { Önerilen Kalman süreç gürültüsü kovaryansı }
    SuggestedAlpha   : Double;  { Önerilen alçak-geçiş / üstel-düzeltme alpha değeri }
    SampleCount      : Integer; { Analize dahil edilen örnek sayısı }
  end;

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

    { Kapsamlı gürültü analizi - çoklu yöntem (MAD, RMS-fark, SNR, öneri) }
    function AnalyzeNoise(AAxis: TAxisType): TNoiseAnalysisResult;

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

  { AnalyzeNoise: sinyal ve gürültünün ihmal edilebilir küçüklükte olduğu
    dejenere durumlarda kullanılan varsayılan alçak-geçiş alpha değeri.
    0.1 seçimi pratik bir orta-nokta (ne çok hızlı ne çok yavaş) sağlar. }
  DEFAULT_ANALYSIS_ALPHA = 0.1;

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

  Yöntem: Çarpan = NORMAL_DIST_95_PERCENTILE * (MAD_sigma / SampleStdDev)

  - Temiz veri (az aykırı değer):
      MAD_sigma ≈ SampleStdDev  →  çarpan ≈ 1.96 (standart %95 eşiği)
  - Çok aykırı değer içeren veri:
      SampleStdDev, aykırı değerlerle şişer; MAD_sigma küçük kalır
      →  MAD_sigma / SampleStdDev < 1  →  çarpan 1.96'dan küçük
  Bu sayede eşik değeri, şişirilmiş StdDev'e değil asıl gürültü seviyesine
  (MAD_sigma) göre ayarlanır; aykırı değerleri daha etkin yakalar.

  Sonuç [MIN_SIGMA_MULTIPLIER, MAX_SIGMA_MULTIPLIER] aralığına sınırlandırılır. }
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

function TFilterController.AnalyzeNoise(AAxis: TAxisType): TNoiseAnalysisResult;
{ Kapsamlı sensör iç gürültüsü analizi.

  Yöntem 1 — MAD-Sigma (tüm veri üzerinde, aykırı değerlere dayanıklı)
    sigma_MAD = 1.4826 × median(|x − median(x)|)

  Yöntem 2 — Birinci-Fark RMS Gürültüsü (yüksek-frekanslı gürültü tahmini)
    Birinci fark d[n] = x[n] − x[n-1] alındığında, iki bağımsız beyaz-gürültü
    örneğinin farkı olduğundan varyansı 2·σ²_noise olur.
    → σ_noise_rms = std(d) / sqrt(2)
    Bu yöntem sinyalin DC veya alçak-frekanslı bileşenlerine duyarsızdır.

  Yöntem 3 — SNR (dB)
    Sinyal RMS'i (ortalama çıkarıldıktan sonra) ile gürültü RMS'i oranından:
    SNR_dB = 20 × log10(SignalRMS / RMSNoise)
    Gürültü ≥ sinyal ise SNR ≤ 0 dB olarak döner.

  Öneriler
    KalmanR  : Gürültü varyansı (σ²_noise) — ölçüm gürültüsü kovaryansı
    KalmanQ  : Dinamik değişim hızına göre ölçeklenir: Q = R × 0.001
    Alpha    : Kesim frekansı temsili: α ≈ RMSNoise / (RMSNoise + SignalRMS)
    SigmaCharpani: GetSuggestedSigmaMultiplier() ile zaten hesaplanır }
var
  RawData : TArray<Double>;
  N, I    : Integer;
  { Ortalama }
  Sum, Mean: Double;
  { Sinyal RMS }
  SumSq, SigRMS: Double;
  { Birinci fark }
  DiffMean, DiffSumSq, DiffStd, RMSNoise: Double;
begin
  Result := Default(TNoiseAnalysisResult);

  RawData := FModel.GetAxisData(AAxis);
  N := Length(RawData);
  Result.SampleCount := N;

  if N < 4 then
    Exit;

  { --- Ortalama --- }
  Sum := 0;
  for I := 0 to N - 1 do
    Sum := Sum + RawData[I];
  Mean := Sum / N;

  { --- Sinyal RMS (ortalama çıkarılmış) --- }
  SumSq := 0;
  for I := 0 to N - 1 do
    SumSq := SumSq + Sqr(RawData[I] - Mean);
  SigRMS := Sqrt(SumSq / N);
  Result.SignalRMS := SigRMS;

  { --- Yöntem 1: MAD-Sigma --- }
  Result.MADSigma := EstimateNoiseSigmaMAD(AAxis);

  { --- Yöntem 2: Birinci-Fark RMS Gürültüsü --- }
  { d[n] = x[n] - x[n-1]; N-1 fark noktası }
  DiffMean := 0;
  for I := 1 to N - 1 do
    DiffMean := DiffMean + (RawData[I] - RawData[I - 1]);
  DiffMean := DiffMean / (N - 1);

  DiffSumSq := 0;
  for I := 1 to N - 1 do
    DiffSumSq := DiffSumSq + Sqr((RawData[I] - RawData[I - 1]) - DiffMean);

  DiffStd := Sqrt(DiffSumSq / (N - 2));
  { Bölen: fark dizisindeki N-1 örnekten biri serbestlik derecesi için
    çıkarılır → N-1-1 = N-2. Beyaz gürültü varsayımında σ²_diff = 2·σ²_noise. }
  RMSNoise := DiffStd / Sqrt(2.0);        { Beyaz gürültü varsayımı }

  if RMSNoise < 1E-12 then
    RMSNoise := 1E-12;
  Result.RMSNoise := RMSNoise;

  { --- Yöntem 3: SNR --- }
  if (SigRMS > 1E-12) and (RMSNoise > 1E-12) then
    Result.SNR_dB := 20.0 * Log10(SigRMS / RMSNoise)
  else
    Result.SNR_dB := 0;

  { --- Türetilmiş Öneriler --- }
  Result.NoiseVariance    := Sqr(RMSNoise);
  Result.SuggestedKalmanR := Max(1E-6, Result.NoiseVariance);
  { Q tipik olarak R'ın binde biri kadar küçük tutulur }
  Result.SuggestedKalmanQ := Max(1E-9, Result.SuggestedKalmanR * 0.001);
  { α ≈ gürültü / (gürültü + sinyal): küçük gürültüde alpha küçük kalır → daha fazla düzeltme }
  if (RMSNoise + SigRMS) > 1E-12 then
    Result.SuggestedAlpha := EnsureRange(RMSNoise / (RMSNoise + SigRMS), 0.01, 0.99)
  else
    Result.SuggestedAlpha := DEFAULT_ANALYSIS_ALPHA;

  Result.SuggestedSigma := GetSuggestedSigmaMultiplier(AAxis);
end;

end.
