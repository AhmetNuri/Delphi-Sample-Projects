{ ============================================================================
  FilterController.pas - Controller Katmanı
  
  Model ve View arasındaki koordinasyonu sağlar. CSV yükleme, filtre uygulama,
  gürültü tespiti ve görselleştirme güncellemelerini yönetir.
  
  MVC Mimarisi: CONTROLLER
  ============================================================================ }
unit FilterController;

interface

uses
  System.SysUtils, System.Classes, SensorDataModel, SignalFilters;

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

    { Olaylar }
    property OnDataLoaded: TDataLoadedEvent read FOnDataLoaded write FOnDataLoaded;
    property OnFilterApplied: TFilterAppliedEvent read FOnFilterApplied write FOnFilterApplied;
    property OnNoiseDetected: TNoiseDetectedEvent read FOnNoiseDetected write FOnNoiseDetected;
    property OnStatusMessage: TStatusMessageEvent read FOnStatusMessage write FOnStatusMessage;
    property OnStatisticsUpdated: TStatisticsUpdatedEvent read FOnStatisticsUpdated write FOnStatisticsUpdated;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

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

end.
