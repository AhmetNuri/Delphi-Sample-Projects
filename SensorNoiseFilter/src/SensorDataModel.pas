{ ============================================================================
  SensorDataModel.pas - Model Katmanı
  
  Sensör veri yapıları, CSV dosyasından veri okuma, istatistiksel analiz ve
  gürültü noktası tespit fonksiyonlarını içerir.
  
  MVC Mimarisi: MODEL
  ============================================================================ }
unit SensorDataModel;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  System.Generics.Defaults, System.Math, System.IOUtils;

type
  { Tek bir sensör ölçüm kaydı }
  TSensorRecord = record
    SerialNumber: string;
    X: Double;
    Y: Double;
    Z: Double;
    Time: TDateTime;
    DeviceTime: TDateTime;
    IsNoiseX: Boolean;
    IsNoiseY: Boolean;
    IsNoiseZ: Boolean;
  end;

  { Eksen seçimi }
  TAxisType = (atX, atY, atZ);

  { İstatistiksel bilgiler }
  TStatistics = record
    Mean: Double;
    StdDev: Double;
    Min: Double;
    Max: Double;
    Variance: Double;
    Count: Integer;
  end;

  { Sensör veri listesi ve işlemleri }
  TSensorDataModel = class
  private
    FData: TList<TSensorRecord>;
    FFilteredX: TArray<Double>;
    FFilteredY: TArray<Double>;
    FFilteredZ: TArray<Double>;
    FNoiseSigmaMultiplier: Double;
    function ParseDateTime(const AStr: string): TDateTime;
    function StripQuotes(const AStr: string): string;
  public
    constructor Create;
    destructor Destroy; override;

    { CSV dosyasından veri yükleme }
    procedure LoadFromCSV(const AFileName: string);

    { Veri erişimi }
    function GetCount: Integer;
    function GetRecord(AIndex: Integer): TSensorRecord;
    procedure SetRecord(AIndex: Integer; const ARec: TSensorRecord);

    { Eksen verilerini dizi olarak döndürme }
    function GetAxisData(AAxis: TAxisType): TArray<Double>;

    { Filtrelenmiş verileri ayarlama }
    procedure SetFilteredData(AAxis: TAxisType; const AData: TArray<Double>);
    function GetFilteredData(AAxis: TAxisType): TArray<Double>;

    { İstatistiksel analiz }
    function CalculateStatistics(const AData: TArray<Double>): TStatistics;

    { Gürültü tespiti - Standart sapma tabanlı }
    procedure DetectNoise(ASigmaMultiplier: Double);

    { Temizle }
    procedure Clear;

    property Count: Integer read GetCount;
    property NoiseSigmaMultiplier: Double read FNoiseSigmaMultiplier write FNoiseSigmaMultiplier;
  end;

implementation

{ TSensorDataModel }

constructor TSensorDataModel.Create;
begin
  inherited Create;
  FData := TList<TSensorRecord>.Create;
  FNoiseSigmaMultiplier := 2.0;
end;

destructor TSensorDataModel.Destroy;
begin
  FData.Free;
  inherited;
end;

function TSensorDataModel.StripQuotes(const AStr: string): string;
begin
  Result := Trim(AStr);
  if (Length(Result) >= 2) and (Result[1] = '"') and (Result[Length(Result)] = '"') then
    Result := Copy(Result, 2, Length(Result) - 2);
end;

function TSensorDataModel.ParseDateTime(const AStr: string): TDateTime;
var
  FormatSettings: TFormatSettings;
  DateStr: string;
begin
  FormatSettings := TFormatSettings.Create;
  FormatSettings.DateSeparator := '-';
  FormatSettings.TimeSeparator := ':';
  FormatSettings.ShortDateFormat := 'yyyy-mm-dd';
  FormatSettings.LongTimeFormat := 'hh:nn:ss.zzz';

  DateStr := StripQuotes(AStr);
  { Mikrosaniye kısmını kırp - sadece milisaniyeye kadar al }
  if Length(DateStr) > 23 then
    DateStr := Copy(DateStr, 1, 23);

  try
    Result := StrToDateTime(DateStr, FormatSettings);
  except
    Result := 0;
  end;
end;

procedure TSensorDataModel.LoadFromCSV(const AFileName: string);
var
  Lines: TStringList;
  I: Integer;
  Fields: TArray<string>;
  Rec: TSensorRecord;
  FS: TFormatSettings;
begin
  Clear;

  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';

  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(AFileName, TEncoding.UTF8);

    { İlk satır başlık, 1'den başla }
    for I := 1 to Lines.Count - 1 do
    begin
      if Trim(Lines[I]) = '' then
        Continue;

      Fields := Lines[I].Split([',']);
      if Length(Fields) < 6 then
        Continue;

      Rec := Default(TSensorRecord);
      Rec.SerialNumber := StripQuotes(Fields[0]);

      try
        Rec.X := StrToFloat(StripQuotes(Fields[1]), FS);
        Rec.Y := StrToFloat(StripQuotes(Fields[2]), FS);
        Rec.Z := StrToFloat(StripQuotes(Fields[3]), FS);
      except
        Continue; { Geçersiz sayısal veri, satırı atla }
      end;

      Rec.Time := ParseDateTime(Fields[4]);
      Rec.DeviceTime := ParseDateTime(Fields[5]);
      Rec.IsNoiseX := False;
      Rec.IsNoiseY := False;
      Rec.IsNoiseZ := False;

      FData.Add(Rec);
    end;
  finally
    Lines.Free;
  end;
end;

function TSensorDataModel.GetCount: Integer;
begin
  Result := FData.Count;
end;

function TSensorDataModel.GetRecord(AIndex: Integer): TSensorRecord;
begin
  Result := FData[AIndex];
end;

procedure TSensorDataModel.SetRecord(AIndex: Integer; const ARec: TSensorRecord);
begin
  FData[AIndex] := ARec;
end;

function TSensorDataModel.GetAxisData(AAxis: TAxisType): TArray<Double>;
var
  I: Integer;
begin
  SetLength(Result, FData.Count);
  for I := 0 to FData.Count - 1 do
  begin
    case AAxis of
      atX: Result[I] := FData[I].X;
      atY: Result[I] := FData[I].Y;
      atZ: Result[I] := FData[I].Z;
    end;
  end;
end;

procedure TSensorDataModel.SetFilteredData(AAxis: TAxisType; const AData: TArray<Double>);
begin
  case AAxis of
    atX: FFilteredX := Copy(AData);
    atY: FFilteredY := Copy(AData);
    atZ: FFilteredZ := Copy(AData);
  end;
end;

function TSensorDataModel.GetFilteredData(AAxis: TAxisType): TArray<Double>;
begin
  case AAxis of
    atX: Result := FFilteredX;
    atY: Result := FFilteredY;
    atZ: Result := FFilteredZ;
  end;
end;

function TSensorDataModel.CalculateStatistics(const AData: TArray<Double>): TStatistics;
var
  I, N: Integer;
  Sum, SumSq: Double;
begin
  N := Length(AData);
  Result := Default(TStatistics);
  Result.Count := N;

  if N = 0 then
    Exit;

  Sum := 0;
  Result.Min := AData[0];
  Result.Max := AData[0];

  for I := 0 to N - 1 do
  begin
    Sum := Sum + AData[I];
    if AData[I] < Result.Min then
      Result.Min := AData[I];
    if AData[I] > Result.Max then
      Result.Max := AData[I];
  end;

  Result.Mean := Sum / N;

  SumSq := 0;
  for I := 0 to N - 1 do
    SumSq := SumSq + Sqr(AData[I] - Result.Mean);

  if N > 1 then
    Result.Variance := SumSq / (N - 1)
  else
    Result.Variance := 0;

  Result.StdDev := Sqrt(Result.Variance);
end;

procedure TSensorDataModel.DetectNoise(ASigmaMultiplier: Double);
var
  I: Integer;
  StatsX, StatsY, StatsZ: TStatistics;
  DataX, DataY, DataZ: TArray<Double>;
  Rec: TSensorRecord;
  ThresholdX, ThresholdY, ThresholdZ: Double;
begin
  FNoiseSigmaMultiplier := ASigmaMultiplier;

  DataX := GetAxisData(atX);
  DataY := GetAxisData(atY);
  DataZ := GetAxisData(atZ);

  StatsX := CalculateStatistics(DataX);
  StatsY := CalculateStatistics(DataY);
  StatsZ := CalculateStatistics(DataZ);

  ThresholdX := StatsX.StdDev * ASigmaMultiplier;
  ThresholdY := StatsY.StdDev * ASigmaMultiplier;
  ThresholdZ := StatsZ.StdDev * ASigmaMultiplier;

  for I := 0 to FData.Count - 1 do
  begin
    Rec := FData[I];
    Rec.IsNoiseX := Abs(Rec.X - StatsX.Mean) > ThresholdX;
    Rec.IsNoiseY := Abs(Rec.Y - StatsY.Mean) > ThresholdY;
    Rec.IsNoiseZ := Abs(Rec.Z - StatsZ.Mean) > ThresholdZ;
    FData[I] := Rec;
  end;
end;

procedure TSensorDataModel.Clear;
begin
  FData.Clear;
  SetLength(FFilteredX, 0);
  SetLength(FFilteredY, 0);
  SetLength(FFilteredZ, 0);
end;

end.
