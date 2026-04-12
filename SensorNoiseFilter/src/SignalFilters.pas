{ ============================================================================
  SignalFilters.pas - Model Katmanı (Filtre Algoritmaları)
  
  Sinyal işleme filtreleri: Hareketli Ortalama, Medyan, Düşük Geçişli,
  Yüksek Geçişli, Bant Geçişli, Kalman ve Üstel Düzeltme filtreleri.
  
  MVC Mimarisi: MODEL
  ============================================================================ }
unit SignalFilters;

interface

uses
  System.SysUtils, System.Math, System.Generics.Collections,
  System.Generics.Defaults;

type
  { Filtre türleri }
  TFilterType = (
    ftMovingAverage,
    ftMedian,
    ftLowPass,
    ftHighPass,
    ftBandPass,
    ftKalman,
    ftExponentialSmoothing
  );

  { Filtre parametreleri }
  TFilterParams = record
    WindowSize: Integer;        { Hareketli Ortalama & Medyan için pencere boyutu }
    AlphaLow: Double;           { Düşük geçişli filtre alpha değeri }
    AlphaHigh: Double;          { Yüksek geçişli filtre alpha değeri }
    KalmanQ: Double;            { Kalman süreç gürültüsü }
    KalmanR: Double;            { Kalman ölçüm gürültüsü }
    ExponentialAlpha: Double;   { Üstel düzeltme alpha değeri }
    class function Default: TFilterParams; static;
  end;

  { Temel filtre arayüzü }
  ISignalFilter = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Hareketli Ortalama Filtresi }
  TMovingAverageFilter = class(TInterfacedObject, ISignalFilter)
  private
    FWindowSize: Integer;
  public
    constructor Create(AWindowSize: Integer);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Medyan Filtre }
  TMedianFilter = class(TInterfacedObject, ISignalFilter)
  private
    FWindowSize: Integer;
  public
    constructor Create(AWindowSize: Integer);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Düşük Geçişli Filtre (Low-Pass) }
  TLowPassFilter = class(TInterfacedObject, ISignalFilter)
  private
    FAlpha: Double;
  public
    constructor Create(AAlpha: Double);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Yüksek Geçişli Filtre (High-Pass) }
  THighPassFilter = class(TInterfacedObject, ISignalFilter)
  private
    FAlpha: Double;
  public
    constructor Create(AAlpha: Double);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Bant Geçişli Filtre (Band-Pass) }
  TBandPassFilter = class(TInterfacedObject, ISignalFilter)
  private
    FAlphaLow: Double;
    FAlphaHigh: Double;
  public
    constructor Create(AAlphaLow, AAlphaHigh: Double);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Kalman Filtresi }
  TKalmanFilter = class(TInterfacedObject, ISignalFilter)
  private
    FQ: Double;  { Süreç gürültüsü kovaryansı }
    FR: Double;  { Ölçüm gürültüsü kovaryansı }
  public
    constructor Create(AQ, AR: Double);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Üstel Düzeltme (Exponential Smoothing) }
  TExponentialSmoothingFilter = class(TInterfacedObject, ISignalFilter)
  private
    FAlpha: Double;
  public
    constructor Create(AAlpha: Double);
    function Apply(const AData: TArray<Double>): TArray<Double>;
    function GetName: string;
    function GetFilterType: TFilterType;
  end;

  { Filtre fabrikası }
  TFilterFactory = class
  public
    class function CreateFilter(AType: TFilterType; const AParams: TFilterParams): ISignalFilter;
    class function GetFilterName(AType: TFilterType): string;
  end;

implementation

{ TFilterParams }

class function TFilterParams.Default: TFilterParams;
begin
  Result.WindowSize := 5;
  Result.AlphaLow := 0.1;
  Result.AlphaHigh := 0.3;
  Result.KalmanQ := 0.01;
  Result.KalmanR := 0.1;
  Result.ExponentialAlpha := 0.3;
end;

{ TMovingAverageFilter }

constructor TMovingAverageFilter.Create(AWindowSize: Integer);
begin
  inherited Create;
  FWindowSize := Max(2, AWindowSize);
end;

function TMovingAverageFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, J, N, Half, StartIdx, EndIdx, WinCount: Integer;
  Sum: Double;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  Half := FWindowSize div 2;

  for I := 0 to N - 1 do
  begin
    StartIdx := Max(0, I - Half);
    EndIdx := Min(N - 1, I + Half);
    Sum := 0;
    WinCount := 0;

    for J := StartIdx to EndIdx do
    begin
      Sum := Sum + AData[J];
      Inc(WinCount);
    end;

    if WinCount > 0 then
      Result[I] := Sum / WinCount
    else
      Result[I] := AData[I];
  end;
end;

function TMovingAverageFilter.GetName: string;
begin
  Result := 'Hareketli Ortalama (Moving Average)';
end;

function TMovingAverageFilter.GetFilterType: TFilterType;
begin
  Result := ftMovingAverage;
end;

{ TMedianFilter }

constructor TMedianFilter.Create(AWindowSize: Integer);
begin
  inherited Create;
  { Pencere boyutu tek sayı olmalı }
  FWindowSize := Max(3, AWindowSize);
  if FWindowSize mod 2 = 0 then
    Inc(FWindowSize);
end;

function TMedianFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, J, N, Half, StartIdx, EndIdx, WinCount: Integer;
  Window: TArray<Double>;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  Half := FWindowSize div 2;

  for I := 0 to N - 1 do
  begin
    StartIdx := Max(0, I - Half);
    EndIdx := Min(N - 1, I + Half);
    WinCount := EndIdx - StartIdx + 1;

    SetLength(Window, WinCount);
    for J := 0 to WinCount - 1 do
      Window[J] := AData[StartIdx + J];

    { Sırala }
    TArray.Sort<Double>(Window);

    { Medyanı al }
    Result[I] := Window[WinCount div 2];
  end;
end;

function TMedianFilter.GetName: string;
begin
  Result := 'Medyan Filtre (Median Filter)';
end;

function TMedianFilter.GetFilterType: TFilterType;
begin
  Result := ftMedian;
end;

{ TLowPassFilter }

constructor TLowPassFilter.Create(AAlpha: Double);
begin
  inherited Create;
  FAlpha := EnsureRange(AAlpha, 0.01, 1.0);
end;

function TLowPassFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, N: Integer;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  { y[0] = x[0] }
  Result[0] := AData[0];

  { y[n] = alpha * x[n] + (1 - alpha) * y[n-1] }
  for I := 1 to N - 1 do
    Result[I] := FAlpha * AData[I] + (1.0 - FAlpha) * Result[I - 1];
end;

function TLowPassFilter.GetName: string;
begin
  Result := 'Düşük Geçişli Filtre (Low-Pass)';
end;

function TLowPassFilter.GetFilterType: TFilterType;
begin
  Result := ftLowPass;
end;

{ THighPassFilter }

constructor THighPassFilter.Create(AAlpha: Double);
begin
  inherited Create;
  FAlpha := EnsureRange(AAlpha, 0.01, 1.0);
end;

function THighPassFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, N: Integer;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  { y[0] = x[0] }
  Result[0] := AData[0];

  { y[n] = alpha * (y[n-1] + x[n] - x[n-1]) }
  for I := 1 to N - 1 do
    Result[I] := FAlpha * (Result[I - 1] + AData[I] - AData[I - 1]);
end;

function THighPassFilter.GetName: string;
begin
  Result := 'Yüksek Geçişli Filtre (High-Pass)';
end;

function THighPassFilter.GetFilterType: TFilterType;
begin
  Result := ftHighPass;
end;

{ TBandPassFilter }

constructor TBandPassFilter.Create(AAlphaLow, AAlphaHigh: Double);
begin
  inherited Create;
  FAlphaLow := EnsureRange(AAlphaLow, 0.01, 1.0);
  FAlphaHigh := EnsureRange(AAlphaHigh, 0.01, 1.0);
end;

function TBandPassFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, N: Integer;
  LowPassed: TArray<Double>;
  HighPassInput: TArray<Double>;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  { Adım 1: Düşük geçişli filtre uygula }
  SetLength(LowPassed, N);
  LowPassed[0] := AData[0];
  for I := 1 to N - 1 do
    LowPassed[I] := FAlphaLow * AData[I] + (1.0 - FAlphaLow) * LowPassed[I - 1];

  { Adım 2: Yüksek geçişli filtre uygula (düşük geçişli sonuç üzerine) }
  Result[0] := LowPassed[0];
  for I := 1 to N - 1 do
    Result[I] := FAlphaHigh * (Result[I - 1] + LowPassed[I] - LowPassed[I - 1]);
end;

function TBandPassFilter.GetName: string;
begin
  Result := 'Bant Geçişli Filtre (Band-Pass)';
end;

function TBandPassFilter.GetFilterType: TFilterType;
begin
  Result := ftBandPass;
end;

{ TKalmanFilter }

constructor TKalmanFilter.Create(AQ, AR: Double);
begin
  inherited Create;
  FQ := Max(0.001, AQ);
  FR := Max(0.001, AR);
end;

function TKalmanFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, N: Integer;
  XEst: Double;       { Tahmin edilen durum }
  PEst: Double;       { Tahmin hatası kovaryansı }
  KGain: Double;      { Kalman kazancı }
  XPred: Double;      { Önceki tahmin }
  PPred: Double;      { Önceki hata kovaryansı }
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  { Başlangıç değerleri }
  XEst := AData[0];
  PEst := 1.0;

  Result[0] := XEst;

  for I := 1 to N - 1 do
  begin
    { Tahmin adımı (Prediction) }
    XPred := XEst;
    PPred := PEst + FQ;

    { Güncelleme adımı (Update) }
    KGain := PPred / (PPred + FR);
    XEst := XPred + KGain * (AData[I] - XPred);
    PEst := (1.0 - KGain) * PPred;

    Result[I] := XEst;
  end;
end;

function TKalmanFilter.GetName: string;
begin
  Result := 'Kalman Filtresi (Kalman Filter)';
end;

function TKalmanFilter.GetFilterType: TFilterType;
begin
  Result := ftKalman;
end;

{ TExponentialSmoothingFilter }

constructor TExponentialSmoothingFilter.Create(AAlpha: Double);
begin
  inherited Create;
  FAlpha := EnsureRange(AAlpha, 0.01, 1.0);
end;

function TExponentialSmoothingFilter.Apply(const AData: TArray<Double>): TArray<Double>;
var
  I, N: Integer;
begin
  N := Length(AData);
  SetLength(Result, N);

  if N = 0 then
    Exit;

  { S[0] = X[0] }
  Result[0] := AData[0];

  { S[n] = alpha * X[n] + (1 - alpha) * S[n-1] }
  for I := 1 to N - 1 do
    Result[I] := FAlpha * AData[I] + (1.0 - FAlpha) * Result[I - 1];
end;

function TExponentialSmoothingFilter.GetName: string;
begin
  Result := 'Üstel Düzeltme (Exponential Smoothing)';
end;

function TExponentialSmoothingFilter.GetFilterType: TFilterType;
begin
  Result := ftExponentialSmoothing;
end;

{ TFilterFactory }

class function TFilterFactory.CreateFilter(AType: TFilterType;
  const AParams: TFilterParams): ISignalFilter;
begin
  case AType of
    ftMovingAverage:
      Result := TMovingAverageFilter.Create(AParams.WindowSize);
    ftMedian:
      Result := TMedianFilter.Create(AParams.WindowSize);
    ftLowPass:
      Result := TLowPassFilter.Create(AParams.AlphaLow);
    ftHighPass:
      Result := THighPassFilter.Create(AParams.AlphaHigh);
    ftBandPass:
      Result := TBandPassFilter.Create(AParams.AlphaLow, AParams.AlphaHigh);
    ftKalman:
      Result := TKalmanFilter.Create(AParams.KalmanQ, AParams.KalmanR);
    ftExponentialSmoothing:
      Result := TExponentialSmoothingFilter.Create(AParams.ExponentialAlpha);
  else
    raise Exception.Create('Bilinmeyen filtre türü');
  end;
end;

class function TFilterFactory.GetFilterName(AType: TFilterType): string;
begin
  case AType of
    ftMovingAverage:        Result := 'Hareketli Ortalama';
    ftMedian:               Result := 'Medyan Filtre';
    ftLowPass:              Result := 'Düşük Geçişli (Low-Pass)';
    ftHighPass:             Result := 'Yüksek Geçişli (High-Pass)';
    ftBandPass:             Result := 'Bant Geçişli (Band-Pass)';
    ftKalman:               Result := 'Kalman Filtresi';
    ftExponentialSmoothing: Result := 'Üstel Düzeltme';
  else
    Result := 'Bilinmeyen';
  end;
end;

end.
