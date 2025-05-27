unit CadSpline;

interface

uses Point2D, Point3D, CadVertex, Generics.Collections, Shape, ShapeType,
  PointSplitter;

type
  TCadSpline = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FDiscreteId: integer;

    function NewID: Single;

    function DeBoor(k: Integer; u: Single): TPoint3D;
    function FindKnotSpan(u: Single): Integer;

    function SplitByInterval(interval: Single): TList<TPoint3D>;
    function SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
  public
    Layer: string;
    Color: string;
    degree: integer;
    knots: TList<Single>;
    Points: TList<TPoint2D>;
    Closed: Boolean;

    function GetType: TShapeType;
    function GetDetail: string;
    function GetLayer: string;
    function GetColor: string;

    function GetStart: TPoint2D;
    function GetEnd: TPoint2D;

    procedure SetId(Id: Single);
    function GetId: Single;

    procedure SetIsDiscrete(Value: Boolean);
    function GetIsDiscrete: Boolean;

    function ToPoints(splitter: TPointSplitter): TList<TPoint3D>;

    constructor Create(knots: TList<Single>; Points: TList<TPoint2D>;
      degree: integer; const Layer, Color: string; Closed: Boolean);
    destructor Destroy; override;
  end;

implementation

uses Classes, SysUtils, Math;

{ TCadSpline }

constructor TCadSpline.Create(knots: TList<Single>; Points: TList<TPoint2D>;
  degree: integer; const Layer, Color: string; Closed: Boolean);
begin
  self.knots := knots;
  self.Points := Points;
  self.Layer := Layer;
  self.Closed := Closed;
  self.Color := Color;
  self.degree := degree;
  self.FDiscreteId := 1;
end;

destructor TCadSpline.Destroy;
var
  I: Integer;
begin
  if Assigned(Points) then
  begin
    for I := 0 to Points.Count - 1 do
    begin
      if Assigned(Points[I]) then
      begin
        Points[I].Free;
        Points[I] := nil;
      end;
    end;
    Points.Free;
    Points := nil;
  end;

  if Assigned(knots) then
  begin
    knots.Free;
    knots := nil;
  end;

  inherited;
end;

function TCadSpline.GetColor: string;
begin
  result := Color;
end;

function TCadSpline.GetDetail: string;
begin
  result := '';
end;

function TCadSpline.GetEnd: TPoint2D;
begin
  if Points.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  if not Closed then
    result := TPoint2D.Create(Points[Points.Count - 1].D1,
      Points[Points.Count - 1].D2)
  else
    result := TPoint2D.Create(Points[0].D1, Points[0].D2);
end;

function TCadSpline.GetId: Single;
begin
  result := FID;
end;

function TCadSpline.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

function TCadSpline.GetLayer: string;
begin
  result := Layer;
end;

function TCadSpline.GetStart: TPoint2D;
begin
  if Points.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  result := TPoint2D.Create(Points[0].D1, Points[0].D2);
end;

function TCadSpline.GetType: TShapeType;
begin
  result := stSpline;
end;

function TCadSpline.NewID: Single;
begin
  if (not FIsDiscrete) then
  begin
    result := FID;
    exit;
  end;

  result := FID + (FDiscreteId / Power(10, FDiscreteId.ToString.Length));

  if FDiscreteId.ToString.EndsWith('9') then
    inc(FDiscreteId);

  inc(FDiscreteId);
end;

procedure TCadSpline.SetId(Id: Single);
begin
  FID := Id;
end;

procedure TCadSpline.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadSpline.SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
begin
  if countAtUnit <= 0 then
    raise Exception.Create('Point count at unit must be greator than zero!');

  result := SplitByInterval(1 / countAtUnit);
end;

function TCadSpline.DeBoor(k: Integer; u: Single): TPoint3D;
var
  d: array of TPoint3D;
  i, j, r: Integer;
  alpha: Double;
begin
  SetLength(d, degree + 1);
  for j := 0 to degree do
    d[j] := TPoint3D.Create(Points[k - degree + j].D1,
      Points[k - degree + j].D2, NewID);

  for r := 1 to degree do
    for j := degree downto r do
    begin
      i := k - degree + j;
      if (knots[i + degree + 1 - r] - knots[i]) <> 0 then
        alpha := (u - knots[i]) / (knots[i + degree + 1 - r] - knots[i])
      else
        alpha := 0;

      d[j].D1 := (1 - alpha) * d[j - 1].D1 + alpha * d[j].D1;
      d[j].D2 := (1 - alpha) * d[j - 1].D2 + alpha * d[j].D2;
    end;

  result := d[degree];
end;

function TCadSpline.FindKnotSpan(u: Single): Integer;
var
  n, low, high, mid: Integer;
begin
  n := knots.Count - degree - 1;

  if u >= knots[n] then
  begin
    result := n - 1;
    exit;
  end;

  low := degree;
  high := n;
  mid := (low + high) div 2;

  while (u < knots[mid]) or (u >= knots[mid + 1]) do
  begin
    if u < knots[mid] then
      high := mid
    else
      low := mid;
    mid := (low + high) div 2;
  end;

  result := mid;
end;

function TCadSpline.SplitByInterval(interval: Single): TList<TPoint3D>;
const
  HighResStep = 0.001;
var
  HighResPoints: TList<TPoint3D>;
  Distances: TList<Single>;
  totalLength, currentLength, targetLength, segment: Single;
  i: Integer;
  pt1, pt2, newPt: TPoint3D;
  ratio: Single;
  u, uStart, uEnd: Single;
  k: Integer;
begin
  result := TList<TPoint3D>.Create;
  HighResPoints := TList<TPoint3D>.Create;
  Distances := TList<Single>.Create;

  if (Points.Count < degree + 1) or (knots.Count < Points.Count + degree + 1)
  then
    exit;

  uStart := knots[degree];
  uEnd := knots[knots.Count - degree - 1];
  u := uStart;

  while u <= uEnd do
  begin
    k := FindKnotSpan(u);
    HighResPoints.Add(DeBoor(k, u));
    u := u + HighResStep;
  end;

  if (u < uEnd + HighResStep) then
  begin
    k := FindKnotSpan(uEnd);
    HighResPoints.Add(DeBoor(k, uEnd));
  end;

  Distances.Add(0);
  totalLength := 0;

  for i := 1 to HighResPoints.Count - 1 do
  begin
    segment := Hypot(HighResPoints[i].D1 - HighResPoints[i - 1].D1,
      HighResPoints[i].D2 - HighResPoints[i - 1].D2);
    totalLength := totalLength + segment;
    Distances.Add(totalLength);
  end;

  result.Add(HighResPoints[0]);
  targetLength := interval / 2;
  i := 1;

  while targetLength < totalLength do
  begin
    while (i < Distances.Count) and (Distances[i] < targetLength) do
      Inc(i);

    if i >= Distances.Count then
      Break;

    pt1 := HighResPoints[i - 1];
    pt2 := HighResPoints[i];

    ratio := (targetLength - Distances[i - 1]) /
      (Distances[i] - Distances[i - 1]);

    newPt := TPoint3D.Create(0, 0, 0);

    newPt.D1 := pt1.D1 + (pt2.D1 - pt1.D1) * ratio;
    newPt.D2 := pt1.D2 + (pt2.D2 - pt1.D2) * ratio;
    newPt.D3 := pt1.D3;

    result.Add(newPt);

    targetLength := targetLength + interval;
  end;

  result.Add(HighResPoints.Last);

  HighResPoints.Free;
  Distances.Free;
end;

function TCadSpline.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
begin
  if (not Assigned(Points)) or (Points.Count = 0) then
    raise Exception.Create('Set spline points before convert to points')
  else if (not Assigned(knots)) or (knots.Count = 0) then
    raise Exception.Create('Set spline knots before convert to points')
  else if not Assigned(splitter) then
    raise Exception.Create('Set splitter before convert to points')
  else if splitter.Value <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  if splitter.SplitType = pstInterval then
    result := SplitByInterval(splitter.Value)
  else if splitter.SplitType = pstDivideByUnit then
    result := SplitByCountAtUnit(Trunc(splitter.Value))
  else
    raise Exception.Create
      ('Invalid split type for convert to points for spline');
end;

end.
