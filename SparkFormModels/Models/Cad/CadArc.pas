unit CadArc;

interface

uses Point2D, Point3D, Shape, ShapeType, PointSplitter, Generics.Collections;

type
  TCadArc = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FDiscreteId: integer;

    function NewID: Single;
  public
    Layer: string;
    Color: string;
    Center: TPoint2D;
    Radius: Single;
    StartAngle: Single;
    EndAngle: Single;

    function CX: Single;
    function CY: Single;

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

    function SplitByInterval(interval: Single): TList<TPoint3D>;
    function SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;

    function ToPoints(splitter: TPointSplitter): TList<TPoint3D>;

    constructor Create(const Center: TPoint2D;
      const Radius, StartAngle, EndAngle: Single;
      const Layer, Color: string); overload;
    constructor Create(const CenterX, CenterY, Radius, StartAngle,
      EndAngle: Single; const Layer, Color: string); overload;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Math;

{ TCadArc }

function TCadArc.NewID: Single;
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

constructor TCadArc.Create(const Center: TPoint2D;
  const Radius, StartAngle, EndAngle: Single; const Layer, Color: string);
begin
  self.Center := Center;
  self.Radius := Radius;
  self.StartAngle := StartAngle;
  self.EndAngle := EndAngle;
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

constructor TCadArc.Create(const CenterX, CenterY, Radius, StartAngle,
  EndAngle: Single; const Layer, Color: string);
begin
  self.Center := TPoint2D.Create(CenterX, CenterY);
  self.Radius := Radius;
  self.StartAngle := StartAngle;
  self.EndAngle := EndAngle;
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

function TCadArc.CX: Single;
begin
  if not Assigned(Center) then
    raise Exception.Create('Set center location before reach to CX');

  result := Center.D1;
end;

function TCadArc.CY: Single;
begin
  if not Assigned(Center) then
    raise Exception.Create('Set center location before reach to CY');

  result := Center.D2;
end;

destructor TCadArc.Destroy;
begin
  if Assigned(Center) then
    Center.Free;
  Center := nil;

  inherited;
end;

function TCadArc.GetColor: string;
begin
  result := Color;
end;

function TCadArc.GetDetail: string;
begin
  result := Format
    ('Layer: %s, Color: %s, CX: %.4f, CY: %.4f, Radius: %.4f, Angle: (%.4f - %.4f)',
    [Layer, Color, CX, CY, Radius, StartAngle, EndAngle]);
end;

function TCadArc.GetLayer: string;
begin
  result := Layer;
end;

function TCadArc.GetStart: TPoint2D;
var
  points: TList<TPoint3D>;
begin
  points := SplitByInterval(0.5);

  if points.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  result := TPoint2D.Create(points[0].D1, points[0].D2);

  FreeAndNil(points);
end;

function TCadArc.GetEnd: TPoint2D;
var
  points: TList<TPoint3D>;
begin
  points := SplitByInterval(0.5);

  if points.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  result := TPoint2D.Create(points[points.Count - 1].D1,
    points[points.Count - 1].D2);

  FreeAndNil(points);
end;

function TCadArc.GetType: TShapeType;
begin
  result := stArc;
end;

function TCadArc.GetId: Single;
begin
  result := FID;
end;

procedure TCadArc.SetId(Id: Single);
begin
  FID := Id;
end;

function TCadArc.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

procedure TCadArc.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadArc.SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
begin
  if countAtUnit <= 0 then
    raise Exception.Create('Point count at unit must be greator than zero!');

  result := SplitByInterval(1 / countAtUnit);
end;

function TCadArc.SplitByInterval(interval: Single): TList<TPoint3D>;
var
  angleDifference, stepAngle, perimeter, Length, angle, radian, x, y: Single;
  isReversed: Boolean;
  pointsBackwards: TList<TPoint3D>;
  i, pointCount: integer;
begin
  angleDifference := 0;
  stepAngle := 0;

  if StartAngle > EndAngle then
    isReversed := (StartAngle - EndAngle) < 180
  else
    isReversed := (EndAngle - StartAngle) > 180;

  if (StartAngle < EndAngle) and (not isReversed) then
    angleDifference := EndAngle - StartAngle

  else if (StartAngle > EndAngle) and (isReversed) then
    angleDifference := StartAngle - EndAngle

  else if (StartAngle < EndAngle) and (isReversed) then
    angleDifference := 360 - EndAngle + StartAngle

  else if (StartAngle > EndAngle) and (not isReversed) then
    angleDifference := 360 - StartAngle + EndAngle;

  perimeter := 2 * PI * Radius;
  Length := perimeter * angleDifference / 360;

  if Length <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  stepAngle := angleDifference / (Length / interval);

  if stepAngle > 0 then
  begin
    pointCount := Trunc(angleDifference / stepAngle);

    if pointCount > 0 then
      stepAngle := angleDifference / pointCount;
  end;

  if isReversed then
    stepAngle := -stepAngle;

  result := TList<TPoint3D>.Create;
  pointsBackwards := TList<TPoint3D>.Create;

  i := 0;
  while True do
  begin
    angle := FMod((StartAngle + stepAngle * i), 360);
    radian := angle * PI / 180;
    x := CX + Radius * Cos(radian);
    y := CY + Radius * Sin(radian);

    if Abs(stepAngle * i) > Abs(angleDifference / 2) + 0.0001 then
      break;

    result.Add(TPoint3D.Create(x, y, NewID));

    if Abs(stepAngle * i) = Abs(angleDifference / 2) then
      break;

    angle := FMod((EndAngle - stepAngle * i), 360);
    radian := angle * PI / 180;
    x := CX + Radius * Cos(radian);
    y := CY + Radius * Sin(radian);

    if (result.Count = 0) or
      ((Abs(result[result.Count - 1].D1 - x) + Abs(result[result.Count - 1].D2 -
      y)) > 0.0001) then
      pointsBackwards.Insert(0, TPoint3D.Create(x, y, NewID));

    inc(i);
  end;

  result.AddRange(pointsBackwards);
  FreeAndNil(pointsBackwards);
end;

function TCadArc.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
begin
  if (not Assigned(Center)) then
    raise Exception.Create('Set position of center before convert to points')
  else if not Assigned(splitter) then
    raise Exception.Create('Set splitter before convert to points')
  else if splitter.Value <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  if splitter.SplitType = pstInterval then
    result := SplitByInterval(splitter.Value)
  else if splitter.SplitType = pstDivideByUnit then
    result := SplitByCountAtUnit(Trunc(splitter.Value))
  else if splitter.SplitType = pstCenterDot then
    result := SplitByInterval(splitter.Value)
  else
    raise Exception.Create('Invalid split type for convert to points');
end;

end.
