unit CadCircle;

interface

uses Point2D, Point3D, Shape, ShapeType, PointSplitter, Generics.Collections;

type
  TCadCircle = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FDiscreteId: integer;

    function NewID: Single;
  public
    Radius: Single;
    Center: TPoint2D;
    Layer: string;
    Color: string;

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

    constructor Create(const Center: TPoint2D; Radius: Single;
      const Layer, Color: string); overload;
    constructor Create(const CenterX, CenterY, Radius: Single;
      const Layer, Color: string); overload;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Math;

{ TCadCircle }

constructor TCadCircle.Create(const CenterX, CenterY, Radius: Single;
  const Layer, Color: string);
begin
  self.Center := TPoint2D.Create(CenterX, CenterY);
  self.Radius := Radius;
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

constructor TCadCircle.Create(const Center: TPoint2D; Radius: Single;
  const Layer, Color: string);
begin
  self.Center := Center;
  self.Radius := Radius;
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

function TCadCircle.CX: Single;
begin
  if not Assigned(Center) then
    raise Exception.Create('Set center location before reach to CX');

  result := Center.D1;
end;

function TCadCircle.CY: Single;
begin
  if not Assigned(Center) then
    raise Exception.Create('Set center location before reach to CY');

  result := Center.D2;
end;

destructor TCadCircle.Destroy;
begin
  if Assigned(Center) then
    Center.Free;
  Center := nil;

  inherited;
end;

function TCadCircle.GetColor: string;
begin
  result := Color;
end;

function TCadCircle.GetDetail: string;
begin
  result := Format
    ('Id: %.f, Layer: %s, Color: %s, CX: %.4f, CY: %.4f, Radius: %.4f',
    [FID, Layer, Color, CX, CY, Radius]);
end;

function TCadCircle.GetLayer: string;
begin
  result := Layer;
end;

function TCadCircle.GetStart: TPoint2D;
var
  points: TList<TPoint3D>;
begin
  points := SplitByInterval(0.5);

  if points.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  result := TPoint2D.Create(points[0].D1, points[0].D2);

  FreeAndNil(points);
end;

function TCadCircle.GetEnd: TPoint2D;
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

function TCadCircle.GetType: TShapeType;
begin
  result := stCircle;
end;

function TCadCircle.NewID: Single;
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

function TCadCircle.GetId: Single;
begin
  result := FID;
end;

procedure TCadCircle.SetId(Id: Single);
begin
  FID := Id;
end;

function TCadCircle.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

procedure TCadCircle.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadCircle.SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
begin
  if countAtUnit <= 0 then
    raise Exception.Create('Point count at unit must be greator than zero!');

  result := SplitByInterval(1 / countAtUnit);
end;

function TCadCircle.SplitByInterval(interval: Single): TList<TPoint3D>;
var
  angleDifference, stepAngle, Length, angle, radian, x, y: Single;
  pointsBackwards: TList<TPoint3D>;
  i, pointCount: integer;
begin
  angleDifference := 0;
  stepAngle := 0;
  angleDifference := 360;
  Length := 2 * PI * Radius;

  if Length <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  stepAngle := angleDifference / (Length / interval);

  if stepAngle > 0 then
  begin
    pointCount := Trunc(angleDifference / stepAngle);

    if pointCount > 0 then
      stepAngle := angleDifference / pointCount;
  end;

  result := TList<TPoint3D>.Create;
  pointsBackwards := TList<TPoint3D>.Create;

  i := 0;
  while True do
  begin
    angle := stepAngle * i;

    radian := angle * PI / 180;
    x := CX + Radius * Cos(radian);
    y := CY + Radius * Sin(radian);

    if angle > 180 + 0.0001 then
      break;

    result.Add(TPoint3D.Create(x, y, NewID));

    if angle = 180 then
      break;

    angle := 360 - stepAngle * i;
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

function TCadCircle.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
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
