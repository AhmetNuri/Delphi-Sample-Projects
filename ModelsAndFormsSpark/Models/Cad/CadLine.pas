unit CadLine;

interface

uses Point2D, Point3D, Shape, ShapeType, PointSplitter, Generics.Collections;

type
  TCadLine = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FP1: TPoint2D;
    FP2: TPoint2D;
    FLayer: string;
    FColor: string;
    FDiscreteId: integer;

    function NewID: Single;
    function SplitByInterval(interval: Single): TList<TPoint3D>;
    function SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
    function SplitForCenterDot(dotLength: Single): TList<TPoint3D>;
  public
    property P1: TPoint2D read FP1 write FP1;
    property P2: TPoint2D read FP2 write FP2;
    property Layer: string read FLayer write FLayer;
    property Color: string read FColor write FColor;

    function P1X: Single;
    function P1Y: Single;
    function P2X: Single;
    function P2Y: Single;

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

    constructor Create(P1, P2: TPoint2D; Layer, Color: string); overload;
    constructor Create(P1X, P1Y, P2X, P2Y: Single;
      Layer, Color: string); overload;
    destructor Destroy; override;
  end;

implementation

uses SysUtils, Math;

{ TCadLine }

constructor TCadLine.Create(P1, P2: TPoint2D; Layer, Color: string);
begin
  FP1 := P1;
  FP2 := P2;
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

constructor TCadLine.Create(P1X, P1Y, P2X, P2Y: Single; Layer, Color: string);
begin
  FP1 := TPoint2D.Create(P1X, P1Y);
  FP2 := TPoint2D.Create(P2X, P2Y);
  self.Layer := Layer;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

destructor TCadLine.Destroy;
begin
  if Assigned(FP1) then
    FP1.Free;
  FP1 := nil;

  if Assigned(FP2) then
    FP2.Free;
  FP2 := nil;

  inherited;
end;

function TCadLine.GetColor: string;
begin
  result := Color;
end;

function TCadLine.GetDetail: string;
begin
  result := Format
    ('Id: %.f, Layer: %s, Color: %s, X1: %.4f, Y1: %.4f, X2: %.4f, Y2: %.4f',
    [FID, Layer, Color, P1X, P1Y, P2X, P2Y]);
end;

function TCadLine.GetLayer: string;
begin
  result := Layer;
end;

function TCadLine.GetStart: TPoint2D;
begin
  result := TPoint2D.Create(P1X, P1Y);
end;

function TCadLine.GetEnd: TPoint2D;
begin
  result := TPoint2D.Create(P2X, P2Y);
end;

function TCadLine.GetType: TShapeType;
begin
  result := stLine;
end;

function TCadLine.NewID: Single;
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

function TCadLine.P1X: Single;
begin
  if not Assigned(FP1) then
    raise Exception.Create('Set position before reach to P1X');

  result := FP1.D1;
end;

function TCadLine.P1Y: Single;
begin
  if not Assigned(FP1) then
    raise Exception.Create('Set position before reach to P1Y');

  result := FP1.D2;
end;

function TCadLine.P2X: Single;
begin
  if not Assigned(FP2) then
    raise Exception.Create('Set position before reach to P2X');

  result := FP2.D1;
end;

function TCadLine.P2Y: Single;
begin
  if not Assigned(FP2) then
    raise Exception.Create('Set position before reach to P2Y');

  result := FP2.D2;
end;

function TCadLine.GetId: Single;
begin
  result := FID;
end;

procedure TCadLine.SetId(Id: Single);
begin
  FID := Id;
end;

function TCadLine.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

procedure TCadLine.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadLine.SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
begin
  if countAtUnit <= 0 then
    raise Exception.Create('Point count at unit must be greator than zero!');

  result := SplitByInterval(1 / countAtUnit);
end;

function TCadLine.SplitByInterval(interval: Single): TList<TPoint3D>;
var
  radian, dX, dY, cX, cY, xStart, xEnd, yStart, yEnd, Length, latestX,
    latestY: Single;
  pointsBackward: TList<TPoint3D>;
  areaCount, pointCount, index: integer;
begin
  if P1X = P2X then
    radian := 90 * PI / 180
  else if P1Y = P2Y then
    radian := 0
  else
    radian := Math.ArcTan2(Abs(P2Y - P1Y), Abs(P2X - P1X));

  Length := Sqrt(Power(P2Y - P1Y, 2) + Power(P2X - P1X, 2));

  areaCount := 1;

  if (Length > 0) and (interval > 0) then
  begin
    areaCount := Trunc(Length / interval);

    if areaCount > 0 then
      interval := Length / areaCount;
  end;

  pointCount := areaCount + 1;

  if P1X = P2X then
    dX := 0
  else
    dX := interval * Cos(radian) * Math.Sign(P2X - P1X);

  if P1Y = P2Y then
    dY := 0
  else
    dY := interval * Sin(radian) * Math.Sign(P2Y - P1Y);

  result := TList<TPoint3D>.Create;

  if (dX = 0) and (dY = 0) then
  begin
    result.Add(TPoint3D.Create(P1X, P1Y, NewID));
    exit;
  end;

  pointsBackward := TList<TPoint3D>.Create;

  index := 0;
  latestX := Single.MaxValue;
  latestY := Single.MaxValue;

  while index < pointCount / 2 do
  begin
    xStart := P1X + (dX * index);
    yStart := P1Y + (dY * index);
    xEnd := P2X - (dX * index);
    yEnd := P2Y - (dY * index);

    if (latestX <> xStart) or (latestY <> yStart) then
    begin
      result.Add(TPoint3D.Create(xStart, yStart, NewID));
      latestX := RoundTo(xStart, -4);
      latestY := RoundTo(yStart, -4);
    end;

    if (RoundTo(latestX, -4) = RoundTo(xEnd, -4)) and
      (RoundTo(latestY, -4) = RoundTo(yEnd, -4)) then
      break;

    if (latestX <> xEnd) or (latestY <> yEnd) then
    begin
      pointsBackward.Insert(0, TPoint3D.Create(xEnd, yEnd, NewID));
      latestX := RoundTo(xEnd, -4);
      latestY := RoundTo(yEnd, -4);
    end;

    inc(index);
  end;

  result.AddRange(pointsBackward);
  FreeAndNil(pointsBackward);
end;

function TCadLine.SplitForCenterDot(dotLength: Single): TList<TPoint3D>;
var
  radian, dX, dY, cX, cY: Single;
begin
  if P1X = P2X then
    radian := 90 * PI / 180
  else if P1Y = P2Y then
    radian := 0
  else
    radian := Math.ArcTan2(Abs(P2Y - P1Y), Abs(P2X - P1X));

  cX := (P1X + P2X) / 2;
  cY := (P1Y + P2Y) / 2;

  if P1X = P2X then
    dX := 0
  else
    dX := dotLength * Cos(radian) * Math.Sign(P2X - P1X) / 2;

  if P1Y = P2Y then
    dY := 0
  else
    dY := dotLength * Sin(radian) * Math.Sign(P2Y - P1Y) / 2;

  result := TList<TPoint3D>.Create;
  result.Add(TPoint3D.Create(cX - dX, cY - dY, FID));
  result.Add(TPoint3D.Create(cX + dX, cY + dY, FID));
end;

function TCadLine.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
begin
  if (not Assigned(FP1)) or (not Assigned(FP2)) then
    raise Exception.Create('Set positions before convert to points')
  else if not Assigned(splitter) then
    raise Exception.Create('Set splitter before convert to points')
  else if splitter.Value <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  if splitter.SplitType = pstInterval then
    result := SplitByInterval(splitter.Value)
  else if splitter.SplitType = pstDivideByUnit then
    result := SplitByCountAtUnit(Trunc(splitter.Value))
  else if splitter.SplitType = pstCenterDot then
    result := SplitForCenterDot(splitter.Value)
  else
    raise Exception.Create('Invalid split type for convert to points');
end;

end.
