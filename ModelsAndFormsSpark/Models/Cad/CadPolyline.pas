unit CadPolyline;

interface

uses Point2D, Point3D, CadVertex, Generics.Collections, Shape, ShapeType,
  PointSplitter;

type
  TCadPolyline = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FDiscreteId: integer;

    function NewID: Single;
    function TryParseAsPoint(treshold: Single): TList<TPoint3D>;
    function SplitByInterval(interval: Single): TList<TPoint3D>;
    function SplitByIntervalWithBulge(fromPoint, toPoint: TCadVertex;
      interval: Single; ignoreFirst, ignoreLast: Boolean): TList<TPoint3D>;
    function SplitByIntervalWithoutBulge(fromPoint, toPoint: TCadVertex;
      interval: Single; ignoreFirst, ignoreLast: Boolean): TList<TPoint3D>;
    function SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
    function SplitForCenterDot(dotLength: Single): TList<TPoint3D>;
    function SplitForCenterDotWithoutBulge(fromPoint, toPoint: TCadVertex;
      dotLength: Single): TList<TPoint3D>;
  public
    Layer: string;
    Color: string;
    Vertexes: TList<TCadVertex>;
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

    constructor Create(Vertexes: TList<TCadVertex>; const Layer, Color: string;
      Closed: Boolean);
    destructor Destroy; override;
  end;

implementation

uses Classes, SysUtils, Math;

{ TCadPolyline }

constructor TCadPolyline.Create(Vertexes: TList<TCadVertex>;
  const Layer, Color: string; Closed: Boolean);
begin
  self.Vertexes := Vertexes;
  self.Layer := Layer;
  self.Closed := Closed;
  self.Color := Color;
  self.FDiscreteId := 1;
end;

destructor TCadPolyline.Destroy;
var
  I: Integer;
begin
  if Assigned(Vertexes) then
  begin
    for I := 0 to Vertexes.Count - 1 do
    begin
      if Assigned(Vertexes[I]) then
      begin
        if Assigned(Vertexes[I].Position) then
        begin
          Vertexes[I].Position.Free;
          Vertexes[I].Position := nil;
        end;
        Vertexes[I].Free;
        Vertexes[I] := nil;
      end;
    end;
    Vertexes.Free;
    Vertexes := nil;
  end;

  inherited;
end;

function TCadPolyline.GetColor: string;
begin
  result := Color;
end;

function TCadPolyline.GetDetail: string;
var
  I: integer;
  items: TStringList;
begin
  items := TStringList.Create;

  for I := 0 to Vertexes.Count - 1 do
  begin
    items.Add(Format
      ('Id: %.f, Layer: %s, Color: %s, X: %.4f, Y: %.4f, Bulge: %.4f, Closed: %d',
      [FID, Layer, Color, Vertexes[I].X, Vertexes[I].Y, Vertexes[I].Bulge,
      Ord(Closed)]));
  end;

  result := TrimRight(items.Text);
end;

function TCadPolyline.GetLayer: string;
begin
  result := Layer;
end;

function TCadPolyline.GetStart: TPoint2D;
begin
  if Vertexes.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  result := TPoint2D.Create(Vertexes[0].X, Vertexes[0].Y);
end;

function TCadPolyline.GetEnd: TPoint2D;
begin
  if Vertexes.Count = 0 then
    raise Exception.Create('Points couldn''t be created!');

  if not Closed then
    result := TPoint2D.Create(Vertexes[Vertexes.Count - 1].X,
      Vertexes[Vertexes.Count - 1].Y)
  else
    result := TPoint2D.Create(Vertexes[0].X, Vertexes[0].Y);
end;

function TCadPolyline.GetType: TShapeType;
begin
  result := stPolyline;
end;

function TCadPolyline.NewID: Single;
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

function TCadPolyline.GetId: Single;
begin
  result := FID;
end;

procedure TCadPolyline.SetId(Id: Single);
begin
  FID := Id;
end;

function TCadPolyline.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

procedure TCadPolyline.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadPolyline.SplitByCountAtUnit(countAtUnit: integer): TList<TPoint3D>;
begin
  if countAtUnit <= 0 then
    raise Exception.Create('Point count at unit must be greator than zero!');

  result := SplitByInterval(1 / countAtUnit);
end;

function TCadPolyline.SplitByInterval(interval: Single): TList<TPoint3D>;
var
  i: integer;
begin
  result := TList<TPoint3D>.Create;

  if (not Assigned(Vertexes)) or (Vertexes.Count = 0) then
    exit;

  for I := 0 to Vertexes.Count - 2 do
  begin
    if Vertexes[I].Bulge <> 0 then
      result.AddRange(SplitByIntervalWithBulge(Vertexes[I], Vertexes[I + 1],
        interval, I > 0, false))
    else
      result.AddRange(SplitByIntervalWithoutBulge(Vertexes[I], Vertexes[I + 1],
        interval, I > 0, false));
  end;

  if (Vertexes.Count > 1) and Closed then
    result.AddRange(SplitByIntervalWithoutBulge(Vertexes[Vertexes.Count - 1],
      Vertexes[0], interval, true, true));
end;

function TCadPolyline.SplitByIntervalWithBulge(fromPoint, toPoint: TCadVertex;
  interval: Single; ignoreFirst, ignoreLast: Boolean): TList<TPoint3D>;
var
  x1, x2, y1, y2, Bulge, theta, angleDifference, mX, mY, chordLength, radius,
    perimeter, length, stepAngle, stepRadian, dX, dY, cX, cY, centerDistance,
    startAngle, endAngle, radian, pX, pY, angle, X, Y, latestX, latestY: Single;
  directionX, directionY, i, pointCount: integer;
  pointsBackward: TList<TPoint3D>;
  isReversed: Boolean;
begin
  if interval <= 0 then
    raise Exception.Create
      ('Interval of polyline splitting must be greator than zero!');

  result := TList<TPoint3D>.Create;

  x1 := fromPoint.X;
  y1 := fromPoint.Y;
  x2 := toPoint.X;
  y2 := toPoint.Y;
  Bulge := fromPoint.Bulge;
  isReversed := Bulge < 0;

  theta := 4 * ArcTan(Abs(Bulge));
  angleDifference := theta * 180 / PI;

  directionX := 1;
  directionY := 1;

  if (x1 < x2) and (y1 < y2) then
  begin
    // sol attan sað üste
    directionX := IfThen(Bulge > 0, -1, 1);
    directionY := IfThen(Bulge > 0, 1, -1);
  end
  else if (x1 < x2) and (y1 > y2) then
  begin
    // sol üstten sað alta
    directionX := IfThen(Bulge > 0, 1, -1);
    directionY := IfThen(Bulge > 0, 1, -1);
  end
  else if (x1 > x2) and (y1 < y2) then
  begin
    // sað alttan sol üste
    directionX := IfThen(Bulge > 0, -1, 1);
    directionY := IfThen(Bulge > 0, -1, 1);
  end
  else if (x1 > x2) and (y1 > y2) then
  begin
    // sað üstten sol alta
    directionX := IfThen(Bulge > 0, 1, -1);
    directionY := IfThen(Bulge > 0, -1, 1);
  end
  else if (x1 = x2) and (y1 < y2) then
  begin
    // dikey
    directionX := IfThen(Bulge > 0, -1, 1);
    directionY := -1;
  end
  else if (x1 = x2) and (y1 > y2) then
  begin
    // dikey
    directionX := IfThen(Bulge > 0, 1, -1);
    directionY := 1;
  end
  else if (x1 < x2) and (y1 = y2) then
  begin
    // yatay
    directionX := -1;
    directionY := IfThen(Bulge > 0, 1, -1);
  end
  else if (x1 > x2) and (y1 = y2) then
  begin
    // yatay
    directionX := 1;
    directionY := IfThen(Bulge > 0, -1, 1);
  end;

  // Orta nokta
  mX := (x1 + x2) / 2;
  mY := (y1 + y2) / 2;

  // Kiriþ uzunluðu
  chordLength := Sqrt(Math.Power(x2 - x1, 2) + Math.Power(y2 - y1, 2));

  radius := (chordLength / 2) / Sin(theta / 2);
  perimeter := 2 * PI * radius;
  length := perimeter * angleDifference / 360;

  stepAngle := angleDifference / (length / interval);

  if angleDifference < stepAngle then
    stepAngle := angleDifference
  else if stepAngle > 0 then
  begin
    pointCount := Trunc(angleDifference / stepAngle);

    if pointCount > 0 then
      stepAngle := angleDifference / pointCount;
  end;

  stepRadian := stepAngle * PI / 180;

  if (isReversed) then
  begin
    stepAngle := -stepAngle;
    stepRadian := -stepRadian;
  end;

  // Kiriþe dik vektör
  dX := y2 - y1;
  dY := x1 - x2;

  // Vektörü normalize et
  dX := dX / chordLength;
  dY := dY / chordLength;

  // Yay merkezi
  centerDistance := Sqrt(Math.Power(radius, 2) -
    Math.Power(chordLength / 2, 2));
  cX := mX + directionX * Abs(centerDistance * dX);
  cY := mY + directionY * Abs(centerDistance * dY);

  // Baþlangýç ve bitiþ açýlarý
  startAngle := Math.ArcTan2(y1 - cY, x1 - cX);
  endAngle := Math.ArcTan2(y2 - cY, x2 - cX);

  pointsBackward := TList<TPoint3D>.Create;

  i := 0;
  latestX := Single.MaxValue;
  latestY := Single.MaxValue;

  while true do
  begin
    radian := startAngle + stepRadian * i;
    pX := cX + radius * Cos(radian);
    pY := cY + radius * Sin(radian);

    if Abs(stepAngle * i) > Abs(angleDifference / 2) + 0.0001 then
      break;

    if (not ignoreFirst) or (i > 0) then
    begin
      result.Add(TPoint3D.Create(pX, pY, NewID));
      latestX := RoundTo(pX, -4);
      latestY := RoundTo(pY, -4);
    end;

    if Abs(stepAngle * i) = Abs(angleDifference / 2) then
      break;

    radian := endAngle - stepRadian * i;
    pX := cX + radius * Cos(radian);
    pY := cY + radius * Sin(radian);

    if (RoundTo(latestX, -4) = RoundTo(pX, -4)) and
      (RoundTo(latestY, -4) = RoundTo(pY, -4)) then
      break;

    if (not ignoreLast) or (i > 0) then
    begin
      if (result.Count = 0) or
        ((Abs(result[result.Count - 1].D1 - pX) + Abs(result[result.Count - 1]
        .D2 - pY)) > 0.0001) then
      begin
        pointsBackward.Insert(0, TPoint3D.Create(pX, pY, NewID));
        latestX := RoundTo(pX, -4);
        latestY := RoundTo(pY, -4);
      end;
    end;

    i := i + 1;
  end;

  result.AddRange(pointsBackward);
  FreeAndNil(pointsBackward);
end;

function TCadPolyline.SplitByIntervalWithoutBulge(fromPoint,
  toPoint: TCadVertex; interval: Single; ignoreFirst, ignoreLast: Boolean)
  : TList<TPoint3D>;
var
  radian, dX, dY, cX, cY, xStart, xEnd, yStart, yEnd, length, latestX,
    latestY: Single;
  pointsBackward: TList<TPoint3D>;
  index, pointCount, areaCount: integer;
begin
  if fromPoint.X = toPoint.X then
    radian := 90 * PI / 180
  else if fromPoint.Y = toPoint.Y then
    radian := 0
  else
    radian := Math.ArcTan2(Abs(toPoint.Y - fromPoint.Y),
      Abs(toPoint.X - fromPoint.X));

  length := Sqrt(Power(toPoint.Y - fromPoint.Y, 2) +
    Power(toPoint.X - fromPoint.X, 2));

  areaCount := 1;

  if (length > 0) and (interval > 0) then
  begin
    areaCount := Trunc(length / interval);

    if areaCount > 0 then
      interval := length / areaCount;
  end;

  pointCount := areaCount + 1;

  if fromPoint.X = toPoint.X then
    dX := 0
  else
    dX := interval * Cos(radian) * Math.Sign(toPoint.X - fromPoint.X);

  if fromPoint.Y = toPoint.Y then
    dY := 0
  else
    dY := interval * Sin(radian) * Math.Sign(toPoint.Y - fromPoint.Y);

  result := TList<TPoint3D>.Create;

  if (dX = 0) and (dY = 0) then
  begin
    result.Add(TPoint3D.Create(fromPoint.X, fromPoint.Y, NewID));
    exit;
  end;

  pointsBackward := TList<TPoint3D>.Create;

  index := 0;
  latestX := Single.MaxValue;
  latestY := Single.MaxValue;

  while index < pointCount / 2 do
  begin
    xStart := fromPoint.X + (dX * index);
    yStart := fromPoint.Y + (dY * index);
    xEnd := toPoint.X - (dX * index);
    yEnd := toPoint.Y - (dY * index);

    if (not ignoreFirst) or (index > 0) then
    begin
      result.Add(TPoint3D.Create(xStart, yStart, NewID));
      latestX := RoundTo(xStart, -4);
      latestY := RoundTo(yStart, -4);
    end;

    if (RoundTo(latestX, -4) = RoundTo(xEnd, -4)) and
      (RoundTo(latestY, -4) = RoundTo(yEnd, -4)) then
      break;

    if (not ignoreLast) or (index > 0) then
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

function TCadPolyline.SplitForCenterDot(dotLength: Single): TList<TPoint3D>;
var
  radian, dX, dY, cX, cY, index, xStart, xEnd, yStart, yEnd: Single;
  pointsBackward: TList<TPoint3D>;
  fromPoint, toPoint: TCadVertex;
  I: integer;
begin
  result := TList<TPoint3D>.Create;

  if (not Assigned(Vertexes)) or (Vertexes.Count = 0) then
    exit;

  for I := 0 to Vertexes.Count - 2 do
  begin
    if Vertexes[I].Bulge = 0 then
      result.AddRange(SplitForCenterDotWithoutBulge(Vertexes[I],
        Vertexes[I + 1], dotLength))
    else
      result.AddRange(SplitByIntervalWithBulge(Vertexes[I], Vertexes[I + 1],
        dotLength, I > 0, false))
  end;

  if (Vertexes.Count > 1) and Closed then
    result.AddRange(SplitForCenterDotWithoutBulge(Vertexes[Vertexes.Count - 1],
      Vertexes[0], dotLength));
end;

function TCadPolyline.SplitForCenterDotWithoutBulge(fromPoint,
  toPoint: TCadVertex; dotLength: Single): TList<TPoint3D>;
var
  radian, dX, dY, cX, cY, index, xStart, xEnd, yStart, yEnd: Single;
  pointsBackward: TList<TPoint3D>;
begin
  if fromPoint.X = toPoint.X then
    radian := 90 * PI / 180
  else if fromPoint.Y = toPoint.Y then
    radian := 0
  else
    radian := Math.ArcTan2(Abs(toPoint.Y - fromPoint.Y),
      Abs(toPoint.X - fromPoint.X));

  cX := (fromPoint.X + toPoint.X) / 2;
  cY := (fromPoint.Y + toPoint.Y) / 2;

  if fromPoint.X = toPoint.X then
    dX := 0
  else
    dX := dotLength * Cos(radian) * Math.Sign(toPoint.X - fromPoint.X) / 2;

  if fromPoint.Y = toPoint.Y then
    dY := 0
  else
    dY := dotLength * Sin(radian) * Math.Sign(toPoint.Y - fromPoint.Y) / 2;

  result := TList<TPoint3D>.Create;
  result.Add(TPoint3D.Create(cX - dX, cY - dY, FID));
  result.Add(TPoint3D.Create(cX + dX, cY + dY, FID));
end;

function TCadPolyline.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
begin
  if (not Assigned(Vertexes)) or (Vertexes.Count = 0) then
    raise Exception.Create('Set vertexes before convert to points')
  else if not Assigned(splitter) then
    raise Exception.Create('Set splitter before convert to points')
  else if splitter.Value <= 0 then
    raise Exception.Create('Wrong splitter value for convert to points');

  result := TryParseAsPoint(splitter.TresholdOfPoint);

  if Assigned(result) then
    exit;

  if splitter.SplitType = pstInterval then
    result := SplitByInterval(splitter.Value)
  else if splitter.SplitType = pstDivideByUnit then
    result := SplitByCountAtUnit(Trunc(splitter.Value))
  else if splitter.SplitType = pstCenterDot then
    result := SplitForCenterDot(splitter.Value)
  else
    raise Exception.Create('Invalid split type for convert to points');
end;

function TCadPolyline.TryParseAsPoint(treshold: Single): TList<TPoint3D>;
var
  vertexCount, i: integer;
  length, lastLength, cX, cY: Single;
begin
  result := nil;

  if treshold <= 0 then
    exit;

  vertexCount := Vertexes.Count;

  if (not Closed) or (vertexCount < 3) then
    exit;

  for I := 0 to vertexCount - 2 do
  begin
    if (Vertexes[I].Bulge <> 0) or (Vertexes[I + 1].Bulge <> 0) then
      exit;

    length := Sqrt(Power(Vertexes[I + 1].X - Vertexes[I].X, 2) +
      Power(Vertexes[I + 1].Y - Vertexes[I].Y, 2));

    if length > treshold then
      exit
    else if I = 0 then
      lastLength := length
    else if Abs(length - lastLength) > 0.0001 then
      exit
    else
      lastLength := length;
  end;

  cX := 0;
  cY := 0;

  for I := 0 to vertexCount - 1 do
  begin
    cX := cX + Vertexes[I].X;
    cY := cY + Vertexes[I].Y;
  end;

  cX := cX / vertexCount;
  cY := cY / vertexCount;

  result := TList<TPoint3D>.Create;
  result.Add(TPoint3D.Create(cX, cY, FID));
end;

end.
