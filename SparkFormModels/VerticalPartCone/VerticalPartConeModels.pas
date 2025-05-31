unit VerticalPartConeModels;

interface

uses Generics.Collections, Point2D, Point3D, Point4D, Point5D, Point6D, Mirror,
  TouchOffsetType, MinMax2D, SysUtils, StretchMode, TouchOffset,
  LengthOfTemplate, Move, Crop, Stretch;

type
  TVerticalPartConeAxisIn = class
  private
    FIsRightSide: Boolean;
    FAngleOfArmRef: Single;
    FPoints: TList<TPoint3D>;

    procedure SetAngleOfArmRef(value: Single);
  public
    // x value of min z
    LowXOfProjection: Single;

    // x value of max z
    HighXOfProjection: Single;

    // depth of z for cone
    DepthOfCone: Single;

    WidthOfPart: Single;
    RadiusOfPart: Single;
    RadiusOfProjection: Single;
    XOfDeviation: Single;
    ZOfDeviation: Single;
    LengthOfTemplate: TLengthOfTemplate;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    OffsetOfTouchAngle: TTouchOffset;
    Mirror: TMirror;
    Move: TMove;
    Crop: TCrop;
    Stretch: TStretch;
    MinMax2D: TMinMax2D;
    IsContinuousAngle: Boolean;
    ValidateAngleOfTouch: Boolean;
    IsStrictDirection: Boolean; // angle of divisor
    IsReversedAngleOfTemplate: Boolean; // angle of divisor

    property IsRightSide: Boolean read FIsRightSide;
    property AngleOfArmRef: Single read FAngleOfArmRef write SetAngleOfArmRef;

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  TVerticalPartConeAxisOut = class
  private
    FGroupNo: string;
    FPoints: TList<TList<TPoint6D>>;
  public
    function Points: TList<TList<TPoint6D>>;
    function FlattenPoints: TList<TPoint6D>;
    procedure AddPoint(groupNo: string; point: TPoint6D);

    constructor Create;
    destructor Destroy; override;
  end;

  TVerticalPartConeShapeIn = class
  private
    FPoints: TList<TPoint3D>;
  public
    // x value of min z
    LowXOfProjection: Single;

    // x value of max z
    HighXOfProjection: Single;

    // depth of z for cone
    DepthOfCone: Single;

    RadiusOfPart: Single;
    LengthOfTemplate: TLengthOfTemplate;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    OffsetOfTouchAngle: TTouchOffset;
    Mirror: TMirror;
    Move: TMove;
    Crop: TCrop;
    Stretch: TStretch;
    MinMax2D: TMinMax2D;
    IsRightSide: Boolean;
    ValidateAngleOfTouch: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  TVerticalPartConeShapeOut = class
  private
    FGroupNo: string;
    FMinMax: TMinMax2D;
    FPoints: TList<TList<TPoint2D>>;
  public
    function MinMax: TMinMax2D;
    function Points: TList<TList<TPoint2D>>;
    function FlattenPoints: TList<TPoint2D>;
    procedure AddPoint(groupNo: string; point: TPoint2D);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TVerticalPartConeAxisIn }

procedure TVerticalPartConeAxisIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure TVerticalPartConeAxisIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor TVerticalPartConeAxisIn.Create;
begin
  inherited;

  LowXOfProjection := 0;
  HighXOfProjection := 0;
  DepthOfCone := 0;
  RadiusOfPart := 0;
  WidthOfPart := 0;
  RadiusOfProjection := 0;
  XOfDeviation := 0;
  ZOfDeviation := 0;
  AngleOfArmRef := 0;
  FIsRightSide := false;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
  IsContinuousAngle := true;
  IsStrictDirection := true;
  IsReversedAngleOfTemplate := false;
  ValidateAngleOfTouch := false;

  OffsetOfTouchAngle.SwitchTypeToNone;

  Mirror.OnX := false;
  Mirror.OnY := false;
  Mirror.OnMixMax := false;

  Move.OnX := 0;
  Move.OnY := 0;
  Move.OnStrictArea := false;

  Crop.OnX.SetArea(0, 100);
  Crop.OnY.SetArea(0, 100);

  Stretch.SwitchModeToNone;
  Stretch.OnX.SetArea(0, 100);
  Stretch.OnY.SetArea(0, 100);

  FPoints := TList<TPoint3D>.Create;
end;

destructor TVerticalPartConeAxisIn.Destroy;
var
  I: Integer;
begin
  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if Assigned(FPoints[I]) then
        FPoints[I].Free;
      FPoints[I] := nil;
    end;
    FPoints.Free;
    FPoints := nil;
  end;

  if Assigned(LengthOfTemplate) then
    LengthOfTemplate.Free;
  LengthOfTemplate := nil;

  if Assigned(MinMax2D) then
    MinMax2D.Free;
  MinMax2D := nil;

  inherited;
end;

function TVerticalPartConeAxisIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure TVerticalPartConeAxisIn.SetAngleOfArmRef(value: Single);
begin
  if (value <> 90) and (value <> -90) and (value <> 0) then
    raise Exception.Create('Angle of arm must be one of (90, -90, 0)!');

  FIsRightSide := false;
  FAngleOfArmRef := value;
  FIsRightSide := value < 0;
end;

procedure TVerticalPartConeAxisIn.Validate;
begin
  if LowXOfProjection = HighXOfProjection then
    raise Exception.Create
      ('Area is not valid with using startX and EndX to output!')
  else if DepthOfCone <= 0 then
    raise Exception.Create('Invalid depth of conical area!')
  else if RadiusOfProjection < 0 then
    raise Exception.Create('Radius of projection is invalid!');
end;

{ TVerticalPartConeAxisOut }

procedure TVerticalPartConeAxisOut.AddPoint(groupNo: string; point: TPoint6D);
begin
  if (FGroupNo <> groupNo) or (FGroupNo = '') then
  begin
    if (FPoints.Count = 0) or
      ((FPoints.Count > 0) and (FPoints[FPoints.Count - 1].Count > 0)) then
      FPoints.Add(TList<TPoint6D>.Create);
  end;

  FPoints[FPoints.Count - 1].Add(point);

  FGroupNo := groupNo;
end;

constructor TVerticalPartConeAxisOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint6D >>.Create;
end;

destructor TVerticalPartConeAxisOut.Destroy;
var
  I, J: Integer;
begin
  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if Assigned(FPoints[I]) then
      begin
        for J := 0 to FPoints[I].Count - 1 do
        begin
          if Assigned(FPoints[I][J]) then
            FPoints[I][J].Free;
          FPoints[I][J] := nil;
        end;
        FPoints[I].Free;
      end;
      FPoints[I] := nil;
    end;
    FPoints.Free;
    FPoints := nil;
  end;
  inherited;
end;

function TVerticalPartConeAxisOut.FlattenPoints: TList<TPoint6D>;
var
  I: Integer;
  J: Integer;
begin
  result := TList<TPoint6D>.Create;

  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if not Assigned(FPoints[I]) then
        continue;

      for J := 0 to FPoints[I].Count - 1 do
      begin
        result.Add(FPoints[I][J]);
      end;
    end;
  end;
end;

function TVerticalPartConeAxisOut.Points: TList<TList<TPoint6D>>;
begin
  result := FPoints;
end;

{ TVerticalPartConeShapeIn }

procedure TVerticalPartConeShapeIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure TVerticalPartConeShapeIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor TVerticalPartConeShapeIn.Create;
begin
  inherited;

  LowXOfProjection := 0;
  HighXOfProjection := 0;
  DepthOfCone := 0;
  RadiusOfPart := 0;
  IsRightSide := false;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
  ValidateAngleOfTouch := false;

  OffsetOfTouchAngle.SwitchTypeToNone;

  Mirror.OnX := false;
  Mirror.OnY := false;
  Mirror.OnMixMax := false;

  Move.OnX := 0;
  Move.OnY := 0;
  Move.OnStrictArea := false;

  Crop.OnX.SetArea(0, 100);
  Crop.OnY.SetArea(0, 100);

  Stretch.SwitchModeToNone;
  Stretch.OnX.SetArea(0, 100);
  Stretch.OnY.SetArea(0, 100);

  FPoints := TList<TPoint3D>.Create;
end;

destructor TVerticalPartConeShapeIn.Destroy;
var
  I: Integer;
begin
  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if Assigned(FPoints[I]) then
        FPoints[I].Free;
      FPoints[I] := nil;
    end;
    FPoints.Free;
    FPoints := nil;
  end;

  if Assigned(LengthOfTemplate) then
    LengthOfTemplate.Free;
  LengthOfTemplate := nil;

  if Assigned(MinMax2D) then
    MinMax2D.Free;
  MinMax2D := nil;

  inherited;
end;

function TVerticalPartConeShapeIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure TVerticalPartConeShapeIn.Validate;
begin
  if LowXOfProjection = HighXOfProjection then
    raise Exception.Create
      ('Area is not valid with using startX and EndX to output!')
  else if DepthOfCone <= 0 then
    raise Exception.Create('Invalid depth of conical area!');
end;

{ TVerticalPartConeShapeOut }

procedure TVerticalPartConeShapeOut.AddPoint(groupNo: string; point: TPoint2D);
begin
  if (FGroupNo <> groupNo) or (FGroupNo = '') then
  begin
    if (FPoints.Count = 0) or
      ((FPoints.Count > 0) and (FPoints[FPoints.Count - 1].Count > 0)) then
      FPoints.Add(TList<TPoint2D>.Create);
  end;

  FPoints[FPoints.Count - 1].Add(point);

  FGroupNo := groupNo;
end;

constructor TVerticalPartConeShapeOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint2D >>.Create;

  FMinMax := TMinMax2D.Create(TList<TPoint2D>.Create);

  FMinMax.MinD1 := Single.MaxValue;
  FMinMax.MaxD1 := Single.MinValue;
  FMinMax.MinD2 := Single.MaxValue;
  FMinMax.MaxD2 := Single.MinValue;
end;

destructor TVerticalPartConeShapeOut.Destroy;
var
  I, J: Integer;
begin
  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if Assigned(FPoints[I]) then
      begin
        for J := 0 to FPoints[I].Count - 1 do
        begin
          if Assigned(FPoints[I][J]) then
            FPoints[I][J].Free;
          FPoints[I][J] := nil;
        end;
        FPoints[I].Free;
      end;
      FPoints[I] := nil;
    end;
    FPoints.Free;
    FPoints := nil;
  end;

  if Assigned(FMinMax) then
    FMinMax.Free;
  FMinMax := nil;

  inherited;
end;

function TVerticalPartConeShapeOut.FlattenPoints: TList<TPoint2D>;
var
  I: Integer;
  J: Integer;
begin
  result := TList<TPoint2D>.Create;

  if Assigned(FPoints) then
  begin
    for I := 0 to FPoints.Count - 1 do
    begin
      if not Assigned(FPoints[I]) then
        continue;

      for J := 0 to FPoints[I].Count - 1 do
      begin
        result.Add(FPoints[I][J]);
      end;
    end;
  end;
end;

function TVerticalPartConeShapeOut.MinMax: TMinMax2D;
begin
  result := FMinMax;
end;

function TVerticalPartConeShapeOut.Points: TList<TList<TPoint2D>>;
begin
  result := FPoints;
end;

end.
