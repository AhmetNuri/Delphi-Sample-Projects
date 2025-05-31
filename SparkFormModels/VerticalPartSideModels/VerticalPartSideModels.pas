unit VerticalPartSideModels;

interface

uses Generics.Collections, Point2D, Point3D, Point4D, Point5D, Point6D,
  TouchOffsetType, MinMax2D, SysUtils, StretchMode, TouchOffset,
  LengthOfTemplate, Move, Crop, Stretch, Mirror;

type
  TVerticalPartSideAxisIn = class
  private
    FPoints: TList<TPoint3D>;
  public
    RadiusOfInside: Single;
    RadiusOfOutside: Single;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    XOfDeviation: Single;
    ZOfDeviation: Single;
    LengthOfTemplate: TLengthOfTemplate;
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

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  TVerticalPartSideAxisOut = class
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

  TVerticalPartSideShapeIn = class
  private
    FPoints: TList<TPoint3D>;
  public
    RadiusOfInside: Single;
    RadiusOfOutside: Single;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    LengthOfTemplate: TLengthOfTemplate;
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

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  TVerticalPartSideShapeOut = class
  private
    FGroupNo: string;
    FMinMax: TMinMax2D;
    FPoints: TList<TList<TPoint6D>>;
  public
    function MinMax: TMinMax2D;
    function Points: TList<TList<TPoint6D>>;
    function FlattenPoints: TList<TPoint6D>;
    procedure AddPoint(groupNo: string; point: TPoint6D);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TVerticalPartSideAxisIn }

procedure TVerticalPartSideAxisIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.Value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure TVerticalPartSideAxisIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.Value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor TVerticalPartSideAxisIn.Create;
begin
  inherited;

  RadiusOfInside := 0;
  RadiusOfOutside := 0;
  IsContinuousAngle := true;
  IsStrictDirection := true;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
  XOfDeviation := 0;
  ZOfDeviation := 0;
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

destructor TVerticalPartSideAxisIn.Destroy;
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

function TVerticalPartSideAxisIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure TVerticalPartSideAxisIn.Validate;
begin
  if (RadiusOfInside <= 0) or (RadiusOfOutside <= 0) or
    (RadiusOfInside >= RadiusOfOutside) then
    raise Exception.Create('Set radius values before using model!');
end;

{ TVerticalPartSideAxisOut }

procedure TVerticalPartSideAxisOut.AddPoint(groupNo: string; point: TPoint6D);
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

constructor TVerticalPartSideAxisOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint6D >>.Create;
end;

destructor TVerticalPartSideAxisOut.Destroy;
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

function TVerticalPartSideAxisOut.FlattenPoints: TList<TPoint6D>;
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

function TVerticalPartSideAxisOut.Points: TList<TList<TPoint6D>>;
begin
  result := FPoints;
end;

{ TVerticalPartSideShapeIn }

procedure TVerticalPartSideShapeIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.Value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure TVerticalPartSideShapeIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.Value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor TVerticalPartSideShapeIn.Create;
begin
  inherited;

  RadiusOfInside := 0;
  RadiusOfOutside := 0;
  IsContinuousAngle := true;
  IsStrictDirection := true;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
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

destructor TVerticalPartSideShapeIn.Destroy;
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

function TVerticalPartSideShapeIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure TVerticalPartSideShapeIn.Validate;
begin
  if (RadiusOfInside <= 0) or (RadiusOfOutside <= 0) or
    (RadiusOfInside >= RadiusOfOutside) then
    raise Exception.Create('Set radius values before using model!');
end;

{ TVerticalPartSideShapeOut }

procedure TVerticalPartSideShapeOut.AddPoint(groupNo: string; point: TPoint6D);
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

constructor TVerticalPartSideShapeOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint6D >>.Create;

  FMinMax := TMinMax2D.Create(TList<TPoint2D>.Create);

  FMinMax.MinD1 := Single.MaxValue;
  FMinMax.MaxD1 := Single.MinValue;
  FMinMax.MinD2 := Single.MaxValue;
  FMinMax.MaxD2 := Single.MinValue;
end;

destructor TVerticalPartSideShapeOut.Destroy;
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

function TVerticalPartSideShapeOut.FlattenPoints: TList<TPoint6D>;
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

function TVerticalPartSideShapeOut.MinMax: TMinMax2D;
begin
  result := FMinMax;
end;

function TVerticalPartSideShapeOut.Points: TList<TList<TPoint6D>>;
begin
  result := FPoints;
end;

end.
