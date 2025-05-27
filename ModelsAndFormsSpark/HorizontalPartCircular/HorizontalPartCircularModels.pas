unit HorizontalPartCircularModels;

interface

uses Generics.Collections, Point2D, Point3D, Point4D, Point5D, Point6D, Mirror,
  TouchOffsetType, MinMax2D, SysUtils, StretchMode, TouchOffset,
  LengthOfTemplate, Move, Crop, Stretch;

type
  THorizontalCircularAxisIn = class
  private
    FPoints: TList<TPoint3D>;
  public
    StartXOfProjectionRadius: Single;
    EndXOfProjectionRadius: Single;
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
    ValidateAngleOfTouch: Boolean;
    IsStrictDirection: Boolean;

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  THorizontalCircularAxisOut = class
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

  THorizontalCircularShapeIn = class
  private
    FPoints: TList<TPoint3D>;
  public
    ValidateAngleOfTouch: Boolean;
    StartXOfProjectionRadius: Single;
    EndXOfProjectionRadius: Single;
    RadiusOfProjection: Single;
    LengthOfTemplate: TLengthOfTemplate;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    OffsetOfTouchAngle: TTouchOffset;
    Mirror: TMirror;
    Move: TMove;
    Crop: TCrop;
    Stretch: TStretch;
    MinMax2D: TMinMax2D;

    constructor Create;
    destructor Destroy; override;

    procedure Validate;

    function Points: TList<TPoint3D>;
    procedure AddPoint(point: TPoint3D);
    procedure Assign(Points: TList<TPoint3D>);
  end;

  THorizontalCircularShapeOut = class
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

{ THorizontalCircularAxisIn }

procedure THorizontalCircularAxisIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure THorizontalCircularAxisIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor THorizontalCircularAxisIn.Create;
begin
  inherited;

  StartXOfProjectionRadius := 0;
  EndXOfProjectionRadius := 0;
  RadiusOfProjection := 0;
  XOfDeviation := 0;
  ZOfDeviation := 0;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
  IsStrictDirection := true;
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

destructor THorizontalCircularAxisIn.Destroy;
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

function THorizontalCircularAxisIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure THorizontalCircularAxisIn.Validate;
begin
  if StartXOfProjectionRadius > EndXOfProjectionRadius then
    raise Exception.Create
      ('Area is not valid with using startX and EndX to output!');
end;

{ THorizontalCircularAxisOut }

procedure THorizontalCircularAxisOut.AddPoint(groupNo: string; point: TPoint6D);
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

constructor THorizontalCircularAxisOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint6D >>.Create;
end;

destructor THorizontalCircularAxisOut.Destroy;
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

function THorizontalCircularAxisOut.FlattenPoints: TList<TPoint6D>;
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

function THorizontalCircularAxisOut.Points: TList<TList<TPoint6D>>;
begin
  result := FPoints;
end;

{ THorizontalCircularShapeIn }

procedure THorizontalCircularShapeIn.AddPoint(point: TPoint3D);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(point) then
    FPoints.Add(point);
end;

procedure THorizontalCircularShapeIn.Assign(Points: TList<TPoint3D>);
begin
  if not Assigned(LengthOfTemplate) or (LengthOfTemplate.value <= 0) then
    raise Exception.Create('Set template length before assign points');

  if Assigned(Points) then
    FPoints := Points;
end;

constructor THorizontalCircularShapeIn.Create;
begin
  inherited;

  StartXOfProjectionRadius := 0;
  EndXOfProjectionRadius := 0;
  RadiusOfProjection := 0;
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

destructor THorizontalCircularShapeIn.Destroy;
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

function THorizontalCircularShapeIn.Points: TList<TPoint3D>;
begin
  result := FPoints;
end;

procedure THorizontalCircularShapeIn.Validate;
begin
  if StartXOfProjectionRadius > EndXOfProjectionRadius then
    raise Exception.Create
      ('Area is not valid with using startX and EndX to output!');
end;

{ THorizontalCircularShapeOut }

procedure THorizontalCircularShapeOut.AddPoint(groupNo: string;
  point: TPoint2D);
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

constructor THorizontalCircularShapeOut.Create;
begin
  FGroupNo := '';
  FPoints := TList < TList < TPoint2D >>.Create;

  FMinMax := TMinMax2D.Create(TList<TPoint2D>.Create);

  FMinMax.MinD1 := Single.MaxValue;
  FMinMax.MaxD1 := Single.MinValue;
  FMinMax.MinD2 := Single.MaxValue;
  FMinMax.MaxD2 := Single.MinValue;
end;

destructor THorizontalCircularShapeOut.Destroy;
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

function THorizontalCircularShapeOut.FlattenPoints: TList<TPoint2D>;
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

function THorizontalCircularShapeOut.MinMax: TMinMax2D;
begin
  result := FMinMax;
end;

function THorizontalCircularShapeOut.Points: TList<TList<TPoint2D>>;
begin
  result := FPoints;
end;

end.
