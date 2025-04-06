unit MinMax2D;

interface

uses Point2D, Point3D, Generics.Collections, SysUtils;

type
  TMinMax2D = class
  private
    FMinD1, FMaxD1, FMinD2, FMaxD2: Single;
  public
    property MinD1: Single read FMinD1 write FMinD1;
    property MaxD1: Single read FMaxD1 write FMaxD1;
    property MinD2: Single read FMinD2 write FMinD2;
    property MaxD2: Single read FMaxD2 write FMaxD2;

    constructor Create; overload;
    constructor Create(Points: TList<TPoint2D>); overload;
    constructor Create(PointsWithGroup: TList<TPoint3D>); overload;
  end;

implementation

{ TMinMax2D }

constructor TMinMax2D.Create;
begin
  FMinD1 := Single.MaxValue;
  FMaxD1 := Single.MinValue;
  FMinD2 := Single.MaxValue;
  FMaxD2 := Single.MinValue;
end;

constructor TMinMax2D.Create(Points: TList<TPoint2D>);
var
  I: Integer;
begin
  FMinD1 := Single.MaxValue;
  FMaxD1 := Single.MinValue;
  FMinD2 := Single.MaxValue;
  FMaxD2 := Single.MinValue;

  if Assigned(Points) then
  begin
    for I := 0 to Points.Count - 1 do
    begin
      if FMinD1 > Points[I].D1 then FMinD1 := Points[I].D1;
      if FMaxD1 < Points[I].D1 then FMaxD1 := Points[I].D1;
      if FMinD2 > Points[I].D2 then FMinD2 := Points[I].D2;
      if FMaxD2 < Points[I].D2 then FMaxD2 := Points[I].D2;
    end;
  end;
end;

constructor TMinMax2D.Create(PointsWithGroup: TList<TPoint3D>);
var
  I: Integer;
begin
  FMinD1 := Single.MaxValue;
  FMaxD1 := Single.MinValue;
  FMinD2 := Single.MaxValue;
  FMaxD2 := Single.MinValue;

  if Assigned(PointsWithGroup) then
  begin
    for I := 0 to PointsWithGroup.Count - 1 do
    begin
      if FMinD1 > PointsWithGroup[I].D1 then FMinD1 := PointsWithGroup[I].D1;
      if FMaxD1 < PointsWithGroup[I].D1 then FMaxD1 := PointsWithGroup[I].D1;
      if FMinD2 > PointsWithGroup[I].D2 then FMinD2 := PointsWithGroup[I].D2;
      if FMaxD2 < PointsWithGroup[I].D2 then FMaxD2 := PointsWithGroup[I].D2;
    end;
  end;
end;

end.
