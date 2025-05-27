unit SquareStoneSetting;

interface

uses Shape, System.Generics.Collections, Classes;

type
  TSquareStoneSettingRecurrenceMode = (ssrCountByDistance, ssrDistanceByCount);

  TSquareStoneSettingIn = class
  public
    Size: Single;
    XCount, YCount: integer;
    XDistance, YDistance: Single;
    RecurrenceMode: TSquareStoneSettingRecurrenceMode;
    LengthOfTemplate: Single;

    constructor Create;
    procedure Validate;
  end;

  TSquareStoneSettingShape = class
  public
    Points: TList<IShape>;
    Lines: TList<IShape>;

    constructor Create;
    destructor Destroy; override;
  end;

  TSquareStoneSettingOut = class
  public
    Shapes: TList<TSquareStoneSettingShape>;

    APointStream: TStream;
    ALineStream: TStream;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TSquareStoneSettingIn }

constructor TSquareStoneSettingIn.Create;
begin
  RecurrenceMode := ssrDistanceByCount;
end;

procedure TSquareStoneSettingIn.Validate;
begin
  if Size <= 0 then
    raise Exception.Create('Invalid size of stone setting!')
  else if (RecurrenceMode = ssrDistanceByCount) and
    (YCount * Size > LengthOfTemplate) then
    raise Exception.Create
      ('Total size has been exceeded to length of template!')
  else if (RecurrenceMode = ssrCountByDistance) and
    (YDistance + Size > LengthOfTemplate) then
    raise Exception.Create
      ('Total size has been exceeded to length of template!')
  else if (RecurrenceMode = ssrCountByDistance) and (YDistance < 0) then
    raise Exception.Create('Invalid distance of Y!')
  else if (RecurrenceMode = ssrCountByDistance) and (YCount <= 0) then
    raise Exception.Create('Invalid count of Y!');
end;

{ TSquareStoneSettingOut }

constructor TSquareStoneSettingOut.Create;
begin
  Shapes := TList<TSquareStoneSettingShape>.Create;
end;

destructor TSquareStoneSettingOut.Destroy;
var
  I: Integer;
begin
  if Assigned(Shapes) then
  begin
    for I := 0 to Shapes.Count - 1 do
    begin
      if Assigned(Shapes[I]) then
      begin
        Shapes[I].Free;
        Shapes[I] := nil;
      end;
    end;
    Shapes.Free;
    Shapes := nil;
  end;

  if Assigned(APointStream) then
    APointStream.Free;
  APointStream := nil;

  if Assigned(ALineStream) then
    ALineStream.Free;
  ALineStream := nil;

  inherited;
end;

{ TSquareStoneSettingShape }

constructor TSquareStoneSettingShape.Create;
begin
  Points := TList<IShape>.Create;
  Lines := TList<IShape>.Create;
end;

destructor TSquareStoneSettingShape.Destroy;
var
  I: Integer;
begin
  if Assigned(Points) then
  begin
    for I := 0 to Points.Count - 1 do
    begin
      if Assigned(Points[I]) then
      begin
        Points[I] := nil;
      end;
    end;
    Points.Free;
    Points := nil;
  end;

  if Assigned(Lines) then
  begin
    for I := 0 to Lines.Count - 1 do
    begin
      if Assigned(Lines[I]) then
      begin
        Lines[I] := nil;
      end;
    end;
    Lines.Free;
    Lines := nil;
  end;

  inherited;
end;

end.
