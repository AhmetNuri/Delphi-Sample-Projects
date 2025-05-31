unit LineStoneSetting;

interface

uses Shape, System.Generics.Collections, Classes;

type
  TLineStoneSettingRecurrenceMode = (lsrCountByDistance, lsrDistanceByCount);

  TLineStoneSettingIn = class
  public
    Size: Single;
    XCount, YCount: integer;
    YDistance: Single;
    RecurrenceMode: TLineStoneSettingRecurrenceMode;
    LengthOfTemplate: Single;

    constructor Create;
    procedure Validate;
  end;

  TLineStoneSettingOut = class
  public
    Points: TList<IShape>;
    Lines: TList<IShape>;

    APointStream: TStream;
    ALineStream: TStream;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TLineStoneSettingIn }

constructor TLineStoneSettingIn.Create;
begin
  RecurrenceMode := lsrDistanceByCount;
end;

procedure TLineStoneSettingIn.Validate;
begin
  if Size <= 0 then
    raise Exception.Create('Invalid size of stone setting!')
  else if (RecurrenceMode = lsrDistanceByCount) and
    (YCount * Size > LengthOfTemplate) then
    raise Exception.Create
      ('Total size has been exceeded to length of template!')
  else if (RecurrenceMode = lsrCountByDistance) and
    (YDistance + Size > LengthOfTemplate) then
    raise Exception.Create
      ('Total size has been exceeded to length of template!')
  else if (RecurrenceMode = lsrCountByDistance) and (YDistance < 0) then
    raise Exception.Create('Invalid distance of Y!')
  else if (RecurrenceMode = lsrCountByDistance) and (YCount <= 0) then
    raise Exception.Create('Invalid count of Y!');
end;

{ TLineStoneSettingOut }

constructor TLineStoneSettingOut.Create;
begin
  Points := TList<IShape>.Create;
  Lines := TList<IShape>.Create;
end;

destructor TLineStoneSettingOut.Destroy;
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

  if Assigned(APointStream) then
    APointStream.Free;
  APointStream := nil;

  if Assigned(ALineStream) then
    ALineStream.Free;
  ALineStream := nil;

  inherited;
end;

end.
