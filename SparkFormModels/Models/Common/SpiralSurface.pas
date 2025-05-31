unit SpiralSurface;

interface

uses Shape, System.Generics.Collections, Classes;

type
  TSpiralSurfaceMode = (ssmCountByDistance, ssmDistanceByCount);

  TSpiralSurfaceIn = class
  public
    RuleValue: Single;
    StartX, EndX: Single;
    RuleMode: TSpiralSurfaceMode;
    LengthOfTemplate: Single;

    constructor Create;
    procedure Validate;
  end;

  TSpiralSurfaceOut = class
  public
    Lines: TList<IShape>;
    AStream: TStream;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TSpiralSurfaceIn }

constructor TSpiralSurfaceIn.Create;
begin
  StartX := 0;
  EndX := 0;
  LengthOfTemplate := 0;
  RuleMode := ssmDistanceByCount;
  RuleValue := 0;
end;

procedure TSpiralSurfaceIn.Validate;
begin
  if RuleValue <= 0 then
    raise Exception.Create('Invalid value of rule for spiral surface!')
  else if LengthOfTemplate <= 0 then
    raise Exception.Create('Invalid length of template for spiral surface!')
  else if StartX >= EndX then
    raise Exception.Create('Invalid area for spiral surface!')
  else if EndX - StartX > 1000 then
    raise Exception.Create('Size of area is too long for spiral surface!')
  else if (RuleMode = ssmDistanceByCount) and
    (RuleValue > (EndX - StartX) * 9999) then
    raise Exception.Create('Invalid value of rule for spiral surface!')
  else if (RuleMode = ssmDistanceByCount) and (RuleValue <= 1) then
    raise Exception.Create('Invalid value of rule for spiral surface!');
end;

{ TSpiralSurfaceOut }

constructor TSpiralSurfaceOut.Create;
begin
  Lines := TList<IShape>.Create;
end;

destructor TSpiralSurfaceOut.Destroy;
var
  I: Integer;
begin
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

  if Assigned(AStream) then
    AStream.Free;
  AStream := nil;

  inherited;
end;

end.
