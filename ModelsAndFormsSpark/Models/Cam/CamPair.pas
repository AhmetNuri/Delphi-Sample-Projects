unit CamPair;

interface

uses Generics.Collections, KeyPair;

type
  TKeys = record
  const
    Header = 'header';
    Footer = 'footer';

    MovementHeader = 'movement_header';
    MovementFooter = 'movement_footer';

    MillingHeader = 'milling_header';
    MillingFooter = 'milling_footer';

    RadiusOfPart = 'radius_of_part';
    RadiusOfProjection = 'radius_of_projection';

    SpeedOfMovement = 'speed_of_movement';
    SpeedOfApproach = 'speed_of_approach';
    SpeedOfMilling = 'speed_of_milling';

    LimitForGapOfTouchAngle = 'limit_for_gap_of_touch_angle';

    SafetyDistance = 'safety_distance';
    ApproachDistance = 'approach_distance';
  end;

  TCamPair = class
  private
    FProperties: TList<TKeyPair>;
  public
    property Properties: TList<TKeyPair> read FProperties;

    procedure AddOrUpdateProperty(key: string; value: integer); overload;
    procedure AddOrUpdateProperty(key: string; value: single); overload;
    procedure AddOrUpdateProperty(key: string; value: Boolean); overload;
    procedure AddOrUpdateProperty(key, value: string); overload;
    procedure AddOrUpdateProperty(areaProperty: TKeyPair); overload;
    procedure RemoveProperty(areaProperty: TKeyPair); overload;
    procedure RemoveProperty(key: string); overload;

    function FindProperty(key: string): TKeyPair;

    constructor Create;
    destructor Destroy; override;
  end;

var
  keys: TKeys;

implementation

uses SysUtils;

{ TCamPair }

constructor TCamPair.Create;
begin
  FProperties := TList<TKeyPair>.Create;
end;

destructor TCamPair.Destroy;
begin
  if Assigned(FProperties) then
  begin
    FProperties.Free;
    FProperties := nil;
  end;

  inherited;
end;

function TCamPair.FindProperty(key: string): TKeyPair;
var
  I: integer;
begin
  result := nil;

  if not Assigned(FProperties) then
    exit;

  for I := 0 to Properties.Count - 1 do
  begin
    if Properties[I].key = key then
    begin
      result := Properties[I];
      exit;
    end;
  end;
end;

procedure TCamPair.AddOrUpdateProperty(key, value: string);
begin
  AddOrUpdateProperty(TKeyPair.Create(key, value));
end;

procedure TCamPair.AddOrUpdateProperty(key: string; value: integer);
begin
  AddOrUpdateProperty(TKeyPair.Create(key, value.ToString));
end;

procedure TCamPair.AddOrUpdateProperty(key: string; value: single);
begin
  AddOrUpdateProperty(TKeyPair.Create(key, value.ToString));
end;

procedure TCamPair.AddOrUpdateProperty(key: string; value: Boolean);
begin
  AddOrUpdateProperty(TKeyPair.Create(key, value.ToString));
end;

procedure TCamPair.AddOrUpdateProperty(areaProperty: TKeyPair);
var
  I: integer;
  Updated: Boolean;
begin
  if not Assigned(areaProperty) then
    exit;

  if not Assigned(FProperties) then
    FProperties := TList<TKeyPair>.Create;

  Updated := false;

  for I := 0 to Properties.Count - 1 do
  begin
    if Properties[I].key = areaProperty.key then
    begin
      Properties[I].SetValue(areaProperty.value);
      Updated := true;
    end;
  end;

  if not Updated then
    Properties.Add(areaProperty);
end;

procedure TCamPair.RemoveProperty(areaProperty: TKeyPair);
begin
  if (not Assigned(FProperties)) or (not Assigned(areaProperty)) then
    exit;

  RemoveProperty(areaProperty.key);
end;

procedure TCamPair.RemoveProperty(key: string);
var
  I: integer;
begin
  if not Assigned(FProperties) then
    exit;

  for I := Properties.Count - 1 downto 0 do
  begin
    if Properties[I].key = key then
      FProperties.Delete(I);
  end;
end;

end.
