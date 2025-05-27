unit CamModel;

interface

uses Generics.Collections, CamBlock, Classes, CamPair, CamType, CamLetter;

type
  TCamModel = class(TCamPair)
  private
    FIsReady: boolean;
    FCamType: TCamType;
    FBlocks: TList<TCamBlock>;
    FLetters: TDictionary<TCamLetter, TCamLetterVariable>;
  public
    property Blocks: TList<TCamBlock> read FBlocks;
    procedure AddBlock(block: TCamBlock);
    procedure DeleteBlock(index: integer);

    property CamType: TCamType read FCamType;
    procedure SwitchCamType(CamType: TCamType);

    property Letters: TDictionary<TCamLetter, TCamLetterVariable> read FLetters;
    procedure SetLetter(key: TCamLetter; value: TCamLetterVariable);
    procedure SetLetters(Letters: TDictionary<TCamLetter, TCamLetterVariable>);

    procedure UpdateBlocks;
    procedure ApplySettings;

    procedure Validate;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TCamModel }

procedure TCamModel.AddBlock(block: TCamBlock);
begin
  if FCamType = ctNone then
    raise Exception.Create('Set cam type before adding block!')
  else if not Assigned(Letters) then
    raise Exception.Create('Set letters before adding block!')
  else if Letters.Keys.Count <> Ord(High(TCamLetter)) - Ord(Low(TCamLetter)) + 1
  then
    raise Exception.Create('Set all letters before adding block!');

  if not Assigned(FBlocks) then
    FBlocks := TList<TCamBlock>.Create;

  FBlocks.Add(block);

  ApplySettings;
end;

constructor TCamModel.Create;
begin
  FIsReady := false;
  FCamType := ctNone;
end;

procedure TCamModel.DeleteBlock(index: integer);
begin
  if not Assigned(FBlocks) then
    exit
  else if (index < 0) or (index >= Blocks.Count) then
    exit;

  Blocks.Delete(index);
end;

destructor TCamModel.Destroy;
begin
  if Assigned(FBlocks) then
  begin
    FBlocks.Free;
    FBlocks := nil;
  end;

  if Assigned(FLetters) then
  begin
    FLetters.Free;
    FLetters := nil;
  end;

  inherited;
end;

procedure TCamModel.ApplySettings;
var
  I: integer;
begin
  if (FindProperty(Keys.RadiusOfPart) = nil) and (CamType in [ctVPA, ctVPC])
  then
    raise Exception.Create('Set radius of part before setting model!')
  else if (FindProperty(Keys.RadiusOfProjection) = nil) and
    (CamType in [ctVPA, ctVPC, ctHPFR, ctHPHR]) then
    raise Exception.Create('Set radius of projection before setting model!')
  else if FindProperty(Keys.SpeedOfMovement) = nil then
    raise Exception.Create('Set speed of movement before setting model!')
  else if FindProperty(Keys.SpeedOfMilling) = nil then
    raise Exception.Create('Set speed of milling before setting model!')
  else if FindProperty(Keys.SpeedOfApproach) = nil then
    raise Exception.Create('Set speed of approach before setting model!')
  else if FindProperty(Keys.LimitForGapOfTouchAngle) = nil then
    raise Exception.Create('Set limit for gap of touch before setting model!')
  else if FindProperty(Keys.SafetyDistance) = nil then
    raise Exception.Create('Set safety distance before setting model!')
  else if FindProperty(Keys.SafetyDistance).AsSingle <= 0 then
    raise Exception.Create('Approach distance must be higher than zero!')
  else if FindProperty(Keys.ApproachDistance) = nil then
    raise Exception.Create('Set approach distance before setting model!')
  else if FindProperty(Keys.ApproachDistance).AsSingle <= 0 then
    raise Exception.Create('Approach distance must be higher than zero!')
  else if FindProperty(Keys.ApproachDistance).AsSingle >=
    FindProperty(Keys.SafetyDistance).AsSingle then
    raise Exception.Create
      ('Approach distance must be less than safety distance!');

  if Assigned(Blocks) and (Blocks.Count > 0) then
  begin
    for I := 0 to Blocks.Count - 1 do
    begin
      if not Assigned(Blocks[I].Axis) then
        raise Exception.Create
          ('Assign all axises of blocks before setting model!');

      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.RadiusOfPart));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.SpeedOfMovement));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.SpeedOfMilling));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.SpeedOfApproach));
      Blocks[I].Axis.AddOrUpdateProperty
        (FindProperty(Keys.LimitForGapOfTouchAngle));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.SafetyDistance));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.ApproachDistance));
      Blocks[I].Axis.AddOrUpdateProperty(FindProperty(Keys.RadiusOfProjection));
    end;

    FIsReady := true;
  end;
end;

procedure TCamModel.SetLetter(key: TCamLetter; value: TCamLetterVariable);
begin
  if not Assigned(Letters) then
    FLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;

  FLetters.AddOrSetValue(key, value);

  UpdateBlocks;
end;

procedure TCamModel.SetLetters(Letters: TDictionary<TCamLetter,
  TCamLetterVariable>);
begin
  FLetters := Letters;

  UpdateBlocks;
end;

procedure TCamModel.SwitchCamType(CamType: TCamType);
begin
  FCamType := CamType;

  UpdateBlocks;
end;

procedure TCamModel.UpdateBlocks;
var
  I: integer;
begin
  if Assigned(Blocks) and (Blocks.Count > 0) then
  begin
    for I := 0 to Blocks.Count - 1 do
    begin
      Blocks[I].SetLetters(Letters);
      Blocks[I].SwitchCamType(CamType);
    end;
  end;
end;

procedure TCamModel.Validate;
var
  I: integer;
begin
  if (not Assigned(Blocks)) or (Blocks.Count = 0) then
    raise Exception.Create('Assign block list before generating code!')
  else if FCamType = ctNone then
    raise Exception.Create('Set cam type before generating code!')
  else if not Assigned(Letters) then
    raise Exception.Create('Set letters before generating code!')
  else if Letters.Keys.Count <> Ord(High(TCamLetter)) - Ord(Low(TCamLetter)) + 1
  then
    raise Exception.Create('Set all letters before generating code!');

  for I := 0 to Blocks.Count - 1 do
  begin
    if not Assigned(Blocks[I].Axis) then
      raise Exception.Create('Assign axis before generating code!')
    else if Blocks[I].Axis.GetType = '' then
      raise Exception.Create('Invalid axis type!')
    else if not Assigned(Blocks[I].Axis.Tool) then
      raise Exception.Create('Assign tool for axis before generating code!')
    else if not Assigned(Blocks[I].Axis.Points) then
      raise Exception.Create('Assign points before generating code!')
    else if Blocks[I].Axis.Points.Count = 0 then
      raise Exception.Create('Empty points found for generating code!');
  end;

  if not FIsReady then
    raise Exception.Create('Apply model settings before generating code!');
end;

end.
