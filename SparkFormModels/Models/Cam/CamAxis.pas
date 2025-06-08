unit CamAxis;

interface

uses Generics.Collections, CamPair, CamTool, Point2D, Point6D, Classes,
  CamLetter, CamType, MillingRequest;

type
  AxisPoint6D = class helper for TPoint6D
  public
    function X: Single;
    function Y: Single;
    function Z: Single;
    function AngleOfTemplate: Single;
    function AngleOfArm: Single;
    function AngleOfTouch: Single;
  end;

  TRowOfMilling = record
  public
    HasLinearAxis: boolean;
    HasCircularAxis: boolean;
  end;

  // Base type for spindle, laser...
  TCamAxis = class(TCamPair)
  private
    FTool: TCamTool;
    FCamType: TCamType;
    FMilling: TMillingRequest;
    FPoints: TList<TList<TPoint6D>>;
    FShapePoints: TList<TList<TPoint2D>>;
    FLetters: TDictionary<TCamLetter, TCamLetterVariable>;

    function CreateRows(point: TPoint6D; depth: Single;
      latestRow: TDictionary<TCamLetter, Single>): TStringList;
    function CreateMovements(point: TPoint6D;
      latestRow: TDictionary<TCamLetter, Single>): TStringList;

    function CenterZ: Single;
    function SpeedOfMovement: Single;
    function SpeedOfMilling: Single;
    function SpeedOfApproach: Single;
    function LimitForGapOfTouchAngle: Single;
    function SafetyDistance: Single;
    function ApproachDistance: Single;
    function MillingSteps: TList<Single>;
  public
    property Tool: TCamTool read FTool;
    property Points: TList < TList < TPoint6D >> read FPoints;
    property ShapePoints: TList < TList < TPoint2D >> read FShapePoints;

    procedure SetTool(Tool: TCamTool);
    procedure SetPoints(Points: TList < TList < TPoint6D >> );
    procedure SetShapePoints(Points: TList < TList < TPoint2D >> );

    property Milling: TMillingRequest read FMilling;
    procedure SetMilling(Milling: TMillingRequest);

    property CamType: TCamType read FCamType;
    procedure SwitchCamType(CamType: TCamType);

    property Letters: TDictionary<TCamLetter, TCamLetterVariable> read FLetters;
    procedure SetLetters(Letters: TDictionary<TCamLetter, TCamLetterVariable>);

    function GenerateCode: TStringList; virtual;
    function GetType: String; virtual;

    function GenerateShape: TStringList;

    constructor Create;
    destructor Destroy; override;
  protected
    AxisLetters: TList<TCamLetter>;

    function GenerateAxisCode: TStringList;
  end;

implementation

uses SysUtils, Math, StrUtils;

{ TCamAxis }

function ToSingle(const input: string): Single;
var
  FS: TFormatSettings;
  strValue: string;
  left, right, eStr: string;
begin
  FS := TFormatSettings.Create;
  FS.DecimalSeparator := '.';
  strValue := input.Replace(',', '.', [rfReplaceAll]);

  if strValue.Contains('E') then
  begin
    left := strValue.Split(['E'])[0].Replace('.', '');
    eStr := strValue.Split(['E'])[1].Replace('-', '');

    if eStr.ToInteger > 3 then
      result := 0
    else if eStr.ToInteger = 3 then
      result := ('0.000' + left.Substring(1, 1)).ToExtended
    else if eStr.ToInteger = 2 then
      result := ('0.00' + left.Substring(1, 2)).ToExtended
    else if eStr.ToInteger = 1 then
      result := ('0.0' + left.Substring(1, 3)).ToExtended
    else
      result := 0;
  end
  else if strValue.Contains('.') then
  begin
    left := strValue.Split(['.'])[0];
    right := strValue.Split(['.'])[1];

    if length(right) > 4 then
      right := Copy(right, 1, 4);

    result := RoundTo(StrToFloat(left + '.' + right, FS), -4);
  end
  else
    result := StrToFloat(input);
end;

function FormattedValue(const input: Single): string;
begin
  result := FormatFloat('0.0000', input).Replace(',', '.', [rfReplaceAll]);
end;

constructor TCamAxis.Create;
begin
  AxisLetters := TList<TCamLetter>.Create;
  FTool := TCamTool.Create;
end;

destructor TCamAxis.Destroy;
begin
  if Assigned(FTool) then
  begin
    FTool.Free;
    FTool := nil;
  end;

  if Assigned(FPoints) then
  begin
    FPoints.Free;
    FPoints := nil;
  end;

  if Assigned(FLetters) then
  begin
    FLetters.Free;
    FLetters := nil;
  end;

  if Assigned(AxisLetters) then
  begin
    AxisLetters.Free;
    AxisLetters := nil;
  end;

  inherited;
end;

function TCamAxis.GenerateCode: TStringList;
begin
  result := TStringList.Create;
end;

function TCamAxis.GetType: String;
begin
  result := '';
end;

procedure TCamAxis.SetLetters(Letters: TDictionary<TCamLetter,
  TCamLetterVariable>);
begin
  FLetters := Letters;
end;

procedure TCamAxis.SetMilling(Milling: TMillingRequest);
begin
  if ((Milling.StepCount * Milling.StepValue) <= 0) and
    (Milling.FinishValue <= 0) then
  begin
    Milling.StepValue := 0;
    Milling.StepCount := 1;
    Milling.FinishValue := 0;
  end;

  FMilling := Milling;
end;

procedure TCamAxis.SetPoints(Points: TList < TList < TPoint6D >> );
begin
  FPoints := Points;
end;

procedure TCamAxis.SetShapePoints(Points: TList < TList < TPoint2D >> );
begin
  FShapePoints := Points;
end;

procedure TCamAxis.SetTool(Tool: TCamTool);
begin
  FTool := Tool;
end;

function TCamAxis.SpeedOfMovement: Single;
begin
  if FindProperty(Keys.SpeedOfMovement) = nil then
    raise Exception.Create('Speed of movement coulnd''t be found!');

  result := FindProperty(Keys.SpeedOfMovement).AsSingle;
end;

function TCamAxis.SpeedOfApproach: Single;
begin
  if FindProperty(Keys.SpeedOfApproach) = nil then
    raise Exception.Create('Speed of approach coulnd''t be found!');

  result := FindProperty(Keys.SpeedOfApproach).AsSingle;
end;

function TCamAxis.SpeedOfMilling: Single;
begin
  if FindProperty(Keys.SpeedOfMilling) = nil then
    raise Exception.Create('Speed of milling coulnd''t be found!');

  result := FindProperty(Keys.SpeedOfMilling).AsSingle;
end;

function TCamAxis.LimitForGapOfTouchAngle: Single;
begin
  if FindProperty(Keys.LimitForGapOfTouchAngle) = nil then
    raise Exception.Create('Limit for gap of touch coulnd''t be found!');

  result := FindProperty(Keys.LimitForGapOfTouchAngle).AsSingle;
end;

function TCamAxis.MillingSteps: TList<Single>;
var
  I: Integer;
  StepValue: Single;
begin
  StepValue := 0;
  result := TList<Single>.Create;

  SetMilling(Milling);

  for I := 0 to Milling.StepCount - 1 do
  begin
    result.Add(StepValue + Milling.StepValue);
    StepValue := StepValue + Milling.StepValue;
  end;

  if Milling.FinishValue > 0 then
    result.Add(StepValue + Milling.FinishValue);
end;

function TCamAxis.ApproachDistance: Single;
begin
  if FindProperty(Keys.ApproachDistance) = nil then
    raise Exception.Create('Approach distance coulnd''t be found!')
  else if FindProperty(Keys.ApproachDistance).AsSingle <= 0 then
    raise Exception.Create('Approach distance must be higher than zero!');

  result := FindProperty(Keys.ApproachDistance).AsSingle;
end;

function TCamAxis.CenterZ: Single;
begin
  if FindProperty(Keys.RadiusOfPart) = nil then
    raise Exception.Create('Radius of part couldn''t be found!');

  result := FindProperty(Keys.RadiusOfPart).AsSingle;
end;

function TCamAxis.SafetyDistance: Single;
begin
  if FindProperty(Keys.SafetyDistance) = nil then
    raise Exception.Create('Safety distance coulnd''t be found!')
  else if FindProperty(Keys.SafetyDistance).AsSingle <= 0 then
    raise Exception.Create('Safety distance must be higher than zero!');

  result := FindProperty(Keys.SafetyDistance).AsSingle;
end;

procedure TCamAxis.SwitchCamType(CamType: TCamType);
begin
  FCamType := CamType;
end;

function TCamAxis.CreateMovements(point: TPoint6D;
  latestRow: TDictionary<TCamLetter, Single>): TStringList;

  function IsSameLocation: boolean;
  var
    currentAngleOfTemplate, latestAngleOfTemplate: Single;
  begin
    result := latestRow.ContainsKey(clXOfCnc);

    if latestRow.ContainsKey(clXOfCnc) and (point.X <> latestRow[clXOfCnc]) then
      result := false
    else if latestRow.ContainsKey(clYOfCnc) and (point.Y <> latestRow[clYOfCnc])
    then
      result := false
    else if latestRow.ContainsKey(clZOfCnc) and (point.Z <> latestRow[clZOfCnc])
    then
      result := false
    else if latestRow.ContainsKey(clArm) and
      (point.AngleOfArm <> latestRow[clArm]) then
      result := false
    else if latestRow.ContainsKey(clAxisC) and
      (point.AngleOfTouch <> latestRow[clAxisC]) then
      result := false
    else if latestRow.ContainsKey(clAxisU) and
      (point.AngleOfTouch <> latestRow[clAxisU]) then
      result := false
    else if latestRow.ContainsKey(clDivisor) then
    begin
      currentAngleOfTemplate := point.AngleOfTemplate;
      while currentAngleOfTemplate > 360 do
        currentAngleOfTemplate := currentAngleOfTemplate - 360;
      while currentAngleOfTemplate < 0 do
        currentAngleOfTemplate := currentAngleOfTemplate + 360;

      latestAngleOfTemplate := latestRow[clDivisor];
      while latestAngleOfTemplate >= 360 do
        latestAngleOfTemplate := latestAngleOfTemplate - 360;
      while latestAngleOfTemplate < 0 do
        latestAngleOfTemplate := latestAngleOfTemplate + 360;

      if currentAngleOfTemplate <> latestAngleOfTemplate then
        result := false;
    end;
  end;

  procedure AddRow(items: TStringList; letterKey: TCamLetter; value: Single;
    errorMessage: string);
  var
    latest, directionValue: Single;
  begin
    if AxisLetters.Contains(letterKey) then
    begin
      if Letters.ContainsKey(letterKey) then
      begin
        directionValue := ToSingle((value * Letters[letterKey].Direction)
          .ToString);

        if latestRow.TryGetValue(letterKey, latest) and (latest = directionValue)
        then
          exit;

        items.Add(Letters[letterKey].Letter.Trim +
          FormattedValue(directionValue));
      end
      else
        raise Exception.Create(errorMessage);

      latestRow.AddOrSetValue(letterKey, directionValue);
    end;
  end;

var
  Z: Single;
  startOfMovement: boolean;
  rows: TStringList;
begin
  Z := point.Z;
  result := TStringList.Create;

  if IsSameLocation then
    exit;

  if latestRow.ContainsKey(clZOfCnc) then
  begin
    Z := Max(latestRow[clZOfCnc], point.Z);

    if (CamType in [ctVPA, ctHPNR, ctHPFR, ctHPHR]) and
      (FindProperty(Keys.RadiusOfProjection).AsSingle <> 0) then
      Z := Max(Z, CenterZ);

    Z := ToSingle(Z.ToString);
  end;

  result.Add('G0 ' + Letters[clF].Letter.Trim + SpeedOfMovement.ToString);
  latestRow.AddOrSetValue(clF, SpeedOfMovement);
  startOfMovement := false;

  if latestRow.ContainsKey(clZOfCnc) then
  begin
    AddRow(result, clZOfCnc, Z + SafetyDistance,
      'Letter of Z couldn''t be found!');
  end
  else
  begin
    startOfMovement := true;

    result.Add('');
    result.Add(FindProperty(Keys.MovementHeader).value);
    result.Add('');
  end;

  rows := TStringList.Create;
  AddRow(rows, clXOfCnc, point.X, 'Letter of X couldn''t be found!');
  AddRow(rows, clYOfCnc, point.Y, 'Letter of Y couldn''t be found!');
  AddRow(rows, clDivisor, point.AngleOfTemplate,
    'Letter of Divisor couldn''t be found!');
  AddRow(rows, clAxisC, point.AngleOfTouch,
    'Letter of Angle of Touch couldn''t be found!');
  AddRow(rows, clAxisU, point.AngleOfTouch,
    'Letter of Angle of Touch couldn''t be found!');
  AddRow(rows, clArm, point.AngleOfArm,
    'Letter of AngleOfArm couldn''t be found!');

  rows.Delimiter := ' ';
  if rows.Count > 0 then
    result.Add(rows.DelimitedText);

  rows.Free;
  rows := nil;

  if startOfMovement then
  begin
    result.Add('');
    AddRow(result, clZOfCnc, Z + SafetyDistance,
      'Letter of Z couldn''t be found!');
    result.Add('');
  end;

  result.Add('G1 ' + Letters[clF].Letter.Trim + SpeedOfApproach.ToString);
  latestRow.AddOrSetValue(clF, SpeedOfApproach);

  if startOfMovement then
  begin
    startOfMovement := false;

    result.Add('');
    result.Add(FindProperty(Keys.MillingHeader).value);
    result.Add('');
  end;

  AddRow(result, clZOfCnc, Z + ApproachDistance,
    'Letter of Z couldn''t be found!');
end;

function TCamAxis.CreateRows(point: TPoint6D; depth: Single;
  latestRow: TDictionary<TCamLetter, Single>): TStringList;

  procedure AddSpeedOfMilling(items: TStringList; rowOfMilling: TRowOfMilling);
  var
    latest: Single;
    speed: integer;
  begin
    if not rowOfMilling.HasLinearAxis and rowOfMilling.HasCircularAxis then
      speed := trunc(SpeedOfMilling * PI)
    else
      speed := trunc(SpeedOfMilling);

    if latestRow.TryGetValue(clF, latest) then
    begin
      if trunc(latest) <> speed then
        items.Add(Letters[clF].Letter.Trim + speed.ToString);
    end
    else
      items.Add(Letters[clF].Letter.Trim + speed.ToString);

    latestRow.AddOrSetValue(clF, speed);
  end;

  function AddRow(items: TStringList; letterKey: TCamLetter;
    value, extValue: Single; errorMessage: string): TRowOfMilling;
  var
    latest, directionValue: Single;
  begin
    result.HasLinearAxis := false;
    result.HasCircularAxis := false;

    if AxisLetters.Contains(letterKey) then
    begin
      directionValue := ToSingle((value * Letters[letterKey].Direction)
        .ToString);

      if Letters.ContainsKey(letterKey) then
      begin
        if latestRow.TryGetValue(letterKey, latest) and (latest = directionValue)
        then
          exit;

        items.Add(Letters[letterKey].Letter.Trim + FormattedValue(directionValue
          + extValue));
      end
      else
        raise Exception.Create(errorMessage);

      if Letters[letterKey].IsLinear then
        result.HasLinearAxis := true
      else
        result.HasCircularAxis := true;

      latestRow.AddOrSetValue(letterKey, directionValue);
    end;
  end;

  function SafetyForAngleOfTouch(Z, AngleOfTouch: Single): string;
  var
    I: integer;
    latest: Single;
    items: TStringList;
    latestZ, currentZ, directionValue: Single;
    lettersOfAngleOfTouch: TList<TCamLetter>;
  begin
    result := '';
    currentZ := Z;
    lettersOfAngleOfTouch := TList<TCamLetter>.Create;
    lettersOfAngleOfTouch.Add(clAxisC);
    lettersOfAngleOfTouch.Add(clAxisU);

    for I := 0 to lettersOfAngleOfTouch.Count - 1 do
    begin
      if AxisLetters.Contains(lettersOfAngleOfTouch[I]) then
      begin
        if Letters.ContainsKey(lettersOfAngleOfTouch[I]) then
        begin
          if latestRow.TryGetValue(lettersOfAngleOfTouch[I], latest) and
            (latest = AngleOfTouch * Letters[lettersOfAngleOfTouch[I]].Direction)
          then
            continue;

          if latestRow.TryGetValue(clZOfCnc, latestZ) then
            currentZ := latestZ;

          if Abs((AngleOfTouch * Letters[lettersOfAngleOfTouch[I]].Direction) -
            latest) <= LimitForGapOfTouchAngle then
          begin
            FreeAndNil(lettersOfAngleOfTouch);
            exit;
          end;

          items := TStringList.Create;

          items.Add('G0 ' + Letters[clF].Letter.Trim +
            SpeedOfMovement.ToString);
          latestRow.AddOrSetValue(clF, SpeedOfMovement);

          directionValue :=
            ToSingle(((currentZ + SafetyDistance) * Letters[clZOfCnc].Direction)
            .ToString);

          items.Add(Letters[clZOfCnc].Letter.Trim +
            FormattedValue(directionValue));

          latestRow.AddOrSetValue(clZOfCnc, directionValue);

          directionValue :=
            ToSingle((AngleOfTouch * Letters[lettersOfAngleOfTouch[I]]
            .Direction).ToString);

          items.Add(Letters[lettersOfAngleOfTouch[I]].Letter.Trim +
            FormattedValue(directionValue));

          latestRow.AddOrSetValue(lettersOfAngleOfTouch[I], directionValue);

          items.Add('G1 ' + Letters[clF].Letter.Trim +
            SpeedOfApproach.ToString);
          latestRow.AddOrSetValue(clF, SpeedOfApproach);

          directionValue :=
            ToSingle(((currentZ + ApproachDistance) * Letters[clZOfCnc]
            .Direction).ToString);

          items.Add(Letters[clZOfCnc].Letter.Trim +
            FormattedValue(directionValue));

          latestRow.AddOrSetValue(clZOfCnc, directionValue);
        end
        else
          continue;

        result := items.Text;

        FreeAndNil(items);

        // run once when found
        break;
      end;
    end;

    FreeAndNil(lettersOfAngleOfTouch);
  end;

var
  safety: string;
  items: TStringList;
  rowOfMilling: TRowOfMilling;
begin
  result := TStringList.Create;
  items := TStringList.Create;

  rowOfMilling.HasLinearAxis := false;
  rowOfMilling.HasCircularAxis := false;

  safety := SafetyForAngleOfTouch(point.Z, point.AngleOfTouch);

  with AddRow(items, clXOfCnc, point.X, 0, 'Letter of X couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clYOfCnc, point.Y, 0, 'Letter of Y couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clZOfCnc, point.Z, -depth,
    'Letter of Z couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clDivisor, point.AngleOfTemplate, 0,
    'Letter of Divisor couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clArm, point.AngleOfArm, 0,
    'Letter of Arm couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clAxisC, point.AngleOfTouch, 0,
    'Letter of Angle of Touch couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  with AddRow(items, clAxisU, point.AngleOfTouch, 0,
    'Letter of Angle of Touch couldn''t be found!') do
  begin
    rowOfMilling.HasLinearAxis := rowOfMilling.HasLinearAxis or HasLinearAxis;
    rowOfMilling.HasCircularAxis := rowOfMilling.HasCircularAxis or
      HasCircularAxis;
  end;

  AddSpeedOfMilling(items, rowOfMilling);

  if items.Count > 0 then
  begin
    if safety <> '' then
    begin
      if safety.EndsWith(sLineBreak) then
        Delete(safety, length(safety) - length(sLineBreak) + 1,
          length(sLineBreak));

      result.Add(safety);
    end;

    result.Add(String.Join(' ', items.ToStringArray));
  end;

  FreeAndNil(items);
end;

function TCamAxis.GenerateShape: TStringList;
var
  groupIndex, partIndex: integer;
begin
  result := TStringList.Create;

  if (not Assigned(ShapePoints)) or (ShapePoints.Count = 0) then
    exit;

  for groupIndex := 0 to ShapePoints.Count - 1 do
  begin
    for partIndex := 0 to ShapePoints[groupIndex].Count - 1 do
    begin
      result.Add('X' + FormattedValue(ShapePoints[groupIndex][partIndex].D1) +
        ' Y' + FormattedValue(ShapePoints[groupIndex][partIndex].D2));
    end;
  end;
end;

function TCamAxis.GenerateAxisCode: TStringList;
var
  row: string;
  movements: TStringList;
  groupIndex, partIndex, depthIndex: integer;
  latestRow: TDictionary<TCamLetter, Single>;
  depthSteps: TList<Single>;
begin
  result := TStringList.Create;

  if (not Assigned(AxisLetters)) or (AxisLetters.Count = 0) then
    exit
  else if (not Assigned(Points)) or (Points.Count = 0) then
    exit;

  latestRow := TDictionary<TCamLetter, Single>.Create;

  depthSteps := MillingSteps;

  for depthIndex := 0 to depthSteps.Count - 1 do
  begin
    for groupIndex := 0 to Points.Count - 1 do
    begin
      if Points[groupIndex].Count > 0 then
      begin
        movements := CreateMovements(Points[groupIndex][0], latestRow);

        if Assigned(movements) and (movements.Count > 0) then
        begin
          result.AddStrings(movements);
          FreeAndNil(movements);
        end;
      end;

      for partIndex := 0 to Points[groupIndex].Count - 1 do
      begin
        row := CreateRows(Points[groupIndex][partIndex], depthSteps[depthIndex],
          latestRow).Text;

        if row <> '' then
          result.Add(row.Trim);
      end;
    end;
  end;

  result.Add('');
  result.Add(FindProperty(Keys.MillingFooter).value);
  result.Add(FindProperty(Keys.MovementFooter).value);
  result.Add('');

  FreeAndNil(movements);
  FreeAndNil(latestRow);
  FreeAndNil(depthSteps);
end;

{ AxisPoint6D }

function AxisPoint6D.X: Single;
begin
  result := ToSingle(self.D1.ToString);
end;

function AxisPoint6D.Y: Single;
begin
  result := ToSingle(self.D2.ToString);
end;

function AxisPoint6D.Z: Single;
begin
  result := ToSingle(self.D3.ToString);
end;

function AxisPoint6D.AngleOfTemplate: Single;
begin
  result := ToSingle(self.D4.ToString);
end;

function AxisPoint6D.AngleOfArm: Single;
begin
  result := ToSingle(self.D5.ToString);
end;

function AxisPoint6D.AngleOfTouch: Single;
begin
  result := ToSingle(self.D6.ToString);
end;

end.
