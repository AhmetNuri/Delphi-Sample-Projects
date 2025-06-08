unit TurningPositiveToPositive;

interface

uses Turning;

type
  TTurningPositiveToPositive = class(TTurning)
  private
    function Lines: string;

  public
    function GenerateCodes: boolean; override;
  end;

implementation

uses System.Classes, System.Generics.Collections, Point3D, Math,
  System.SysUtils;

const
  FileName = 'turning.txt';

procedure CreateOrClearFile;
var
  FileHandle: TextFile;
begin
  AssignFile(FileHandle, FileName);
  Rewrite(FileHandle);
  CloseFile(FileHandle);
end;

{ TTurningPositiveToPositive }

function TTurningPositiveToPositive.Lines: string;
var
  point: TPoint3D;
  codes: TStringList;
  stepPoints, finishPoints: TList<TPoint3D>;
  forwardPoints, backwardPoints: TList<TPoint3D>;
  mStartX, mEndX, radian, scale, centerZOfProjection, toolOffset, x, newX, newZ,
    firstBottomX, firstBottomZ, lastAtFirstBottomX, lastAtFirstBottomZ,
    lastBottomX, lastBottomZ, residual, dX, dZ, angle, radianOfProjection,
    rotatedX, rotatedZ: Single;
  pointCount, zDepthIndex, directionIndex, i, currentIndex: integer;
  reversedFinish, throwEmpty: boolean;
begin
  codes := TStringList.Create;

  result := '';
  throwEmpty := false;
  reversedFinish := false;
  stepPoints := TList<TPoint3D>.Create;
  finishPoints := TList<TPoint3D>.Create;
  forwardPoints := TList<TPoint3D>.Create;
  backwardPoints := TList<TPoint3D>.Create;

  mStartX := StartX;
  mEndX := EndX;

  radian := Math.ArcSin(PartWidth / 2 / LastProjectionRadius);
  scale := 2 * (LastProjectionRadius * Math.Tan(radian)) / PartWidth;
  centerZOfProjection := LastPartRadius - LastProjectionRadius;
  toolOffset := (ToolDiameter / 2) / scale;

  StartX := StartX + toolOffset;
  EndX := EndX - toolOffset;

  pointCount := Math.Floor(Math.Floor((EndX - StartX) / XStep) / 2);
  XStep := (EndX - StartX) / (pointCount * 2);

  for i := 0 to pointCount - 1 do
  begin
    x := (StartX + (i * XStep)) * scale;
    radian := Math.ArcTan2(x, LastProjectionRadius);
    newX := LastProjectionRadius * Sin(radian);
    newZ := centerZOfProjection + LastProjectionRadius * Cos(radian);
    forwardPoints.Add(TPoint3D.Create(newX, newZ, radian * 180 / PI));

    x := (EndX - (i * XStep)) * scale;
    radian := Math.ArcTan2(x, LastProjectionRadius);
    newX := LastProjectionRadius * Sin(radian);
    newZ := centerZOfProjection + LastProjectionRadius * Cos(radian);
    backwardPoints.Insert(0, TPoint3D.Create(newX, newZ, radian * 180 / PI));
  end;

  forwardPoints.Add(TPoint3D.Create(0, LastPartRadius, 0));
  forwardPoints.AddRange(backwardPoints);

  radian := Math.ArcSin(PartWidth / 2 / FirstProjectionRadius);
  scale := 2 * (FirstProjectionRadius * Math.Tan(radian)) / PartWidth;
  centerZOfProjection := FirstPartRadius - FirstProjectionRadius;
  firstBottomX := StartX * scale;
  radian := Math.ArcTan2(firstBottomX, FirstProjectionRadius);
  firstBottomZ := centerZOfProjection + FirstProjectionRadius * Cos(radian);

  radian := Math.ArcSin(PartWidth / 2 / LastProjectionRadius);
  scale := 2 * (LastProjectionRadius * Math.Tan(radian)) / PartWidth;
  centerZOfProjection := FirstPartRadius - LastProjectionRadius;
  lastAtFirstBottomX := StartX * scale;
  radian := Math.ArcTan2(lastAtFirstBottomX, LastProjectionRadius);
  lastAtFirstBottomZ := centerZOfProjection + LastProjectionRadius *
    Cos(radian);

  firstBottomZ := Math.Max(firstBottomZ, lastAtFirstBottomZ);

  radian := Math.ArcSin(PartWidth / 2 / LastProjectionRadius);
  scale := 2 * (LastProjectionRadius * Math.Tan(radian)) / PartWidth;
  centerZOfProjection := LastPartRadius - LastProjectionRadius;
  lastBottomX := StartX * scale;
  radian := Math.ArcTan2(lastBottomX, LastProjectionRadius);
  lastBottomZ := centerZOfProjection + LastProjectionRadius * Cos(radian);

  zDepthIndex := 0;
  residual := firstBottomZ - lastBottomZ;

  if residual < 0 then
    raise Exception.Create('Invalid parameters for turning request!');

  dX := (PartWidth / 2) - DeviationX;
  dZ := DeviationZ;

  if (IsRightSide) then
  begin
    dX := dX * -1;
    dZ := dZ * -1;
  end;

  for currentIndex := 0 to forwardPoints.Count - 1 do
  begin
    angle := forwardPoints[currentIndex].D3;
    radianOfProjection := angle * PI / 180;

    point := TPoint3D.Create(forwardPoints[currentIndex].D1 - dX,
      forwardPoints[currentIndex].D2 + dZ, ArmReferencePosition - angle);

    rotatedX := point.D1 * Cos(radianOfProjection) - point.D2 *
      Sin(radianOfProjection);
    rotatedZ := point.D1 * Sin(radianOfProjection) + point.D2 *
      Cos(radianOfProjection);

    forwardPoints[currentIndex].D1 := rotatedX;
    forwardPoints[currentIndex].D2 := rotatedZ - dZ;

    point.D1 := rotatedX;
    point.D2 := rotatedZ - dZ;

    finishPoints.Add(point);
  end;

  while residual >= 0 do
  begin
    for currentIndex := 0 to forwardPoints.Count - 1 do
    begin
      if zDepthIndex mod 2 = 0 then
        directionIndex := currentIndex
      else
        directionIndex := forwardPoints.Count - currentIndex - 1;

      angle := forwardPoints[directionIndex].D3;

      stepPoints.Add(TPoint3D.Create(forwardPoints[directionIndex].D1,
        forwardPoints[directionIndex].D2 + residual,
        ArmReferencePosition - angle));
    end;

    if residual = 0 then
    begin
      if zDepthIndex mod 2 = 0 then
        reversedFinish := true;

      break;
    end;

    inc(zDepthIndex);
    residual := Math.Max(residual - ZStep, 0);
  end;

  if (stepPoints.Count > 0) and (finishPoints.Count > 0) then
  begin
    codes.Add(TurningHeaderOfAxis);
    codes.Add(MovementHeaderOfAxis);

    codes.Add('G0');
    codes.Add(LetterOfTurningSpeed + SpeedOfTurning.ToString);
    codes.Add(LetterOfSpeed + SpeedOfMovement.ToString);

    codes.Add(LetterOfZ + FormattedValue(stepPoints[0].D2 + SafetyDistance));
    codes.Add(LetterOfX + FormattedValue(stepPoints[0].D1));
    codes.Add(LetterOfArm + FormattedValue(stepPoints[0].D3));

    codes.Add(LetterOfSpeed + SpeedOfApproach.ToString);
    codes.Add(LetterOfZ + FormattedValue(stepPoints[0].D2 + ApproachDistance));

    codes.Add(LetterOfSpeed + SpeedOfMilling.ToString);
    codes.Add('G1');

    for i := 0 to stepPoints.Count - 1 do
    begin
      codes.Add((LetterOfX + FormattedValue(stepPoints[i].D1) + ' ' + LetterOfZ
        + FormattedValue(stepPoints[i].D2) + ' ' + LetterOfArm +
        FormattedValue(stepPoints[i].D3)).Trim);
    end;

    codes.Add(MovementFooterOfAxis);
    codes.Add(TurningFooterOfAxis);
    codes.Add(LetterOfTurningSpeed + SpeedOfFinish.ToString);
    codes.Add(FinishHeaderOfAxis);

    for i := 0 to finishPoints.Count - 1 do
    begin
      if reversedFinish then
        currentIndex := finishPoints.Count - i - 1
      else
        currentIndex := i;

      codes.Add((LetterOfX + FormattedValue(finishPoints[currentIndex].D1) + ' '
        + LetterOfZ + FormattedValue(finishPoints[currentIndex].D2) + ' ' +
        LetterOfArm + FormattedValue(finishPoints[currentIndex].D3)).Trim);
    end;

    codes.Add(FinishFooterOfAxis);

    result := codes.Text;
  end
  else
    throwEmpty := true;

  codes.Free;
  stepPoints.Free;
  finishPoints.Free;
  forwardPoints.Free;
  backwardPoints.Free;

  codes := nil;
  stepPoints := nil;
  finishPoints := nil;
  forwardPoints := nil;
  backwardPoints := nil;

  if throwEmpty then
    raise Exception.Create('Codes couldn''t be created!');
end;

function TTurningPositiveToPositive.GenerateCodes: boolean;
var
  FileHandle: TextFile;
begin
  result := false;

  // validate before generating codes
  Validate;

  EnsureValidated;
  CreateOrClearFile;

  AssignFile(FileHandle, FileName);
  Append(FileHandle);

  Writeln(FileHandle, HeaderOfProgram);
  Writeln(FileHandle, HeaderOfAxis);
  Writeln(FileHandle, HeaderOfTool);
  Writeln(FileHandle, '');

  Writeln(FileHandle, Lines);

  Writeln(FileHandle, FooterOfTool);
  Writeln(FileHandle, FooterOfAxis);
  Writeln(FileHandle, FooterOfProgram);

  CloseFile(FileHandle);

  result := true;
end;

end.
