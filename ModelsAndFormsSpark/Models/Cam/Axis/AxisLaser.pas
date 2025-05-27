unit AxisLaser;

interface

uses CamAxis, Classes, CamPair, CamType, CamLetter, Generics.Collections;

type
  TAxisLaser = class(TCamAxis)
  public
    function GenerateCode: TStringList; override;
    function GetType: String; override;
  end;

implementation

{ TAxisLaser }

function TAxisLaser.GenerateCode: TStringList;
begin
  result := TStringList.Create;

  result.Add(FindProperty(Keys.Header).AsLines.Text);
  result.Add(Tool.FindProperty(Keys.Header).AsLines.Text);

  AxisLetters := TList<TCamLetter>.Create;

  // add all current cam types
  AxisLetters.Add(clXOfCnc);

  if CamType in [ctHPNR, ctHPHR] then
    AxisLetters.Add(clYOfCnc);

  // add all current cam types
  AxisLetters.Add(clZOfCnc);

  if CamType in [ctVPA, ctVPC, ctVPS, ctHPFR] then
    AxisLetters.Add(clDivisor);

  // add all current cam types
  AxisLetters.Add(clArm);

  result.AddStrings(GenerateAxisCode);

  result.Add(Tool.FindProperty(Keys.Footer).AsLines.Text);
  result.Add(FindProperty(Keys.Footer).AsLines.Text);
end;

function TAxisLaser.GetType: String;
begin
  result := 'Laser';
end;

end.
