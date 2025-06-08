unit CamBlock;

interface

uses CamAxis, CamPair, CamTool, Point2D, Point6D, Generics.Collections,
  CamLetter, CamType;

type
  TCamBlock = class(TCamPair)
  private
    FAxis: TCamAxis;
  public
    property Axis: TCamAxis read FAxis;

    procedure SetAxis(Axis: TCamAxis);
    procedure SetPoints(points: TList < TList < TPoint6D >> );
    procedure SetShapePoints(points: TList < TList < TPoint2D >> );
    procedure SetLetters(Letters: TDictionary<TCamLetter, TCamLetterVariable>);
    procedure SwitchCamType(CamType: TCamType);

    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TCamBlock }

constructor TCamBlock.Create;
begin
  FAxis := TCamAxis.Create;
end;

destructor TCamBlock.Destroy;
begin
  if Assigned(FAxis) then
  begin
    FAxis.Free;
    FAxis := nil;
  end;

  inherited;
end;

procedure TCamBlock.SetAxis(Axis: TCamAxis);
begin
  if not Assigned(Axis) then
    raise Exception.Create('Assign axis before set!')
  else if Axis.GetType = '' then
    raise Exception.Create('Invalid axis type!');

  FAxis := Axis;
end;

procedure TCamBlock.SetLetters(Letters: TDictionary<TCamLetter,
  TCamLetterVariable>);
begin
  if not Assigned(Axis) then
    raise Exception.Create('Assign axis before setting letters!');

  Axis.SetLetters(Letters);
end;

procedure TCamBlock.SetPoints(points: TList < TList < TPoint6D >> );
begin
  if not Assigned(Axis) then
    raise Exception.Create('Assign axis before setting points!');

  Axis.SetPoints(points);
end;

procedure TCamBlock.SetShapePoints(points: TList < TList < TPoint2D >> );
begin
  if not Assigned(Axis) then
    raise Exception.Create('Assign axis before setting points!');

  Axis.SetShapePoints(points);
end;

procedure TCamBlock.SwitchCamType(CamType: TCamType);
begin
  if not Assigned(Axis) then
    raise Exception.Create('Assign axis before setting cam type!');

  Axis.SwitchCamType(CamType);
end;

end.
