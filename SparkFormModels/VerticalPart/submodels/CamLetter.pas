unit CamLetter;

interface

type
  TCamLetter = (clXOfCnc, clYOfCnc, clZOfCnc, clDivisor, clArm, clAxisC,
    clAxisU, clF);

  TCamLetterVariable = class
  private
    FLetter: string;
    FDirection: integer;
    FIsLinear: boolean;
  public
    property Letter: string read FLetter;
    property Direction: integer read FDirection;
    property IsLinear: boolean read FIsLinear;

    constructor Create(Letter: string; Direction: integer; IsLinear: boolean);

    procedure SetDirection(Direction: integer);
    procedure SetLetter(Letter: string);
  end;

  TCamLetterLinearVariable = class(TCamLetterVariable)
  public
    constructor Create(Letter: string; Direction: integer);
  end;

  TCamLetterCircularVariable = class(TCamLetterVariable)
  public
    constructor Create(Letter: string; Direction: integer);
  end;

implementation

uses SysUtils;

{ TCamLetterVariable }

constructor TCamLetterVariable.Create(Letter: string; Direction: integer;
  IsLinear: boolean);
begin
  if (Direction <> 1) and (Direction <> -1) then
    raise Exception.Create('Directions must be one of (1, -1)!')
  else if Letter.Trim = '' then
    raise Exception.Create('Letter cant be empty!');

  FLetter := Letter;
  FDirection := Direction;
  FIsLinear := IsLinear;
end;

procedure TCamLetterVariable.SetDirection(Direction: integer);
begin
  if (Direction <> 1) and (Direction <> -1) then
    raise Exception.Create('Directions must be one of (1, -1)!');

  FDirection := Direction;
end;

procedure TCamLetterVariable.SetLetter(Letter: string);
begin
  if Letter.Trim = '' then
    raise Exception.Create('Letter cant be empty!');

  FLetter := Letter;
end;

{ TCamLetterLinearVariable }

constructor TCamLetterLinearVariable.Create(Letter: string; Direction: integer);
begin
  inherited Create(Letter, Direction, true);
end;

{ TCamLetterCircularVariable }

constructor TCamLetterCircularVariable.Create(Letter: string;
  Direction: integer);
begin
  inherited Create(Letter, Direction, false);
end;

end.
