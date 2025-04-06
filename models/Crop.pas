unit Crop;

interface

type
  TCropX = class
  private
    FPercentFrom: Single;    //  x hangi yüzdelilk alanda baþlangýç kesilecek
    FPercentTo: Single;       // x  hangi yüzdelilk alanda bitiþ kesilecek
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);

    constructor Create;
  end;

  TCropY = class
  private
    FPercentFrom: Single; // y hangi yüzdelilk alanda baþlangýç kesilecek
    FPercentTo: Single;    // y hangi yüzdelilk alanda bitiþ kesilecek
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);

    constructor Create;
  end;

  TCrop = class
  public
    OnX: TCropX;
    OnY: TCropY;

    constructor Create;
  end;

implementation

uses SysUtils;

{ TCropX }

constructor TCropX.Create;
begin
  FPercentFrom := 0;
  FPercentTo := 100;
end;

function TCropX.PercentFrom: Single;
begin
  result := FPercentFrom;
end;

function TCropX.PercentTo: Single;
begin
  result := FPercentTo;
end;

procedure TCropX.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of area restriction');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

{ TCropY }

constructor TCropY.Create;
begin
  FPercentFrom := 0;
  FPercentTo := 100;
end;

function TCropY.PercentFrom: Single;
begin
  result := FPercentFrom;
end;

function TCropY.PercentTo: Single;
begin
  result := FPercentTo;
end;

procedure TCropY.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of area restriction');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

{ TCrop }

constructor TCrop.Create;
begin
  OnX := TCropX.Create;
  OnY := TCropY.Create;
end;

end.
