unit Crop;

interface

type
  TCropX = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TCropY = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TCrop = record
  public
    OnX: TCropX;
    OnY: TCropY;
  end;

implementation

uses SysUtils;

{ TCropX }

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

end.
