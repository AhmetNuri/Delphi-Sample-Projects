unit Crop;

interface

type
  TCropX = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    property PercentFrom: Single read FPercentFrom;
    property PercentTo: Single read FPercentTo;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TCropY = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    property PercentFrom: Single read FPercentFrom;
    property PercentTo: Single read FPercentTo;

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

procedure TCropX.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of area restriction');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

{ TCropY }

procedure TCropY.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of area restriction');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

end.
