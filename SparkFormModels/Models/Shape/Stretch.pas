unit Stretch;

interface

uses StretchMode;

type
  TStretchX = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    property PercentFrom: Single read FPercentFrom;
    property PercentTo: Single read FPercentTo;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TStretchY = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    property PercentFrom: Single read FPercentFrom;
    property PercentTo: Single read FPercentTo;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TStretch = record
  private
    FMode: TStretchMode;
  public
    OnX: TStretchX;
    OnY: TStretchY;

    property Mode: TStretchMode read FMode;

    procedure SwitchModeToTemplate;
    procedure SwitchModeToMinMax;
    procedure SwitchModeToNone;
  end;

implementation

uses SysUtils;

{ TStretch }

procedure TStretch.SwitchModeToMinMax;
begin
  FMode := stMinMax;
end;

procedure TStretch.SwitchModeToNone;
begin
  FMode := stNone;
end;

procedure TStretch.SwitchModeToTemplate;
begin
  FMode := stTemplate;
end;

{ TStretchX }

procedure TStretchX.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of stretch');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

{ TStretchY }

procedure TStretchY.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of stretch');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

end.
