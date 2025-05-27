unit Stretch;

interface

uses StretchMode;

type
  TStretchX = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TStretchY = record
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);
  end;

  TStretch = record
  private
    FMode: TStretchMode;
  public
    OnX: TStretchX;
    OnY: TStretchY;

    function Mode: TStretchMode;

    procedure SwitchModeToTemplate;
    procedure SwitchModeToMinMax;
    procedure SwitchModeToNone;
  end;

implementation

uses SysUtils;

{ TStretch }

function TStretch.Mode: TStretchMode;
begin
  result := FMode;
end;

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

function TStretchX.PercentFrom: Single;
begin
  result := FPercentFrom;
end;

function TStretchX.PercentTo: Single;
begin
  result := FPercentTo;
end;

procedure TStretchX.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of stretch');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

{ TStretchY }

function TStretchY.PercentFrom: Single;
begin
  result := FPercentFrom;
end;

function TStretchY.PercentTo: Single;
begin
  result := FPercentTo;
end;

procedure TStretchY.SetArea(PercentFrom, PercentTo: Single);
begin
  if (PercentFrom < 0) or (PercentTo > 100) or (PercentFrom >= PercentTo) then
    raise Exception.Create('Percentage error of stretch');

  FPercentFrom := PercentFrom;
  FPercentTo := PercentTo;
end;

end.
