unit Stretch;

interface

uses StretchMode;

type
  TStretchX = class
  private
    FPercentFrom: Single;  // yuzdelik aland  strech seçim baþlangýç
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);

    constructor Create;
  end;

  TStretchY = class
  private
    FPercentFrom: Single;
    FPercentTo: Single;
  public
    function PercentFrom: Single;
    function PercentTo: Single;

    procedure SetArea(PercentFrom, PercentTo: Single);

    constructor Create;
  end;

  TStretch = class
  private
    FMode: TStretchMode;
  public
    OnX: TStretchX; // strech x mi
    OnY: TStretchY;

    function Mode: TStretchMode;

    procedure SwitchModeToTemplate; // þekildeki boyut oranýna göre mi ? þablon mu þekilmi strech edilecek?
    procedure SwitchModeToMinMax;  // Þeklin kendisini seçilen alana strech et
    procedure SwitchModeToNone;   // strech yok

    constructor Create;
  end;

implementation

uses SysUtils;

{ TStretch }

constructor TStretch.Create;
begin
  FMode := stNone;
  OnX := TStretchX.Create;
  OnY := TStretchY.Create;
end;

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

constructor TStretchX.Create;
begin
  FPercentFrom := 0;
  FPercentTo := 100;
end;

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

constructor TStretchY.Create;
begin
  FPercentFrom := 0;
  FPercentTo := 100;
end;

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
