unit CadVertex;

interface

uses Point2D;

type
  TCadVertex = class
  public
    Position: TPoint2D;
    Bulge: Single;
    Layer: string;

    function X: Single;
    function Y: Single;

    constructor Create(Location: TPoint2D; Bulge: Single = 0;
      Layer: string = '0'); overload;
    constructor Create(X, Y, Bulge: Single; Layer: string = '0'); overload;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TCadVertex }

constructor TCadVertex.Create(Location: TPoint2D; Bulge: Single; Layer: string);
begin
  self.Position := Location;
  self.Bulge := Bulge;
  self.Layer := Layer;
end;

constructor TCadVertex.Create(X, Y, Bulge: Single; Layer: string);
begin
  self.Position := TPoint2D.Create(X, Y);
  self.Bulge := Bulge;
  self.Layer := Layer;
end;

destructor TCadVertex.Destroy;
begin
  if Assigned(Position) then
    Position.Free;
  Position := nil;

  inherited;
end;

function TCadVertex.X: Single;
begin
  if not Assigned(Position) then
    raise Exception.Create('Set location before reach to X');

  result := Position.D1;
end;

function TCadVertex.Y: Single;
begin
  if not Assigned(Position) then
    raise Exception.Create('Set location before reach to Y');

  result := Position.D2;
end;

end.
