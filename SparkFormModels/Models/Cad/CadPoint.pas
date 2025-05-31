unit CadPoint;

interface

uses Point2D, Point3D, Shape, ShapeType, PointSplitter, Generics.Collections;

type
  TCadPoint = class(TInterfacedObject, IShape)
  private
    FID: Single;
    FIsDiscrete: Boolean;
    FPosition: TPoint2D;
    FLayer: string;
    FColor: string;
  public
    property Position: TPoint2D read FPosition write FPosition;
    property Layer: string read FLayer write FLayer;
    property Color: string read FColor write FColor;

    function X: Single;
    function Y: Single;

    function GetType: TShapeType;
    function GetDetail: string;
    function GetLayer: string;
    function GetColor: string;

    function GetStart: TPoint2D;
    function GetEnd: TPoint2D;

    procedure SetId(Id: Single);
    function GetId: Single;

    procedure SetIsDiscrete(Value: Boolean);
    function GetIsDiscrete: Boolean;

    function ToPoints(splitter: TPointSplitter): TList<TPoint3D>;

    constructor Create(Position: TPoint2D; Layer, Color: string); overload;
    constructor Create(X, Y: Single; Layer, Color: string); overload;
    destructor Destroy; override;
  end;

implementation

uses SysUtils;

{ TCadPoint }

constructor TCadPoint.Create(Position: TPoint2D; Layer, Color: string);
begin
  FPosition := Position;
  self.Layer := Layer;
  self.Color := Color;
end;

constructor TCadPoint.Create(X, Y: Single; Layer, Color: string);
begin
  FPosition := TPoint2D.Create(X, Y);
  self.Layer := Layer;
  self.Color := Color;
end;

destructor TCadPoint.Destroy;
begin
  if Assigned(FPosition) then
    FPosition.Free;
  FPosition := nil;

  inherited;
end;

function TCadPoint.GetColor: string;
begin
  result := Color;
end;

function TCadPoint.GetDetail: string;
begin
  result := Format('Id: %.f, Layer: %s, Color: %s, X: %.4f, Y: %.4f',
    [FID, Layer, Color, X, Y])
end;

function TCadPoint.GetLayer: string;
begin
  result := Layer;
end;

function TCadPoint.GetStart: TPoint2D;
begin
  result := TPoint2D.Create(X, Y);
end;

function TCadPoint.GetEnd: TPoint2D;
begin
  result := TPoint2D.Create(X, Y);
end;

function TCadPoint.GetType: TShapeType;
begin
  result := stPoint;
end;

function TCadPoint.GetId: Single;
begin
  result := FID;
end;

procedure TCadPoint.SetId(Id: Single);
begin
  FID := Id;
end;

function TCadPoint.GetIsDiscrete: Boolean;
begin
  result := FIsDiscrete;
end;

procedure TCadPoint.SetIsDiscrete(Value: Boolean);
begin
  FIsDiscrete := Value;
end;

function TCadPoint.ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
begin
  if not Assigned(FPosition) then
    raise Exception.Create('Set position before convert to points');

  result := TList<TPoint3D>.Create;
  result.Add(TPoint3D.Create(X, Y, FID));
end;

function TCadPoint.X: Single;
begin
  if not Assigned(FPosition) then
    raise Exception.Create('Set position before reach to X');

  result := FPosition.D1;
end;

function TCadPoint.Y: Single;
begin
  if not Assigned(FPosition) then
    raise Exception.Create('Set position before reach to Y');

  result := FPosition.D2;
end;

end.
