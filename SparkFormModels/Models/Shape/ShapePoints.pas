unit ShapePoints;

interface

uses Generics.Collections, Shape, Point3D;

type
  TShapePoints = TList<TList<TList<TPoint3D>>>;

  TShapePointsParams = class
  public
    IsDiscrete: Boolean;
  end;

  TShapePointsGroupModel = class
  public
    Data: TShapePoints;
    Params: TShapePointsParams;

    constructor Create;
    destructor Destroy; override;
  end;

  TShapePointsFlattenModel = class
  public
    Data: TList<TPoint3D>;
    Params: TShapePointsParams;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TShapePointsGroupModel }

constructor TShapePointsGroupModel.Create;
begin
  Data := TShapePoints.Create;
  Params := TShapePointsParams.Create;
end;

destructor TShapePointsGroupModel.Destroy;
begin
  if Assigned(Data) then
    Data.Free;
  Data := nil;
  if Assigned(Params) then
    Params.Free;
  Params := nil;

  inherited;
end;

{ TShapePointsFlattenModel }

constructor TShapePointsFlattenModel.Create;
begin
  Data := TList<TPoint3D>.Create;
  Params := TShapePointsParams.Create;
end;

destructor TShapePointsFlattenModel.Destroy;
begin
  if Assigned(Data) then
    Data.Free;
  Data := nil;
  if Assigned(Params) then
    Params.Free;
  Params := nil;

  inherited;
end;

end.
