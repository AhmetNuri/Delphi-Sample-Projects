unit CadModel;

interface

uses Generics.Collections, Shape, CadShapes, PointSplitter, ShapePoints,
  Point3D;

type
  TCadModel = class
  private
    FColors: TList<string>;
    FLayers: TList<string>;
    FShapes: TCadShapes;
  public
    property Colors: TList<string> read FColors write FColors;
    property Layers: TList<string> read FLayers write FLayers;
    property Shapes: TCadShapes read FShapes write FShapes;

    procedure AddShape(item: IShape);

    function ToGroupPoints(splitter: TPointSplitter): TShapePointsGroupModel;
    function ToFlattenPoints(splitter: TPointSplitter): TShapePointsFlattenModel;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TCadModel }

uses CadHelpers;

procedure TCadModel.AddShape(item: IShape);
begin
  if not Assigned(FShapes) then
    FShapes := TCadShapes.Create;
  if FShapes.Count = 0 then
    FShapes.Add(TList<IShape>.Create);

  if FShapes[FShapes.Count - 1].Count > 0 then
  begin
    with FShapes[FShapes.Count - 1][FShapes[FShapes.Count - 1].Count - 1] do
      if GetType <> item.GetType then
        FShapes.Add(TList<IShape>.Create);
  end;

  if not FLayers.Contains(item.GetLayer) then
    FLayers.Add(item.GetLayer);
  if not FColors.Contains(item.GetColor) then
    FColors.Add(item.GetColor);

  FShapes.Append(item);
end;

constructor TCadModel.Create;
begin
  FColors := TList<string>.Create;
  FLayers := TList<string>.Create;
  FShapes := TCadShapes.Create;
end;

destructor TCadModel.Destroy;
var
  I: integer;
begin
  if Assigned(FShapes) then
  begin
    if FShapes.Count > 0 then
    begin
      for I := 0 to FShapes.Count - 1 do
      begin
        if Assigned(FShapes[I]) then
        begin
          FShapes[I].Free;
          FShapes[I] := nil;
        end;
      end;
    end;
    FShapes.Free;
    FShapes := nil;
  end;

  if Assigned(FColors) then
    FColors.Free;
  FColors := nil;

  if Assigned(FLayers) then
    FLayers.Free;
  FLayers := nil;

  inherited;
end;

function TCadModel.ToGroupPoints(splitter: TPointSplitter): TShapePointsGroupModel;
var
  I, J: integer;
begin
  result := TShapePointsGroupModel.Create;
  result.Params.IsDiscrete := splitter.IsDiscrete;

  if Assigned(FShapes) then
  begin
    for I := 0 to FShapes.Count - 1 do
    begin
      if Assigned(FShapes[I]) then
      begin
        if FShapes[I].Count > 0 then
        begin
          with result do
          begin
            Data.Add(TList < TList < TPoint3D >>.Create);

            for J := 0 to FShapes[I].Count - 1 do
            begin
              FShapes[I][J].SetIsDiscrete(splitter.IsDiscrete);
              Data[Data.Count - 1].Add(FShapes[I][J].ToPoints(splitter));
            end;
          end;
        end;
      end;
    end;
  end;
end;

function TCadModel.ToFlattenPoints(splitter: TPointSplitter): TShapePointsFlattenModel;
var
  I, J: integer;
begin
  result := TShapePointsFlattenModel.Create;
  result.Params.IsDiscrete := splitter.IsDiscrete;

  if Assigned(FShapes) then
  begin
    for I := 0 to FShapes.Count - 1 do
    begin
      if Assigned(FShapes[I]) then
      begin
        if FShapes[I].Count > 0 then
        begin
          with result do
          begin
            for J := 0 to FShapes[I].Count - 1 do
            begin
              FShapes[I][J].SetIsDiscrete(splitter.IsDiscrete);
              Data.AddRange(FShapes[I][J].ToPoints(splitter));
            end;
          end;
        end;
      end;
    end;
  end;
end;

end.
