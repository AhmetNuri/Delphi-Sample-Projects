unit Shape;

interface

uses ShapeType, PointSplitter, Point2D, Point3D, Generics.Collections;

type
  IShape = interface
    ['{46BCF089-A482-420F-BE8D-B40C66EFF729}']
    function GetType: TShapeType;
    function GetDetail: string;
    function GetLayer: string;
    function GetColor: string;

    // get shape positions of start and end
    function GetStart: TPoint2D;
    function GetEnd: TPoint2D;

    // group id of shape types
    procedure SetId(Id: Single);
    function GetId: Single;

    // group has discrete points
    procedure SetIsDiscrete(Value: Boolean);
    function GetIsDiscrete: Boolean;

    function ToPoints(splitter: TPointSplitter): TList<TPoint3D>;
  end;

implementation

end.
