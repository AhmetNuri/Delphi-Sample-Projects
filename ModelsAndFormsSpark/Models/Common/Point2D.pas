unit Point2D;

interface

type
  TPoint2D = class
  private
    FD1, FD2: Single;
  public
    property D1: Single read FD1 write FD1;
    property D2: Single read FD2 write FD2;

    constructor Create(D1, D2: Single);
  end;

implementation

{ TPoint2D }

constructor TPoint2D.Create(D1, D2: Single);
begin
  FD1 := D1;
  FD2 := D2;
end;

end.
