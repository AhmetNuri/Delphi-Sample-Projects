unit Point3D;

interface

type
  TPoint3D = class
  private
    FD1, FD2, FD3: Single;
  public
    property D1: Single read FD1 write FD1;
    property D2: Single read FD2 write FD2;
    property D3: Single read FD3 write FD3;

    constructor Create(D1, D2, D3: Single);
  end;

implementation

{ TPoint3D }

constructor TPoint3D.Create(D1, D2, D3: Single);
begin
  FD1 := D1;
  FD2 := D2;
  FD3 := D3;
end;

end.
