unit Point4D;

interface

type
  TPoint4D = class
  private
    FD1, FD2, FD3, FD4: Single;
  public
    property D1: Single read FD1 write FD1;
    property D2: Single read FD2 write FD2;
    property D3: Single read FD3 write FD3;
    property D4: Single read FD4 write FD4;

    constructor Create(D1, D2, D3, D4: Single);
  end;

implementation

{ TPoint4D }

constructor TPoint4D.Create(D1, D2, D3, D4: Single);
begin
  FD1 := D1;
  FD2 := D2;
  FD3 := D3;
  FD4 := D4;
end;

end.
