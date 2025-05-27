unit Point5D;

interface

type
  TPoint5D = class
  private
    FD1, FD2, FD3, FD4, FD5: Single;
  public
    property D1: Single read FD1 write FD1;
    property D2: Single read FD2 write FD2;
    property D3: Single read FD3 write FD3;
    property D4: Single read FD4 write FD4;
    property D5: Single read FD5 write FD5;

    constructor Create(D1, D2, D3, D4, D5: Single);
  end;

implementation

{ TPoint4D }

constructor TPoint5D.Create(D1, D2, D3, D4, D5: Single);
begin
  FD1 := D1;
  FD2 := D2;
  FD3 := D3;
  FD4 := D4;
  FD5 := D5;
end;

end.
