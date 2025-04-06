unit Move;

interface

type
  TMove = class
  public
    OnX: Single; // x �teleme
    OnY: Single; // y �teleme + -
    OnStrictArea: Boolean;  // true -> �ablonu a�maz
                            // false  �ablondan ��kan k�s�m silinir
    constructor Create;
  end;

implementation

{ TMove }

constructor TMove.Create;
begin
  OnX := 0;
  OnY := 0;
  OnStrictArea := false;
end;

end.
