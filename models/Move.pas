unit Move;

interface

type
  TMove = class
  public
    OnX: Single; // x öteleme
    OnY: Single; // y öteleme + -
    OnStrictArea: Boolean;  // true -> þablonu aþmaz
                            // false  þablondan çýkan kýsým silinir
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
