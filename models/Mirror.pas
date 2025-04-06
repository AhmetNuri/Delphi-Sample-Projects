unit Mirror;

interface

type
  TMirror = class
  public
    OnX: Boolean;
    OnY: Boolean;
    OnMixMax: Boolean; // kendi içinde çevir
                      // false þablonda  çevir
    constructor Create;
  end;

implementation

{ TMirror }

constructor TMirror.Create;
begin
  OnX := false;
  OnY := false;
  OnMixMax := false;
end;

end.
