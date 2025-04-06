unit Mirror;

interface

type
  TMirror = class
  public
    OnX: Boolean;
    OnY: Boolean;
    OnMixMax: Boolean; // kendi i�inde �evir
                      // false �ablonda  �evir
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
