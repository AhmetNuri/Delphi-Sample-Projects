unit LengthOfTemplate;

interface

type
  TLengthOfTemplate = class
  public
    Value: Single;

    constructor Create(ValueIn: Single);
  end;

implementation

{ TLengthOfTemplate }

constructor TLengthOfTemplate.Create(ValueIn: Single);
begin
  Value := ValueIn;
end;

end.
