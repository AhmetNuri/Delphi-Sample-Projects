unit CadPair;

interface

type
  TCadPair = class
  private
    FCode: Integer;
    FValue: string;
  public
    constructor Create(Code: Integer; const Value: string);
    property Code: Integer read FCode write FCode;
    property Value: string read FValue write FValue;
  end;

implementation

{ TCadPair }

constructor TCadPair.Create(Code: Integer; const Value: string);
begin
  FCode := Code;
  FValue := Value;
end;

end.
