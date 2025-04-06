unit TouchOffset;

interface

uses TouchOffsetType;

type
  TTouchOffset = class
  private
    FOffsetType: TTouchOffsetType;
  public
    OffsetValue: Single;

    function OffsetType: TTouchOffsetType;

    procedure SwitchTypeToDynamic;
    procedure SwitchTypeToStatic;

    constructor Create;
  end;

implementation

{ TTouchOffset }

constructor TTouchOffset.Create;
begin
  FOffsetType := totDynamic;
  OffsetValue := 0;
end;

function TTouchOffset.OffsetType: TTouchOffsetType;
begin
  result := FOffsetType;
end;

procedure TTouchOffset.SwitchTypeToDynamic;
begin
  FOffsetType := totDynamic;
end;

procedure TTouchOffset.SwitchTypeToStatic;
begin
  FOffsetType := totStatic;
end;

end.
