unit TouchOffset;

interface

uses TouchOffsetType;

type
  TTouchOffset = record
  private
    FOffsetType: TTouchOffsetType;
  public
    OffsetValue: Single;

    function OffsetType: TTouchOffsetType;

    procedure SwitchTypeToNone;
    procedure SwitchTypeToDynamic;
    procedure SwitchTypeToStatic;
  end;

implementation

{ TTouchOffset }

function TTouchOffset.OffsetType: TTouchOffsetType;
begin
  result := FOffsetType;
end;

procedure TTouchOffset.SwitchTypeToDynamic;
begin
  FOffsetType := totDynamic;
end;

procedure TTouchOffset.SwitchTypeToNone;
begin
  FOffsetType := totNone;
end;

procedure TTouchOffset.SwitchTypeToStatic;
begin
  FOffsetType := totStatic;
end;

end.
