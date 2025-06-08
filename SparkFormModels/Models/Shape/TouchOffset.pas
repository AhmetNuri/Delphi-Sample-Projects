unit TouchOffset;

interface

uses TouchOffsetType;

type
  TTouchOffset = record
  private
    FOffsetType: TTouchOffsetType;
  public
    OffsetValue: Single;

    property OffsetType: TTouchOffsetType read FOffsetType;

    procedure SwitchTypeToNone;
    procedure SwitchTypeToDynamic;
    procedure SwitchTypeToStatic;
  end;

implementation

{ TTouchOffset }

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
