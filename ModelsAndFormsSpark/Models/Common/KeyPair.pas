unit KeyPair;

interface

uses Classes;

type
  TKeyPair = class
  private
    FKey, FValue: string;
  public
    property Key: string read FKey;
    property Value: string read FValue;

    function AsInt: integer;
    function AsSingle: single;
    function AsBoolean: Boolean;
    function AsIntDef(default: integer): integer;
    function AsSingleDef(default: single): single;
    function AsBooleanDef(default: Boolean): Boolean;
    function AsLines: TStringList;

    procedure SetValue(Value: string);
    constructor Create(Key, Value: string);
  end;

implementation

uses StrUtils, SysUtils;

{ TKeyPair }

function TKeyPair.AsBoolean: Boolean;
begin
  result := StrToBool(Value);
end;

function TKeyPair.AsBooleanDef(default: Boolean): Boolean;
begin
  result := StrToBoolDef(Value, default);
end;

function TKeyPair.AsInt: integer;
begin
  result := StrToInt(Value);
end;

function TKeyPair.AsIntDef(default: integer): integer;
begin
  result := StrToIntDef(Value, default);
end;

function TKeyPair.AsSingle: single;
begin
  result := StrToFloat(Value);
end;

function TKeyPair.AsSingleDef(default: single): single;
begin
  result := StrToFloatDef(Value, default);
end;

function TKeyPair.AsLines: TStringList;
begin
  result := TStringList.Create;
  result.Text := StringReplace(Value, '\n', #10, [rfReplaceAll]);
end;

constructor TKeyPair.Create(Key, Value: string);
begin
  FKey := Key;
  FValue := Value;
end;

procedure TKeyPair.SetValue(Value: string);
begin
  FValue := Value;
end;

end.
