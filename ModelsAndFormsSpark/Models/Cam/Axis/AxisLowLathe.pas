unit AxisLowLathe;

interface

uses CamAxis, Classes;

type
  TAxisLowLathe = class(TCamAxis)
  public
    function GenerateCode: TStringList; override;
    function GetType: String; override;
  end;

implementation

{ TAxisLowLathe }

function TAxisLowLathe.GenerateCode: TStringList;
begin
  result := TStringList.Create;
end;

function TAxisLowLathe.GetType: String;
begin
  result := 'LowLathe';
end;

end.
