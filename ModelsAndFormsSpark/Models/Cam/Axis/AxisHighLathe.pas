unit AxisHighLathe;

interface

uses CamAxis, Classes;

type
  TAxisHighLathe = class(TCamAxis)
  public
    function GenerateCode: TStringList; override;
    function GetType: String; override;
  end;

implementation

{ TAxisHighLathe }

function TAxisHighLathe.GenerateCode: TStringList;
begin
  result := TStringList.Create;
end;

function TAxisHighLathe.GetType: String;
begin
  result := 'HighLathe';
end;

end.
