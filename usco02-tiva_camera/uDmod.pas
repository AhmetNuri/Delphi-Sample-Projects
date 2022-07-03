unit uDmod;

interface

uses
  System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, FireDAC.VCLUI.Wait, Data.DB,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
  FireDAC.DApt, FireDAC.Comp.DataSet, FireDAC.Stan.StorageBin;

type
  TfrmDmod = class(TDataModule)
    FDConnection1: TFDConnection;
    qCreateTables: TFDQuery;
    qSENARYO: TFDQuery;
    qDATAS: TFDQuery;
    qSENARYOSENARYO_ID: TFDAutoIncField;
    qSENARYOAD: TStringField;
    qSENARYOACIKLAMA: TStringField;
    dsDATAS: TDataSource;
    dsSENARYO: TDataSource;
    qDATASDATA_ID: TFDAutoIncField;
    qDATASDATA_SENRYA_ID: TIntegerField;
    qDATASAD: TStringField;
    qIO: TFDQuery;
    qIOIO_ID: TFDAutoIncField;
    qIOIO_AD: TStringField;
    qIOIO_NO: TSmallintField;
    dsIO: TDataSource;
    qDATASCIHAZ: TStringField;
    qDATASPIN: TSmallintField;
    memDatas: TFDMemTable;
    memDatasDATA_ID: TFDAutoIncField;
    memDatasDATA_SENRYA_ID: TIntegerField;
    memDatasAD: TStringField;
    qSENARYOAKTIF: TBooleanField;
    qSENARYOSIRA: TIntegerField;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDmod: TfrmDmod;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

end.
