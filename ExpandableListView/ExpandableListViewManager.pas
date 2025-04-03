unit ExpandableListViewManager;

interface

uses
  System.SysUtils, System.Classes, System.JSON, FMX.Forms, FMX.Types,
  FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.Stan.Async, FireDAC.DApt,
  FireDAC.Stan.Def, FireDAC.Stan.Pool, FireDAC.UI.Intf, FireDAC.Stan.Intf,
  FireDAC.Stan.Error, FireDAC.Phys.Intf, FireDAC.Phys, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.Phys.SQLiteWrapper.Stat,
  ExpandableListView, DBHelper,  System.UITypes ,ExpandableListViewJSON ,
FMX.Text,   Math,FMX.Edit,    FMX.NumberBox, FMX.Layouts,
  FMX.StdCtrls, FMX.Ani, FMX.ListBox, System.Generics.Collections, FMX.Colors,
  StrUtils, FMX.DateTimeCtrls, FMX.Memo, FMX.Calendar
  ;

type
  TExpandableListViewManager = class
  private
    FExpandableListView: TExpandableListView;
    FDBHelper: TDBHelper;
    FJSONHelper: TExpandableListViewJSONHelper;
    FConnection: TFDConnection;
    FLogEnabled: Boolean;
    FLogPath: string;

    procedure InitializeConnection;
    procedure SetupLogOptions;

  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    // Veritabanı işlemleri
    function SaveListViewToDatabase(const AListViewName: string = ''): Boolean;
    function LoadListViewFromDatabase(const AListViewName: string = ''): Boolean;
    function ExportToJSON: string;

    // JSON işlemleri
    function LoadFromJSON(const AJSON: string): Boolean;
    function LoadFromJSONFile(const AFileName: string): Boolean;
    function SaveToJSONFile(const AFileName: string): Boolean;

    // ListView işlemleri
    function AddHeader(const ATitle: string; AImageIndex: Integer; AColor: TAlphaColor): THeaderInfo;
    function AddEditField(AHeaderInfo: THeaderInfo; const AFieldName, AValue: string): TEdit;
    function AddNumberField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AValue: Double; AMin: Double = 0; AMax: Double = 100;
      AValueType: TNumberValueType = nvtFloat; AVertIncrement: Boolean = False): TNumberBox;
    function AddCheckBoxField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AChecked: Boolean = False): TCheckBox;
    function AddComboBoxField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AItems: array of string; ASelectedIndex: Integer = 0): TComboBox;
    function AddColorBoxField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      ASelectedColor: TAlphaColor = TAlphaColorRec.Blue): TColorComboBox;
    function AddRadioButtonField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AValue: Boolean; const AGroupName: string = 'DefaultRadioGroup'): TRadioButton;

    // Header işlemleri
    procedure SetHeaderSVG(AHeaderInfo: THeaderInfo; const ASVGData: string);
    procedure ExpandHeader(AHeaderInfo: THeaderInfo);
    procedure CollapseHeader(AHeaderInfo: THeaderInfo);
    procedure ExpandAll;
    procedure CollapseAll;

    // Properties
    property ExpandableListView: TExpandableListView read FExpandableListView;
    property DBHelper: TDBHelper read FDBHelper;
    property JSONHelper: TExpandableListViewJSONHelper read FJSONHelper;
    property LogEnabled: Boolean read FLogEnabled write FLogEnabled;
    property LogPath: string read FLogPath write FLogPath;
  end;

implementation

{ TExpandableListViewManager }

constructor TExpandableListViewManager.Create(AOwner: TComponent);
begin
  inherited Create;

  // ExpandableListView oluştur
  FExpandableListView := TExpandableListView.Create(AOwner);

  // Veritabanı bağlantısını hazırla
  InitializeConnection;

  // DBHelper oluştur
  FDBHelper := TDBHelper.Create(FConnection);

  // JSON Helper oluştur
  FJSONHelper := TExpandableListViewJSONHelper.Create(FExpandableListView);

  // Log ayarlarını yap
  SetupLogOptions;
end;

destructor TExpandableListViewManager.Destroy;
begin
  // Önce yardımcı sınıfları temizle
  if Assigned(FJSONHelper) then
    FreeAndNil(FJSONHelper);

  if Assigned(FDBHelper) then
    FreeAndNil(FDBHelper);

  if Assigned(FConnection) then
  begin
    FConnection.Close;
    FreeAndNil(FConnection);
  end;

  // En son UI bileşenini temizle
  if Assigned(FExpandableListView) then
    FreeAndNil(FExpandableListView);

  inherited;
end;

procedure TExpandableListViewManager.InitializeConnection;
begin
  FConnection := TFDConnection.Create(nil);

  // SQLite bağlantı ayarları
  FConnection.DriverName := 'SQLite';
  FConnection.Params.Database := 'expandablelistview.db'; // Varsayılan db adı
  FConnection.LoginPrompt := False;

  try
    FConnection.Connected := True;
  except
    on E: Exception do
      raise Exception.Create('Veritabanı bağlantı hatası: ' + E.Message);
  end;
end;

procedure TExpandableListViewManager.SetupLogOptions;
begin
  // Varsayılan log ayarları
  FLogEnabled := False;
  FLogPath := 'ExpandableListView.log';

  // Log özelliklerini component'lere aktar
  FExpandableListView.EnableLogging := FLogEnabled;
  FExpandableListView.LogFilePath := FLogPath;
end;

// Veritabanı işlemleri
function TExpandableListViewManager.SaveListViewToDatabase(const AListViewName: string = ''): Boolean;
var
  JSONData: string;
begin
  Result := False;
  try
    // View'ı JSON'a dönüştür
    JSONData := FJSONHelper.ExportToJSON;

    // JSON'ı veritabanına kaydet
    FDBHelper.JSONToDatabase(JSONData, AListViewName);
    Result := True;
  except
    on E: Exception do
      raise Exception.Create('Veritabanına kayıt hatası: ' + E.Message);
  end;
end;

function TExpandableListViewManager.LoadListViewFromDatabase(const AListViewName: string = ''): Boolean;
var
  JSONData: string;
begin
  Result := False;
  try
    // Veritabanından JSON verisi al
    JSONData := FDBHelper.DatabaseToJSON(AListViewName);

    // JSON'ı ListView'a yükle
    Result := FJSONHelper.LoadFromJSON(JSONData);
  except
    on E: Exception do
      raise Exception.Create('Veritabanından okuma hatası: ' + E.Message);
  end;
end;

// JSON işlemleri
function TExpandableListViewManager.ExportToJSON: string;
begin
  Result := FJSONHelper.ExportToJSON;
end;

function TExpandableListViewManager.LoadFromJSON(const AJSON: string): Boolean;
begin
  Result := FJSONHelper.LoadFromJSON(AJSON);
end;

function TExpandableListViewManager.LoadFromJSONFile(const AFileName: string): Boolean;
begin
  Result := FJSONHelper.LoadFromJSONFile(AFileName);
end;

function TExpandableListViewManager.SaveToJSONFile(const AFileName: string): Boolean;
var
  JSONData: string;
begin
  Result := False;
  try
    JSONData := ExportToJSON;
    with TStringList.Create do
    try
      Text := JSONData;
      SaveToFile(AFileName, TEncoding.UTF8);
      Result := True;
    finally
      Free;
    end;
  except
    // Hata durumunda False döner
  end;
end;

// ListView işlemleri
function TExpandableListViewManager.AddHeader(const ATitle: string; AImageIndex: Integer;
  AColor: TAlphaColor): THeaderInfo;
begin
  Result := FExpandableListView.AddHeader(ATitle, AImageIndex, AColor);
end;

function TExpandableListViewManager.AddEditField(AHeaderInfo: THeaderInfo;
  const AFieldName, AValue: string): TEdit;
begin
  Result := FExpandableListView.AddEditField(AHeaderInfo, AFieldName, AValue);
end;

function TExpandableListViewManager.AddNumberField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; AValue: Double; AMin: Double = 0; AMax: Double = 100;
  AValueType: TNumberValueType = nvtFloat; AVertIncrement: Boolean = False): TNumberBox;
begin
  Result := FExpandableListView.AddNumberField(AHeaderInfo, AFieldName, AValue,
    AMin, AMax, AValueType, AVertIncrement);
end;

function TExpandableListViewManager.AddCheckBoxField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; AChecked: Boolean = False): TCheckBox;
begin
  Result := FExpandableListView.AddCheckBoxField(AHeaderInfo, AFieldName, AChecked);
end;

function TExpandableListViewManager.AddComboBoxField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; AItems: array of string;
  ASelectedIndex: Integer = 0): TComboBox;
begin
  Result := FExpandableListView.AddComboBoxField(AHeaderInfo, AFieldName, AItems, ASelectedIndex);
end;

function TExpandableListViewManager.AddColorBoxField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; ASelectedColor: TAlphaColor = TAlphaColorRec.Blue): TColorComboBox;
begin
  Result := FExpandableListView.AddColorBoxField(AHeaderInfo, AFieldName, ASelectedColor);
end;

function TExpandableListViewManager.AddRadioButtonField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; AValue: Boolean;
  const AGroupName: string = 'DefaultRadioGroup'): TRadioButton;
begin
  Result := FExpandableListView.AddRadioButtonField(AHeaderInfo, AFieldName,
    AValue, AGroupName);
end;

// Header işlemleri
procedure TExpandableListViewManager.SetHeaderSVG(AHeaderInfo: THeaderInfo;
  const ASVGData: string);
begin
  FExpandableListView.SetHeaderSVG(AHeaderInfo, ASVGData);
end;

procedure TExpandableListViewManager.ExpandHeader(AHeaderInfo: THeaderInfo);
begin
  FExpandableListView.ExpandHeader(AHeaderInfo);
end;

procedure TExpandableListViewManager.CollapseHeader(AHeaderInfo: THeaderInfo);
begin
  FExpandableListView.CollapseHeader(AHeaderInfo);
end;

procedure TExpandableListViewManager.ExpandAll;
begin
  FExpandableListView.ExpandAll;
end;

procedure TExpandableListViewManager.CollapseAll;
begin
  FExpandableListView.CollapseAll;
end;

end.
