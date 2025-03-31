 unit ExpandableListView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ListView,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Objects, FMX.Controls.Presentation, FMX.Edit, FMX.NumberBox, FMX.Layouts,
  FMX.StdCtrls, FMX.Ani, FMX.ListBox, System.Generics.Collections, FMX.Colors,
  FMX.Filter.Effects, System.Skia, FMX.Skia, FMX.Text, System.JSON, Math,
  StrUtils, FMX.DateTimeCtrls, FMX.Memo, FMX.Calendar, FMX.DateTimeCtrls.Types;
// SVGIcon için eklendi

type
  THeaderInfo = class
    Title: string;
    Color: TAlphaColor;
    ImageIndex: Integer;
    IsExpanded: Boolean;
    HeaderItem: TListBoxItem;
    ChildItems: TList<TListBoxItem>;
    SVGData: string; // SVG verisi için eklendi
    SVGIcon: TSkSvg; // SVG ikonu için eklendi
    constructor Create(const ATitle: string; AImageIndex: Integer;
      AColor: TAlphaColor);
    destructor Destroy; override;
  end;

  TControlType = (ctEdit, ctNumberBox, ctCheckBox, ctSwitch, ctComboBox,
    ctColorComboBox);
  TNumberValueType = (nvtInteger, nvtFloat, nvtCurrency);

  THeaderClickEvent = procedure(Sender: TObject; HeaderInfo: THeaderInfo)
    of object;

  { TExpandableListView }
  TExpandableListView = class(TListBox)
  private
    FOnHeaderClick: THeaderClickEvent;
    FHeaderHeight: Single;
    FItemHeight: Single;
    FAnimationDuration: Single;
    FShowIcons: Boolean;
    FHeaderTextColor: TAlphaColor;
    FHeaderTextSize: Single;
    FItemTextSize: Single;

    procedure CreateHeaderSection(AHeaderInfo: THeaderInfo);
    procedure HeaderRectangleClick(Sender: TObject);
    procedure ToggleHeaderSection(AHeaderInfo: THeaderInfo);
    procedure ShowChildItems(AHeaderInfo: THeaderInfo; AShow: Boolean);
    function FindListBoxItemIndex(Item: TListBoxItem): Integer;
    procedure UpdateSVGIcon(AHeaderInfo: THeaderInfo);
    function HexToTAlphaColor(const HexColor: string): TAlphaColor;
    function ColorToString(Color: TAlphaColor): string;
    procedure ProcessComponent(Component: TComponent; const FieldName: string;
      FieldsObj: TJSONObject);
    function CreateValueObjectForComponent(Component: TComponent): TJSONObject;
    function IsValidComponent(Component: TComponent): Boolean;
    procedure ProcessLayoutComponents(Layout: TLayout; const FieldName: string;
      FieldsObj: TJSONObject);
    function FindLabelTextForComponent(Component: TComponent): string;

  protected
    procedure Loaded; override;
  public
     FHeaders: TList<THeaderInfo>;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddHeader(const ATitle: string; AImageIndex: Integer;
      AColor: TAlphaColor): THeaderInfo;
    procedure AddDataField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AControlType: TControlType); overload;

    // Yeni eklenen metotlar - farklı kontrol tipleri için
    function AddEditField(AHeaderInfo: THeaderInfo;
      const AFieldName, AValue: string): TEdit;
    function AddNumberField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AValue: Double; AMin: Double = 0; AMax: Double = 100;
      AValueType: TNumberValueType = nvtFloat; AVertIncrement: Boolean = False)
      : TNumberBox;
    function AddCheckBoxField(AHeaderInfo: THeaderInfo;
      const AFieldName: string; AChecked: Boolean = False): TCheckBox;
    function AddSwitchField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      AIsOn: Boolean = False): TSwitch;
    function AddComboBoxField(AHeaderInfo: THeaderInfo;
      const AFieldName: string; AItems: array of string;
      ASelectedIndex: Integer = 0): TComboBox;
    function AddColorBoxField(AHeaderInfo: THeaderInfo;
      const AFieldName: string;
      ASelectedColor: TAlphaColor = TAlphaColorRec.Blue): TColorComboBox;

    // SVG ikonu ayarlamak için
    procedure SetHeaderSVG(AHeaderInfo: THeaderInfo; const ASVGData: string);

    procedure ExpandHeader(AHeaderInfo: THeaderInfo);
    procedure CollapseHeader(AHeaderInfo: THeaderInfo);
    procedure ExpandAll;
    procedure CollapseAll;

    // JSON çıktısı üreten yeni fonksiyon
    function ExportToJSON: string;

    // Belirtilen başlık için JSON çıktısı üreten fonksiyon
    function ExportHeaderToJSON(AHeaderInfo: THeaderInfo): TJSONObject;

    // JSON işlemleri için yeni fonksiyonlar
    function LoadFromJSON(const AJSONString: string): Boolean;
    function LoadFromJSONFile(const AFileName: string): Boolean;
    function LoadHeaderFromJSON(const ATitle: string; AJSONObj: TJSONObject)
      : THeaderInfo;
    procedure LoadFieldsFromJSON(HeaderInfo: THeaderInfo;
      FieldsObj: TJSONObject);
function  GenerateComponentName(Component: TComponent; const BaseName: string; Index: Integer): string;
  published
    property OnHeaderClick: THeaderClickEvent read FOnHeaderClick
      write FOnHeaderClick;
    property HeaderHeight: Single read FHeaderHeight write FHeaderHeight;
    property ItemHeight: Single read FItemHeight write FItemHeight;
    property AnimationDuration: Single read FAnimationDuration
      write FAnimationDuration;
    property ShowIcons: Boolean read FShowIcons write FShowIcons;
    property HeaderTextColor: TAlphaColor read FHeaderTextColor
      write FHeaderTextColor;
    property HeaderTextSize: Single read FHeaderTextSize write FHeaderTextSize;
    property ItemTextSize: Single read FItemTextSize write FItemTextSize;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Custom', [TExpandableListView]);
end;

{ THeaderInfo }

constructor THeaderInfo.Create(const ATitle: string; AImageIndex: Integer;
  AColor: TAlphaColor);
begin
  Title := ATitle;
  ImageIndex := AImageIndex;
  Color := AColor;
  IsExpanded := False;
  HeaderItem := nil;
  ChildItems := TList<TListBoxItem>.Create;
  SVGData := '';
  SVGIcon := nil;
end;

destructor THeaderInfo.Destroy;
begin
  ChildItems.Free;
  inherited;
end;

{ TExpandableListView }

constructor TExpandableListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHeaders := TList<THeaderInfo>.Create;

  // Varsayılan özellikler
  ShowScrollBars := True;
  StyleLookup := 'listboxstyle';

  // Varsayılan değerleri ayarla
  FHeaderHeight := 40;
  FItemHeight := 35;
  FAnimationDuration := 0.2;
  FShowIcons := True;
  FHeaderTextColor := TAlphaColorRec.White;
  FHeaderTextSize := 16;
  FItemTextSize := 14;
end;

destructor TExpandableListView.Destroy;
var
  i: Integer;
begin
  // HeaderInfo nesnelerini temizle
  for i := 0 to FHeaders.Count - 1 do
    FHeaders[i].Free;

  FHeaders.Free;
  inherited;
end;

procedure TExpandableListView.Loaded;
begin
  inherited;
  // Component tamamen yüklendiğinde çağrılır
end;

 procedure TExpandableListView.LoadFieldsFromJSON(HeaderInfo: THeaderInfo; FieldsObj: TJSONObject);
var
  i: Integer;
  FieldName: string;
  FieldValue: TJSONValue;
  ValueObj: TJSONObject;
  ActualValue: TJSONValue;
  Min, Max: Double;
  DecimalDigits: Integer;
  ValueType: string;
  VertIncrement: Boolean;
  Items: TJSONArray;
  ItemsArray: array of string;
  SelectedIndex: Integer;
  ColorValue: TAlphaColor;
  LabelText: string;
begin
  if (HeaderInfo = nil) or (FieldsObj = nil) then
    Exit;

  for i := 0 to FieldsObj.Count - 1 do
  begin
    try
      FieldName := FieldsObj.Pairs[i].JsonString.Value;
      FieldValue := FieldsObj.Pairs[i].JsonValue;

      // HeaderIndex, HeaderColor ve SVGData alanlarını atla
      if (FieldName = 'HeaderIndex') or (FieldName = 'HeaderColor') or
        (FieldName = 'SVGData') or (FieldName = 'Fields') then
        Continue;

      // Varsayılan değerleri ayarla
      Min := 0;
      Max := 100;
      DecimalDigits := 2;
      ValueType := 'float';
      VertIncrement := False;
      ActualValue := nil;
      LabelText := ''; // Etiket metni için varsayılan değer

      // JSON nesne kontrolü
      if FieldValue is TJSONObject then
      begin
        ValueObj := TJSONObject(FieldValue);

        // LabelText değerini al (varsa)
        if ValueObj.GetValue('labelText') <> nil then
          LabelText := ValueObj.GetValue('labelText').Value;

        // Min, Max, DecimalDigits ve ValueType parametrelerini al
        if ValueObj.GetValue('Min') <> nil then
          Min := (ValueObj.GetValue('Min') as TJSONNumber).AsDouble;

        if ValueObj.GetValue('Max') <> nil then
          Max := (ValueObj.GetValue('Max') as TJSONNumber).AsDouble;

        if ValueObj.GetValue('DecimalDigits') <> nil then
          DecimalDigits := (ValueObj.GetValue('DecimalDigits')
            as TJSONNumber).AsInt;

        if ValueObj.GetValue('ValueType') <> nil then
          ValueType := (ValueObj.GetValue('ValueType') as TJSONString).Value;

        if ValueObj.GetValue('VertIncrement') <> nil then
          VertIncrement := (ValueObj.GetValue('VertIncrement') as TJSONBool)
            .AsBoolean;

        // Değeri al
        ActualValue := ValueObj.GetValue('Value');

        // JSON nesne kontrolü
        if ValueObj.GetValue('Items') <> nil then
        begin
          // ComboBox için
          Items := ValueObj.GetValue('Items') as TJSONArray;
          SetLength(ItemsArray, Items.Count);

          for var j := 0 to Items.Count - 1 do
            ItemsArray[j] := Items.Items[j].Value;

          SelectedIndex := 0;
          if ValueObj.GetValue('SelectedIndex') <> nil then
            SelectedIndex := (ValueObj.GetValue('SelectedIndex')
              as TJSONNumber).AsInt;

          // Etiket metnini kullan (varsa)
          if LabelText <> '' then
            FieldName := LabelText;

          AddComboBoxField(HeaderInfo, FieldName, ItemsArray, SelectedIndex);
        end
        else if ActualValue is TJSONNumber then
        begin
          // Sayısal değer
          if LabelText <> '' then
            FieldName := LabelText;

          if LowerCase(ValueType) = 'integer' then
          begin
            var
            IntValue := Round((ActualValue as TJSONNumber).AsDouble);
            AddNumberField(HeaderInfo, FieldName, IntValue, Min, Max,
              nvtInteger, VertIncrement);
          end
          else
          begin
            var
            FloatValue := (ActualValue as TJSONNumber).AsDouble;
            AddNumberField(HeaderInfo, FieldName, FloatValue, Min, Max,
              nvtFloat, VertIncrement);
          end;
        end
        else if ActualValue is TJSONBool then
        begin
          var
          BoolValue := (ActualValue as TJSONBool).AsBoolean;

          if LabelText <> '' then
            FieldName := LabelText;

          AddCheckBoxField(HeaderInfo, FieldName, BoolValue);
        end
        else if ActualValue is TJSONString then
        begin
          var
          StrValue := (ActualValue as TJSONString).Value;

          // Renk değeri kontrolü
          if (Length(StrValue) > 0) and (StrValue[1] = '#') then
          begin
            // Renk değeri
            if LabelText <> '' then
              FieldName := LabelText;

            try
              ColorValue := HexToTAlphaColor(StrValue);
              AddColorBoxField(HeaderInfo, FieldName, ColorValue);
            except
              AddEditField(HeaderInfo, FieldName, StrValue);
            end;
          end
          else
          begin
            // Normal metin
            if LabelText <> '' then
              FieldName := LabelText;

            AddEditField(HeaderInfo, FieldName, StrValue);
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        // Hata durumunda işleme devam et
        Continue;
      end;
    end;
  end;
end;

// JSON dosyasından veri yükleme

function TExpandableListView.LoadFromJSON(const AJSONString: string): Boolean;
var
  JSONObj: TJSONObject;
  HeaderNames: TJSONArray;
  HeaderName: string;
  HeaderObj: TJSONObject;
  HeaderInfo: THeaderInfo;
  HeaderIndex: Integer;
  HeaderColor: TAlphaColor;
  FieldsObj: TJSONObject;
  SVGData: string;
  i: Integer;
begin
  Result := False;

  try
    // Önce mevcut başlıkları temizle
    for i := FHeaders.Count - 1 downto 0 do
      FHeaders[i].Free;
    FHeaders.Clear;

    // Tüm liste öğelerini temizle
    Clear;

    // JSON nesnesini oluştur
    JSONObj := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
    if JSONObj = nil then
    begin
      ShowMessage('JSON parse hatası: Geçersiz JSON formatı');
      Exit;
    end;

    try
      // Her bir başlık için
      for i := 0 to JSONObj.Count - 1 do
      begin
        try
          HeaderName := JSONObj.Pairs[i].JsonString.Value;
          if not(JSONObj.Pairs[i].JsonValue is TJSONObject) then
          begin
            ShowMessage('Hata: "' + HeaderName + '" için geçersiz başlık yapısı');
            Continue;
          end;

          HeaderObj := JSONObj.Pairs[i].JsonValue as TJSONObject;

          // Başlık indeksi ve rengi
          HeaderIndex := 0;
          if HeaderObj.TryGetValue('HeaderIndex', HeaderIndex) = False then
            HeaderIndex := i; // Varsayılan olarak sıra numarası

          HeaderColor := TAlphaColorRec.Blue; // Varsayılan renk
          if HeaderObj.GetValue('HeaderColor') <> nil then
          begin
            var
              ColorStr := HeaderObj.GetValue('HeaderColor').Value;
            if ColorStr.StartsWith('#') then
            begin
              // Hex formatından renk oluştur
              HeaderColor := HexToTAlphaColor(ColorStr);
            end
            else
              HeaderColor := ColorStr.ToInt64;
          end;

          // Başlık oluştur
          HeaderInfo := AddHeader(HeaderName, HeaderIndex, HeaderColor);
          if HeaderInfo = nil then
          begin
            ShowMessage('Başlık oluşturma hatası: ' + HeaderName);
            Continue;
          end;

          // SVG verisi
          if HeaderObj.GetValue('SVGData') <> nil then
          begin
            SVGData := HeaderObj.GetValue('SVGData').Value;
            if SVGData <> '' then
              SetHeaderSVG(HeaderInfo, SVGData);
          end;

          // Alanları işle
          if HeaderObj.GetValue('Fields') <> nil then
          begin
            FieldsObj := HeaderObj.GetValue('Fields') as TJSONObject;
            LoadFieldsFromJSON(HeaderInfo, FieldsObj);
          end
          else
          begin
            // Eski format - doğrudan alanlar
            LoadFieldsFromJSON(HeaderInfo, HeaderObj);
          end;
        except
          on E: Exception do
            ShowMessage('Başlık işleme hatası: ' + HeaderName + ' - ' + E.Message);
        end;
      end;

      Result := True;
    finally
      JSONObj.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('JSON işleme hatası: ' + E.Message);
      Result := False;
    end;
  end;
end;


// JSON dosyasından başlık bilgisi yükleme
function TExpandableListView.LoadHeaderFromJSON(const ATitle: string;
  AJSONObj: TJSONObject): THeaderInfo;
var
  HeaderInfo: THeaderInfo;
  JSONPair: TJSONPair;
  FieldName, FieldValue: string;
  i: Integer;
  ColorValue: TAlphaColor;
  SVGData: string;
  Control: TControl;
begin
  Result := nil;

  try
    // Başlık oluştur (varsayılan değerlerle)
    HeaderInfo := THeaderInfo.Create(ATitle, 0, TAlphaColorRec.Blue);

    // JSON nesnesindeki her alanı işle
    for i := 0 to AJSONObj.Count - 1 do
    begin
      JSONPair := AJSONObj.Pairs[i];
      FieldName := JSONPair.JsonString.Value;

      // SVG verisi için özel işlem
      if FieldName = 'SVGData' then
      begin
        SVGData := JSONPair.JsonValue.Value;
        if SVGData <> '' then
          SetHeaderSVG(HeaderInfo, SVGData);
        Continue;
      end;

      // Alan tipine göre kontrol oluştur
      if JSONPair.JsonValue is TJSONString then
      begin
        FieldValue := JSONPair.JsonValue.Value;

        // Boolean değerler için kontrol
        if (UpperCase(FieldValue) = 'true') or (UpperCase(FieldValue) = 'false')
        then
        begin
          // Switch veya CheckBox oluştur
          AddCheckBoxField(HeaderInfo, FieldName, FieldValue = 'true');
        end
        else if FieldValue.StartsWith('#') then
        begin
          // Renk değeri
          try
            ColorValue :=
              StrToInt('$' + Copy(FieldValue, 2, Length(FieldValue) - 1));
            AddColorBoxField(HeaderInfo, FieldName, ColorValue);
          except
            AddEditField(HeaderInfo, FieldName, FieldValue);
          end;
        end
        else
        begin
          // Normal metin
          AddEditField(HeaderInfo, FieldName, FieldValue);
        end;
      end
      else if JSONPair.JsonValue is TJSONNumber then
      begin
        // Sayısal değer
        AddNumberField(HeaderInfo, FieldName, TJSONNumber(JSONPair.JsonValue)
          .AsDouble);
      end
      else if JSONPair.JsonValue is TJSONArray then
      begin
        // ComboBox için dizi
        var
        JSONArray := TJSONArray(JSONPair.JsonValue);
        var
          Items: array of string;
        SetLength(Items, JSONArray.Count);

        for var j := 0 to JSONArray.Count - 1 do
          Items[j] := JSONArray.Items[j].Value;

        AddComboBoxField(HeaderInfo, FieldName, Items);
      end;
    end;

    Result := HeaderInfo;
  except
    on E: Exception do
    begin
      if HeaderInfo <> nil then
        HeaderInfo.Free;
      Result := nil;
    end;
  end;
end;

function TExpandableListView.LoadFromJSONFile(const AFileName: string): Boolean;
var
  JSONStr: string;
  FileStream: TFileStream;
  StringStream: TStringStream;
begin
  Result := False;

  try
    FileStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      StringStream := TStringStream.Create('', TEncoding.UTF8);
      try
        StringStream.CopyFrom(FileStream, 0);
        JSONStr := StringStream.DataString;
        Result := LoadFromJSON(JSONStr);
      finally
        StringStream.Free;
      end;
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;

function TExpandableListView.AddHeader(const ATitle: string;
  AImageIndex: Integer; AColor: TAlphaColor): THeaderInfo;
var
  HeaderInfo: THeaderInfo;
begin
  HeaderInfo := THeaderInfo.Create(ATitle, AImageIndex, AColor);
  FHeaders.Add(HeaderInfo);
  CreateHeaderSection(HeaderInfo);
  Result := HeaderInfo;
end;
 function TExpandableListView.FindLabelTextForComponent(Component: TComponent): string;
var
  ParentComponent: TComponent;
  i: Integer;
  Label1: TLabel;
  Control: TControl;
begin
  Result := '';

  // Component'i TControl'e dönüştür (eğer mümkünse)
  if not (Component is TControl) then
    Exit;

  Control := TControl(Component);

  // Bileşenin ebeveyn bileşenini bul (genellikle bir Layout)
  ParentComponent := Component.Owner;
  if not (ParentComponent is TLayout) then
    Exit;

  // Bu Layout içindeki tüm Label bileşenlerini kontrol et
  for i := 0 to TLayout(ParentComponent).ComponentCount - 1 do
  begin
    if TLayout(ParentComponent).Components[i] is TLabel then
    begin
      Label1 := TLabel(TLayout(ParentComponent).Components[i]);
      // Label'ın solda olduğunu varsayıyoruz (alan adı etiketi olarak)
      if (Label1.Position.X < Control.Position.X) then
      begin
        Result := Label1.Text;
        Break;
      end
      else if Component is TCheckBox then
        Result := Label1.Text;

    end;
  end;
end;

function TExpandableListView.FindListBoxItemIndex(Item: TListBoxItem): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
  begin
    if ListItems[i] = Item then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TExpandableListView.GenerateComponentName(Component: TComponent; const BaseName: string; Index: Integer): string;
var
  TypeName: string;
begin
  // Bileşen tipini al
  TypeName := Component.ClassName;
  if TypeName.StartsWith('T') then
    TypeName := TypeName.Substring(1);

  // Bileşen adını oluştur
  Result := LowerCase(BaseName + '_' + TypeName + IntToStr(Index));
end;


procedure TExpandableListView.CreateHeaderSection(AHeaderInfo: THeaderInfo);
var
  ListItem: TListBoxItem;
  Layout: TLayout;
  TitleLabel: TLabel;
  Rectangle: TRectangle;
  Arrow: TPath;
  IconLayout: TLayout;
begin
  ListItem := TListBoxItem.Create(Self);
  ListItem.Height := 60;
  ListItem.Selectable := False;
  ListItem.Text := '';
  AddObject(ListItem);

  // HeaderInfo'ya ListItem'ı kaydet
  AHeaderInfo.HeaderItem := ListItem;

  // Arkaplan için dikdörtgen
  Rectangle := TRectangle.Create(ListItem);
  Rectangle.Fill.Color := AHeaderInfo.Color;
  Rectangle.Stroke.Kind := TBrushKind.None;
  Rectangle.Align := TAlignLayout.Client;
  Rectangle.XRadius := 8;
  Rectangle.YRadius := 8;
  Rectangle.Margins.Rect := TRectF.Create(8, 4, 8, 4);
  Rectangle.Opacity := 0.95;
  Rectangle.HitTest := True;
  Rectangle.OnClick := HeaderRectangleClick;
  Rectangle.TagObject := AHeaderInfo; // HeaderInfo'yu Rectangle'a bağla
  ListItem.AddObject(Rectangle);

  // Ana layout
  Layout := TLayout.Create(ListItem);
  Layout.Align := TAlignLayout.Client;
  Layout.Padding.Rect := TRectF.Create(16, 8, 16, 8);
  Layout.HitTest := False;
  ListItem.AddObject(Layout);

  // İkon için layout
  IconLayout := TLayout.Create(Layout);
  IconLayout.Align := TAlignLayout.Left;
  IconLayout.Width := 32;
  IconLayout.Height := 32;
  IconLayout.HitTest := False;
  IconLayout.Name := 'IconLayout';
  Layout.AddObject(IconLayout);

  // SVG İkon oluştur
  AHeaderInfo.SVGIcon := TSkSvg.Create(IconLayout);
  AHeaderInfo.SVGIcon.Align := TAlignLayout.Client;
  AHeaderInfo.SVGIcon.HitTest := False;
  AHeaderInfo.SVGIcon.Opacity := 1;
  AHeaderInfo.SVGIcon.Svg.WrapMode := TSkSvgWrapMode.Fit;
  IconLayout.AddObject(AHeaderInfo.SVGIcon);

  // Başlık
  TitleLabel := TLabel.Create(Layout);
  TitleLabel.Align := TAlignLayout.Client;
  TitleLabel.Text := AHeaderInfo.Title;
  TitleLabel.StyledSettings := [];
  TitleLabel.TextSettings.Font.Size := 18;
  TitleLabel.TextSettings.Font.Family := 'Segoe UI';
  TitleLabel.TextSettings.FontColor := TAlphaColorRec.White;
  TitleLabel.TextSettings.Font.Style := [TFontStyle.fsBold];
  TitleLabel.Margins.Left := 16;
  TitleLabel.HitTest := False;
  Layout.AddObject(TitleLabel);

  // Açılıp kapanma oku
  Arrow := TPath.Create(Layout);
  Arrow.Name := 'Arrow'; // İsim ver, sonra bulmak için
  Arrow.Align := TAlignLayout.Right;
  Arrow.Width := 24;
  Arrow.Height := 24;
  Arrow.Stroke.Color := TAlphaColorRec.White;
  Arrow.Fill.Color := TAlphaColorRec.White;
  Arrow.Data.Data := 'M 4,8 L 12,16 L 20,8'; // Aşağı ok
  Arrow.HitTest := False;
  Layout.AddObject(Arrow);
end;

procedure TExpandableListView.HeaderRectangleClick(Sender: TObject);
var
  HeaderInfo: THeaderInfo;
begin
  try
    if (Sender is TRectangle) and Assigned(TRectangle(Sender).TagObject) and
      (TRectangle(Sender).TagObject is THeaderInfo) then
    begin
      HeaderInfo := THeaderInfo(TRectangle(Sender).TagObject);
      ToggleHeaderSection(HeaderInfo);

      // Olay işleyicisini çağır
      if Assigned(FOnHeaderClick) then
        FOnHeaderClick(Self, HeaderInfo);
    end;
  except
    on E: Exception do
      raise Exception.Create('Tıklama Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.HexToTAlphaColor(const HexColor: string)
  : TAlphaColor;
var
  ColorStr: string;
  R, G, B, A: Byte;
begin
  ColorStr := HexColor;
  if ColorStr.StartsWith('#') then
    ColorStr := Copy(ColorStr, 2, Length(ColorStr) - 1);

  // 6 karakterli hex (#RRGGBB) - alfa kanalı FF olarak kabul edilir
  if Length(ColorStr) = 6 then
  begin
    A := $FF;
    R := StrToInt('$' + Copy(ColorStr, 1, 2));
    G := StrToInt('$' + Copy(ColorStr, 3, 2));
    B := StrToInt('$' + Copy(ColorStr, 5, 2));
  end
  // 8 karakterli hex (#AARRGGBB) - alfa kanalı dahil
  else if Length(ColorStr) = 8 then
  begin
    A := StrToInt('$' + Copy(ColorStr, 1, 2));
    R := StrToInt('$' + Copy(ColorStr, 3, 2));
    G := StrToInt('$' + Copy(ColorStr, 5, 2));
    B := StrToInt('$' + Copy(ColorStr, 7, 2));
  end
  else
    raise Exception.Create('Geçersiz renk formatı');

  Result := TAlphaColor(A shl 24 or R shl 16 or G shl 8 or B);
end;

procedure TExpandableListView.ShowChildItems(AHeaderInfo: THeaderInfo;
  AShow: Boolean);
var
  i: Integer;
  ChildItem: TListBoxItem;
begin
  // Alt öğeleri göster/gizle
  for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
  begin
    ChildItem := AHeaderInfo.ChildItems[i];

    if AShow then
    begin
      ChildItem.Height := 50;
      ChildItem.Opacity := 1;
      ChildItem.Visible := True;
    end
    else
    begin
      ChildItem.Height := 0;
      ChildItem.Opacity := 0;
      ChildItem.Visible := False;
    end;
  end;
end;

procedure TExpandableListView.ToggleHeaderSection(AHeaderInfo: THeaderInfo);
var
  i: Integer;
  Arrow: TPath;
  Layout: TLayout;
begin
  try
    // Durumu değiştir
    AHeaderInfo.IsExpanded := not AHeaderInfo.IsExpanded;

    // Ok simgesini bul ve döndür
    for i := 0 to AHeaderInfo.HeaderItem.ComponentCount - 1 do
    begin
      if (AHeaderInfo.HeaderItem.Components[i] is TLayout) then
      begin
        Layout := TLayout(AHeaderInfo.HeaderItem.Components[i]);
        for var j := 0 to Layout.ComponentCount - 1 do
        begin
          if (Layout.Components[j] is TPath) and
            (Layout.Components[j].Name = 'Arrow') then
          begin
            Arrow := TPath(Layout.Components[j]);

            // Animasyonlu döndürme
            if AHeaderInfo.IsExpanded then
              TAnimator.AnimateFloat(Arrow, 'RotationAngle', 180, 0.3)
              // Yukarı ok
            else
              TAnimator.AnimateFloat(Arrow, 'RotationAngle', 0, 0.3);
            // Aşağı ok
            Break;
          end;
        end;
      end;
    end;

    // Alt öğeleri göster/gizle
    if AHeaderInfo.IsExpanded then
    begin
      // Alt öğeleri göster
      for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
      begin
        var
        ChildItem := AHeaderInfo.ChildItems[i];
        ChildItem.Height := 0;
        ChildItem.Opacity := 0;
        ChildItem.Visible := True;

        TAnimator.AnimateFloat(ChildItem, 'Height', 50, 0.3);
        TAnimator.AnimateFloat(ChildItem, 'Opacity', 1, 0.3);
      end;
    end
    else
    begin
      // Alt öğeleri gizle - iyileştirilmiş versiyon
      for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
      begin
        var
        ChildItem := AHeaderInfo.ChildItems[i];

        // Önce opaklığı animasyonla azalt
        TAnimator.AnimateFloat(ChildItem, 'Opacity', 0, 0.2);

        // Sonra yüksekliği animasyonla azalt
        TAnimator.AnimateFloat(ChildItem, 'Height', 0, 0.3);

        // Animasyon tamamlandıktan sonra görünürlüğü kapat
        TThread.CreateAnonymousThread(
          procedure
          begin
            Sleep(350); // Animasyonun tamamlanması için yeterli süre
            TThread.Synchronize(nil,
              procedure
              begin
                if not AHeaderInfo.IsExpanded then
                begin
                  ChildItem.Visible := False;
                end;
              end);
          end).Start;
      end;
    end;
  except
    on E: Exception do
      raise Exception.Create('ToggleHeaderSection Hatası: ' + E.Message);
  end;
end;

// SVG ikonunu güncelleme
procedure TExpandableListView.UpdateSVGIcon(AHeaderInfo: THeaderInfo);
begin
  if Assigned(AHeaderInfo.SVGIcon) and (AHeaderInfo.SVGData <> '') then
  begin
    AHeaderInfo.SVGIcon.Svg.Source := AHeaderInfo.SVGData;
  end;
end;

// SVG verisi ayarlama
procedure TExpandableListView.SetHeaderSVG(AHeaderInfo: THeaderInfo;
const ASVGData: string);
begin
  AHeaderInfo.SVGData := ASVGData;
  UpdateSVGIcon(AHeaderInfo);
end;
procedure TExpandableListView.AddDataField(AHeaderInfo: THeaderInfo; const AFieldName: string; AControlType: TControlType);
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  HeaderIndex, LastChildIndex: Integer;
begin
  // Başlık öğesinin indeksini bul
  HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
  if HeaderIndex < 0 then
  begin
    raise Exception.Create('Başlık öğesi bulunamadı!');
    Exit;
  end;

  // Son alt öğe indeksini bul
  LastChildIndex := HeaderIndex;
  for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
  begin
    var
    idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
    if idx > LastChildIndex then
      LastChildIndex := idx;
  end;

  // Yeni alt öğeyi oluştur
  DataItem := TListBoxItem.Create(Self);
  DataItem.Height := 0; // Başlangıçta 0 yükseklik
  DataItem.Selectable := False;
  DataItem.Text := '';
  DataItem.Visible := False; // Başlangıçta gizli
  DataItem.Opacity := 0; // Başlangıçta saydam
  DataItem.Tag := 0; // Etiket metni için Tag kullanabiliriz

  // Alt öğeyi doğru pozisyona ekle
  InsertObject(LastChildIndex + 1, DataItem);

  // HeaderInfo'nun ChildItems listesine ekle
  AHeaderInfo.ChildItems.Add(DataItem);

  // Arkaplan
  Rectangle := TRectangle.Create(DataItem);
  Rectangle.Fill.Color := TAlphaColorRec.White;
  Rectangle.Stroke.Color := $FFE0E0E0;
  Rectangle.Stroke.Thickness := 1;
  Rectangle.Align := TAlignLayout.Client;
  Rectangle.XRadius := 6;
  Rectangle.YRadius := 6;
  Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
  Rectangle.Opacity := 0.95;
  DataItem.AddObject(Rectangle);

  // Ana layout
  Layout := TLayout.Create(DataItem);
  Layout.Align := TAlignLayout.Client;
  Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
  DataItem.AddObject(Layout);

  // Etiket
  lbl := TLabel.Create(Layout);
  lbl.Align := TAlignLayout.Left;
  lbl.Width := 120;
  lbl.Text := AFieldName;
  lbl.StyledSettings := [];
  lbl.TextSettings.Font.Size := 14;
  lbl.TextSettings.Font.Family := 'Segoe UI';
  lbl.TextSettings.FontColor := TAlphaColorRec.Black;
  Layout.AddObject(lbl);

  // Etiket metnini DataItem.Tag'de sakla
  DataItem.TagString := AFieldName;

  // Kontrol tipine göre işlem yap
  case AControlType of
    ctEdit:
      AddEditField(AHeaderInfo, AFieldName, '');
    ctNumberBox:
      AddNumberField(AHeaderInfo, AFieldName, 0);
    ctCheckBox:
      AddCheckBoxField(AHeaderInfo, AFieldName, False);
    ctSwitch:
      AddSwitchField(AHeaderInfo, AFieldName, False);
    ctComboBox:
      AddComboBoxField(AHeaderInfo, AFieldName, ['Seçenek 1', 'Seçenek 2']);
    ctColorComboBox:
      AddColorBoxField(AHeaderInfo, AFieldName, TAlphaColorRec.Blue);
  end;
end;


// Yeni metotlar - belirli kontrol tipleri için
function TExpandableListView.AddEditField(AHeaderInfo: THeaderInfo;
const AFieldName, AValue: string): TEdit;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  Edit: TEdit;
  HeaderIndex, LastChildIndex: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru
    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // Edit kontrolü
    Edit := TEdit.Create(Layout);
    Edit.Align := TAlignLayout.Client;
    Edit.Margins.Left := 8;
    Edit.StyleLookup := 'editstyle';
    Edit.Text := AValue;
    Layout.AddObject(Edit);

    Result := Edit;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddEditField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.AddNumberField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AValue: Double; AMin: Double = 0; AMax: Double = 100;
AValueType: TNumberValueType = nvtFloat; AVertIncrement: Boolean = False)
  : TNumberBox;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  NumBox: TNumberBox;
  HeaderIndex, LastChildIndex: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // NumberBox kontrolü
    NumBox := TNumberBox.Create(Layout);
    NumBox.Align := TAlignLayout.Client;
    NumBox.Margins.Left := 8;
    NumBox.Min := AMin;
    NumBox.Max := AMax;

    // ValueType ayarlaması
    case AValueType of
      nvtInteger:
        begin
          NumBox.ValueType := TNumValueType.Integer;
          NumBox.DecimalDigits := 0;
        end;
      nvtFloat:
        begin
          NumBox.ValueType := TNumValueType.Float;
          NumBox.DecimalDigits := 2;
          // Float için önemli: Ondalık ayırıcıyı ayarla
          // NumBox.FormatSettings.DecimalSeparator := '.';
        end;

    end;

    // Değeri ayarla
    NumBox.Value := AValue;

    // Dikey artırma düğmeleri
    NumBox.VertIncrementEnabled := AVertIncrement;

    Layout.AddObject(NumBox);

    Result := NumBox;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddNumberField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.AddCheckBoxField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AChecked: Boolean): TCheckBox;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  CheckBox: TCheckBox;
  HeaderIndex, LastChildIndex: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // CheckBox kontrolü
    CheckBox := TCheckBox.Create(Layout);
    CheckBox.Align := TAlignLayout.Right;
    CheckBox.Width := 60;
    CheckBox.StyleLookup := 'checkboxstyle';
    CheckBox.IsChecked := AChecked;
    Layout.AddObject(CheckBox);

    Result := CheckBox;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddCheckBoxField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.AddSwitchField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AIsOn: Boolean): TSwitch;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  Switch: TSwitch;
  HeaderIndex, LastChildIndex: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // Switch kontrolü
    Switch := TSwitch.Create(Layout);
    Switch.Align := TAlignLayout.Right;
    Switch.Width := 60;
    Switch.StyleLookup := 'switchstyle';
    Switch.IsChecked := AIsOn;
    Layout.AddObject(Switch);

    Result := Switch;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddSwitchField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.AddComboBoxField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AItems: array of string; ASelectedIndex: Integer)
  : TComboBox;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  ComboBox: TComboBox;
  HeaderIndex, LastChildIndex, i: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var ii := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[ii]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // ComboBox kontrolü
    ComboBox := TComboBox.Create(Layout);
    ComboBox.Align := TAlignLayout.Client;
    ComboBox.Margins.Left := 8;

    // Öğeleri ekle
    for i := Low(AItems) to High(AItems) do
      ComboBox.Items.Add(AItems[i]);

    // Seçili öğe
    if (ASelectedIndex >= 0) and (ASelectedIndex < Length(AItems)) then
      ComboBox.ItemIndex := ASelectedIndex;

    Layout.AddObject(ComboBox);

    Result := ComboBox;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddComboBoxField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.AddColorBoxField(AHeaderInfo: THeaderInfo;
const AFieldName: string; ASelectedColor: TAlphaColor): TColorComboBox;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  ColorBox: TColorComboBox;
  HeaderIndex, LastChildIndex: Integer;
begin
  Result := nil;
  try
    // Başlık öğesinin indeksini bul
    HeaderIndex := FindListBoxItemIndex(AHeaderInfo.HeaderItem);
    if HeaderIndex < 0 then
    begin
      raise Exception.Create('Başlık öğesi bulunamadı!');
      Exit;
    end;

    // Son alt öğe indeksini bul
    LastChildIndex := HeaderIndex;
    for var i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      var
      idx := FindListBoxItemIndex(AHeaderInfo.ChildItems[i]);
      if idx > LastChildIndex then
        LastChildIndex := idx;
    end;

    // Yeni alt öğeyi oluştur
    DataItem := TListBoxItem.Create(Self);
    DataItem.Height := 0; // Başlangıçta 0 yükseklik
    DataItem.Selectable := False;
    DataItem.Text := '';
    DataItem.Visible := False; // Başlangıçta gizli
    DataItem.Opacity := 0; // Başlangıçta saydam

    // Alt öğeyi doğru pozisyona ekle
    InsertObject(LastChildIndex + 1, DataItem);

    // HeaderInfo'nun ChildItems listesine ekle
    AHeaderInfo.ChildItems.Add(DataItem);

    // Arkaplan
    Rectangle := TRectangle.Create(DataItem);
    Rectangle.Fill.Color := TAlphaColorRec.White;
    Rectangle.Stroke.Color := $FFE0E0E0;
    Rectangle.Stroke.Thickness := 1;
    Rectangle.Align := TAlignLayout.Client;
    Rectangle.XRadius := 6;
    Rectangle.YRadius := 6;
    Rectangle.Margins.Rect := TRectF.Create(16, 4, 16, 4);
    Rectangle.Opacity := 0.95;
    DataItem.AddObject(Rectangle);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Left;
    lbl.Width := 120;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.Black;
    Layout.AddObject(lbl);

    // ColorBox kontrolü
    ColorBox := TColorComboBox.Create(Layout);
    ColorBox.Align := TAlignLayout.Client;
    ColorBox.Margins.Left := 8;
    ColorBox.Color := ASelectedColor;
    Layout.AddObject(ColorBox);

    Result := ColorBox;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', 50, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddColorBoxField Hatası: ' + E.Message);
  end;
end;

procedure TExpandableListView.ExpandHeader(AHeaderInfo: THeaderInfo);
begin
  if not AHeaderInfo.IsExpanded then
    ToggleHeaderSection(AHeaderInfo);
end;


function TExpandableListView.ExportToJSON: string;
var
  RootObj, HeaderObj, FieldsObj, ValueObj: TJSONObject;
  HeaderInfo: THeaderInfo;
  Component: TComponent;
  ComponentName: string;
  i, j, k: Integer;
  ChildItem: TListBoxItem;
  ItemCounter: Integer; // Tüm başlıklar için ortak sayaç
begin
  RootObj := TJSONObject.Create;
  ItemCounter := 0; // Sayacı başlat

  try
    // Her başlık için
    for i := 0 to FHeaders.Count - 1 do
    begin
      HeaderInfo := FHeaders[i];
      HeaderObj := TJSONObject.Create;

      // Başlık bilgilerini ekle
      HeaderObj.AddPair('HeaderIndex', TJSONNumber.Create(HeaderInfo.ImageIndex));
      HeaderObj.AddPair('HeaderColor', TJSONString.Create(ColorToString(HeaderInfo.Color)));

      // SVG verisi varsa ekle
      if HeaderInfo.SVGData <> '' then
        HeaderObj.AddPair('SVGData', TJSONString.Create(HeaderInfo.SVGData));

      // Alanları ekle
      FieldsObj := TJSONObject.Create;

      // Her bir alt öğe için
      for j := 0 to HeaderInfo.ChildItems.Count - 1 do
      begin
        ChildItem := HeaderInfo.ChildItems[j];

        // Öğe adını belirle - artık başlığa özgü değil, genel sayaç kullanıyor
        ComponentName := 'item_' + IntToStr(ItemCounter);
        Inc(ItemCounter); // Her öğe için sayacı artır

        // Her bir çocuk öğe içindeki kontrolleri bul
        for k := 0 to ChildItem.ComponentCount - 1 do
        begin
          if ChildItem.Components[k] is TLayout then
          begin
            // Layout içindeki bileşenleri işle
            ProcessLayoutComponents(TLayout(ChildItem.Components[k]), ComponentName, FieldsObj);
          end
          else if IsValidComponent(ChildItem.Components[k]) then
          begin
            // Doğrudan bileşeni işle
            ProcessComponent(ChildItem.Components[k], ComponentName, FieldsObj);
          end;
        end;
      end;

      HeaderObj.AddPair('Fields', FieldsObj);
      RootObj.AddPair(HeaderInfo.Title, HeaderObj);
    end;

    Result := RootObj.ToString;
  finally
    RootObj.Free;
  end;
end;


 // Layout içindeki bileşenleri işleyen yardımcı fonksiyon
procedure TExpandableListView.ProcessLayoutComponents(Layout: TLayout; const FieldName: string; FieldsObj: TJSONObject);
var
  i: Integer;
  Component: TComponent;
  ValueObj: TJSONObject;
  LabelFound: Boolean;
  LabelText: string;
begin
  // Layout içindeki tüm etiketleri tara ve metin değerlerini al
  for i := 0 to Layout.ComponentCount - 1 do
  begin
    Component := Layout.Components[i];

    // Etiket (Label) bileşenlerini atla - bunlar ayrı işlenecek
    if Component is TLabel then
      Continue;

    // Layout içinde başka layout varsa, onu da işle
    if Component is TLayout then
    begin
      // Önemli: Alt layout için de aynı alan adını kullan
      ProcessLayoutComponents(TLayout(Component), FieldName, FieldsObj);
      Continue;
    end;

    // Geçerli bir bileşense işle
    if IsValidComponent(Component) then
    begin
      // Bileşen adını belirle - artık tip + indeks eklemiyoruz
      // Doğrudan FieldName kullanıyoruz (item_X formatında)

      // Bileşeni işle
      ValueObj := CreateValueObjectForComponent(Component);
      if ValueObj <> nil then
      begin
        // Bu bileşene ait bir etiket var mı kontrol et
        LabelText := FindLabelTextForComponent(Component);
        if LabelText <> '' then
          ValueObj.AddPair('labelText', TJSONString.Create(LabelText));

        // Artık bileşen adını doğrudan FieldName olarak kullanıyoruz
        FieldsObj.AddPair(FieldName, ValueObj);
        Break; // Her alan için bir kontrol yeterli
      end;
    end;
  end;
end;



// Bileşenin geçerli olup olmadığını kontrol eden fonksiyon
function TExpandableListView.IsValidComponent(Component: TComponent): Boolean;
begin
  Result := (Component is TEdit) or (Component is TNumberBox) or
    (Component is TCheckBox) or (Component is TSwitch) or
    (Component is TComboBox) or (Component is TColorComboBox) or
    (Component is TMemo) or (Component is TRadioButton) or

    (Component is TTrackBar);
  // (Component is TDateTimePicker);
end;

// Bileşen için değer nesnesi oluşturan fonksiyon
function TExpandableListView.CreateValueObjectForComponent(Component: TComponent): TJSONObject;
var
  ValueObj: TJSONObject;
  Items: TJSONArray;
  k: Integer;
  ComponentType: string;
  LabelText: string;
  Control: TControl;
begin
  ValueObj := TJSONObject.Create;

  try
    // Bileşen tipini belirle
    ComponentType := Component.ClassName;
    ValueObj.AddPair('ComponentType', TJSONString.Create(ComponentType));

    // Etiket metnini bul
    LabelText := FindLabelTextForComponent(Component);
    if LabelText <> '' then
      ValueObj.AddPair('labelText', TJSONString.Create(LabelText));

    if Component is TComboBox then
    begin
      var ComboBox := TComboBox(Component);
      Items := TJSONArray.Create;

      // ComboBox öğelerini ekle
      for k := 0 to ComboBox.Items.Count - 1 do
        Items.Add(ComboBox.Items[k]);

      ValueObj.AddPair('ValueType', TJSONString.Create('ComboBox'));
      ValueObj.AddPair('Items', Items);
      ValueObj.AddPair('SelectedIndex', TJSONNumber.Create(ComboBox.ItemIndex));

      if ComboBox.ItemIndex >= 0 then
        ValueObj.AddPair('Value', TJSONString.Create(ComboBox.Items[ComboBox.ItemIndex]))
      else
        ValueObj.AddPair('Value', TJSONString.Create(''));
    end
    else if Component is TEdit then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('String'));
      ValueObj.AddPair('Value', TJSONString.Create(TEdit(Component).Text));
    end
    else if Component is TMemo then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Memo'));
      ValueObj.AddPair('Value', TJSONString.Create(TMemo(Component).Text));
    end
    else if Component is TNumberBox then
    begin
      var NumBox := TNumberBox(Component);


      if  (NumBox.ValueType) = TNumValueType.Integer then
        ValueObj.AddPair('ValueType', TJSONString.Create('Integer'))
      else
        ValueObj.AddPair('ValueType', TJSONString.Create('Float'));

      ValueObj.AddPair('Value', TJSONNumber.Create(NumBox.Value));
      ValueObj.AddPair('Min', TJSONNumber.Create(NumBox.Min));
      ValueObj.AddPair('Max', TJSONNumber.Create(NumBox.Max));
      ValueObj.AddPair('DecimalDigits', TJSONNumber.Create(NumBox.DecimalDigits));

      // Dikey artırma özelliğini ekle
      if NumBox.Tag = 1 then // Tag=1 dikey artırmayı temsil ediyorsa
        ValueObj.AddPair('VertIncrement', TJSONBool.Create(True))
      else
        ValueObj.AddPair('VertIncrement', TJSONBool.Create(False));
    end
    else if Component is TCheckBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Boolean'));
      ValueObj.AddPair('Value', TJSONBool.Create(TCheckBox(Component).IsChecked));
    end
    else if Component is TSwitch then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Boolean'));
      ValueObj.AddPair('Value', TJSONBool.Create(TSwitch(Component).IsChecked));
    end
    else if Component is TColorComboBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Color'));
      ValueObj.AddPair('Value', TJSONString.Create(ColorToString(TColorComboBox(Component).Color)));
    end
    else if Component is TColorBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Color'));
      ValueObj.AddPair('Value', TJSONString.Create(ColorToString(TColorBox(Component).Color)));
    end
    else if Component is TTrackBar then
    begin
      var TrackBar := TTrackBar(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Integer'));
      ValueObj.AddPair('Value', TJSONNumber.Create(TrackBar.Value));
      ValueObj.AddPair('Min', TJSONNumber.Create(TrackBar.Min));
      ValueObj.AddPair('Max', TJSONNumber.Create(TrackBar.Max));
      ValueObj.AddPair('Frequency', TJSONNumber.Create(TrackBar.Frequency));

      // Yönlendirme özelliğini ekle
      if TrackBar.Orientation = TOrientation.Vertical then
        ValueObj.AddPair('Orientation', TJSONString.Create('Vertical'))
      else
        ValueObj.AddPair('Orientation', TJSONString.Create('Horizontal'));
    end
    else if Component is TDateEdit then
    begin
      var DateEdit := TDateEdit(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Date'));
      ValueObj.AddPair('Value', TJSONString.Create(DateToStr(DateEdit.Date)));

      // Tarih formatını ekle
//      ValueObj.AddPair('Format',  DateToStr(.Create(DateEdit.Date)));
    end
    else if Component is TTimeEdit then
    begin
      var TimeEdit := TTimeEdit(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Time'));
      ValueObj.AddPair('Value', TJSONString.Create(TimeToStr(TimeEdit.Time)));

      // Zaman formatını ekle
//      ValueObj.AddPair('Format', TJSONString.Create(TimeEdit.FormatSettings.ShortTimeFormat));
    end
//    else if Component is TSpinBox then
//    begin
//      var SpinBox := TSpinBox(Component);
//
//      ValueObj.AddPair('ValueType', TJSONString.Create('Integer'));
//      ValueObj.AddPair('Value', TJSONNumber.Create(SpinBox.Value));
//      ValueObj.AddPair('Min', TJSONNumber.Create(SpinBox.Min));
//      ValueObj.AddPair('Max', TJSONNumber.Create(SpinBox.Max));
//      ValueObj.AddPair('Increment', TJSONNumber.Create(SpinBox.Increment));
//    end
    else if Component is TRadioButton then
    begin
      var RadioButton := TRadioButton(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Boolean'));
      ValueObj.AddPair('Value', TJSONBool.Create(RadioButton.IsChecked));
      ValueObj.AddPair('GroupName', TJSONString.Create(RadioButton.GroupName));
    end
    else if Component is TListBox then
    begin
      var ListBox := TListBox(Component);
      Items := TJSONArray.Create;

      // ListBox öğelerini ekle
      for k := 0 to ListBox.Count - 1 do
        Items.Add(ListBox.Items[k]);

      ValueObj.AddPair('ValueType', TJSONString.Create('ListBox'));
      ValueObj.AddPair('Items', Items);
      ValueObj.AddPair('ItemIndex', TJSONNumber.Create(ListBox.ItemIndex));

      if ListBox.ItemIndex >= 0 then
        ValueObj.AddPair('Value', TJSONString.Create(ListBox.Items[ListBox.ItemIndex]))
      else
        ValueObj.AddPair('Value', TJSONString.Create(''));
    end
    else
    begin
      // Desteklenmeyen bileşen tipi
      ValueObj.Free;
      Result := nil;
      Exit;
    end;

    Result := ValueObj;
  except
    on E: Exception do
    begin
      ValueObj.Free;
      Result := nil;
    end;
  end;
end;


// Bileşeni doğrudan işleyen fonksiyon
procedure TExpandableListView.ProcessComponent(Component: TComponent; const FieldName: string; FieldsObj: TJSONObject);
var
  ValueObj: TJSONObject;
begin
  ValueObj := CreateValueObjectForComponent(Component);
  if ValueObj <> nil then
    // Doğrudan FieldName kullanıyoruz
    FieldsObj.AddPair(FieldName, ValueObj);
end;

function TExpandableListView.ExportHeaderToJSON(AHeaderInfo: THeaderInfo): TJSONObject;
var
  HeaderObj: TJSONObject;
  i: Integer;
  ChildItem: TListBoxItem;
  FieldName: string;
  Control: TControl;
  ValueObj: TJSONObject;
  j: Integer;
begin
  HeaderObj := TJSONObject.Create;

  // Başlık öğelerini dolaş
  for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
  begin
    ChildItem := AHeaderInfo.ChildItems[i];

    // Etiket metnini al (TagString'de saklanan)
    FieldName := ChildItem.TagString;
    if FieldName = '' then
      FieldName := 'item_' + IntToStr(i);

    // Her bir çocuk öğe içindeki kontrolleri bul
    for j := 0 to ChildItem.ComponentCount - 1 do
    begin
      if ChildItem.Components[j] is TLayout then
      begin
        // Layout içindeki bileşenleri bul
        var Layout := TLayout(ChildItem.Components[j]);

        // Etiket metnini bul
        for var k := 0 to Layout.ComponentCount - 1 do
        begin
          if Layout.Components[k] is TLabel then
          begin
            FieldName := TLabel(Layout.Components[k]).Text;
            Break;
          end;
        end;

        // Her bir kontrol için değer nesnesi oluştur
        for var k := 0 to Layout.ComponentCount - 1 do
        begin
          if not (Layout.Components[k] is TLabel) and IsValidComponent(Layout.Components[k]) then
          begin
            Control := TControl(Layout.Components[k]);

            ValueObj := CreateValueObjectForComponent(Control);
            if ValueObj <> nil then
            begin
              // Etiket metnini ekle
              ValueObj.AddPair('labelText', TJSONString.Create(FieldName));
              HeaderObj.AddPair(FieldName, ValueObj);
              Break; // Her alan için bir kontrol yeterli
            end;
          end;
        end;
      end;
    end;
  end;

  Result := HeaderObj;
end;


procedure TExpandableListView.CollapseHeader(AHeaderInfo: THeaderInfo);
begin
  if AHeaderInfo.IsExpanded then
    ToggleHeaderSection(AHeaderInfo);
end;

function TExpandableListView.ColorToString(Color: TAlphaColor): string;
begin
  Result := Format('#%.2X%.2X%.2X%.2X', [
    TAlphaColorRec(Color).A, // Alpha
    TAlphaColorRec(Color).R, // Red
    TAlphaColorRec(Color).G, // Green
    TAlphaColorRec(Color).B  // Blue
  ]);  // Boş bir değer üretirse varsayılan renk ata

end;

procedure TExpandableListView.ExpandAll;
var
  i: Integer;
begin
  for i := 0 to FHeaders.Count - 1 do
    ExpandHeader(FHeaders[i]);
end;

procedure TExpandableListView.CollapseAll;
var
  i: Integer;
begin
  for i := 0 to FHeaders.Count - 1 do
    CollapseHeader(FHeaders[i]);
end;

end.
