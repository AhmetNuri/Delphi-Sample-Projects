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
  StrUtils, FMX.DateTimeCtrls, FMX.Memo, FMX.Calendar, FMX.DateTimeCtrls.Types

    , System.DateUtils;
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
    ctColorComboBox, ctRadioButton, ctMemo);
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
    FEnableLogging: Boolean; // Loglama açık/kapalı durumu
    FLogFilePath: string; // Log dosyası yolu
    // Add to private section
    FItemBackgroundColor: TAlphaColor;
    FItemBorderColor: TAlphaColor;
    // label   settings
    FLabelWidth: Single;
    FLabelFontSize: Single;
    FLabelFontFamily: string;
    FLabelTextColor: TAlphaColor;
    FEnableAutoSelect: Boolean; // Otomatik seçim parametresi

    procedure CreateHeaderSection(AHeaderInfo: THeaderInfo);
    procedure HeaderRectangleClick(Sender: TObject);
    procedure ToggleHeaderSection(AHeaderInfo: THeaderInfo);
    procedure ShowChildItems(AHeaderInfo: THeaderInfo; AShow: Boolean);
    function FindListBoxItemIndex(Item: TListBoxItem): Integer;
    procedure UpdateSVGIcon(AHeaderInfo: THeaderInfo);
    function HexToTAlphaColor(const HexColor: string): TAlphaColor;
    function ColorToString(Color: TAlphaColor): string;
    procedure DebugLog(const AMessage: string);
    function CreateBackgroundRectangle(Parent: TListBoxItem): TRectangle;
    function CreateFieldLabel(Parent: TFmxObject; const AFieldName: string): TLabel;
    procedure SelectControl(AControl: TControl);
    procedure AddControlEventHandlers(AControl: TControl);
     procedure HandleControlEnter(Sender: TObject); // OnEnter için bir metot

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
    function AddRadioButtonField(AHeaderInfo: THeaderInfo;
      const AFieldName: string; AValue: Boolean;
      const AGroupName: string = 'DefaultRadioGroup'): TRadioButton;
    function AddMemoField(AHeaderInfo: THeaderInfo; const AFieldName: string;
      const AValue: string = ''; AHeight: Single = 80): TMemo;

    // Add the declaration here
    function GetSelectedRadioButtonInGroup(const AGroupName: string)
      : TRadioButton;
    function GetSelectedRadioButtonLabelInGroup(const AGroupName
      : string): string;
    // SVG ikonu ayarlamak için
    procedure SetHeaderSVG(AHeaderInfo: THeaderInfo; const ASVGData: string);
    function FindLabelTextForComponent(Component: TComponent): string;

    procedure ExpandHeader(AHeaderInfo: THeaderInfo);
    procedure CollapseHeader(AHeaderInfo: THeaderInfo);
    procedure ExpandAll;
    procedure CollapseAll;
    function GenerateComponentName(Component: TComponent;
      const BaseName: string; Index: Integer): string;
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
    property EnableLogging: Boolean read FEnableLogging write FEnableLogging;
    property LogFilePath: string read FLogFilePath write FLogFilePath;
    // Add these properties to the published section of TExpandableListView class
    property ItemBackgroundColor: TAlphaColor read FItemBackgroundColor
      write FItemBackgroundColor default TAlphaColorRec.White;
    property ItemBorderColor: TAlphaColor read FItemBorderColor
      write FItemBorderColor default $FFE0E0E0;
// Published bölümüne eklenecek özellikler
    property LabelWidth: Single read FLabelWidth write FLabelWidth ;
    property LabelFontSize: Single read FLabelFontSize write FLabelFontSize   ;
    property LabelFontFamily: string read FLabelFontFamily write FLabelFontFamily;
    property LabelTextColor: TAlphaColor read FLabelTextColor write FLabelTextColor  ;

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

  FreeAndNil(ChildItems);
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
  // Yeni renk özellikleri için varsayılan değerler
  FItemBackgroundColor := TAlphaColorRec.Black;
  FItemBorderColor := $FFE0E0E0;

  // Etiket özellikleri için varsayılan değerler
  FLabelWidth := 120;
  FLabelFontSize := 14;
  FLabelFontFamily := 'Segoe UI';
  FLabelTextColor := TAlphaColorRec.White;

  FEnableAutoSelect := True; // Varsayılan olarak otomatik seçim etkin

  // Loglama için varsayılan ayarlar
  FEnableLogging := False;
  FLogFilePath := ('ExpandableListView.log');

end;

function TExpandableListView.CreateBackgroundRectangle(Parent: TListBoxItem)
  : TRectangle;
begin
  Result := TRectangle.Create(Parent);
  Result.Fill.Color := FItemBackgroundColor;
  Result.Stroke.Kind := TBrushKind.Solid;
  Result.Stroke.Color := FItemBorderColor;
  Result.Stroke.Thickness := 1;
  Result.Align := TAlignLayout.Client;
  Result.XRadius := 4;
  Result.YRadius := 4;
  Result.Margins.Rect := TRectF.Create(8, 2, 8, 2);
  Result.Opacity := 0.8;
  Parent.AddObject(Result);
end;

function TExpandableListView.CreateFieldLabel(Parent: TFmxObject;
  const AFieldName: string): TLabel;
begin
  Result := TLabel.Create(Parent);
  Result.Align := TAlignLayout.Left;
  Result.Width := FLabelWidth;
  Result.Text := AFieldName;
  Result.StyledSettings := [];
  Result.TextSettings.Font.Size := FLabelFontSize;
  Result.TextSettings.Font.Family := FLabelFontFamily;
  Result.TextSettings.FontColor := FLabelTextColor;
  Parent.AddObject(Result);
end;

procedure TExpandableListView.DebugLog(const AMessage: string);

var
  LogMessage: string;
  LogFile: TextFile;
begin
  // Eğer loglama etkinse, dosyaya da yaz
  if FEnableLogging then
  begin
    try
      LogMessage := Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss',
        Now), AMessage]);

      AssignFile(LogFile, FLogFilePath);
      if FileExists(FLogFilePath) then
        Append(LogFile)
      else
        Rewrite(LogFile);

      WriteLn(LogFile, LogMessage);
      CloseFile(LogFile);
    except
      // Log yazma hatalarını sessizce geç
    end;
  end;

end;

destructor TExpandableListView.Destroy;
var
  i: Integer;
begin
  // HeaderInfo nesnelerini temizle
  for i := 0 to FHeaders.Count - 1 do
    FreeAndNil(FHeaders[i]);

  FreeAndNil(FHeaders);
  inherited;
end;

procedure TExpandableListView.Loaded;
begin
  inherited;
  // Component tamamen yüklendiğinde çağrılır
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

function TExpandableListView.AddMemoField(AHeaderInfo: THeaderInfo;
  const AFieldName: string; const AValue: string = '';
  AHeight: Single = 80): TMemo;
var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  Memo: TMemo;
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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := TLabel.Create(Layout);
    lbl.Align := TAlignLayout.Top;
    lbl.Height := 20;
    lbl.Text := AFieldName;
    lbl.StyledSettings := [];
    lbl.TextSettings.Font.Size := 14;
    lbl.TextSettings.Font.Family := 'Segoe UI';
    lbl.TextSettings.FontColor := TAlphaColorRec.White;
    Layout.AddObject(lbl);

    // Memo kontrolü
    Memo := TMemo.Create(Layout);
    Memo.Align := TAlignLayout.Client;
    Memo.Margins.Top := 8;
    Memo.StyleLookup := 'memostyle';
    Memo.Text := AValue;
    Memo.Height := AHeight;
    AddControlEventHandlers(Memo); // OnEnter olay işleyicilerini ekle

    Layout.AddObject(Memo);

    Result := Memo;

    // Eğer başlık açıksa, yeni eklenen öğeyi hemen göster
    if AHeaderInfo.IsExpanded then
    begin
      DataItem.Visible := True;
      DataItem.Opacity := 0;
      DataItem.Height := 0;

      TAnimator.AnimateFloat(DataItem, 'Height', AHeight + 35, 0.3);
      TAnimator.AnimateFloat(DataItem, 'Opacity', 1, 0.3);
    end;
  except
    on E: Exception do
      raise Exception.Create('AddMemoField Hatası: ' + E.Message);
  end;
end;

function TExpandableListView.FindLabelTextForComponent
  (Component: TComponent): string;
var
  ParentComponent: TComponent;
  i: Integer;
  Label1: TLabel;
  Control: TControl;
begin
  Result := '';

  // Component'i TControl'e dönüştür (eğer mümkünse)
  if not(Component is TControl) then
    Exit;

  Control := TControl(Component);

  // Bileşenin ebeveyn bileşenini bul (genellikle bir Layout)
  ParentComponent := Component.Owner;
  if not(ParentComponent is TLayout) then
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

function TExpandableListView.GenerateComponentName(Component: TComponent;
  const BaseName: string; Index: Integer): string;
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

function TExpandableListView.GetSelectedRadioButtonInGroup(const AGroupName
  : string): TRadioButton;
var
  i, j: Integer;
  HeaderInfo: THeaderInfo;
  ChildItem: TListBoxItem;
  Component: TComponent;
begin
  Result := nil;

  // Tüm başlıkları ve alt öğeleri döngüye alarak kontrol et
  for i := 0 to FHeaders.Count - 1 do
  begin
    HeaderInfo := FHeaders[i];

    for j := 0 to HeaderInfo.ChildItems.Count - 1 do
    begin
      ChildItem := HeaderInfo.ChildItems[j];

      // Alt öğe içindeki tüm komponentleri kontrol et
      for var k := 0 to ChildItem.ComponentCount - 1 do
      begin
        Component := ChildItem.Components[k];

        // TLayout içindeki komponentleri kontrol et
        if Component is TLayout then
        begin
          for var m := 0 to TLayout(Component).ComponentCount - 1 do
          begin
            var
            InnerComponent := TLayout(Component).Components[m];

            // RadioButton'u bul ve grup adını kontrol et
            if InnerComponent is TRadioButton then
            begin
              var
              RadioBtn := TRadioButton(InnerComponent);

              if (RadioBtn.GroupName = AGroupName) and RadioBtn.IsChecked then
              begin
                Result := RadioBtn;
                Exit; // Bulunca çık
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  // Bulunamazsa nil döndür
  DebugLog('Grup adı "' + AGroupName + '" için seçili RadioButton bulunamadı.');
end;

// Seçilen RadioButton'un etiketini almak için yardımcı fonksiyon
function TExpandableListView.GetSelectedRadioButtonLabelInGroup(const AGroupName
  : string): string;
var
  RadioBtn: TRadioButton;
begin
  Result := '';
  RadioBtn := GetSelectedRadioButtonInGroup(AGroupName);

  if RadioBtn <> nil then
    Result := FindLabelTextForComponent(RadioBtn);
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

procedure TExpandableListView.HandleControlEnter(Sender: TObject);
begin
  // Odaklanılan kontrolü seç
  if Sender is TControl then
    SelectControl(TControl(Sender));
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
  i, j, k: Integer;
  Arrow: TPath;
  Layout: TLayout;
  ChildItem: TListBoxItem;
  Component: TComponent;
  HasMemo: Boolean;
  ItemHeight: Single;
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
        for j := 0 to Layout.ComponentCount - 1 do
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
        ChildItem := AHeaderInfo.ChildItems[i];
        ChildItem.Height := 0;
        ChildItem.Opacity := 0;
        ChildItem.Visible := True;

        // TMemo kontrolü var mı diye kontrol et
        HasMemo := False;
        ItemHeight := 50; // Varsayılan yükseklik

        // ChildItem içindeki tüm bileşenleri kontrol et
        for j := 0 to ChildItem.ComponentCount - 1 do
        begin
          Component := ChildItem.Components[j];

          // Eğer Layout bulunduysa onun içindeki bileşenleri kontrol et
          if Component is TLayout then
          begin
            Layout := TLayout(Component);
            for k := 0 to Layout.ComponentCount - 1 do
            begin
              // Memo kontrolü bulduysa
              if Layout.Components[k] is TMemo then
              begin
                HasMemo := True;
                var
                Memo := TMemo(Layout.Components[k]);
                // Memo içeriğine göre yükseklik hesapla (minimum 100 piksel)
                ItemHeight := Max(100, Memo.Lines.Count * 20 + 40);
                // Her satır için 20 piksel + ekstra alan
                Break;
              end;
            end;
          end;

          if HasMemo then
            Break;
        end;

        // Bulunan bileşene göre yüksekliği ayarla
        TAnimator.AnimateFloat(ChildItem, 'Height', ItemHeight, 0.3);
        TAnimator.AnimateFloat(ChildItem, 'Opacity', 1, 0.3);
      end;
    end
    else
    begin
      // Alt öğeleri gizle - iyileştirilmiş versiyon
      for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
      begin
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

procedure TExpandableListView.SelectControl(AControl: TControl);
begin
  if not FEnableAutoSelect then
    Exit; // Eğer otomatik seçim devre dışıysa, herhangi bir işlem yapma
   TThread.CreateAnonymousThread(
    procedure
    begin
      // Kısa bir gecikme ekleyerek kontrolün hazır olmasını sağlayalım
      Sleep(50);
      TThread.Synchronize(nil,
        procedure
        begin
          if AControl is TEdit then
          begin
            var Edit := TEdit(AControl);
            Edit.SetFocus;
            if Edit.CanFocus then
            begin
              Edit.SelStart := 0;
              Edit.SelLength := Length(Edit.Text);
            end;
          end
          else if AControl is TMemo then
          begin
            var Memo := TMemo(AControl);
            Memo.SetFocus;
            if Memo.CanFocus then
            begin
              Memo.SelStart := 0;
              Memo.SelLength := Length(Memo.Text);
            end;
          end
          else if AControl is TNumberBox then
          begin
            var NumBox := TNumberBox(AControl);
            NumBox.SetFocus;
            if NumBox.CanFocus then
            begin
              // NumberBox için özel bir seçim işlemi gerekiyorsa burada yapılabilir
              NumBox.SelectAll;
            end;
          end;
        end);
    end).Start;
end;
// SVG verisi ayarlama
procedure TExpandableListView.SetHeaderSVG(AHeaderInfo: THeaderInfo;
const ASVGData: string);
begin
  AHeaderInfo.SVGData := ASVGData;
  UpdateSVGIcon(AHeaderInfo);
end;

procedure TExpandableListView.AddDataField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AControlType: TControlType);
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

  // Arkaplan için ortak fonksiyonu kullan
  Rectangle := CreateBackgroundRectangle(DataItem);

  // Ana layout
  Layout := TLayout.Create(DataItem);
  Layout.Align := TAlignLayout.Client;
  Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
  DataItem.AddObject(Layout);

  // Etiket
lbl := CreateFieldLabel(Layout, AFieldName);

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
    ctMemo:
      AddMemoField(AHeaderInfo, AFieldName, ''); // Memo için destek ekledik
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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
    lbl := CreateFieldLabel(Layout, AFieldName);


    // Edit kontrolü
    Edit := TEdit.Create(Layout);
    Edit.Align := TAlignLayout.Client;
    Edit.Margins.Left := 8;
    Edit.StyleLookup := 'editstyle';
    Edit.Text := AValue;
    Layout.AddObject(Edit);
    AddControlEventHandlers(Edit); // OnEnter olay işleyicilerini ekle

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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
  lbl := CreateFieldLabel(Layout, AFieldName);


    // NumberBox kontrolü
    NumBox := TNumberBox.Create(Layout);
    NumBox.Align := TAlignLayout.Client;
    NumBox.Margins.Left := 8;
    NumBox.Min := AMin;
    NumBox.Max := AMax;
    NumBox.VertIncrement := 0;
    NumBox.HorzIncrement := 0;

   AddControlEventHandlers(NumBox); // OnEnter olay işleyicilerini ekle

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

function TExpandableListView.AddRadioButtonField(AHeaderInfo: THeaderInfo;
const AFieldName: string; AValue: Boolean;
const AGroupName: string = 'DefaultRadioGroup'): TRadioButton;

var
  DataItem: TListBoxItem;
  Layout: TLayout;
  Rectangle: TRectangle;
  lbl: TLabel;
  RadioBtn: TRadioButton;
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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
  lbl := CreateFieldLabel(Layout, AFieldName);



    // CheckBox kontrolü
    RadioBtn := TRadioButton.Create(Layout);
    RadioBtn.Align := TAlignLayout.Right;
    RadioBtn.Width := 60;
    RadioBtn.StyleLookup := 'radiobuttonstyle';
    RadioBtn.IsChecked := AValue;
    RadioBtn.GroupName := 'RadioGroup';
    RadioBtn.Name := GenerateComponentName(RadioBtn, AFieldName, Random(10000));

    Layout.AddObject(RadioBtn);
    RadioBtn.Text := ''; // RadioButton metnini boş yap

    Result := RadioBtn;

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


  // RadioButton kontrolü
  // RadioBtn := TRadioButton.Create(Layout);
  // RadioBtn.Align := TAlignLayout.Right;
  // RadioBtn.Width := 40;
  // RadioBtn.IsChecked := AValue;
  // // RadioBtn.StyledSettings := [];
  // RadioBtn.Text := ''; // RadioButton metnini boş yap
  // RadioBtn.GroupName := 'RadioGroup' + IntToStr(Random(1000));
  // // Varsayılan grup adı

  // Unique isim oluştur
  // RadioBtn.Name := GenerateComponentName(RadioBtn, AFieldName, Random(10000));

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
    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
  lbl := CreateFieldLabel(Layout, AFieldName);



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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
 lbl := CreateFieldLabel(Layout, AFieldName);



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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
 lbl := CreateFieldLabel(Layout, AFieldName);



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

procedure TExpandableListView.AddControlEventHandlers(AControl: TControl);
begin
 // Bileşenlerin OnEnter olayını ayarlar
  // Bileşenlerin OnEnter olayını ayarlar
  if AControl is TEdit then
    TEdit(AControl).OnEnter := HandleControlEnter
  else if AControl is TMemo then
    TMemo(AControl).OnEnter := HandleControlEnter
  else if AControl is TNumberBox then
    TNumberBox(AControl).OnEnter := HandleControlEnter;

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

    // Arkaplan için ortak fonksiyonu kullan
    Rectangle := CreateBackgroundRectangle(DataItem);

    // Ana layout
    Layout := TLayout.Create(DataItem);
    Layout.Align := TAlignLayout.Client;
    Layout.Padding.Rect := TRectF.Create(16, 4, 16, 4);
    DataItem.AddObject(Layout);

    // Etiket
 lbl := CreateFieldLabel(Layout, AFieldName);



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

procedure TExpandableListView.CollapseHeader(AHeaderInfo: THeaderInfo);
begin
  if AHeaderInfo.IsExpanded then
    ToggleHeaderSection(AHeaderInfo);
end;

function TExpandableListView.ColorToString(Color: TAlphaColor): string;
begin
  Result := Format('#%.2X%.2X%.2X%.2X', [TAlphaColorRec(Color).A, // Alpha
  TAlphaColorRec(Color).R, // Red
  TAlphaColorRec(Color).G, // Green
  TAlphaColorRec(Color).B // Blue
    ]); // Boş bir değer üretirse varsayılan renk ata

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
