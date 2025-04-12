 unit ExpandableListViewJSON;

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
  ,ExpandableListView
;
type


  TExpandableListViewJSONHelper = class
  private
    FExpandableListView: TExpandableListView;
    FEnableLogging: Boolean; // Loglama açık/kapalı durumu
    FLogFilePath: string; // Log dosyası yolu

    function HexToTAlphaColor(const HexColor: string): TAlphaColor;
    function ColorToString(Color: TAlphaColor): string;
    procedure ProcessComponent(Component: TComponent; const FieldName: string;
      FieldsObj: TJSONObject);
    function CreateValueObjectForComponent(Component: TComponent): TJSONObject;
    function IsValidComponent(Component: TComponent): Boolean;
    procedure ProcessLayoutComponents(Layout: TLayout; const FieldName: string;
      FieldsObj: TJSONObject);
    function FindLabelTextForComponent(Component: TComponent): string;
    function GenerateComponentName(Component: TComponent; const BaseName: string; Index: Integer): string;
    procedure DebugLog(const AMessage: string);

  public
    constructor Create(AExpandableListView: TExpandableListView);
    destructor Destroy; override;

    // JSON işlemleri için yeni fonksiyonlar
    function LoadFromJSON(const AJSONString: string): Boolean;
    function LoadFromJSONFile(const AFileName: string): Boolean;
    function LoadHeaderFromJSON(const ATitle: string; AJSONObj: TJSONObject)
      : THeaderInfo;

    procedure LoadFieldsFromJSON(HeaderInfo: THeaderInfo;
      FieldsObj: TJSONObject);
    function   UpdateValuesFromJSON(const AJSONString: string): Boolean;

    function      UpdateOnlyValuesFromJSON(const AJSONString: string): Boolean;


    function ExportToJSON: string;
    function ExportValuesToJSON: string;

    // Belirtilen başlık için JSON çıktısı üreten fonksiyon
    function ExportHeaderToJSON(AHeaderInfo: THeaderInfo): TJSONObject;
  end;

implementation

uses
  System.IOUtils;


{ TExpandableListViewJSONHelper }

function TExpandableListViewJSONHelper.ColorToString(
  Color: TAlphaColor): string;
begin
  Result := Format('#%.2X%.2X%.2X%.2X', [TAlphaColorRec(Color).A, // Alpha
  TAlphaColorRec(Color).R, // Red
  TAlphaColorRec(Color).G, // Green
  TAlphaColorRec(Color).B // Blue
    ]); // Boş bir değer üretirse varsayılan renk ata

end;

constructor TExpandableListViewJSONHelper.Create(
  AExpandableListView: TExpandableListView);
begin
  FExpandableListView := AExpandableListView;
end;



// Bileşen için değer nesnesi oluşturan fonksiyon
function TExpandableListViewJSONHelper.CreateValueObjectForComponent(Component: TComponent): TJSONObject;
var
  ValueObj: TJSONObject;
  Items: TJSONArray;
  k: Integer;
  UIType: string;
  LabelText: string;
  Control: TControl;
  RadioButton: TRadioButton;
  ImagePath: string;
begin
  ValueObj := TJSONObject.Create;

  try
    // Bileşen tipini belirle
    UIType := Component.ClassName;
    ValueObj.AddPair('UIType', TJSONString.Create(UIType));

    // Etiket metnini bul
    LabelText := FindLabelTextForComponent(Component);
    if LabelText <> '' then
      ValueObj.AddPair('labelText', TJSONString.Create(LabelText));

    if Component is TComboBox then
    begin
      var
      ComboBox := TComboBox(Component);
      Items := TJSONArray.Create;

      // ComboBox öğelerini ekle
      for k := 0 to ComboBox.Items.Count - 1 do
        Items.Add(ComboBox.Items[k]);

      ValueObj.AddPair('ValueType', TJSONString.Create('ComboBox'));
      ValueObj.AddPair('Items', Items);
      ValueObj.AddPair('SelectedIndex', TJSONNumber.Create(ComboBox.ItemIndex));

      if ComboBox.ItemIndex >= 0 then
        ValueObj.AddPair('Value',
          TJSONString.Create(ComboBox.Items[ComboBox.ItemIndex]))
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
      // TMemo için düzgün işleme
      ValueObj.AddPair('ValueType', TJSONString.Create('Memo'));
      ValueObj.AddPair('Value', TJSONString.Create(TMemo(Component).Text));
      ValueObj.AddPair('Height', TJSONNumber.Create(TMemo(Component).Height));
    end
    else if Component is TNumberBox then
    begin
      var
      NumBox := TNumberBox(Component);

      if (NumBox.ValueType) = TNumValueType.Integer then
        ValueObj.AddPair('ValueType', TJSONString.Create('Integer'))
      else
        ValueObj.AddPair('ValueType', TJSONString.Create('Float'));

      ValueObj.AddPair('Value', TJSONNumber.Create(NumBox.Value));
      ValueObj.AddPair('Min', TJSONNumber.Create(NumBox.Min));
      ValueObj.AddPair('Max', TJSONNumber.Create(NumBox.Max));
      ValueObj.AddPair('DecimalDigits',
        TJSONNumber.Create(NumBox.DecimalDigits));

      // Dikey artırma özelliğini ekle
      if NumBox.Tag = 1 then // Tag=1 dikey artırmayı temsil ediyorsa
        ValueObj.AddPair('VertIncrement', TJSONBool.Create(True))
      else
        ValueObj.AddPair('VertIncrement', TJSONBool.Create(False));
    end
    else if Component is TCheckBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Boolean'));
      ValueObj.AddPair('Value', TJSONBool.Create(TCheckBox(Component)
        .IsChecked));
    end
    else if Component is TSwitch then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Boolean'));
      ValueObj.AddPair('Value', TJSONBool.Create(TSwitch(Component).IsChecked));
    end
    else if Component is TColorComboBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Color'));
      ValueObj.AddPair('Value',
        TJSONString.Create(ColorToString(TColorComboBox(Component).Color)));
    end
    else if Component is TColorBox then
    begin
      ValueObj.AddPair('ValueType', TJSONString.Create('Color'));
      ValueObj.AddPair('Value',
        TJSONString.Create(ColorToString(TColorBox(Component).Color)));
    end
    else if Component is TTrackBar then
    begin
      var
      TrackBar := TTrackBar(Component);

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
      var
      DateEdit := TDateEdit(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Date'));
      ValueObj.AddPair('Value', TJSONString.Create(DateToStr(DateEdit.Date)));
    end
    else if Component is TTimeEdit then
    begin
      var
      TimeEdit := TTimeEdit(Component);

      ValueObj.AddPair('ValueType', TJSONString.Create('Time'));
      ValueObj.AddPair('Value', TJSONString.Create(TimeToStr(TimeEdit.Time)));
    end
    else if Component is TRadioButton then
    begin
      RadioButton := TRadioButton(Component);
      ValueObj.AddPair('UIType', TJSONString.Create('TRadioButton'));
      ValueObj.AddPair('Value', TJSONBool.Create(RadioButton.IsChecked));
      // GroupName bilgisini sakla
      ValueObj.AddPair('GroupName', TJSONString.Create(RadioButton.GroupName));

      // RadioButton için resim yolunu kontrol et ve ekle
      if Assigned(FExpandableListView.FRadioButtonImagePaths) and
         FExpandableListView.FRadioButtonImagePaths.TryGetValue(RadioButton, ImagePath) then
      begin
        if ImagePath <> '' then
          ValueObj.AddPair('ImagePath', TJSONString.Create(ImagePath));
      end;
    end
    else if Component is TListBox then
    begin
      var
      ListBox := TListBox(Component);
      Items := TJSONArray.Create;

      // ListBox öğelerini ekle
      for k := 0 to ListBox.Count - 1 do
        Items.Add(ListBox.Items[k]);

      ValueObj.AddPair('ValueType', TJSONString.Create('ListBox'));
      ValueObj.AddPair('Items', Items);
      ValueObj.AddPair('ItemIndex', TJSONNumber.Create(ListBox.ItemIndex));

      if ListBox.ItemIndex >= 0 then
        ValueObj.AddPair('Value',
          TJSONString.Create(ListBox.Items[ListBox.ItemIndex]))
      else
        ValueObj.AddPair('Value', TJSONString.Create(''));
    end
    else
    begin
      // Desteklenmeyen bileşen tipi
      FreeAndNil(ValueObj);
      Result := nil;
      Exit;
    end;

    Result := ValueObj;
  except
    on E: Exception do
    begin
      FreeAndNil(ValueObj);
      Result := nil;
    end;
  end;
end;



procedure TExpandableListViewJSONHelper.DebugLog(const AMessage: string);

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

destructor TExpandableListViewJSONHelper.Destroy;
begin
  inherited;
end;



function TExpandableListViewJSONHelper.ExportHeaderToJSON(AHeaderInfo: THeaderInfo)
  : TJSONObject;
var
  HeaderObj: TJSONObject;
  FieldsObj: TJSONObject;
  i, ComponentCount: Integer;
  Component: TComponent;
  FieldName: string;
  Layout: TLayout;
  LayoutChildrenAdded: Boolean;
begin
  HeaderObj := TJSONObject.Create;

  try
    // Başlık bilgileri
    HeaderObj.AddPair('Title', TJSONString.Create(AHeaderInfo.Title));
    HeaderObj.AddPair('HeaderIndex',
      TJSONNumber.Create(AHeaderInfo.ImageIndex));
    HeaderObj.AddPair('HeaderColor',
      TJSONString.Create(ColorToString(AHeaderInfo.Color)));

    // SVG verisi varsa ekle
    if AHeaderInfo.SVGData <> '' then
      HeaderObj.AddPair('SVGData', TJSONString.Create(AHeaderInfo.SVGData));

    // Fields nesnesi
    FieldsObj := TJSONObject.Create;

    // Başlığa ait içerik nesnelerini (EditBox, CheckBox vb.) bul ve ekle
    for i := 0 to AHeaderInfo.ChildItems.Count - 1 do
    begin
      if not(AHeaderInfo.ChildItems[i] is TListBoxItem) then
        Continue;

      LayoutChildrenAdded := False;
      ComponentCount := AHeaderInfo.ChildItems[i].ComponentCount;

      // ListBoxItem'in içindeki bileşenleri dolaş
      // (genellikle bir Layout içinde olurlar)
      for var j := 0 to ComponentCount - 1 do
      begin
        Component := AHeaderInfo.ChildItems[i].Components[j];

        if Component is TLayout then
        begin
          Layout := TLayout(Component);
          // Layout içindeki bileşenleri işle
          ProcessLayoutComponents(Layout, '', FieldsObj);
          LayoutChildrenAdded := True;
          Break;
        end;
      end;

      // Eğer Layout içindeki bileşenler eklenmediyse, doğrudan bileşenleri kontrol et
      if not LayoutChildrenAdded then
      begin
        for var j := 0 to ComponentCount - 1 do
        begin
          Component := AHeaderInfo.ChildItems[i].Components[j];
          if IsValidComponent(Component) then
          begin
            FieldName := FindLabelTextForComponent(Component);
            if FieldName <> '' then
              ProcessComponent(Component, FieldName, FieldsObj);
          end;
        end;
      end;
    end;

    // Fields nesnesini Header'a ekle
    HeaderObj.AddPair('Fields', FieldsObj);

    Result := HeaderObj;
  except
    on E: Exception do
    begin
      FreeAndNil(HeaderObj);
      Result := nil;
    end;
  end;
end;


function TExpandableListViewJSONHelper.ExportToJSON: string;
var
  RootObj: TJSONObject;
  HeadersArray: TJSONArray;
  HeaderObj: TJSONObject;
  i: Integer;
begin
  Result := '';
  RootObj := TJSONObject.Create;
  HeadersArray := TJSONArray.Create;

  try
    // Metadata bilgisi ekle
    var MetadataObj := TJSONObject.Create;
    MetadataObj.AddPair('CreatedAt', TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)));
    MetadataObj.AddPair('CreatedBy', TJSONString.Create('AhmetNuri'));
    MetadataObj.AddPair('Version', TJSONString.Create('1.1')); // TMemo desteği ile sürümü 1.1 yapıldı
    RootObj.AddPair('Metadata', MetadataObj);

    // Her bir başlık için JSON nesnesi oluştur
    for i := 0 to FExpandableListView.FHeaders.Count - 1 do
    begin
      HeaderObj := ExportHeaderToJSON(FExpandableListView.FHeaders[i]);
      HeadersArray.Add(HeaderObj);
    end;

    // Headers array'ini root objeye ekle
    RootObj.AddPair('Headers', HeadersArray);

    // JSON string'ini oluştur
    Result := RootObj.ToString;
  finally
    FreeAndNil(RootObj);
  end;
end;

function TExpandableListViewJSONHelper.ExportValuesToJSON: string;
var
  RootObj, HeaderObj, FieldsObj: TJSONObject;
  HeadersArray: TJSONArray;
  i, j, k: Integer;
  HeaderInfo: THeaderInfo;
  ChildItem: TListBoxItem;
  Component: TComponent;
  FieldName, FieldValue: string;
  Layout: TLayout;
begin
  try
    // Ana JSON nesnelerini oluştur
    RootObj := TJSONObject.Create;
    HeadersArray := TJSONArray.Create;

    // Metadata ekle
    var MetadataObj := TJSONObject.Create;
    MetadataObj.AddPair('CreatedAt', TJSONString.Create(FormatDateTime('yyyy-mm-dd hh:nn:ss', Now)));
    MetadataObj.AddPair('CreatedBy', TJSONString.Create('AhmetNuri'));
    MetadataObj.AddPair('Version', TJSONString.Create('1.1')); // TMemo desteği ile sürümü 1.1 yapıldı
    RootObj.AddPair('Metadata', MetadataObj);

    try
      // Her başlık için işlem yap
      for i := 0 to FExpandableListView.FHeaders.Count - 1 do
      begin
        HeaderInfo := FExpandableListView.FHeaders[i];
        HeaderObj := TJSONObject.Create;
        FieldsObj := TJSONObject.Create;

        // Başlık bilgilerini ekle
        HeaderObj.AddPair('Title', TJSONString.Create(HeaderInfo.Title));
        HeaderObj.AddPair('HeaderIndex', TJSONNumber.Create(HeaderInfo.ImageIndex));
        HeaderObj.AddPair('HeaderColor', TJSONString.Create(ColorToString(HeaderInfo.Color)));

        // SVG verisi varsa ekle
        if HeaderInfo.SVGData <> '' then
          HeaderObj.AddPair('SVGData', TJSONString.Create(HeaderInfo.SVGData));

        // Alt öğelerin kontrollerini dolaş
        for j := 0 to HeaderInfo.ChildItems.Count - 1 do
        begin
          ChildItem := HeaderInfo.ChildItems[j];

          // Önce Layout'ları ara
          for k := 0 to ChildItem.ComponentCount - 1 do
          begin
            if ChildItem.Components[k] is TLayout then
            begin
              Layout := TLayout(ChildItem.Components[k]);

              // Layout içindeki her bir kontrol için
              for var l := 0 to Layout.ComponentCount - 1 do
              begin
                Component := Layout.Components[l];

                // Değer içeren kontrolleri işle
                if IsValidComponent(Component) then
                begin
                  // Kontrol için etiket adını bul
                  FieldName := FindLabelTextForComponent(Component);

                  if FieldName <> '' then
                  begin
                    var FieldObj := TJSONObject.Create;

                    // TMemo için özel işlem
                    if Component is TMemo then
                    begin
                      var MemoField := TMemo(Component);
                      FieldObj.AddPair('UIType', TJSONString.Create('TMemo'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('Memo'));
                      FieldObj.AddPair('Value', TJSONString.Create(MemoField.Text));
                      FieldObj.AddPair('Height', TJSONNumber.Create(MemoField.Height));
                      FieldObj.AddPair('labelText', TJSONString.Create(FieldName));

                      // Alan nesnesini Fields nesnesine ekle
                      FieldsObj.AddPair(FieldName, FieldObj);

                      // TMemo için debug log ekle
                      DebugLog('TMemo processed in ExportValuesToJSON: ' + FieldName);

                      // TMemo işlendikten sonra döngünün geri kalanını atla
                      continue;
                    end;

                    // Kontrol tipine göre değeri al
                    if Component is TEdit then
                      FieldValue := TEdit(Component).Text
                    else if Component is TNumberBox then
                      FieldValue := FloatToStr(TNumberBox(Component).Value)
                    else if Component is TCheckBox then
                      FieldValue := BoolToStr(TCheckBox(Component).IsChecked, True)
                    else if Component is TSwitch then
                      FieldValue := BoolToStr(TSwitch(Component).IsChecked, True)
                    else if Component is TComboBox then
                    begin
                      var ComboBox := TComboBox(Component);
                      if ComboBox.ItemIndex >= 0 then
                        FieldValue := ComboBox.Items[ComboBox.ItemIndex]
                      else
                        FieldValue := '';
                    end
                    else if Component is TColorComboBox then
                      FieldValue := ColorToString(TColorComboBox(Component).Color)
else if Component is TRadioButton then
begin
  var RadioBtn := TRadioButton(Component);
  FieldValue := BoolToStr(RadioBtn.IsChecked, True);

  // Her bir alan için ayrı bir JSON nesnesi oluştur
  if RadioBtn.IsChecked then
  begin
    FieldObj.AddPair('UIType', TJSONString.Create('TRadioButton'));
    FieldObj.AddPair('Value', TJSONBool.Create(True));
    FieldObj.AddPair('GroupName', TJSONString.Create(RadioBtn.GroupName));
    FieldObj.AddPair('labelText', TJSONString.Create(FieldName));
    FieldObj.AddPair('ValueType', TJSONString.Create('Boolean'));

    // Resim yolunu kontrol et
    var ImagePath: string;
    if Assigned(FExpandableListView.FRadioButtonImagePaths) and
       FExpandableListView.FRadioButtonImagePaths.TryGetValue(RadioBtn, ImagePath) then
    begin
      if ImagePath <> '' then
        FieldObj.AddPair('ImagePath', TJSONString.Create(ImagePath));
    end;

    // Alan nesnesini Fields nesnesine ekle
    FieldsObj.AddPair(FieldName, FieldObj);
  end;

  // RadioButton için özel işlem yaptıysak, döngünün geri kalanını atla
  continue;
end
                   else
                      continue; // Desteklenmeyen kontrol tiplerini atla

                    // Kontrol tipine göre UIType ve ValueType ayarla
                    if Component is TEdit then
                    begin
                      FieldObj.AddPair('UIType', TJSONString.Create('TEdit'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('String'));
                      FieldObj.AddPair('Value', TJSONString.Create(FieldValue));
                    end
                    else if Component is TNumberBox then
                    begin
                      var NumBox := TNumberBox(Component);
                      FieldObj.AddPair('UIType', TJSONString.Create('TNumberBox'));

                      if NumBox.ValueType = TNumValueType.Integer then
                      begin
                        FieldObj.AddPair('ValueType', TJSONString.Create('Integer'));
                        FieldObj.AddPair('Value', TJSONNumber.Create(Round(NumBox.Value)));
                      end
                      else
                      begin
                        FieldObj.AddPair('ValueType', TJSONString.Create('Float'));
                        FieldObj.AddPair('Value', TJSONNumber.Create(NumBox.Value));
                      end;

                      FieldObj.AddPair('Min', TJSONNumber.Create(NumBox.Min));
                      FieldObj.AddPair('Max', TJSONNumber.Create(NumBox.Max));
                      FieldObj.AddPair('DecimalDigits', TJSONNumber.Create(NumBox.DecimalDigits));
                      FieldObj.AddPair('VertIncrement', TJSONBool.Create(NumBox.VertIncrementEnabled));
                    end
                    else if Component is TCheckBox then
                    begin
                      FieldObj.AddPair('UIType', TJSONString.Create('TCheckBox'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('Boolean'));
                      FieldObj.AddPair('Value', TJSONBool.Create(TCheckBox(Component).IsChecked));
                    end
                    else if Component is TSwitch then
                    begin
                      FieldObj.AddPair('UIType', TJSONString.Create('TSwitch'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('Boolean'));
                      FieldObj.AddPair('Value', TJSONBool.Create(TSwitch(Component).IsChecked));
                    end
                    else if Component is TComboBox then
                    begin
                      var ComboBox := TComboBox(Component);
                      var ItemsArray := TJSONArray.Create;

                      // ComboBox öğelerini ekle
                      for var m := 0 to ComboBox.Items.Count - 1 do
                        ItemsArray.AddElement(TJSONString.Create(ComboBox.Items[m]));

                      FieldObj.AddPair('UIType', TJSONString.Create('TComboBox'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('ComboBox'));
                      FieldObj.AddPair('Items', ItemsArray);
                      FieldObj.AddPair('SelectedIndex', TJSONNumber.Create(ComboBox.ItemIndex));

                      if ComboBox.ItemIndex >= 0 then
                        FieldObj.AddPair('Value', TJSONString.Create(ComboBox.Items[ComboBox.ItemIndex]))
                      else
                        FieldObj.AddPair('Value', TJSONString.Create(''));
                    end
                    else if Component is TColorComboBox then
                    begin
                      FieldObj.AddPair('UIType', TJSONString.Create('TColorComboBox'));
                      FieldObj.AddPair('ValueType', TJSONString.Create('Color'));
                      FieldObj.AddPair('Value', TJSONString.Create(ColorToString(TColorComboBox(Component).Color)));
                    end;

                    // Etiket metnini ekle
                    FieldObj.AddPair('labelText', TJSONString.Create(FieldName));

                    // Alan nesnesini Fields nesnesine ekle
                    FieldsObj.AddPair(FieldName, FieldObj);
                  end;
                end;
              end;
            end;
          end;
        end;

        // Fields nesnesini Header'a ekle
        HeaderObj.AddPair('Fields', FieldsObj);

        // Header'ı Headers dizisine ekle
        HeadersArray.Add(HeaderObj);
      end;

      // Headers dizisini ana nesneye ekle
      RootObj.AddPair('Headers', HeadersArray);

      // JSON çıktısını döndür
      Result := RootObj.ToString;
    finally
      // Sadece RootObj'yi serbest bırak, alt nesneler otomatik olarak serbest bırakılır
      FreeAndNil(RootObj);
    end;
  except
    on E: Exception do
    begin
      Result := '';
      DebugLog('ExportValuesToJSON Error: ' + E.Message);
    end;
  end;
end;

function TExpandableListViewJSONHelper.FindLabelTextForComponent
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

  // Eğer etiket bulunamadıysa ve bu bir TMemo ise, başka bir strateji dene
  if (Result = '') and (Component is TMemo) then
  begin
    // TMemo için, etiketin herhangi bir konumda olabileceğini düşünerek tüm etiketleri tara
    for i := 0 to TLayout(ParentComponent).ComponentCount - 1 do
    begin
      if TLayout(ParentComponent).Components[i] is TLabel then
      begin
        Label1 := TLabel(TLayout(ParentComponent).Components[i]);

        // Sadece tek bir label varsa, onu kullan
        if Result = '' then
          Result := Label1.Text;

        // Debug log - yardımcı olması için
        DebugLog('Possible label for TMemo: ' + Label1.Text +
                 ' at X:' + FloatToStr(Label1.Position.X) +
                 ' Y:' + FloatToStr(Label1.Position.Y));
      end;
    end;

    // Debug log
    if Result <> '' then
      DebugLog('Selected label for TMemo: ' + Result)
    else
      DebugLog('No label found for TMemo at position X:' +
               FloatToStr(Control.Position.X) + ' Y:' + FloatToStr(Control.Position.Y));
  end;
end;


function TExpandableListViewJSONHelper.GenerateComponentName(Component: TComponent;
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



function TExpandableListViewJSONHelper.HexToTAlphaColor(const HexColor: string)
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


// Bileşenin geçerli olup olmadığını kontrol eden fonksiyon
function TExpandableListViewJSONHelper.IsValidComponent(Component: TComponent): Boolean;
begin
  Result := (Component is TEdit) or (Component is TNumberBox) or
    (Component is TCheckBox) or (Component is TSwitch) or
    (Component is TComboBox) or (Component is TColorComboBox) or
    (Component is TMemo) or (Component is TRadioButton) or

    (Component is TTrackBar);
  // (Component is TDateTimePicker);
end;


procedure TExpandableListViewJSONHelper.LoadFieldsFromJSON(HeaderInfo: THeaderInfo;
  FieldsObj: TJSONObject);
var
  i: Integer;
  FieldName: string;
  FieldValue: TJSONValue;
  ValueObj: TJSONObject;
  ActualValue: TJSONValue;
  Min, Max: Double;
  DecimalDigits: Integer;
  ValueType, UIType: string;
  VertIncrement: Boolean;
  Items: TJSONArray;
  ItemsArray: array of string;
  SelectedIndex: Integer;
  ColorValue: TAlphaColor;
  LabelText: string;
  GroupName: string;

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
      UIType := '';
      VertIncrement := False;
      ActualValue := nil;
      LabelText := ''; // Etiket metni için varsayılan değer
      GroupName := ''; // RadioButton için grup adı

      // JSON nesne kontrolü
      if FieldValue is TJSONObject then
      begin
        ValueObj := TJSONObject(FieldValue);

        // LabelText değerini al (varsa)
        if ValueObj.GetValue('labelText') <> nil then
          LabelText := ValueObj.GetValue('labelText').Value;

        // UIType değerini al
        if ValueObj.GetValue('UIType') <> nil then
          UIType := (ValueObj.GetValue('UIType') as TJSONString).Value;

        // ValueType değerini al
        if ValueObj.GetValue('ValueType') <> nil then
          ValueType := (ValueObj.GetValue('ValueType') as TJSONString).Value;

        // Radio için GroupName değerini al
        if ValueObj.GetValue('GroupName') <> nil then
          GroupName := (ValueObj.GetValue('GroupName') as TJSONString).Value;

        // Değeri al
        ActualValue := ValueObj.GetValue('Value');

        // UIType'a göre uygun kontrol ekleme kararı ver
 if UIType = 'TRadioButton' then
begin
  // RadioButton için
  if ActualValue is TJSONBool then
  begin
    var BoolValue := (ActualValue as TJSONBool).AsBoolean;
    if LabelText <> '' then
      FieldName := LabelText;

    DebugLog('RadioButton oluşturuluyor: ' + FieldName);
    // GroupName'i al veya varsayılan değeri kullan
    var RadioGroupName := 'DefaultRadioGroup';
    if GroupName <> '' then
      RadioGroupName := GroupName;

    // Resim yolu kontrolü
    var ImagePath: string := '';
    if ValueObj.GetValue('ImagePath') <> nil then
      ImagePath := ValueObj.GetValue('ImagePath').Value;

    // Resim yolu varsa, resimli RadioButton ekle
    var RadioBtn: TRadioButton;
    if ImagePath <> '' then
      RadioBtn := FExpandableListView.AddRadioButtonFieldWithImage(HeaderInfo, FieldName, BoolValue, RadioGroupName, ImagePath)
    else
      RadioBtn := FExpandableListView.AddRadioButtonField(HeaderInfo, FieldName, BoolValue, RadioGroupName);
  end;
end

        else if UIType = 'TCheckBox' then
        begin
          // CheckBox için
          if ActualValue is TJSONBool then
          begin
            var
            BoolValue := (ActualValue as TJSONBool).AsBoolean;
            if LabelText <> '' then
              FieldName := LabelText;

            FExpandableListView.AddCheckBoxField(HeaderInfo, FieldName, BoolValue);
          end;
        end
  else if (UIType = 'TMemo') or (LowerCase(ValueType) = 'memo') then
  begin
    // Memo alanı için
    var StrValue := '';
    var MemoHeight: Single := 80; // Varsayılan yükseklik

    if ActualValue is TJSONString then
      StrValue := (ActualValue as TJSONString).Value;

    // Yükseklik bilgisi varsa al
    if ValueObj.GetValue('Height') <> nil then
      MemoHeight := (ValueObj.GetValue('Height') as TJSONNumber).AsDouble;

    if LabelText <> '' then
      FieldName := LabelText;

    FExpandableListView.AddMemoField(HeaderInfo, FieldName, StrValue, MemoHeight);
  end


        else if ValueObj.GetValue('Items') <> nil then
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

          FExpandableListView.AddComboBoxField(HeaderInfo, FieldName, ItemsArray, SelectedIndex);
        end
        else if ActualValue is TJSONNumber then
        begin
          // Bu kısımda değişiklik yok - sayısal değerler için olan kısım
          if LabelText <> '' then
            FieldName := LabelText;

          if LowerCase(ValueType) = 'integer' then
          begin
            var
            IntValue := Round((ActualValue as TJSONNumber).AsDouble);
            FExpandableListView.AddNumberField(HeaderInfo, FieldName, IntValue, Min, Max,
              nvtInteger, VertIncrement);
          end
          else
          begin
            var
            FloatValue := (ActualValue as TJSONNumber).AsDouble;
            FExpandableListView.AddNumberField(HeaderInfo, FieldName, FloatValue, Min, Max,
              nvtFloat, VertIncrement);
          end;
        end
        else if ActualValue is TJSONBool then
        begin
          // Boolean değer ama belirli bir UIType yoksa varsayılan olarak CheckBox
          var
          BoolValue := (ActualValue as TJSONBool).AsBoolean;

          if LabelText <> '' then
            FieldName := LabelText;

          FExpandableListView.AddCheckBoxField(HeaderInfo, FieldName, BoolValue);
        end
        else if ActualValue is TJSONString then
        begin
          // Bu kısımda değişiklik yok - string değerler için olan kısım
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
              FExpandableListView.AddColorBoxField(HeaderInfo, FieldName, ColorValue);
            except
              FExpandableListView.AddEditField(HeaderInfo, FieldName, StrValue);
            end;
          end
          else
          begin
            // Normal metin
            if LabelText <> '' then
              FieldName := LabelText;

            FExpandableListView.AddEditField(HeaderInfo, FieldName, StrValue);
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        DebugLog('Hata: Alan yükleme hatası - ' + FieldName + ': ' + E.Message);
        Continue;
      end;
    end;
  end;
end;
// JSON dosyasından veri yükleme



function TExpandableListViewJSONHelper.LoadFromJSON(const AJSONString: string): Boolean;
var
  RootObj: TJSONObject;
  HeadersArray: TJSONArray;
  HeaderObj: TJSONObject;
  HeaderInfo: THeaderInfo;
  HeaderTitle: string;
  HeaderIndex: Integer;
  HeaderColor: TAlphaColor;
  FieldsObj: TJSONObject;
  SVGData: string;
  MetadataObj: TJSONObject;
  MetadataCreatedAt, MetadataCreatedBy, MetadataVersion: string;
  i, j: Integer;
begin
  Result := False;

  try
    // Önce mevcut başlıkları temizle
    for i := FExpandableListView.FHeaders.Count - 1 downto 0 do
      FreeAndNil(FExpandableListView.FHeaders[i]);
    FExpandableListView.FHeaders.Clear;

    // Tüm liste öğelerini temizle
    FExpandableListView.Clear;

    // JSON nesnesini oluştur
    RootObj := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
    if RootObj = nil then
    begin
      ShowMessage('JSON parse hatası: Geçersiz JSON formatı');
      Exit;
    end;

    try
      // Metadata bilgilerini oku (varsa)
      if RootObj.TryGetValue<TJSONObject>('Metadata', MetadataObj) then
      begin
        if MetadataObj.TryGetValue('CreatedAt', MetadataCreatedAt) then
          DebugLog(PChar('JSON oluşturma zamanı: ' + MetadataCreatedAt));

        if MetadataObj.TryGetValue('CreatedBy', MetadataCreatedBy) then
          DebugLog(PChar('JSON oluşturan kullanıcı: ' + MetadataCreatedBy));

        if MetadataObj.TryGetValue('Version', MetadataVersion) then
          DebugLog(PChar('JSON versiyon: ' + MetadataVersion));
      end;

      // Yeni format (Headers array olarak)
      if RootObj.TryGetValue<TJSONArray>('Headers', HeadersArray) then
      begin
        // Headers array'indeki her bir başlık için işlem yap
        for i := 0 to HeadersArray.Count - 1 do
        begin
          try
            HeaderObj := HeadersArray.Items[i] as TJSONObject;

            // Başlık bilgilerini al
            if not HeaderObj.TryGetValue('Title', HeaderTitle) then
            begin
              ShowMessage('Hata: Başlık ' + IntToStr(i) +
                ' için Title bilgisi eksik');
              Continue;
            end;

            // HeaderIndex bilgisini al (varsayılan değer = i)
            if not HeaderObj.TryGetValue('HeaderIndex', HeaderIndex) then
              HeaderIndex := i;

            // HeaderColor bilgisini al
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
              begin
                try
                  HeaderColor := ColorStr.ToInt64;
                except
                  HeaderColor := TAlphaColorRec.Blue;
                  // Geçersiz renk varsa varsayılan kullan
                end;
              end;
            end;

            // Başlık oluştur
            HeaderInfo := FExpandableListView.AddHeader(HeaderTitle, HeaderIndex, HeaderColor);
            if HeaderInfo = nil then
            begin
              ShowMessage('Başlık oluşturma hatası: ' + HeaderTitle);
              Continue;
            end;

            // SVG verisi
            if HeaderObj.GetValue('SVGData') <> nil then
            begin
              SVGData := HeaderObj.GetValue('SVGData').Value;
              if SVGData <> '' then
                FExpandableListView.SetHeaderSVG(HeaderInfo, SVGData);
            end;

            // Fields nesnesini işle
            if HeaderObj.TryGetValue<TJSONObject>('Fields', FieldsObj) then
            begin
              LoadFieldsFromJSON(HeaderInfo, FieldsObj);
            end;
          except
            on E: Exception do
              ShowMessage('Başlık işleme hatası: Başlık ' + IntToStr(i) + ' - '
                + E.Message);
          end;
        end;

        Result := True;
      end
      else
        Exit;

      // Eski format (doğrudan başlıklar ana objede)
      // Bu kısmı backward compatibility (geriye dönük uyumluluk) için tutuyoruz
      for i := 0 to RootObj.Count - 1 do
      begin
        try
          HeaderTitle := RootObj.Pairs[i].JsonString.Value;

          // Metadata'yı atla
          if HeaderTitle = 'Metadata' then
            Continue;
          if (not(RootObj.Pairs[i].JsonValue is TJSONObject)) or
            (not(RootObj.Pairs[i].JsonValue is TJSONArray)) then
          begin
            DebugLog('Hata: "' + HeaderTitle + '" için geçersiz başlık yapısı');
            // var xx:=    RootObj.Pairs[i].JsonValue.ToString;

            Continue;
          end;

          HeaderObj := RootObj.Pairs[i].JsonValue as TJSONObject;

          // Başlık indeksi ve rengi
          HeaderIndex := 0;
          if HeaderObj.TryGetValue('HeaderIndex', HeaderIndex) = False then
            HeaderIndex := i;

          HeaderColor := TAlphaColorRec.Blue;
          if HeaderObj.GetValue('HeaderColor') <> nil then
          begin
            var
            ColorStr := HeaderObj.GetValue('HeaderColor').Value;
            if ColorStr.StartsWith('#') then
            begin
              HeaderColor := HexToTAlphaColor(ColorStr);
            end
            else
              HeaderColor := ColorStr.ToInt64;
          end;

          // Başlık oluştur
          HeaderInfo := FExpandableListView.AddHeader(HeaderTitle, HeaderIndex, HeaderColor);
          if HeaderInfo = nil then
          begin
            ShowMessage('Başlık oluşturma hatası: ' + HeaderTitle);
            Continue;
          end;

          // SVG verisi
          if HeaderObj.GetValue('SVGData') <> nil then
          begin
            SVGData := HeaderObj.GetValue('SVGData').Value;
            if SVGData <> '' then
              FExpandableListView.SetHeaderSVG(HeaderInfo, SVGData);
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
            ShowMessage('Başlık işleme hatası: ' + HeaderTitle + ' - ' +
              E.Message);
        end;
      end;

      Result := True;

    finally
      FreeAndNil(RootObj);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('JSON işleme hatası: ' + E.Message);
      Result := False;
    end;
  end;
end;


function TExpandableListViewJSONHelper.LoadFromJSONFile(const AFileName: string): Boolean;
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
        FreeAndNil(StringStream);
      end;
    finally
      FreeAndNil(FileStream);
    end;
  except
    on E: Exception do
      Result := False;
  end;
end;


// JSON dosyasından başlık bilgisi yükleme
function TExpandableListViewJSONHelper.LoadHeaderFromJSON(const ATitle: string;
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
          FExpandableListView.SetHeaderSVG(HeaderInfo, SVGData);
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
          FExpandableListView.AddCheckBoxField(HeaderInfo, FieldName, FieldValue = 'true');
        end
        else if FieldValue.StartsWith('#') then
        begin
          // Renk değeri
          try
            ColorValue :=
              StrToInt('$' + Copy(FieldValue, 2, Length(FieldValue) - 1));
            FExpandableListView.AddColorBoxField(HeaderInfo, FieldName, ColorValue);
          except
            FExpandableListView.AddEditField(HeaderInfo, FieldName, FieldValue);
          end;
        end
        else
        begin
          // Normal metin
          FExpandableListView.AddEditField(HeaderInfo, FieldName, FieldValue);
        end;
      end
      else if JSONPair.JsonValue is TJSONNumber then
      begin
        // Sayısal değer
       FExpandableListView.AddNumberField(HeaderInfo, FieldName, TJSONNumber(JSONPair.JsonValue)
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

        FExpandableListView.AddComboBoxField(HeaderInfo, FieldName, Items);
      end;
    end;

    Result := HeaderInfo;
  except
    on E: Exception do
    begin
      if HeaderInfo <> nil then
        FreeAndNil(HeaderInfo);
      Result := nil;
    end;
  end;
end;

// Bileşeni doğrudan işleyen fonksiyon
procedure TExpandableListViewJSONHelper.ProcessComponent(Component: TComponent;
const FieldName: string; FieldsObj: TJSONObject);
var
  ValueObj: TJSONObject;
begin
  ValueObj := CreateValueObjectForComponent(Component);

  if ValueObj <> nil then
    FieldsObj.AddPair(FieldName, ValueObj);
end;


// Layout içindeki bileşenleri işleyen yardımcı fonksiyon


procedure TExpandableListViewJSONHelper.ProcessLayoutComponents(Layout: TLayout;
const FieldName: string; FieldsObj: TJSONObject);
var
  i: Integer;
  Component: TComponent;
  ComponentFieldName: string;
begin
  // Layout içindeki tüm bileşenleri dolaş
  for i := 0 to Layout.ComponentCount - 1 do
  begin
    Component := Layout.Components[i];

    if IsValidComponent(Component) then
    begin
      // Eğer belirtilmiş bir alan adı yoksa, bileşenin yanındaki etiketten bul
      ComponentFieldName := FieldName;
      if ComponentFieldName = '' then
        ComponentFieldName := FindLabelTextForComponent(Component);

      if ComponentFieldName <> '' then
        ProcessComponent(Component, ComponentFieldName, FieldsObj);
    end;
  end;
end;


function TExpandableListViewJSONHelper.UpdateOnlyValuesFromJSON(
  const AJSONString: string): Boolean;
var
  RootObj, HeaderObj, FieldsObj: TJSONObject;
  HeadersArray: TJSONArray;
  HeaderInfo: THeaderInfo;
  i, j: Integer;
  Component: TComponent;
  Value: TJSONValue;
  ListBoxItem: TListBoxItem;
  Layout: TLayout;
  HeaderTitle: string;
begin
  Result := False;
  try
    // JSON nesnesini parse et
    RootObj := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
    if RootObj = nil then
    begin
      DebugLog('[UpdateValuesFromJSON] Geçersiz JSON formatı');
      Exit;
    end;

    try
      // Headers array'ini al
      if not RootObj.TryGetValue<TJSONArray>('Headers', HeadersArray) then
      begin
        DebugLog('[UpdateValuesFromJSON] Headers array bulunamadı');
        Exit;
      end;

      // Her başlık için döngü
      for i := 0 to HeadersArray.Count - 1 do
      begin
        HeaderObj := HeadersArray.Items[i] as TJSONObject;

        // Başlık adını al
        if not HeaderObj.TryGetValue('Title', HeaderTitle) then Continue;

        // Mevcut HeaderInfo'yu bul
        HeaderInfo := nil;
        for j := 0 to FExpandableListView.FHeaders.Count - 1 do
        begin
          if FExpandableListView.FHeaders[j].Title = HeaderTitle then
          begin
            HeaderInfo := FExpandableListView.FHeaders[j];
            Break;
          end;
        end;

        if HeaderInfo = nil then Continue;

        // Fields nesnesini al
        if not HeaderObj.TryGetValue<TJSONObject>('Fields', FieldsObj) then Continue;

        // Her ListBoxItem için döngü
        for j := 0 to HeaderInfo.ChildItems.Count - 1 do
        begin
          ListBoxItem := TListBoxItem(HeaderInfo.ChildItems[j]);

          // Layout'u bul
          Layout := nil;
          for var k := 0 to ListBoxItem.ComponentCount - 1 do
          begin
            if ListBoxItem.Components[k] is TLayout then
            begin
              Layout := TLayout(ListBoxItem.Components[k]);
              Break;
            end;
          end;

          if Layout = nil then Continue;

          // Layout içindeki her bileşen için
          for var k := 0 to Layout.ComponentCount - 1 do
          begin
            Component := Layout.Components[k];

            // Etiket metnini bul
            var LabelText := FindLabelTextForComponent(Component);
            if LabelText = '' then Continue;

            // JSON'dan değeri al
            Value := FieldsObj.Values[LabelText];
            if Value = nil then Continue;

            try
              // Bileşen tipine göre değeri güncelle
              if Component is TEdit then
                TEdit(Component).Text := Value.Value
              else if Component is TMemo then
                TMemo(Component).Text := Value.Value
              else if Component is TNumberBox then
              begin
                var NumBox := TNumberBox(Component);
                if Value is TJSONNumber then
                  NumBox.Value := (Value as TJSONNumber).AsDouble;
              end
              else if Component is TCheckBox then
              begin
                if Value is TJSONBool then
                  TCheckBox(Component).IsChecked := (Value as TJSONBool).AsBoolean;
              end
              else if Component is TSwitch then
              begin
                if Value is TJSONBool then
                  TSwitch(Component).IsChecked := (Value as TJSONBool).AsBoolean;
              end
              else if Component is TComboBox then
              begin
                var ComboBox := TComboBox(Component);
                var StrValue := Value.Value;
                var ItemIndex := ComboBox.Items.IndexOf(StrValue);
                if ItemIndex >= 0 then
                  ComboBox.ItemIndex := ItemIndex;
              end
              else if Component is TColorComboBox then
              begin
                if Value.Value.StartsWith('#') then
                  TColorComboBox(Component).Color := HexToTAlphaColor(Value.Value);
              end
              else if Component is TColorBox then
              begin
                if Value.Value.StartsWith('#') then
                  TColorBox(Component).Color := HexToTAlphaColor(Value.Value);
              end
              else if Component is TRadioButton then
              begin
                if Value is TJSONBool then
                  TRadioButton(Component).IsChecked := (Value as TJSONBool).AsBoolean;
              end
              else if Component is TTrackBar then
              begin
                if Value is TJSONNumber then
                  TTrackBar(Component).Value := (Value as TJSONNumber).AsDouble;
              end;
            except
              on E: Exception do
                DebugLog(Format('[UpdateValuesFromJSON] Değer güncelleme hatası - %s: %s',
                  [LabelText, E.Message]));
            end;
          end;
        end;
      end;

      Result := True;
    finally
      FreeAndNil(RootObj);
    end;
  except
    on E: Exception do
    begin
      DebugLog('[UpdateValuesFromJSON] Genel hata: ' + E.Message);
      Result := False;
    end;
  end;
end;

function TExpandableListViewJSONHelper.UpdateValuesFromJSON(const AJSONString: string): Boolean;
var
  RootObj: TJSONObject;
  HeadersArray: TJSONArray;
  HeaderObj, FieldsObj: TJSONObject;
  HeaderInfo: THeaderInfo;
  HeaderTitle: string;
  Component: TComponent;
  i, j, k: Integer;
  FieldName, FieldValue: string;
  FieldPair: TJSONPair;
begin
  Result := False;
  try
    // JSON string'i parse et
    RootObj := TJSONObject.ParseJSONValue(AJSONString) as TJSONObject;
    if not Assigned(RootObj) then
      Exit;

    try
      // Headers array'ini al
      if not RootObj.TryGetValue<TJSONArray>('Headers', HeadersArray) then
        Exit;

      // Her başlık için döngü
      for i := 0 to HeadersArray.Count - 1 do
      begin
        HeaderObj := HeadersArray.Items[i] as TJSONObject;

        // Başlık adını al
        if not HeaderObj.TryGetValue('Title', HeaderTitle) then
          Continue;

        // Bu başlığı ExpandableListView'da bul
        HeaderInfo := nil;
        for j := 0 to FExpandableListView.FHeaders.Count - 1 do
        begin
          if FExpandableListView.FHeaders[j].Title = HeaderTitle then
          begin
            HeaderInfo := FExpandableListView.FHeaders[j];
            Break;
          end;
        end;

        // Başlık bulunamadıysa sonraki başlığa geç
        if not Assigned(HeaderInfo) then
          Continue;

        // Fields nesnesini al
        if not HeaderObj.TryGetValue<TJSONObject>('Fields', FieldsObj) then
          Continue;

        // Her alan için döngü
        for j := 0 to FieldsObj.Count - 1 do
        begin
          FieldPair := FieldsObj.Pairs[j];
          FieldName := FieldPair.JsonString.Value;
          FieldValue := FieldPair.JsonValue.Value;

          // Header'ın alt öğelerini kontrol et
          for k := 0 to HeaderInfo.ChildItems.Count - 1 do
          begin
            if not(HeaderInfo.ChildItems[k] is TListBoxItem) then
              Continue;

            // ListBoxItem içindeki bileşenleri kontrol et
            for var m := 0 to HeaderInfo.ChildItems[k].ComponentCount - 1 do
            begin
              Component := HeaderInfo.ChildItems[k].Components[m];

              // Bileşenin etiketini kontrol et
              if FindLabelTextForComponent(Component) <> FieldName then
                Continue;

              // Bileşen tipine göre değeri güncelle
              try
                if Component is TEdit then
                  TEdit(Component).Text := FieldValue
                else if Component is TNumberBox then
                begin
                  var Value: Double;
                  if TryStrToFloat(FieldValue, Value) then
                    TNumberBox(Component).Value := Value;
                end
                else if Component is TCheckBox then
                  TCheckBox(Component).IsChecked := StrToBool(FieldValue)
                else if Component is TSwitch then
                  TSwitch(Component).IsChecked := StrToBool(FieldValue)
                else if Component is TComboBox then
                begin
                  var ComboBox := TComboBox(Component);
                  var ItemIndex := ComboBox.Items.IndexOf(FieldValue);
                  if ItemIndex >= 0 then
                    ComboBox.ItemIndex := ItemIndex;
                end
                else if Component is TColorComboBox then
                begin
                  if FieldValue.StartsWith('#') then
                    TColorComboBox(Component).Color := HexToTAlphaColor(FieldValue);
                end
                else if Component is TColorBox then
                begin
                  if FieldValue.StartsWith('#') then
                    TColorBox(Component).Color := HexToTAlphaColor(FieldValue);
                end
                else if Component is TMemo then
                  TMemo(Component).Text := FieldValue
                else if Component is TRadioButton then
                  TRadioButton(Component).IsChecked := StrToBool(FieldValue)
                else if Component is TTrackBar then
                begin
                  var Value: Double;
                  if TryStrToFloat(FieldValue, Value) then
                    TTrackBar(Component).Value := Value;
                end;
              except
                on E: Exception do
                  DebugLog(Format('Değer güncelleme hatası - Alan: %s, Değer: %s, Hata: %s',
                    [FieldName, FieldValue, E.Message]));
              end;
            end;
          end;
        end;
      end;

      Result := True;
    finally
      FreeAndNil(RootObj);
    end;

  except
    on E: Exception do
    begin
      DebugLog('UpdateValuesFromJSON Hatası: ' + E.Message);
      Result := False;
    end;
  end;
end;

end.
