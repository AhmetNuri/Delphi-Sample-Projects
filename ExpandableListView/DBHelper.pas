 unit DBHelper;

interface

uses
  System.SysUtils, System.Classes, System.JSON, FireDAC.Comp.Client, FireDAC.Stan.Param,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.UI.Intf, FireDAC.Stan.Intf, FireDAC.Stan.Error, FireDAC.Phys.Intf,
  FireDAC.Phys, FireDAC.Phys.SQLite, FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs,
  FireDAC.Phys.SQLiteWrapper.Stat, Data.DB;

type
  TDBHelper = class
  private
    FConnection: TFDConnection;

    // Yardımcı fonksiyonlar
    function GetLastInsertRowID: Integer;
    function EscapeString(const AValue: string): string;
    procedure ExecuteSQL(const ASQL: string);
    function GetFieldDataType(const AFieldDataType: string): string;
    function GetFieldType(const AFieldType: string): string;

  public
    constructor Create(AConnection: TFDConnection);
    destructor Destroy; override;

    // Veritabanından JSON oluşturma fonksiyonu
    function DatabaseToJSON(const AListViewName: string = ''): string;

    // JSON'dan veritabanına kaydetme fonksiyonu
    function JSONToDatabase(const AJSON: string; const AListViewName: string = ''): Integer;
  end;

implementation

{ TDBHelper }

constructor TDBHelper.Create(AConnection: TFDConnection);
begin
  inherited Create;
  FConnection := AConnection;
end;

destructor TDBHelper.Destroy;
begin
  // Bağlantıyı kapatmıyoruz, dışarıdan sağlandığı için
  inherited;
end;

function TDBHelper.EscapeString(const AValue: string): string;
begin
  // SQL injection'dan korunmak için string değerleri kaçış karakterleri ile güvenli hale getir
  Result := StringReplace(AValue, '''', '''''', [rfReplaceAll]);
end;

procedure TDBHelper.ExecuteSQL(const ASQL: string);
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := ASQL;
    Query.ExecSQL;
  finally
    FreeAndNil(Query);
  end;
end;

function TDBHelper.GetLastInsertRowID: Integer;
var
  Query: TFDQuery;
begin
  Query := TFDQuery.Create(nil);
  try
    Query.Connection := FConnection;
    Query.SQL.Text := 'SELECT last_insert_rowid()';
    Query.Open;
    Result := Query.Fields[0].AsInteger;
  finally
    FreeAndNil(Query);
  end;
end;

function TDBHelper.GetFieldDataType(const AFieldDataType: string): string;
begin
  // Veri tipi için standart dönüşüm
  if AFieldDataType = 'string' then Result := 'TEXT'
  else if AFieldDataType = 'integer' then Result := 'INTEGER'
  else if AFieldDataType = 'float' then Result := 'REAL'
  else if AFieldDataType = 'boolean' then Result := 'INTEGER'
  else if AFieldDataType = 'date' then Result := 'TEXT'
  else if AFieldDataType = 'datetime' then Result := 'TEXT'
  else Result := 'TEXT'; // Varsayılan
end;

 function TDBHelper.GetFieldType(const AFieldType: string): string;
begin
  // Alan tipi için standart dönüşüm
  if AFieldType = 'edit' then Result := 'edit'
  else if AFieldType = 'number' then Result := 'number'
  else if AFieldType = 'checkbox' then Result := 'checkbox'
  else if AFieldType = 'switch' then Result := 'switch'
  else if AFieldType = 'combobox' then Result := 'combobox'
  else if AFieldType = 'colorbox' then Result := 'colorbox'
  else if AFieldType = 'radiobutton' then Result := 'radiobutton'
  else Result := 'edit'; // Varsayılan
end;

function TDBHelper.DatabaseToJSON(const AListViewName: string = ''): string;
var
  JSONObject: TJSONObject;
  MetadataObject: TJSONObject;
  HeadersArray: TJSONArray;
  FieldsObject: TJSONObject;

  QueryListViews, QueryHeaders, QueryFields, QueryParams, QueryComboItems: TFDQuery;

  ListViewID, HeaderID, FieldID: Integer;
  WhereClause: string;
begin
  // Create main JSON object
  JSONObject := TJSONObject.Create;
  try
    // Create Metadata section with updated values
    MetadataObject := TJSONObject.Create;
    MetadataObject.AddPair('CreatedAt', TJSONString.Create('2025-04-01 17:13:02'));
    MetadataObject.AddPair('CreatedBy', TJSONString.Create('AhmetNuri'));
    MetadataObject.AddPair('Version', TJSONString.Create('1.1'));

    // Eğer liste adı belirtilmişse, metadata'ya ekle
    if AListViewName <> '' then
      MetadataObject.AddPair('ListName', TJSONString.Create(AListViewName));

    JSONObject.AddPair('Metadata', MetadataObject);

    // Create query objects
    QueryListViews := TFDQuery.Create(nil);
    QueryHeaders := TFDQuery.Create(nil);
    QueryFields := TFDQuery.Create(nil);
    QueryParams := TFDQuery.Create(nil);
    QueryComboItems := TFDQuery.Create(nil);

    try
      // Set connections
      QueryListViews.Connection := FConnection;
      QueryHeaders.Connection := FConnection;
      QueryFields.Connection := FConnection;
      QueryParams.Connection := FConnection;
      QueryComboItems.Connection := FConnection;

      // Create where clause if needed
      WhereClause := '';
      if AListViewName <> '' then
        WhereClause := ' WHERE ListViews.ListViewName = ''' + EscapeString(AListViewName) + '''';

      // Get ListView list
      QueryListViews.SQL.Text := 'SELECT * FROM ListViews ' + WhereClause;
      QueryListViews.Open;

      // Headers array
      HeadersArray := TJSONArray.Create;

      // Process each ListView
      while not QueryListViews.Eof do
      begin
        ListViewID := QueryListViews.FieldByName('ListViewID').AsInteger;

        // Eğer liste adı daha önce metadata'ya eklenmemişse ve bir liste bulunduysa
        if (AListViewName = '')  then
        begin
          var CurrentListName := QueryListViews.FieldByName('ListViewName').AsString;
          MetadataObject.AddPair('ListName', TJSONString.Create(CurrentListName));
        end;

        // Headers query
        QueryHeaders.SQL.Text := 'SELECT * FROM Headers WHERE HeaderListViewID = :ListViewID ORDER BY HeaderOrder';
        QueryHeaders.ParamByName('ListViewID').AsInteger := ListViewID;
        QueryHeaders.Open;

        // Process each Header
        while not QueryHeaders.Eof do
        begin
          HeaderID := QueryHeaders.FieldByName('HeaderID').AsInteger;
          var HeaderObj := TJSONObject.Create;

          // Add header information
          HeaderObj.AddPair('Title', TJSONString.Create(QueryHeaders.FieldByName('HeaderTitle').AsString));

          // Process HeaderIndex (formerly Order)
          HeaderObj.AddPair('HeaderIndex', TJSONNumber.Create(QueryHeaders.FieldByName('HeaderOrder').AsInteger));

          // Process HeaderColor - convert numeric color to hex format (#FFDC143C)
          if not QueryHeaders.FieldByName('HeaderColor').IsNull then
          begin
            var ColorValue := QueryHeaders.FieldByName('HeaderColor').AsString;
            var ColorValueInt := StrToInt64(ColorValue);
            var HexColor := '#' + IntToHex(ColorValueInt, 8); // Convert to ARGB hex format
            HeaderObj.AddPair('HeaderColor', TJSONString.Create(HexColor));
          end;

          // Process HeaderSVGData
          if not QueryHeaders.FieldByName('HeaderSVGData').IsNull then
            HeaderObj.AddPair('SVGData', TJSONString.Create(QueryHeaders.FieldByName('HeaderSVGData').AsString));

          // Fields object
          FieldsObject := TJSONObject.Create;

          // Fields query
          QueryFields.SQL.Text := 'SELECT * FROM Fields WHERE FieldHeaderID = :HeaderID ORDER BY FieldOrder';
          QueryFields.ParamByName('HeaderID').AsInteger := HeaderID;
          QueryFields.Open;

          // Process each Field
          while not QueryFields.Eof do
          begin
            FieldID := QueryFields.FieldByName('FieldID').AsInteger;
            var FieldName := QueryFields.FieldByName('FieldName').AsString;
            var FieldObj := TJSONObject.Create;
            var FieldTypeStr := '';
            var DataTypeStr := '';

            if not QueryFields.FieldByName('FieldType').IsNull then
              FieldTypeStr := LowerCase(QueryFields.FieldByName('FieldType').AsString);

            if not QueryFields.FieldByName('FieldDataType').IsNull then
              DataTypeStr := LowerCase(QueryFields.FieldByName('FieldDataType').AsString);

            // Map field type to UIType
            var UITypeStr := 'TEdit'; // Default
            if FieldTypeStr = 'number'.ToLower then UITypeStr := 'TNumberBox'
            else if FieldTypeStr = 'checkbox'.ToLower then UITypeStr := 'TCheckBox'
            else if FieldTypeStr = 'radiobutton'.ToLower then UITypeStr := 'TRadioButton'
            else if FieldTypeStr = 'switch'.ToLower then UITypeStr := 'TSwitch'
            else if FieldTypeStr = 'combobox'.ToLower then UITypeStr := 'TComboBox'
            else if FieldTypeStr = 'colorbox'.ToLower then UITypeStr := 'TColorComboBox';

            // Special case for ComboBox detection based on DataType
            if DataTypeStr = 'combobox' then UITypeStr := 'TComboBox';

            // Add UIType
            FieldObj.AddPair('UIType', TJSONString.Create(UITypeStr));

            // Add label text (use FieldLabel if available, otherwise use FieldName)
            if not QueryFields.FieldByName('FieldLabel').IsNull then
              FieldObj.AddPair('labelText', TJSONString.Create(QueryFields.FieldByName('FieldLabel').AsString))
            else
              FieldObj.AddPair('labelText', TJSONString.Create(FieldName));

            // Add ValueType (converted from FieldDataType)
            var ValueTypeStr := 'String'; // Default
            if DataTypeStr = 'float' then ValueTypeStr := 'Float'
            else if DataTypeStr = 'integer' then ValueTypeStr := 'Integer'
            else if DataTypeStr = 'boolean' then ValueTypeStr := 'Boolean'
            else if DataTypeStr = 'combobox' then ValueTypeStr := 'ComboBox'
            else if DataTypeStr = 'color' then ValueTypeStr := 'Color';

            FieldObj.AddPair('ValueType', TJSONString.Create(ValueTypeStr));

            // Handle ComboBox fields
            if UITypeStr = 'TComboBox' then
            begin
              // Get combo items
              QueryComboItems.SQL.Text :=
                'SELECT * FROM ComboBoxItems WHERE ComboBoxItemFieldID = :FieldID ORDER BY ComboBoxItemOrder';
              QueryComboItems.ParamByName('FieldID').AsInteger := FieldID;
              QueryComboItems.Open;

              var ItemsArray := TJSONArray.Create;
              var SelectedIndex := 0;
              var FirstItemValue := '';

              // Add each combo item to array
              while not QueryComboItems.Eof do
              begin
                var ItemValue := QueryComboItems.FieldByName('ComboBoxItemValue').AsString;
                ItemsArray.Add(TJSONString.Create(ItemValue).ToString);

                if QueryComboItems.RecNo = 1 then
                  FirstItemValue := ItemValue;

                QueryComboItems.Next;
              end;

              // Try to get selected index from properties
              var PropertiesStr := '';
              if not QueryFields.FieldByName('FieldProperties').IsNull then
                PropertiesStr := QueryFields.FieldByName('FieldProperties').AsString;

              if PropertiesStr <> '' then
              begin
                var PropsObj := TJSONObject(TJSONObject.ParseJSONValue(PropertiesStr));
                if PropsObj <> nil then
                begin
                  try
                    PropsObj.TryGetValue<Integer>('selectedIndex', SelectedIndex);
                  finally
                    PropsObj.Free;
                  end;
                end;
              end;

              // Add items array and selected index
              FieldObj.AddPair('Items', ItemsArray);
              FieldObj.AddPair('SelectedIndex', TJSONNumber.Create(SelectedIndex));

              // Add Value from FieldValue or use first item if empty
              var FieldValue := '';
              if not QueryFields.FieldByName('FieldValue').IsNull then
                FieldValue := QueryFields.FieldByName('FieldValue').AsString;

              if (FieldValue = '') and (ItemsArray.Count > 0) then
                FieldValue := FirstItemValue;

              FieldObj.AddPair('Value', TJSONString.Create(FieldValue));
            end
            else if UITypeStr = 'TNumberBox' then
            begin
              // Set default number properties
              var DecimalDigits := 0;
              if DataTypeStr = 'float' then
                DecimalDigits := 2;

              FieldObj.AddPair('Min', TJSONNumber.Create(0.0));
              FieldObj.AddPair('Max', TJSONNumber.Create(100.0));
              FieldObj.AddPair('DecimalDigits', TJSONNumber.Create(DecimalDigits));
              FieldObj.AddPair('VertIncrement', TJSONBool.Create(False));

              // Parse custom properties if available
              var PropertiesStr := '';
              if not QueryFields.FieldByName('FieldProperties').IsNull then
                PropertiesStr := QueryFields.FieldByName('FieldProperties').AsString;

              if PropertiesStr <> '' then
              begin
                var PropsObj := TJSONObject(TJSONObject.ParseJSONValue(PropertiesStr));
                if PropsObj <> nil then
                begin
                  try
                    var MinValue: Double;
                    var MaxValue: Double;
                    var DecDigits: Integer;
                    var VertInc: Boolean;

                    if PropsObj.TryGetValue<Double>('min', MinValue) then
                    begin
                      FieldObj.RemovePair('Min').Free;
                      FieldObj.AddPair('Min', TJSONNumber.Create(MinValue));
                    end;

                    if PropsObj.TryGetValue<Double>('max', MaxValue) then
                    begin
                      FieldObj.RemovePair('Max').Free;
                      FieldObj.AddPair('Max', TJSONNumber.Create(MaxValue));
                    end;

                    if PropsObj.TryGetValue<Integer>('decimalDigits', DecDigits) then
                    begin
                      FieldObj.RemovePair('DecimalDigits').Free;
                      FieldObj.AddPair('DecimalDigits', TJSONNumber.Create(DecDigits));
                    end;

                    if PropsObj.TryGetValue<Boolean>('vertIncrement', VertInc) then
                    begin
                      FieldObj.RemovePair('VertIncrement').Free;
                      FieldObj.AddPair('VertIncrement', TJSONBool.Create(VertInc));
                    end;
                  finally
                    PropsObj.Free;
                  end;
                end;
              end;

              // Add Value
              if not QueryFields.FieldByName('FieldValue').IsNull then
              begin
                var FieldValue := QueryFields.FieldByName('FieldValue').AsString;
                var NumValue: Double;
                if TryStrToFloat(FieldValue, NumValue) then
                  FieldObj.AddPair('Value', TJSONNumber.Create(NumValue))
                else
                  FieldObj.AddPair('Value', TJSONNumber.Create(0));
              end
              else
                FieldObj.AddPair('Value', TJSONNumber.Create(0));
            end
            else if UITypeStr = 'TCheckBox' then
            begin
              // Add Value
              var BoolValue := False;
              if not QueryFields.FieldByName('FieldValue').IsNull then
                BoolValue := UpperCase(QueryFields.FieldByName('FieldValue').AsString) = 'TRUE';

              FieldObj.AddPair('Value', TJSONBool.Create(BoolValue));
            end
            else if UITypeStr = 'TRadioButton' then
            begin
              // Add Value
              var BoolValue := False;
              if not QueryFields.FieldByName('FieldValue').IsNull then
                BoolValue := UpperCase(QueryFields.FieldByName('FieldValue').AsString) = 'TRUE';

              FieldObj.AddPair('Value', TJSONBool.Create(BoolValue));

              // Add radio button specific properties
              var GroupName := '';
              var PropertiesStr := '';

              if not QueryFields.FieldByName('FieldProperties').IsNull then
                PropertiesStr := QueryFields.FieldByName('FieldProperties').AsString;

              if PropertiesStr <> '' then
              begin
                var PropsObj := TJSONObject(TJSONObject.ParseJSONValue(PropertiesStr));
                if PropsObj <> nil then
                begin
                  try
                    PropsObj.TryGetValue<string>('groupName', GroupName);
                  finally
                    PropsObj.Free;
                  end;
                end;
              end;

              // Default group name if none provided
              if GroupName = '' then
                GroupName := 'RadioGroup' + IntToStr(HeaderID);

              FieldObj.AddPair('GroupName', TJSONString.Create(GroupName));
            end
            else if UITypeStr = 'TColorComboBox' then
            begin
              // Add Value
              var ColorValue := '#FF800080'; // Default color
              if not QueryFields.FieldByName('FieldValue').IsNull then
                ColorValue := QueryFields.FieldByName('FieldValue').AsString;

              // Ensure color value is properly formatted
              if (Length(ColorValue) > 0) and (ColorValue[1] <> '#') then
                ColorValue := '#' + ColorValue;

              FieldObj.AddPair('Value', TJSONString.Create(ColorValue));
            end
            else
            begin
              // Add Value for other types (TEdit, etc.)
              var FieldValue := '';
              if not QueryFields.FieldByName('FieldValue').IsNull then
                FieldValue := QueryFields.FieldByName('FieldValue').AsString;

              FieldObj.AddPair('Value', TJSONString.Create(FieldValue));
            end;

            // Add field object to fields collection
            FieldsObject.AddPair(FieldName, FieldObj);
            QueryFields.Next;
          end;

          // Add fields object to header
          HeaderObj.AddPair('Fields', FieldsObject);

          // Add header to headers array
          HeadersArray.Add(HeaderObj);
          QueryHeaders.Next;
        end;

        QueryListViews.Next;
      end;

      // Add headers array to main JSON object
      JSONObject.AddPair('Headers', HeadersArray);

      // Return JSON output
      Result := JSONObject.ToString;
    finally
      QueryListViews.Free;
      QueryHeaders.Free;
      QueryFields.Free;
      QueryParams.Free;
      QueryComboItems.Free;
    end;
  except
    on E: Exception do
    begin
      JSONObject.Free;
      raise Exception.Create('DatabaseToJSON error: ' + E.Message);
    end;
  end;
end;

function TDBHelper.JSONToDatabase(const AJSON: string; const AListViewName: string = ''): Integer;
var
  JSONObject: TJSONObject;
  HeadersArray: TJSONArray;
  FieldsObject: TJSONObject;
  ListViewID, HeaderID, FieldID: Integer;
  ListViewName: string;
begin
  Result := 0; // Count of processed records

  JSONObject := TJSONObject(TJSONObject.ParseJSONValue(AJSON));
  if not Assigned(JSONObject) then
    raise Exception.Create('Invalid JSON format');

  try
    // Start transaction
    FConnection.StartTransaction;
    try
      // Determine ListView name
      if AListViewName <> '' then
        ListViewName := AListViewName
      else if not JSONObject.TryGetValue<string>('Name', ListViewName) then
        ListViewName := 'DefaultListView';

      // Add or update ListView record
      var QueryListView := TFDQuery.Create(nil);
      try
        QueryListView.Connection := FConnection;

        // Check if ListView already exists
        QueryListView.SQL.Text := 'SELECT ListViewID FROM ListViews WHERE ListViewName = :ListViewName';
        QueryListView.ParamByName('ListViewName').AsString := ListViewName;
        QueryListView.Open;

        if QueryListView.IsEmpty then
        begin
          // Add new ListView
          ExecuteSQL('INSERT INTO ListViews (ListViewName) VALUES (''' +
                      EscapeString(ListViewName) + ''')');
          ListViewID := GetLastInsertRowID;
        end
        else
        begin
          // Get existing ListView ID
          ListViewID := QueryListView.FieldByName('ListViewID').AsInteger;

          // Temizleme işlemini doğru sırayla yapıyoruz:
          // 1. İlk önce ComboBoxItem tablosundaki bağlı kayıtları temizle
          var HeaderIDList := '';
          var FieldIDList := '';

          // Headers'a bağlı olan Field ID'leri alıyoruz
          var QueryFields := TFDQuery.Create(nil);
          try
            QueryFields.Connection := FConnection;
            QueryFields.SQL.Text :=
              'SELECT FieldID FROM Fields WHERE FieldHeaderID IN ' +
              '(SELECT HeaderID FROM Headers WHERE HeaderListViewID = :ListViewID)';
            QueryFields.ParamByName('ListViewID').AsInteger := ListViewID;
            QueryFields.Open;

            // Field ID'leri bir liste haline getir
            var FirstField := True;
            while not QueryFields.Eof do
            begin
              if FirstField then
              begin
                FieldIDList := IntToStr(QueryFields.FieldByName('FieldID').AsInteger);
                FirstField := False;
              end
              else
                FieldIDList := FieldIDList + ',' + IntToStr(QueryFields.FieldByName('FieldID').AsInteger);

              QueryFields.Next;
            end;
          finally
            QueryFields.Free;
          end;

          // Field ID listesi oluşturulduysa, ComboBoxItem'ları sil
          if FieldIDList <> '' then
            ExecuteSQL('DELETE FROM ComboBoxItems WHERE ComboBoxItemFieldID IN (' + FieldIDList + ')');

          // 2. Sonra Field tablosundaki bağlı kayıtları temizle
          ExecuteSQL('DELETE FROM Fields WHERE FieldHeaderID IN ' +
                    '(SELECT HeaderID FROM Headers WHERE HeaderListViewID = ' + IntToStr(ListViewID) + ')');

          // 3. En son Headers tablosundaki bağlı kayıtları temizle
          ExecuteSQL('DELETE FROM Headers WHERE HeaderListViewID = ' + IntToStr(ListViewID));
        end;
      finally
        QueryListView.Free;
      end;

      // Process Headers array
      if JSONObject.TryGetValue<TJSONArray>('Headers', HeadersArray) then
      begin
        for var i := 0 to HeadersArray.Count - 1 do
        begin
          var HeaderObj := HeadersArray.Items[i] as TJSONObject;
          var HeaderTitle := '';
          var SVGData := '';
          var HeaderColor := '';
          var HeaderIndex := i;

          // Get header properties
          HeaderObj.TryGetValue<string>('Title', HeaderTitle);
          if HeaderTitle = '' then
            HeaderTitle := 'Header ' + IntToStr(i+1);

          // Get HeaderIndex (or use i as fallback)
          HeaderObj.TryGetValue<Integer>('HeaderIndex', HeaderIndex);

          // Get SVGData
          HeaderObj.TryGetValue<string>('SVGData', SVGData);

          // Get and convert HeaderColor
          if HeaderObj.TryGetValue<string>('HeaderColor', HeaderColor) then
          begin
            // Convert from hex (#FFDC143C) to numeric color
            if (Length(HeaderColor) > 0) and (HeaderColor[1] = '#') then
            begin
              var ColorValue := StrToInt64Def('$' + Copy(HeaderColor, 2, Length(HeaderColor)-1), 0);

              // Insert header record
              ExecuteSQL('INSERT INTO Headers (HeaderListViewID, HeaderTitle, HeaderOrder, HeaderColor, HeaderSVGData) ' +
                        'VALUES (' + IntToStr(ListViewID) + ', ''' + EscapeString(HeaderTitle) + ''', ' +
                        IntToStr(HeaderIndex) + ', ' + IntToStr(ColorValue) + ', ''' +
                        EscapeString(SVGData) + ''')');
            end
            else
            begin
              // Insert header record without color conversion
              ExecuteSQL('INSERT INTO Headers (HeaderListViewID, HeaderTitle, HeaderOrder, HeaderSVGData) ' +
                        'VALUES (' + IntToStr(ListViewID) + ', ''' + EscapeString(HeaderTitle) + ''', ' +
                        IntToStr(HeaderIndex) + ', ''' + EscapeString(SVGData) + ''')');
            end;
          end
          else
          begin
            // Insert header record without color
            ExecuteSQL('INSERT INTO Headers (HeaderListViewID, HeaderTitle, HeaderOrder, HeaderSVGData) ' +
                      'VALUES (' + IntToStr(ListViewID) + ', ''' + EscapeString(HeaderTitle) + ''', ' +
                      IntToStr(HeaderIndex) + ', ''' + EscapeString(SVGData) + ''')');
          end;

          HeaderID := GetLastInsertRowID;
          Inc(Result);

          // Process Fields object
          if HeaderObj.TryGetValue<TJSONObject>('Fields', FieldsObject) then
          begin
            for var j := 0 to FieldsObject.Count - 1 do
            begin
              var FieldName := FieldsObject.Pairs[j].JsonString.Value;
              var FieldObj := FieldsObject.Pairs[j].JsonValue as TJSONObject;

              var UIType := '';
              var LabelText := '';
              var ValueType := '';
              var FieldOrder := j;

              // Get field properties
              FieldObj.TryGetValue<string>('UIType', UIType);
              FieldObj.TryGetValue<string>('labelText', LabelText);
              FieldObj.TryGetValue<string>('ValueType', ValueType);

              // Map UIType to FieldType
              var FieldType := 'edit'; // Default
              if UIType = 'TNumberBox' then FieldType := 'number'
              else if UIType = 'TCheckBox' then FieldType := 'checkbox'
              else if UIType = 'TRadioButton' then FieldType := 'radiobutton'
              else if UIType = 'TSwitch' then FieldType := 'switch'
              else if UIType = 'TComboBox' then FieldType := 'combobox'
              else if UIType = 'TColorComboBox' then FieldType := 'colorbox';

              // Map ValueType to FieldDataType
              var FieldDataType := 'string'; // Default
              if ValueType = 'Integer' then FieldDataType := 'integer'
              else if ValueType = 'Float' then FieldDataType := 'float'
              else if ValueType = 'Boolean' then FieldDataType := 'boolean'
              else if ValueType = 'ComboBox' then FieldDataType := 'ComboBox'
              else if ValueType = 'Color' then FieldDataType := 'Color';

              // Get Value based on type
              var FieldValue := '';
              var JsonValue := FieldObj.GetValue('Value');

              if JsonValue is TJSONString then
                FieldValue := (JsonValue as TJSONString).Value
              else if JsonValue is TJSONNumber then
                FieldValue := FloatToStr((JsonValue as TJSONNumber).AsDouble)
              else if JsonValue is TJSONBool then
                FieldValue := BoolToStr((JsonValue as TJSONBool).AsBoolean, True);

              // Prepare and store properties as a JSON object
              var PropertiesObj := TJSONObject.Create;
              try
                // Add properties based on field type
                if UIType = 'TNumberBox' then
                begin
                  var MinValue: Double;
                  var MaxValue: Double;
                  var DecimalDigits: Integer;
                  var VertIncrement: Boolean;

                  if FieldObj.TryGetValue<Double>('Min', MinValue) then
                    PropertiesObj.AddPair('min', TJSONNumber.Create(MinValue));

                  if FieldObj.TryGetValue<Double>('Max', MaxValue) then
                    PropertiesObj.AddPair('max', TJSONNumber.Create(MaxValue));

                  if FieldObj.TryGetValue<Integer>('DecimalDigits', DecimalDigits) then
                    PropertiesObj.AddPair('decimalDigits', TJSONNumber.Create(DecimalDigits));

                  if FieldObj.TryGetValue<Boolean>('VertIncrement', VertIncrement) then
                    PropertiesObj.AddPair('vertIncrement', TJSONBool.Create(VertIncrement));
                end
                else if UIType = 'TComboBox' then
                begin
                  var SelectedIndex: Integer;
                  if FieldObj.TryGetValue<Integer>('SelectedIndex', SelectedIndex) then
                    PropertiesObj.AddPair('selectedIndex', TJSONNumber.Create(SelectedIndex));
                end
                else if UIType = 'TRadioButton' then
                begin
                  var GroupName: string;
                  if FieldObj.TryGetValue<string>('GroupName', GroupName) then
                    PropertiesObj.AddPair('groupName', TJSONString.Create(GroupName))
                  else
                    // Default group name if none provided
                    PropertiesObj.AddPair('groupName', TJSONString.Create('RadioGroup' + IntToStr(HeaderID)));
                end;

                // Insert field record
                ExecuteSQL('INSERT INTO Fields (FieldHeaderID, FieldName, FieldValue, FieldDataType, FieldType, ' +
                          'FieldLabel, FieldProperties, FieldOrder) VALUES (' +
                          IntToStr(HeaderID) + ', ''' + EscapeString(FieldName) + ''', ''' +
                          EscapeString(FieldValue) + ''', ''' + EscapeString(FieldDataType) + ''', ''' +
                          EscapeString(FieldType) + ''', ''' + EscapeString(LabelText) + ''', ''' +
                          EscapeString(PropertiesObj.ToString) + ''', ' + IntToStr(FieldOrder) + ')');

                FieldID := GetLastInsertRowID;
                Inc(Result);

                // Process ComboBox items if applicable
                if UIType = 'TComboBox' then
                begin
                  var ItemsArray: TJSONArray;
                  if FieldObj.TryGetValue<TJSONArray>('Items', ItemsArray) then
                  begin
                    for var k := 0 to ItemsArray.Count - 1 do
                    begin
                      var ItemValue := '';
                      var ItemObj := ItemsArray.Items[k];

                      if ItemObj is TJSONString then
                      begin
                        // Temizle JSON string formatını
                        var StringValue := (ItemObj as TJSONString).Value;
                        if (Length(StringValue) > 1) and (StringValue[1] = '"') and (StringValue[Length(StringValue)] = '"') then
                          ItemValue := Copy(StringValue, 2, Length(StringValue) - 2)
                        else
                          ItemValue := StringValue;
                      end
                      else if ItemObj is TJSONObject then
                      begin
                        var ItemValueObj := ItemObj as TJSONObject;
                        if not ItemValueObj.TryGetValue<string>('Value', ItemValue) then
                          continue;
                      end;

                      // Insert ComboBoxItem
                      ExecuteSQL('INSERT INTO ComboBoxItems (ComboBoxItemFieldID, ComboBoxItemValue, ComboBoxItemOrder) ' +
                                'VALUES (' + IntToStr(FieldID) + ', ''' + EscapeString(ItemValue) + ''', ' +
                                IntToStr(k) + ')');
                      Inc(Result);
                    end;
                  end;
                end;
              finally
                PropertiesObj.Free;
              end;
            end;
          end;
        end;
      end;

      // Commit all changes
      FConnection.Commit;

    except
      on E: Exception do
      begin
        // Rollback on error
        FConnection.Rollback;
        Result := 0;
        raise Exception.Create('JSONToDatabase error: ' + E.Message);
      end;
    end;

  finally
    JSONObject.Free;
  end;
end;

end.
