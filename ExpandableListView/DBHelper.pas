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
  else Result := 'edit'; // Varsayılan
end;
function TDBHelper.DatabaseToJSON(const AListViewName: string = ''): string;
var
  JSONObject: TJSONObject;
  HeadersArray: TJSONArray;
  FieldsObject: TJSONObject;
  ParametersArray: TJSONArray;
  ComboItemsArray: TJSONArray;

  QueryListViews, QueryHeaders, QueryFields, QueryParams, QueryComboItems: TFDQuery;

  ListViewID, HeaderID, FieldID: Integer;
  WhereClause: string;
begin
  JSONObject := TJSONObject.Create;
  try
    QueryListViews := TFDQuery.Create(nil);
    QueryHeaders := TFDQuery.Create(nil);
    QueryFields := TFDQuery.Create(nil);
    QueryParams := TFDQuery.Create(nil);
    QueryComboItems := TFDQuery.Create(nil);

    try
      // Bağlantıları ayarla
      QueryListViews.Connection := FConnection;
      QueryHeaders.Connection := FConnection;
      QueryFields.Connection := FConnection;
      QueryParams.Connection := FConnection;
      QueryComboItems.Connection := FConnection;

      // Where koşulu oluştur (belirli bir ListView için filtreleme)
      WhereClause := '';
      if AListViewName <> '' then
        WhereClause := ' WHERE ListViews.ListViewName = ''' + EscapeString(AListViewName) + '''';

      // ListView'ların listesi
      QueryListViews.SQL.Text := 'SELECT * FROM ListViews ' + WhereClause;
      QueryListViews.Open;

      // Her ListView için
      while not QueryListViews.Eof do
      begin
        ListViewID := QueryListViews.FieldByName('ListViewID').AsInteger;
        JSONObject.AddPair('Name', TJSONString.Create(QueryListViews.FieldByName('ListViewName').AsString));
        HeadersArray := TJSONArray.Create;

        // Headers sorgusu
        QueryHeaders.SQL.Text := 'SELECT * FROM Headers WHERE HeaderListViewID = :ListViewID ORDER BY HeaderOrder';
        QueryHeaders.ParamByName('ListViewID').AsInteger := ListViewID;
        QueryHeaders.Open;

        // Her Header için
        while not QueryHeaders.Eof do
        begin
          HeaderID := QueryHeaders.FieldByName('HeaderID').AsInteger;
          var HeaderObj := TJSONObject.Create;

          // Header bilgilerini ekle
          HeaderObj.AddPair('Title', TJSONString.Create(QueryHeaders.FieldByName('HeaderTitle').AsString));

          // HeaderColor ekle (null kontrolü yap)
          if not QueryHeaders.FieldByName('HeaderColor').IsNull then
            HeaderObj.AddPair('Color', TJSONString.Create(QueryHeaders.FieldByName('HeaderColor').AsString));

          // HeaderSVGData ekle (null kontrolü yap)
          if not QueryHeaders.FieldByName('HeaderSVGData').IsNull then
            HeaderObj.AddPair('SVGData', TJSONString.Create(QueryHeaders.FieldByName('HeaderSVGData').AsString));

          // HeaderOrder ekle
          HeaderObj.AddPair('Order', TJSONNumber.Create(QueryHeaders.FieldByName('HeaderOrder').AsInteger));

          // Fields nesnesi
          FieldsObject := TJSONObject.Create;

          // Fields sorgusu
          QueryFields.SQL.Text := 'SELECT * FROM Fields WHERE FieldHeaderID = :HeaderID ORDER BY FieldOrder';
          QueryFields.ParamByName('HeaderID').AsInteger := HeaderID;
          QueryFields.Open;

          // Her Field için
          while not QueryFields.Eof do
          begin
            FieldID := QueryFields.FieldByName('FieldID').AsInteger;
            var FieldName := QueryFields.FieldByName('FieldName').AsString;
            var FieldObj := TJSONObject.Create;

            // Field bilgilerini ekle
            if not QueryFields.FieldByName('FieldValue').IsNull then
              FieldObj.AddPair('Value', TJSONString.Create(QueryFields.FieldByName('FieldValue').AsString));

            if not QueryFields.FieldByName('FieldDataType').IsNull then
              FieldObj.AddPair('DataType', TJSONString.Create(QueryFields.FieldByName('FieldDataType').AsString));

            if not QueryFields.FieldByName('FieldType').IsNull then
              FieldObj.AddPair('Type', TJSONString.Create(QueryFields.FieldByName('FieldType').AsString));

            if not QueryFields.FieldByName('FieldLabel').IsNull then
              FieldObj.AddPair('Label', TJSONString.Create(QueryFields.FieldByName('FieldLabel').AsString));

            if not QueryFields.FieldByName('FieldProperties').IsNull then
              FieldObj.AddPair('Properties', TJSONString.Create(QueryFields.FieldByName('FieldProperties').AsString));

            if not QueryFields.FieldByName('FieldOrder').IsNull then
              FieldObj.AddPair('Order', TJSONNumber.Create(QueryFields.FieldByName('FieldOrder').AsInteger));

            // FieldParameters sorgusu
            QueryParams.SQL.Text := 'SELECT * FROM FieldParameters WHERE FieldID = :FieldID';
            QueryParams.ParamByName('FieldID').AsInteger := FieldID;
            QueryParams.Open;

            // Parametreler varsa ekle
            if not QueryParams.IsEmpty then
            begin
              ParametersArray := TJSONArray.Create;

              // Her parametre için
              while not QueryParams.Eof do
              begin
                var ParamObj := TJSONObject.Create;
                ParamObj.AddPair('Name', TJSONString.Create(QueryParams.FieldByName('ParameterName').AsString));
                ParamObj.AddPair('Value', TJSONString.Create(QueryParams.FieldByName('ParameterValue').AsString));
                ParametersArray.Add(ParamObj);
                QueryParams.Next;
              end;

              FieldObj.AddPair('Parameters', ParametersArray);
            end;

            // ComboBox öğeleri için
            if (not QueryFields.FieldByName('FieldType').IsNull) and
               (QueryFields.FieldByName('FieldType').AsString = 'combobox') then
            begin
              // ComboBoxItems sorgusu
              QueryComboItems.SQL.Text := 'SELECT * FROM ComboBoxItems WHERE ComboBoxItemFieldID = :FieldID ORDER BY ComboBoxItemOrder';
              QueryComboItems.ParamByName('FieldID').AsInteger := FieldID;
              QueryComboItems.Open;

              // ComboBox öğeleri varsa ekle
              if not QueryComboItems.IsEmpty then
              begin
                ComboItemsArray := TJSONArray.Create;

                // Her ComboBox öğesi için
                while not QueryComboItems.Eof do
                begin
                  var ItemObj := TJSONObject.Create;
                  ItemObj.AddPair('Value', TJSONString.Create(QueryComboItems.FieldByName('ComboBoxItemValue').AsString));

                  if not QueryComboItems.FieldByName('ComboBoxItemOrder').IsNull then
                    ItemObj.AddPair('Order', TJSONNumber.Create(QueryComboItems.FieldByName('ComboBoxItemOrder').AsInteger));

                  ComboItemsArray.Add(ItemObj);
                  QueryComboItems.Next;
                end;

                FieldObj.AddPair('ComboItems', ComboItemsArray);
              end;
            end;

            // Field'ı Fields nesnesine ekle (FieldName'i anahtar olarak kullan)
            FieldsObject.AddPair(FieldName, FieldObj);
            QueryFields.Next;
          end;

          // Fields nesnesini Header'a ekle (boş olsa bile eklemek zorundayız)
          HeaderObj.AddPair('Fields', FieldsObject);

          // Header'ı Headers dizisine ekle
          HeadersArray.Add(HeaderObj);
          QueryHeaders.Next;
        end;

        // Headers dizisini ana JSON nesnesine ekle
        JSONObject.AddPair('Headers', HeadersArray);
        QueryListViews.Next;
      end;

      // JSON çıktısını döndür
      Result := JSONObject.ToString;

    finally
      FreeAndNil(QueryListViews);
      FreeAndNil(QueryHeaders);
      FreeAndNil(QueryFields);
      FreeAndNil(QueryParams);
      FreeAndNil(QueryComboItems);
    end;
  except
    on E: Exception do
    begin
      FreeAndNil(JSONObject);
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
  Result := 0; // İşlenen kayıt sayısı

  JSONObject := TJSONObject.ParseJSONValue(AJSON) as TJSONObject;
  if not Assigned(JSONObject) then
    raise Exception.Create('Geçersiz JSON formatı');

  try
    // Başlıyoruz
    FConnection.StartTransaction;
    try
      // ListView adını belirle
      if AListViewName <> '' then
        ListViewName := AListViewName
      else if JSONObject.TryGetValue('Name', ListViewName) = False then
        ListViewName := 'DefaultListView';

      // ListView tablosuna kayıt ekle veya güncelle
      var QueryListView := TFDQuery.Create(nil);
      try
        QueryListView.Connection := FConnection;

        // Önceden bu isimde bir ListView var mı kontrol et
        QueryListView.SQL.Text := 'SELECT ListViewID FROM ListViews WHERE ListViewName = :ListViewName';
        QueryListView.ParamByName('ListViewName').AsString := ListViewName;
        QueryListView.Open;

        if QueryListView.IsEmpty then
        begin
          // ListView yoksa ekle
          ExecuteSQL('INSERT INTO ListViews (ListViewName) VALUES (''' +
                      EscapeString(ListViewName) + ''')');
          ListViewID := GetLastInsertRowID;
        end
        else
        begin
          // ListView varsa ID'sini al
          ListViewID := QueryListView.FieldByName('ListViewID').AsInteger;
        end;
      finally
        FreeAndNil(QueryListView);
      end;

      // Headers dizisini al
      if JSONObject.TryGetValue<TJSONArray>('Headers', HeadersArray) then
      begin
        for var i := 0 to HeadersArray.Count - 1 do
        begin
          var HeaderObj := HeadersArray.Items[i] as TJSONObject;
          var HeaderTitle: string;

          if HeaderObj.TryGetValue('Title', HeaderTitle) then
          begin
            // Header tablosuna kayıt ekle
            ExecuteSQL('INSERT INTO Headers (HeaderListViewID, HeaderTitle) VALUES (' +
                        ListViewID.ToString + ', ''' + EscapeString(HeaderTitle) + ''')');
            HeaderID := GetLastInsertRowID;
            Inc(Result);

            // Fields nesnesini al
            if HeaderObj.TryGetValue<TJSONObject>('Fields', FieldsObject) then
            begin
              for var j := 0 to FieldsObject.Count - 1 do
              begin
                var FieldName := FieldsObject.Pairs[j].JsonString.Value;
                var FieldObj := FieldsObject.Pairs[j].JsonValue as TJSONObject;

                var FieldValue, FieldDataType, FieldType, FieldLabel: string;

                // Field değerlerini al
                if not FieldObj.TryGetValue('FieldValue', FieldValue) then FieldValue := '';
                if not FieldObj.TryGetValue('FieldDataType', FieldDataType) then FieldDataType := 'string';
                if not FieldObj.TryGetValue('FieldType', FieldType) then FieldType := 'edit';
                if not FieldObj.TryGetValue('FieldLabel', FieldLabel) then FieldLabel := FieldName;

                // Fields tablosuna kayıt ekle
                ExecuteSQL('INSERT INTO Fields (FieldHeaderID, FieldName, FieldValue, FieldDataType, FieldType, FieldLabel) ' +
                          'VALUES (' + HeaderID.ToString + ', ''' + EscapeString(FieldName) + ''', ''' + EscapeString(FieldValue) + ''', ''' +
                          EscapeString(FieldDataType) + ''', ''' + EscapeString(FieldType) + ''', ''' +
                          EscapeString(FieldLabel) + ''')');
                FieldID := GetLastInsertRowID;
                Inc(Result);

                // Parameters dizisini kontrol et
                var ParametersArray: TJSONArray;
                if FieldObj.TryGetValue<TJSONArray>('Parameters', ParametersArray) then
                begin
                  for var k := 0 to ParametersArray.Count - 1 do
                  begin
                    var ParamObj := ParametersArray.Items[k] as TJSONObject;
                    var ParamKey, ParamValue: string;

                    if ParamObj.TryGetValue('Key', ParamKey) and
                       ParamObj.TryGetValue('Value', ParamValue) then
                    begin
                      // FieldParameters tablosuna kayıt ekle
                      ExecuteSQL('INSERT INTO FieldParameters (FieldID, ParameterName, ParameterValue) ' +
                                'VALUES (' + FieldID.ToString + ', ''' + EscapeString(ParamKey) + ''', ''' +
                                EscapeString(ParamValue) + ''')');
                      Inc(Result);
                    end;
                  end;
                end;

                // ComboBox öğelerini kontrol et
                var ComboItemsArray: TJSONArray;
                if (FieldType = 'combobox') and
                   FieldObj.TryGetValue<TJSONArray>('ComboItems', ComboItemsArray) then
                begin
                  for var l := 0 to ComboItemsArray.Count - 1 do
                  begin
                    var ItemObj := ComboItemsArray.Items[l] as TJSONObject;
                    var ItemText, ItemValue: string;

                    if ItemObj.TryGetValue('Text', ItemText) and
                       ItemObj.TryGetValue('Value', ItemValue) then
                    begin
                      // ComboBoxItems tablosuna kayıt ekle
                      ExecuteSQL('INSERT INTO ComboBoxItems (ComboBoxItemFieldID, ComboBoxItemValue, ComboBoxItemOrder) ' +
                                'VALUES (' + FieldID.ToString + ', ''' + EscapeString(ItemValue) + ''', ' + IntToStr(l) + ')');
                      Inc(Result);
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;

      // Tüm işlemler başarılı, değişiklikleri kaydet
      FConnection.Commit;

    except
      on E: Exception do
      begin
        // Hata durumunda geri al
        FConnection.Rollback;
        Result := 0;
        raise Exception.Create('JSONToDatabase error: ' + E.Message);
      end;
    end;

  finally
    FreeAndNil(JSONObject);
  end;
end;

end.
