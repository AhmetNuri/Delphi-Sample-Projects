 unit TestExpandableListViewU;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ListBox, FMX.Memo,
  FMX.TabControl, FMX.Memo.Types, FMX.ScrollBox, System.JSON, System.IOUtils,
  FMX.Objects, ExpandableListView, ExpandableListViewManager, FMX.EditBox,
  FMX.NumberBox, FMX.Colors, FMX.Ani, System.DateUtils, FMX.Calendar,
  FMX.DateTimeCtrls, FireDAC.UI.Intf, FireDAC.FMXUI.Wait, FireDAC.Stan.Intf,
  FireDAC.Comp.UI, System.Generics.Collections;

type
  TAddComponentType = (actHeader, actEditField, actNumberField, actCheckBox,
                       actComboBox, actColorBox, actRadioButton);



  TfrmTestExpandableListView = class(TForm)
    tabDatabase: TTabItem;
    tabJSON: TTabItem;
    tabComponents: TTabItem;
    lytMain: TLayout;
    memLog: TMemo;
    lblLog: TLabel;
    Rectangle1: TRectangle;
    pnlHeader: TRectangle;
    lblTitle: TLabel;
    lblVersion: TLabel;
    pnlDBSettings: TRectangle;
    edtDatabaseLocation: TEdit;
    lblDatabaseLocation: TLabel;
    btnBrowseDB: TButton;
    pnlDatabaseOps: TRectangle;
    btnSaveListViewToDB: TButton;
    btnLoadListViewFromDB: TButton;
    edtListViewName: TEdit;
    lblListViewName: TLabel;
    btnLoadValuesFromDB: TButton;
    btnSaveValuesToDB: TButton;
    lytExpandableListView: TLayout;
    pnlJSONOps: TRectangle;
    btnExportToJSON: TButton;
    btnLoadFromJSON: TButton;
    btnSaveToJSONFile: TButton;
    btnLoadFromJSONFile: TButton;
    dlgOpenJSON: TOpenDialog;
    dlgSaveJSON: TSaveDialog;
    dlgOpenDB: TOpenDialog;
    memJSON: TMemo;
    btnCreateSampleData: TButton;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Rectangle2: TRectangle;
    pnlComponentsOps: TRectangle;
    pnlHeaderSelection: TRectangle;
    lblSelectHeader: TLabel;
    cmbHeaders: TComboBox;
    btnRefreshHeaders: TButton;
    lblHeaderInfo: TLabel;
    pnlComponentButtons: TRectangle;
    btnAddHeader: TButton;
    btnAddEditField: TButton;
    btnAddNumberField: TButton;
    btnAddCheckBox: TButton;
    btnAddComboBox: TButton;
    btnAddColorBox: TButton;
    btnAddRadioButton: TButton;
    memComponentLog: TMemo;
    lblComponentLog: TLabel;
    btnAddMemo: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnBrowseDBClick(Sender: TObject);
    procedure btnSaveListViewToDBClick(Sender: TObject);
    procedure btnLoadListViewFromDBClick(Sender: TObject);
    procedure btnLoadValuesFromDBClick(Sender: TObject);
    procedure btnSaveValuesToDBClick(Sender: TObject);
    procedure btnExportToJSONClick(Sender: TObject);
    procedure btnLoadFromJSONClick(Sender: TObject);
    procedure btnSaveToJSONFileClick(Sender: TObject);
    procedure btnLoadFromJSONFileClick(Sender: TObject);
    procedure btnCreateSampleDataClick(Sender: TObject);
    // Yeni olaylar
    procedure btnRefreshHeadersClick(Sender: TObject);
    procedure btnAddHeaderClick(Sender: TObject);
    procedure btnAddEditFieldClick(Sender: TObject);
    procedure btnAddNumberFieldClick(Sender: TObject);
    procedure btnAddCheckBoxClick(Sender: TObject);
    procedure btnAddComboBoxClick(Sender: TObject);
    procedure btnAddColorBoxClick(Sender: TObject);
    procedure btnAddRadioButtonClick(Sender: TObject);
    procedure btnAddMemoClick(Sender: TObject);
  private
    { Private declarations }
    FExpandableListViewManager: TExpandableListViewManager;
    FHeaderList: TDictionary<string, THeaderInfo>; // Baþlýklarý ve isimlerini saklamak için
    procedure Log(const AMessage: string);
    procedure ComponentLog(const AMessage: string);
    procedure ClearLog;
    procedure UpdateDatabaseConnection;
    procedure CreateSampleData;
    procedure RefreshHeaderList;
    function GetSelectedHeader: THeaderInfo;

    // Dialog fonksiyonlarý
    function ShowAddHeaderDialog: Boolean;
    function ShowAddEditFieldDialog: Boolean;
    function ShowAddNumberFieldDialog: Boolean;
    function ShowAddCheckBoxDialog: Boolean;
    function ShowAddComboBoxDialog: Boolean;
    function ShowAddColorBoxDialog: Boolean;
    function ShowAddRadioButtonDialog: Boolean;
    function ShowAddMemoFieldDialog: Boolean;

  public
    { Public declarations }
  end;


  TFormX = class(TForm)
  private
    { Private declarations }
  public
    { Public declarations }


  end;
var
  frmTestExpandableListView: TfrmTestExpandableListView;
  FormX   : TFormX  ;
implementation

{$R *.fmx}




procedure TfrmTestExpandableListView.FormCreate(Sender: TObject);
begin
  // Baþlýklar için sözlük oluþtur
  FHeaderList := TDictionary<string, THeaderInfo>.Create;

  // ExpandableListViewManager oluþtur
  FExpandableListViewManager := TExpandableListViewManager.Create(Self);

  // ExpandableListView kontrolünü ayarla
  FExpandableListViewManager.ExpandableListView.Parent := lytExpandableListView;
  FExpandableListViewManager.ExpandableListView.Align := TAlignLayout.Client;
  FExpandableListViewManager.ExpandableListView.EnableLogging := True;

  // Varsayýlan veritabaný konumunu ayarla
  edtDatabaseLocation.Text := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetAppPath, 'expandablelistview.db');

  // Günlük kaydýný etkinleþtir
  FExpandableListViewManager.LogEnabled := True;
  FExpandableListViewManager.LogPath := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'ExpandableListView.log');

  ClearLog;
  Log('Application started');
  Log('Default database location: ' + edtDatabaseLocation.Text);

  // Dialog ayarlarýný yap
  dlgOpenJSON.Filter := 'JSON Files (*.json)|*.json|All Files (*.*)|*.*';
  dlgSaveJSON.Filter := 'JSON Files (*.json)|*.json|All Files (*.*)|*.*';
  dlgOpenDB.Filter := 'SQLite Database (*.db;*.sqlite;*.sqlite3)|*.db;*.sqlite;*.sqlite3|All Files (*.*)|*.*';

  // Baþlýk listesini yenile
  RefreshHeaderList;
end;

procedure TfrmTestExpandableListView.FormDestroy(Sender: TObject);
begin
  // HeaderList'i serbest býrak
  if Assigned(FHeaderList) then
    FreeAndNil(FHeaderList);

  // ExpandableListViewManager'ý serbest býrak
  if Assigned(FExpandableListViewManager) then
    FreeAndNil(FExpandableListViewManager);
end;

procedure TfrmTestExpandableListView.Log(const AMessage: string);
begin
  memLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMessage]));
  memLog.GoToTextEnd;
end;

procedure TfrmTestExpandableListView.ComponentLog(const AMessage: string);
begin
  memComponentLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMessage]));
  memComponentLog.GoToTextEnd;
end;

procedure TfrmTestExpandableListView.ClearLog;
begin
  memLog.Lines.Clear;
  if Assigned(memComponentLog) then
    memComponentLog.Lines.Clear;
end;

procedure TfrmTestExpandableListView.UpdateDatabaseConnection;
begin
  try
    // Close existing connection if open
    if FExpandableListViewManager.DBHelper.FConnection.Connected then
      FExpandableListViewManager.DBHelper.FConnection.Close;

    // Set new database location
    FExpandableListViewManager.DBHelper.FConnection.Params.Database := edtDatabaseLocation.Text;

    // Try to connect
    FExpandableListViewManager.DBHelper.FConnection.Connected := True;

    Log('Database connection updated: ' + edtDatabaseLocation.Text);
  except
    on E: Exception do
    begin
      Log('Error updating database connection: ' + E.Message);
      raise;
    end;
  end;
end;

procedure TfrmTestExpandableListView.btnBrowseDBClick(Sender: TObject);
begin
  if dlgOpenDB.Execute then
  begin
    edtDatabaseLocation.Text := dlgOpenDB.FileName;
    try
      UpdateDatabaseConnection;
    except
      on E: Exception do
        ShowMessage('Failed to connect to database: ' + E.Message);
    end;
  end;
end;

procedure TfrmTestExpandableListView.btnSaveListViewToDBClick(Sender: TObject);
begin
  try
    UpdateDatabaseConnection;

    if FExpandableListViewManager.SaveListViewToDatabase(edtListViewName.Text) then
      Log('Successfully saved ListView to database as "' + edtListViewName.Text + '"')
    else
      Log('Failed to save ListView to database');
  except
    on E: Exception do
      Log('Error saving ListView to database: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnLoadListViewFromDBClick(Sender: TObject);
begin
  try
    UpdateDatabaseConnection;

    if FExpandableListViewManager.LoadListViewFromDatabase(edtListViewName.Text) then
    begin
      Log('Successfully loaded ListView from database: "' + edtListViewName.Text + '"');
      RefreshHeaderList;
      FExpandableListViewManager.ExpandAll;
    end
    else
      Log('Failed to load ListView from database');
  except
    on E: Exception do
      Log('Error loading ListView from database: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnLoadValuesFromDBClick(Sender: TObject);
var
  JSONData: string;
begin
  try
    UpdateDatabaseConnection;

    // This function is not directly exposed in the manager, so we'll access it through DBHelper
    JSONData := FExpandableListViewManager.DBHelper.ExportFieldValuesAsJSON;

    if JSONData <> '' then
    begin
      if FExpandableListViewManager.JSONHelper.UpdateOnlyValuesFromJSON(JSONData) then
      begin
        Log('Successfully loaded values from database');
        memJSON.Text := JSONData;
      end
      else
        Log('Failed to update values from JSON data');
    end
    else
      Log('Failed to export field values from database');
  except
    on E: Exception do
      Log('Error loading values from database: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnSaveValuesToDBClick(Sender: TObject);
var
  JSONData: string;
begin
  try
    UpdateDatabaseConnection;

    // Export values to JSON
    JSONData := FExpandableListViewManager.JSONHelper.ExportValuesToJSON;

    if JSONData <> '' then
    begin
      // This function is not directly exposed, but we can use JSONToDatabase
      if FExpandableListViewManager.DBHelper.JSONToDatabase(JSONData, edtListViewName.Text) > 0 then
      begin
        Log('Successfully saved values to database');
        memJSON.Text := JSONData;
      end
      else
        Log('Failed to save values to database');
    end
    else
      Log('Failed to export values to JSON');
  except
    on E: Exception do
      Log('Error saving values to database: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnExportToJSONClick(Sender: TObject);
var
  JSONData: string;
begin
  try
    JSONData := FExpandableListViewManager.ExportToJSON;

    if JSONData <> '' then
    begin
      Log('Successfully exported to JSON');
      memJSON.Text := JSONData;
    end
    else
      Log('Failed to export to JSON');
  except
    on E: Exception do
      Log('Error exporting to JSON: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnLoadFromJSONClick(Sender: TObject);
begin
  try
    if memJSON.Text <> '' then
    begin
      if FExpandableListViewManager.LoadFromJSON(memJSON.Text) then
      begin
        Log('Successfully loaded from JSON');
        RefreshHeaderList;
      end
      else
        Log('Failed to load from JSON');
    end
    else
      Log('JSON data is empty. Please export JSON first or enter valid JSON.');
  except
    on E: Exception do
      Log('Error loading from JSON: ' + E.Message);
  end;
end;

procedure TfrmTestExpandableListView.btnSaveToJSONFileClick(Sender: TObject);
begin
  if dlgSaveJSON.Execute then
  begin
    try
      if FExpandableListViewManager.SaveToJSONFile(dlgSaveJSON.FileName) then
        Log('Successfully saved to JSON file: ' + dlgSaveJSON.FileName)
      else
        Log('Failed to save to JSON file');
    except
      on E: Exception do
        Log('Error saving to JSON file: ' + E.Message);
    end;
  end;
end;

procedure TfrmTestExpandableListView.btnLoadFromJSONFileClick(Sender: TObject);
begin
  if dlgOpenJSON.Execute then
  begin
    try
      if FExpandableListViewManager.LoadFromJSONFile(dlgOpenJSON.FileName) then
      begin
        Log('Successfully loaded from JSON file: ' + dlgOpenJSON.FileName);

        // Display loaded JSON
        try
          memJSON.Lines.LoadFromFile(dlgOpenJSON.FileName);
          RefreshHeaderList;
        except
          on E: Exception do
            Log('Error loading JSON content to display: ' + E.Message);
        end;
      end
      else
        Log('Failed to load from JSON file');
    except
      on E: Exception do
        Log('Error loading from JSON file: ' + E.Message);
    end;
  end;
end;

procedure TfrmTestExpandableListView.CreateSampleData;
var
  HeaderInfo1, HeaderInfo2, HeaderInfo3: THeaderInfo;
begin
  // Clear any existing data
  FExpandableListViewManager.ExpandableListView.Clear;
  FExpandableListViewManager.ExpandableListView.FHeaders.Clear;

  // Create sample headers with different colors
  HeaderInfo1 := FExpandableListViewManager.AddHeader('Personal Information', 0, $FFDC143C); // Crimson
  HeaderInfo2 := FExpandableListViewManager.AddHeader('Preferences', 1, $FF4682B4); // SteelBlue
  HeaderInfo3 := FExpandableListViewManager.AddHeader('System Information', 2, $FF2E8B57); // SeaGreen

  // Add fields to Personal Information header
  FExpandableListViewManager.AddEditField(HeaderInfo1, 'First Name', 'John');
  FExpandableListViewManager.AddEditField(HeaderInfo1, 'Last Name', 'Smith');
  FExpandableListViewManager.AddEditField(HeaderInfo1, 'Email', 'john.smith@example.com');
  FExpandableListViewManager.AddNumberField(HeaderInfo1, 'Age', 30, 0, 120, nvtInteger);

  // Add fields to Preferences header
  FExpandableListViewManager.AddCheckBoxField(HeaderInfo2, 'Receive Notifications', True);
  FExpandableListViewManager.AddCheckBoxField(HeaderInfo2, 'Dark Mode', False);
  FExpandableListViewManager.AddColorBoxField(HeaderInfo2, 'Theme Color', $FF1E90FF); // DodgerBlue
  FExpandableListViewManager.AddComboBoxField(HeaderInfo2, 'Language', ['English', 'Spanish', 'French', 'German'], 0);
  FExpandableListViewManager.AddRadioButtonField(HeaderInfo2, 'Beginner', True, 'SkillLevel');
  FExpandableListViewManager.AddRadioButtonField(HeaderInfo2, 'Intermediate', False, 'SkillLevel');
  FExpandableListViewManager.AddRadioButtonField(HeaderInfo2, 'Expert', False, 'SkillLevel');

  // Add fields to System Information header
  FExpandableListViewManager.AddEditField(HeaderInfo3, 'Operating System', 'Windows 11');
  FExpandableListViewManager.AddEditField(HeaderInfo3, 'Processor', 'Intel Core i7');
  FExpandableListViewManager.AddNumberField(HeaderInfo3, 'RAM (GB)', 16, 1, 128, nvtInteger);
  FExpandableListViewManager.AddNumberField(HeaderInfo3, 'Disk Space (GB)', 512, 1, 2000, nvtInteger);

  // Add SVG data for icons
  FExpandableListViewManager.SetHeaderSVG(HeaderInfo1,
    '<svg viewBox="0 0 24 24"><path d="M12 12c2.21 0 4-1.79 4-4s-1.79-4-4-4-4 1.79-4 4 1.79 4 4 4zm0 2c-2.67 0-8 1.34-8 4v2h16v-2c0-2.66-5.33-4-8-4z"/></svg>');

  FExpandableListViewManager.SetHeaderSVG(HeaderInfo2,
    '<svg viewBox="0 0 24 24"><path d="M19.14 12.94c.04-.3.06-.61.06-.94 0-.32-.02-.64-.07-.94l2.03-1.58c.18-.14.23-.41.12-.61l-1.92-3.32c-.12-.22-.37-.29-.59-.22l-2.39.96c-.5-.38-1.03-.7-1.62-.94l-.3[...]');

  FExpandableListViewManager.SetHeaderSVG(HeaderInfo3,
    '<svg viewBox="0 0 24 24"><path d="M21 2H3c-1.1 0-2 .9-2 2v12c0 1.1.9 2 2 2h7v2H8v2h8v-2h-2v-2h7c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 14H3V4h18v12z"/></svg>');

  // Expand all sections
  FExpandableListViewManager.ExpandAll;

  // Refresh the header list
  RefreshHeaderList;

  Log('Sample data created successfully');
end;

procedure TfrmTestExpandableListView.btnCreateSampleDataClick(Sender: TObject);
begin
  CreateSampleData;
end;

// YENÝ EKLENEN FONKSÝYONLAR

procedure TfrmTestExpandableListView.RefreshHeaderList;
var
  I: Integer;
  HeaderTitle: string;
begin
  // Sözlüðü ve combobox'ý temizle
  FHeaderList.Clear;
  cmbHeaders.Clear;

  // Mevcut baþlýklarý döngüye sok
  for I := 0 to FExpandableListViewManager.ExpandableListView.FHeaders.Count - 1 do
  begin
    HeaderTitle := FExpandableListViewManager.ExpandableListView.FHeaders[I].Title;
    FHeaderList.Add(HeaderTitle, FExpandableListViewManager.ExpandableListView.FHeaders[I]);
    cmbHeaders.Items.Add(HeaderTitle);
  end;

  // Eðer varsa ilk öðeyi seç
  if cmbHeaders.Items.Count > 0 then
    cmbHeaders.ItemIndex := 0;

  if Assigned(memComponentLog) then
    ComponentLog('Baþlýk listesi yenilendi, ' + cmbHeaders.Items.Count.ToString + ' baþlýk bulundu');
end;

function TfrmTestExpandableListView.GetSelectedHeader: THeaderInfo;
var
  HeaderTitle: string;
begin
  Result := nil;

  // Seçili bir baþlýk var mý kontrol et
  if (cmbHeaders.ItemIndex >= 0) and (cmbHeaders.ItemIndex < cmbHeaders.Items.Count) then
  begin
    HeaderTitle := cmbHeaders.Items[cmbHeaders.ItemIndex];
    if FHeaderList.ContainsKey(HeaderTitle) then
      Result := FHeaderList[HeaderTitle];
  end;
end;

procedure TfrmTestExpandableListView.btnRefreshHeadersClick(Sender: TObject);
begin
  RefreshHeaderList;
end;

// Header Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddHeaderDialog: Boolean;
var
  Form: TFormX;
  TitleEdit: TEdit;
  ColorBox: TColorComboBox;
  ImageIndexNumberBox: TNumberBox;
  SVGMemo: TMemo;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  HeaderTitle: string;
  HeaderColor: TAlphaColor;
  HeaderImageIndex: Integer;
  HeaderSVG: string;
begin
  Result := False;

  // Form oluþtur
  Application.CreateForm(TFormX,   Form);
  //Form := TFormX.Create(Application);
  try
    Form.Caption := 'Baþlýk Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 350;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Baþlýk alaný
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Baþlýk:';
    end;

    TitleEdit := TEdit.Create(Form);
    TitleEdit.Parent := Form;
    TitleEdit.Position.X := 120;
    TitleEdit.Position.Y := 20;
    TitleEdit.Size.Width := 300;
    TitleEdit.Size.Height := 30;
    TitleEdit.Text := 'Yeni Baþlýk';

    // Renk alaný
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Renk:';
    end;

    ColorBox := TColorComboBox.Create(Form);
    ColorBox.Parent := Form;
    ColorBox.Position.X := 120;
    ColorBox.Position.Y := 60;
    ColorBox.Size.Width := 300;
    ColorBox.Size.Height := 30;
    ColorBox.Color := TAlphaColorRec.Skyblue;

    // Image Index alaný
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 100;
      Text := 'Image Index:';
    end;

    ImageIndexNumberBox := TNumberBox.Create(Form);
    ImageIndexNumberBox.Parent := Form;
    ImageIndexNumberBox.Position.X := 120;
    ImageIndexNumberBox.Position.Y := 100;
    ImageIndexNumberBox.Size.Width := 300;
    ImageIndexNumberBox.Size.Height := 30;
    ImageIndexNumberBox.Min := 0;
    ImageIndexNumberBox.Max := 100;
    ImageIndexNumberBox.Value := 0;

    // SVG Veri alaný
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 140;
      Text := 'SVG Veri:';
    end;

    SVGMemo := TMemo.Create(Form);
    SVGMemo.Parent := Form;
    SVGMemo.Position.X := 120;
    SVGMemo.Position.Y := 140;
    SVGMemo.Size.Width := 300;
    SVGMemo.Size.Height := 120;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 280;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 280;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      HeaderTitle := TitleEdit.Text;
      HeaderColor := ColorBox.Color;
      HeaderImageIndex := Round(ImageIndexNumberBox.Value);
      HeaderSVG := SVGMemo.Text;

      // Baþlýk ekle
      HeaderInfo := FExpandableListViewManager.AddHeader(HeaderTitle, HeaderImageIndex, HeaderColor);

      // SVG ekle (eðer girilmiþse)
      if not HeaderSVG.IsEmpty then
        FExpandableListViewManager.SetHeaderSVG(HeaderInfo, HeaderSVG);

      ComponentLog('Baþlýk "' + HeaderTitle + '" baþarýyla eklendi');
      RefreshHeaderList;
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;
// Memo Ekle
function TfrmTestExpandableListView.ShowAddMemoFieldDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit, ValueEdit: TMemo;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName, FieldValue: string;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
    Application.CreateForm(TFormX,   Form);
  //  Form := TFormX.Create(Application);
  try
    Form.Caption := 'Metin Alaný Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 180;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TMemo.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 120;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 300;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni Alan';

    // Deðer
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Deðer:';
    end;

    ValueEdit := TMemo.Create(Form);
    ValueEdit.Parent := Form;
    ValueEdit.Position.X := 120;
    ValueEdit.Position.Y := 60;
    ValueEdit.Size.Width := 300;
    ValueEdit.Size.Height := 30;
    ValueEdit.Text := '';

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 110;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 110;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      FieldValue := ValueEdit.Text;

      // Alan ekle
      FExpandableListViewManager.AddMemoField(HeaderInfo, FieldName, FieldValue);

      ComponentLog('Memo Alaný "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// Edit Field Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddEditFieldDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit, ValueEdit: TEdit;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName, FieldValue: string;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
    Application.CreateForm(TFormX,   Form);
  //  Form := TFormX.Create(Application);
  try
    Form.Caption := 'Metin Alaný Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 180;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 120;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 300;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni Alan';

    // Deðer
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Deðer:';
    end;

    ValueEdit := TEdit.Create(Form);
    ValueEdit.Parent := Form;
    ValueEdit.Position.X := 120;
    ValueEdit.Position.Y := 60;
    ValueEdit.Size.Width := 300;
    ValueEdit.Size.Height := 30;
    ValueEdit.Text := '';

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 110;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 110;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      FieldValue := ValueEdit.Text;

      // Alan ekle
      FExpandableListViewManager.AddEditField(HeaderInfo, FieldName, FieldValue);

      ComponentLog('Metin Alaný "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// Number Field Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddNumberFieldDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit: TEdit;
  ValueNumberBox, MinNumberBox, MaxNumberBox: TNumberBox;
  ValueTypeComboBox: TComboBox;
  VertIncrementCheckBox: TCheckBox;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName: string;
  FieldValue, MinValue, MaxValue: Double;
  ValueType: TNumberValueType;
  VertIncrement: Boolean;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
  Form := TFormX.Create(Application);
  try
    Form.Caption := 'Sayý Alaný Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 340;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 150;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 270;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni Sayý';

    // Deðer
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Deðer:';
    end;

    ValueNumberBox := TNumberBox.Create(Form);
    ValueNumberBox.Parent := Form;
    ValueNumberBox.Position.X := 150;
    ValueNumberBox.Position.Y := 60;
    ValueNumberBox.Size.Width := 270;
    ValueNumberBox.Size.Height := 30;
    ValueNumberBox.Value := 0;

    // Min Deðer
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 100;
      Text := 'Min Deðer:';
    end;

    MinNumberBox := TNumberBox.Create(Form);
    MinNumberBox.Parent := Form;
    MinNumberBox.Position.X := 150;
    MinNumberBox.Position.Y := 100;
    MinNumberBox.Size.Width := 270;
    MinNumberBox.Size.Height := 30;
    MinNumberBox.Value := 0;

    // Max Deðer
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 140;
      Text := 'Max Deðer:';
    end;

    MaxNumberBox := TNumberBox.Create(Form);
    MaxNumberBox.Parent := Form;
    MaxNumberBox.Position.X := 150;
    MaxNumberBox.Position.Y := 140;
    MaxNumberBox.Size.Width := 270;
    MaxNumberBox.Size.Height := 30;
    MaxNumberBox.Value := 100;

    // Deðer Tipi
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 180;
      Text := 'Deðer Tipi:';
    end;

    ValueTypeComboBox := TComboBox.Create(Form);
    ValueTypeComboBox.Parent := Form;
    ValueTypeComboBox.Position.X := 150;
    ValueTypeComboBox.Position.Y := 180;
    ValueTypeComboBox.Size.Width := 270;
    ValueTypeComboBox.Size.Height := 30;
    ValueTypeComboBox.Items.Add('Float');
    ValueTypeComboBox.Items.Add('Integer');
    ValueTypeComboBox.Items.Add('Currency');
    ValueTypeComboBox.ItemIndex := 0;

    // Dikey Artýrma
    VertIncrementCheckBox := TCheckBox.Create(Form);
    VertIncrementCheckBox.Parent := Form;
    VertIncrementCheckBox.Position.X := 150;
    VertIncrementCheckBox.Position.Y := 220;
    VertIncrementCheckBox.Size.Width := 270;
    VertIncrementCheckBox.Size.Height := 30;
    VertIncrementCheckBox.Text := 'Dikey Artýrma';
    VertIncrementCheckBox.IsChecked := False;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 270;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 270;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      FieldValue := ValueNumberBox.Value;
      MinValue := MinNumberBox.Value;
      MaxValue := MaxNumberBox.Value;

      case ValueTypeComboBox.ItemIndex of
        0: ValueType := nvtFloat;
        1: ValueType := nvtInteger;
        2: ValueType := nvtCurrency;
      else
        ValueType := nvtFloat;
      end;

      VertIncrement := VertIncrementCheckBox.IsChecked;

      // Sayý alaný ekle
      FExpandableListViewManager.AddNumberField(
        HeaderInfo, FieldName, FieldValue, MinValue, MaxValue, ValueType, VertIncrement);

      ComponentLog('Sayý Alaný "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// CheckBox Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddCheckBoxDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit: TEdit;
  IsCheckedCheckBox: TCheckBox;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName: string;
  IsChecked: Boolean;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
  Form := TFormX.Create(Application);
  try
    Form.Caption := 'CheckBox Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 180;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 120;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 300;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni CheckBox';

    // Ýþaretli mi
    IsCheckedCheckBox := TCheckBox.Create(Form);
    IsCheckedCheckBox.Parent := Form;
    IsCheckedCheckBox.Position.X := 120;
    IsCheckedCheckBox.Position.Y := 60;
    IsCheckedCheckBox.Size.Width := 300;
    IsCheckedCheckBox.Size.Height := 30;
    IsCheckedCheckBox.Text := 'Ýþaretli';
    IsCheckedCheckBox.IsChecked := False;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 110;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 110;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      IsChecked := IsCheckedCheckBox.IsChecked;

      // CheckBox ekle
      FExpandableListViewManager.AddCheckBoxField(HeaderInfo, FieldName, IsChecked);

      ComponentLog('CheckBox "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// ComboBox Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddComboBoxDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit: TEdit;
  ItemsMemo: TMemo;
  SelectedIndexNumberBox: TNumberBox;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName: string;
  Items: TArray<string>;
  SelectedIndex, I: Integer;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
  Form := TFormx.Create(Application);
  try
    Form.Caption := 'ComboBox Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 300;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 150;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 270;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni ComboBox';

    // Öðeler
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Öðeler (her satýrda bir öðe):';
    end;

    ItemsMemo := TMemo.Create(Form);
    ItemsMemo.Parent := Form;
    ItemsMemo.Position.X := 150;
    ItemsMemo.Position.Y := 60;
    ItemsMemo.Size.Width := 270;
    ItemsMemo.Size.Height := 120;
    ItemsMemo.Text := 'Öðe 1' + sLineBreak + 'Öðe 2' + sLineBreak + 'Öðe 3';

    // Seçili Öðe Indeksi
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 190;
      Text := 'Seçili Öðe Ýndeksi:';
    end;

    SelectedIndexNumberBox := TNumberBox.Create(Form);
    SelectedIndexNumberBox.Parent := Form;
    SelectedIndexNumberBox.Position.X := 150;
    SelectedIndexNumberBox.Position.Y := 190;
    SelectedIndexNumberBox.Size.Width := 270;
    SelectedIndexNumberBox.Size.Height := 30;
    SelectedIndexNumberBox.Value := 0;
    SelectedIndexNumberBox.Min := 0;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 240;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 240;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      SelectedIndex := Round(SelectedIndexNumberBox.Value);

      // Memo'dan öðeleri al
      SetLength(Items, ItemsMemo.Lines.Count);
      for I := 0 to ItemsMemo.Lines.Count - 1 do
        Items[I] := ItemsMemo.Lines[I];

      // ComboBox ekle
      FExpandableListViewManager.AddComboBoxField(HeaderInfo, FieldName, Items, SelectedIndex);

      ComponentLog('ComboBox "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// ColorBox Ekleme Dialog Fonksiyonu
// ColorBox Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddColorBoxDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit: TEdit;
  ColorComboBox: TColorComboBox;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName: string;
  SelectedColor: TAlphaColor;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
  Form := TFormX.Create(Application);
  try
    Form.Caption := 'Renk Kutusu Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 180;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 120;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 300;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni Renk Seçici';

    // Renk
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Renk:';
    end;

    ColorComboBox := TColorComboBox.Create(Form);
    ColorComboBox.Parent := Form;
    ColorComboBox.Position.X := 120;
    ColorComboBox.Position.Y := 60;
    ColorComboBox.Size.Width := 300;
    ColorComboBox.Size.Height := 30;
    ColorComboBox.Color := TAlphaColorRec.Blue;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 110;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 110;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      SelectedColor := ColorComboBox.Color;

      // ColorBox ekle
      FExpandableListViewManager.AddColorBoxField(HeaderInfo, FieldName, SelectedColor);

      ComponentLog('Renk Kutusu "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// RadioButton Ekleme Dialog Fonksiyonu
function TfrmTestExpandableListView.ShowAddRadioButtonDialog: Boolean;
var
  Form: TFormX;
  FieldNameEdit, GroupNameEdit: TEdit;
  IsCheckedCheckBox: TCheckBox;
  OKButton, CancelButton: TButton;
  HeaderInfo: THeaderInfo;
  FieldName, GroupName: string;
  IsChecked: Boolean;
begin
  Result := False;

  // Baþlýk seçili mi kontrol et
  HeaderInfo := GetSelectedHeader;
  if HeaderInfo = nil then
  begin
    ShowMessage('Lütfen önce bir baþlýk seçin veya mevcut deðilse yeni bir baþlýk oluþturun');
    Exit;
  end;

  // Form oluþtur
  Form := TFormX.Create(Application);
  try
    Form.Caption := 'Radyo Düðmesi Ekle';
    Form.Position := TFormPosition.ScreenCenter;
    Form.Width := 450;
    Form.Height := 220;
    Form.BorderStyle := TFmxFormBorderStyle.Sizeable;
    Form.BorderIcons := [TBorderIcon.biSystemMenu];

    // Alan Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 20;
      Text := 'Alan Adý:';
    end;

    FieldNameEdit := TEdit.Create(Form);
    FieldNameEdit.Parent := Form;
    FieldNameEdit.Position.X := 120;
    FieldNameEdit.Position.Y := 20;
    FieldNameEdit.Size.Width := 300;
    FieldNameEdit.Size.Height := 30;
    FieldNameEdit.Text := 'Yeni Radyo Düðmesi';

    // Grup Adý
    with TLabel.Create(Form) do
    begin
      Parent := Form;
      Position.X := 20;
      Position.Y := 60;
      Text := 'Grup Adý:';
    end;

    GroupNameEdit := TEdit.Create(Form);
    GroupNameEdit.Parent := Form;
    GroupNameEdit.Position.X := 120;
    GroupNameEdit.Position.Y := 60;
    GroupNameEdit.Size.Width := 300;
    GroupNameEdit.Size.Height := 30;
    GroupNameEdit.Text := 'VarsayýlanRadyoGrubu';

    // Ýþaretli mi
    IsCheckedCheckBox := TCheckBox.Create(Form);
    IsCheckedCheckBox.Parent := Form;
    IsCheckedCheckBox.Position.X := 120;
    IsCheckedCheckBox.Position.Y := 100;
    IsCheckedCheckBox.Size.Width := 300;
    IsCheckedCheckBox.Size.Height := 30;
    IsCheckedCheckBox.Text := 'Ýþaretli';
    IsCheckedCheckBox.IsChecked := False;

    // Butonlar
    OKButton := TButton.Create(Form);
    OKButton.Parent := Form;
    OKButton.Position.X := 240;
    OKButton.Position.Y := 150;
    OKButton.Size.Width := 80;
    OKButton.Size.Height := 35;
    OKButton.Text := 'Tamam';
    OKButton.ModalResult := mrOk;

    CancelButton := TButton.Create(Form);
    CancelButton.Parent := Form;
    CancelButton.Position.X := 340;
    CancelButton.Position.Y := 150;
    CancelButton.Size.Width := 80;
    CancelButton.Size.Height := 35;
    CancelButton.Text := 'Ýptal';
    CancelButton.ModalResult := mrCancel;

    // Formu göster
    if Form.ShowModal = mrOk then
    begin
      FieldName := FieldNameEdit.Text;
      GroupName := GroupNameEdit.Text;
      IsChecked := IsCheckedCheckBox.IsChecked;

      // RadioButton ekle
      FExpandableListViewManager.AddRadioButtonField(HeaderInfo, FieldName, IsChecked, GroupName);

      ComponentLog('Radyo Düðmesi "' + FieldName + '" baþarýyla "' + HeaderInfo.Title + '" baþlýðýna eklendi');
      Result := True;
    end;

  finally
    Form.Free;
  end;
end;

// Buton Olay Ýþleyicileri
procedure TfrmTestExpandableListView.btnAddHeaderClick(Sender: TObject);
begin
  ShowAddHeaderDialog;
end;

procedure TfrmTestExpandableListView.btnAddMemoClick(Sender: TObject);
begin
ShowAddMemoFieldDialog;
end;

procedure TfrmTestExpandableListView.btnAddEditFieldClick(Sender: TObject);
begin
  ShowAddEditFieldDialog;
end;

procedure TfrmTestExpandableListView.btnAddNumberFieldClick(Sender: TObject);
begin
  ShowAddNumberFieldDialog;
end;

procedure TfrmTestExpandableListView.btnAddCheckBoxClick(Sender: TObject);
begin
  ShowAddCheckBoxDialog;
end;

procedure TfrmTestExpandableListView.btnAddComboBoxClick(Sender: TObject);
begin
  ShowAddComboBoxDialog;
end;

procedure TfrmTestExpandableListView.btnAddColorBoxClick(Sender: TObject);
begin
  ShowAddColorBoxDialog;
end;

procedure TfrmTestExpandableListView.btnAddRadioButtonClick(Sender: TObject);
begin
  ShowAddRadioButtonDialog;
end;



end.
