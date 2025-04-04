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
  FireDAC.Comp.UI;

type
  TfrmTestExpandableListView = class(TForm)
    tabDatabase: TTabItem;
    tabJSON: TTabItem;
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
  private
    { Private declarations }
    FExpandableListViewManager: TExpandableListViewManager;
    procedure Log(const AMessage: string);
    procedure ClearLog;
    procedure UpdateDatabaseConnection;
    procedure CreateSampleData;
  public
    { Public declarations }
  end;

var
  frmTestExpandableListView: TfrmTestExpandableListView;

implementation

{$R *.fmx}

procedure TfrmTestExpandableListView.FormCreate(Sender: TObject);
begin
  // Create the ExpandableListViewManager
  FExpandableListViewManager := TExpandableListViewManager.Create(Self);

  // Setup the ExpandableListView control
  FExpandableListViewManager.ExpandableListView.Parent := lytExpandableListView;
  FExpandableListViewManager.ExpandableListView.Align := TAlignLayout.Client;
  FExpandableListViewManager.ExpandableListView.EnableLogging := True;

  // Set default database location
  edtDatabaseLocation.Text := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetAppPath, 'expandablelistview.db');

  // Enable logging
  FExpandableListViewManager.LogEnabled := True;
  FExpandableListViewManager.LogPath := System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath, 'ExpandableListView.log');

  ClearLog;
  Log('Application started');
  Log('Default database location: ' + edtDatabaseLocation.Text);

  // Set up dialogs
  dlgOpenJSON.Filter := 'JSON Files (*.json)|*.json|All Files (*.*)|*.*';
  dlgSaveJSON.Filter := 'JSON Files (*.json)|*.json|All Files (*.*)|*.*';
  dlgOpenDB.Filter := 'SQLite Database (*.db;*.sqlite;*.sqlite3)|*.db;*.sqlite;*.sqlite3|All Files (*.*)|*.*';
end;

procedure TfrmTestExpandableListView.FormDestroy(Sender: TObject);
begin
  // Free the ExpandableListViewManager
  if Assigned(FExpandableListViewManager) then
    FreeAndNil(FExpandableListViewManager);
end;

procedure TfrmTestExpandableListView.Log(const AMessage: string);
begin
  memLog.Lines.Add(Format('[%s] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMessage]));
  memLog.GoToTextEnd;
end;

procedure TfrmTestExpandableListView.ClearLog;
begin
  memLog.Lines.Clear;
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
      Log('Successfully loaded ListView from database: "' + edtListViewName.Text + '"')
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
        Log('Successfully loaded from JSON')
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
    '<svg viewBox="0 0 24 24"><path d="M19.14 12.94c.04-.3.06-.61.06-.94 0-.32-.02-.64-.07-.94l2.03-1.58c.18-.14.23-.41.12-.61l-1.92-3.32c-.12-.22-.37-.29-.59-.22l-2.39.96c-.5-.38-1.03-.7-1.62-.94l-.36-2.54c-.04-.24-.24-.41-.48-.41h-3.84c-.24 0-.43.17-.47.41l-.36 2.54c-.59.24-1.13.57-1.62.94l-2.39-.96c-.22-.08-.47 0-.59.22L2.74 8.87c-.12.21-.08.47.12.61l2.03 1.58c-.05.3-.09.63-.09.94s.02.64.07.94l-2.03 1.58c-.18.14-.23.41-.12.61l1.92 3.32c.12.22.37.29.59.22l2.39-.96c.5.38 1.03.7 1.62.94l.36 2.54c.05.24.24.41.48.41h3.84c.24 0 .44-.17.47-.41l.36-2.54c.59-.24 1.13-.56 1.62-.94l2.39.96c.22.08.47 0 .59-.22l1.92-3.32c.12-.22.07-.47-.12-.61l-2.01-1.58zM12 15.6c-1.98 0-3.6-1.62-3.6-3.6s1.62-3.6 3.6-3.6 3.6 1.62 3.6 3.6-1.62 3.6-3.6 3.6z"/></svg>');
  
  FExpandableListViewManager.SetHeaderSVG(HeaderInfo3, 
    '<svg viewBox="0 0 24 24"><path d="M21 2H3c-1.1 0-2 .9-2 2v12c0 1.1.9 2 2 2h7v2H8v2h8v-2h-2v-2h7c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2zm0 14H3V4h18v12z"/></svg>');

  // Expand all sections
  FExpandableListViewManager.ExpandAll;

  Log('Sample data created successfully');
end;

procedure TfrmTestExpandableListView.btnCreateSampleDataClick(Sender: TObject);
begin
  CreateSampleData;
end;

end.
