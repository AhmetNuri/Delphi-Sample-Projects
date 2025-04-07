unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.UITypes,
  System.Math,
  System.Types,
  System.Math.Vectors,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Menus, Vcl.ToolWin, Vcl.ImgList,
  Vcl.Grids, System.ImageList,

  // For TList, TQueue
  System.Generics.Collections,
  System.Generics.Defaults,

  // MtxVec / TeeChart / Math387
  MtxExpr,
  MtxVec,
  MtxVecTools,
  Math387,
  VCLTee.TeEngine, VCLTee.TeeProcs, VCLTee.Chart, VCLTee.Series;

type
  TAnimationAxis = (aaNone, aaX, aaY, aaZ);

  TPoint3D = record
    X, Y, Z: Double;
    OrigX, OrigY, OrigZ: Double;
    AnimationAxis: TAnimationAxis;
    AnimationPhase: Double;
  end;

  TEdge = record
    FromIndex, ToIndex: Integer;
  end;

  TMyComplex = record
    Re, Im: Double;
  end;

  TNodeModeShape = record
    X, Y, Z: TMyComplex;
  end;

  TfrmMain = class(TForm)
    MainMenu1: TMainMenu;
    mnuFile: TMenuItem;
    mnuNew: TMenuItem;
    mnuOpen: TMenuItem;
    mnuSave: TMenuItem;
    mnuExit: TMenuItem;
    mnuEdit: TMenuItem;
    mnuDeleteNode: TMenuItem;
    mnuView: TMenuItem;
    mnuTogglePanel: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAbout: TMenuItem;
    ToolBar1: TToolBar;
    ToolImages: TImageList;
    btnNew: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    StatusBar1: TStatusBar;
    pnlLeft: TPanel;
    grdCoordinates: TStringGrid;
    btnAddNodeLeft: TButton;
    btnDeleteNodeLeft: TButton;
    btnConnectNodesLeft: TButton;
    btnClearAll: TButton;
    lblObjectName: TLabel;
    edtObjectName: TEdit;
    lblGlobalCoords: TLabel;
    lblGlobalX: TLabel;
    lblGlobalY: TLabel;
    lblGlobalZ: TLabel;
    pnlBottom: TPanel;
    lblSpeed: TLabel;
    trkSpeed: TTrackBar;
    lblOscillation: TLabel;
    trkOscillation: TTrackBar;
    btnStartAnim: TButton;
    btnStopAnim: TButton;
    chkOMA: TCheckBox;
    PaintBox1: TPaintBox;
    tmrAnimation: TTimer;
    pnlOMA: TPanel;
    lblSelectMode: TLabel;
    cmbModeSelect: TComboBox;
    lblFreq: TLabel;
    edtFreq: TEdit;
    lblDamp: TLabel;
    edtDamp: TEdit;
    btnApplyOMA: TButton;
    btnRevertOMA: TButton;
    btnAnalyzeOMA: TButton;
    chartOMA: TChart;
    serOMA: TLineSeries;
    btnAttachOMA: TButton;
    btnDetachOMA: TButton;
    splLeft: TSplitter;
    splRight: TSplitter;
    splBottom: TSplitter;
    Panel1: TPanel;
    grdModeData: TStringGrid;
    lblApplyDone: TLabel;
    lblAnalyzeDone: TLabel;
    lblAttachDone: TLabel;
    lblDetachDone: TLabel;
    tmrHideDone: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint; var Handled: Boolean);
    procedure tmrAnimationTimer(Sender: TObject);
    procedure trkSpeedChange(Sender: TObject);
    procedure trkOscillationChange(Sender: TObject);
    procedure grdCoordinatesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);

    procedure mnuNewClick(Sender: TObject);
    procedure mnuExitClick(Sender: TObject);
    procedure mnuDeleteNodeClick(Sender: TObject);
    procedure mnuTogglePanelClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

    // Node manipulation
    procedure btnAddNodeLeftClick(Sender: TObject);
    procedure btnDeleteNodeLeftClick(Sender: TObject);
    procedure btnConnectNodesLeftClick(Sender: TObject);
    procedure btnClearAllClick(Sender: TObject);

    procedure btnStartAnimClick(Sender: TObject);
    procedure btnStopAnimClick(Sender: TObject);

    // OMA
    procedure cmbModeSelectChange(Sender: TObject);
    procedure btnApplyOMAClick(Sender: TObject);
    procedure btnRevertOMAClick(Sender: TObject);
    procedure btnAnalyzeOMAClick(Sender: TObject);
    procedure btnAttachOMAClick(Sender: TObject);
    procedure btnDetachOMAClick(Sender: TObject);

    // Grid selection sync
    procedure grdCoordinatesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure grdModeDataMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);

    // Hide "Done." label timer
    procedure tmrHideDoneTimer(Sender: TObject);

  private
    FNodes: array of TPoint3D;
    FEdges: array of TEdge;

    FModeShapes: array of array of TNodeModeShape;
    FFrequencies: array of Double;
    FDampings: array of Double;
    FDefaultModeShapes: array of array of TNodeModeShape;
    FDefaultFreqs: array of Double;
    FDefaultDamps: array of Double;
    FModeCount: Integer;
    FSelectedMode: Integer;
    FTimeAccumulator: Double;

    FOMAAttached: array of Boolean;

    // Mouse nav
    FMouseDown: Boolean;
    FLastMouseX, FLastMouseY: Integer;
    FRightMouseDown: Boolean;
    FLastRightMouseX, FLastRightMouseY: Integer;
    FMiddleMouseDown: Boolean;
    FLastMiddleMouseX, FLastMiddleMouseY: Integer;

    // 3D transforms
    FRotationX, FRotationY, FRotationZ: Double;
    FScale: Double;
    FZoom: Double;
    FPanOffsetX, FPanOffsetY: Integer;

    FOscillation: Double;
    FAnimationSpeedFactor: Double;

    FPerspective: Boolean;
    FShowGrid: Boolean;
    FShowAxis: Boolean;
    FShowLabels: Boolean;

    procedure InitializeGrids;
    procedure CreateTwoStackedCubes;
    procedure PopulateGridFromNodes;
    procedure UpdateNodesFromGrid;

    procedure InitializeFakeOMAData;
    procedure OMAUpdateNodes(ATime: Double);

    procedure DrawScene(Canvas: TCanvas);
    procedure DrawGradientBackground(Canvas: TCanvas);
    procedure DrawFloorGrid(Canvas: TCanvas);
    procedure DrawAxis(Canvas: TCanvas);
    procedure DrawCubes(Canvas: TCanvas);
    procedure DrawNodeLabels(Canvas: TCanvas);
    procedure DrawOscillationArrow(const NodePos: TPoint3D;
      Axis: TAnimationAxis; ACanvas: TCanvas);
    function ProjectPoint(const P: TPoint3D): TPoint;

    procedure DeleteNode(Index: Integer);
    procedure ConnectNodes(Node1, Node2: Integer);

    procedure ResetView;
    procedure ZoomExtents;
    procedure UpdateStatus;

    procedure btnTogglePerspectiveClick(Sender: TObject);
    procedure btnZoomExtentsClick(Sender: TObject);
    procedure btnResetViewClick(Sender: TObject);
    procedure btnToggleGridClick(Sender: TObject);
    procedure btnToggleAxisClick(Sender: TObject);
    procedure btnToggleLabelsClick(Sender: TObject);

    procedure PopulateOMAUI;
    procedure ApplyOMAUI;
    procedure RevertOMAData;

    // BFS for unattached nodes
    procedure GuessUnattachedModeShapes;
    procedure BFSForUnattachedNode(UnIdx: Integer);

    // Save/load
    procedure SaveModelToFile(const AFileName: string);
    procedure LoadModelFromFile(const AFileName: string);

    // Automatic col sizing
    procedure AutoSizeGridColumns(Grid: TStringGrid);

    // Multi-row selection
    function GetSelectedRows(Grid: TStringGrid): TArray<Integer>;
    procedure SyncGridSelections(Source, Target: TStringGrid);

  public
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

uses
  System.StrUtils;

//==============================================================================
procedure TfrmMain.FormCreate(Sender: TObject);
var
  newBtn: TToolButton;
begin
  Self.DoubleBuffered := True;
  Randomize;

  BorderStyle := bsSizeable;
  WindowState := wsMaximized;

  while ToolBar1.ButtonCount>5 do
    ToolBar1.Buttons[ToolBar1.ButtonCount-1].Free;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Labels On/Off';
  newBtn.OnClick := btnToggleLabelsClick;
  newBtn.AutoSize := True;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Axis On/Off';
  newBtn.OnClick := btnToggleAxisClick;
  newBtn.AutoSize := True;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Grid On/Off';
  newBtn.OnClick := btnToggleGridClick;
  newBtn.AutoSize := True;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Reset';
  newBtn.OnClick := btnResetViewClick;
  newBtn.AutoSize := True;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Zoom Extents';
  newBtn.OnClick := btnZoomExtentsClick;
  newBtn.AutoSize := True;

  newBtn := TToolButton.Create(ToolBar1);
  newBtn.Parent := ToolBar1;
  newBtn.Caption := 'Ortho/Persp';
  newBtn.OnClick := btnTogglePerspectiveClick;
  newBtn.AutoSize := True;

  ToolBar1.ButtonWidth := 70;
  ToolBar1.ShowCaptions := True;
  ToolBar1.Wrapable := True;

  FRotationX := -Pi/2;
  FRotationY := 0;
  FRotationZ := 0;
  FScale := 1;
  FZoom := 1.0;
  FPanOffsetX := 0;
  FPanOffsetY := 0;

  tmrAnimation.Interval := 16;
  tmrAnimation.Enabled := False;
  FAnimationSpeedFactor := trkSpeed.Position/100.0;

  FOscillation := trkOscillation.Position;
  FPerspective := True;
  FShowGrid := True;
  FShowAxis := True;
  FShowLabels := True;

  InitializeGrids;
  CreateTwoStackedCubes;
  PopulateGridFromNodes;

  InitializeFakeOMAData;
  PopulateOMAUI;
  chkOMA.Checked := False;
  FTimeAccumulator := 0;

  lblApplyDone.Visible := False;
  lblAnalyzeDone.Visible := False;
  lblAttachDone.Visible := False;
  lblDetachDone.Visible := False;
  tmrHideDone.Enabled := False;

  UpdateStatus;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  PaintBox1.Invalidate;
  UpdateStatus;
end;

//==============================================================================
procedure TfrmMain.InitializeGrids;
begin
  // We ALLOW cell editing and SHIFT-based multi-row selection
  grdCoordinates.Options := [goFixedVertLine, goFixedHorzLine,
                             goVertLine, goHorzLine,
                             goEditing, goRangeSelect];
  grdCoordinates.DefaultRowHeight := 22;
  grdCoordinates.DefaultColWidth  := 60;

  grdModeData.Options := [goFixedVertLine, goFixedHorzLine,
                          goVertLine, goHorzLine,
                          goEditing, goRangeSelect];
  grdModeData.DefaultRowHeight := 22;
  grdModeData.DefaultColWidth  := 60;

  // Coordinates
  grdCoordinates.ColCount := 5;
  grdCoordinates.RowCount := 2;
  grdCoordinates.FixedRows := 1;
  grdCoordinates.Cells[0,0] := 'Index';
  grdCoordinates.Cells[1,0] := 'X';
  grdCoordinates.Cells[2,0] := 'Y';
  grdCoordinates.Cells[3,0] := 'Z';
  grdCoordinates.Cells[4,0] := 'Axis';

  // Mode Data
  grdModeData.ColCount := 7;
  grdModeData.RowCount := 2;
  grdModeData.FixedRows := 1;
  grdModeData.Cells[0,0] := 'Index';
  grdModeData.Cells[1,0] := 'X.Re';
  grdModeData.Cells[2,0] := 'X.Im';
  grdModeData.Cells[3,0] := 'Y.Re';
  grdModeData.Cells[4,0] := 'Y.Im';
  grdModeData.Cells[5,0] := 'Z.Re';
  grdModeData.Cells[6,0] := 'Z.Im';
end;

procedure TfrmMain.CreateTwoStackedCubes;
var
  i: Integer;
begin
  SetLength(FNodes, 12);

  // bottom face
  FNodes[0].OrigX := -50; FNodes[0].OrigY := -50; FNodes[0].OrigZ := 0;
  FNodes[1].OrigX :=  50; FNodes[1].OrigY := -50; FNodes[1].OrigZ := 0;
  FNodes[2].OrigX :=  50; FNodes[2].OrigY :=  50; FNodes[2].OrigZ := 0;
  FNodes[3].OrigX := -50; FNodes[3].OrigY :=  50; FNodes[3].OrigZ := 0;

  // top face
  FNodes[4].OrigX := -50; FNodes[4].OrigY := -50; FNodes[4].OrigZ := 100;
  FNodes[5].OrigX :=  50; FNodes[5].OrigY := -50; FNodes[5].OrigZ := 100;
  FNodes[6].OrigX :=  50; FNodes[6].OrigY :=  50; FNodes[6].OrigZ := 100;
  FNodes[7].OrigX := -50; FNodes[7].OrigY :=  50; FNodes[7].OrigZ := 100;

  // second cube on top
  FNodes[8].OrigX  := -50; FNodes[8].OrigY  := -50; FNodes[8].OrigZ  := 200;
  FNodes[9].OrigX  :=  50; FNodes[9].OrigY  := -50; FNodes[9].OrigZ  := 200;
  FNodes[10].OrigX :=  50; FNodes[10].OrigY :=  50; FNodes[10].OrigZ := 200;
  FNodes[11].OrigX := -50; FNodes[11].OrigY :=  50; FNodes[11].OrigZ := 200;

  for i := 0 to 11 do
  begin
    FNodes[i].X := FNodes[i].OrigX;
    FNodes[i].Y := FNodes[i].OrigY;
    FNodes[i].Z := FNodes[i].OrigZ;
    FNodes[i].AnimationAxis := aaX;
    FNodes[i].AnimationPhase := 0;
  end;

  SetLength(FEdges, 0);
  ConnectNodes(0,1); ConnectNodes(1,2); ConnectNodes(2,3); ConnectNodes(3,0);
  ConnectNodes(4,5); ConnectNodes(5,6); ConnectNodes(6,7); ConnectNodes(7,4);
  ConnectNodes(0,4); ConnectNodes(1,5); ConnectNodes(2,6); ConnectNodes(3,7);
  ConnectNodes(8,9); ConnectNodes(9,10); ConnectNodes(10,11); ConnectNodes(11,8);
  ConnectNodes(4,8); ConnectNodes(5,9); ConnectNodes(6,10); ConnectNodes(7,11);

  SetLength(FOMAAttached, 12);
  for i := 0 to 11 do
    FOMAAttached[i] := True;
end;

procedure TfrmMain.PopulateGridFromNodes;
var
  i: Integer;
  axisStr: string;
begin
  grdCoordinates.RowCount := Length(FNodes)+1;
  for i := 0 to High(FNodes) do
  begin
    grdCoordinates.Cells[0, i+1] := IntToStr(i);
    grdCoordinates.Cells[1, i+1] := FloatToStrF(FNodes[i].X, ffGeneral, 6, 4);
    grdCoordinates.Cells[2, i+1] := FloatToStrF(FNodes[i].Y, ffGeneral, 6, 4);
    grdCoordinates.Cells[3, i+1] := FloatToStrF(FNodes[i].Z, ffGeneral, 6, 4);

    case FNodes[i].AnimationAxis of
      aaNone: axisStr := '0';
      aaX:    axisStr := 'X';
      aaY:    axisStr := 'Y';
      aaZ:    axisStr := 'Z';
    end;
    grdCoordinates.Cells[4, i+1] := axisStr;
  end;
  AutoSizeGridColumns(grdCoordinates);
end;

procedure TfrmMain.UpdateNodesFromGrid;
var
  i: Integer;
  rowIndex: Integer;
  valX, valY, valZ: Double;
  axisStr: string;
begin
  for i := 0 to High(FNodes) do
  begin
    rowIndex := i+1;
    if TryStrToFloat(grdCoordinates.Cells[1, rowIndex], valX) then
      FNodes[i].X := valX;
    if TryStrToFloat(grdCoordinates.Cells[2, rowIndex], valY) then
      FNodes[i].Y := valY;
    if TryStrToFloat(grdCoordinates.Cells[3, rowIndex], valZ) then
      FNodes[i].Z := valZ;

    axisStr := grdCoordinates.Cells[4, rowIndex];
    if axisStr='0' then
      FNodes[i].AnimationAxis := aaNone
    else if axisStr='X' then
      FNodes[i].AnimationAxis := aaX
    else if axisStr='Y' then
      FNodes[i].AnimationAxis := aaY
    else if axisStr='Z' then
      FNodes[i].AnimationAxis := aaZ;

    FNodes[i].OrigX := FNodes[i].X;
    FNodes[i].OrigY := FNodes[i].Y;
    FNodes[i].OrigZ := FNodes[i].Z;
  end;
end;

//==============================================================================
procedure TfrmMain.InitializeFakeOMAData;
var
  i, m: Integer;
begin
  FModeCount := 2;
  SetLength(FModeShapes, FModeCount);
  SetLength(FFrequencies, FModeCount);
  SetLength(FDampings, FModeCount);
  SetLength(FDefaultModeShapes, FModeCount);
  SetLength(FDefaultFreqs, FModeCount);
  SetLength(FDefaultDamps, FModeCount);

  for m := 0 to FModeCount-1 do
  begin
    FFrequencies[m] := 1.0 + (m*0.5);
    FDampings[m] := 0.02 + (m*0.01);
    FDefaultFreqs[m] := FFrequencies[m];
    FDefaultDamps[m] := FDampings[m];

    SetLength(FModeShapes[m], Length(FNodes));
    SetLength(FDefaultModeShapes[m], Length(FNodes));

    for i := 0 to High(FNodes) do
    begin
      FModeShapes[m][i].X.Re := Random*50;
      FModeShapes[m][i].X.Im := Random*50;
      FModeShapes[m][i].Y.Re := Random*50;
      FModeShapes[m][i].Y.Im := Random*50;
      FModeShapes[m][i].Z.Re := Random*50;
      FModeShapes[m][i].Z.Im := Random*50;

      FDefaultModeShapes[m][i] := FModeShapes[m][i];
    end;
  end;
  FSelectedMode := 0;
end;

procedure TfrmMain.OMAUpdateNodes(ATime: Double);
var
  i: Integer;
  freq, damp, w: Double;
  cosPart, sinPart, decay: Double;
begin
  if (Length(FNodes)=0) or (FModeCount=0) then Exit;
  if (FSelectedMode<0) or (FSelectedMode>=FModeCount) then Exit;

  freq := FFrequencies[FSelectedMode];
  damp := FDampings[FSelectedMode];
  w := 2*Pi*freq;

  for i := 0 to High(FNodes) do
  begin
    if not FOMAAttached[i] then
    begin
      FNodes[i].X := FNodes[i].OrigX;
      FNodes[i].Y := FNodes[i].OrigY;
      FNodes[i].Z := FNodes[i].OrigZ;
      Continue;
    end;

    cosPart := Cos(w*ATime);
    sinPart := Sin(w*ATime);
    decay := Exp(-damp*w*ATime);

    FNodes[i].X := FNodes[i].OrigX + decay*
      (FModeShapes[FSelectedMode][i].X.Re*cosPart
       - FModeShapes[FSelectedMode][i].X.Im*sinPart);
    FNodes[i].Y := FNodes[i].OrigY + decay*
      (FModeShapes[FSelectedMode][i].Y.Re*cosPart
       - FModeShapes[FSelectedMode][i].Y.Im*sinPart);
    FNodes[i].Z := FNodes[i].OrigZ + decay*
      (FModeShapes[FSelectedMode][i].Z.Re*cosPart
       - FModeShapes[FSelectedMode][i].Z.Im*sinPart);
  end;
  PaintBox1.Invalidate;
end;

procedure TfrmMain.tmrAnimationTimer(Sender: TObject);
var
  i: Integer;
  delta: Double;
begin
  if Length(FNodes)=0 then Exit;

  delta := 0.1*FAnimationSpeedFactor;

  if chkOMA.Checked then
  begin
    FTimeAccumulator := FTimeAccumulator+(0.01*FAnimationSpeedFactor);
    OMAUpdateNodes(FTimeAccumulator);
    Exit;
  end;

  for i := 0 to High(FNodes) do
  begin
    case FNodes[i].AnimationAxis of
      aaX:
        begin
          FNodes[i].AnimationPhase := FNodes[i].AnimationPhase+delta;
          if FNodes[i].AnimationPhase>2*Pi then
            FNodes[i].AnimationPhase := FNodes[i].AnimationPhase-2*Pi;
          FNodes[i].X := FNodes[i].OrigX+FOscillation*Sin(FNodes[i].AnimationPhase);
        end;
      aaY:
        begin
          FNodes[i].AnimationPhase := FNodes[i].AnimationPhase+delta;
          if FNodes[i].AnimationPhase>2*Pi then
            FNodes[i].AnimationPhase := FNodes[i].AnimationPhase-2*Pi;
          FNodes[i].Y := FNodes[i].OrigY+FOscillation*Sin(FNodes[i].AnimationPhase);
        end;
      aaZ:
        begin
          FNodes[i].AnimationPhase := FNodes[i].AnimationPhase+delta;
          if FNodes[i].AnimationPhase>2*Pi then
            FNodes[i].AnimationPhase := FNodes[i].AnimationPhase-2*Pi;
          FNodes[i].Z := FNodes[i].OrigZ+FOscillation*Sin(FNodes[i].AnimationPhase);
        end;
      aaNone: ;
    end;
  end;

  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.trkSpeedChange(Sender: TObject);
begin
  FAnimationSpeedFactor := trkSpeed.Position/100.0;
  UpdateStatus;
end;

procedure TfrmMain.trkOscillationChange(Sender: TObject);
begin
  FOscillation := trkOscillation.Position;
  UpdateStatus;
end;

procedure TfrmMain.grdCoordinatesSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  if (ARow>0) and (ARow-1<=High(FNodes)) then
  begin
    UpdateNodesFromGrid;
    PaintBox1.Invalidate;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.mnuNewClick(Sender: TObject);
begin
  tmrAnimation.Enabled := False;

  btnClearAllClick(Sender);
  CreateTwoStackedCubes;
  PopulateGridFromNodes;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.mnuExitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuDeleteNodeClick(Sender: TObject);
begin
  btnDeleteNodeLeftClick(Sender);
end;

procedure TfrmMain.mnuTogglePanelClick(Sender: TObject);
begin
  pnlLeft.Visible := not pnlLeft.Visible;
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  MessageDlg('2 Stacked Cubes (Shared Face) Demo'+sLineBreak+
             'With SHIFT-based multi-row selection and BFS amplitude scaling.'+sLineBreak+
             'Version 5.5',
             mtInformation, [mbOK], 0);
end;

procedure TfrmMain.btnNewClick(Sender: TObject);
begin
  mnuNewClick(Sender);
end;

procedure TfrmMain.btnOpenClick(Sender: TObject);
var
  dlg: TOpenDialog;
begin
  dlg := TOpenDialog.Create(Self);
  try
    dlg.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
    if dlg.Execute then
    begin
      tmrAnimation.Enabled := False;
      LoadModelFromFile(dlg.FileName);
    end;
  finally
    dlg.Free;
  end;
end;

procedure TfrmMain.btnSaveClick(Sender: TObject);
var
  dlg: TSaveDialog;
begin
  dlg := TSaveDialog.Create(Self);
  try
    dlg.Filter := 'Text Files (*.txt)|*.txt|All Files (*.*)|*.*';
    if dlg.Execute then
      SaveModelToFile(dlg.FileName);
  finally
    dlg.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.btnAddNodeLeftClick(Sender: TObject);
var
  pt: TPoint3D;
  idx: Integer;
begin
  pt.OrigX := 0; pt.OrigY := 0; pt.OrigZ := 0;
  pt.X := 0; pt.Y := 0; pt.Z := 0;
  pt.AnimationAxis := aaX;
  pt.AnimationPhase := 0;

  idx := Length(FNodes);
  SetLength(FNodes, idx+1);
  FNodes[idx] := pt;

  SetLength(FOMAAttached, idx+1);
  FOMAAttached[idx] := True;

  if FModeCount>0 then
  begin
    SetLength(FModeShapes[0], idx+1);
    FillChar(FModeShapes[0][idx], SizeOf(TNodeModeShape), 0);
    if FModeCount>1 then
    begin
      SetLength(FModeShapes[1], idx+1);
      FModeShapes[1][idx] := FModeShapes[0][idx];
    end;
  end;

  PopulateGridFromNodes;
  PopulateOMAUI;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnDeleteNodeLeftClick(Sender: TObject);
var
  rowsCoord, rowsMode: TArray<Integer>;
  i: Integer;
begin
  rowsCoord := GetSelectedRows(grdCoordinates);
  if Length(rowsCoord)>0 then
  begin
    for i := High(rowsCoord) downto 0 do
      DeleteNode(rowsCoord[i]);
    PopulateGridFromNodes;
    PopulateOMAUI;
    PaintBox1.Invalidate;
    UpdateStatus;
    Exit;
  end;

  rowsMode := GetSelectedRows(grdModeData);
  if Length(rowsMode)>0 then
  begin
    for i := High(rowsMode) downto 0 do
      DeleteNode(rowsMode[i]);
    PopulateGridFromNodes;
    PopulateOMAUI;
    PaintBox1.Invalidate;
    UpdateStatus;
    Exit;
  end;

  ShowMessage('No rows selected on either grid.');
end;

procedure TfrmMain.btnConnectNodesLeftClick(Sender: TObject);
var
  rowsCoord: TArray<Integer>;
begin
  rowsCoord := GetSelectedRows(grdCoordinates);
  if Length(rowsCoord)<>2 then
  begin
    ShowMessage('Please SHIFT-select exactly 2 rows in the Coordinates grid to connect.');
    Exit;
  end;

  ConnectNodes(rowsCoord[0], rowsCoord[1]);
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnClearAllClick(Sender: TObject);
begin
  tmrAnimation.Enabled := False;

  SetLength(FNodes,0);
  SetLength(FEdges,0);
  SetLength(FOMAAttached,0);

  if FModeCount>0 then
  begin
    SetLength(FModeShapes[0],0);
    if FModeCount>1 then
      SetLength(FModeShapes[1],0);
  end;

  InitializeGrids;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnStartAnimClick(Sender: TObject);
begin
  if Length(FNodes)=0 then
  begin
    ShowMessage('No nodes exist. Add or open a model first.');
    Exit;
  end;
  tmrAnimation.Enabled := True;
  UpdateStatus;
end;

procedure TfrmMain.btnStopAnimClick(Sender: TObject);
var
  i: Integer;
begin
  tmrAnimation.Enabled := False;
  for i := 0 to High(FNodes) do
  begin
    FNodes[i].X := FNodes[i].OrigX;
    FNodes[i].Y := FNodes[i].OrigY;
    FNodes[i].Z := FNodes[i].OrigZ;
    FNodes[i].AnimationPhase := 0;
  end;
  FTimeAccumulator := 0;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.DeleteNode(Index: Integer);
var
  i,j: Integer;
begin
  if (Index<0) or (Index>High(FNodes)) then Exit;

  for i := High(FEdges) downto 0 do
  begin
    if (FEdges[i].FromIndex=Index) or (FEdges[i].ToIndex=Index) then
    begin
      for j := i to High(FEdges)-1 do
        FEdges[j] := FEdges[j+1];
      SetLength(FEdges, Length(FEdges)-1);
    end
    else
    begin
      if FEdges[i].FromIndex>Index then Dec(FEdges[i].FromIndex);
      if FEdges[i].ToIndex>Index then Dec(FEdges[i].ToIndex);
    end;
  end;

  for i := Index to High(FNodes)-1 do
  begin
    FNodes[i] := FNodes[i+1];
    FOMAAttached[i] := FOMAAttached[i+1];
  end;
  SetLength(FNodes, Length(FNodes)-1);
  SetLength(FOMAAttached, Length(FOMAAttached)-1);

  if FModeCount>0 then
  begin
    for i := 0 to FModeCount-1 do
    begin
      for j := Index to High(FModeShapes[i])-1 do
        FModeShapes[i][j] := FModeShapes[i][j+1];
      SetLength(FModeShapes[i], Length(FModeShapes[i])-1);
    end;
  end;
end;

procedure TfrmMain.ConnectNodes(Node1, Node2: Integer);
begin
  if (Node1<0) or (Node1>High(FNodes)) or
     (Node2<0) or (Node2>High(FNodes)) then
  begin
    ShowMessage('Node indices out of range.');
    Exit;
  end;
  SetLength(FEdges, Length(FEdges)+1);
  FEdges[High(FEdges)].FromIndex := Node1;
  FEdges[High(FEdges)].ToIndex   := Node2;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.PaintBox1Paint(Sender: TObject);
begin
  DrawScene(PaintBox1.Canvas);
end;

procedure TfrmMain.DrawScene(Canvas: TCanvas);
begin
  DrawGradientBackground(Canvas);
  if FShowGrid then
    DrawFloorGrid(Canvas);
  if FShowAxis then
    DrawAxis(Canvas);
  DrawCubes(Canvas);
  if FShowLabels then
    DrawNodeLabels(Canvas);
end;

procedure TfrmMain.DrawGradientBackground(Canvas: TCanvas);
var
  R: TRect;
  i: Integer;
  topColor, bottomColor: TColor;
  r1,g1,b1, r2,g2,b2: Byte;
  h: Integer;
begin
  R := Rect(0,0, PaintBox1.Width, PaintBox1.Height);
  topColor := $00FFF7ED;
  bottomColor := $00FFF0D8;

  r1 := GetRValue(ColorToRGB(topColor));
  g1 := GetGValue(ColorToRGB(topColor));
  b1 := GetBValue(ColorToRGB(topColor));
  r2 := GetRValue(ColorToRGB(bottomColor));
  g2 := GetGValue(ColorToRGB(bottomColor));
  b2 := GetBValue(ColorToRGB(bottomColor));

  h := R.Bottom-R.Top;
  for i := 0 to h-1 do
  begin
    Canvas.Pen.Color := RGB(
      r1 + MulDiv(i,(r2-r1), h-1),
      g1 + MulDiv(i,(g2-g1), h-1),
      b1 + MulDiv(i,(b2-b1), h-1));
    Canvas.MoveTo(R.Left, R.Top+i);
    Canvas.LineTo(R.Right, R.Top+i);
  end;
end;

procedure TfrmMain.DrawFloorGrid(Canvas: TCanvas);
var
  i: Integer;
  step, maxDist: Integer;
  tmp: TPoint3D;
  p1,p2: TPoint;
begin
  Canvas.Pen.Color := clGray;
  Canvas.Pen.Style := psDot;
  step := 20;
  maxDist := 300;

  for i := -maxDist to maxDist do
  begin
    if (i mod step)<>0 then Continue;

    tmp.X := -maxDist;
    tmp.Y := i;
    tmp.Z := 0;
    p1 := ProjectPoint(tmp);
    tmp.X := maxDist;
    p2 := ProjectPoint(tmp);
    Canvas.MoveTo(p1.X,p1.Y);
    Canvas.LineTo(p2.X,p2.Y);

    tmp.X := i;
    tmp.Y := -maxDist;
    p1 := ProjectPoint(tmp);
    tmp.Y := maxDist;
    p2 := ProjectPoint(tmp);
    Canvas.MoveTo(p1.X,p1.Y);
    Canvas.LineTo(p2.X,p2.Y);
  end;
  Canvas.Pen.Style := psSolid;
end;

function TfrmMain.ProjectPoint(const P: TPoint3D): TPoint;
const
  C_FOCAL_LENGTH=3000;
  C_NEAR_OFFSET=20;
var
  x1,y1,z1: Double;
  x2,y2,z2: Double;
  x3,y3,z3: Double;
  factor, nearPlane: Double;
begin
  x1 := Cos(FRotationY)*P.X + Sin(FRotationY)*P.Z;
  y1 := P.Y;
  z1 := -Sin(FRotationY)*P.X + Cos(FRotationY)*P.Z;

  x2 := x1;
  y2 := Cos(FRotationX)*y1 - Sin(FRotationX)*z1;
  z2 := Sin(FRotationX)*y1 + Cos(FRotationX)*z1;

  x3 := (x2*Cos(FRotationZ))-(y2*Sin(FRotationZ));
  y3 := (x2*Sin(FRotationZ))+(y2*Cos(FRotationZ));
  z3 := z2;

  if FPerspective then
  begin
    nearPlane := -(C_FOCAL_LENGTH-C_NEAR_OFFSET);
    if z3<nearPlane then
      z3 := nearPlane;
    factor := C_FOCAL_LENGTH/(C_FOCAL_LENGTH+z3);
  end
  else
    factor := 1.0;

  Result.X := Round((x3*factor*FZoom*FScale)+FPanOffsetX+(PaintBox1.Width/2));
  Result.Y := Round(-(y3*factor*FZoom*FScale)+FPanOffsetY+(PaintBox1.Height/2));
end;

procedure TfrmMain.DrawAxis(Canvas: TCanvas);
var
  origin, px, py, pz: TPoint;
  dx,dy: Integer;
  function ProjectLocalAxis(AX,AY,AZ: Double): TPoint;
  const
    CFocal=3000;
    CNear=20;
  var
    xx1,yy1,zz1: Double;
    xx2,yy2,zz2: Double;
    xx3,yy3: Double;
    f: Double;
  begin
    xx1 := Cos(FRotationY)*AX + Sin(FRotationY)*AZ;
    yy1 := AY;
    zz1 := -Sin(FRotationY)*AX + Cos(FRotationY)*AZ;

    xx2 := xx1;
    yy2 := Cos(FRotationX)*yy1 - Sin(FRotationX)*zz1;
    zz2 := Sin(FRotationX)*yy1 + Cos(FRotationX)*zz1;

    xx3 := (xx2*Cos(FRotationZ))-(yy2*Sin(FRotationZ));
    yy3 := (xx2*Sin(FRotationZ))+(yy2*Cos(FRotationZ));

    if FPerspective then
    begin
      if zz2<-(CFocal - CNear) then
        zz2 := -(CFocal - CNear);
      f := CFocal/(CFocal+zz2);
    end
    else
      f := 1.0;

    Result.X := Round(xx3*f*FZoom*FScale);
    Result.Y := Round(-yy3*f*FZoom*FScale);
  end;
begin
  origin := ProjectLocalAxis(0,0,0);
  px     := ProjectLocalAxis(30,0,0);
  py     := ProjectLocalAxis(0,30,0);
  pz     := ProjectLocalAxis(0,0,30);

  dx := 50-origin.X;
  dy := (PaintBox1.Height-50)-origin.Y;

  origin.Offset(dx, dy);
  px.Offset(dx, dy);
  py.Offset(dx, dy);
  pz.Offset(dx, dy);

  Canvas.Pen.Width := 2;

  Canvas.Pen.Color := clRed;
  Canvas.MoveTo(origin.X, origin.Y);
  Canvas.LineTo(px.X, px.Y);
  Canvas.TextOut(px.X+2, px.Y-2, 'X');

  Canvas.Pen.Color := clGreen;
  Canvas.MoveTo(origin.X, origin.Y);
  Canvas.LineTo(py.X, py.Y);
  Canvas.TextOut(py.X+2, py.Y-2, 'Y');

  Canvas.Pen.Color := clBlue;
  Canvas.MoveTo(origin.X, origin.Y);
  Canvas.LineTo(pz.X, pz.Y);
  Canvas.TextOut(pz.X+2, pz.Y-2, 'Z');
end;

procedure TfrmMain.DrawCubes(Canvas: TCanvas);
var
  i: Integer;
  p1,p2: TPoint;
begin
  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 2;
  for i := 0 to High(FEdges) do
  begin
    p1 := ProjectPoint(FNodes[FEdges[i].FromIndex]);
    p2 := ProjectPoint(FNodes[FEdges[i].ToIndex]);
    Canvas.MoveTo(p1.X,p1.Y);
    Canvas.LineTo(p2.X,p2.Y);
  end;

  for i := 0 to High(FNodes) do
  begin
    p1 := ProjectPoint(FNodes[i]);
    Canvas.Brush.Color := clYellow;
    Canvas.Pen.Color := clBlack;
    Canvas.Rectangle(p1.X-4, p1.Y-4, p1.X+4, p1.Y+4);
    DrawOscillationArrow(FNodes[i], FNodes[i].AnimationAxis, Canvas);
  end;
end;

procedure TfrmMain.DrawNodeLabels(Canvas: TCanvas);
var
  i: Integer;
  p: TPoint;
begin
  Canvas.Font.Size := 12;
  Canvas.Font.Color := clBlack;
  for i := 0 to High(FNodes) do
  begin
    p := ProjectPoint(FNodes[i]);
    Canvas.Brush.Style := bsClear;
    Canvas.TextOut(p.X+5, p.Y-8, IntToStr(i));
  end;
end;

procedure TfrmMain.DrawOscillationArrow(const NodePos: TPoint3D; Axis: TAnimationAxis;
  ACanvas: TCanvas);
var
  basePt, tipPt, lineEnd: TPoint;
  arrowDir: TPoint3D;
  dx, dy, len2D: Double;
  arrowHeadLen, arrowHeadWidth: Double;
  arrowPts: array[0..2] of TPoint;
begin
  if Axis=aaNone then Exit;

  arrowHeadLen := 12;
  arrowHeadWidth := 7;
  arrowDir := NodePos;

  case Axis of
    aaX: arrowDir.X := arrowDir.X+40;
    aaY: arrowDir.Y := arrowDir.Y+40;
    aaZ: arrowDir.Z := arrowDir.Z+40;
  end;

  basePt := ProjectPoint(NodePos);
  tipPt  := ProjectPoint(arrowDir);
  dx := tipPt.X-basePt.X;
  dy := tipPt.Y-basePt.Y;
  len2D := Sqrt(dx*dx+dy*dy);
  if len2D<1e-6 then Exit;

  dx := dx/len2D;
  dy := dy/len2D;

  lineEnd.X := tipPt.X-Round(dx*arrowHeadLen);
  lineEnd.Y := tipPt.Y-Round(dy*arrowHeadLen);

  ACanvas.Pen.Color := clBlue;
  ACanvas.Pen.Width := 2;
  ACanvas.MoveTo(basePt.X, basePt.Y);
  ACanvas.LineTo(lineEnd.X,lineEnd.Y);

  arrowPts[0] := tipPt;
  arrowPts[1] := Point(
    tipPt.X-Round(dx*arrowHeadLen)+Round(dy*(arrowHeadWidth/2)),
    tipPt.Y-Round(dy*arrowHeadLen)-Round(dx*(arrowHeadWidth/2))
  );
  arrowPts[2] := Point(
    tipPt.X-Round(dx*arrowHeadLen)-Round(dy*(arrowHeadWidth/2)),
    tipPt.Y-Round(dy*arrowHeadLen)+Round(dx*(arrowHeadWidth/2))
  );

  ACanvas.Brush.Color := clBlue;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.Polygon(arrowPts);
end;

//------------------------------------------------------------------------------
procedure TfrmMain.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  FZoom := FZoom+(WheelDelta/120)*0.05;
  if FZoom<0.05 then
    FZoom := 0.05;
  PaintBox1.Invalidate;
  UpdateStatus;
  Handled := True;
end;

procedure TfrmMain.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
  begin
    FMouseDown := True;
    FLastMouseX := X;
    FLastMouseY := Y;
  end
  else if Button=mbRight then
  begin
    FRightMouseDown := True;
    FLastRightMouseX := X;
    FLastRightMouseY := Y;
  end
  else if Button=mbMiddle then
  begin
    FMiddleMouseDown := True;
    FLastMiddleMouseX := X;
    FLastMiddleMouseY := Y;
  end;
end;

procedure TfrmMain.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  dx, dy: Integer;
begin
  if FMouseDown then
  begin
    dx := X-FLastMouseX;
    dy := Y-FLastMouseY;
    FRotationY := FRotationY+(dx*0.01);
    FRotationX := FRotationX+(dy*0.01);
    FLastMouseX := X;
    FLastMouseY := Y;
    PaintBox1.Invalidate;
    UpdateStatus;
  end
  else if FRightMouseDown then
  begin
    dx := X-FLastRightMouseX;
    dy := Y-FLastRightMouseY;
    FPanOffsetX := FPanOffsetX+dx;
    FPanOffsetY := FPanOffsetY+dy;
    FLastRightMouseX := X;
    FLastRightMouseY := Y;
    PaintBox1.Invalidate;
    UpdateStatus;
  end
  else if FMiddleMouseDown then
  begin
    dx := X-FLastMiddleMouseX;
    FRotationZ := FRotationZ+(dx*0.01);
    FLastMiddleMouseX := X;
    FLastMiddleMouseY := Y;
    PaintBox1.Invalidate;
    UpdateStatus;
  end;
end;

procedure TfrmMain.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button=mbLeft then
    FMouseDown := False
  else if Button=mbRight then
    FRightMouseDown := False
  else if Button=mbMiddle then
    FMiddleMouseDown := False;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.ResetView;
begin
  FRotationX := -Pi/2;
  FRotationY := 0;
  FRotationZ := 0;
  FZoom := 1.0;
  FPanOffsetX := 0;
  FPanOffsetY := 0;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.ZoomExtents;
var
  i: Integer;
  minX, maxX, minY, maxY, dx, dy, range: Double;
begin
  if Length(FNodes)=0 then Exit;
  minX := FNodes[0].X;
  maxX := FNodes[0].X;
  minY := FNodes[0].Y;
  maxY := FNodes[0].Y;

  for i := 1 to High(FNodes) do
  begin
    if FNodes[i].X<minX then minX := FNodes[i].X;
    if FNodes[i].X>maxX then maxX := FNodes[i].X;
    if FNodes[i].Y<minY then minY := FNodes[i].Y;
    if FNodes[i].Y>maxY then maxY := FNodes[i].Y;
  end;

  dx := maxX-minX;
  dy := maxY-minY;
  if dx<1 then dx := 1;
  if dy<1 then dy := 1;
  range := System.Math.Max(dx,dy);

  FRotationX := -Pi/2;
  FRotationY := 0;
  FRotationZ := 0;
  FZoom := (PaintBox1.Width/range)*0.4;
  FPanOffsetX := 0;
  FPanOffsetY := 0;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.UpdateStatus;
var
  modeStr: string;
begin
  if FPerspective then
    modeStr := 'Perspective'
  else
    modeStr := 'Orthographic';

  StatusBar1.SimpleText :=
    Format('RotX=%.2f RotY=%.2f RotZ=%.2f Zoom=%.2f Nodes=%d Edges=%d Mode=%s OMA=%s SpeedFactor=%.2f',
    [FRotationX,FRotationY,FRotationZ,FZoom,
     Length(FNodes),Length(FEdges),
     modeStr,
     BoolToStr(chkOMA.Checked,True),
     FAnimationSpeedFactor]);
end;

procedure TfrmMain.btnTogglePerspectiveClick(Sender: TObject);
begin
  FPerspective := not FPerspective;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnZoomExtentsClick(Sender: TObject);
begin
  ZoomExtents;
end;

procedure TfrmMain.btnResetViewClick(Sender: TObject);
begin
  ResetView;
end;

procedure TfrmMain.btnToggleGridClick(Sender: TObject);
begin
  FShowGrid := not FShowGrid;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnToggleAxisClick(Sender: TObject);
begin
  FShowAxis := not FShowAxis;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

procedure TfrmMain.btnToggleLabelsClick(Sender: TObject);
begin
  FShowLabels := not FShowLabels;
  PaintBox1.Invalidate;
  UpdateStatus;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.cmbModeSelectChange(Sender: TObject);
begin
  FSelectedMode := cmbModeSelect.ItemIndex;
  PopulateOMAUI;
end;

procedure TfrmMain.btnApplyOMAClick(Sender: TObject);
begin
  ApplyOMAUI;
  lblApplyDone.Visible := True;
  tmrHideDone.Enabled := False;
  tmrHideDone.Enabled := True;
end;

procedure TfrmMain.btnRevertOMAClick(Sender: TObject);
begin
  RevertOMAData;
  ShowMessage('OMA data reverted to defaults.');
end;

procedure TfrmMain.btnAnalyzeOMAClick(Sender: TObject);
var
  freqVec, magVec: TVec;
  i,node: Integer;
  freqStart,freqStep,freqEnd, localAmp: Double;
  reX,imX,reY,imY,reZ,imZ: Double;
begin
  if (FModeCount=0) or (Length(FNodes)=0) then
  begin
    ShowMessage('Cannot Analyze OMA. No mode data or no nodes.');
    Exit;
  end;
  if (FSelectedMode<0) or (FSelectedMode>=FModeCount) then Exit;

  freqStart := 0.0;
  freqStep  := 0.2;
  freqEnd   := 20.0;

  freqVec := TVec.Create;
  magVec  := TVec.Create;
  try
    freqVec.Length := Trunc((freqEnd-freqStart)/freqStep)+1;
    magVec.Length := freqVec.Length;
    for i := 0 to freqVec.Length-1 do
      freqVec.Values[i] := freqStart + i*freqStep;

    for i := 0 to freqVec.Length-1 do
    begin
      localAmp := 0.0;
      for node := 0 to High(FNodes) do
      begin
        if not FOMAAttached[node] then Continue;
        reX := FModeShapes[FSelectedMode][node].X.Re;
        imX := FModeShapes[FSelectedMode][node].X.Im;
        reY := FModeShapes[FSelectedMode][node].Y.Re;
        imY := FModeShapes[FSelectedMode][node].Y.Im;
        reZ := FModeShapes[FSelectedMode][node].Z.Re;
        imZ := FModeShapes[FSelectedMode][node].Z.Im;
        localAmp := localAmp + Sqrt(Sqr(reX)+Sqr(imX)+Sqr(reY)+Sqr(imY)+Sqr(reZ)+Sqr(imZ));
      end;
      localAmp := localAmp*(1.0 + 0.05*freqVec.Values[i]);
      magVec.Values[i] := localAmp;
    end;

    serOMA.Clear;
    for i := 0 to freqVec.Length-1 do
      serOMA.AddXY(freqVec.Values[i], magVec.Values[i]);
  finally
    freqVec.Free;
    magVec.Free;
  end;

  GuessUnattachedModeShapes;
  lblAnalyzeDone.Visible := True;
  tmrHideDone.Enabled := False;
  tmrHideDone.Enabled := True;
end;

procedure TfrmMain.btnAttachOMAClick(Sender: TObject);
var
  rows: TArray<Integer>;
  i: Integer;
begin
  rows := GetSelectedRows(grdCoordinates);
  if Length(rows)=0 then
  begin
    ShowMessage('No rows selected in Coordinates grid to attach.');
    Exit;
  end;
  for i := 0 to High(rows) do
    FOMAAttached[ rows[i] ] := True;

  lblAttachDone.Visible := True;
  tmrHideDone.Enabled := False;
  tmrHideDone.Enabled := True;
end;

procedure TfrmMain.btnDetachOMAClick(Sender: TObject);
var
  rows: TArray<Integer>;
  i: Integer;
begin
  rows := GetSelectedRows(grdCoordinates);
  if Length(rows)=0 then
  begin
    ShowMessage('No rows selected in Coordinates grid to detach.');
    Exit;
  end;
  for i := 0 to High(rows) do
    FOMAAttached[ rows[i] ] := False;

  lblDetachDone.Visible := True;
  tmrHideDone.Enabled := False;
  tmrHideDone.Enabled := True;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.PopulateOMAUI;
var
  i,m: Integer;
begin
  if (FModeCount=0) or (FSelectedMode<0) or (FSelectedMode>=FModeCount) then Exit;

  m := FSelectedMode;
  edtFreq.Text := FloatToStrF(FFrequencies[m], ffGeneral, 6, 4);
  edtDamp.Text := FloatToStrF(FDampings[m], ffGeneral, 6, 4);

  grdModeData.RowCount := Length(FNodes)+1;
  for i := 0 to High(FNodes) do
  begin
    grdModeData.Cells[0, i+1] := IntToStr(i);
    grdModeData.Cells[1, i+1] := FloatToStrF(FModeShapes[m][i].X.Re, ffGeneral, 6, 4);
    grdModeData.Cells[2, i+1] := FloatToStrF(FModeShapes[m][i].X.Im, ffGeneral, 6, 4);
    grdModeData.Cells[3, i+1] := FloatToStrF(FModeShapes[m][i].Y.Re, ffGeneral, 6, 4);
    grdModeData.Cells[4, i+1] := FloatToStrF(FModeShapes[m][i].Y.Im, ffGeneral, 6, 4);
    grdModeData.Cells[5, i+1] := FloatToStrF(FModeShapes[m][i].Z.Re, ffGeneral, 6, 4);
    grdModeData.Cells[6, i+1] := FloatToStrF(FModeShapes[m][i].Z.Im, ffGeneral, 6, 4);
  end;
  AutoSizeGridColumns(grdModeData);
end;

procedure TfrmMain.ApplyOMAUI;
var
  i,m: Integer;
  val: Double;
begin
  if (FModeCount=0) or (FSelectedMode<0) or (FSelectedMode>=FModeCount) then Exit;

  m := FSelectedMode;
  if TryStrToFloat(edtFreq.Text, val) then
    FFrequencies[m] := val;
  if TryStrToFloat(edtDamp.Text, val) then
    FDampings[m] := val;

  for i := 0 to High(FNodes) do
  begin
    if TryStrToFloat(grdModeData.Cells[1,i+1], val) then
      FModeShapes[m][i].X.Re := val;
    if TryStrToFloat(grdModeData.Cells[2,i+1], val) then
      FModeShapes[m][i].X.Im := val;
    if TryStrToFloat(grdModeData.Cells[3,i+1], val) then
      FModeShapes[m][i].Y.Re := val;
    if TryStrToFloat(grdModeData.Cells[4,i+1], val) then
      FModeShapes[m][i].Y.Im := val;
    if TryStrToFloat(grdModeData.Cells[5,i+1], val) then
      FModeShapes[m][i].Z.Re := val;
    if TryStrToFloat(grdModeData.Cells[6,i+1], val) then
      FModeShapes[m][i].Z.Im := val;
  end;
end;

procedure TfrmMain.RevertOMAData;
var
  i,m: Integer;
begin
  if FModeCount=0 then Exit;
  for m := 0 to FModeCount-1 do
  begin
    FFrequencies[m] := FDefaultFreqs[m];
    FDampings[m] := FDefaultDamps[m];
    for i := 0 to High(FNodes) do
      FModeShapes[m][i] := FDefaultModeShapes[m][i];
  end;
  PopulateOMAUI;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.GuessUnattachedModeShapes;
var
  i: Integer;
begin
  if (FModeCount=0) or (Length(FNodes)=0) then Exit;
  for i := 0 to High(FNodes) do
    if not FOMAAttached[i] then
      BFSForUnattachedNode(i);
end;

procedure TfrmMain.BFSForUnattachedNode(UnIdx: Integer);
var
  queue: TQueue<Integer>;
  visited: array of Boolean;
  dist: array of Integer;
  cur,eIdx,neighbor,m: Integer;
  scaleFactor: Double;
begin
  if (UnIdx<0) or (UnIdx>High(FNodes)) then Exit;

  SetLength(visited, Length(FNodes));
  SetLength(dist, Length(FNodes));
  FillChar(visited[0], Length(FNodes)*SizeOf(Boolean),0);
  FillChar(dist[0],   Length(FNodes)*SizeOf(Integer),0);

  queue := TQueue<Integer>.Create;
  try
    visited[UnIdx] := True;
    dist[UnIdx] := 0;
    queue.Enqueue(UnIdx);

    while queue.Count>0 do
    begin
      cur := queue.Dequeue;
      if FOMAAttached[cur] then
      begin
        if dist[cur]>0 then
        begin
          scaleFactor := 1/(dist[cur]+1);
          for m := 0 to FModeCount-1 do
          begin
            FModeShapes[m][UnIdx].X.Re := scaleFactor*FModeShapes[m][cur].X.Re;
            FModeShapes[m][UnIdx].X.Im := scaleFactor*FModeShapes[m][cur].X.Im;
            FModeShapes[m][UnIdx].Y.Re := scaleFactor*FModeShapes[m][cur].Y.Re;
            FModeShapes[m][UnIdx].Y.Im := scaleFactor*FModeShapes[m][cur].Y.Im;
            FModeShapes[m][UnIdx].Z.Re := scaleFactor*FModeShapes[m][cur].Z.Re;
            FModeShapes[m][UnIdx].Z.Im := scaleFactor*FModeShapes[m][cur].Z.Im;
          end;
        end;
        Exit;
      end;

      for eIdx := 0 to High(FEdges) do
      begin
        if FEdges[eIdx].FromIndex=cur then
          neighbor := FEdges[eIdx].ToIndex
        else if FEdges[eIdx].ToIndex=cur then
          neighbor := FEdges[eIdx].FromIndex
        else
          continue;

        if not visited[neighbor] then
        begin
          visited[neighbor] := True;
          dist[neighbor] := dist[cur]+1;
          queue.Enqueue(neighbor);
        end;
      end;
    end;
  finally
    queue.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.grdCoordinatesMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SyncGridSelections(grdCoordinates, grdModeData);
end;

procedure TfrmMain.grdModeDataMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  SyncGridSelections(grdModeData, grdCoordinates);
end;

function TfrmMain.GetSelectedRows(Grid: TStringGrid): TArray<Integer>;
var
  row: Integer;
  list: TList<Integer>;
  r: TGridRect;
begin
  list := TList<Integer>.Create;
  try
    r := Grid.Selection;
    if (r.Top<=r.Bottom) and (r.Top>=0) then
    begin
      for row := r.Top to r.Bottom do
      begin
        if (row>0) and (row<=High(FNodes)+1) then
          list.Add(row-1);
      end;
    end;
    Result := list.ToArray;
  finally
    list.Free;
  end;
end;

procedure TfrmMain.SyncGridSelections(Source, Target: TStringGrid);
var
  rows: TArray<Integer>;
  topRow, bottomRow, i: Integer;
  newRect: TGridRect;
begin
  rows := GetSelectedRows(Source);
  if Length(rows)=0 then
  begin
    newRect.Left := -1;
    newRect.Top := -1;
    newRect.Right := -1;
    newRect.Bottom := -1;
    Target.Selection := newRect;
    Exit;
  end;

  topRow := rows[0];
  bottomRow := rows[0];
  for i := 1 to High(rows) do
  begin
    if rows[i]<topRow then topRow := rows[i];
    if rows[i]>bottomRow then bottomRow := rows[i];
  end;

  newRect.Left := 0;
  newRect.Top  := topRow+1;
  newRect.Right := Target.ColCount-1;
  newRect.Bottom := bottomRow+1;
  Target.Selection := newRect;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.tmrHideDoneTimer(Sender: TObject);
begin
  lblApplyDone.Visible := False;
  lblAnalyzeDone.Visible := False;
  lblAttachDone.Visible := False;
  lblDetachDone.Visible := False;
  tmrHideDone.Enabled := False;
end;

procedure TfrmMain.AutoSizeGridColumns(Grid: TStringGrid);
var
  c,r,best,w: Integer;
  txt: string;
  tempCanvas: TControlCanvas;
begin
  tempCanvas := TControlCanvas.Create;
  try
    tempCanvas.Control := Grid;
    for c := 0 to Grid.ColCount-1 do
    begin
      best := 30;
      for r := 0 to Grid.RowCount-1 do
      begin
        txt := Grid.Cells[c,r];
        w := tempCanvas.TextWidth(txt)+20;
        if w>best then
          best := w;
      end;
      Grid.ColWidths[c] := best;
    end;
  finally
    tempCanvas.Free;
  end;
end;

//------------------------------------------------------------------------------
procedure TfrmMain.SaveModelToFile(const AFileName: string);
var
  f: TextFile;
  i,m: Integer;
begin
  AssignFile(f,AFileName);
  Rewrite(f);
  try
    Writeln(f,'[NODES]');
    Writeln(f,'NodeCount=',Length(FNodes));
    for i := 0 to High(FNodes) do
      Writeln(f,Format('%d %g %g %g',[i,FNodes[i].X,FNodes[i].Y,FNodes[i].Z]));

    Writeln(f);
    Writeln(f,'[EDGES]');
    for i := 0 to High(FEdges) do
      Writeln(f,Format('%d %d',[FEdges[i].FromIndex,FEdges[i].ToIndex]));

    Writeln(f);
    Writeln(f,'[OMA]');
    Writeln(f,'ModeCount=',FModeCount);
    for m := 0 to FModeCount-1 do
    begin
      Writeln(f,Format('ModeIndex=%d Freq=%g Damp=%g',[m,FFrequencies[m],FDampings[m]]));
      for i := 0 to High(FNodes) do
      begin
        Writeln(f,Format('N%d  X.Re=%g  X.Im=%g  Y.Re=%g  Y.Im=%g  Z.Re=%g  Z.Im=%g',
                 [i,
                  FModeShapes[m][i].X.Re, FModeShapes[m][i].X.Im,
                  FModeShapes[m][i].Y.Re, FModeShapes[m][i].Y.Im,
                  FModeShapes[m][i].Z.Re, FModeShapes[m][i].Z.Im]));
      end;
      Writeln(f);
    end;

    Writeln(f,'[OMAAttached]');
    for i := 0 to High(FNodes) do
      Writeln(f,Format('Node=%d Attached=%s',[i,BoolToStr(FOMAAttached[i],True)]));
  finally
    CloseFile(f);
  end;
end;

procedure TfrmMain.LoadModelFromFile(const AFileName: string);
var
  sl: TStringList;
  i,nodeCount,nodeIdx,modeIndex: Integer;
  line,val: string;
  reX,imX,reY,imY,reZ,imZ,freq,damp: Double;
  parts: TArray<string>;
  section: string;
begin
  tmrAnimation.Enabled := False;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFileName);

    SetLength(FNodes,0);
    SetLength(FEdges,0);
    SetLength(FOMAAttached,0);
    SetLength(FModeShapes,0);
    SetLength(FFrequencies,0);
    SetLength(FDampings,0);
    FModeCount := 0;

    section := '';
    nodeCount := 0;
    modeIndex := 0;

    for i := 0 to sl.Count-1 do
    begin
      line := Trim(sl[i]);
      if line='' then Continue;
      if line[1]='[' then
      begin
        section := line;
        Continue;
      end;

      if SameText(section,'[NODES]') then
      begin
        if StartsText('NodeCount=',line) then
        begin
          nodeCount := StrToIntDef(Copy(line,Length('NodeCount=')+1,100),0);
          SetLength(FNodes,nodeCount);
          Continue;
        end
        else
        begin
          parts := line.Split([' ']);
          if Length(parts)=4 then
          begin
            nodeIdx := StrToIntDef(parts[0],0);
            if (nodeIdx>=0) and (nodeIdx<nodeCount) then
            begin
              FNodes[nodeIdx].X := StrToFloatDef(parts[1],0);
              FNodes[nodeIdx].Y := StrToFloatDef(parts[2],0);
              FNodes[nodeIdx].Z := StrToFloatDef(parts[3],0);
              FNodes[nodeIdx].OrigX := FNodes[nodeIdx].X;
              FNodes[nodeIdx].OrigY := FNodes[nodeIdx].Y;
              FNodes[nodeIdx].OrigZ := FNodes[nodeIdx].Z;
              FNodes[nodeIdx].AnimationAxis := aaNone;
              FNodes[nodeIdx].AnimationPhase := 0;
            end;
          end;
        end;
      end
      else if SameText(section,'[EDGES]') then
      begin
        parts := line.Split([' ']);
        if Length(parts)=2 then
        begin
          SetLength(FEdges,Length(FEdges)+1);
          FEdges[High(FEdges)].FromIndex := StrToIntDef(parts[0],0);
          FEdges[High(FEdges)].ToIndex   := StrToIntDef(parts[1],0);
        end;
      end
      else if SameText(section,'[OMA]') then
      begin
        if StartsText('ModeCount=',line) then
        begin
          FModeCount := StrToIntDef(Copy(line,Length('ModeCount=')+1,100),0);
          SetLength(FModeShapes,FModeCount);
          SetLength(FFrequencies,FModeCount);
          SetLength(FDampings,FModeCount);
          Continue;
        end
        else if StartsText('ModeIndex=',line) then
        begin
          parts := line.Split([' ']);
          freq := 0; damp := 0;
          if Length(parts)>=3 then
          begin
            modeIndex := StrToIntDef(Copy(parts[0],Pos('=',parts[0])+1,100),0);
            freq := StrToFloatDef(Copy(parts[1],Pos('=',parts[1])+1,100),1.0);
            damp := StrToFloatDef(Copy(parts[2],Pos('=',parts[2])+1,100),0.01);
          end;
          FFrequencies[modeIndex] := freq;
          FDampings[modeIndex] := damp;
          SetLength(FModeShapes[modeIndex],nodeCount);
        end
        else if StartsText('N',line) then
        begin
          parts := line.Split([' '], TStringSplitOptions.ExcludeEmpty);
          if Length(parts)>=7 then
          begin
            nodeIdx := StrToIntDef(Copy(parts[0],2,100),0);
            if (nodeIdx>=0) and (nodeIdx<nodeCount) then
            begin
              reX := StrToFloatDef(Copy(parts[1],Pos('=',parts[1])+1,100),0);
              imX := StrToFloatDef(Copy(parts[2],Pos('=',parts[2])+1,100),0);
              reY := StrToFloatDef(Copy(parts[3],Pos('=',parts[3])+1,100),0);
              imY := StrToFloatDef(Copy(parts[4],Pos('=',parts[4])+1,100),0);
              reZ := StrToFloatDef(Copy(parts[5],Pos('=',parts[5])+1,100),0);
              imZ := StrToFloatDef(Copy(parts[6],Pos('=',parts[6])+1,100),0);

              FModeShapes[modeIndex][nodeIdx].X.Re := reX;
              FModeShapes[modeIndex][nodeIdx].X.Im := imX;
              FModeShapes[modeIndex][nodeIdx].Y.Re := reY;
              FModeShapes[modeIndex][nodeIdx].Y.Im := imY;
              FModeShapes[modeIndex][nodeIdx].Z.Re := reZ;
              FModeShapes[modeIndex][nodeIdx].Z.Im := imZ;
            end;
          end;
        end;
      end
      else if SameText(section,'[OMAAttached]') then
      begin
        if StartsText('Node=',line) then
        begin
          parts := line.Split([' ']);
          if Length(parts)=2 then
          begin
            nodeIdx := StrToIntDef(Copy(parts[0],Pos('=',parts[0])+1,100),0);
            val := Copy(parts[1],Pos('=',parts[1])+1,100);
            if (nodeIdx>=0) and (nodeIdx<nodeCount) then
            begin
              FOMAAttached[nodeIdx] := SameText(val,'True');
            end;
          end;
        end;
      end;
    end;

    SetLength(FDefaultModeShapes,FModeCount);
    SetLength(FDefaultFreqs,FModeCount);
    SetLength(FDefaultDamps,FModeCount);
    for i := 0 to High(FNodes) do
      FNodes[i].AnimationPhase := 0;

    for modeIndex := 0 to FModeCount-1 do
    begin
      SetLength(FDefaultModeShapes[modeIndex],nodeCount);
      for i := 0 to nodeCount-1 do
        FDefaultModeShapes[modeIndex][i] := FModeShapes[modeIndex][i];
      FDefaultFreqs[modeIndex] := FFrequencies[modeIndex];
      FDefaultDamps[modeIndex] := FDampings[modeIndex];
    end;

    PopulateGridFromNodes;
    FSelectedMode := 0;
    PopulateOMAUI;
    PaintBox1.Invalidate;
    UpdateStatus;
  finally
    sl.Free;
  end;
end;

end.

