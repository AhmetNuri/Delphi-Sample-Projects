unit UnitHorizontalPartCircularGCodeGenerator;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, Generics.Collections, FMX.Memo,
  FMX.ListBox, FMX.Layouts, HorizontalPartCircularRequest;

type
  TformHorizontalPartCircularGCodeGenerator = class(TForm)
    Label2: TLabel;
    edtRadiusOfProjection: TNumberBox;
    Label3: TLabel;
    edtStartX: TNumberBox;
    Label4: TLabel;
    edtEndX: TNumberBox;
    Label5: TLabel;
    edtMinAngleOfTouch: TNumberBox;
    Label6: TLabel;
    edtMaxAngleOfTouch: TNumberBox;
    edtPointCountInMM: TNumberBox;
    Label7: TLabel;
    Label8: TLabel;
    edtDeviationX: TNumberBox;
    Label9: TLabel;
    edtDeviationZ: TNumberBox;
    Label11: TLabel;
    edtLengthOfTemplate: TNumberBox;
    btnSelectFile: TButton;
    odDXF: TOpenDialog;
    Label13: TLabel;
    edtMillingStepValue: TNumberBox;
    Label14: TLabel;
    edtMillingStepCount: TNumberBox;
    Label15: TLabel;
    edtMillingFinish: TNumberBox;
    procedure btnSelectFileClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    request: THorizontalPartCircularRequest;

    procedure Generate(model: THorizontalPartCircularRequest);
  public
    { Public declarations }
  end;

var
  formHorizontalPartCircularGCodeGenerator
    : TformHorizontalPartCircularGCodeGenerator;
  HistoryOfDxfStream: TList<TStream>;

implementation

{$R *.fmx}

uses CadHelpers, ShapeType, CadModel, CadService, DxfManager, Point2D, Point3D,
  Point6D, HorizontalPartCircular, HorizontalPartCircularModels, CadShapes,
  Move, Mirror, PointSplitter, MinMax2D, ShapeAnchor, LengthOfTemplate,
  CamService, CamManager, CamModel, CamLetter, CamType, CamPair, CamBlock,
  AxisSpindle, AxisType, AxisC, AxisU, AxisLaser, AxisFixed, AxisHighLathe,
  AxisLowLathe;

procedure TformHorizontalPartCircularGCodeGenerator.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if Assigned(request) then
    request.Free;
  request := nil;
end;

procedure TformHorizontalPartCircularGCodeGenerator.Generate
  (model: THorizontalPartCircularRequest);
var
  splitter: TPointSplitter;
  cadSrv: ICadService;
  camSrv: ICamService;
  AStream: TStream;
  I, J, indexOfPoints: Integer;
  instance: THorizontalPartCircular;
  cadInput: THorizontalCircularAxisIn;
  cadOutput: THorizontalCircularAxisOut;
  camInput: TCamModel;
  block: TCamBlock;
  points: TList<TPoint3D>;
  validIndexList: TList<integer>;
begin
  if not Assigned(model) then
    exit;

  instance := THorizontalPartCircular.Create;
  cadSrv := TDxfManager.Create;
  camSrv := TCamManager.Create;

  if not Assigned(model.History) then
    model.History := TList<THorizontalPartCircularRequestDetail>.Create;

  validIndexList := TList<integer>.Create;

  for I := 0 to model.History.Count do
  begin
    if I < model.History.Count then
    begin
      cadSrv.LoadFromStream(model.History[I].AStream);
      splitter := TPointSplitter.Create;
      splitter.SplitType := model.History[I].PointSplitType;
      splitter.Value := model.History[I].PointSplitValue;
      splitter.IsDiscrete := model.History[I].IsDiscrete;
    end
    else if FileExists(model.Detail.AFileName) then
    begin
      AStream := cadSrv.LoadFromFile(model.Detail.AFileName);
      splitter := TPointSplitter.Create;
      splitter.SplitType := model.Detail.PointSplitType;
      splitter.Value := model.Detail.PointSplitValue;
      splitter.IsDiscrete := model.Detail.IsDiscrete;
    end;

    cadSrv.Data.Shapes := TCadShapes(cadSrv.Data.Shapes).SortForAllTypeNearest;

    if cadSrv.Data.Shapes.Count > 0 then
    begin
      validIndexList.Add(I);
      instance.AddPoints(cadSrv.Data.ToFlattenPoints(splitter).Data);

      if (I = model.History.Count) and FileExists(model.Detail.AFileName) then
      begin
        model.Detail.AStream := AStream;
        model.Detail.AFileName := '';
        model.History.Add(model.Detail);
      end;
    end;

    FreeAndNil(splitter);
  end;

  camInput := TCamModel.Create;

  for I := 0 to model.CamLetters.Keys.Count - 1 do
  begin
    camInput.SetLetter(model.CamLetters.ToArray[I].Key,
      model.CamLetters.ToArray[I].Value);
  end;

  camInput.SwitchCamType(ctHPFR);
  camInput.AddOrUpdateProperty(Keys.Header, model.HeaderOfProject);
  camInput.AddOrUpdateProperty(Keys.Footer, model.FooterOfProject);
  camInput.AddOrUpdateProperty(Keys.RadiusOfPart, 0);
  camInput.AddOrUpdateProperty(Keys.RadiusOfProjection,
    model.RadiusOfProjection);

  indexOfPoints := 0;
  for I := 0 to model.History.Count - 1 do
  begin
    if not validIndexList.Contains(I) then
      continue;

    camInput.AddOrUpdateProperty(Keys.SpeedOfMovement,
      model.History[I].SpeedOfMovement);
    camInput.AddOrUpdateProperty(Keys.SpeedOfMilling,
      model.History[I].SpeedOfMilling);
    camInput.AddOrUpdateProperty(Keys.SpeedOfApproach,
      model.History[I].SpeedOfApproach);
    camInput.AddOrUpdateProperty(Keys.LimitForGapOfTouchAngle,
      model.History[I].LimitForGapOfTouchAngle);
    camInput.AddOrUpdateProperty(Keys.SafetyDistance,
      model.History[I].SafetyDistance);
    camInput.AddOrUpdateProperty(Keys.ApproachDistance,
      model.History[I].ApproachDistance);

    cadInput := THorizontalCircularAxisIn.Create;
    cadInput.StartXOfProjectionRadius := model.History[I].StartXOfProjection;
    cadInput.EndXOfProjectionRadius := model.History[I].EndXOfProjection;
    cadInput.RadiusOfProjection := model.RadiusOfProjection;
    cadInput.XOfDeviation := model.XOfDeviation;
    cadInput.ZOfDeviation := model.ZOfDeviation;
    cadInput.LengthOfTemplate := TLengthOfTemplate.Create
      (model.LengthOfTemplate);
    cadInput.MinLimitOfTouchAngle := model.History[I].MinLimitOfTouchAngle;
    cadInput.MaxLimitOfTouchAngle := model.History[I].MaxLimitOfTouchAngle;
    cadInput.ValidateAngleOfTouch := model.History[I].TypeOfAxis in [atC, atU];
    cadInput.IsStrictDirection := model.History[I].IsStrictDirection;

    cadInput.OffsetOfTouchAngle := model.History[I].OffsetOfTouchAngle;
    cadInput.Mirror := model.History[I].Mirror;
    cadInput.Move := model.History[I].Move;
    cadInput.Crop := model.History[I].Crop;
    cadInput.Stretch := model.History[I].Stretch;

    cadInput.Assign(instance.GetPoints[indexOfPoints]);
    cadOutput := instance.GenerateForAxis(cadInput);

    block := TCamBlock.Create;

    if model.History[I].TypeOfAxis = atSpindle then
      block.SetAxis(TAxisSpindle.Create)
    else if model.History[I].TypeOfAxis = atC then
      block.SetAxis(TAxisC.Create)
    else if model.History[I].TypeOfAxis = atU then
      block.SetAxis(TAxisU.Create)
    else if model.History[I].TypeOfAxis = atLaser then
      block.SetAxis(TAxisLaser.Create)
    else if model.History[I].TypeOfAxis = atFixed then
      block.SetAxis(TAxisFixed.Create)
    else if model.History[I].TypeOfAxis = atHighLathe then
      block.SetAxis(TAxisHighLathe.Create)
    else if model.History[I].TypeOfAxis = atLowLathe then
      block.SetAxis(TAxisLowLathe.Create);

    block.SetPoints(cadOutput.points);

    block.AddOrUpdateProperty(Keys.Header, model.History[I].HeaderOfProgram);
    block.AddOrUpdateProperty(Keys.Footer, model.History[I].FooterOfProgram);

    block.Axis.AddOrUpdateProperty(Keys.Header, model.History[I].HeaderOfAxis);
    block.Axis.AddOrUpdateProperty(Keys.Footer, model.History[I].FooterOfAxis);

    block.Axis.AddOrUpdateProperty(Keys.MovementHeader,
      model.History[I].MovementHeaderOfAxis);
    block.Axis.AddOrUpdateProperty(Keys.MovementFooter,
      model.History[I].MovementFooterOfAxis);

    block.Axis.AddOrUpdateProperty(Keys.MillingHeader,
      model.History[I].MillingHeaderOfAxis);
    block.Axis.AddOrUpdateProperty(Keys.MillingFooter,
      model.History[I].MillingFooterOfAxis);

    block.Axis.SetMilling(request.History[I].Milling);

    block.Axis.Tool.AddOrUpdateProperty(Keys.Header,
      model.History[I].HeaderOfTool);
    block.Axis.Tool.AddOrUpdateProperty(Keys.Footer,
      model.History[I].FooterOfTool);

    camInput.AddBlock(block);

    inc(indexOfPoints);
  end;

  if camSrv.GenerateCode(camInput) and (validIndexList.Count > 0) then
    ShowMessage(model.SuccessMessage)
  else
    ShowMessage(model.ErrorMessage);

  FreeAndNil(cadInput);
  FreeAndNil(camInput);
  FreeAndNil(cadOutput);
  FreeAndNil(instance);
end;

procedure TformHorizontalPartCircularGCodeGenerator.btnSelectFileClick
  (Sender: TObject);
begin
  if odDXF.Execute then
  begin
    if not Assigned(request) then
      request := THorizontalPartCircularRequest.Create;

    request.HeaderOfProject := '(header of project)';
    request.FooterOfProject := '(footer of project)';
    request.RadiusOfProjection := edtRadiusOfProjection.Value;
    request.XOfDeviation := edtDeviationX.Value;
    request.ZOfDeviation := edtDeviationZ.Value;
    request.LengthOfTemplate := edtLengthOfTemplate.Value;

    request.CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
    request.CamLetters.Add(clXOfCnc, TCamLetterLinearVariable.Create('X', 1));
    request.CamLetters.Add(clYOfCnc, TCamLetterLinearVariable.Create('Y', 1));
    request.CamLetters.Add(clZOfCnc, TCamLetterLinearVariable.Create('Z', 1));
    request.CamLetters.Add(clDivisor,
      TCamLetterCircularVariable.Create('A', 1));
    request.CamLetters.Add(clArm, TCamLetterCircularVariable.Create('B', 1));
    request.CamLetters.Add(clAxisC, TCamLetterCircularVariable.Create('C', 1));
    request.CamLetters.Add(clAxisU, TCamLetterCircularVariable.Create('U', 1));
    request.CamLetters.Add(clF,
      TCamLetterLinearVariable.Create('F', 1));

    // details are managable by request
    request.Detail := THorizontalPartCircularRequestDetail.Create;
    request.Detail.AFileName := odDXF.FileName;
    request.Detail.TypeOfAxis := atSpindle;
    request.Detail.PointSplitType := pstDivideByUnit;
    request.Detail.PointSplitValue := edtPointCountInMM.Value;
    request.Detail.HeaderOfProgram := '(header of program)';
    request.Detail.FooterOfProgram := '(footer of program)';
    request.Detail.HeaderOfAxis := '(header of axis)';
    request.Detail.FooterOfAxis := '(footer of axis)';
    request.Detail.MovementHeaderOfAxis := '(movement header of axis)';
    request.Detail.MovementFooterOfAxis := '(movement footer of axis)';
    request.Detail.MillingHeaderOfAxis := '(milling header of axis)';
    request.Detail.MillingFooterOfAxis := '(milling footer of axis)';
    request.Detail.HeaderOfTool := '(header of tool)';
    request.Detail.FooterOfTool := '(footer of tool)';
    request.Detail.SpeedOfMovement := 10000;
    request.Detail.SpeedOfMilling := 2000;
    request.Detail.SpeedOfApproach := 5000;
    request.Detail.LimitForGapOfTouchAngle := 5;
    request.Detail.SafetyDistance := 1;
    request.Detail.ApproachDistance := 0.5;
    request.Detail.StartXOfProjection := edtStartX.Value;
    request.Detail.EndXOfProjection := edtEndX.Value;
    request.Detail.MinLimitOfTouchAngle := -120;
    request.Detail.MaxLimitOfTouchAngle := 120;
    request.Detail.IsContinuousAngle := true;
    request.Detail.IsStrictDirection := true;
    request.Detail.IsDiscrete := false;

    request.Detail.Milling.StepValue := edtMillingStepValue.Value;
    request.Detail.Milling.StepCount := Trunc(edtMillingStepCount.Value);
    request.Detail.Milling.FinishValue := edtMillingFinish.Value;

    // request.Detail.Crop.OnX.SetArea(25, 75);
    // request.Detail.Crop.OnY.SetArea(50, 100);

    // request.Detail.Stretch.OnX.SetArea(25, 75);
    // request.Detail.Stretch.OnY.SetArea(50, 100);

    // request.Detail.OffsetOfTouchAngle.SwitchTypeToStatic;
    // request.Detail.OffsetOfTouchAngle.OffsetValue := 45;

    // request.Detail.Mirror.OnX := true;
    // request.Detail.Mirror.OnY := true;
    // request.Detail.Mirror.OnMixMax := false;

    // request.Detail.Move.OnX := 0.5;
    // request.Detail.Move.OnY := 0.5;
    // request.Detail.Move.OnStrictArea := true;

    Generate(request);
  end;
end;

end.
