unit UnitVerticalPartLineStoneSettingGCodeGenerator;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.Dialogs, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Memo.Types, FMX.ScrollBox, Generics.Collections, FMX.Memo,
  FMX.ListBox, FMX.Layouts, VerticalPartLineStoneSettingRequest;

type
  TformVerticalPartLineStoneSettingGCodeGenerator = class(TForm)
    Label1: TLabel;
    edtRadiusOfPart: TNumberBox;
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
    Label10: TLabel;
    edtAngleOfArm: TNumberBox;
    Label11: TLabel;
    edtLengthOfTemplate: TNumberBox;
    btnGenerate: TButton;
    Label12: TLabel;
    edtSize: TNumberBox;
    edtXCount: TNumberBox;
    Label13: TLabel;
    edtYCount: TNumberBox;
    Label14: TLabel;
    Label16: TLabel;
    edtYDistance: TNumberBox;
    Label17: TLabel;
    radioCountByDistance: TRadioButton;
    radioDistanceByCount: TRadioButton;
    Label15: TLabel;
    edtWidthOfPart: TNumberBox;
    Label18: TLabel;
    edtMillingStepValue: TNumberBox;
    Label19: TLabel;
    edtMillingStepCount: TNumberBox;
    Label20: TLabel;
    edtMillingFinish: TNumberBox;
    procedure btnGenerateClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    request: TVerticalPartLineStoneSettingRequest;

    procedure Generate(model: TVerticalPartLineStoneSettingRequest);
  public
    { Public declarations }
  end;

var
  formVerticalPartLineStoneSettingGCodeGenerator
    : TformVerticalPartLineStoneSettingGCodeGenerator;

implementation

{$R *.fmx}

uses CadHelpers, ShapeType, CadModel, CadService, DxfManager, Point2D, Point3D,
  Point6D, VerticalPartRadius, VerticalPartRadiusModels, CadShapes,
  LineStoneSetting, PointSplitter, MinMax2D, ShapeAnchor, LengthOfTemplate,
  CamService, CamManager, CamModel, CamLetter, CamType, CamPair, CamBlock,
  AxisSpindle, AxisType, AxisC, AxisU, AxisLaser, AxisFixed, AxisHighLathe,
  AxisLowLathe, Math, Move, Mirror;

procedure TformVerticalPartLineStoneSettingGCodeGenerator.FormClose
  (Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(request) then
    request.Free;
  request := nil;
end;

procedure TformVerticalPartLineStoneSettingGCodeGenerator.Generate
  (model: TVerticalPartLineStoneSettingRequest);
var
  splitter: TPointSplitter;
  cadSrv: ICadService;
  camSrv: ICamService;
  ALineStream, APointStream: TStream;
  I, J, indexOfPoints, shapeIndex: Integer;
  instance: TVerticalPartRadius;
  cadInput: TVerticalRadiusAxisIn;
  cadOutput: TVerticalRadiusAxisOut;
  LineStoneSettingOut: TLineStoneSettingOut;
  camInput: TCamModel;
  block: TCamBlock;
  points: TList<TPoint3D>;
  validIndexList: TList<integer>;
begin
  if not Assigned(model) then
    exit;

  model.Prepare;

  instance := TVerticalPartRadius.Create;
  cadSrv := TDxfManager.Create;
  camSrv := TCamManager.Create;

  if not Assigned(model.History) then
    model.History := TList<TVerticalPartLineStoneSettingRequestDetail>.Create;

  validIndexList := TList<integer>.Create;

  I := 0;
  while I <= model.History.Count do
  begin
    if I < model.History.Count then
    begin
      cadSrv.LoadFromStream(model.History[I].AStream);
      splitter := TPointSplitter.Create;
      splitter.SplitType := model.History[I].PointSplitType;
      splitter.Value := model.History[I].PointSplitValue;
      splitter.IsDiscrete := model.History[I].IsDiscrete;

      if cadSrv.Data.Shapes.Count > 0 then
      begin
        validIndexList.Add(I);
        instance.AddPoints(cadSrv.Data.ToFlattenPoints(splitter).Data);
      end;
    end
    else
    begin
      splitter := TPointSplitter.Create;
      splitter.SplitType := model.LineDetail.PointSplitType;
      splitter.Value := model.LineDetail.PointSplitValue;
      splitter.IsDiscrete := model.LineDetail.IsDiscrete;
      LineStoneSettingOut := cadSrv.CreateLineStoneSetting
        (model.LineDetail.StoneSettingRule);
      ALineStream := LineStoneSettingOut.ALineStream;

      if LineStoneSettingOut.Lines.Count > 0 then
      begin
        cadSrv.ClearData;
        for shapeIndex := 0 to LineStoneSettingOut.Lines.Count - 1 do
        begin
          cadSrv.Data.AddShape(LineStoneSettingOut.Lines[shapeIndex]);
        end;
      end;

      if cadSrv.Data.Shapes.Count > 0 then
      begin
        validIndexList.Add(I);
        instance.AddPoints(cadSrv.Data.ToFlattenPoints(splitter).Data);

        model.LineDetail.AStream := ALineStream;
        model.LineDetail.AFileName := '';
        model.History.Add(model.LineDetail);
        inc(I);
      end;

      splitter.SplitType := model.PointDetail.PointSplitType;
      splitter.Value := model.PointDetail.PointSplitValue;
      splitter.IsDiscrete := model.PointDetail.IsDiscrete;
      LineStoneSettingOut := cadSrv.CreateLineStoneSetting
        (model.PointDetail.StoneSettingRule);
      APointStream := LineStoneSettingOut.APointStream;

      if LineStoneSettingOut.points.Count > 0 then
      begin
        cadSrv.ClearData;
        for shapeIndex := 0 to LineStoneSettingOut.points.Count - 1 do
        begin
          cadSrv.Data.AddShape(LineStoneSettingOut.points[shapeIndex]);
        end;
      end;

      if cadSrv.Data.Shapes.Count > 0 then
      begin
        validIndexList.Add(I);
        instance.AddPoints(cadSrv.Data.ToFlattenPoints(splitter).Data);

        model.PointDetail.AStream := APointStream;
        model.PointDetail.AFileName := '';
        model.History.Add(model.PointDetail);
      end;

      break;
    end;

    inc(I);
    FreeAndNil(splitter);
    FreeAndNil(LineStoneSettingOut);
  end;

  camInput := TCamModel.Create;

  for I := 0 to model.CamLetters.Keys.Count - 1 do
  begin
    camInput.SetLetter(model.CamLetters.ToArray[I].Key,
      model.CamLetters.ToArray[I].Value);
  end;

  camInput.SwitchCamType(ctVPA);
  camInput.AddOrUpdateProperty(Keys.Header, model.HeaderOfProject);
  camInput.AddOrUpdateProperty(Keys.Footer, model.FooterOfProject);
  camInput.AddOrUpdateProperty(Keys.RadiusOfPart, model.RadiusOfPart);
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

    cadInput := TVerticalRadiusAxisIn.Create;
    cadInput.StartXOfProjectionRadius := model.History[I].StartXOfProjection;
    cadInput.EndXOfProjectionRadius := model.History[I].EndXOfProjection;
    cadInput.RadiusOfPart := model.RadiusOfPart;
    cadInput.WidthOfPart := model.WidthOfPart;
    cadInput.RadiusOfProjection := model.RadiusOfProjection;
    cadInput.XOfDeviation := model.XOfDeviation;
    cadInput.ZOfDeviation := model.ZOfDeviation;
    cadInput.AngleOfArmRef := model.History[I].AngleOfArmRef;
    cadInput.LengthOfTemplate := TLengthOfTemplate.Create
      (model.LengthOfTemplate);
    cadInput.MinLimitOfTouchAngle := model.History[I].MinLimitOfTouchAngle;
    cadInput.MaxLimitOfTouchAngle := model.History[I].MaxLimitOfTouchAngle;
    cadInput.ValidateAngleOfTouch := model.History[I].TypeOfAxis in [atC, atU];
    cadInput.IsContinuousAngle := model.History[I].IsContinuousAngle;
    cadInput.IsStrictDirection := model.History[I].IsStrictDirection;
    cadInput.IsReversedAngleOfTemplate := model.History[I]
      .IsReversedAngleOfTemplate;

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

procedure TformVerticalPartLineStoneSettingGCodeGenerator.btnGenerateClick
  (Sender: TObject);
begin
  if not Assigned(request) then
    request := TVerticalPartLineStoneSettingRequest.Create;

  request.HeaderOfProject := '(header of project)';
  request.FooterOfProject := '(footer of project)';
  request.RadiusOfPart := edtRadiusOfPart.Value;
  request.WidthOfPart := edtWidthOfPart.Value;
  request.RadiusOfProjection := edtRadiusOfProjection.Value;
  request.XOfDeviation := edtDeviationX.Value;
  request.ZOfDeviation := edtDeviationZ.Value;
  request.LengthOfTemplate := edtLengthOfTemplate.Value;

  request.CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
  request.CamLetters.Add(clXOfCnc, TCamLetterLinearVariable.Create('X', 1));
  request.CamLetters.Add(clYOfCnc, TCamLetterLinearVariable.Create('Y', 1));
  request.CamLetters.Add(clZOfCnc, TCamLetterLinearVariable.Create('Z', 1));
  request.CamLetters.Add(clDivisor, TCamLetterCircularVariable.Create('A', 1));
  request.CamLetters.Add(clArm, TCamLetterCircularVariable.Create('B', 1));
  request.CamLetters.Add(clAxisC, TCamLetterCircularVariable.Create('C', 1));
  request.CamLetters.Add(clAxisU, TCamLetterCircularVariable.Create('U', 1));
    request.CamLetters.Add(clF,
      TCamLetterLinearVariable.Create('F', 1));

  // line details are managable by request
  request.LineDetail := TVerticalPartLineStoneSettingRequestDetail.Create;
  request.LineDetail.TypeOfAxis := atSpindle;
  request.LineDetail.PointSplitType := pstDivideByUnit;
  request.LineDetail.PointSplitValue := edtPointCountInMM.Value;
  request.LineDetail.HeaderOfProgram := '(header of program)';
  request.LineDetail.FooterOfProgram := '(footer of program)';
  request.LineDetail.HeaderOfAxis := '(header of axis)';
  request.LineDetail.FooterOfAxis := '(footer of axis)';
  request.LineDetail.MovementHeaderOfAxis := '(movement header of axis)';
  request.LineDetail.MovementFooterOfAxis := '(movement footer of axis)';
  request.LineDetail.MillingHeaderOfAxis := '(milling header of axis)';
  request.LineDetail.MillingFooterOfAxis := '(milling footer of axis)';
  request.LineDetail.HeaderOfTool := '(header of tool)';
  request.LineDetail.FooterOfTool := '(footer of tool)';
  request.LineDetail.SpeedOfMovement := 10000;
  request.LineDetail.SpeedOfMilling := 2000;
  request.LineDetail.SpeedOfApproach := 5000;
  request.LineDetail.LimitForGapOfTouchAngle := 5;
  request.LineDetail.SafetyDistance := 1;
  request.LineDetail.ApproachDistance := 0.5;
  request.LineDetail.StartXOfProjection := edtStartX.Value;
  request.LineDetail.EndXOfProjection := edtEndX.Value;
  request.LineDetail.AngleOfArmRef := 90;
  request.LineDetail.MinLimitOfTouchAngle := -120;
  request.LineDetail.MaxLimitOfTouchAngle := 120;
  request.LineDetail.IsContinuousAngle := false;
  request.LineDetail.IsStrictDirection := true;
  request.LineDetail.IsDiscrete := false;
  request.LineDetail.IsReversedAngleOfTemplate := false;
  request.LineDetail.StoneSettingRule.Size := edtSize.Value;
  request.LineDetail.StoneSettingRule.XCount := Floor(edtXCount.Value);
  request.LineDetail.StoneSettingRule.YCount := Floor(edtYCount.Value);
  request.LineDetail.StoneSettingRule.YDistance := edtYDistance.Value;

  if radioCountByDistance.IsChecked then
    request.LineDetail.StoneSettingRule.RecurrenceMode := lsrCountByDistance
  else
    request.LineDetail.StoneSettingRule.RecurrenceMode := lsrDistanceByCount;

  request.LineDetail.Milling.StepValue := edtMillingStepValue.Value;
  request.LineDetail.Milling.StepCount := Trunc(edtMillingStepCount.Value);
  request.LineDetail.Milling.FinishValue := edtMillingFinish.Value;

  // request.LineDetail.Crop.OnX.SetArea(25, 75);
  // request.LineDetail.Crop.OnY.SetArea(50, 100);

  // request.LineDetail.Stretch.OnX.SetArea(25, 75);
  // request.LineDetail.Stretch.OnY.SetArea(50, 100);

  // request.LineDetail.OffsetOfTouchAngle.SwitchTypeToStatic;
  // request.LineDetail.OffsetOfTouchAngle.OffsetValue := 45;

  // request.LineDetail.Mirror.OnX := true;
  // request.LineDetail.Mirror.OnY := true;
  // request.LineDetail.Mirror.OnMixMax := false;

  // request.LineDetail.Move.OnX := 0.5;
  // request.LineDetail.Move.OnY := 0.5;
  // request.LineDetail.Move.OnStrictArea := true;

  // point details are managable by request
  request.PointDetail := TVerticalPartLineStoneSettingRequestDetail.Create;
  request.PointDetail.TypeOfAxis := atLaser;
  request.PointDetail.PointSplitType := pstDivideByUnit;
  request.PointDetail.PointSplitValue := edtPointCountInMM.Value;
  request.PointDetail.HeaderOfProgram := '(header of program)';
  request.PointDetail.FooterOfProgram := '(footer of program)';
  request.PointDetail.HeaderOfAxis := '(header of axis)';
  request.PointDetail.FooterOfAxis := '(footer of axis)';
  request.PointDetail.MovementHeaderOfAxis := '(movement header of axis)';
  request.PointDetail.MovementFooterOfAxis := '(movement footer of axis)';
  request.PointDetail.MillingHeaderOfAxis := '(milling header of axis)';
  request.PointDetail.MillingFooterOfAxis := '(milling footer of axis)';
  request.PointDetail.HeaderOfTool := '(header of tool)';
  request.PointDetail.FooterOfTool := '(footer of tool)';
  request.PointDetail.SpeedOfMovement := 10000;
  request.PointDetail.SpeedOfMilling := 2000;
  request.PointDetail.SpeedOfApproach := 5000;
  request.PointDetail.LimitForGapOfTouchAngle := 5;
  request.PointDetail.SafetyDistance := 1;
  request.PointDetail.ApproachDistance := 0.5;
  request.PointDetail.StartXOfProjection := edtStartX.Value;
  request.PointDetail.EndXOfProjection := edtEndX.Value;
  request.PointDetail.AngleOfArmRef := 90;
  request.PointDetail.MinLimitOfTouchAngle := -120;
  request.PointDetail.MaxLimitOfTouchAngle := 120;
  request.PointDetail.IsContinuousAngle := false;
  request.PointDetail.IsStrictDirection := true;
  request.PointDetail.IsDiscrete := false;
  request.PointDetail.IsReversedAngleOfTemplate := false;
  request.PointDetail.StoneSettingRule.Size := edtSize.Value;
  request.PointDetail.StoneSettingRule.XCount := Floor(edtXCount.Value);
  request.PointDetail.StoneSettingRule.YCount := Floor(edtYCount.Value);
  request.PointDetail.StoneSettingRule.YDistance := edtYDistance.Value;

  if radioCountByDistance.IsChecked then
    request.PointDetail.StoneSettingRule.RecurrenceMode := lsrCountByDistance
  else
    request.PointDetail.StoneSettingRule.RecurrenceMode := lsrDistanceByCount;

  request.PointDetail.Milling.StepValue := edtMillingStepValue.Value;
  request.PointDetail.Milling.StepCount := Trunc(edtMillingStepCount.Value);
  request.PointDetail.Milling.FinishValue := edtMillingFinish.Value;

  // request.PointDetail.Crop.OnX.SetArea(25, 75);
  // request.PointDetail.Crop.OnY.SetArea(50, 100);

  // request.PointDetail.Stretch.OnX.SetArea(25, 75);
  // request.PointDetail.Stretch.OnY.SetArea(50, 100);

  // request.PointDetail.OffsetOfTouchAngle.SwitchTypeToStatic;
  // request.PointDetail.OffsetOfTouchAngle.OffsetValue := 45;

  // request.PointDetail.Mirror.OnX := true;
  // request.PointDetail.Mirror.OnY := true;
  // request.PointDetail.Mirror.OnMixMax := false;

  // request.PointDetail.Move.OnX := 0.5;
  // request.PointDetail.Move.OnY := 0.5;
  // request.PointDetail.Move.OnStrictArea := true;

  Generate(request);
end;

end.
