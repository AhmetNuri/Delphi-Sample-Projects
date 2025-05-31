unit VerticalPartLineStoneSettingRequest;

interface

uses AxisType, System.Generics.Collections, System.Classes, PointSplitter,
  CamLetter, LineStoneSetting, TouchOffset, Mirror, Move, Crop, Stretch,
  MillingRequest;

type
  TVerticalPartLineStoneSettingRequestDetail = class
  public
    AStream: TStream;
    AFileName: string;
    TypeOfAxis: TAxisType;
    StoneSettingRule: TLineStoneSettingIn;
    PointSplitType: TPointSplitType;
    PointSplitValue: Single;
    StartXOfProjection: Single;
    EndXOfProjection: Single;
    HeaderOfProgram, FooterOfProgram: string;
    HeaderOfAxis, FooterOfAxis: string;
    MovementHeaderOfAxis, MovementFooterOfAxis: string;
    MillingHeaderOfAxis, MillingFooterOfAxis: string;
    HeaderOfTool, FooterOfTool: string;
    SpeedOfMovement: Single;
    SpeedOfMilling: Single;
    SpeedOfApproach: Single;
    LimitForGapOfTouchAngle: Single;
    SafetyDistance: Single;
    ApproachDistance: Single;
    AngleOfArmRef: Single;
    MinLimitOfTouchAngle: Single;
    MaxLimitOfTouchAngle: Single;
    IsContinuousAngle: Boolean;
    IsStrictDirection: Boolean;
    IsDiscrete: Boolean;
    IsReversedAngleOfTemplate: Boolean;
    OffsetOfTouchAngle: TTouchOffset;
    Mirror: TMirror;
    Move: TMove;
    Crop: TCrop;
    Stretch: TStretch;
    Milling: TMillingRequest;

    constructor Create;
    destructor Destroy; override;
  end;

  TVerticalPartLineStoneSettingRequest = class
  private
    FLengthOfTemplate: Single;
    procedure SetLengthOfTemplate(value: Single);
  public
    History: TList<TVerticalPartLineStoneSettingRequestDetail>;
    LineDetail: TVerticalPartLineStoneSettingRequestDetail;
    PointDetail: TVerticalPartLineStoneSettingRequestDetail;
    RadiusOfPart: Single;
    WidthOfPart: Single;
    RadiusOfProjection: Single;
    CamLetters: TDictionary<TCamLetter, TCamLetterVariable>;
    HeaderOfProject, FooterOfProject: string;
    SuccessMessage, ErrorMessage: string;
    XOfDeviation: Single;
    ZOfDeviation: Single;

    property LengthOfTemplate: Single read FLengthOfTemplate
      write SetLengthOfTemplate;

    constructor Create;
    destructor Destroy; override;

    procedure Prepare;
  end;

implementation

{ TVerticalPartLineStoneSettingRequest }

constructor TVerticalPartLineStoneSettingRequestDetail.Create;
begin
  StoneSettingRule := TLineStoneSettingIn.Create;
  TypeOfAxis := atSpindle;
  AStream := nil;
  AFileName := '';
  PointSplitType := pstDivideByUnit;
  PointSplitValue := 10;
  HeaderOfProgram := '';
  FooterOfProgram := '';
  HeaderOfAxis := '';
  FooterOfAxis := '';
  MovementHeaderOfAxis := '';
  MovementFooterOfAxis := '';
  MillingHeaderOfAxis := '';
  MillingFooterOfAxis := '';
  HeaderOfTool := '';
  FooterOfTool := '';
  SpeedOfMovement := 0;
  SpeedOfMilling := 0;
  SpeedOfApproach := 0;
  LimitForGapOfTouchAngle := 10;
  SafetyDistance := 1;
  ApproachDistance := 0.5;
  StartXOfProjection := 0;
  EndXOfProjection := 0;
  AngleOfArmRef := 90;
  MinLimitOfTouchAngle := 0;
  MaxLimitOfTouchAngle := 0;
  IsContinuousAngle := true;
  IsStrictDirection := true;
  IsDiscrete := false;
  IsReversedAngleOfTemplate := false;

  Milling.StepValue := 0;
  Milling.StepCount := 1;
  Milling.FinishValue := 0;

  OffsetOfTouchAngle.SwitchTypeToNone;

  Mirror.OnX := false;
  Mirror.OnY := false;
  Mirror.OnMixMax := false;

  Move.OnX := 0;
  Move.OnY := 0;
  Move.OnStrictArea := false;

  Crop.OnX.SetArea(0, 100);
  Crop.OnY.SetArea(0, 100);

  Stretch.SwitchModeToNone;
  Stretch.OnX.SetArea(0, 100);
  Stretch.OnY.SetArea(0, 100);
end;

destructor TVerticalPartLineStoneSettingRequestDetail.Destroy;
begin
  if Assigned(AStream) then
    AStream.Free;
  AStream := nil;

  StoneSettingRule := nil;

  inherited;
end;

{ TVerticalPartLineStoneSettingRequest }

constructor TVerticalPartLineStoneSettingRequest.Create;
begin
  LineDetail := TVerticalPartLineStoneSettingRequestDetail.Create;
  PointDetail := TVerticalPartLineStoneSettingRequestDetail.Create;
  History := TList<TVerticalPartLineStoneSettingRequestDetail>.Create;
  CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
  LineDetail.StoneSettingRule := TLineStoneSettingIn.Create;
  PointDetail.StoneSettingRule := TLineStoneSettingIn.Create;
  HeaderOfProject := '';
  FooterOfProject := '';
  RadiusOfPart := 0;
  WidthOfPart := 0;
  RadiusOfProjection := 0;
  XOfDeviation := 0;
  ZOfDeviation := 0;
  LengthOfTemplate := 0;
  SuccessMessage := 'GCodes has been generated!';
  ErrorMessage := 'GCodes couldn''t be generated!';
end;

destructor TVerticalPartLineStoneSettingRequest.Destroy;
begin
  if Assigned(CamLetters) then
    CamLetters.Free;
  CamLetters := nil;

  if Assigned(History) then
    History.Free;
  History := nil;

  if Assigned(LineDetail) then
    LineDetail.Free;
  LineDetail := nil;

  if Assigned(PointDetail) then
    PointDetail.Free;
  PointDetail := nil;

  inherited;
end;

procedure TVerticalPartLineStoneSettingRequest.SetLengthOfTemplate
  (value: Single);
begin
  FLengthOfTemplate := value;
  LineDetail.StoneSettingRule.LengthOfTemplate := value;
  PointDetail.StoneSettingRule.LengthOfTemplate := value;
end;

procedure TVerticalPartLineStoneSettingRequest.Prepare;
begin
  LineDetail.StoneSettingRule.LengthOfTemplate := FLengthOfTemplate;
  PointDetail.StoneSettingRule.LengthOfTemplate := FLengthOfTemplate;
end;

end.
