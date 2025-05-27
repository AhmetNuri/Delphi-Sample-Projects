unit VerticalPartRequest;

interface

uses AxisType, System.Generics.Collections, System.Classes, PointSplitter,
  CamLetter, TouchOffset, Mirror, Move, Crop, Stretch, MillingRequest;

type
  TVerticalPartRequestDetail = class
  public
    AStream: TStream;
    AFileName: string;
    TypeOfAxis: TAxisType;
    PointSplitType: TPointSplitType;
    PointSplitValue: Single;
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
    StartXOfProjection: Single;
    EndXOfProjection: Single;
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

  TVerticalPartRequest = class
  public
    History: TList<TVerticalPartRequestDetail>;
    Detail: TVerticalPartRequestDetail;
    WidthOfPart: Single;
    RadiusOfPart: Single;
    RadiusOfProjection: Single;
    CamLetters: TDictionary<TCamLetter, TCamLetterVariable>;
    HeaderOfProject, FooterOfProject: string;
    SuccessMessage, ErrorMessage: string;
    XOfDeviation: Single;
    ZOfDeviation: Single;
    LengthOfTemplate: Single;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TVerticalPartRequest }

constructor TVerticalPartRequestDetail.Create;
begin
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

destructor TVerticalPartRequestDetail.Destroy;
begin
  if Assigned(AStream) then
    AStream.Free;
  AStream := nil;

  inherited;
end;

{ TVerticalPartRequest }

constructor TVerticalPartRequest.Create;
begin
  Detail := TVerticalPartRequestDetail.Create;
  History := TList<TVerticalPartRequestDetail>.Create;
  CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
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

destructor TVerticalPartRequest.Destroy;
begin
  if Assigned(CamLetters) then
    CamLetters.Free;
  CamLetters := nil;

  if Assigned(History) then
    History.Free;
  History := nil;

  if Assigned(Detail) then
    Detail.Free;
  Detail := nil;

  inherited;
end;

end.
