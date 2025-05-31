unit VerticalPartSideRequest;

interface

uses AxisType, System.Generics.Collections, System.Classes, PointSplitter,
  CamLetter, TouchOffset, Mirror, Move, Crop, Stretch, MillingRequest;

type
  TVerticalPartSideRequestDetail = class
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
    RadiusOfInside: Single;
    RadiusOfOutside: Single;
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

  TVerticalPartSideRequest = class
  public
    History: TList<TVerticalPartSideRequestDetail>;
    Detail: TVerticalPartSideRequestDetail;
    CamLetters: TDictionary<TCamLetter, TCamLetterVariable>;
    HeaderOfProject, FooterOfProject: string;
    SuccessMessage, ErrorMessage: string;
    LengthOfTemplate: Single;
    XOfDeviation: Single;
    ZOfDeviation: Single;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TVerticalPartSideRequest }

constructor TVerticalPartSideRequestDetail.Create;
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
  RadiusOfInside := 0;
  RadiusOfOutside := 0;
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

destructor TVerticalPartSideRequestDetail.Destroy;
begin
  if Assigned(AStream) then
    AStream.Free;
  AStream := nil;

  inherited;
end;

{ TVerticalPartSideRequest }

constructor TVerticalPartSideRequest.Create;
begin
  Detail := TVerticalPartSideRequestDetail.Create;
  History := TList<TVerticalPartSideRequestDetail>.Create;
  CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
  HeaderOfProject := '';
  FooterOfProject := '';
  LengthOfTemplate := 0;
  XOfDeviation := 0;
  ZOfDeviation := 0;
  SuccessMessage := 'GCodes has been generated!';
  ErrorMessage := 'GCodes couldn''t be generated!';
end;

destructor TVerticalPartSideRequest.Destroy;
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
