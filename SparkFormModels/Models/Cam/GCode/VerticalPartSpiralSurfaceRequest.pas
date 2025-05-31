unit VerticalPartSpiralSurfaceRequest;

interface

uses AxisType, System.Generics.Collections, System.Classes, PointSplitter,
  CamLetter, SpiralSurface, TouchOffset, Mirror, Move, Crop, Stretch,
  MillingRequest;

type
  TVerticalPartSpiralSurfaceRequestDetail = class
  private
    FStartXOfProjection: Single;
    FEndXOfProjection: Single;
    procedure SetStartXOfProjection(value: Single);
    procedure SetEndXOfProjection(value: Single);
  public
    AStream: TStream;
    AFileName: string;
    TypeOfAxis: TAxisType;
    SurfaceRule: TSpiralSurfaceIn;
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

    property StartXOfProjection: Single read FStartXOfProjection
      write SetStartXOfProjection;
    property EndXOfProjection: Single read FEndXOfProjection
      write SetEndXOfProjection;

    constructor Create;
    destructor Destroy; override;
  end;

  TVerticalPartSpiralSurfaceRequest = class
  private
    FLengthOfTemplate: Single;
    procedure SetLengthOfTemplate(value: Single);
  public
    History: TList<TVerticalPartSpiralSurfaceRequestDetail>;
    Detail: TVerticalPartSpiralSurfaceRequestDetail;
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

{ TVerticalPartSpiralSurfaceRequest }

constructor TVerticalPartSpiralSurfaceRequestDetail.Create;
begin
  SurfaceRule := TSpiralSurfaceIn.Create;
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

destructor TVerticalPartSpiralSurfaceRequestDetail.Destroy;
begin
  if Assigned(AStream) then
    AStream.Free;
  AStream := nil;

  SurfaceRule := nil;

  inherited;
end;

procedure TVerticalPartSpiralSurfaceRequestDetail.SetStartXOfProjection
  (value: Single);
begin
  FStartXOfProjection := value;
  SurfaceRule.StartX := value;
end;

procedure TVerticalPartSpiralSurfaceRequestDetail.SetEndXOfProjection
  (value: Single);
begin
  FEndXOfProjection := value;
  SurfaceRule.EndX := value;
end;

{ TVerticalPartSpiralSurfaceRequest }

constructor TVerticalPartSpiralSurfaceRequest.Create;
begin
  Detail := TVerticalPartSpiralSurfaceRequestDetail.Create;
  History := TList<TVerticalPartSpiralSurfaceRequestDetail>.Create;
  CamLetters := TDictionary<TCamLetter, TCamLetterVariable>.Create;
  Detail.SurfaceRule := TSpiralSurfaceIn.Create;
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

destructor TVerticalPartSpiralSurfaceRequest.Destroy;
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

procedure TVerticalPartSpiralSurfaceRequest.SetLengthOfTemplate(value: Single);
begin
  FLengthOfTemplate := value;
  Detail.SurfaceRule.LengthOfTemplate := value;
end;

procedure TVerticalPartSpiralSurfaceRequest.Prepare;
begin
  Detail.SurfaceRule.LengthOfTemplate := FLengthOfTemplate;
  Detail.SurfaceRule.StartX := Detail.StartXOfProjection;
  Detail.SurfaceRule.EndX := Detail.EndXOfProjection;
end;

end.
