unit PointSplitter;

interface

type
  TPointSplitType = (pstInterval, pstDivideByUnit, pstCenterDot);

  TPointSplitter = class
  public
    SplitType: TPointSplitType;
    IsDiscrete: Boolean;

    // 1: step for interval
    // 2: division count by unit
    // 3: dot length for center point of lines
    Value: Single;

    // convert closed polyline
    // if line sizes are less than treshold
    TresholdOfPoint: Single;

    constructor Create;
  end;

implementation

{ TPointSplitter }

constructor TPointSplitter.Create;
begin
  IsDiscrete := false;
  TresholdOfPoint := 0;
  SplitType := pstInterval;
end;

end.
