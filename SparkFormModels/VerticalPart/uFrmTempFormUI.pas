unit uFrmTempFormUI;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.ComboEdit,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.Memo.Types, Skia,  FMX.Skia,
  System.Generics.Collections, VerticalPartRequest, AxisType, PointSplitter,
  CamLetter, TouchOffset, Mirror, Move, Crop, Stretch, MillingRequest, StretchMode,
  FMX.Colors, FMX.ComboTrackBar , TouchOffsetType
  ,  // Generate metodu için gerekli ek uses
  CadHelpers, ShapeType, CadModel, CadService, DxfManager, Point2D, Point3D,
  Point6D, VerticalPartRadius, VerticalPartRadiusModels, CadShapes,
  MinMax2D, ShapeAnchor, LengthOfTemplate, CamService,
  CamManager, CamModel, CamType, CamPair, CamBlock, AxisSpindle,
  AxisC, AxisU, AxisLaser, AxisFixed, AxisHighLathe, AxisLowLathe
   , uModelFormTemplate;

const
  // SVG Icons
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
  SVG_GEAR = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.35 19.43,11.03L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.22,8.95 2.27,9.22 2.46,9.37L4.57,11.03C4.53,11.35 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.22,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.68 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z"/></svg>';
  SVG_SPEED = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,16A3,3 0 0,1 9,13C9,11.88 9.61,10.9 10.5,10.39L20.21,4.77L14.68,14.35C14.18,15.33 13.17,16 12,16M12,3C13.81,3 15.5,3.5 16.97,4.32L14.87,6.4C14.04,6.05 13.06,5.85 12,5.85C8.43,5.85 5.5,8.78 5.5,12.35C5.5,15.92 8.43,18.85 12,18.85C14.21,18.85 16.15,17.61 17.18,15.8L19.28,17.9C17.6,20.03 15,21.35 12,21.35C7.03,21.35 3,17.32 3,12.35C3,7.38 7.03,3.35 12,3.35L12,3Z"/></svg>';
  SVG_PROCESS = '<svg viewBox="0 0 24 24" fill="white"><path d="M17,12C17,14.42 15.28,16.44 13,16.9V21H11V16.9C8.72,16.44 7,14.42 7,12C7,9.58 8.72,7.56 11,7.1V3H13V7.1C15.28,7.56 17,9.58 17,12M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9Z"/></svg>';
  SVG_GEOMETRY = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,4A8,8 0 0,1 20,12A8,8 0 0,1 12,20A8,8 0 0,1 4,12A8,8 0 0,1 12,4Z"/></svg>';
  SVG_ANGLE = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 2C6.48 2 2 6.48 2 12S6.48 22 12 22 22 17.52 22 12 17.52 2 12 2M12 20C7.59 20 4 16.41 4 12S7.59 4 12 4 20 7.59 20 12 16.41 20 12 20M16.59 7.58L10 14.17L7.41 11.59L6 13L10 17L18 9L16.59 7.58Z"/></svg>';
  SVG_COLOR = '<svg viewBox="0 0 24 24" fill="white"><path d="M17.5,12A1.5,1.5 0 0,1 16,10.5A1.5,1.5 0 0,1 17.5,9A1.5,1.5 0 0,1 19,10.5A1.5,1.5 0 0,1 17.5,12M14.5,8A1.5,1.5 0 0,1 13,6.5A1.5,1.5 0 0,1 14.5,5A1.5,1.5 0 0,1 16,6.5A1.5,1.5 0 0,1 14.5,8M9.5,8A1.5,1.5 0 0,1 8,6.5A1.5,1.5 0 0,1 9.5,5A1.5,1.5 0 0,1 11,6.5A1.5,1.5 0 0,1 9.5,8M6.5,12A1.5,1.5 0 0,1 5,10.5A1.5,1.5 0 0,1 6.5,9A1.5,1.5 0 0,1 8,10.5A1.5,1.5 0 0,1 6.5,12Z"/></svg>';
  SVG_HELP = '<svg viewBox="0 0 24 24" fill="white"><path d="M11,18H13V16H11V18M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,6A4,4 0 0,0 8,10H10A2,2 0 0,1 12,8A2,2 0 0,1 14,10C14,12 11,11.75 11,15H13C13,12.75 16,12.5 16,10A4,4 0 0,0 12,6Z"/></svg>';
  SVG_STRETCH = '<svg viewBox="0 0 24 24" fill="white"><path d="M16,6H18V9H20V4A2,2 0 0,0 18,2H13V4H16V6M18,16V18H16V20H18A2,2 0 0,0 20,18V13H18V16M8,18H6V16H4V18A2,2 0 0,0 6,20H8V18M6,8V6H8V4H6A2,2 0 0,0 4,6V8H6M12,10A2,2 0 0,1 14,12A2,2 0 0,1 12,14A2,2 0 0,1 10,12A2,2 0 0,1 12,10Z"/></svg>';
  SVG_CROP = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,17V1H5V5H1V7H5V17A2,2 0 0,0 7,19H17V23H19V19H23V17H7M7,17V7H17V17H7Z"/></svg>';
  SVG_MOVE = '<svg viewBox="0 0 24 24" fill="white"><path d="M13,20H11V8L5.5,13.5L4.08,12.08L12,4.16L19.92,12.08L18.5,13.5L13,8V20Z"/></svg>';


type
  TFrmTempFormUI = class(TTempFormModel)
    lstParameters: TListBox;
     // Eksen Parametreleri Grubu
    lItemAxisHeader: TListBoxItem;
    recGrupEksenParametreleri: TRectangle;
    recGrupEksenParametreleriSvg: TRectangle;
    svgEksenParametreleriIcon: TSkSvg;
    lblGrupEksenParametreleri: TSkLabel;
    recGrupEksenParametreleriToggle: TRectangle;
    svgEksenParametreleriArrow: TSkSvg;

    lItemTypeOfAxis: TListBoxItem;
    recTypeOfAxis: TRectangle;
    layoutTypeOfAxisLabel: TLayout;
    lblTypeOfAxis: TSkLabel;
    recTypeOfAxisInfo: TRectangle;
    svgHelpTypeOfAxis: TSkSvg;
    recTypeOfAxisControl: TRectangle;
    cbxTypeOfAxis: TComboBox;

    lItemPointSplitType: TListBoxItem;
    recPointSplitType: TRectangle;
    layoutPointSplitTypeLabel: TLayout;
    lblPointSplitType: TSkLabel;
    recPointSplitTypeInfo: TRectangle;
    svgHelpPointSplitType: TSkSvg;
    recPointSplitTypeControl: TRectangle;
    cbxPointSplitType: TComboBox;

    lItemPointSplitValue: TListBoxItem;
    recPointSplitValue: TRectangle;
    layoutPointSplitValueLabel: TLayout;
    lblPointSplitValue: TSkLabel;
    recPointSplitValueInfo: TRectangle;
    svgHelpPointSplitValue: TSkSvg;
    recPointSplitValueControl: TRectangle;
    numPointSplitValue: TNumberBox;

    lItemAngleOfArmRef: TListBoxItem;
    recAngleOfArmRef: TRectangle;
    layoutAngleOfArmRefLabel: TLayout;
    lblAngleOfArmRef: TSkLabel;
    recAngleOfArmRefInfo: TRectangle;
    svgHelpAngleOfArmRef: TSkSvg;
    recAngleOfArmRefControl: TRectangle;
    numAngleOfArmRef: TNumberBox;

    // Paso-Kesme Parametreleri Grubu
    lItemStepHeader: TListBoxItem;
    recGrupPasoKesmeParametreleri: TRectangle;
    recGrupPasoKesmeParametreleriSvg: TRectangle;
    svgPasoKesmeParametreleriIcon: TSkSvg;
    lblGrupPasoKesmeParametreleri: TSkLabel;
    recGrupPasoKesmeParametreleriToggle: TRectangle;
    svgPasoKesmeParametreleriArrow: TSkSvg;

    lItemMillingStepValue: TListBoxItem;
    recMillingStepValue: TRectangle;
    layoutMillingStepValueLabel: TLayout;
    lblMillingStepValue: TSkLabel;
    recMillingStepValueInfo: TRectangle;
    svgHelpMillingStepValue: TSkSvg;
    recMillingStepValueControl: TRectangle;
    numMillingStepValue: TNumberBox;

    lItemMillingStepCount: TListBoxItem;
    recMillingStepCount: TRectangle;
    layoutMillingStepCountLabel: TLayout;
    lblMillingStepCount: TSkLabel;
    recMillingStepCountInfo: TRectangle;
    svgHelpMillingStepCount: TSkSvg;
    recMillingStepCountControl: TRectangle;
    numMillingStepCount: TNumberBox;

    lItemMillingFinish: TListBoxItem;
    recMillingFinish: TRectangle;
    layoutMillingFinishLabel: TLayout;
    lblMillingFinish: TSkLabel;
    recMillingFinishInfo: TRectangle;
    svgHelpMillingFinish: TSkSvg;
    recMillingFinishControl: TRectangle;
    numMillingFinish: TNumberBox;

    // Hýz Parametreleri Grubu
    lItemSpeedHeader: TListBoxItem;
    recGrupHizParametreleri: TRectangle;
    recGrupHizParametreleriSvg: TRectangle;
    svgHizParametreleriIcon: TSkSvg;
    lblGrupHizParametreleri: TSkLabel;
    recGrupHizParametreleriToggle: TRectangle;
    svgHizParametreleriArrow: TSkSvg;

    lItemSpeedOfMovement: TListBoxItem;
    recSpeedOfMovement: TRectangle;
    layoutSpeedOfMovementLabel: TLayout;
    lblSpeedOfMovement: TSkLabel;
    recSpeedOfMovementInfo: TRectangle;
    svgHelpSpeedOfMovement: TSkSvg;
    recSpeedOfMovementControl: TRectangle;
    numSpeedOfMovement: TNumberBox;

    lItemSpeedOfMilling: TListBoxItem;
    recSpeedOfMilling: TRectangle;
    layoutSpeedOfMillingLabel: TLayout;
    lblSpeedOfMilling: TSkLabel;
    recSpeedOfMillingInfo: TRectangle;
    svgHelpSpeedOfMilling: TSkSvg;
    recSpeedOfMillingControl: TRectangle;
    numSpeedOfMilling: TNumberBox;

    lItemSpeedOfApproach: TListBoxItem;
    recSpeedOfApproach: TRectangle;
    layoutSpeedOfApproachLabel: TLayout;
    lblSpeedOfApproach: TSkLabel;
    recSpeedOfApproachInfo: TRectangle;
    svgHelpSpeedOfApproach: TSkSvg;
    recSpeedOfApproachControl: TRectangle;
    numSpeedOfApproach: TNumberBox;

    lItemSafetyDistance: TListBoxItem;
    recSafetyDistance: TRectangle;
    layoutSafetyDistanceLabel: TLayout;
    lblSafetyDistance: TSkLabel;
    recSafetyDistanceInfo: TRectangle;
    svgHelpSafetyDistance: TSkSvg;
    recSafetyDistanceControl: TRectangle;
    numSafetyDistance: TNumberBox;

    lItemApproachDistance: TListBoxItem;
    recApproachDistance: TRectangle;
    layoutApproachDistanceLabel: TLayout;
    lblApproachDistance: TSkLabel;
    recApproachDistanceInfo: TRectangle;
    svgHelpApproachDistance: TSkSvg;
    recApproachDistanceControl: TRectangle;
    numApproachDistance: TNumberBox;

    // Ýþlem Parametreleri Grubu
    lItemProcessHeader: TListBoxItem;
    recGrupIslemParametreleri: TRectangle;
    recGrupIslemParametreleriSvg: TRectangle;
    svgIslemParametreleriIcon: TSkSvg;
    lblGrupIslemParametreleri: TSkLabel;
    recGrupIslemParametreleriToggle: TRectangle;
    svgIslemParametreleriArrow: TSkSvg;

    lItemAFileName: TListBoxItem;
    recAFileName: TRectangle;
    layoutAFileNameLabel: TLayout;
    lblAFileName: TSkLabel;
    recAFileNameInfo: TRectangle;
    svgHelpAFileName: TSkSvg;
    recAFileNameControl: TRectangle;
    edtAFileName: TEdit;
    btnSelectAFileName: TButton;

    lItemHeaderOfProgram: TListBoxItem;
    recHeaderOfProgram: TRectangle;
    layoutHeaderOfProgramLabel: TLayout;
    lblHeaderOfProgram: TSkLabel;
    recHeaderOfProgramInfo: TRectangle;
    svgHelpHeaderOfProgram: TSkSvg;
    recHeaderOfProgramControl: TRectangle;
    edtHeaderOfProgram: TEdit;

    lItemFooterOfProgram: TListBoxItem;
    recFooterOfProgram: TRectangle;
    layoutFooterOfProgramLabel: TLayout;
    lblFooterOfProgram: TSkLabel;
    recFooterOfProgramInfo: TRectangle;
    svgHelpFooterOfProgram: TSkSvg;
    recFooterOfProgramControl: TRectangle;
    edtFooterOfProgram: TEdit;

    lItemHeaderOfAxis: TListBoxItem;
    recHeaderOfAxis: TRectangle;
    layoutHeaderOfAxisLabel: TLayout;
    lblHeaderOfAxis: TSkLabel;
    recHeaderOfAxisInfo: TRectangle;
    svgHelpHeaderOfAxis: TSkSvg;
    recHeaderOfAxisControl: TRectangle;
    edtHeaderOfAxis: TEdit;

    lItemFooterOfAxis: TListBoxItem;
    recFooterOfAxis: TRectangle;
    layoutFooterOfAxisLabel: TLayout;
    lblFooterOfAxis: TSkLabel;
    recFooterOfAxisInfo: TRectangle;
    svgHelpFooterOfAxis: TSkSvg;
    recFooterOfAxisControl: TRectangle;
    edtFooterOfAxis: TEdit;

    lItemMovementHeaderOfAxis: TListBoxItem;
    recMovementHeaderOfAxis: TRectangle;
    layoutMovementHeaderOfAxisLabel: TLayout;
    lblMovementHeaderOfAxis: TSkLabel;
    recMovementHeaderOfAxisInfo: TRectangle;
    svgHelpMovementHeaderOfAxis: TSkSvg;
    recMovementHeaderOfAxisControl: TRectangle;
    edtMovementHeaderOfAxis: TEdit;

    lItemMovementFooterOfAxis: TListBoxItem;
    recMovementFooterOfAxis: TRectangle;
    layoutMovementFooterOfAxisLabel: TLayout;
    lblMovementFooterOfAxis: TSkLabel;
    recMovementFooterOfAxisInfo: TRectangle;
    svgHelpMovementFooterOfAxis: TSkSvg;
    recMovementFooterOfAxisControl: TRectangle;
    edtMovementFooterOfAxis: TEdit;

    // YENÝ EKLENEN: Milling Header ve Footer
    lItemMillingHeaderOfAxis: TListBoxItem;
    recMillingHeaderOfAxis: TRectangle;
    layoutMillingHeaderOfAxisLabel: TLayout;
    lblMillingHeaderOfAxis: TSkLabel;
    recMillingHeaderOfAxisInfo: TRectangle;
    svgHelpMillingHeaderOfAxis: TSkSvg;
    recMillingHeaderOfAxisControl: TRectangle;
    edtMillingHeaderOfAxis: TEdit;

    lItemMillingFooterOfAxis: TListBoxItem;
    recMillingFooterOfAxis: TRectangle;
    layoutMillingFooterOfAxisLabel: TLayout;
    lblMillingFooterOfAxis: TSkLabel;
    recMillingFooterOfAxisInfo: TRectangle;
    svgHelpMillingFooterOfAxis: TSkSvg;
    recMillingFooterOfAxisControl: TRectangle;
    edtMillingFooterOfAxis: TEdit;

    // YENÝ EKLENEN: Tool Header ve Footer
    lItemHeaderOfTool: TListBoxItem;
    recHeaderOfTool: TRectangle;
    layoutHeaderOfToolLabel: TLayout;
    lblHeaderOfTool: TSkLabel;
    recHeaderOfToolInfo: TRectangle;
    svgHelpHeaderOfTool: TSkSvg;
    recHeaderOfToolControl: TRectangle;
    edtHeaderOfTool: TEdit;

    lItemFooterOfTool: TListBoxItem;
    recFooterOfTool: TRectangle;
    layoutFooterOfToolLabel: TLayout;
    lblFooterOfTool: TSkLabel;
    recFooterOfToolInfo: TRectangle;
    svgHelpFooterOfTool: TSkSvg;
    recFooterOfToolControl: TRectangle;
    edtFooterOfTool: TEdit;

    // Geometri Parametreleri Grubu
    lItemGeometryHeader: TListBoxItem;
    recGrupGeometriParametreleri: TRectangle;
    recGrupGeometriParametreleriSvg: TRectangle;
    svgGeometriParametreleriIcon: TSkSvg;
    lblGrupGeometriParametreleri: TSkLabel;
    recGrupGeometriParametreleriToggle: TRectangle;
    svgGeometriParametreleriArrow: TSkSvg;

    lItemRadiusOfPart: TListBoxItem;
    recRadiusOfPart: TRectangle;
    layoutRadiusOfPartLabel: TLayout;
    lblRadiusOfPart: TSkLabel;
    recRadiusOfPartInfo: TRectangle;
    svgHelpRadiusOfPart: TSkSvg;
    recRadiusOfPartControl: TRectangle;
    numRadiusOfPart: TNumberBox;

    lItemRadiusOfProjection: TListBoxItem;
    recRadiusOfProjection: TRectangle;
    layoutRadiusOfProjectionLabel: TLayout;
    lblRadiusOfProjection: TSkLabel;
    recRadiusOfProjectionInfo: TRectangle;
    svgHelpRadiusOfProjection: TSkSvg;
    recRadiusOfProjectionControl: TRectangle;
    numRadiusOfProjection: TNumberBox;

    lItemWidthOfPart: TListBoxItem;
    recWidthOfPart: TRectangle;
    layoutWidthOfPartLabel: TLayout;
    lblWidthOfPart: TSkLabel;
    recWidthOfPartInfo: TRectangle;
    svgHelpWidthOfPart: TSkSvg;
    recWidthOfPartControl: TRectangle;
    numWidthOfPart: TNumberBox;

    lItemStartXOfProjection: TListBoxItem;
    recStartXOfProjection: TRectangle;
    layoutStartXOfProjectionLabel: TLayout;
    lblStartXOfProjection: TSkLabel;
    recStartXOfProjectionInfo: TRectangle;
    svgHelpStartXOfProjection: TSkSvg;
    recStartXOfProjectionControl: TRectangle;
    numStartXOfProjection: TNumberBox;

    lItemEndXOfProjection: TListBoxItem;
    recEndXOfProjection: TRectangle;
    layoutEndXOfProjectionLabel: TLayout;
    lblEndXOfProjection: TSkLabel;
    recEndXOfProjectionInfo: TRectangle;
    svgHelpEndXOfProjection: TSkSvg;
    recEndXOfProjectionControl: TRectangle;
    numEndXOfProjection: TNumberBox;

    lItemXOfDeviation: TListBoxItem;
    recXOfDeviation: TRectangle;
    layoutXOfDeviationLabel: TLayout;
    lblXOfDeviation: TSkLabel;
    recXOfDeviationInfo: TRectangle;
    svgHelpXOfDeviation: TSkSvg;
    recXOfDeviationControl: TRectangle;
    numXOfDeviation: TNumberBox;

    lItemZOfDeviation: TListBoxItem;
    recZOfDeviation: TRectangle;
    layoutZOfDeviationLabel: TLayout;
    lblZOfDeviation: TSkLabel;
    recZOfDeviationInfo: TRectangle;
    svgHelpZOfDeviation: TSkSvg;
    recZOfDeviationControl: TRectangle;
    numZOfDeviation: TNumberBox;

    lItemLengthOfTemplate: TListBoxItem;
    recLengthOfTemplate: TRectangle;
    layoutLengthOfTemplateLabel: TLayout;
    lblLengthOfTemplate: TSkLabel;
    recLengthOfTemplateInfo: TRectangle;
    svgHelpLengthOfTemplate: TSkSvg;
    recLengthOfTemplateControl: TRectangle;
    numLengthOfTemplate: TNumberBox;

    // Açý Parametreleri Grubu
    lItemAngleHeader: TListBoxItem;
    recGrupAciParametreleri: TRectangle;
    recGrupAciParametreleriSvg: TRectangle;
    svgAciParametreleriIcon: TSkSvg;
    lblGrupAciParametreleri: TSkLabel;
    recGrupAciParametreleriToggle: TRectangle;
    svgAciParametreleriArrow: TSkSvg;

    lItemMinLimitOfTouchAngle: TListBoxItem;
    recMinLimitOfTouchAngle: TRectangle;
    layoutMinLimitOfTouchAngleLabel: TLayout;
    lblMinLimitOfTouchAngle: TSkLabel;
    recMinLimitOfTouchAngleInfo: TRectangle;
    svgHelpMinLimitOfTouchAngle: TSkSvg;
    recMinLimitOfTouchAngleControl: TRectangle;
    numMinLimitOfTouchAngle: TNumberBox;

    lItemMaxLimitOfTouchAngle: TListBoxItem;
    recMaxLimitOfTouchAngle: TRectangle;
    layoutMaxLimitOfTouchAngleLabel: TLayout;
    lblMaxLimitOfTouchAngle: TSkLabel;
    recMaxLimitOfTouchAngleInfo: TRectangle;
    svgHelpMaxLimitOfTouchAngle: TSkSvg;
    recMaxLimitOfTouchAngleControl: TRectangle;
    numMaxLimitOfTouchAngle: TNumberBox;

    lItemLimitForGapOfTouchAngle: TListBoxItem;
    recLimitForGapOfTouchAngle: TRectangle;
    layoutLimitForGapOfTouchAngleLabel: TLayout;
    lblLimitForGapOfTouchAngle: TSkLabel;
    recLimitForGapOfTouchAngleInfo: TRectangle;
    svgHelpLimitForGapOfTouchAngle: TSkSvg;
    recLimitForGapOfTouchAngleControl: TRectangle;
    numLimitForGapOfTouchAngle: TNumberBox;

    // Renk Parametreleri Grubu
    lItemColorHeader: TListBoxItem;
    recGrupRenkParametreleri: TRectangle;
    recGrupRenkParametreleriSvg: TRectangle;
    svgRenkParametreleriIcon: TSkSvg;
    lblGrupRenkParametreleri: TSkLabel;
    recGrupRenkParametreleriToggle: TRectangle;
    svgRenkParametreleriArrow: TSkSvg;

    lItemColorOfPart: TListBoxItem;
    recColorOfPart: TRectangle;
    layoutColorOfPartLabel: TLayout;
    lblColorOfPart: TSkLabel;
    recColorOfPartInfo: TRectangle;
    svgHelpColorOfPart: TSkSvg;
    recColorOfPartControl: TRectangle;
    edtColorOfPart: TColorComboBox;

    lItemColorOfProjection: TListBoxItem;
    recColorOfProjection: TRectangle;
    layoutColorOfProjectionLabel: TLayout;
    lblColorOfProjection: TSkLabel;
    recColorOfProjectionInfo: TRectangle;
    svgHelpColorOfProjection: TSkSvg;
    recColorOfProjectionControl: TRectangle;
    edtColorOfProjection: TColorComboBox;

    // Takým Seçimi
    lItemToolSelect: TListBoxItem;
    recToolSelect: TRectangle;
    layoutToolSelectLabel: TLayout;
    lblToolSelect: TSkLabel;
    recToolSelectInfo: TRectangle;
    svgHelpToolSelect: TSkSvg;
    recToolSelectControl: TRectangle;
    cbxToolSelect: TComboBox;

    // CU Axis Follow Type
    lItemCUAxisFollowType: TListBoxItem;
    recCUAxisFollowType: TRectangle;
    layoutCUAxisFollowTypeLabel: TLayout;
    lblCUAxisFollowType: TSkLabel;
    recCUAxisFollowTypeInfo: TRectangle;
    svgHelpCUAxisFollowType: TSkSvg;
    recCUAxisFollowTypeControl: TRectangle;

    // Stretch Modes
    lItemStretchModes: TListBoxItem;
    recStretchModes: TRectangle;
    layoutStretchModesLabel: TLayout;
    lblStretchModes: TSkLabel;
    recStretchModesInfo: TRectangle;
    svgHelpStretchModes: TSkSvg;
    recStretchModesControl: TRectangle;
    rbtStretch_SwitchModeToTemplate: TRadioButton;
    rbtStretch_SwitchModeToNone: TRadioButton;
    rbtStretch_SwitchModeToMinMax: TRadioButton;

    // Touch Angle Types
    lItemTouchAngleTypes: TListBoxItem;
    recTouchAngleTypes: TRectangle;
    layoutTouchAngleTypesLabel: TLayout;
    lblTouchAngleTypes: TSkLabel;
    recTouchAngleTypesInfo: TRectangle;
    svgHelpTouchAngleTypes: TSkSvg;
    recTouchAngleTypesControl: TRectangle;
    rbtTouchAngle_SwitchTypeToNone: TRadioButton;
    rbtTouchAngle_SwitchTypeToStatic: TRadioButton;
    rbtTouchAngle_SwitchTypeToDynamic: TRadioButton;

    // Touch Angle Offset
    lItemTouchAngleOffset: TListBoxItem;
    recTouchAngleOffset: TRectangle;
    layoutTouchAngleOffsetLabel: TLayout;
    lblTouchAngleOffset: TSkLabel;
    recTouchAngleOffsetInfo: TRectangle;
    svgHelpTouchAngleOffset: TSkSvg;
    recTouchAngleOffsetControl: TRectangle;
    numTouchAngleOffset: TNumberBox;

    // Stretch Areas
    lItemStretchAreaX: TListBoxItem;
    recStretchAreaX: TRectangle;
    layoutStretchAreaXLabel: TLayout;
    lblStretchAreaX: TSkLabel;
    recStretchAreaXInfo: TRectangle;
    svgHelpStretchAreaX: TSkSvg;
    recStretchAreaXControl: TRectangle;
    ctbStretch_OnX_SetAreaFrom: TComboTrackBar;
    ctbStretch_OnX_SetAreaTo: TComboTrackBar;

    lItemStretchAreaY: TListBoxItem;
    recStretchAreaY: TRectangle;
    layoutStretchAreaYLabel: TLayout;
    lblStretchAreaY: TSkLabel;
    recStretchAreaYInfo: TRectangle;
    svgHelpStretchAreaY: TSkSvg;
    recStretchAreaYControl: TRectangle;
    ctbStretch_OnY_SetAreaFrom: TComboTrackBar;
    ctbStretch_OnY_SetAreaTo: TComboTrackBar;

    // Crop Areas
    lItemCropAreaX: TListBoxItem;
    recCropAreaX: TRectangle;
    layoutCropAreaXLabel: TLayout;
    lblCropAreaX: TSkLabel;
    recCropAreaXInfo: TRectangle;
    svgHelpCropAreaX: TSkSvg;
    recCropAreaXControl: TRectangle;
    ctbCrop_OnX_SetAreaFrom: TComboTrackBar;
    ctbCrop_OnX_SetAreaTo: TComboTrackBar;

    lItemCropAreaY: TListBoxItem;
    recCropAreaY: TRectangle;
    layoutCropAreaYLabel: TLayout;
    lblCropAreaY: TSkLabel;
    recCropAreaYInfo: TRectangle;
    svgHelpCropAreaY: TSkSvg;
    recCropAreaYControl: TRectangle;
    ctbCrop_OnY_SetAreaFrom: TComboTrackBar;
    ctbCrop_OnY_SetAreaTo: TComboTrackBar;

    // Move Controls
    lItemMoveControls: TListBoxItem;
    recMoveControls: TRectangle;
    layoutMoveControlsLabel: TLayout;
    lblMoveControls: TSkLabel;
    recMoveControlsInfo: TRectangle;
    svgHelpMoveControls: TSkSvg;
    recMoveControlsControl: TRectangle;
    ctbMoveOnX: TComboTrackBar;
    ctbMoveOnY: TComboTrackBar;
    cbxMove_OnStrictArea: TCheckBox;

    // Mirror Controls
    lItemMirrorControls: TListBoxItem;
    recMirrorControls: TRectangle;
    layoutMirrorControlsLabel: TLayout;
    lblMirrorControls: TSkLabel;
    recMirrorControlsInfo: TRectangle;
    svgHelpMirrorControls: TSkSvg;
    recMirrorControlsControl: TRectangle;
    cbxMirrorOnX: TCheckBox;
    cbxMirrorOnY: TCheckBox;
    cbxMirrorOnMixMax: TCheckBox;

    // Group Headers (existing ones)
    lItemStrechHeader: TListBoxItem;
    recGrupStretchParametreleri: TRectangle;
    recGrupStretchParametreleriSvg: TRectangle;
    svgStretchParametreleriIcon: TSkSvg;
    lblGrupStretchParametreleri: TSkLabel;
    recGrupStretchParametreleriToggle: TRectangle;
    svgStretchParametreleriArrow: TSkSvg;

    lItemCropHeader: TListBoxItem;
    recGrupCropParametreleri: TRectangle;
    recGrupCropParametreleriSvg: TRectangle;
    svgCropParametreleriIcon: TSkSvg;
    lblGrupCropParametreleri: TSkLabel;
    recGrupCropParametreleriToggle: TRectangle;
    svgCropParametreleriArrow: TSkSvg;

    lItemMoveHeader: TListBoxItem;
    recGrupMoveParametreleri: TRectangle;
    recGrupMoveParametreleriSvg: TRectangle;
    svgMoveParametreleriIcon: TSkSvg;
    lblGrupMoveParametreleri: TSkLabel;
    recGrupMoveParametreleriToggle: TRectangle;
    svgMoveParametreleriArrow: TSkSvg;
    cbxIsContinuousAngle: TCheckBox;
    cbxIsStrictDirection: TCheckBox;
    cbxIsDiscrete: TCheckBox;
    cbxIsReversedAngleOfTemplate: TCheckBox;

     // Header Click Events
    procedure svgEksenParametreleriArrowClick(Sender: TObject);
    procedure svgPasoKesmeParametreleriArrowClick(Sender: TObject);
    procedure svgHizParametreleriArrowClick(Sender: TObject);
    procedure svgIslemParametreleriArrowClick(Sender: TObject);
    procedure svgGeometriParametreleriArrowClick(Sender: TObject);
    procedure svgAciParametreleriArrowClick(Sender: TObject);
    procedure svgRenkParametreleriArrowClick(Sender: TObject);
    procedure svgStretchParametreleriArrowClick(Sender: TObject);
    procedure svgCropParametreleriArrowClick(Sender: TObject);
    procedure svgMoveParametreleriArrowClick(Sender: TObject);

    // Rectangle Click Events (parent click events)
    procedure recGrupEksenParametreleriClick(Sender: TObject);
    procedure recGrupPasoKesmeParametreleriClick(Sender: TObject);
    procedure recGrupHizParametreleriClick(Sender: TObject);
    procedure recGrupIslemParametreleriClick(Sender: TObject);
    procedure recGrupGeometriParametreleriClick(Sender: TObject);
    procedure recGrupAciParametreleriClick(Sender: TObject);
    procedure recGrupRenkParametreleriClick(Sender: TObject);
    procedure recGrupStretchParametreleriClick(Sender: TObject);
    procedure recGrupCropParametreleriClick(Sender: TObject);
    procedure recGrupMoveParametreleriClick(Sender: TObject);

    // Help Click Events
    procedure svgHelpTypeOfAxisClick(Sender: TObject);
    procedure svgHelpPointSplitTypeClick(Sender: TObject);
    procedure svgHelpPointSplitValueClick(Sender: TObject);
    procedure svgHelpAngleOfArmRefClick(Sender: TObject);
    procedure svgHelpMillingStepValueClick(Sender: TObject);
    procedure svgHelpMillingStepCountClick(Sender: TObject);
    procedure svgHelpMillingFinishClick(Sender: TObject);
    procedure svgHelpSpeedOfMovementClick(Sender: TObject);
    procedure svgHelpSpeedOfMillingClick(Sender: TObject);
    procedure svgHelpSpeedOfApproachClick(Sender: TObject);
    procedure svgHelpSafetyDistanceClick(Sender: TObject);
    procedure svgHelpApproachDistanceClick(Sender: TObject);
    procedure svgHelpAFileNameClick(Sender: TObject);
    procedure svgHelpHeaderOfProgramClick(Sender: TObject);
    procedure svgHelpFooterOfProgramClick(Sender: TObject);
    procedure svgHelpHeaderOfAxisClick(Sender: TObject);
    procedure svgHelpFooterOfAxisClick(Sender: TObject);
    procedure svgHelpMovementHeaderOfAxisClick(Sender: TObject);
    procedure svgHelpMovementFooterOfAxisClick(Sender: TObject);
    procedure svgHelpMillingHeaderOfAxisClick(Sender: TObject);
    procedure svgHelpMillingFooterOfAxisClick(Sender: TObject);
    procedure svgHelpHeaderOfToolClick(Sender: TObject);
    procedure svgHelpFooterOfToolClick(Sender: TObject);
    procedure svgHelpRadiusOfPartClick(Sender: TObject);
    procedure svgHelpRadiusOfProjectionClick(Sender: TObject);
    procedure svgHelpWidthOfPartClick(Sender: TObject);
    procedure svgHelpStartXOfProjectionClick(Sender: TObject);
    procedure svgHelpEndXOfProjectionClick(Sender: TObject);
    procedure svgHelpXOfDeviationClick(Sender: TObject);
    procedure svgHelpZOfDeviationClick(Sender: TObject);
    procedure svgHelpLengthOfTemplateClick(Sender: TObject);
    procedure svgHelpMinLimitOfTouchAngleClick(Sender: TObject);
    procedure svgHelpMaxLimitOfTouchAngleClick(Sender: TObject);
    procedure svgHelpLimitForGapOfTouchAngleClick(Sender: TObject);
    procedure svgHelpColorOfPartClick(Sender: TObject);
    procedure svgHelpColorOfProjectionClick(Sender: TObject);
    procedure svgHelpToolSelectClick(Sender: TObject);
    procedure svgHelpCUAxisFollowTypeClick(Sender: TObject);
    procedure svgHelpStretchModesClick(Sender: TObject);
    procedure svgHelpTouchAngleTypesClick(Sender: TObject);
    procedure svgHelpTouchAngleOffsetClick(Sender: TObject);
    procedure svgHelpStretchAreaXClick(Sender: TObject);
    procedure svgHelpStretchAreaYClick(Sender: TObject);
    procedure svgHelpCropAreaXClick(Sender: TObject);
    procedure svgHelpCropAreaYClick(Sender: TObject);
    procedure svgHelpMoveControlsClick(Sender: TObject);
    procedure svgHelpMirrorControlsClick(Sender: TObject);
    procedure recStretchAreaXInfoClick(Sender: TObject);
    procedure btnSelectAFileNameClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);


  private
    { Private declarations }

//    FRequest: TVerticalPartRequest;
    FAxisGroupExpanded: Boolean;
    FStepGroupExpanded: Boolean;
    FSpeedGroupExpanded: Boolean;
    FProcessGroupExpanded: Boolean;
    FGeometryGroupExpanded: Boolean;
    FAngleGroupExpanded: Boolean;
    FColorGroupExpanded: Boolean;
    FStretchGroupExpanded: Boolean;
    FCropGroupExpanded: Boolean;
    FMoveGroupExpanded: Boolean;
    procedure LoadSVGIcons;
    procedure ToggleAxisGroup;
    procedure ToggleStepGroup;
    procedure ToggleSpeedGroup;
    procedure ToggleProcessGroup;
    procedure ToggleGeometryGroup;
    procedure ToggleAngleGroup;
    procedure ToggleColorGroup;
    procedure ToggleStretchGroup;
    procedure ToggleCropGroup;
    procedure ToggleMoveGroup;

     procedure InitializeForm;

  public
    { Public declarations }
//     property Request: TVerticalPartRequest read FRequest write FRequest;

  end;

var
  FrmTempFormUI: TFrmTempFormUI;

implementation

{$R *.fmx}







// Help Click Events
procedure TFrmTempFormUI.svgHelpTypeOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Türü: Ýþlenecek parçanýn hangi eksende döneceðini belirler.' + #13#10 +
              'X, Y, Z, C veya U eksenlerinden birini seçebilirsiniz.');
end;

procedure TFrmTempFormUI.svgHelpPointSplitTypeClick(Sender: TObject);
begin
  ShowMessage('Nokta Bölme Türü: Parçanýn nasýl noktalara bölüneceðini belirler.' + #13#10 +
              '- Eþit Mesafe: Belirtilen mesafe aralýklarýnda bölünür' + #13#10 +
              '- Eþit Açý: Belirtilen açý aralýklarýnda bölünür' + #13#10 +
              '- Adaptif: Otomatik olarak optimize edilmiþ bölünür');
end;

procedure TFrmTempFormUI.svgHelpPointSplitValueClick(Sender: TObject);
begin
  ShowMessage('Nokta Bölme Deðeri: Bölme iþleminde kullanýlacak deðer.' + #13#10 +
              'Eþit Mesafe için: mm cinsinden mesafe' + #13#10 +
              'Eþit Açý için: derece cinsinden açý');
end;

procedure TFrmTempFormUI.svgHelpAngleOfArmRefClick(Sender: TObject);
begin
  ShowMessage('Kol Referans Açýsý: Ýþleme kolunun referans açýsýný belirler.' + #13#10 +
              'Bu açý iþleme baþlangýç pozisyonunu etkiler.');
end;

procedure TFrmTempFormUI.svgHelpMillingStepValueClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Adým Deðeri: Her frezeleme adýmýnda alýnacak talaþ miktarýný belirler.' + #13#10 +
              'Küçük deðerler daha hassas, büyük deðerler daha hýzlý iþleme saðlar.');
end;

procedure TFrmTempFormUI.svgHelpMillingStepCountClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Adým Sayýsý: Toplam kaç adýmda frezeleme yapýlacaðýný belirler.' + #13#10 +
              'Daha fazla adým daha hassas yüzey kalitesi saðlar.');
end;

procedure TFrmTempFormUI.svgHelpMillingFinishClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Finish Deðeri: Son geçiþte alýnacak talaþ miktarýný belirler.' + #13#10 +
              'Küçük deðerler daha iyi yüzey kalitesi saðlar.');
end;

procedure TFrmTempFormUI.svgHelpSpeedOfMovementClick(Sender: TObject);
begin
  ShowMessage('Hareket Hýzý: Takýmýn boþta hareket ederken kullanacaðý hýzý belirler.' + #13#10 +
              'mm/dk cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpSpeedOfMillingClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Hýzý: Takýmýn malzemeyi iþlerken kullanacaðý hýzý belirler.' + #13#10 +
              'mm/dk cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpSpeedOfApproachClick(Sender: TObject);
begin
  ShowMessage('Yaklaþma Hýzý: Takýmýn malzemeye yaklaþýrken kullanacaðý hýzý belirler.' + #13#10 +
              'mm/dk cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpSafetyDistanceClick(Sender: TObject);
begin
  ShowMessage('Güvenlik Mesafesi: Takýmýn malzemeden güvenli mesafede duracaðý uzaklýðý belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpApproachDistanceClick(Sender: TObject);
begin
  ShowMessage('Yaklaþma Mesafesi: Takýmýn malzemeye yaklaþma mesafesini belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;



procedure TFrmTempFormUI.svgHelpAFileNameClick(Sender: TObject);
begin
  ShowMessage('A Dosya Adý: Ýþlenecek DXF dosyasýnýn yolunu belirler.' + #13#10 +
              'Browse butonunu kullanarak dosya seçebilirsiniz.');
end;

procedure TFrmTempFormUI.svgHelpHeaderOfProgramClick(Sender: TObject);
begin
  ShowMessage('Program Baþlýðý: G-Code programýnýn baþýna eklenecek komutlarý belirler.' + #13#10 +
              'Örnek: G90 G21 (Mutlak koordinat sistemi ve milimetre birimi)');
end;

procedure TFrmTempFormUI.svgHelpFooterOfProgramClick(Sender: TObject);
begin
  ShowMessage('Program Sonu: G-Code programýnýn sonuna eklenecek komutlarý belirler.' + #13#10 +
              'Örnek: M30 (Program sonu ve reset)');
end;

procedure TFrmTempFormUI.svgHelpHeaderOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Baþlýðý: Her eksen iþleminin baþýna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpFooterOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Sonu: Her eksen iþleminin sonuna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpMovementHeaderOfAxisClick(Sender: TObject);
begin
  ShowMessage('Hareket Baþlýðý: Eksen hareket komutlarýnýn baþýna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpMovementFooterOfAxisClick(Sender: TObject);
begin
  ShowMessage('Hareket Sonu: Eksen hareket komutlarýnýn sonuna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpMillingHeaderOfAxisClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Baþlýðý: Frezeleme iþlemlerinin baþýna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpMillingFooterOfAxisClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Sonu: Frezeleme iþlemlerinin sonuna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpHeaderOfToolClick(Sender: TObject);
begin
  ShowMessage('Takým Baþlýðý: Takým deðiþimi komutlarýnýn baþýna eklenecek komutlarý belirler.' + #13#10 +
              'Örnek: M6 T1 (Takým deðiþimi)');
end;

procedure TFrmTempFormUI.svgHelpFooterOfToolClick(Sender: TObject);
begin
  ShowMessage('Takým Sonu: Takým iþlemlerinin sonuna eklenecek komutlarý belirler.');
end;

procedure TFrmTempFormUI.svgHelpRadiusOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Yarýçapý: Ýþlenecek parçanýn yarýçapýný belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpRadiusOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Yarýçapý: Projeksiyon alanýnýn yarýçapýný belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpWidthOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Geniþliði: Ýþlenecek parçanýn geniþliðini belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpStartXOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Baþlangýç X: Projeksiyon alanýnýn X eksenindeki baþlangýç noktasýný belirler.');
end;

procedure TFrmTempFormUI.svgHelpEndXOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Bitiþ X: Projeksiyon alanýnýn X eksenindeki bitiþ noktasýný belirler.');
end;

procedure TFrmTempFormUI.svgHelpXOfDeviationClick(Sender: TObject);
begin
  ShowMessage('X Sapmasý: X eksenindeki sapma miktarýný belirler.' + #13#10 +
              'Pozitif deðerler saða, negatif deðerler sola sapma saðlar.');
end;

procedure TFrmTempFormUI.svgHelpZOfDeviationClick(Sender: TObject);
begin
  ShowMessage('Z Sapmasý: Z eksenindeki sapma miktarýný belirler.' + #13#10 +
              'Pozitif deðerler yukarý, negatif deðerler aþaðý sapma saðlar.');
end;

procedure TFrmTempFormUI.svgHelpLengthOfTemplateClick(Sender: TObject);
begin
  ShowMessage('Þablon Uzunluðu: Kullanýlacak þablonun uzunluðunu belirler.' + #13#10 +
              'mm cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpMinLimitOfTouchAngleClick(Sender: TObject);
begin
  ShowMessage('Minimum Dokunma Açýsý: Takýmýn malzemeye dokunabileceði minimum açýyý belirler.' + #13#10 +
              'Derece cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpMaxLimitOfTouchAngleClick(Sender: TObject);
begin
  ShowMessage('Maksimum Dokunma Açýsý: Takýmýn malzemeye dokunabileceði maksimum açýyý belirler.' + #13#10 +
              'Derece cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpLimitForGapOfTouchAngleClick(Sender: TObject);
begin
  ShowMessage('Dokunma Açýsý Boþluk Limiti: Dokunma açýlarý arasýndaki boþluk limitini belirler.' + #13#10 +
              'Derece cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpColorOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Rengi: Görselleþtirmede parçanýn rengini belirler.');
end;

procedure TFrmTempFormUI.svgHelpColorOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Rengi: Görselleþtirmede projeksiyonun rengini belirler.');
end;

procedure TFrmTempFormUI.svgHelpToolSelectClick(Sender: TObject);
begin
  ShowMessage('Takým Seçimi: Kullanýlacak takýmý belirler.' + #13#10 +
              'Þimdilik T1 ve T2 takýmlarý mevcuttur.');
end;

procedure TFrmTempFormUI.svgHelpCUAxisFollowTypeClick(Sender: TObject);
begin
  ShowMessage('CU Eksen Takip Türü: C ve U eksenlerinin takip þeklini belirler.' + #13#10 +
              '- Sürekli Açý: Sürekli açýsal hareket' + #13#10 +
              '- Ayrýk: Belirli noktalarda durma' + #13#10 +
              '- Katý Yön: Sabit yönde hareket');
end;

procedure TFrmTempFormUI.svgHelpStretchModesClick(Sender: TObject);
begin
  ShowMessage('Germe Modlarý: Görüntünün nasýl gerileceðini belirler.' + #13#10 +
              '- Þablona Göre: Þablon boyutlarýna göre germe' + #13#10 +
              '- Yok: Germe yapýlmaz' + #13#10 +
              '- Min-Max: Minimum ve maksimum deðerlere göre germe');
end;

procedure TFrmTempFormUI.svgHelpTouchAngleTypesClick(Sender: TObject);
begin
  ShowMessage('Dokunma Açýsý Türleri: Dokunma açýsýnýn nasýl hesaplanacaðýný belirler.' + #13#10 +
              '- Yok: Dokunma açýsý hesaplanmaz' + #13#10 +
              '- Statik: Sabit açý deðeri' + #13#10 +
              '- Dinamik: Hareket sýrasýnda deðiþken açý');
end;

procedure TFrmTempFormUI.svgHelpTouchAngleOffsetClick(Sender: TObject);
begin
  ShowMessage('Dokunma Açýsý Ofset: Dokunma açýsýna eklenen ofset deðerini belirler.' + #13#10 +
              'Derece cinsinden girilir.');
end;

procedure TFrmTempFormUI.svgHelpStretchAreaXClick(Sender: TObject);
begin
  ShowMessage('X Germe Alaný: X ekseninde germe yapýlacak alaný belirler.' + #13#10 +
              'Yüzde cinsinden baþlangýç ve bitiþ deðerleri girilir.');
end;

procedure TFrmTempFormUI.svgHelpStretchAreaYClick(Sender: TObject);
begin
  ShowMessage('Y Germe Alaný: Y ekseninde germe yapýlacak alaný belirler.' + #13#10 +
              'Yüzde cinsinden baþlangýç ve bitiþ deðerleri girilir.');
end;

procedure TFrmTempFormUI.svgHelpCropAreaXClick(Sender: TObject);
begin
  ShowMessage('X Kýrpma Alaný: X ekseninde kýrpýlacak alaný belirler.' + #13#10 +
              'Yüzde cinsinden baþlangýç ve bitiþ deðerleri girilir.');
end;

procedure TFrmTempFormUI.svgHelpCropAreaYClick(Sender: TObject);
begin
  ShowMessage('Y Kýrpma Alaný: Y ekseninde kýrpýlacak alaný belirler.' + #13#10 +
              'Yüzde cinsinden baþlangýç ve bitiþ deðerleri girilir.');
end;

procedure TFrmTempFormUI.svgHelpMoveControlsClick(Sender: TObject);
begin
  ShowMessage('Hareket Kontrolleri: X ve Y eksenlerinde yapýlacak hareket miktarýný belirler.' + #13#10 +
              '"Katý Alan" seçeneði hareketin belirli alan içinde kalmasýný saðlar.');
end;

procedure TFrmTempFormUI.svgHelpMirrorControlsClick(Sender: TObject);
begin
  ShowMessage('Aynalama Kontrolleri: Görüntünün hangi eksenlerde ayn?lanacaðýný belirler.' + #13#10 +
              '- X Aynalama: X ekseni boyunca aynalama' + #13#10 +
              '- Y Aynalama: Y ekseni boyunca aynalama' + #13#10 +
              '- Min-Max Aynalama: Minimum ve maksimum deðerlere göre aynalama');
end;



// Toggle Group Methods
procedure TFrmTempFormUI.ToggleAxisGroup;
begin
  FAxisGroupExpanded := not FAxisGroupExpanded;

  lItemTypeOfAxis.Visible := FAxisGroupExpanded;
  lItemPointSplitType.Visible := FAxisGroupExpanded;
  lItemPointSplitValue.Visible := FAxisGroupExpanded;
  lItemAngleOfArmRef.Visible := FAxisGroupExpanded;
  lItemToolSelect.Visible := FAxisGroupExpanded;
  if FAxisGroupExpanded then
    svgEksenParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgEksenParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleStepGroup;
begin
  FStepGroupExpanded := not FStepGroupExpanded;

  lItemMillingStepValue.Visible := FStepGroupExpanded;
  lItemMillingStepCount.Visible := FStepGroupExpanded;
  lItemMillingFinish.Visible := FStepGroupExpanded;

  if FStepGroupExpanded then
    svgPasoKesmeParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgPasoKesmeParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleSpeedGroup;
begin
  FSpeedGroupExpanded := not FSpeedGroupExpanded;

  lItemSpeedOfMovement.Visible := FSpeedGroupExpanded;
  lItemSpeedOfMilling.Visible := FSpeedGroupExpanded;
  lItemSpeedOfApproach.Visible := FSpeedGroupExpanded;
  lItemSafetyDistance.Visible := FSpeedGroupExpanded;
  lItemApproachDistance.Visible := FSpeedGroupExpanded;

  if FSpeedGroupExpanded then
    svgHizParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgHizParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleProcessGroup;
begin
  FProcessGroupExpanded := not FProcessGroupExpanded;

  lItemAFileName.Visible := FProcessGroupExpanded;
  lItemHeaderOfProgram.Visible := FProcessGroupExpanded;
  lItemFooterOfProgram.Visible := FProcessGroupExpanded;
  lItemHeaderOfAxis.Visible := FProcessGroupExpanded;
  lItemFooterOfAxis.Visible := FProcessGroupExpanded;
  lItemMovementHeaderOfAxis.Visible := FProcessGroupExpanded;
  lItemMovementFooterOfAxis.Visible := FProcessGroupExpanded;
  lItemMillingHeaderOfAxis.Visible := FProcessGroupExpanded;
  lItemMillingFooterOfAxis.Visible := FProcessGroupExpanded;
  lItemHeaderOfTool.Visible := FProcessGroupExpanded;
  lItemFooterOfTool.Visible := FProcessGroupExpanded;
//  lItemToolSelect.Visible := FProcessGroupExpanded;

  if FProcessGroupExpanded then
    svgIslemParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgIslemParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleGeometryGroup;
begin
  FGeometryGroupExpanded := not FGeometryGroupExpanded;

  lItemRadiusOfPart.Visible := FGeometryGroupExpanded;
  lItemRadiusOfProjection.Visible := FGeometryGroupExpanded;
  lItemWidthOfPart.Visible := FGeometryGroupExpanded;
  lItemStartXOfProjection.Visible := FGeometryGroupExpanded;
  lItemEndXOfProjection.Visible := FGeometryGroupExpanded;
  lItemXOfDeviation.Visible := FGeometryGroupExpanded;
  lItemZOfDeviation.Visible := FGeometryGroupExpanded;
  lItemLengthOfTemplate.Visible := FGeometryGroupExpanded;

  if FGeometryGroupExpanded then
    svgGeometriParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgGeometriParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleMoveGroup;
begin
  FMoveGroupExpanded := not FMoveGroupExpanded;
  lItemMirrorControls.Visible := FMoveGroupExpanded;
  lItemMoveControls.Visible := FMoveGroupExpanded;
  if FMoveGroupExpanded then
    svgMoveParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgMoveParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;

end;

procedure TFrmTempFormUI.ToggleAngleGroup;
begin
  FAngleGroupExpanded := not FAngleGroupExpanded;

  lItemMinLimitOfTouchAngle.Visible := FAngleGroupExpanded;
  lItemMaxLimitOfTouchAngle.Visible := FAngleGroupExpanded;
  lItemLimitForGapOfTouchAngle.Visible := FAngleGroupExpanded;
  lItemCUAxisFollowType.Visible := FAngleGroupExpanded;
  lItemTouchAngleTypes.Visible := FAngleGroupExpanded;
  lItemTouchAngleOffset.Visible := FAngleGroupExpanded;

  if FAngleGroupExpanded then
    svgAciParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgAciParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleColorGroup;
begin
  FColorGroupExpanded := not FColorGroupExpanded;

  lItemColorOfPart.Visible := FColorGroupExpanded;
  lItemColorOfProjection.Visible := FColorGroupExpanded;

  if FColorGroupExpanded then
    svgRenkParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgRenkParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleStretchGroup;
begin
  FStretchGroupExpanded := not FStretchGroupExpanded;

  lItemStretchModes.Visible := FStretchGroupExpanded;
  lItemStretchAreaX.Visible := FStretchGroupExpanded;
  lItemStretchAreaY.Visible := FStretchGroupExpanded;

  if FStretchGroupExpanded then
    svgStretchParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgStretchParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmTempFormUI.ToggleCropGroup;
begin
  FCropGroupExpanded := not FCropGroupExpanded;

  lItemCropAreaX.Visible := FCropGroupExpanded;
  lItemCropAreaY.Visible := FCropGroupExpanded;

  if FCropGroupExpanded then
    svgCropParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgCropParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;


// Header Click Events - Rectangle Click (Parent click events)






procedure TFrmTempFormUI.btnSelectAFileNameClick(
  Sender: TObject);
begin
  inherited;
  if odDXF.Execute then
  begin
    edtAFileName.Text := odDXF.FileName;
  end;
end;

procedure TFrmTempFormUI.FormCreate(Sender: TObject);
begin
  inherited;
  InitializeForm;
  LoadSVGIcons;
  // Baþlangýçta tüm gruplarý geniþletilmiþ olarak ayarla
  FAxisGroupExpanded := True;
  FStepGroupExpanded := True;
  FSpeedGroupExpanded := True;
  FProcessGroupExpanded := True;
  FGeometryGroupExpanded := True;
  FAngleGroupExpanded := True;
  FColorGroupExpanded := True;
  FStretchGroupExpanded := True;
  FCropGroupExpanded := True;
  FMoveGroupExpanded := True;
end;

procedure TFrmTempFormUI.InitializeForm;
begin
 // Form ayarlarý
  Fill.Color := TAlphaColorRec.Black;

  // ListBox ayarlarý
  lstParameters.ShowScrollBars := True;
  lstParameters.StyleLookup := 'listboxstyle';
  lstParameters.ItemHeight := 35;

  // ComboBox items'larýný doldur
  cbxTypeOfAxis.Items.Clear;
  cbxTypeOfAxis.Items.Add('C');
  cbxTypeOfAxis.Items.Add('U');
  cbxTypeOfAxis.Items.Add('Fixed');
  cbxTypeOfAxis.Items.Add('Lazer');
  cbxTypeOfAxis.Items.Add('Spindle');

  cbxTypeOfAxis.Items.Add('Torna1');
  cbxTypeOfAxis.Items.Add('Torna2');
  cbxTypeOfAxis.Items.Add('Zemin');

  cbxTypeOfAxis.ItemIndex := 4;

  cbxPointSplitType.Items.Clear;
  cbxPointSplitType.Items.Add('Nokta Modu');
  cbxPointSplitType.Items.Add('mm Nokta Sayýsý');
  cbxPointSplitType.Items.Add('Orta Nokta');
  cbxPointSplitType.ItemIndex := 1;
 // interval verilen mm kadar nokta koy
 // devide by unit 1 mm ye kaç notka.
  // Tool selection - þimdilik T1, T2
  cbxToolSelect.Items.Clear;
  cbxToolSelect.Items.Add('T1');
  cbxToolSelect.Items.Add('T2');
  cbxToolSelect.ItemIndex := 0;

  // NumberBox varsayýlan deðerleri

  numRadiusOfPart.Value := 12.4;
  numRadiusOfProjection.Value := 8.0;


  numStartXOfProjection.Value := -4.05;
  numEndXOfProjection.Value := 4.05;
  numMinLimitOfTouchAngle.Value := -110.0;
  numMaxLimitOfTouchAngle.Value := 110.0;
  numPointSplitValue.Value := 10.0;
  numWidthOfPart.Value := 8.1;

  numXOfDeviation.Value := 3.34560;
  numZOfDeviation.Value := 0.12850;
  numAngleOfArmRef.Value := 90.0;

  numLengthOfTemplate.Value := 60.0;
  numMillingStepValue.Value := 0.01;
  numMillingStepCount.Value := 1.0;
  numMillingFinish.Value := 0.0;

  numSpeedOfMovement.Value := 5000.0;
  numSpeedOfMilling.Value := 3000.0;
  numSpeedOfApproach.Value := 3500.0;
  numLimitForGapOfTouchAngle.Value := 5;
  numSafetyDistance.Value := 1.0;
  numApproachDistance.Value := 0.5;


  numTouchAngleOffset.Value := 0.0;

  // Edit varsayýlan deðerleri
  edtHeaderOfProgram.Text := 'G90 G21';
  edtFooterOfProgram.Text := 'M30';
  edtHeaderOfAxis.Text := '';
  edtFooterOfAxis.Text := '';
  edtMovementHeaderOfAxis.Text := '';
  edtMovementFooterOfAxis.Text := '';
  edtMillingHeaderOfAxis.Text := '';
  edtMillingFooterOfAxis.Text := '';
  edtHeaderOfTool.Text := '';
  edtFooterOfTool.Text := '';
  edtColorOfPart.Color := TAlphaColorRec.White;
  edtColorOfProjection.Color := TAlphaColorRec.Alpha;
  edtAFileName.Text := '';

  // Boolean controls
  cbxIsContinuousAngle.IsChecked := True;
  cbxIsStrictDirection.IsChecked := True ;
  cbxIsDiscrete.IsChecked := False;
  rbtStretch_SwitchModeToTemplate.IsChecked := True;
  rbtTouchAngle_SwitchTypeToNone.IsChecked := false;
  cbxIsReversedAngleOfTemplate.IsChecked := true;
  // TrackBars
  ctbStretch_OnX_SetAreaFrom.Value := 0;
  ctbStretch_OnX_SetAreaTo.Value := 100;
  ctbStretch_OnY_SetAreaFrom.Value := 0;
  ctbStretch_OnY_SetAreaTo.Value := 100;
  ctbCrop_OnX_SetAreaFrom.Value := 0;
  ctbCrop_OnX_SetAreaTo.Value := 100;
  ctbCrop_OnY_SetAreaFrom.Value := 0;
  ctbCrop_OnY_SetAreaTo.Value := 100;
  ctbMoveOnX.Value := 0;
  ctbMoveOnY.Value := 0;

  // CheckBoxes
  cbxMove_OnStrictArea.IsChecked := False;
  cbxMirrorOnX.IsChecked := False;
  cbxMirrorOnY.IsChecked := False;
  cbxMirrorOnMixMax.IsChecked := False;
end;

procedure TFrmTempFormUI.LoadSVGIcons;
begin
  // Group icons
  svgEksenParametreleriIcon.Svg.Source := SVG_GEAR;
  svgPasoKesmeParametreleriIcon.Svg.Source := SVG_PROCESS;
  svgHizParametreleriIcon.Svg.Source := SVG_SPEED;
  svgIslemParametreleriIcon.Svg.Source := SVG_PROCESS;
  svgGeometriParametreleriIcon.Svg.Source := SVG_GEOMETRY;
  svgAciParametreleriIcon.Svg.Source := SVG_ANGLE;
  svgRenkParametreleriIcon.Svg.Source := SVG_COLOR;
  svgStretchParametreleriIcon.Svg.Source := SVG_STRETCH;
  svgCropParametreleriIcon.Svg.Source := SVG_CROP;
  svgMoveParametreleriIcon.Svg.Source := SVG_MOVE;

  // Arrow icons
  svgEksenParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgPasoKesmeParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgHizParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgIslemParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgGeometriParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgAciParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgRenkParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgStretchParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgCropParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;
  svgMoveParametreleriArrow.Svg.Source := SVG_ARROW_DOWN;

  // Help icons
  svgHelpTypeOfAxis.Svg.Source := SVG_HELP;
  svgHelpPointSplitType.Svg.Source := SVG_HELP;
  svgHelpPointSplitValue.Svg.Source := SVG_HELP;
  svgHelpAngleOfArmRef.Svg.Source := SVG_HELP;
  svgHelpMillingStepValue.Svg.Source := SVG_HELP;
  svgHelpMillingStepCount.Svg.Source := SVG_HELP;
  svgHelpMillingFinish.Svg.Source := SVG_HELP;
  svgHelpSpeedOfMovement.Svg.Source := SVG_HELP;
  svgHelpSpeedOfMilling.Svg.Source := SVG_HELP;
  svgHelpSpeedOfApproach.Svg.Source := SVG_HELP;
  svgHelpSafetyDistance.Svg.Source := SVG_HELP;
  svgHelpApproachDistance.Svg.Source := SVG_HELP;
  svgHelpAFileName.Svg.Source := SVG_HELP;
  svgHelpHeaderOfProgram.Svg.Source := SVG_HELP;
  svgHelpFooterOfProgram.Svg.Source := SVG_HELP;
  svgHelpHeaderOfAxis.Svg.Source := SVG_HELP;
  svgHelpFooterOfAxis.Svg.Source := SVG_HELP;
  svgHelpMovementHeaderOfAxis.Svg.Source := SVG_HELP;
  svgHelpMovementFooterOfAxis.Svg.Source := SVG_HELP;
  svgHelpMillingHeaderOfAxis.Svg.Source := SVG_HELP;
  svgHelpMillingFooterOfAxis.Svg.Source := SVG_HELP;
  svgHelpHeaderOfTool.Svg.Source := SVG_HELP;
  svgHelpFooterOfTool.Svg.Source := SVG_HELP;
  svgHelpRadiusOfPart.Svg.Source := SVG_HELP;
  svgHelpRadiusOfProjection.Svg.Source := SVG_HELP;
  svgHelpWidthOfPart.Svg.Source := SVG_HELP;
  svgHelpStartXOfProjection.Svg.Source := SVG_HELP;
  svgHelpEndXOfProjection.Svg.Source := SVG_HELP;
  svgHelpXOfDeviation.Svg.Source := SVG_HELP;
  svgHelpZOfDeviation.Svg.Source := SVG_HELP;
  svgHelpLengthOfTemplate.Svg.Source := SVG_HELP;
  svgHelpMinLimitOfTouchAngle.Svg.Source := SVG_HELP;
  svgHelpMaxLimitOfTouchAngle.Svg.Source := SVG_HELP;
  svgHelpLimitForGapOfTouchAngle.Svg.Source := SVG_HELP;
  svgHelpColorOfPart.Svg.Source := SVG_HELP;
  svgHelpColorOfProjection.Svg.Source := SVG_HELP;
  svgHelpToolSelect.Svg.Source := SVG_HELP;
  svgHelpCUAxisFollowType.Svg.Source := SVG_HELP;
  svgHelpStretchModes.Svg.Source := SVG_HELP;
  svgHelpTouchAngleTypes.Svg.Source := SVG_HELP;
  svgHelpTouchAngleOffset.Svg.Source := SVG_HELP;
  svgHelpStretchAreaX.Svg.Source := SVG_HELP;
  svgHelpStretchAreaY.Svg.Source := SVG_HELP;
  svgHelpCropAreaX.Svg.Source := SVG_HELP;
  svgHelpCropAreaY.Svg.Source := SVG_HELP;
  svgHelpMoveControls.Svg.Source := SVG_HELP;
  svgHelpMirrorControls.Svg.Source := SVG_HELP;
end;



// Header Click Events - Arrow Click

procedure TFrmTempFormUI.svgPasoKesmeParametreleriArrowClick(Sender: TObject);
begin
  ToggleStepGroup;
end;

procedure TFrmTempFormUI.svgHizParametreleriArrowClick(Sender: TObject);
begin
  ToggleSpeedGroup;
end;

procedure TFrmTempFormUI.svgIslemParametreleriArrowClick(Sender: TObject);
begin
  ToggleProcessGroup;
end;




procedure TFrmTempFormUI.svgRenkParametreleriArrowClick(Sender: TObject);
begin
  ToggleColorGroup;
end;

procedure TFrmTempFormUI.svgStretchParametreleriArrowClick(Sender: TObject);
begin
  ToggleStretchGroup;
end;



procedure TFrmTempFormUI.svgMoveParametreleriArrowClick(Sender: TObject);
begin
  ToggleMoveGroup;
end;

// Header Click Events - Rectangle Click (Parent click events)
procedure TFrmTempFormUI.recGrupEksenParametreleriClick(Sender: TObject);
begin
  ToggleAxisGroup;
end;

procedure TFrmTempFormUI.recGrupPasoKesmeParametreleriClick(Sender: TObject);
begin
  ToggleStepGroup;
end;

procedure TFrmTempFormUI.recGrupHizParametreleriClick(Sender: TObject);
begin
  ToggleSpeedGroup;
end;

procedure TFrmTempFormUI.recGrupIslemParametreleriClick(Sender: TObject);
begin
  ToggleProcessGroup;
end;

procedure TFrmTempFormUI.recGrupGeometriParametreleriClick(Sender: TObject);
begin
  ToggleGeometryGroup;
end;

procedure TFrmTempFormUI.recGrupAciParametreleriClick(Sender: TObject);
begin
  ToggleAngleGroup;
end;

procedure TFrmTempFormUI.recGrupRenkParametreleriClick(Sender: TObject);
begin
  ToggleColorGroup;
end;

procedure TFrmTempFormUI.recGrupStretchParametreleriClick(Sender: TObject);
begin
  ToggleStretchGroup;
end;

procedure TFrmTempFormUI.recGrupCropParametreleriClick(Sender: TObject);
begin
  ToggleCropGroup;
end;

procedure TFrmTempFormUI.recGrupMoveParametreleriClick(Sender: TObject);
begin
  ToggleMoveGroup;

end;

procedure TFrmTempFormUI.recStretchAreaXInfoClick(
  Sender: TObject);
begin
  inherited;
  svgHelpStretchAreaXClick(self)  ;
end;

procedure TFrmTempFormUI.svgAciParametreleriArrowClick(
  Sender: TObject);
begin
  ToggleAngleGroup;
end;

procedure TFrmTempFormUI.svgCropParametreleriArrowClick(
  Sender: TObject);
begin
 ToggleCropGroup;
end;

procedure TFrmTempFormUI.svgEksenParametreleriArrowClick(Sender: TObject);
begin
  ToggleAxisGroup;
end;

procedure TFrmTempFormUI.svgGeometriParametreleriArrowClick(
  Sender: TObject);
begin
  ToggleGeometryGroup;
end;


end.
