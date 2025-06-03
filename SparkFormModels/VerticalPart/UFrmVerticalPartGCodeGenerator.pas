 unit UFrmVerticalPartGCodeGenerator;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.EditBox, FMX.NumberBox, FMX.ComboEdit,
  FMX.ListBox, FMX.Layouts, FMX.Controls.Presentation,
  FMX.ScrollBox, FMX.Memo, FMX.Memo.Types, Skia,  FMX.Skia,
  System.Generics.Collections, VerticalPartRequest, AxisType, PointSplitter,
  CamLetter, TouchOffset, Mirror, Move, Crop, Stretch, MillingRequest,
  FMX.Colors, FMX.ComboTrackBar;

type
  TFrmVerticalPartGCodeGenerator = class(TForm)
    pnlMain: TPanel;
    lstParameters: TListBox;
    pnlBottom: TPanel;

    // Eksen Parametreleri Grubu
    lItemAxisHeader: TListBoxItem;

    lItemTypeOfAxis: TListBoxItem;

    lItemPointSplitType: TListBoxItem;

    lItemPointSplitValue: TListBoxItem;

    // Paso-Kesme Parametreleri Grubu
    lItemStepHeader: TListBoxItem;

    lItemMillingStepValue: TListBoxItem;
    recMillingStepValue: TRectangle;
    lblMillingStepValue: TSkLabel;
    numMillingStepValue: TNumberBox;

    lItemMillingStepCount: TListBoxItem;
    recMillingStepCount: TRectangle;
    lblMillingStepCount: TSkLabel;
    numMillingStepCount: TNumberBox;

    lItemMillingFinish: TListBoxItem;
    recMillingFinish: TRectangle;
    lblMillingFinish: TSkLabel;
    numMillingFinish: TNumberBox;

    // Hýz Parametreleri Grubu
    lItemSpeedHeader: TListBoxItem;

    lItemPointCountInMM: TListBoxItem;

    // Ýþlem Parametreleri Grubu
    lItemProcessHeader: TListBoxItem;

    lItemAFileName: TListBoxItem;
    recAFileName: TRectangle;
    lblAFileName: TSkLabel;
    edtAFileName: TEdit;
    btnSelectAFileName: TButton;
    svgHelpAFileName: TSkSvg;

    lItemHeaderOfProgram: TListBoxItem;
    recHeaderOfProgram: TRectangle;
    edtHeaderOfProgram: TEdit;

    lItemFooterOfProgram: TListBoxItem;
    recFooterOfProgram: TRectangle;
    lblFooterOfProgram: TSkLabel;
    edtFooterOfProgram: TEdit;

    lItemHeaderOfAxis: TListBoxItem;
    recHeaderOfAxis: TRectangle;
    lblHeaderOfAxis: TSkLabel;
    edtHeaderOfAxis: TEdit;

    lItemFooterOfAxis: TListBoxItem;
    recFooterOfAxis: TRectangle;
    lblFooterOfAxis: TSkLabel;
    edtFooterOfAxis: TEdit;

    lItemMovementHeaderOfAxis: TListBoxItem;
    recMovementHeaderOfAxis: TRectangle;
    lblMovementHeaderOfAxis: TSkLabel;
    edtMovementHeaderOfAxis: TEdit;

    lItemMovementFooterOfAxis: TListBoxItem;
    recMovementFooterOfAxis: TRectangle;
    lblMovementFooterOfAxis: TSkLabel;
    edtMovementFooterOfAxis: TEdit;

    // Geometri Parametreleri Grubu
    lItemGeometryHeader: TListBoxItem;

    lItemRadiusOfPart: TListBoxItem;
    recRadiusOfPart: TRectangle;
    lblRadiusOfPart: TSkLabel;
    numRadiusOfPart: TNumberBox;

    lItemRadiusOfProjection: TListBoxItem;
    recRadiusOfProjection: TRectangle;
    lblRadiusOfProjection: TSkLabel;
    numRadiusOfProjection: TNumberBox;

    lItemWidthOfPart: TListBoxItem;
    recWidthOfPart: TRectangle;
    lblWidthOfPart: TSkLabel;
    numWidthOfPart: TNumberBox;

    lItemStartX: TListBoxItem;

    lItemEndX: TListBoxItem;

    lItemDeviationX: TListBoxItem;

    lItemDeviationZ: TListBoxItem;

    lItemLengthOfTemplate: TListBoxItem;

    // Açý Parametreleri Grubu
    lItemAngleHeader: TListBoxItem;

    lItemMinAngleOfTouch: TListBoxItem;

    lItemMaxAngleOfTouch: TListBoxItem;

    lItemAngleOfArm: TListBoxItem;

    // Renk Parametreleri Grubu
    lItemColorHeader: TListBoxItem;

    lItemColorOfPart: TListBoxItem;
    recColorOfPart: TRectangle;
    lblColorOfPart: TSkLabel;
    svgHelpColorOfPart: TSkSvg;

    lItemColorOfProjection: TListBoxItem;
    recColorOfProjection: TRectangle;
    lblColorOfProjection: TSkLabel;
    svgHelpColorOfProjection: TSkSvg;
    recGrupEksenParametreleri: TRectangle;
    recGrupEksenParametreleriSvg: TRectangle;
    svgEksenParametreleriIcon: TSkSvg;
    lblGrupEksenParametreleri: TSkLabel;
    recGrupEksenParametreleriToggle: TRectangle;
    svgEksenParametreleriArrow: TSkSvg;
    odDXF: TOpenDialog;
    SaveDialog1: TSaveDialog;
    recGrupPasoKesmeParametreleri: TRectangle;
    recGrupPasoKesmeParametreleriSvg: TRectangle;
    svgPasoKesmeParametreleriIcon: TSkSvg;
    lblGrupPasoKesmeParametreleri: TSkLabel;
    recGrupPasoKesmeParametreleriToggle: TRectangle;
    svgPasoKesmeParametreleriArrow: TSkSvg;
    recGrupIslemParametreleri: TRectangle;
    recGrupIslemParametreleriSvg: TRectangle;
    svgIslemParametreleriIcon: TSkSvg;
    lblGrupIslemParametreleri: TSkLabel;
    recGrupIslemParametreleriToggle: TRectangle;
    svgIslemParametreleriArrow: TSkSvg;
    recGrupGeometriParametreleri: TRectangle;
    recGrupGeometriParametreleriSvg: TRectangle;
    svgGeometriParametreleriIcon: TSkSvg;
    lblGrupGeometriParametreleri: TSkLabel;
    recGrupGeometriParametreleriToggle: TRectangle;
    svgGeometriParametreleriArrow: TSkSvg;
    recGrupAciParametreleri: TRectangle;
    recGrupAciParametreleriSvg: TRectangle;
    svgAciParametreleriIcon: TSkSvg;
    lblGrupAciParametreleri: TSkLabel;
    recGrupAciParametreleriToggle: TRectangle;
    svgAciParametreleriArrow: TSkSvg;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    SkSvg1: TSkSvg;
    SkLabelColor: TSkLabel;
    Rectangle3: TRectangle;
    svgColorHeader: TSkSvg;
    recTypeOfAxis: TRectangle;
    layoutTypeOfAxisLabel: TLayout;
    lblTypeOfAxis: TSkLabel;
    recTypeOfAxisControl: TRectangle;
    cbxTypeOfAxis: TComboBox;
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
    svgSpeedOfMovementInfo: TSkSvg;
    recSpeedOfMovementControl: TRectangle;
    numSpeedOfMovement: TNumberBox;
    lItemSpeedOfMilling: TListBoxItem;
    recSpeedOfMilling: TRectangle;
    layoutSpeedOfMillingLabel: TLayout;
    lblSpeedOfMilling: TSkLabel;
    recSpeedOfMillingInfo: TRectangle;
    svgSpeedOfMillingInfo: TSkSvg;
    recSpeedOfMillingControl: TRectangle;
    numSpeedOfMilling: TNumberBox;
    lItemSpeedOfApproach: TListBoxItem;
    recSpeedOfApproach: TRectangle;
    layoutSpeedOfApproachLabel: TLayout;
    lblSpeedOfApproach: TSkLabel;
    recSpeedOfApproachInfo: TRectangle;
    svgSpeedOfApproachInfo: TSkSvg;
    recSpeedOfApproachControl: TRectangle;
    numSpeedOfApproach: TNumberBox;
    lItemSafetyDistance: TListBoxItem;
    recSafetyDistance: TRectangle;
    layoutSafetyDistanceLabel: TLayout;
    lblSafetyDistance: TSkLabel;
    recSafetyDistanceInfo: TRectangle;
    svgSafetyDistanceInfo: TSkSvg;
    recSafetyDistanceControl: TRectangle;
    numSafetyDistance: TNumberBox;
    lItemApproachDistance: TListBoxItem;
    recApproachDistance: TRectangle;
    layoutApproachDistanceLabel: TLayout;
    lblApproachDistance: TSkLabel;
    recApproachDistanceInfo: TRectangle;
    svgApproachDistanceInfo: TSkSvg;
    recApproachDistanceControl: TRectangle;
    numApproachDistance: TNumberBox;
    recPointSplitType: TRectangle;
    layoutPointSplitTypeLabel: TLayout;
    lblPointSplitType: TSkLabel;
    recPointSplitTypeInfo: TRectangle;
    svgPointSplitTypeInfo: TSkSvg;
    recPointSplitTypeControl: TRectangle;
    cbxPointSplitType: TComboBox;
    recPointSplitValue: TRectangle;
    layoutPointSplitValueLabel: TLayout;
    lblPointSplitValue: TSkLabel;
    recPointSplitValueInfo: TRectangle;
    svgPointSplitValueInfo: TSkSvg;
    recPointSplitValueControl: TRectangle;
    numPointSplitValue: TNumberBox;
    recAngleOfArmRef: TRectangle;
    layoutAngleOfArmRefLabel: TLayout;
    lblAngleOfArmRef: TSkLabel;
    recAngleOfArmRefInfo: TRectangle;
    svgAngleOfArmRefInfo: TSkSvg;
    recAngleOfArmRefControl: TRectangle;
    numAngleOfArmRef: TNumberBox;
    Rectangle4: TRectangle;
    Rectangle5: TRectangle;
    Rectangle6: TRectangle;
    svgHelpMillingStepValue: TSkSvg;
    svgHelpMillingStepCount: TSkSvg;
    svgHelpMillingFinish: TSkSvg;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Rectangle7: TRectangle;
    recStartXOfProjection: TRectangle;
    layoutStartXOfProjectionLabel: TLayout;
    lblStartXOfProjection: TSkLabel;
    recStartXOfProjectionInfo: TRectangle;
    svgStartXOfProjectionInfo: TSkSvg;
    recStartXOfProjectionControl: TRectangle;
    numStartXOfProjection: TNumberBox;
    recEndXOfProjection: TRectangle;
    layoutEndXOfProjectionLabel: TLayout;
    lblEndXOfProjection: TSkLabel;
    recEndXOfProjectionInfo: TRectangle;
    svgEndXOfProjectionInfo: TSkSvg;
    recEndXOfProjectionControl: TRectangle;
    numEndXOfProjection: TNumberBox;
    recXOfDeviation: TRectangle;
    layoutXOfDeviationLabel: TLayout;
    lblXOfDeviation: TSkLabel;
    recXOfDeviationInfo: TRectangle;
    svgXOfDeviationInfo: TSkSvg;
    recXOfDeviationControl: TRectangle;
    numXOfDeviation: TNumberBox;
    recZOfDeviation: TRectangle;
    layoutZOfDeviationLabel: TLayout;
    lblZOfDeviation: TSkLabel;
    recZOfDeviationInfo: TRectangle;
    svgZOfDeviationInfo: TSkSvg;
    recZOfDeviationControl: TRectangle;
    numZOfDeviation: TNumberBox;
    recLengthOfTemplate: TRectangle;
    layoutLengthOfTemplateLabel: TLayout;
    lblLengthOfTemplate: TSkLabel;
    recLengthOfTemplateInfo: TRectangle;
    svgLengthOfTemplateInfo: TSkSvg;
    recLengthOfTemplateControl: TRectangle;
    numLengthOfTemplate: TNumberBox;
    recMinLimitOfTouchAngle: TRectangle;
    layoutMinLimitOfTouchAngleLabel: TLayout;
    lblMinLimitOfTouchAngle: TSkLabel;
    recMinLimitOfTouchAngleInfo: TRectangle;
    svgMinLimitOfTouchAngleInfo: TSkSvg;
    recMinLimitOfTouchAngleControl: TRectangle;
    numMinLimitOfTouchAngle: TNumberBox;
    recMaxLimitOfTouchAngle: TRectangle;
    layoutMaxLimitOfTouchAngleLabel: TLayout;
    lblMaxLimitOfTouchAngle: TSkLabel;
    recMaxLimitOfTouchAngleInfo: TRectangle;
    svgMaxLimitOfTouchAngleInfo: TSkSvg;
    recMaxLimitOfTouchAngleControl: TRectangle;
    numMaxLimitOfTouchAngle: TNumberBox;
    Rectangle8: TRectangle;
    svgHelpTypeOfAxis: TSkSvg;
    ItemProjectSettingsHeader: TListBoxItem;
    Rectangle9: TRectangle;
    Rectangle10: TRectangle;
    SkSvg3: TSkSvg;
    SkLabelProjectSettings: TSkLabel;
    Rectangle11: TRectangle;
    SkSvg4: TSkSvg;
    Rectangle12: TRectangle;
    Layout4: TLayout;
    SkLabelSpindleSpeed: TSkLabel;
    Rectangle13: TRectangle;
    SkSvg5: TSkSvg;
    Rectangle14: TRectangle;
    numSpindleSpeed: TNumberBox;
    lItemLimitForGapOfTouchAngle: TListBoxItem;
    recLimitForGapOfTouchAngle: TRectangle;
    layoutLimitForGapOfTouchAngleLabel: TLayout;
    lblLimitForGapOfTouchAngle: TSkLabel;
    recLimitForGapOfTouchAngleInfo: TRectangle;
    svgLimitForGapOfTouchAngleInfo: TSkSvg;
    recLimitForGapOfTouchAngleControl: TRectangle;
    numLimitForGapOfTouchAngle: TNumberBox;
    Layout5: TLayout;
    Layout6: TLayout;
    Layout7: TLayout;
    Layout8: TLayout;
    Layout9: TLayout;
    Layout10: TLayout;
    Layout11: TLayout;
    Layout12: TLayout;
    Layout13: TLayout;
    Layout14: TLayout;
    lblHeaderOfProgram: TSkLabel;
    Rectangle18: TRectangle;
    Rectangle19: TRectangle;
    Rectangle20: TRectangle;
    Rectangle21: TRectangle;
    Rectangle22: TRectangle;
    Rectangle23: TRectangle;
    Rectangle24: TRectangle;
    Rectangle25: TRectangle;
    Rectangle26: TRectangle;
    Rectangle27: TRectangle;
    Rectangle28: TRectangle;
    svgHelpHeaderOfProgram: TSkSvg;
    svgHelpFooterOfProgram: TSkSvg;
    svgHelpHeaderOfAxis: TSkSvg;
    svgHelpFooterOfAxis: TSkSvg;
    svgHelpMovementHeaderOfAxis: TSkSvg;
    svgHelpMovementFooterOfAxis: TSkSvg;
    svgHelpRadiusOfPart: TSkSvg;
    svgHelpRadiusOfProjection: TSkSvg;
    svgHelpWidthOfPart: TSkSvg;
    Layout15: TLayout;
    Layout16: TLayout;
    Layout17: TLayout;
    edtColorOfPart: TColorComboBox;
    edtColorOfProjection: TColorComboBox;
    ItemToolSelect: TListBoxItem;
    Rectangle29: TRectangle;
    Layout18: TLayout;
    SkLabel4: TSkLabel;
    Rectangle30: TRectangle;
    SkSvg2: TSkSvg;
    Rectangle31: TRectangle;
    cbToolSelect: TComboBox;
    ItemCUAxisFollowType: TListBoxItem;
    Rectangle32: TRectangle;
    Rectangle33: TRectangle;
    SkSvg6: TSkSvg;
    Layout19: TLayout;
    SkLabel5: TSkLabel;
    Layout20: TLayout;
    rbtIsContinuousAngle: TRadioButton;
    rbtIsDiscrete: TRadioButton;
    rbtIsStrictDirection: TRadioButton;
    ListBoxItem1: TListBoxItem;
    Rectangle34: TRectangle;
    Rectangle35: TRectangle;
    SkSvg7: TSkSvg;
    Layout21: TLayout;
    SkLabelStretchModes: TSkLabel;
    Layout22: TLayout;
    rbtStretch_SwitchModeToTemplate: TRadioButton;
    rbtStretch_SwitchModeToNone: TRadioButton;
    rbtStretch_SwitchModeToMinMax: TRadioButton;
    ListBoxItem5: TListBoxItem;
    Rectangle36: TRectangle;
    Rectangle37: TRectangle;
    SkSvg8: TSkSvg;
    Layout23: TLayout;
    SkLabel7: TSkLabel;
    Layout24: TLayout;
    rbtTouchAngle_SwitchTypeToNone: TRadioButton;
    rbtTouchAngle_SwitchTypeToStatic: TRadioButton;
    rbtTouchAngle_SwitchTypeToDynamic: TRadioButton;
    ItemStretchAreaX: TListBoxItem;
    Rectangle38: TRectangle;
    Layout25: TLayout;
    SkLabel8: TSkLabel;
    Rectangle39: TRectangle;
    SkSvg9: TSkSvg;
    Rectangle40: TRectangle;
    ListBoxItem7: TListBoxItem;
    Rectangle41: TRectangle;
    Layout26: TLayout;
    labTouchAngle_OffsetValue: TSkLabel;
    Rectangle42: TRectangle;
    SkSvg10: TSkSvg;
    Rectangle43: TRectangle;
    numTouchAngle_OffsetValue: TNumberBox;
    ListBoxItem8: TListBoxItem;
    Rectangle44: TRectangle;
    Layout27: TLayout;
    SkLabel10: TSkLabel;
    Rectangle45: TRectangle;
    SkSvg11: TSkSvg;
    Rectangle46: TRectangle;
    ctbStretch_OnX_SetAreaFrom: TComboTrackBar;
    ctbStretch_OnX_SetAreaTo: TComboTrackBar;
    ItemOnYSetArea: TListBoxItem;
    Rectangle50: TRectangle;
    Layout29: TLayout;
    SkLabel6: TSkLabel;
    Rectangle51: TRectangle;
    SkSvg13: TSkSvg;
    Rectangle52: TRectangle;
    ctbStretch_OnY_SetAreaFrom: TComboTrackBar;
    ctbStretch_OnY_SetAreaTo: TComboTrackBar;
    ItemCropOnXSetArea: TListBoxItem;
    Rectangle53: TRectangle;
    Layout30: TLayout;
    SkLabel12: TSkLabel;
    Rectangle54: TRectangle;
    SkSvg14: TSkSvg;
    Rectangle55: TRectangle;
    ctbCrop_OnX_SetAreaFrom: TComboTrackBar;
    ctbCrop_OnX_SetAreaTo: TComboTrackBar;
    ListBoxItem6: TListBoxItem;
    Rectangle56: TRectangle;
    Layout31: TLayout;
    SkLabel13: TSkLabel;
    Rectangle57: TRectangle;
    SkSvg15: TSkSvg;
    Rectangle58: TRectangle;
    ctbCrop_OnY_SetAreaFrom: TComboTrackBar;
    ctbCrop_OnY_SetAreaTo: TComboTrackBar;
    ListBoxItem2: TListBoxItem;
    Rectangle59: TRectangle;
    Layout32: TLayout;
    SkLabel9: TSkLabel;
    Rectangle60: TRectangle;
    SkSvg16: TSkSvg;
    Rectangle61: TRectangle;
    ctbMoveOnX: TComboTrackBar;
    ctbMoveOnY: TComboTrackBar;
    cbMove_OnStrictArea: TCheckBox;
    cbxMirrorOnY: TCheckBox;
    cbxMirrorOnX: TCheckBox;
    cbxMirrorOnMixMax: TCheckBox;
    ItemStrechHeader: TListBoxItem;
    Rectangle62: TRectangle;
    Rectangle63: TRectangle;
    SkSvg17: TSkSvg;
    SkLabel14: TSkLabel;
    Rectangle64: TRectangle;
    SkSvg18: TSkSvg;
    ItemCropHeader: TListBoxItem;
    Rectangle65: TRectangle;
    Rectangle66: TRectangle;
    SkSvg19: TSkSvg;
    SkLabel15: TSkLabel;
    Rectangle67: TRectangle;
    SkSvg20: TSkSvg;
    ListBoxItem3: TListBoxItem;
    Rectangle15: TRectangle;
    Rectangle16: TRectangle;
    SkSvg12: TSkSvg;
    SkLabel1: TSkLabel;
    Rectangle17: TRectangle;
    SkSvg21: TSkSvg;

    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnSelectFileClick(Sender: TObject);
    procedure btnSaveModelClick(Sender: TObject);
    procedure btnSelectAFileNameClick(Sender: TObject);
    procedure HeaderIconClick(Sender: TObject);
    procedure HelpIconClick(Sender: TObject);
    procedure svgEksenParametreleriArrowClick(Sender: TObject);
    procedure svgPasoKesmeParametreleriArrowClick(Sender: TObject);
    procedure svgHizParametreleriArrowClick(Sender: TObject);
    procedure svgIslemParametreleriArrowClick(Sender: TObject);
    procedure svgGeometriParametreleriArrowClick(Sender: TObject);
    procedure svgAciParametreleriArrowClick(Sender: TObject);
    procedure svgColorHeaderClick(Sender: TObject);
    procedure svgHelpTypeOfAxisClick(Sender: TObject);
    procedure svgHelpPointSplitTypeClick(Sender: TObject);
    procedure svgHelpPointSplitValueClick(Sender: TObject);
    procedure svgHelpMillingStepValueClick(Sender: TObject);
    procedure svgHelpMillingStepCountClick(Sender: TObject);
    procedure svgHelpMillingFinishClick(Sender: TObject);
    procedure svgHelpPointCountInMMClick(Sender: TObject);
    procedure svgHelpAFileNameClick(Sender: TObject);
    procedure svgHelpHeaderOfProgramClick(Sender: TObject);
    procedure svgHelpFooterOfProgramClick(Sender: TObject);
    procedure svgHelpHeaderOfAxisClick(Sender: TObject);
    procedure svgHelpFooterOfAxisClick(Sender: TObject);
    procedure svgHelpMovementHeaderOfAxisClick(Sender: TObject);
    procedure svgHelpMovementFooterOfAxisClick(Sender: TObject);
    procedure svgHelpRadiusOfPartClick(Sender: TObject);
    procedure svgHelpRadiusOfProjectionClick(Sender: TObject);
    procedure svgHelpWidthOfPartClick(Sender: TObject);
    procedure svgHelpStartXClick(Sender: TObject);
    procedure svgHelpEndXClick(Sender: TObject);
    procedure svgHelpDeviationXClick(Sender: TObject);
    procedure svgHelpDeviationZClick(Sender: TObject);
    procedure svgHelpLengthOfTemplateClick(Sender: TObject);
    procedure svgHelpMinAngleOfTouchClick(Sender: TObject);
    procedure svgHelpMaxAngleOfTouchClick(Sender: TObject);
    procedure svgHelpAngleOfArmClick(Sender: TObject);
    procedure svgHelpColorOfPartClick(Sender: TObject);
    procedure svgHelpColorOfProjectionClick(Sender: TObject);

  private
    FRequest: TVerticalPartRequest;
    FAxisGroupExpanded: Boolean;
    FStepGroupExpanded: Boolean;
    FSpeedGroupExpanded: Boolean;
    FProcessGroupExpanded: Boolean;
    FGeometryGroupExpanded: Boolean;
    FAngleGroupExpanded: Boolean;
    FColorGroupExpanded: Boolean;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';

    procedure InitializeForm;
    procedure LoadSVGIcons;
    procedure ToggleAxisGroup;
    procedure ToggleStepGroup;
    procedure ToggleSpeedGroup;
    procedure ToggleProcessGroup;
    procedure ToggleGeometryGroup;
    procedure ToggleAngleGroup;
    procedure ToggleColorGroup;
    procedure LoadModelToForm;
    procedure SaveFormToModel;
    procedure SetGroupItemsVisible(const GroupName: string; Visible: Boolean);
    
  public
    property Request: TVerticalPartRequest read FRequest write FRequest;
  end;

var
  FrmVerticalPartGCodeGenerator: TFrmVerticalPartGCodeGenerator;

implementation

{$R *.fmx}

procedure TFrmVerticalPartGCodeGenerator.FormCreate(Sender: TObject);
begin
  FRequest := TVerticalPartRequest.Create;
  InitializeForm;
  LoadSVGIcons;
  LoadModelToForm;
  
  // Baþlangýçta tüm gruplarý geniþletilmiþ olarak ayarla
  FAxisGroupExpanded := True;
  FStepGroupExpanded := True;
  FSpeedGroupExpanded := True;
  FProcessGroupExpanded := True;
  FGeometryGroupExpanded := True;
  FAngleGroupExpanded := True;
  FColorGroupExpanded := True;
end;

procedure TFrmVerticalPartGCodeGenerator.FormDestroy(Sender: TObject);
begin
  if Assigned(FRequest) then
  FreeAndNil(FRequest);
end;

procedure TFrmVerticalPartGCodeGenerator.InitializeForm;
begin
  // Form ayarlarý
  Fill.Color := TAlphaColorRec.Black;
  
  // ListBox ayarlarý
  lstParameters.ShowScrollBars := True;
  lstParameters.StyleLookup := 'listboxstyle';
  lstParameters.ItemHeight := 35;
//  lstParameters.Fill.Color := TAlphaColorRec.Black;

  // ComboBox items'larýný doldur
  cbxTypeOfAxis.Items.Clear;
  cbxTypeOfAxis.Items.Add('X Ekseni');
  cbxTypeOfAxis.Items.Add('Y Ekseni');
  cbxTypeOfAxis.Items.Add('Z Ekseni');
  cbxTypeOfAxis.Items.Add('C Ekseni');
  cbxTypeOfAxis.Items.Add('U Ekseni');
  cbxTypeOfAxis.ItemIndex := 0;
  
  cbxPointSplitType.Items.Clear;
  cbxPointSplitType.Items.Add('Eþit Mesafe');
  cbxPointSplitType.Items.Add('Eþit Açý');
  cbxPointSplitType.Items.Add('Adaptif');
  cbxPointSplitType.ItemIndex := 0;

  // NumberBox varsayýlan deðerleri
  numRadiusOfPart.Value := 12.4;
  numRadiusOfProjection.Value := 8.0;
  numWidthOfPart.Value := 25.0;
  numStartXOfProjection.Value := 0.0;
  numEndXOfProjection.Value := 100.0;
  numMinLimitOfTouchAngle.Value := 0.0;
  numMaxLimitOfTouchAngle.Value := 360.0;
  numAngleOfArmRef.Value := 45.0;
//  numPointCountInMM.Value := 10.0;
  numXOfDeviation.Value := 0.0;
  numZOfDeviation.Value := 0.0;
  numLengthOfTemplate.Value := 100.0;
  numMillingStepValue.Value := 0.5;
  numMillingStepCount.Value := 5.0;
  numMillingFinish.Value := 0.1;
  numPointSplitValue.Value := 1.0;
  
  // Edit varsayýlan deðerleri
  edtHeaderOfProgram.Text := 'G90 G21';
  edtFooterOfProgram.Text := 'M30';
  edtHeaderOfAxis.Text := '';
  edtFooterOfAxis.Text := '';
  edtMovementHeaderOfAxis.Text := '';
  edtMovementFooterOfAxis.Text := '';
  edtColorOfPart.Color :=  TAlphaColorRec.White;
  edtColorOfProjection.Color := TAlphaColorRec.Alpha;
  edtAFileName.Text := '';
end;

procedure TFrmVerticalPartGCodeGenerator.LoadSVGIcons;
const
//  SVG_AXIS = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 2L13.09 8.26L22 9L13.09 9.74L12 16L10.91 9.74L2 9L10.91 8.26L12 2Z"/></svg>';
//  SVG_STEP = '<svg viewBox="0 0 24 24" fill="white"><path d="M3 17V19H9V17H3M3 5V7H13V5H3M13 21V19H21V17H13V15H11V21H13M7 9V11H3V13H7V15H9V9H7M21 13V11H11V13H21M15 9H17V7H19V5H17V3H15V5H13V7H15V9Z"/></svg>';
//  SVG_SPEED = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 16C13.66 16 15 14.66 15 13C15 11.34 13.66 10 12 10C10.34 10 9 11.34 9 13C9 14.66 10.34 16 12 16M12 2C17.52 2 22 6.48 22 12C22 17.52 17.52 22 12 22C6.48 22 2 17.52 2 12C2 6.48 6.48 2 12 2Z"/></svg>';
//  SVG_PROCESS = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 2C13.1 2 14 2.9 14 4C14 5.1 13.1 6 12 6C10.9 6 10 5.1 10 4C10 2.9 10.9 2 12 2M21 9V7L15 1L13.5 2.5L16.17 5.17L10.58 10.76C10.22 10.54 9.8 10.4 9.35 10.35L8.8 9.8L12 6.6L10.6 5.2L7.4 8.4L8.05 9.05C7.9 9.5 7.9 10 8.05 10.45L2 16.5V18.5L4 20.5H6L12.05 14.45C12.5 14.6 13 14.6 13.45 14.45L14.1 15.1L17.3 11.9L15.9 10.5L12.7 13.7L12.15 13.15C12.1 12.7 11.96 12.28 11.74 11.92L17.33 6.33L20 9H22V7L21 9Z"/></svg>';
//  SVG_GEOMETRY = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 2L2 7L12 12L22 7L12 2M2 17L12 22L22 17M2 12L12 17L22 12"/></svg>';
//  SVG_ANGLE = '<svg viewBox="0 0 24 24" fill="white"><path d="M12 2C6.48 2 2 6.48 2 12S6.48 22 12 22 22 17.52 22 12 17.52 2 12 2M12 20C7.59 20 4 16.41 4 12S7.59 4 12 4 20 7.59 20 12 16.41 20 12 20M12.5 7V12.25L17 14.92L16.25 16.15L11 13V7H12.5Z"/></svg>';
//  SVG_COLOR = '<svg viewBox="0 0 24 24" fill="white"><path d="M17.5 12C17.5 16.14 14.36 19.5 10.5 19.5C6.64 19.5 3.5 16.14 3.5 12S6.64 4.5 10.5 4.5C14.36 4.5 17.5 7.86 17.5 12M10.5 6C7.74 6 5.5 8.24 5.5 11S7.74 16 10.5 16 15.5 13.76 15.5 11 13.26 6 10.5 6Z"/></svg>';
//  SVG_HELP = '<svg viewBox="0 0 24 24" fill="white"><path d="M11,18H13V16H11V18M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,6A4,4 0 0,0 8,10H10A2,2 0 0,1 12,8A2,2 0 0,1 14,10C14,12 11,11.75 11,15H13C13,12.75 16,12.5 16,10A4,4 0 0,0 12,6Z"/></svg>';
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';

//  GEAR_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,15.5A3.5,3.5 0 0,1 8.5,12A3.5,3.5 0 0,1 12,8.5A3.5,3.5 0 0,1 15.5,12A3.5,3.5 0 0,1 12,15.5M19.43,12.97C19.47,12.65 19.5,12.33 19.5,12C19.5,11.67 19.47,11.34 19.43,11L21.54,9.37C21.73,9.22 21.78,8.95 21.66,8.73L19.66,5.27C19.54,5.05 19.27,4.96 19.05,5.05L16.56,6.05C16.04,5.66 15.5,5.32 14.87,5.07L14.5,2.42C14.46,2.18 14.25,2 14,2H10C9.75,2 9.54,2.18 9.5,2.42L9.13,5.07C8.5,5.32 7.96,5.66 7.44,6.05L4.95,5.05C4.73,4.96 4.46,5.05 4.34,5.27L2.34,8.73C2.22,8.95 2.27,9.22 2.46,9.37L4.57,11C4.53,11.34 4.5,11.67 4.5,12C4.5,12.33 4.53,12.65 4.57,12.97L2.46,14.63C2.27,14.78 2.22,15.05 2.34,15.27L4.34,18.73C4.46,18.95 4.73,19.03 4.95,18.95L7.44,17.94C7.96,18.34 8.5,18.68 9.13,18.93L9.5,21.58C9.54,21.82 9.75,22 10,22H14C14.25,22 14.46,21.82 14.5,21.58L14.87,18.93C15.5,18.68 16.04,18.34 16.56,17.94L19.05,18.95C19.27,19.03 19.54,18.95 19.66,18.73L21.66,15.27C21.78,15.05 21.73,14.78 21.54,14.63L19.43,12.97Z"/></svg>';
//  SPEED_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,16A3,3 0 0,1 9,13C9,11.88 9.61,10.9 10.5,10.39L20.21,4.77L14.68,14.35C14.18,15.33 13.17,16 12,16M12,3C13.81,3 15.5,3.5 16.97,4.32L14.87,5.53C14,5.19 13,5 12,5A8,8 0 0,0 4,13C4,15.21 4.89,17.21 6.34,18.65H6.35C6.74,19.04 6.74,19.67 6.35,20.06C5.96,20.45 5.33,20.45 4.94,20.06C3.1,18.22 2,15.76 2,13A10,10 0 0,1 12,3M22,13C22,15.76 20.9,18.22 19.06,20.06C18.67,20.45 18.04,20.45 17.65,20.06C17.26,19.67 17.26,19.04 17.65,18.65C19.11,17.21 20,15.21 20,13C20,12 19.81,11 19.47,10.13L20.68,8.03C21.5,9.5 22,11.19 22,13Z"/></svg>';
//  PROCESS_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M17,12C17,14.42 15.28,16.44 13,16.9V21H11V16.9C8.72,16.44 7,14.42 7,12C7,9.58 8.72,7.56 11,7.1V3H13V7.1C15.28,7.56 17,9.58 17,12M12,9A3,3 0 0,0 9,12A3,3 0 0,0 12,15A3,3 0 0,0 15,12A3,3 0 0,0 12,9Z"/></svg>';
//  GEOMETRY_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M12,4A8,8 0 0,1 20,12A8,8 0 0,1 12,20A8,8 0 0,1 4,12A8,8 0 0,1 12,4M12,6A6,6 0 0,0 6,12A6,6 0 0,0 12,18A6,6 0 0,0 18,12A6,6 0 0,0 12,6M12,8A4,4 0 0,1 16,12A4,4 0 0,1 12,16A4,4 0 0,1 8,12A4,4 0 0,1 12,8Z"/></svg>';
//  ANGLE_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M2.5,4L20.5,6L19,8L4,7.5L2.5,4M5.5,9L18.5,10L17,12L8,11.5L5.5,9M8.5,14L15.5,14.5L14,16.5L11,16L8.5,14M11.5,19L12.5,19.5L11,21.5L11.5,19Z"/></svg>';
//  INFO_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M11,9H13V7H11M12,20C7.59,20 4,16.41 4,12C4,7.59 7.59,4 12,4C16.41,4 20,7.59 20,12C20,16.41 16.41,20 12,20M12,2A10,10 0 0,0 2,12A10,10 0 0,0 12,22A10,10 0 0,0 22,12A10,10 0 0,0 12,2M11,17H13V11H11V17Z"/></svg>';
  ARROW_DOWN_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  ARROW_UP_SVG = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,14L12,9L17,14H7Z"/></svg>';

  begin
  // Header ikonlarý
//  svgAxisHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgStepHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgSpeedHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgProcessHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgGeometryHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgAngleHeader.Svg.Source := SVG_ARROW_DOWN;
//  svgColorHeader.Svg.Source := SVG_ARROW_DOWN;
////


//  // Help ikonlarý
//  svgHelpTypeOfAxis.Svg.Source := SVG_HELP;
////  svgHelpPointSplitType.Svg.Source := SVG_HELP;
////  svgHelpPointSplitValue.Svg.Source := SVG_HELP;
//   svgHelpMillingStepValue.Svg.Source := SVG_HELP;
//  svgHelpMillingStepCount.Svg.Source := SVG_HELP;
//  svgHelpMillingFinish.Svg.Source := SVG_HELP;
//  svgHelpPointCountInMM.Svg.Source := SVG_HELP;
//  svgHelpAFileName.Svg.Source := SVG_HELP;
//  svgHelpHeaderOfProgram.Svg.Source := SVG_HELP;
//  svgHelpFooterOfProgram.Svg.Source := SVG_HELP;
//  svgHelpHeaderOfAxis.Svg.Source := SVG_HELP;
//  svgHelpFooterOfAxis.Svg.Source := SVG_HELP;
//  svgHelpMovementHeaderOfAxis.Svg.Source := SVG_HELP;
//  svgHelpMovementFooterOfAxis.Svg.Source := SVG_HELP;
//  svgHelpRadiusOfPart.Svg.Source := SVG_HELP;
//  svgHelpRadiusOfProjection.Svg.Source := SVG_HELP;
//  svgHelpWidthOfPart.Svg.Source := SVG_HELP;
//  svgStartXOfProjectionInfo.Svg.Source := SVG_HELP;
//  svgEndXOfProjectionInfo.Svg.Source := SVG_HELP;
//  svgXOfDeviationInfo.Svg.Source := SVG_HELP;
//  svgZOfDeviationInfo.Svg.Source := SVG_HELP;
//  svgLengthOfTemplateInfo.Svg.Source := SVG_HELP;
//  svgMinLimitOfTouchAngleInfo.Svg.Source := SVG_HELP;
//  svgMaxLimitOfTouchAngleInfo.Svg.Source := SVG_HELP;
//  svgAngleOfArmRefInfo.Svg.Source := SVG_HELP;
//  svgColorHeader.Svg.Source := SVG_HELP;
//  svgHelpColorOfProjection.Svg.Source := SVG_HELP;
//
// // Grup ikonlarý
//  svgEksenParametreleriIcon.Svg.Source := GEAR_SVG;
//  svgPasoKesmeParametreleriIcon.Svg.Source := PROCESS_SVG;
//  svgHizParametreleriIcon.Svg.Source := SPEED_SVG;
//  svgIslemParametreleriIcon.Svg.Source := PROCESS_SVG;
//  svgGeometriParametreleriIcon.Svg.Source := GEOMETRY_SVG;
//  svgAciParametreleriIcon.Svg.Source := ANGLE_SVG;
//
//  // Ok ikonlarý
//  svgEksenParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;
//  svgPasoKesmeParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;
//  svgHizParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;
//  svgIslemParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;
//  svgGeometriParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;
//  svgAciParametreleriArrow.Svg.Source := ARROW_DOWN_SVG;

end;

procedure TFrmVerticalPartGCodeGenerator.LoadModelToForm;
begin
  if not Assigned(FRequest) then Exit;

  // Model verilerini form kontrollerine yükle
  // Bu kýsým model yapýsýna göre doldurulacak
end;

procedure TFrmVerticalPartGCodeGenerator.SaveFormToModel;
begin

  if not Assigned(FRequest) then Exit;

  // Eksen Parametreleri
  FRequest.Detail.TypeOfAxis := TAxisType(cbxTypeOfAxis.ItemIndex);
  FRequest.Detail.AngleOfArmRef := numAngleOfArmRef.Value;

  // Paso Kesme Parametreleri
  FRequest.Detail.PointSplitType := TPointSplitType(cbxPointSplitType.ItemIndex);
  FRequest.Detail.PointSplitValue := numPointSplitValue.Value;

  // Hýz Parametreleri
  FRequest.Detail.SpeedOfMovement := numSpeedOfMovement.Value;
  FRequest.Detail.SpeedOfMilling := numSpeedOfMilling.Value;
  FRequest.Detail.SpeedOfApproach := numSpeedOfApproach.Value;

  // Ýþlem Parametreleri
  FRequest.Detail.LimitForGapOfTouchAngle := numMinLimitOfTouchAngle.Value;
  FRequest.Detail.SafetyDistance := numSafetyDistance.Value;
  FRequest.Detail.ApproachDistance := numApproachDistance.Value;

  // Geometri Parametreleri
  FRequest.WidthOfPart := numWidthOfPart.Value;
  FRequest.RadiusOfPart := numRadiusOfPart.Value;
  FRequest.RadiusOfProjection := numRadiusOfProjection.Value;
  FRequest.Detail.StartXOfProjection := numStartXOfProjection.Value;
  FRequest.Detail.EndXOfProjection := numEndXOfProjection.Value;
  FRequest.XOfDeviation := numXOfDeviation.Value;
  FRequest.ZOfDeviation := numZOfDeviation.Value;
  FRequest.LengthOfTemplate := numLengthOfTemplate.Value;

  // Açý Parametreleri
  FRequest.Detail.MinLimitOfTouchAngle := numMinLimitOfTouchAngle.Value;
  FRequest.Detail.MaxLimitOfTouchAngle := numMaxLimitOfTouchAngle.Value;
//  FRequest.Detail.IsContinuousAngle := chkIsContinuousAngle.IsChecked;
//  FRequest.Detail.IsStrictDirection := chkIsStrictDirection.IsChecked;
//  FRequest.Detail.IsDiscrete := chkIsDiscrete.IsChecked;
//  FRequest.Detail.IsReversedAngleOfTemplate := chkIsReversedAngleOfTemplate.IsChecked;

  // Dosya
//  FRequest.Detail.AFileName := edtAFileName.Text;
end;

procedure TFrmVerticalPartGCodeGenerator.btnSelectFileClick(Sender: TObject);
begin
  if odDXF.Execute then
  begin
    edtAFileName.Text := odDXF.FileName;
  end;
end;

procedure TFrmVerticalPartGCodeGenerator.btnSelectAFileNameClick(Sender: TObject);
begin
  if odDXF.Execute then
  begin
    edtAFileName.Text := odDXF.FileName;
  end;
end;

procedure TFrmVerticalPartGCodeGenerator.btnSaveModelClick(Sender: TObject);
begin
  SaveFormToModel;
  ShowMessage('Model kaydedildi.');
end;

// Header Click Events
procedure TFrmVerticalPartGCodeGenerator.svgEksenParametreleriArrowClick(Sender: TObject);
begin
  ToggleAxisGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgPasoKesmeParametreleriArrowClick(Sender: TObject);
begin
  ToggleStepGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgHizParametreleriArrowClick(Sender: TObject);
begin
  ToggleSpeedGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgIslemParametreleriArrowClick(Sender: TObject);
begin
  ToggleProcessGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgGeometriParametreleriArrowClick(Sender: TObject);
begin
  ToggleGeometryGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgAciParametreleriArrowClick(Sender: TObject);
begin
  ToggleAngleGroup;
end;

procedure TFrmVerticalPartGCodeGenerator.svgColorHeaderClick(Sender: TObject);
begin
  ToggleColorGroup;
end;

// Toggle Group Methods
procedure TFrmVerticalPartGCodeGenerator.ToggleAxisGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FAxisGroupExpanded := not FAxisGroupExpanded;
  
  lItemTypeOfAxis.Visible := FAxisGroupExpanded;
  lItemPointSplitType.Visible := FAxisGroupExpanded;
  lItemPointSplitValue.Visible := FAxisGroupExpanded;
  
  if FAxisGroupExpanded then
    svgEksenParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgEksenParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.ToggleStepGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
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

procedure TFrmVerticalPartGCodeGenerator.ToggleSpeedGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FSpeedGroupExpanded := not FSpeedGroupExpanded;
  
  lItemPointCountInMM.Visible := FSpeedGroupExpanded;

  if FSpeedGroupExpanded then
    svgHizParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgHizParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.ToggleProcessGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FProcessGroupExpanded := not FProcessGroupExpanded;
  
  lItemAFileName.Visible := FProcessGroupExpanded;
  lItemHeaderOfProgram.Visible := FProcessGroupExpanded;
  lItemFooterOfProgram.Visible := FProcessGroupExpanded;
  lItemHeaderOfAxis.Visible := FProcessGroupExpanded;
  lItemFooterOfAxis.Visible := FProcessGroupExpanded;
  lItemMovementHeaderOfAxis.Visible := FProcessGroupExpanded;
  lItemMovementFooterOfAxis.Visible := FProcessGroupExpanded;
  
  if FProcessGroupExpanded then
    svgIslemParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgIslemParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.ToggleGeometryGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FGeometryGroupExpanded := not FGeometryGroupExpanded;
  
  lItemRadiusOfPart.Visible := FGeometryGroupExpanded;
  lItemRadiusOfProjection.Visible := FGeometryGroupExpanded;
  lItemWidthOfPart.Visible := FGeometryGroupExpanded;
  lItemStartX.Visible := FGeometryGroupExpanded;
  lItemEndX.Visible := FGeometryGroupExpanded;
  lItemDeviationX.Visible := FGeometryGroupExpanded;
  lItemDeviationZ.Visible := FGeometryGroupExpanded;
  lItemLengthOfTemplate.Visible := FGeometryGroupExpanded;
  
  if FGeometryGroupExpanded then
    svgGeometriParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgGeometriParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.ToggleAngleGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FAngleGroupExpanded := not FAngleGroupExpanded;
  
  lItemMinAngleOfTouch.Visible := FAngleGroupExpanded;
  lItemMaxAngleOfTouch.Visible := FAngleGroupExpanded;
  lItemAngleOfArm.Visible := FAngleGroupExpanded;
  
  if FAngleGroupExpanded then
    svgAciParametreleriArrow.Svg.Source := SVG_ARROW_DOWN
  else
    svgAciParametreleriArrow.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.ToggleColorGroup;
const
  SVG_ARROW_DOWN = '<svg viewBox="0 0 24 24" fill="white"><path d="M7,10L12,15L17,10H7Z"/></svg>';
  SVG_ARROW_RIGHT = '<svg viewBox="0 0 24 24" fill="white"><path d="M8.59,16.58L13.17,12L8.59,7.41L10,6L16,12L10,18L8.59,16.58Z"/></svg>';
begin
  FColorGroupExpanded := not FColorGroupExpanded;
  
  lItemColorOfPart.Visible := FColorGroupExpanded;
  lItemColorOfProjection.Visible := FColorGroupExpanded;
  
  if FColorGroupExpanded then
    svgColorHeader.Svg.Source := SVG_ARROW_DOWN
  else
    svgColorHeader.Svg.Source := SVG_ARROW_RIGHT;
end;

procedure TFrmVerticalPartGCodeGenerator.SetGroupItemsVisible(const GroupName: string; Visible: Boolean);
begin
  // Bu method gerekirse kullanýlabilir
end;

procedure TFrmVerticalPartGCodeGenerator.HeaderIconClick(Sender: TObject);
begin
  // Genel header click handler
end;

procedure TFrmVerticalPartGCodeGenerator.HelpIconClick(Sender: TObject);
begin
  // Genel help click handler
end;

// Help Click Events
procedure TFrmVerticalPartGCodeGenerator.svgHelpTypeOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Türü: Ýþlenecek parçanýn hangi eksende döneceðini belirler.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpPointSplitTypeClick(Sender: TObject);
begin
  ShowMessage('Nokta Bölme Türü: Parçanýn nasýl noktalara bölüneceðini belirler.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpPointSplitValueClick(Sender: TObject);
begin
  ShowMessage('Nokta Bölme Deðeri: Bölme iþleminde kullanýlacak deðer.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMillingStepValueClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Paso Deðeri: Her paso için kesim derinliði (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMillingStepCountClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Paso Sayýsý: Toplam paso sayýsý.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMillingFinishClick(Sender: TObject);
begin
  ShowMessage('Frezeleme Finish: Son paso için kesim derinliði (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpPointCountInMMClick(Sender: TObject);
begin
  ShowMessage('MM Baþýna Nokta Sayýsý: Her milimetre için kaç nokta kullanýlacaðý.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpAFileNameClick(Sender: TObject);
begin
  ShowMessage('A Dosya Adý: Ýþlenecek DXF dosyasýnýn yolu.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpHeaderOfProgramClick(Sender: TObject);
begin
  ShowMessage('Program Baþlýðý: G-Code programýnýn baþýnda yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpFooterOfProgramClick(Sender: TObject);
begin
  ShowMessage('Program Sonu: G-Code programýnýn sonunda yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpHeaderOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Baþlýðý: Her eksen için baþlangýçta yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpFooterOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Sonu: Her eksen için sonunda yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMovementHeaderOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Hareket Baþlýðý: Her hareket öncesi yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMovementFooterOfAxisClick(Sender: TObject);
begin
  ShowMessage('Eksen Hareket Sonu: Her hareket sonrasý yer alacak komutlar.');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpRadiusOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Yarýçapý: Ýþlenecek parçanýn yarýçapý (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpRadiusOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Yarýçapý: Projeksiyon için kullanýlacak yarýçap (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpWidthOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Geniþliði: Ýþlenecek parçanýn geniþliði (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpStartXClick(Sender: TObject);
begin
  ShowMessage('Baþlangýç X: Ýþlemenin baþlayacaðý X koordinatý (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpEndXClick(Sender: TObject);
begin
  ShowMessage('Bitiþ X: Ýþlemenin biteceði X koordinatý (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpDeviationXClick(Sender: TObject);
begin
  ShowMessage('X Sapmasý: X eksenindeki sapma deðeri (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpDeviationZClick(Sender: TObject);
begin
  ShowMessage('Z Sapmasý: Z eksenindeki sapma deðeri (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpLengthOfTemplateClick(Sender: TObject);
begin
  ShowMessage('Þablon Uzunluðu: Kullanýlacak þablonun uzunluðu (mm).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMinAngleOfTouchClick(Sender: TObject);
begin
  ShowMessage('Minimum Temas Açýsý: Temas için minimum açý deðeri (derece).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpMaxAngleOfTouchClick(Sender: TObject);
begin
  ShowMessage('Maksimum Temas Açýsý: Temas için maksimum açý deðeri (derece).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpAngleOfArmClick(Sender: TObject);
begin
  ShowMessage('Kol Açýsý: Ýþleme kolunun açýsý (derece).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpColorOfPartClick(Sender: TObject);
begin
  ShowMessage('Parça Rengi: Görselleþtirmede kullanýlacak parça rengi (HEX).');
end;

procedure TFrmVerticalPartGCodeGenerator.svgHelpColorOfProjectionClick(Sender: TObject);
begin
  ShowMessage('Projeksiyon Rengi: Görselleþtirmede kullanýlacak projeksiyon rengi (HEX).');
end;

end.
