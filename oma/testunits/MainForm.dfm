object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'Geometry Generator - 2 Stacked Cubes (Shared Face)'
  ClientHeight = 875
  ClientWidth = 1500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 120
  TextHeight = 22
  object PaintBox1: TPaintBox
    Left = 317
    Top = 41
    Width = 804
    Height = 728
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alClient
    OnMouseDown = PaintBox1MouseDown
    OnMouseMove = PaintBox1MouseMove
    OnMouseUp = PaintBox1MouseUp
    OnPaint = PaintBox1Paint
  end
  object splLeft: TSplitter
    Left = 313
    Top = 41
    Width = 4
    Height = 728
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    MinSize = 38
  end
  object splRight: TSplitter
    Left = 1121
    Top = 41
    Width = 4
    Height = 728
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    MinSize = 38
  end
  object splBottom: TSplitter
    Left = 0
    Top = 769
    Width = 1500
    Height = 5
    Cursor = crVSplit
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    MinSize = 38
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 774
    Width = 1500
    Height = 101
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alBottom
    Caption = 'pnlBottom'
    TabOrder = 2
    object lblSpeed: TLabel
      Left = 10
      Top = 14
      Width = 108
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Speed Factor:'
    end
    object lblOscillation: TLabel
      Left = 363
      Top = 14
      Width = 86
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Oscillation:'
    end
    object trkSpeed: TTrackBar
      Left = 100
      Top = 1
      Width = 250
      Height = 56
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 500
      Min = 10
      Position = 100
      TabOrder = 0
      ThumbLength = 25
      OnChange = trkSpeedChange
    end
    object trkOscillation: TTrackBar
      Left = 450
      Top = 3
      Width = 250
      Height = 56
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Max = 50
      Position = 10
      TabOrder = 1
      ThumbLength = 25
      OnChange = trkOscillationChange
    end
    object btnStartAnim: TButton
      Left = 536
      Top = 33
      Width = 150
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Start Animation'
      TabOrder = 2
      OnClick = btnStartAnimClick
    end
    object btnStopAnim: TButton
      Left = 708
      Top = 33
      Width = 150
      Height = 32
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Stop Animation'
      TabOrder = 3
      OnClick = btnStopAnimClick
    end
    object chkOMA: TCheckBox
      Left = 708
      Top = 3
      Width = 106
      Height = 21
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Use OMA'
      TabOrder = 4
    end
    object StatusBar1: TStatusBar
      Left = 1
      Top = 73
      Width = 1498
      Height = 27
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Panels = <>
      SimplePanel = True
    end
  end
  object pnlLeft: TPanel
    Left = 0
    Top = 41
    Width = 313
    Height = 728
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alLeft
    Caption = 'PanelLeft'
    TabOrder = 0
    object lblObjectName: TLabel
      Left = 10
      Top = 613
      Width = 108
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Object Name:'
    end
    object lblGlobalCoords: TLabel
      Left = 10
      Top = 675
      Width = 105
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Global X/Y/Z:'
    end
    object lblGlobalX: TLabel
      Left = 10
      Top = 698
      Width = 33
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'X=0'
    end
    object lblGlobalY: TLabel
      Left = 80
      Top = 698
      Width = 33
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Y=0'
    end
    object lblGlobalZ: TLabel
      Left = 150
      Top = 698
      Width = 33
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Z=0'
    end
    object grdCoordinates: TStringGrid
      Left = 10
      Top = 60
      Width = 288
      Height = 175
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      DefaultColWidth = 80
      DefaultRowHeight = 30
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 0
      OnMouseUp = grdCoordinatesMouseUp
      OnSetEditText = grdCoordinatesSetEditText
    end
    object btnAddNodeLeft: TButton
      Left = 10
      Top = 245
      Width = 125
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Add Node'
      TabOrder = 1
      OnClick = btnAddNodeLeftClick
    end
    object btnDeleteNodeLeft: TButton
      Left = 145
      Top = 245
      Width = 125
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Delete'
      TabOrder = 2
      OnClick = btnDeleteNodeLeftClick
    end
    object btnConnectNodesLeft: TButton
      Left = 10
      Top = 285
      Width = 260
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Connect'
      TabOrder = 3
      OnClick = btnConnectNodesLeftClick
    end
    object btnClearAll: TButton
      Left = 10
      Top = 325
      Width = 260
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Clear All'
      TabOrder = 4
      OnClick = btnClearAllClick
    end
    object grdModeData: TStringGrid
      Left = 10
      Top = 375
      Width = 288
      Height = 225
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ColCount = 7
      DefaultColWidth = 50
      DefaultRowHeight = 30
      FixedCols = 0
      Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
      TabOrder = 5
      OnMouseUp = grdModeDataMouseUp
    end
    object edtObjectName: TEdit
      Left = 10
      Top = 633
      Width = 288
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 6
      Text = 'Object1'
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1500
    Height = 41
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 3
    object ToolBar1: TToolBar
      AlignWithMargins = True
      Left = 5
      Top = 5
      Width = 1490
      Height = 53
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      ButtonHeight = 30
      ButtonWidth = 49
      ShowCaptions = True
      TabOrder = 0
      object btnNew: TToolButton
        Left = 0
        Top = 0
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'New'
        OnClick = btnNewClick
      end
      object btnOpen: TToolButton
        Left = 49
        Top = 0
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Open'
        OnClick = btnOpenClick
      end
      object btnSave: TToolButton
        Left = 98
        Top = 0
        Margins.Left = 4
        Margins.Top = 4
        Margins.Right = 4
        Margins.Bottom = 4
        Caption = 'Save'
        OnClick = btnSaveClick
      end
    end
  end
  object pnlOMA: TPanel
    Left = 1125
    Top = 41
    Width = 375
    Height = 728
    Margins.Left = 4
    Margins.Top = 4
    Margins.Right = 4
    Margins.Bottom = 4
    Align = alRight
    Caption = 'OMA Data'
    TabOrder = 1
    object lblSelectMode: TLabel
      Left = 10
      Top = 10
      Width = 101
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Select Mode:'
    end
    object lblFreq: TLabel
      Left = 10
      Top = 75
      Width = 126
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Frequency (Hz):'
    end
    object lblDamp: TLabel
      Left = 125
      Top = 75
      Width = 76
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Damping:'
    end
    object lblApplyDone: TLabel
      Left = 130
      Top = 147
      Width = 46
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Done.'
      Visible = False
    end
    object lblAnalyzeDone: TLabel
      Left = 130
      Top = 227
      Width = 46
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Done.'
      Visible = False
    end
    object lblAttachDone: TLabel
      Left = 130
      Top = 267
      Width = 46
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Done.'
      Visible = False
    end
    object lblDetachDone: TLabel
      Left = 130
      Top = 307
      Width = 46
      Height = 22
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Done.'
      Visible = False
    end
    object cmbModeSelect: TComboBox
      Left = 10
      Top = 30
      Width = 150
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Style = csDropDownList
      TabOrder = 0
      OnChange = cmbModeSelectChange
      Items.Strings = (
        'Mode 0'
        'Mode 1')
    end
    object edtFreq: TEdit
      Left = 10
      Top = 95
      Width = 88
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 1
    end
    object edtDamp: TEdit
      Left = 125
      Top = 95
      Width = 88
      Height = 30
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      TabOrder = 2
    end
    object btnApplyOMA: TButton
      Left = 10
      Top = 140
      Width = 113
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Apply OMA'
      TabOrder = 3
      OnClick = btnApplyOMAClick
    end
    object btnRevertOMA: TButton
      Left = 10
      Top = 180
      Width = 113
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Revert OMA'
      TabOrder = 4
      OnClick = btnRevertOMAClick
    end
    object btnAnalyzeOMA: TButton
      Left = 10
      Top = 220
      Width = 113
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Analyze OMA'
      TabOrder = 5
      OnClick = btnAnalyzeOMAClick
    end
    object btnAttachOMA: TButton
      Left = 10
      Top = 260
      Width = 113
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Attach OMA'
      TabOrder = 6
      OnClick = btnAttachOMAClick
    end
    object btnDetachOMA: TButton
      Left = 10
      Top = 300
      Width = 113
      Height = 31
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Caption = 'Detach OMA'
      TabOrder = 7
      OnClick = btnDetachOMAClick
    end
    object chartOMA: TChart
      Left = 1
      Top = 367
      Width = 373
      Height = 360
      Margins.Left = 4
      Margins.Top = 4
      Margins.Right = 4
      Margins.Bottom = 4
      Title.Text.Strings = (
        'Mode Magnitude vs Frequency')
      View3D = False
      View3DOptions.FontZoom = 125
      View3DOptions.PenZoom = 125
      View3DOptions.CheckBoxZoom = 125
      Align = alBottom
      TabOrder = 8
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 0
      object serOMA: TLineSeries
        Brush.BackColor = clDefault
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        XValues.Name = 'X'
        XValues.Order = loNone
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
  object MainMenu1: TMainMenu
    Left = 1
    Top = 1
    object mnuFile: TMenuItem
      Caption = '&File'
      object mnuNew: TMenuItem
        Caption = '&New'
        OnClick = mnuNewClick
      end
      object mnuOpen: TMenuItem
        Caption = '&Open'
        OnClick = btnOpenClick
      end
      object mnuSave: TMenuItem
        Caption = '&Save'
        OnClick = btnSaveClick
      end
      object mnuExit: TMenuItem
        Caption = 'E&xit'
        OnClick = mnuExitClick
      end
    end
    object mnuEdit: TMenuItem
      Caption = '&Edit'
      object mnuDeleteNode: TMenuItem
        Caption = 'Delete &Node'
        OnClick = mnuDeleteNodeClick
      end
    end
    object mnuView: TMenuItem
      Caption = '&View'
      object mnuTogglePanel: TMenuItem
        Caption = 'Toggle &Control Panels'
        OnClick = mnuTogglePanelClick
      end
    end
    object mnuHelp: TMenuItem
      Caption = '&Help'
      object mnuAbout: TMenuItem
        Caption = '&About'
        OnClick = mnuAboutClick
      end
    end
  end
  object tmrAnimation: TTimer
    Enabled = False
    Interval = 16
    OnTimer = tmrAnimationTimer
    Left = 40
    Top = 80
  end
  object tmrHideDone: TTimer
    Enabled = False
    Interval = 800
    OnTimer = tmrHideDoneTimer
    Left = 120
    Top = 80
  end
  object ToolImages: TImageList
    Left = 200
    Top = 80
  end
end
