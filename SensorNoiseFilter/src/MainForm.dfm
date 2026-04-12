object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Sens'#246'r Verisi G'#252'r'#252'lt'#252' Temizleme'
  ClientHeight = 680
  ClientWidth = 1050
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object PanelTop: TPanel
    Left = 0
    Top = 0
    Width = 1050
    Height = 105
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LabelFile: TLabel
      Left = 16
      Top = 14
      Width = 57
      Height = 13
      Caption = 'CSV Dosya:'
    end
    object LabelFilter: TLabel
      Left = 16
      Top = 44
      Width = 29
      Height = 13
      Caption = 'Filtre:'
    end
    object LabelAxis: TLabel
      Left = 16
      Top = 74
      Width = 33
      Height = 13
      Caption = 'Eksen:'
    end
    object LabelParam1: TLabel
      Left = 370
      Top = 44
      Width = 63
      Height = 13
      Caption = 'Parametre 1:'
    end
    object LabelParam2: TLabel
      Left = 560
      Top = 44
      Width = 63
      Height = 13
      Caption = 'Parametre 2:'
    end
    object LabelSigma: TLabel
      Left = 370
      Top = 74
      Width = 91
      Height = 13
      Caption = 'G'#252'r'#252'lt'#252' Sigma (x):'
    end
    object EditFilePath: TEdit
      Left = 80
      Top = 11
      Width = 350
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object BtnLoadCSV: TButton
      Left = 436
      Top = 9
      Width = 90
      Height = 25
      Caption = 'CSV Y'#252'kle...'
      TabOrder = 1
      OnClick = BtnLoadCSVClick
    end
    object ComboFilter: TComboBox
      Left = 80
      Top = 41
      Width = 275
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = ComboFilterChange
    end
    object ComboAxis: TComboBox
      Left = 80
      Top = 71
      Width = 120
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 3
      Text = 'X Ekseni'
      OnChange = ComboAxisChange
      Items.Strings = (
        'X Ekseni'
        'Y Ekseni'
        'Z Ekseni')
    end
    object EditParam1: TEdit
      Left = 445
      Top = 41
      Width = 100
      Height = 21
      TabOrder = 4
      Text = '5'
    end
    object EditParam2: TEdit
      Left = 635
      Top = 41
      Width = 100
      Height = 21
      TabOrder = 5
      Text = '0.3'
    end
    object BtnApplyFilter: TButton
      Left = 750
      Top = 39
      Width = 120
      Height = 25
      Caption = 'Filtre Uygula'
      TabOrder = 6
      OnClick = BtnApplyFilterClick
    end
    object EditSigma: TEdit
      Left = 460
      Top = 71
      Width = 60
      Height = 21
      TabOrder = 7
      Text = '2.0'
    end
    object BtnDetectNoise: TButton
      Left = 530
      Top = 69
      Width = 120
      Height = 25
      Caption = 'G'#252'r'#252'lt'#252' Tespit Et'
      TabOrder = 8
      OnClick = BtnDetectNoiseClick
    end
    object BtnExportFiltered: TButton
      Left = 885
      Top = 39
      Width = 150
      Height = 25
      Caption = 'Filtreli Veriyi Kaydet...'
      TabOrder = 9
      OnClick = BtnExportFilteredClick
    end
    object CheckShowRaw: TCheckBox
      Left = 220
      Top = 74
      Width = 70
      Height = 17
      Caption = 'Ham Veri'
      Checked = True
      State = cbChecked
      TabOrder = 10
      OnClick = CheckShowRawClick
    end
    object CheckShowFiltered: TCheckBox
      Left = 295
      Top = 74
      Width = 65
      Height = 17
      Caption = 'Filtreli'
      Checked = True
      State = cbChecked
      TabOrder = 11
      OnClick = CheckShowFilteredClick
    end
    object CheckDrawGraph: TCheckBox
      Left = 660
      Top = 74
      Width = 115
      Height = 17
      Caption = 'Grafik '#199'iz'
      Checked = True
      State = cbChecked
      TabOrder = 12
      OnClick = CheckDrawGraphClick
    end
    object BtnAutoEstimateNoise: TButton
      Left = 782
      Top = 69
      Width = 130
      Height = 25
      Caption = 'Otomatik Tahmin'
      TabOrder = 13
      OnClick = BtnAutoEstimateNoiseClick
    end
    object CheckShowAllAxes: TCheckBox
      Left = 920
      Top = 74
      Width = 125
      Height = 17
      Caption = 'T'#252'm Eksenleri G'#246'ster'
      TabOrder = 14
      OnClick = CheckShowAllAxesClick
    end
  end
  object PanelGraph: TPanel
    Left = 0
    Top = 105
    Width = 1050
    Height = 430
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object PaintBoxGraph: TSkPaintBox
      Left = 0
      Top = 0
      Width = 1050
      Height = 430
      Align = alClient
      OnMouseDown = PaintBoxGraphMouseDown
      OnMouseMove = PaintBoxGraphMouseMove
      OnMouseUp = PaintBoxGraphMouseUp
      OnDraw = PaintBoxGraphDraw
    end
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 535
    Width = 1050
    Height = 145
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    object PanelStats: TPanel
      Left = 0
      Top = 0
      Width = 1050
      Height = 120
      Align = alTop
      BevelOuter = bvLowered
      TabOrder = 0
      object LabelStatsTitle: TLabel
        Left = 16
        Top = 8
        Width = 82
        Height = 13
        Caption = #304'statistik Bilgiler'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object LabelRawStats: TLabel
        Left = 16
        Top = 30
        Width = 55
        Height = 13
        Caption = 'Ham Veri: -'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object LabelFilteredStats: TLabel
        Left = 16
        Top = 50
        Width = 64
        Height = 13
        Caption = 'Filtreli Veri: -'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object LabelNoiseInfo: TLabel
        Left = 16
        Top = 70
        Width = 83
        Height = 13
        Caption = 'G'#252'r'#252'lt'#252' Bilgisi: -'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentFont = False
      end
      object LabelLegend: TLabel
        Left = 16
        Top = 95
        Width = 480
        Height = 13
        Caption = 
          'Renk: Mavi = Ham Veri | Ye'#351'il = Filtreli | K'#305'rm'#305'z'#305' = G'#252'r'#252'lt'#252' | (' +
          'T'#252'm Eksen: Mavi=X, Turuncu=Y, Mor=Z)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = [fsItalic]
        ParentFont = False
      end
    end
    object StatusBar: TStatusBar
      Left = 0
      Top = 126
      Width = 1050
      Height = 19
      Panels = <
        item
          Width = 600
        end
        item
          Width = 200
        end
        item
          Width = 200
        end>
    end
  end
  object OpenDialogCSV: TOpenDialog
    Filter = 'CSV Dosyalar'#305' (*.csv)|*.csv|T'#252'm Dosyalar (*.*)|*.*'
    Title = 'Sens'#246'r Verisi CSV Dosyas'#305' Se'#231'in'
    Left = 992
    Top = 8
  end
  object SaveDialogCSV: TSaveDialog
    DefaultExt = 'csv'
    Filter = 'CSV Dosyalar'#305' (*.csv)|*.csv'
    Title = 'Filtreli Veriyi Kaydet'
    Left = 952
    Top = 8
  end
end
