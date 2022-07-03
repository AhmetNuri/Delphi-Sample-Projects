object Form3: TForm3
  Left = 0
  Top = 0
  Caption = 'Form3'
  ClientHeight = 342
  ClientWidth = 662
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 662
    Height = 41
    Align = alTop
    Caption = 'Panel1'
    TabOrder = 0
    object EditKomut: TEdit
      Left = 113
      Top = 1
      Width = 272
      Height = 39
      Align = alLeft
      Enabled = False
      TabOrder = 0
      Text = 'do 02 1'
      ExplicitHeight = 21
    end
    object btnKomutGoner: TButton
      Left = 385
      Top = 1
      Width = 120
      Height = 39
      Align = alLeft
      Caption = 'Komut G'#246'nder'
      Enabled = False
      TabOrder = 1
      OnClick = btnKomutGonerClick
    end
    object btnBaglan: TButton
      Left = 1
      Top = 1
      Width = 112
      Height = 39
      Align = alLeft
      Caption = 'Ba'#287'lan'
      TabOrder = 2
      OnClick = btnBaglanClick
    end
    object btnRun: TButton
      Left = 505
      Top = 1
      Width = 96
      Height = 39
      Align = alLeft
      Caption = 'Run '
      Enabled = False
      TabOrder = 3
      OnClick = btnRunClick
    end
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 41
    Width = 662
    Height = 301
    ActivePage = tiSenaryo
    Align = alClient
    TabOrder = 1
    object tiSenaryo: TTabSheet
      Caption = 'tiSenaryo'
      object DBGrid1: TDBGrid
        Left = 0
        Top = 0
        Width = 305
        Height = 232
        Align = alLeft
        DataSource = frmDmod.dsSENARYO
        PopupMenu = PopupMenu
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'AKTIF'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SIRA'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'AD'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'ACIKLAMA'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'SENARYO_ID'
            Visible = True
          end>
      end
      object DBGrid2: TDBGrid
        Left = 305
        Top = 0
        Width = 349
        Height = 232
        Align = alClient
        DataSource = frmDmod.dsDATAS
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
        Columns = <
          item
            Expanded = False
            FieldName = 'CIHAZ'
            Visible = True
          end
          item
            Expanded = False
            FieldName = 'PIN'
            Visible = True
          end>
      end
      object Panel2: TPanel
        Left = 0
        Top = 232
        Width = 654
        Height = 41
        Align = alBottom
        TabOrder = 2
        object DBNavigator1: TDBNavigator
          Left = 1
          Top = 1
          Width = 280
          Height = 39
          DataSource = frmDmod.dsSENARYO
          Align = alLeft
          TabOrder = 0
        end
        object DBNavigator2: TDBNavigator
          Left = 321
          Top = 1
          Width = 332
          Height = 39
          DataSource = frmDmod.dsDATAS
          Align = alRight
          TabOrder = 1
        end
      end
    end
    object tiCihaz: TTabSheet
      Caption = 'tiCihaz'
      ImageIndex = 1
      object DBGrid3: TDBGrid
        Left = 0
        Top = 0
        Width = 654
        Height = 227
        Align = alClient
        DataSource = frmDmod.dsIO
        PopupMenu = PopupMenuIO
        TabOrder = 0
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'Tahoma'
        TitleFont.Style = []
      end
      object DBNavigator3: TDBNavigator
        Left = 0
        Top = 227
        Width = 654
        Height = 46
        DataSource = frmDmod.dsIO
        Align = alBottom
        TabOrder = 1
      end
    end
    object tiLog: TTabSheet
      Caption = 'LOG'
      ImageIndex = 2
      object MemoLog: TMemo
        Left = 0
        Top = 0
        Width = 654
        Height = 273
        Align = alClient
        Lines.Strings = (
          'MemoLog')
        TabOrder = 0
      end
    end
    object TabSheet1: TTabSheet
      Caption = 'Comport Settings'
      ImageIndex = 3
      object MemoCOM: TMemo
        Left = 24
        Top = 32
        Width = 185
        Height = 49
        Lines.Strings = (
          'COM3')
        TabOrder = 0
      end
      object btnComportATA: TButton
        Left = 24
        Top = 129
        Width = 185
        Height = 39
        Caption = 'Set Comport'
        TabOrder = 1
        OnClick = btnComportATAClick
      end
    end
  end
  object Timer1: TTimer
    Enabled = False
    OnTimer = Timer1Timer
    Left = 592
    Top = 176
  end
  object PopupMenu: TPopupMenu
    Left = 600
    Top = 120
    object ReceteyiKopyala1: TMenuItem
      Caption = 'Re'#231'eteyi Kopyala'
      OnClick = ReceteyiKopyala1Click
    end
    object BtnIOEkle1: TMenuItem
      Caption = 'B'#252't'#252'n IO Ekle'
      OnClick = BtnIOEkle1Click
    end
  end
  object PopupMenuIO: TPopupMenu
    Left = 592
    Top = 72
    object IOOlutur1: TMenuItem
      Caption = 'IO Olu'#351'tur'
      OnClick = IOOlutur1Click
    end
  end
  object ComPort1: TComPort
    BaudRate = br115200
    Port = 'COM14'
    Parity.Bits = prNone
    StopBits = sbOneStopBit
    DataBits = dbEight
    Events = [evRxChar, evTxEmpty, evRxFlag, evRing, evBreak, evCTS, evDSR, evError, evRLSD, evRx80Full]
    FlowControl.OutCTSFlow = False
    FlowControl.OutDSRFlow = False
    FlowControl.ControlDTR = dtrDisable
    FlowControl.ControlRTS = rtsDisable
    FlowControl.XonXoffOut = False
    FlowControl.XonXoffIn = False
    StoredProps = [spBasic]
    TriggersOnRxChar = True
    Left = 592
    Top = 232
  end
end
