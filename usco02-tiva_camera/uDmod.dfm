object frmDmod: TfrmDmod
  OldCreateOrder = False
  Height = 430
  Width = 568
  object FDConnection1: TFDConnection
    Params.Strings = (
      'Database=D:\usco_camera\Win32\Debug\CameraControl.db'
      'LockingMode=Normal'
      'DriverID=SQLite')
    Connected = True
    LoginPrompt = False
    Left = 88
    Top = 56
  end
  object qCreateTables: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      ' '
      'CREATE TABLE IF NOT EXISTS DATAS('
      '   DATA_ID  INTEGER PRIMARY KEY AUTOINCREMENT,'
      '   DATA_SENRYA_ID INT ,'
      '   AD           CHAR(10) NOT NULL'
      ''
      '   '
      ');'
      ''
      ''
      'CREATE TABLE IF NOT EXISTS SENARYO('
      '   SENARYO_ID  INTEGER PRIMARY KEY AUTOINCREMENT,'
      '   AD           CHAR(10) NOT NULL,'
      '   AKTIF BOOLEAN,'
      '   SIRA INTEGER,'
      '   ACIKLAMA          CHAR(20)  '
      ');'
      ''
      'CREATE TABLE IF NOT EXISTS IO('
      '   IO_ID  INTEGER PRIMARY KEY AUTOINCREMENT,'
      '   IO_AD           CHAR(10) NOT NULL,'
      '   IO_NO      SMALLINT  NOT NULL'
      ');')
    Left = 72
    Top = 136
  end
  object qSENARYO: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'SELECT * FROM SENARYO ORDER BY SIRA, AKTIF')
    Left = 184
    Top = 56
    object qSENARYOAKTIF: TBooleanField
      FieldName = 'AKTIF'
      Origin = 'AKTIF'
    end
    object qSENARYOSIRA: TIntegerField
      FieldName = 'SIRA'
      Origin = 'SIRA'
    end
    object qSENARYOAD: TStringField
      FieldName = 'AD'
      Origin = 'AD'
      Required = True
      FixedChar = True
      Size = 10
    end
    object qSENARYOACIKLAMA: TStringField
      FieldName = 'ACIKLAMA'
      Origin = 'ACIKLAMA'
      FixedChar = True
    end
    object qSENARYOSENARYO_ID: TFDAutoIncField
      FieldName = 'SENARYO_ID'
      Origin = 'SENARYO_ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
  end
  object qDATAS: TFDQuery
    IndexFieldNames = 'DATA_SENRYA_ID'
    MasterSource = dsSENARYO
    MasterFields = 'SENARYO_ID'
    DetailFields = 'DATA_SENRYA_ID'
    Connection = FDConnection1
    SQL.Strings = (
      'select * from DATAS')
    Left = 184
    Top = 128
    object qDATASDATA_ID: TFDAutoIncField
      FieldName = 'DATA_ID'
      Origin = 'DATA_ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qDATASDATA_SENRYA_ID: TIntegerField
      FieldName = 'DATA_SENRYA_ID'
      Origin = 'DATA_SENRYA_ID'
      Required = True
    end
    object qDATASAD: TStringField
      FieldName = 'AD'
      LookupDataSet = qIO
      LookupKeyFields = 'IO_NO'
      LookupResultField = 'IO_NO'
      Origin = 'AD'
      Required = True
      FixedChar = True
      Size = 10
    end
    object qDATASCIHAZ: TStringField
      FieldKind = fkLookup
      FieldName = 'CIHAZ'
      LookupDataSet = qIO
      LookupKeyFields = 'IO_AD'
      LookupResultField = 'IO_AD'
      KeyFields = 'AD'
      Size = 10
      Lookup = True
    end
    object qDATASPIN: TSmallintField
      FieldKind = fkLookup
      FieldName = 'PIN'
      LookupDataSet = qIO
      LookupKeyFields = 'IO_AD'
      LookupResultField = 'IO_NO'
      KeyFields = 'AD'
      Lookup = True
    end
  end
  object dsDATAS: TDataSource
    DataSet = qDATAS
    Left = 240
    Top = 120
  end
  object dsSENARYO: TDataSource
    DataSet = qSENARYO
    Left = 264
    Top = 56
  end
  object qIO: TFDQuery
    Connection = FDConnection1
    SQL.Strings = (
      'select * from IO')
    Left = 176
    Top = 184
    object qIOIO_ID: TFDAutoIncField
      FieldName = 'IO_ID'
      Origin = 'IO_ID'
      ProviderFlags = [pfInWhere, pfInKey]
      ReadOnly = True
    end
    object qIOIO_AD: TStringField
      FieldName = 'IO_AD'
      Origin = 'IO_AD'
      Required = True
      FixedChar = True
      Size = 10
    end
    object qIOIO_NO: TSmallintField
      FieldName = 'IO_NO'
      Origin = 'IO_NO'
      Required = True
    end
  end
  object dsIO: TDataSource
    DataSet = qIO
    Left = 248
    Top = 184
  end
  object memDatas: TFDMemTable
    Active = True
    FieldDefs = <
      item
        Name = 'DATA_ID'
        Attributes = [faReadonly]
        DataType = ftAutoInc
      end
      item
        Name = 'DATA_SENRYA_ID'
        DataType = ftInteger
      end
      item
        Name = 'AD'
        Attributes = [faRequired, faFixed]
        DataType = ftFixedChar
        Size = 10
      end>
    IndexDefs = <>
    FetchOptions.AssignedValues = [evMode]
    FetchOptions.Mode = fmAll
    FormatOptions.AssignedValues = [fvMaxBcdPrecision, fvMaxBcdScale]
    FormatOptions.MaxBcdPrecision = 2147483647
    FormatOptions.MaxBcdScale = 1073741823
    ResourceOptions.AssignedValues = [rvPersistent, rvSilentMode]
    ResourceOptions.Persistent = True
    ResourceOptions.SilentMode = True
    UpdateOptions.AssignedValues = [uvUpdateChngFields, uvUpdateMode, uvLockMode, uvLockPoint, uvLockWait, uvRefreshMode, uvFetchGeneratorsPoint, uvCheckRequired, uvCheckReadOnly, uvCheckUpdatable, uvAutoCommitUpdates]
    UpdateOptions.LockWait = True
    UpdateOptions.FetchGeneratorsPoint = gpNone
    UpdateOptions.CheckRequired = False
    UpdateOptions.AutoCommitUpdates = True
    StoreDefs = True
    Left = 344
    Top = 168
    Content = {
      414442530F000000A9010000FF00010001FF02FF03040016000000460044004D
      0065006D005400610062006C006500310005000A0000005400610062006C0065
      00060000000000070000080032000000090000FF0AFF0B04000E000000440041
      00540041005F004900440005000E00000044004100540041005F00490044000C
      00010000000E000D000F00011000011100011200011300FFFFFFFF1400FFFFFF
      FF15000116000117000118000119000E00000044004100540041005F00490044
      00FEFF0B04001C00000044004100540041005F00530045004E00520059004100
      5F004900440005001C00000044004100540041005F00530045004E0052005900
      41005F00490044000C00020000000E000D000F00011000011200011500011A00
      0117000119001C00000044004100540041005F00530045004E00520059004100
      5F0049004400FEFF0B04000400000041004400050004000000410044000C0003
      0000000E001B001C000A0000000F00011D00011200011A000117000119000400
      0000410044001E000A000000FEFEFF1FFEFF20FEFF21FEFEFEFF22FEFF232400
      1D000000FF25FEFEFE0E004D0061006E0061006700650072001E005500700064
      0061007400650073005200650067006900730074007200790012005400610062
      006C0065004C006900730074000A005400610062006C00650008004E0061006D
      006500140053006F0075007200630065004E0061006D0065000A005400610062
      0049004400240045006E0066006F0072006300650043006F006E007300740072
      00610069006E00740073001E004D0069006E0069006D0075006D004300610070
      0061006300690074007900180043006800650063006B004E006F0074004E0075
      006C006C00140043006F006C0075006D006E004C006900730074000C0043006F
      006C0075006D006E00100053006F007500720063006500490044000E00640074
      0049006E00740033003200100044006100740061005400790070006500140053
      0065006100720063006800610062006C006500120041006C006C006F0077004E
      0075006C006C000E004100750074006F0049006E006300080042006100730065
      0022004100750074006F0049006E006300720065006D0065006E007400530065
      006500640022004100750074006F0049006E006300720065006D0065006E0074
      00530074006500700014004F0041006C006C006F0077004E0075006C006C0012
      004F0052006500610064004F006E006C00790010004F0049006E005700680065
      007200650020004F004100660074006500720049006E0073004300680061006E
      006700650064001A004F0072006900670069006E0043006F006C004E0061006D
      00650012004F0049006E0055007000640061007400650018006400740041006E
      007300690053007400720069006E0067000800530069007A0065001000460069
      007800650064004C0065006E00140053006F007500720063006500530069007A
      0065001C0043006F006E00730074007200610069006E0074004C006900730074
      00100056006900650077004C006900730074000E0052006F0077004C00690073
      0074001800520065006C006100740069006F006E004C006900730074001C0055
      007000640061007400650073004A006F00750072006E0061006C001200530061
      007600650050006F0069006E0074000E004300680061006E00670065007300}
    object memDatasDATA_ID: TFDAutoIncField
      FieldName = 'DATA_ID'
      ReadOnly = True
    end
    object memDatasDATA_SENRYA_ID: TIntegerField
      FieldName = 'DATA_SENRYA_ID'
    end
    object memDatasAD: TStringField
      FieldName = 'AD'
      Required = True
      FixedChar = True
      Size = 10
    end
  end
end
