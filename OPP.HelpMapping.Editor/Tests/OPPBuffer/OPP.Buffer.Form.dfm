object OPPBufferForm: TOPPBufferForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'OPPBufferForm'
  ClientHeight = 485
  ClientWidth = 487
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object cxGrid1: TcxGrid
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 481
    Height = 479
    Align = alClient
    TabOrder = 0
    object cxGrid1DBTableView1: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = DataSource1
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsCustomize.ColumnFiltering = False
      OptionsCustomize.ColumnGrouping = False
      OptionsCustomize.ColumnMoving = False
      OptionsSelection.MultiSelect = True
      OptionsSelection.InvertSelect = False
      OptionsView.GroupByBox = False
      object cxGrid1DBTableView1Column1: TcxGridDBColumn
        DataBinding.FieldName = 'order'
        Options.Editing = False
        Options.Focusing = False
        Width = 46
      end
      object cxGrid1DBTableView1Column2: TcxGridDBColumn
        DataBinding.FieldName = 'value'
        Options.Editing = False
        Width = 339
      end
      object cxGrid1DBTableView1Column3: TcxGridDBColumn
        DataBinding.FieldName = 'isFixed'
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  object ActionList1: TActionList
    Left = 16
    Top = 104
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      ShortCut = 27
      OnExecute = actionCloseExecute
    end
    object actionExportSettings: TAction
      Caption = #1069#1082#1089#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object actionImportSettings: TAction
      Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object actionExportBuffer: TAction
      Caption = #1069#1082#1089#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
    end
    object actionImportBuffer: TAction
      Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
    end
    object actionNewRecord: TAction
      Caption = #1053#1086#1074#1072#1103' '#1079#1072#1087#1080#1089#1100
      ShortCut = 45
      OnExecute = actionNewRecordExecute
    end
    object actionDeleteRecord: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1079#1072#1087#1080#1089#1100
      ShortCut = 46
    end
    object actionWipeRecords: TAction
      Caption = #1054#1095#1080#1089#1090#1080#1090#1100' '#1079#1072#1087#1080#1089#1080
    end
    object actionShowSettings: TAction
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object actionLoadRecords: TAction
      Caption = 'actionLoadRecords'
      OnExecute = actionLoadRecordsExecute
    end
    object actionSaveRecords: TAction
      Caption = 'actionSaveRecords'
      OnExecute = actionSaveRecordsExecute
    end
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      440000009619E0BD01000000180000000200000000000300000044000576616C
      7565020049000000010005574944544802000200FF0007697346697865640200
      0300000000000000}
    Active = True
    Aggregates = <>
    Params = <>
    OnCalcFields = ClientDataSet1CalcFields
    Left = 152
    Top = 152
    object ClientDataSet1order: TIntegerField
      FieldKind = fkCalculated
      FieldName = 'order'
      Calculated = True
    end
    object ClientDataSet1value: TStringField
      FieldName = 'value'
      Size = 255
    end
    object ClientDataSet1isFixed: TBooleanField
      FieldName = 'isFixed'
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 320
    Top = 248
  end
  object MainMenu1: TMainMenu
    Left = 80
    Top = 104
    object N1: TMenuItem
      Caption = #1041#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
      object N15: TMenuItem
        Action = actionShowSettings
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object N6: TMenuItem
        Action = actionExportSettings
      end
      object N7: TMenuItem
        Action = actionImportSettings
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object actionClose1: TMenuItem
        Action = actionClose
      end
    end
    object N8: TMenuItem
      Caption = #1047#1072#1087#1080#1089#1080
      object N11: TMenuItem
        Action = actionNewRecord
      end
      object N12: TMenuItem
        Action = actionDeleteRecord
      end
      object N13: TMenuItem
        Caption = '-'
      end
      object N14: TMenuItem
        Action = actionWipeRecords
      end
      object N10: TMenuItem
        Caption = '-'
      end
      object N4: TMenuItem
        Action = actionExportBuffer
      end
      object N5: TMenuItem
        Action = actionImportBuffer
      end
    end
  end
end
