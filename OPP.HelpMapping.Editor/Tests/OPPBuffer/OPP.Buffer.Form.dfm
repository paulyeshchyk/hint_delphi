object OPPBufferForm: TOPPBufferForm
  Left = 0
  Top = 0
  BorderIcons = []
  Caption = 'OPPBufferForm'
  ClientHeight = 425
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
    Height = 419
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
      OptionsSelection.CellSelect = False
      OptionsSelection.MultiSelect = True
      OptionsSelection.InvertSelect = False
      OptionsView.GroupByBox = False
      object cxGrid1DBTableView1Column1: TcxGridDBColumn
        Caption = #8470' '#1087'/'#1087
        DataBinding.FieldName = 'SortIndex'
        Options.Editing = False
        Options.Focusing = False
        VisibleForEditForm = bFalse
        Width = 46
      end
      object cxGrid1DBTableView1Column2: TcxGridDBColumn
        Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        DataBinding.FieldName = 'data'
        Width = 147
      end
      object cxGrid1DBTableView1Column3: TcxGridDBColumn
        Caption = #1060#1080#1082#1089#1080#1088#1086#1074#1072#1085#1085#1072#1103
        DataBinding.FieldName = 'isFixed'
        Width = 85
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
      OnExecute = actionExportSettingsExecute
    end
    object actionImportSettings: TAction
      Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
    end
    object actionExportBuffer: TAction
      Caption = #1069#1082#1089#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      OnExecute = actionExportBufferExecute
    end
    object actionImportBuffer: TAction
      Caption = #1048#1084#1087#1086#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1079#1072#1087#1080#1089#1080
      OnExecute = actionImportBufferExecute
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
    object actionTurnEditMode: TAction
      Caption = #1056#1077#1078#1080#1084' '#1088#1077#1076#1072#1082#1090#1080#1088#1086#1074#1072#1085#1080#1103
      OnExecute = actionTurnEditModeExecute
    end
    object actionApplySelection: TAction
      Caption = 'actionApplySelection'
      ShortCut = 13
      OnExecute = actionApplySelectionExecute
    end
  end
  object DataSource1: TDataSource
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
      object menuItemIsEditMode: TMenuItem
        Action = actionTurnEditMode
      end
      object N9: TMenuItem
        Caption = '-'
      end
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
  object OpenDialog1: TOpenDialog
    DefaultExt = '*.oppclipboarddata'
    Filter = 'oppclipboarddata|*.oppclipboarddata'
    Left = 240
    Top = 248
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.oppclipboarddata'
    Filter = 'oppclipboarddata|*.oppclipboarddata'
    Left = 296
    Top = 168
  end
end
