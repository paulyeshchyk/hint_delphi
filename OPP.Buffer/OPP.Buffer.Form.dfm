object OPPBufferForm: TOPPBufferForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1043#1054#1051#1068#1060#1057#1058#1056#1048#1052': '#1041#1091#1092#1077#1088' '#1086#1073#1084#1077#1085#1072
  ClientHeight = 610
  ClientWidth = 1002
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnResize = FormResize
  PixelsPerInch = 192
  TextHeight = 27
  object cxGrid1: TcxGrid
    Left = 0
    Top = 0
    Width = 1002
    Height = 610
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    TabOrder = 0
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    ExplicitWidth = 988
    ExplicitHeight = 609
    object cxGrid1DBTableView1: TcxGridDBTableView
      PopupMenu = PopupMenu1
      Navigator.Buttons.CustomButtons = <>
      ScrollbarAnnotations.CustomAnnotations = <>
      OnCellDblClick = cxGrid1DBTableView1CellDblClick
      DataController.DataSource = DataSource1
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      DataController.OnDataChanged = cxGrid1DBTableView1DataControllerDataChanged
      FilterRow.SeparatorWidth = 12
      FixedDataRows.SeparatorWidth = 12
      NewItemRow.SeparatorWidth = 12
      OptionsSelection.InvertSelect = False
      OptionsView.NavigatorOffset = 100
      OptionsView.NoDataToDisplayInfoText = 
        #1057#1087#1080#1089#1086#1082' '#1087#1091#1089#1090'. '#1044#1083#1103' '#1090#1086#1075#1086' '#1095#1090#1086#1073#1099' '#1076#1086#1073#1072#1074#1080#1090#1100' '#1079#1072#1087#1080#1089#1100', '#1089#1082#1086#1087#1080#1088#1091#1081#1090#1077' '#1090#1077#1082#1089#1090' '#1074' ' +
        #1073#1091#1092#1077#1088'.'
      OptionsView.GroupByBox = False
      OptionsView.IndicatorWidth = 24
      Preview.LeftIndent = 40
      Preview.RightIndent = 10
      object cxGrid1DBTableView1Column1: TcxGridDBColumn
        Caption = #8470' '#1087'/'#1087
        DataBinding.FieldName = 'SortIndex'
        MinWidth = 40
        Options.Editing = False
        Options.Filtering = False
        Options.Focusing = False
        Options.FilteringAddValueItems = False
        Options.FilteringFilteredItemsList = False
        Options.FilteringMRUItemsList = False
        Options.FilteringPopup = False
        Styles.Content = cxStyle1
        VisibleForEditForm = bFalse
        Width = 92
      end
      object cxGrid1DBTableView1Column2: TcxGridDBColumn
        Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        DataBinding.FieldName = 'data'
        PropertiesClassName = 'TcxTextEditProperties'
        Properties.ReadOnly = True
        Properties.OnValidate = cxGrid1DBTableView1Column2PropertiesValidate
        MinWidth = 200
        Width = 400
      end
      object cxGrid1DBTableView1Column3: TcxGridDBColumn
        Caption = #1060#1080#1082#1089'.'
        DataBinding.FieldName = 'isFixed'
        PropertiesClassName = 'TcxCheckBoxProperties'
        Properties.ReadOnly = True
        MinWidth = 40
        Width = 73
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
      OnExecute = actionNewRecordExecute
    end
    object actionDeleteRecord: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1099#1073#1088#1072#1085#1085#1099#1077
      OnExecute = actionDeleteRecordExecute
    end
    object actionWipeRecords: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100' '#1074#1089#1077' '#1079#1072#1087#1080#1089#1080
      OnExecute = actionWipeRecordsExecute
    end
    object actionShowSettings: TAction
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      OnExecute = actionShowSettingsExecute
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
    object actionMultiSelectMode: TAction
      Caption = #1042#1099#1073#1086#1088' '#1085#1077#1089#1082#1086#1083#1100#1082#1080#1093' '#1079#1072#1087#1080#1089#1077#1081
      OnExecute = actionMultiSelectModeExecute
    end
    object actionMarkAsFixed: TAction
      Caption = #1055#1086#1084#1077#1090#1080#1090#1100' '#1082#1072#1082' '#1092#1080#1082#1089#1080#1088#1086#1074#1072#1085#1085#1099#1077
      OnExecute = actionMarkAsFixedExecute
    end
    object actionMarkAsNonFixed: TAction
      Caption = #1057#1085#1103#1090#1100' '#1087#1086#1084#1077#1090#1082#1091' '#1092#1080#1082#1089#1072#1094#1080#1080
      OnExecute = actionMarkAsNonFixedExecute
    end
    object actionMarkAsInverted: TAction
      Caption = #1048#1085#1074#1077#1088#1090#1080#1088#1086#1074#1072#1090#1100' '#1087#1086#1084#1077#1090#1082#1091
      OnExecute = actionMarkAsInvertedExecute
    end
  end
  object DataSource1: TDataSource
    OnDataChange = DataSource1DataChange
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
        Caption = #1047#1072#1082#1088#1099#1090#1100
        OnClick = actionClose1Click
      end
    end
    object N8: TMenuItem
      Caption = #1047#1072#1087#1080#1089#1080
      object menuItemIsEditMode: TMenuItem
        Action = actionTurnEditMode
      end
      object menuMultiSelectMode: TMenuItem
        Action = actionMultiSelectMode
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
      object N23: TMenuItem
        Action = actionMarkAsFixed
      end
      object N26: TMenuItem
        Action = actionMarkAsInverted
      end
      object N24: TMenuItem
        Action = actionMarkAsNonFixed
      end
      object N22: TMenuItem
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
  object PopupMenu1: TPopupMenu
    Left = 272
    Top = 72
    object N17: TMenuItem
      Action = actionNewRecord
    end
    object N18: TMenuItem
      Caption = '-'
    end
    object N16: TMenuItem
      Action = actionDeleteRecord
    end
    object N19: TMenuItem
      Action = actionWipeRecords
    end
    object N20: TMenuItem
      Caption = '-'
    end
    object N21: TMenuItem
      Action = actionMarkAsFixed
    end
    object N25: TMenuItem
      Action = actionMarkAsNonFixed
    end
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 104
    Top = 24
    PixelsPerInch = 192
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = clBtnFace
    end
  end
end
