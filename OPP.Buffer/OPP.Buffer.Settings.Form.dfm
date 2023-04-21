object OPPBufferSettingsForm: TOPPBufferSettingsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1043#1054#1051#1068#1060#1057#1058#1056#1048#1052': '#1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1073#1091#1092#1077#1088#1072' '#1086#1073#1084#1077#1085#1072
  ClientHeight = 471
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 3
    Width = 346
    Height = 105
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = #1054#1075#1088#1072#1085#1080#1095#1077#1085#1080#1077
    TabOrder = 0
    object cxCheckBox1: TcxCheckBox
      Left = 8
      Top = 24
      Caption = #1053#1077#1086#1075#1088#1072#1085#1080#1095#1077#1085#1085#1086#1077' '#1095#1080#1089#1083#1086' '#1079#1072#1087#1080#1089#1077#1081
      Properties.OnChange = cxCheckBox1PropertiesChange
      TabOrder = 0
    end
    object cxSpinEdit1: TcxSpinEdit
      Left = 141
      Top = 63
      Properties.MinValue = 5.000000000000000000
      TabOrder = 1
      Value = 5
      Width = 112
    end
    object cxLabel1: TcxLabel
      Left = 8
      Top = 64
      Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 430
    Width = 362
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object cxButton1: TcxButton
      AlignWithMargins = True
      Left = 271
      Top = 8
      Width = 75
      Height = 25
      Margins.Top = 8
      Margins.Right = 16
      Margins.Bottom = 8
      Align = alRight
      Action = actionSaveSettings
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 114
    Width = 346
    Height = 127
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = #1042#1086#1079#1084#1086#1078#1085#1086#1089#1090#1080
    TabOrder = 2
    object cxCheckBox2: TcxCheckBox
      Left = 8
      Top = 24
      Action = actionAddRecordsFromOtherApps
      TabOrder = 0
    end
    object cxCheckBox3: TcxCheckBox
      Left = 8
      Top = 51
      Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1088#1072#1079#1084#1077#1088#1099' '#1080' '#1087#1086#1083#1086#1078#1077#1085#1080#1077' '#1086#1082#1085#1072
      TabOrder = 1
    end
    object cxLabel2: TcxLabel
      Left = 8
      Top = 88
      Caption = #1042#1099#1074#1086#1076' '#1086#1082#1085#1072' '#1082#1086#1084#1073#1080#1085#1072#1094#1080#1077#1081' '#1082#1083#1072#1074#1080#1096
    end
    object HotKey1: THotKey
      Left = 206
      Top = 88
      Width = 121
      Height = 19
      HotKey = 24662
      Modifiers = [hkShift, hkCtrl]
      TabOrder = 3
      OnChange = HotKey1Change
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 8
    Top = 247
    Width = 346
    Height = 160
    Margins.Left = 8
    Margins.Right = 8
    Align = alTop
    Caption = #1057#1086#1088#1090#1080#1088#1086#1074#1082#1072' '#1079#1072#1087#1080#1089#1077#1081' '#1087#1086' '#1082#1086#1083#1086#1085#1082#1072#1084
    TabOrder = 3
    object cxGrid1: TcxGrid
      AlignWithMargins = True
      Left = 10
      Top = 23
      Width = 326
      Height = 127
      Margins.Left = 8
      Margins.Top = 8
      Margins.Right = 8
      Margins.Bottom = 8
      Align = alClient
      TabOrder = 0
      LookAndFeel.Kind = lfFlat
      LookAndFeel.NativeStyle = False
      object cxGrid1DBTableView1: TcxGridDBTableView
        Navigator.Buttons.CustomButtons = <>
        DataController.DataSource = DataSource1
        DataController.Summary.DefaultGroupSummaryItems = <>
        DataController.Summary.FooterSummaryItems = <>
        DataController.Summary.SummaryGroups = <>
        OptionsCustomize.ColumnFiltering = False
        OptionsCustomize.ColumnGrouping = False
        OptionsCustomize.ColumnHidingOnGrouping = False
        OptionsCustomize.ColumnHorzSizing = False
        OptionsCustomize.ColumnMoving = False
        OptionsCustomize.ColumnSorting = False
        OptionsData.CancelOnExit = False
        OptionsData.Deleting = False
        OptionsData.DeletingConfirmation = False
        OptionsData.Inserting = False
        OptionsSelection.InvertSelect = False
        OptionsView.GroupByBox = False
        object cxGrid1DBTableView1Column1: TcxGridDBColumn
          Caption = #1055#1086#1088#1103#1076#1086#1082
          DataBinding.FieldName = 'SortOrder'
          Options.Filtering = False
          Options.ShowEditButtons = isebNever
          Options.GroupFooters = False
          Options.Grouping = False
          Options.Moving = False
          Options.ShowCaption = False
          Options.Sorting = False
          SortIndex = 0
          SortOrder = soAscending
          Width = 25
        end
        object cxGrid1DBTableView1Column2: TcxGridDBColumn
          Caption = #1050#1086#1083#1086#1085#1082#1072
          DataBinding.FieldName = 'FieldNameCaption'
          MinWidth = 159
          Options.Editing = False
          Options.Filtering = False
          Options.FilteringWithFindPanel = False
          Options.IncSearch = False
          Options.FilteringAddValueItems = False
          Options.FilteringFilteredItemsList = False
          Options.FilteringMRUItemsList = False
          Options.FilteringPopup = False
          Options.FilteringPopupMultiSelect = False
          Options.GroupFooters = False
          Options.Grouping = False
          Options.HorzSizing = False
          Options.Moving = False
          Options.Sorting = False
          Width = 159
        end
        object cxGrid1DBTableView1Column3: TcxGridDBColumn
          Caption = #1042#1080#1076' '#1089#1086#1088#1090#1080#1088#1086#1074#1082#1080
          DataBinding.FieldName = 'SortTypeCaption'
          MinWidth = 139
          Options.Filtering = False
          Options.FilteringWithFindPanel = False
          Options.IncSearch = False
          Options.FilteringAddValueItems = False
          Options.FilteringFilteredItemsList = False
          Options.FilteringMRUItemsList = False
          Options.FilteringPopup = False
          Options.FilteringPopupMultiSelect = False
          Options.GroupFooters = False
          Options.Grouping = False
          Options.HorzSizing = False
          Options.Moving = False
          Options.Sorting = False
          Width = 139
        end
      end
      object cxGrid1Level1: TcxGridLevel
        GridView = cxGrid1DBTableView1
      end
    end
  end
  object ActionList1: TActionList
    Left = 256
    Top = 16
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      ShortCut = 27
      OnExecute = actionCloseExecute
    end
    object actionUnlimitedRecordsCount: TAction
      Caption = #1053#1077#1086#1075#1088#1072#1085#1080#1095#1077#1085#1085#1086#1077' '#1095#1080#1089#1083#1086' '#1079#1072#1087#1080#1089#1077#1081
    end
    object actionAddRecordsFromOtherApps: TAction
      Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1079#1072#1087#1080#1089#1080' '#1080#1079' '#1076#1088#1091#1075#1080#1093' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1081
    end
    object actionSaveSettings: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = actionSaveSettingsExecute
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 40
    Top = 407
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      530000009619E0BD01000000180000000300000000000300000053000A536F72
      74547970654944020001000000000009536F72744F7264657202000100000000
      000B4669656C644E616D65494402000100000000000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 24
    Top = 351
    object ClientDataSet1SortTypeID: TSmallintField
      FieldName = 'SortTypeID'
    end
    object ClientDataSet1SortTypeCaption: TStringField
      FieldKind = fkLookup
      FieldName = 'SortTypeCaption'
      LookupDataSet = ClientDataSet2
      LookupKeyFields = 'SortTypeID'
      LookupResultField = 'SortTypeName'
      KeyFields = 'SortTypeID'
      Lookup = True
    end
    object ClientDataSet1SortOrder: TSmallintField
      FieldName = 'SortOrder'
    end
    object ClientDataSet1FieldNameID: TSmallintField
      FieldName = 'FieldNameID'
    end
    object ClientDataSet1FieldNameCaption: TStringField
      FieldKind = fkLookup
      FieldName = 'FieldNameCaption'
      LookupDataSet = ClientDataSet3
      LookupKeyFields = 'FieldNameID'
      LookupResultField = 'FieldNameCaption'
      KeyFields = 'FieldNameID'
      Lookup = True
    end
  end
  object ClientDataSet2: TClientDataSet
    PersistDataPacket.Data = {
      870000009619E0BD0100000018000000020003000000030000004E000A536F72
      7454797065494402000100100000000C536F7274547970654E616D6501004900
      100001000557494454480200020014000000000001000ECFEE20E2EEE7F0E0F1
      F2E0EDE8FE000002000ECDE520F1EEF0F2E8F0EEE2E0F2FC000002000ECDE520
      F1EEF0F2E8F0EEE2E0F2FC}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 120
    Top = 410
    object ClientDataSet2SortTypeID: TSmallintField
      FieldName = 'SortTypeID'
    end
    object ClientDataSet2SortTypeName: TStringField
      FieldName = 'SortTypeName'
    end
  end
  object ClientDataSet3: TClientDataSet
    PersistDataPacket.Data = {
      7C0000009619E0BD01000000180000000200030000000300000053000B466965
      6C644E616D6549440200010010000000104669656C644E616D6543617074696F
      6E010049001000010005574944544802000200140000000000010005B920EF2F
      EF0000020008C7EDE0F7E5EDE8E5000003000DD4E8EAF1E8F0EEE2E0EDEDE0FF}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 216
    Top = 408
    object ClientDataSet3FieldNameID: TSmallintField
      FieldName = 'FieldNameID'
    end
    object ClientDataSet3FieldNameCaption: TStringField
      FieldName = 'FieldNameCaption'
    end
  end
end
