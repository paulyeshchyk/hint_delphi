object OPPBufferSettingsForm: TOPPBufferSettingsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = #1043#1054#1051#1068#1060#1057#1058#1056#1048#1052': '#1053#1072#1089#1090#1088#1086#1081#1082#1080' '#1073#1091#1092#1077#1088#1072' '#1086#1073#1084#1077#1085#1072
  ClientHeight = 368
  ClientWidth = 469
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 13
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 16
    Top = 6
    Width = 437
    Height = 155
    Margins.Left = 16
    Margins.Top = 6
    Margins.Right = 16
    Margins.Bottom = 6
    Align = alTop
    Caption = #1054#1075#1088#1072#1085#1080#1095#1077#1085#1080#1077
    TabOrder = 0
    ExplicitLeft = 11
    object Label1: TLabel
      AlignWithMargins = True
      Left = 18
      Top = 111
      Width = 401
      Height = 26
      Margins.Left = 16
      Margins.Top = 16
      Margins.Right = 16
      Margins.Bottom = 16
      Align = alBottom
      Caption = 
        #1042#1085#1080#1084#1072#1085#1080#1077'! '#1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1079#1085#1072#1095#1077#1085#1080#1103' '#1074' '#1084#1077#1085#1100#1096#1091#1102' '#1089#1090#1086#1088#1086#1085#1091' '#1087#1088#1080#1074#1077#1076#1105#1090' '#1082' '#1091#1076#1072#1083#1077#1085 +
        #1080#1102' '#1089#1086#1086#1090#1074#1077#1090#1089#1090#1074#1091#1102#1097#1077#1075#1086' '#1082#1086#1083#1080#1095#1077#1089#1090#1074#1072' '#1080#1084#1077#1102#1097#1080#1093#1089#1103' '#1085#1077#1079#1072#1092#1080#1082#1089#1080#1088#1086#1074#1072#1085#1085#1099#1093' '#1079#1072#1087#1080#1089 +
        #1077#1081'.'
      WordWrap = True
      ExplicitTop = 124
      ExplicitWidth = 381
    end
    object Label3: TLabel
      Left = 16
      Top = 64
      Width = 123
      Height = 21
      Caption = #1052#1072#1082#1089#1080#1084#1072#1083#1100#1085#1086#1077' '#1079#1085#1072#1095#1077#1085#1080#1077
      Layout = tlCenter
    end
    object recordsCountLimitCheckbox: TcxCheckBox
      Left = 8
      Top = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = #1054#1075#1088#1072#1085#1080#1095#1077#1085#1085#1086#1077' '#1095#1080#1089#1083#1086' '#1079#1072#1087#1080#1089#1077#1081
      Properties.OnEditValueChanged = recordsCountLimitCheckboxPropertiesEditValueChanged
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 0
    end
    object recordsCountLimitEdit: TcxSpinEdit
      Left = 178
      Top = 64
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Properties.ImmediatePost = True
      Properties.MaxValue = 32768.000000000000000000
      Properties.MinValue = 5.000000000000000000
      Properties.OnEditValueChanged = recordsCountLimitEditPropertiesEditValueChanged
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 1
      Value = 20
      Width = 58
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 311
    Width = 469
    Height = 57
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 378
      Top = 16
      Width = 75
      Height = 25
      Action = actionSaveSettings
      TabOrder = 0
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 16
    Top = 173
    Width = 437
    Height = 132
    Margins.Left = 16
    Margins.Top = 6
    Margins.Right = 16
    Margins.Bottom = 6
    Align = alTop
    Caption = #1042#1086#1079#1084#1086#1078#1085#1086#1089#1090#1080
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 341
      Top = 93
      Width = 76
      Height = 22
      Hint = #1054#1095#1080#1089#1090#1080#1090#1100
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Action = actionWipeShortcut
      Flat = True
      ParentShowHint = False
      ShowHint = True
    end
    object Label2: TLabel
      Left = 16
      Top = 93
      Width = 170
      Height = 19
      Caption = #1042#1099#1074#1086#1076' '#1086#1082#1085#1072' '#1082#1086#1084#1073#1080#1085#1072#1094#1080#1077#1081' '#1082#1083#1072#1074#1080#1096
      Layout = tlCenter
    end
    object AllowExternalsCheckBox: TcxCheckBox
      Left = 8
      Top = 32
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = #1044#1086#1073#1072#1074#1083#1103#1090#1100' '#1079#1072#1087#1080#1089#1080' '#1080#1079' '#1076#1088#1091#1075#1080#1093' '#1087#1088#1080#1083#1086#1078#1077#1085#1080#1081
      Properties.OnEditValueChanged = AllowExternalsCheckBoxPropertiesEditValueChanged
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 0
    end
    object CanSaveFormFrameCheckbox: TcxCheckBox
      Left = 8
      Top = 60
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Caption = #1057#1086#1093#1088#1072#1085#1103#1090#1100' '#1088#1072#1079#1084#1077#1088#1099' '#1080' '#1087#1086#1083#1086#1078#1077#1085#1080#1077' '#1086#1082#1085#1072
      Properties.OnEditValueChanged = CanSaveFormFrameCheckboxPropertiesEditValueChanged
      Style.LookAndFeel.NativeStyle = True
      StyleDisabled.LookAndFeel.NativeStyle = True
      StyleFocused.LookAndFeel.NativeStyle = True
      StyleHot.LookAndFeel.NativeStyle = True
      TabOrder = 1
    end
    object clipboardManagerShortcut: THotKey
      Left = 221
      Top = 93
      Width = 108
      Height = 19
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      HotKey = 24662
      Modifiers = [hkShift, hkCtrl]
      TabOrder = 2
      OnChange = clipboardManagerShortcutChange
    end
  end
  object ActionList1: TActionList
    Left = 8
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
    object actionWipeShortcut: TAction
      Caption = #1054#1095#1080#1089#1090#1080#1090#1100
      OnExecute = actionWipeShortcutExecute
    end
  end
end
