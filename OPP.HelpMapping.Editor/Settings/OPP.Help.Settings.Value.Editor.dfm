object OPPHelpSettingsValueEditor: TOPPHelpSettingsValueEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 130
  ClientWidth = 431
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 13
  object Label1: TLabel
    Left = 16
    Top = 16
    Width = 31
    Height = 13
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = 'Label1'
  end
  object Panel1: TPanel
    Left = 0
    Top = 93
    Width = 431
    Height = 37
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 150
    ExplicitWidth = 870
    object cxButton1: TcxButton
      Left = 240
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Action = actionSave
      OptionsImage.Spacing = 8
      TabOrder = 0
    end
    object cxButton2: TcxButton
      Left = 344
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Action = actionClose
      OptionsImage.Spacing = 8
      TabOrder = 1
    end
  end
  object cxTextEdit1: TcxTextEdit
    Left = 16
    Top = 56
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    TabOrder = 1
    Text = 'cxTextEdit1'
    Width = 403
  end
  object ActionList1: TActionList
    Left = 96
    object actionSave: TAction
      Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      ShortCut = 16397
      OnExecute = actionSaveExecute
    end
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      ShortCut = 27
      OnExecute = actionCloseExecute
    end
  end
end
