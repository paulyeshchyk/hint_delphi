object OPPHelpSettingsValueEditor: TOPPHelpSettingsValueEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1079#1085#1072#1095#1077#1085#1080#1077' '#1085#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 92
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 31
    Height = 13
    Caption = 'Label1'
  end
  object Panel1: TPanel
    Left = 0
    Top = 55
    Width = 428
    Height = 37
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 104
    ExplicitWidth = 441
    object cxButton1: TcxButton
      Left = 248
      Top = 8
      Width = 75
      Height = 25
      Action = actionSave
      TabOrder = 0
    end
    object cxButton2: TcxButton
      Left = 344
      Top = 8
      Width = 75
      Height = 25
      Action = actionClose
      TabOrder = 1
    end
  end
  object cxTextEdit1: TcxTextEdit
    Left = 8
    Top = 28
    TabOrder = 1
    Text = 'cxTextEdit1'
    Width = 412
  end
  object ActionList1: TActionList
    Left = 96
    object actionSave: TAction
      Caption = #1055#1088#1080#1084#1077#1085#1080#1090#1100
      OnExecute = actionSaveExecute
    end
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = actionCloseExecute
    end
  end
end
