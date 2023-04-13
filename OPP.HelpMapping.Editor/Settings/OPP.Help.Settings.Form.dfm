object OPPHelpSettingsForm: TOPPHelpSettingsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 384
  ClientWidth = 435
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 343
    Width = 435
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object Button1: TButton
      Left = 352
      Top = 6
      Width = 75
      Height = 25
      Action = actionSave
      TabOrder = 0
    end
  end
  object cxListView1: TcxListView
    Left = 0
    Top = 0
    Width = 435
    Height = 343
    Align = alClient
    Columns = <
      item
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        Width = 150
      end
      item
        Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        Width = 260
      end>
    Items.ItemData = {
      05460000000100000000000000FFFFFFFFFFFFFFFF00000000FFFFFFFF000000
      00161F04430442044C0420003A0420004404300439043B04430420003F043E04
      340441043A04300437043E043A04}
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnDblClick = actionEditValueExecute
  end
  object ActionList1: TActionList
    Left = 200
    Top = 192
    object actionSave: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = actionSaveExecute
    end
    object actionEditValue: TAction
      Caption = 'actionEditValue'
      OnExecute = actionEditValueExecute
    end
  end
end
