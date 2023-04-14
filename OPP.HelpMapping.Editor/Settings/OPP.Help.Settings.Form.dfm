object OPPHelpSettingsForm: TOPPHelpSettingsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 767
  ClientWidth = 884
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 27
  object Panel1: TPanel
    Left = 0
    Top = 685
    Width = 884
    Height = 82
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 686
    ExplicitWidth = 898
    object Button1: TButton
      Left = 704
      Top = 12
      Width = 150
      Height = 50
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Action = actionSave
      TabOrder = 0
    end
  end
  object cxListView1: TcxListView
    Left = 0
    Top = 0
    Width = 884
    Height = 685
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    Columns = <
      item
        Caption = #1053#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
        Width = 300
      end
      item
        Caption = #1047#1085#1072#1095#1077#1085#1080#1077
        Width = 520
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
