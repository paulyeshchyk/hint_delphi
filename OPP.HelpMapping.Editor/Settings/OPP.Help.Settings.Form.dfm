object OPPHelpSettingsForm: TOPPHelpSettingsForm
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 460
  ClientWidth = 457
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poMainFormCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 419
    Width = 457
    Height = 41
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      457
      41)
    object Button1: TButton
      Left = 310
      Top = 8
      Width = 75
      Height = 25
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Action = actionSave
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
  end
  object cxListView1: TcxListView
    Left = 0
    Top = 0
    Width = 457
    Height = 419
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
    ParentFont = False
    RowSelect = True
    Style.Font.Charset = DEFAULT_CHARSET
    Style.Font.Color = clWindowText
    Style.Font.Height = -11
    Style.Font.Name = 'Tahoma'
    Style.Font.Style = []
    Style.IsFontAssigned = True
    TabOrder = 0
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
      ShortCut = 13
      OnExecute = actionEditValueExecute
    end
    object actionClose: TAction
      Caption = 'actionClose'
      ShortCut = 27
      OnExecute = actionCloseExecute
    end
  end
end
