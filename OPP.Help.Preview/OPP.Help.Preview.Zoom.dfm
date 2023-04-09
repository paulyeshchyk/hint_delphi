object OPPHelpPreviewZoomForm: TOPPHelpPreviewZoomForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1048#1079#1084#1077#1085#1077#1085#1080#1077' '#1084#1072#1089#1096#1090#1072#1073#1072
  ClientHeight = 233
  ClientWidth = 786
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poMainFormCenter
  PixelsPerInch = 192
  TextHeight = 27
  object cxLabel1: TcxLabel
    Left = 16
    Top = 48
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = #1058#1086#1095#1085#1086
  end
  object cxSpinEdit1: TcxSpinEdit
    Left = 192
    Top = 46
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    AutoSize = False
    Properties.Alignment.Horz = taCenter
    Properties.MaxValue = 499.000000000000000000
    Properties.MinValue = 10.000000000000000000
    Properties.SpinButtons.Position = sbpHorzLeftRight
    Properties.SpinButtons.ShowFastButtons = True
    Properties.ValidationOptions = [evoRaiseException, evoShowErrorIcon]
    Properties.OnEditValueChanged = cxSpinEdit1PropertiesEditValueChanged
    TabOrder = 1
    Value = 10
    Height = 35
    Width = 242
  end
  object cxTrackBar1: TcxTrackBar
    Left = 174
    Top = 128
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Position = 10
    Properties.Frequency = 30
    Properties.Max = 499
    Properties.Min = 10
    Properties.ThumbHeight = 24
    Properties.ThumbWidth = 14
    Properties.TickSize = 6
    Properties.TrackSize = 10
    Properties.OnChange = cxTrackBar1PropertiesChange
    TabOrder = 2
    Height = 50
    Width = 516
  end
  object cxLabel2: TcxLabel
    Left = 16
    Top = 128
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Caption = #1055#1088#1080#1073#1083#1080#1078#1105#1085#1085#1086
  end
  object ActionList1: TActionList
    Left = 528
    Top = 8
    object Action1: TAction
      Caption = 'Action1'
      ShortCut = 27
      OnExecute = Action1Execute
    end
  end
end
