object OPPHelpPreviewZoomForm: TOPPHelpPreviewZoomForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'OPPHelpPreviewZoomForm'
  ClientHeight = 117
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object cxLabel1: TcxLabel
    Left = 8
    Top = 24
    Caption = #1058#1086#1095#1085#1086
  end
  object cxSpinEdit1: TcxSpinEdit
    Left = 96
    Top = 23
    Properties.Alignment.Horz = taCenter
    Properties.MaxValue = 499.000000000000000000
    Properties.MinValue = 10.000000000000000000
    Properties.SpinButtons.Position = sbpHorzLeftRight
    Properties.SpinButtons.ShowFastButtons = True
    Properties.OnEditValueChanged = cxSpinEdit1PropertiesEditValueChanged
    TabOrder = 1
    Value = 10
    Width = 121
  end
  object cxTrackBar1: TcxTrackBar
    Left = 87
    Top = 64
    Position = 10
    Properties.Frequency = 30
    Properties.Max = 499
    Properties.Min = 10
    Properties.OnChange = cxTrackBar1PropertiesChange
    TabOrder = 2
    Height = 25
    Width = 258
  end
  object cxLabel2: TcxLabel
    Left = 8
    Top = 64
    Caption = #1055#1088#1080#1073#1083#1080#1078#1105#1085#1085#1086
  end
  object ActionList1: TActionList
    Left = 256
    Top = 8
    object Action1: TAction
      Caption = 'Action1'
      ShortCut = 27
      OnExecute = Action1Execute
    end
  end
end
