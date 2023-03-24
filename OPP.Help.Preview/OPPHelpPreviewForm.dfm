object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 498
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object oppHelpView: TOPPHelpViewFullScreen
    Left = 0
    Top = 0
    Width = 621
    Height = 478
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 478
    Width = 621
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarContainerPanelStyle'
        PanelStyle.Container = dxStatusBar1Container0
        Width = 141
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 1000
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 20
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    object dxStatusBar1Container0: TdxStatusBarContainerControl
      Left = 2
      Top = 4
      Width = 139
      Height = 14
      object cxProgressBar1: TcxProgressBar
        Left = 0
        Top = 0
        Align = alClient
        AutoSize = False
        ParentColor = False
        Properties.PeakValue = 42.000000000000000000
        Style.BorderStyle = ebsNone
        Style.Color = clBtnFace
        StyleHot.Color = clBtnFace
        TabOrder = 0
        Height = 14
        Width = 139
      end
    end
  end
end
