object OPPHelpPreviewForm: TOPPHelpPreviewForm
  Left = 0
  Top = 0
  Caption = 'OPPHelpPreviewForm'
  ClientHeight = 1061
  ClientWidth = 1504
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 27
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 1021
    Width = 1504
    Height = 40
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarContainerPanelStyle'
        PanelStyle.Container = dxStatusBar1Container0
        Width = 282
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 2000
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 40
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -22
    Font.Name = 'Tahoma'
    Font.Style = []
    object dxStatusBar1Container0: TdxStatusBarContainerControl
      Left = 2
      Top = 4
      Width = 280
      Height = 34
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      object cxProgressBar1: TcxProgressBar
        Left = 0
        Top = 0
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        AutoSize = False
        ParentColor = False
        Properties.PeakSize = 4
        Properties.PeakValue = 42.000000000000000000
        Style.BorderStyle = ebsNone
        Style.Color = clBtnFace
        StyleHot.Color = clBtnFace
        TabOrder = 0
        Height = 34
        Width = 280
      end
    end
  end
  object oppHelpView: TOPPHelpViewFullScreen
    Left = 0
    Top = 48
    Width = 1504
    Height = 973
    Align = alClient
    BevelOuter = bvNone
    Caption = 'oppHelpView'
    TabOrder = 5
    ExplicitTop = 25
    ExplicitWidth = 614
    ExplicitHeight = 344
  end
  object dxDockingManager1: TdxDockingManager
    AutoHideMovingSize = 40
    Color = clBtnFace
    DefaultHorizContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultHorizContainerSiteProperties.Dockable = True
    DefaultHorizContainerSiteProperties.ImageIndex = -1
    DefaultVertContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultVertContainerSiteProperties.Dockable = True
    DefaultVertContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.CustomCaptionButtons.Buttons = <>
    DefaultTabContainerSiteProperties.Dockable = True
    DefaultTabContainerSiteProperties.ImageIndex = -1
    DefaultTabContainerSiteProperties.TabsProperties.CustomButtons.Buttons = <>
    DockZonesWidth = 40
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -22
    Font.Name = 'Tahoma'
    Font.Style = []
    ResizeZonesWidth = 6
    SelectionFrameWidth = 8
    Left = 304
    Top = 256
    PixelsPerInch = 192
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -24
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 640
    Top = 328
    PixelsPerInch = 192
    DockControlHeights = (
      0
      0
      48
      0)
    object dxBarManager1Bar1: TdxBar
      Caption = 'default'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 655
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarSubItem1'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object dxBarButtonExit: TdxBarButton
      Caption = 'E&xit'
      Category = 0
      Hint = 'Exit'
      Visible = ivAlways
      OnClick = dxBarButtonExitClick
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = '&File'
      Category = 0
      Visible = ivAlways
      AllowCustomizing = False
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarSeparator1'
        end
        item
          Visible = True
          ItemName = 'dxBarButtonExit'
        end>
    end
    object dxBarSubItem2: TdxBarSubItem
      Caption = 'New SubItem'
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton1: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarSeparator1: TdxBarSeparator
      Caption = 'New separator'
      Category = 0
      Hint = 'New separator'
      Visible = ivAlways
      ShowCaption = False
    end
  end
end
