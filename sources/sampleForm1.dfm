object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 633
  ClientWidth = 990
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 0
    Width = 449
    Height = 592
    ParentColor = True
    Align = alLeft
    DockingType = 5
    OriginalWidth = 449
    OriginalHeight = 592
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 449
      Height = 592
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxDockPanel1: TdxDockPanel
      Left = 0
      Top = 0
      Width = 449
      Height = 592
      HelpType = htKeyword
      HelpKeyword = 'KodOKPD2'
      ParentColor = True
      ParentShowHint = False
      ShowHint = True
      AllowFloating = True
      AutoHide = False
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      DockingType = 0
      OriginalWidth = 369
      OriginalHeight = 140
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 445
        Height = 564
        HelpType = htKeyword
        Align = alClient
        BevelKind = bkFlat
        BevelOuter = bvNone
        Caption = 'Panel2'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        object Kod_OKWED: TCheckBox
          Left = 8
          Top = 12
          Width = 97
          Height = 17
          HelpType = htKeyword
          HelpKeyword = 'Kod_OKWED'
          Caption = 'Kod_OKWED'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
        end
        object Kod_MKC: TEdit
          Left = 111
          Top = 12
          Width = 121
          Height = 21
          Hint = 'cxHint test'
          HelpType = htKeyword
          HelpKeyword = 'Kod_MKC'
          ParentCustomHint = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          Text = 'Kod_MKC'
        end
        object cxGrid2: TcxGrid
          Left = 8
          Top = 49
          Width = 425
          Height = 568
          HelpType = htKeyword
          TabOrder = 2
          object cxGrid2TableView1: TcxGridTableView
            Navigator.Buttons.CustomButtons = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <
              item
              end>
            DataController.Summary.SummaryGroups = <>
            OptionsCustomize.ColumnGrouping = False
            OptionsView.GroupByBox = False
            object cxGrid2TableView1Column1: TcxGridColumn
              Caption = #1080#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088
              SortIndex = 0
              SortOrder = soDescending
            end
            object IGK: TcxGridColumn
              Caption = #1048#1043#1050
            end
            object cxGrid2TableView1Column3: TcxGridColumn
              Caption = #1085#1072#1080#1084#1077#1085#1086#1074#1072#1085#1080#1077
            end
          end
          object cxGrid2Level1: TcxGridLevel
            GridView = cxGrid2TableView1
          end
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 592
    Width = 990
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      Left = 544
      Top = 6
      Width = 75
      Height = 25
      Caption = #1055#1086#1084#1086#1097#1100
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object dxDockSite2: TdxDockSite
    Left = 690
    Top = 0
    Width = 300
    Height = 592
    Align = alRight
    DockingType = 5
    OriginalWidth = 300
    OriginalHeight = 592
    object dxLayoutDockSite3: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 300
      Height = 592
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxDockPanel2: TdxDockPanel
      Left = 0
      Top = 0
      Width = 300
      Height = 592
      AllowFloating = True
      AutoHide = False
      Caption = 'dxDockPanel2'
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      DockingType = 0
      OriginalWidth = 185
      OriginalHeight = 140
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 296
        Height = 564
        Align = alClient
        Caption = 'Panel3'
        TabOrder = 0
        object cxGrid1: TcxGrid
          Left = 1
          Top = 1
          Width = 294
          Height = 562
          Align = alClient
          TabOrder = 0
          object cxGrid1DBTableView1: TcxGridDBTableView
            Navigator.Buttons.CustomButtons = <>
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
          end
          object cxGrid1Level1: TcxGridLevel
            GridView = cxGrid1DBTableView1
          end
        end
      end
    end
  end
  object cxHintController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    UseHintControlLookAndFeel = True
    OnShowHint = cxHintControllerShowHint
    OnShowHintEx = cxHintControllerShowHintEx
    Left = 560
    Top = 32
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 560
    Top = 88
    PixelsPerInch = 96
  end
end
