object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 693
  ClientWidth = 631
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 0
    Width = 449
    Height = 652
    ParentColor = True
    Align = alLeft
    ExplicitHeight = 676
    DockingType = 5
    OriginalWidth = 449
    OriginalHeight = 652
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 449
      Height = 652
      ExplicitHeight = 676
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
    end
    object dxDockPanel1: TdxDockPanel
      Left = 0
      Top = 0
      Width = 449
      Height = 652
      HelpType = htKeyword
      HelpKeyword = 'KodOKPD2'
      ParentColor = True
      ParentShowHint = False
      ShowHint = True
      AllowFloating = True
      AutoHide = False
      Caption = #1050#1086#1076' '#1054#1050#1055#1044
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      ExplicitHeight = 676
      DockingType = 0
      OriginalWidth = 369
      OriginalHeight = 140
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 445
        Height = 624
        HelpType = htKeyword
        Align = alClient
        BevelKind = bkFlat
        BevelOuter = bvNone
        Caption = 'Panel2'
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        ExplicitHeight = 648
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
    Top = 652
    Width = 631
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = 528
    ExplicitTop = 616
    ExplicitWidth = 185
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
