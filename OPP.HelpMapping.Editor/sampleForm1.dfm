object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1080#1085#1076#1077#1082#1089#1086#1074' '#1089#1080#1089#1090#1077#1084#1099' '#1087#1086#1084#1086#1097#1080
  ClientHeight = 574
  ClientWidth = 1074
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poDefault
  Visible = True
  OnActivate = FormActivate
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 14
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 0
    Width = 1074
    Height = 554
    Align = alClient
    DockingType = 5
    OriginalWidth = 1074
    OriginalHeight = 554
    object dxLayoutDockSite2: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 537
      Height = 554
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
      object dxLayoutDockSite3: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 537
        Height = 554
        DockingType = 0
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object dxDockPanelList: TdxDockPanel
        Left = 0
        Top = 0
        Width = 537
        Height = 554
        ParentColor = True
        OnVisibleChanged = dxDockPanelListVisibleChanged
        AllowFloating = True
        AutoHide = False
        Caption = #1057#1087#1080#1089#1086#1082
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        OnClose = dxDockPanelListClose
        DockingType = 0
        OriginalWidth = 185
        OriginalHeight = 140
        object dxBarDockControl1: TdxBarDockControl
          Left = 0
          Top = 0
          Width = 533
          Height = 28
          Align = dalTop
          BarManager = dxBarManager1
        end
        object cxGrid1: TcxGrid
          AlignWithMargins = True
          Left = 0
          Top = 28
          Width = 532
          Height = 502
          Margins.Left = 0
          Margins.Top = 0
          Margins.Right = 1
          Margins.Bottom = 0
          Align = alClient
          BorderStyle = cxcbsNone
          TabOrder = 1
          OnResize = cxGrid1Resize
          LookAndFeel.Kind = lfFlat
          LookAndFeel.NativeStyle = False
          object cxGrid1DBTableView1: TcxGridDBTableView
            Navigator.Buttons.CustomButtons = <>
            FindPanel.ApplyInputDelay = 10
            FindPanel.DisplayMode = fpdmManual
            OnCanFocusRecord = cxGrid1DBTableView1CanFocusRecord
            OnFindPanelVisibilityChanged = cxGrid1DBTableView1FindPanelVisibilityChanged
            OnFocusedRecordChanged = cxGrid1DBTableView1FocusedRecordChanged
            DataController.DataSource = DataSource1
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsBehavior.ImmediateEditor = False
            OptionsCustomize.ColumnFiltering = False
            OptionsCustomize.ColumnGrouping = False
            OptionsData.Editing = False
            OptionsSelection.HideFocusRectOnExit = False
            OptionsSelection.InvertSelect = False
            OptionsView.CellEndEllipsis = True
            OptionsView.GridLineColor = clBtnShadow
            OptionsView.GroupByBox = False
            OptionsView.Indicator = True
            Styles.ContentOdd = cxStyle3
            Styles.FindPanel = cxStyle2
            object cxGrid1DBTableView1Column2: TcxGridDBColumn
              Caption = #1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088
              DataBinding.FieldName = 'ComponentIdentifier'
              PropertiesClassName = 'TcxTextEditProperties'
              Properties.ValidateOnEnter = True
              Properties.OnValidate = cxDBTextEdit1PropertiesValidate
              Width = 282
            end
          end
          object cxGrid1Level1: TcxGridLevel
            GridView = cxGrid1DBTableView1
          end
        end
        object PanelFind: TPanel
          AlignWithMargins = True
          Left = 19
          Top = 530
          Width = 514
          Height = 0
          Margins.Left = 19
          Margins.Top = 0
          Margins.Right = 0
          Margins.Bottom = 0
          Align = alBottom
          BevelOuter = bvNone
          TabOrder = 2
        end
      end
    end
    object dxVertContainerDockSite1: TdxVertContainerDockSite
      Left = 537
      Top = 0
      Width = 537
      Height = 554
      ActiveChildIndex = -1
      AllowFloating = True
      AutoHide = False
      CustomCaptionButtons.Buttons = <>
      DockingType = 3
      OriginalWidth = 537
      OriginalHeight = 140
      object dxDockPanelIdentifier: TdxDockPanel
        Left = 0
        Top = 0
        Width = 537
        Height = 78
        ParentColor = True
        OnVisibleChanged = dxDockPanelIdentifierVisibleChanged
        AllowFloating = True
        AutoHide = False
        Caption = #1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        OnClose = dxDockPanelIdentifierClose
        DockingType = 2
        OriginalWidth = 537
        OriginalHeight = 86
        object PanelIDContainer: TPanel
          Left = 0
          Top = 28
          Width = 533
          Height = 33
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alTop
          AutoSize = True
          BevelOuter = bvNone
          TabOrder = 0
          object cxLabel5: TcxLabel
            AlignWithMargins = True
            Left = 8
            Top = 8
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alLeft
            Caption = #1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088' '#1082#1086#1084#1087#1086#1085#1077#1085#1090#1072
            Properties.Alignment.Vert = taVCenter
            AnchorY = 19
          end
          object cxDBTextEdit1: TcxDBTextEdit
            AlignWithMargins = True
            Left = 173
            Top = 8
            Margins.Left = 8
            Margins.Top = 8
            Margins.Right = 8
            Margins.Bottom = 4
            Align = alClient
            DataBinding.DataField = 'ComponentIdentifier'
            DataBinding.DataSource = DataSource1
            Properties.ValidateOnEnter = True
            Properties.OnValidate = cxDBTextEdit1PropertiesValidate
            TabOrder = 1
            Width = 352
          end
        end
        object dxBarDockControl4: TdxBarDockControl
          Left = 0
          Top = 0
          Width = 533
          Height = 28
          Align = dalTop
          BarManager = dxBarManager1
        end
      end
      object dxTabContainerDockSite1: TdxTabContainerDockSite
        Left = 0
        Top = 78
        Width = 537
        Height = 476
        ActiveChildIndex = 0
        AllowFloating = True
        AutoHide = False
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        TabsProperties.TabPosition = tpTop
        DockingType = 2
        OriginalWidth = 537
        OriginalHeight = 528
        object dxDockPanelHint: TdxDockPanel
          Left = 0
          Top = 0
          Width = 533
          Height = 431
          ParentColor = True
          OnVisibleChanged = dxDockPanelHintVisibleChanged
          AllowFloating = True
          AutoHide = False
          Caption = #1055#1086#1076#1089#1082#1072#1079#1082#1072
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 0
          OriginalWidth = 537
          OriginalHeight = 528
          object Panel7: TPanel
            Left = 0
            Top = 0
            Width = 533
            Height = 431
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object Panel1: TPanel
              Left = 0
              Top = 28
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object cxLabel2: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1048#1089#1090#1086#1095#1085#1080#1082
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object cxEditHintPredicateFilename: TcxButtonEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.Buttons = <
                  item
                    Default = True
                    Kind = bkEllipsis
                  end
                  item
                    ImageIndex = 0
                    Kind = bkGlyph
                  end>
                Properties.Images = cxImageList1
                Properties.ReadOnly = False
                Properties.OnButtonClick = cxButtonEdit1PropertiesButtonClick
                Properties.OnEditValueChanged = OnEditValueChanged
                Properties.OnValidate = cxEditHintPredicateFilenamePropertiesValidate
                TabOrder = 1
                Width = 411
              end
            end
            object Panel2: TPanel
              Left = 0
              Top = 61
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object cxComboBoxHintKeywordType: TcxComboBox
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 0
                Width = 411
              end
              object cxLabel3: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
            end
            object Panel3: TPanel
              Left = 0
              Top = 94
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 2
              object cxLabel4: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1080#1089#1082#1072
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object cxTextEditHintPredicateValue: TcxTextEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object Panel4: TPanel
              Left = 0
              Top = 127
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 3
              object cxLabel1: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1059#1090#1086#1095#1085#1077#1085#1080#1077
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
            end
            object Panel6: TPanel
              Left = 0
              Top = 160
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 4
              object cxLabel12: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object cxComboBoxHintDetailsKeywordType: TcxComboBox
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object Panel8: TPanel
              Left = 0
              Top = 193
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 5
              object cxLabel11: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object cxTextEditHintDetailsPredicateValue: TcxTextEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object dxBarDockControl3: TdxBarDockControl
              Left = 0
              Top = 0
              Width = 533
              Height = 28
              Align = dalTop
              BarManager = dxBarManager1
            end
          end
        end
        object dxDockPanelHelp: TdxDockPanel
          Left = 0
          Top = 0
          Width = 533
          Height = 431
          ParentColor = True
          OnVisibleChanged = dxDockPanelHelpVisibleChanged
          AllowFloating = True
          AutoHide = False
          Caption = #1055#1086#1084#1086#1097#1100
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 0
          OriginalWidth = 537
          OriginalHeight = 528
          object Panel5: TPanel
            Left = 0
            Top = 0
            Width = 533
            Height = 431
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object Panel9: TPanel
              Left = 0
              Top = 28
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 0
              object cxLabel6: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1048#1089#1090#1086#1095#1085#1080#1082
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object ShortcutPredicateFilenameEdit: TcxButtonEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.Buttons = <
                  item
                    Default = True
                    Kind = bkEllipsis
                  end
                  item
                    ImageIndex = 0
                    Kind = bkGlyph
                  end>
                Properties.Images = cxImageList1
                Properties.ReadOnly = False
                Properties.OnButtonClick = cxEditShortcutPredicateFilenamePropertiesButtonClick
                Properties.OnEditValueChanged = OnEditValueChanged
                Properties.OnValidate = cxEditHintPredicateFilenamePropertiesValidate
                TabOrder = 1
                Width = 411
              end
            end
            object Panel10: TPanel
              Left = 0
              Top = 61
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 1
              object cxLabel7: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object ShortcutKeywordTypeComboBox: TcxComboBox
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object Panel11: TPanel
              Left = 0
              Top = 94
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 2
              object cxLabel8: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1080#1089#1082#1072
                Constraints.MaxWidth = 90
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object ShortcutPredicateValueEdit: TcxTextEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object Panel12: TPanel
              Left = 0
              Top = 127
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 3
              object cxLabel13: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1059#1090#1086#1095#1085#1077#1085#1080#1077
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
            end
            object Panel13: TPanel
              Left = 0
              Top = 160
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 4
              object cxLabel10: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object ShortcutDetailsKeywordTypeComboBox: TcxComboBox
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object Panel14: TPanel
              Left = 0
              Top = 193
              Width = 533
              Height = 33
              Align = alTop
              BevelOuter = bvNone
              TabOrder = 5
              object cxLabel9: TcxLabel
                AlignWithMargins = True
                Left = 8
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alLeft
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077
                Constraints.MinWidth = 90
                Properties.Alignment.Vert = taVCenter
                AnchorY = 19
              end
              object ShortcutDetailsPredicateValueEdit: TcxTextEdit
                AlignWithMargins = True
                Left = 114
                Top = 8
                Margins.Left = 8
                Margins.Top = 8
                Margins.Right = 8
                Margins.Bottom = 4
                Align = alClient
                Properties.OnEditValueChanged = OnEditValueChanged
                TabOrder = 1
                Width = 411
              end
            end
            object dxBarDockControl2: TdxBarDockControl
              Left = 0
              Top = 0
              Width = 533
              Height = 28
              Align = dalTop
              BarManager = dxBarManager1
            end
          end
        end
        object dxDockPanelPreview: TdxDockPanel
          Left = 0
          Top = 0
          Width = 533
          Height = 431
          OnVisibleChanged = dxDockPanelPreviewVisibleChanged
          AllowFloating = True
          AutoHide = False
          Caption = #1055#1088#1086#1089#1084#1086#1090#1088
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          OnClose = dxDockPanelPreviewClose
          DockingType = 0
          OriginalWidth = 537
          OriginalHeight = 528
          object PanelPreview: TPanel
            AlignWithMargins = True
            Left = 0
            Top = 3
            Width = 533
            Height = 425
            Hint = 'Wrong hint'
            HelpType = htKeyword
            HelpKeyword = 'Kod_OKWED'
            Margins.Left = 0
            Margins.Right = 0
            Align = alClient
            BevelInner = bvRaised
            BevelOuter = bvLowered
            Constraints.MinHeight = 7
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -11
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
        end
      end
    end
  end
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 554
    Width = 1074
    Height = 20
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Text = #1047#1072#1087#1080#1089#1077#1081': 0'
      end>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object cxHintController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    UseHintControlLookAndFeel = True
    Left = 264
    Top = 72
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 272
    Top = 192
    PixelsPerInch = 96
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Filter = 'RTF|*.rtf|PDF|*.pdf'
    Left = 201
    Top = 120
  end
  object MainMenu1: TMainMenu
    Left = 16
    Top = 80
    object N1: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088
      object N5: TMenuItem
        Action = actionShowSettings
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object N3: TMenuItem
        Action = actionExit
      end
    end
    object N4: TMenuItem
      Caption = #1058#1077#1089#1090#1099
      object N11: TMenuItem
        Caption = #1058#1077#1089#1090'1'
        OnClick = N11Click
      end
      object N21: TMenuItem
        Caption = #1058#1077#1089#1090'2'
        OnClick = N21Click
      end
      object N31: TMenuItem
        Caption = #1058#1077#1089#1090'3'
        OnClick = N31Click
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object N7: TMenuItem
        Action = actionShowBuffer
      end
    end
    object N12: TMenuItem
      Caption = #1042#1080#1076
      object N8: TMenuItem
        Caption = #1055#1072#1085#1077#1083#1080
        object NHint: TMenuItem
          Caption = #1055#1072#1085#1077#1083#1100' "'#1055#1086#1076#1089#1082#1072#1079#1082#1072'"'
          OnClick = NHintClick
        end
        object NHelp: TMenuItem
          Caption = #1055#1072#1085#1077#1083#1100' "'#1055#1086#1084#1086#1097#1100'"'
          OnClick = NHelpClick
        end
        object NIdentifier: TMenuItem
          Caption = #1055#1072#1085#1077#1083#1100' "'#1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088'"'
          OnClick = NIdentifierClick
        end
        object NList: TMenuItem
          Caption = #1055#1072#1085#1077#1083#1100' "'#1057#1087#1080#1089#1086#1082'"'
          OnClick = NListClick
        end
        object NPreview: TMenuItem
          Caption = #1055#1072#1085#1077#1083#1100' "'#1055#1088#1086#1089#1084#1086#1090#1088'"'
          OnClick = NPreviewClick
        end
        object N9: TMenuItem
          Caption = '-'
        end
        object N10: TMenuItem
          Caption = #1057#1073#1088#1086#1089
          OnClick = N10Click
        end
      end
    end
  end
  object dxBarManager1: TdxBarManager
    AllowReset = False
    AutoAlignBars = True
    Scaled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    CanCustomize = False
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    MenusShowRecentItemsFirst = False
    PopupMenuLinks = <>
    ShowCloseButton = True
    SunkenBorder = True
    UseSystemFont = True
    Left = 155
    Top = 73
    PixelsPerInch = 96
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'main'
      CaptionButtons = <>
      DockControl = dxBarDockControl1
      DockedDockControl = dxBarDockControl1
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 0
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end
        item
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton12'
        end>
      OneOnRow = True
      RotateWhenVertical = False
      Row = 0
      ShowMark = False
      SizeGrip = False
      UseOwnFont = False
      UseRecentItems = False
      UseRestSpace = True
      Visible = True
      WholeRow = True
    end
    object dxBarManager1Bar2: TdxBar
      Caption = 'help preview'
      CaptionButtons = <>
      DockControl = dxBarDockControl2
      DockedDockControl = dxBarDockControl2
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1108
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton7'
        end>
      OneOnRow = True
      Row = 0
      ShowMark = False
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar3: TdxBar
      Caption = 'hint preview'
      CaptionButtons = <>
      DockControl = dxBarDockControl3
      DockedDockControl = dxBarDockControl3
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 793
      FloatTop = 293
      FloatClientWidth = 51
      FloatClientHeight = 22
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton11'
        end>
      OneOnRow = True
      Row = 0
      ShowMark = False
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object dxBarManager1Bar4: TdxBar
      Caption = 'save undo'
      CaptionButtons = <>
      DockControl = dxBarDockControl4
      DockedDockControl = dxBarDockControl4
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1108
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end
        item
          Visible = True
          ItemName = 'dxBarButton8'
        end>
      OneOnRow = True
      Row = 0
      ShowMark = False
      UseOwnFont = False
      UseRestSpace = True
      Visible = True
      WholeRow = False
    end
    object dxBarButton2: TdxBarButton
      Action = actionNewRecord
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001B744558745469746C6500466F6F7465723B41646420466F6F746572
        3B416464444DED1E0000022349444154785E75934D48945D14C77FF79949825A
        0541BBD6211258460BCDAF4C246813180941DF16528B1661F1AE841681AD0ADB
        B488CA5C44C9A4A0050D04921951A32E5A58AD0A0241A2E4B599E7DEF30F662E
        0E0C762FDC73E070CEFF77CEE13A2019795C789524499B1000082445B7FA4860
        DEBFBE7CB2A9535200C8020E68EB3FD600509B14C3D562771E7E6C053240B540
        0806C0BD27058430814C9889F235F17B758DABE75AF0DE88A244020806028EF6
        D4E3A2B2705502000943040BAC9F2A410A82B1897964B157091469449960F0C2
        817F100400D17BB8A1121128028043123890C0FB0D087C1A10303A5E58579484
        044891E07FAE0FB493A61B1094E2108F1FD95D5174355B8804C1840FB5048079
        0F8207CF0A51510890552C120327F63192FBC0F7F40F7D43B9A9BEA189B610AC
        D2429A1A024EF736026056DD7C369BB07573964D19C7C2E765FACFB470FB6EBE
        B5A3BB91A9E77371062120C1A3C55D584C1402C1F9A6770C8F7D61F1EB322188
        F9A59F148B9EC9F1B7F23EE4E3160C10290602194800624BDD760A4BB35CBAD8
        CEFB4F2B14D34073D71E64E2456EB6A3D2423024585D0104410E3321397EADFD
        A07EE7366EDE9A2E0B3577ED253F3957F67DB0E9384443C095CE251008617248
        4642C27FA77690383838304AB11428A59E969EFD2E9F9BC1017567079F4E6732
        75EDB16FCCC03901808101C3D70E71E3FE0C6F16BE117C7829E836331C9000D9
        EACF84686B8F030478A024C900FE02076661D4056BDB3A0000000049454E44AE
        426082}
    end
    object dxBarButton3: TdxBarButton
      Action = actionDeleteRecord
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000014744558745469746C650052656D6F76653B466F6F7465723B0C8041
        730000027649444154785E75935F48145114C6BF3BAEAE596051A1B4602E64A5
        B5541B410F662EA2FD032B22238A92CADA0A858AA27A124922B01E427C0883A8
        AC07A388C8A034F3217C086A2333298BB22CDADDA475B77575E6DEE9CE99D91D
        309B61CE19E6F27DE777CFB9C300282DB7025D8AA294EAD041974CBAAE5BAF76
        303E094DEBA9AB5E5526D7390038003000A5877678483049642DDB66CD375EAD
        059006C036E05C90B0B53D00834218043208F9D02D7334368653356BA0690296
        2B9206E0C274DFB66109581217CC26201869081D5C70A42E9B402587DB0F5E53
        35DAAB0C243213119CF697FC87809B55AB3679C0924D4C0280990D6564240DA6
        20D0544EB06DF702A98A524459068B208EB3477D50D5290826AC26EEAC5C6656
        6493A6601170E9AEF17F0868B624B87E37409C54DD6ADCF2A25C14CC9F855FBF
        E3282CC8C5D68A85387F120EC6986248690B128B04FBAABC20436B2A99E90CEA
        401F220D1790E3F140E45563DAFD3604D66D7C1F1A4FEC2D7FF6B4930C241651
        DE7C5308011B7DF3A26B485CBE82FC03DB11EB1FC4E7DA5ACC5D5D8419C776E7
        C6CEB55E05E0B6A62048A4C22CAD0BB38991C45768F36622FCE431E6781723BF
        CA07251AC48F8E47180E86BA525350B92082D808F980EB8C4E612F6F47F1FE23
        78E76F82D7A9C3999D85E870182F3A5E0E5FFA3E740280B09A68F6E078D90732
        308F3343BA223050DF8005C52BE0C88823FAE91B32B3A7C35B59E2AABBD3D3B4
        67A8FFA061C0C7D544775D7D874F27B5D944C6741CDEB5123FBB7B91B7C583C1
        E77D78FB514EC2E5806BA91B23A391F500146604DA8AFD67C2CE486BCC7157B8
        1DCE962F7F463B9BA3A133FEACD98D19407950D36A2E8E851FFE054F9B57CA49
        D993700000000049454E44AE426082}
    end
    object dxBarButton4: TdxBarButton
      Action = actionReload
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000027744558745469746C6500526566726573685069766F745461626C65
        3B526566726573683B5570646174653BCE2F81B9000002D949444154785E8D92
        7B48946914C63F2F9997220802211A5128C882C255D7B4B551CB14C108F1D2EA
        AA38D3B6EABA3AB9BA5E721A2FE46DF29638339A94288EE688D1344C0682A5E8
        286B65D36639BA56DB7A5975839AB152779FDEF7EB53CCF9A70F7E9CF73D9CF7
        39E7F07C4C6175B7ACA8E62EBE065ACB308C05B3F1CBAFBC83D5FFFE877169C5
        8C77A6E57596575621A9D082085851916F037996AC405EB9862DAEBEF79A65EE
        DFF7985D5C62199B9A84AAA701254D6771BED20FA2F20824157B4BCE157A1D4E
        957E87C44B470E3159C5B7360AB00F67164C18181D4046650894DDC57830711B
        336F476964EF195521D00ED62341EC2E67328A6E5201DA79BDBBDE308E9FF2FD
        A1EEAF8561410BFD4C2B1E4D5F67E3F8BC06C38636BC7CD387F0B40360D20BBA
        364EC076AF5396BEA96DFF11BFBFAA42EBFD64FC7625902DCE2251FD301D3DCF
        73A19B2A4368E23E302249E7BA4055EF5F989E372232CDEBCFCE3E21D48F0510
        498F821FE5941424704145DB29748CC4A059170E8D5E8893092E6052C52A3381
        E8347EBA5FB493CE3FC609FC33BC0662D676F760C723C7A27857696E0DBFEF79
        0A2625AFC36C850BE22207F2682BC18EC3968BF604070A77B76192736F7CF11F
        BC357E44664D70CB2F64F4357E2EF55108C49E6E54949E692EA5CC47975CE21D
        CF9CCB6C6948CA69C767DA9098AD8440E281C5A567F8C7A8C7BCE90F6806E410
        4A3C119BED2694AB7EC582690C0F2734105CF41864E818DC483B083B09BBA83D
        4FE73A31F2BA1E83130A945E132036CB03979B92D1FBB8114F66DB51A74A4558
        8AAB980A5812AC095BB85D1DA83DBD06099AFB13D03D9689A117523C9D6DC4E8
        DF32F44D1642A68E475CB63B8E473BEF6500ACB12E7622CEB95EAA0C25DEBB22
        47C687421B86AE07F16CCC95FB21527410C49100DA74B38005C1CAFBF4EE6F02
        7E7002B5CE37624F21B16B98D8F681583BE41BB9A7C03DC8713FE792B5B90037
        85DD366B5A60BBC93A7B2E67C3AD6DF90981823369E83C28690000000049454E
        44AE426082}
    end
    object dxBarButton1: TdxBarButton
      Action = actionSave
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000012744558745469746C6500536176652041733B536176659559E8FB00
        0001F649444154785E75533D6B1451143D3399F1A3B7D0C242B00958086A2BE9
        440B2BC1468BFC0105B58B85462B0BADC4266821D8D8A8A045248DA295691404
        3B0B216CCC2E86FDC8CEDC8FE7BDEF31332EAC07CEBB0FE69E733F1E53841030
        0F5996E5771EAD4B5085E76824ECAE50BB04E383958B6581FF235755DCBD710E
        F370EBDE2B0F0B45660090D8C1EFA588C0B13B2104A44E43487473D7147E2CDF
        7CB8616169F87B1B1A042A0A666A92A02E4E42378A9193415EF8618E4B674E9D
        C0FBB7EB587BB21A679E4C2678FCEC53AAAB2E6C0C9076229D41A62A606248AC
        1AD01B4C301E8D21A2293955F57B6B346B20026236125814350BA6CC10D594EC
        C29001DD081091CE40446307CC1C3B205610892725B176428747D130DB41ED15
        8D57AE5E83041FA9C291A38BB8BCFC1D685B4F26870E1F47484BD468E04222C6
        E2C9D3EDCCA3411FBDDE16F69765AAAA80DAD1EFEFE0CD8BD563FEBAC6713460
        EFA0226C6FFDC2CBA7F7310F172E5D47511EC0426C1A3B566804431A81194C14
        19BFEE4E3DB44B7310D5C84DDC2CCF88D6C085E4AC6B00DD7335474006AA6AEB
        E060F37C5967D0ED2026DD7EFD03AC015E2845A319524DC8A77B5095C6BA35E0
        E19FC187AF9B5FCEEE2B0B7CDBFC09960CB509AA291BF710C104190E41547D74
        CDBF06D5E78DE7E70194B1B5776BDD0F358BA632B90609F80B8A878F212AE2B9
        010000000049454E44AE426082}
    end
    object dxBarButton5: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarButton6: TdxBarButton
      Category = 0
      Enabled = False
      Visible = ivAlways
    end
    object dxBarButton7: TdxBarButton
      Action = actionPreviewHelp
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000013744558745469746C6500507265766965773B5072696E749891A1F3
        0000028649444154785E6D915D48D45918C67FE73FA3A649F481F6A1F831D137
        A34562D85E440EA8A0D48D15425DB4D2D645D04D52BB4B73951521E145484448
        A44412151951BB62178991659F1F7AA146C85C2CACB84BECEC7CFCCF396FC330
        CC20CE73F13EE7C0737E3C2FC72B220074F58E0F23520F200822A0002BC98988
        02B1080A30CF7E3DBE3B00E025256B6C7DC7B15A001052926C573A7B5ED4032C
        0068B1E9802099736A480687D19645001337E9C791A8414430364511050EE4E7
        7900D0C66401680308D60A7FCF7FE7CED30F4CCDCE6145F095ACE4606335656B
        57E02887989BA5816B6C321CFAEB5F823D43F8FC1BD9DBBA030798FA3243F0EA
        1382C70394971663B4CEDE4044E87F344EA57F03B575DBD8BE3A87E97943D4BF
        152596BEC1D79C39D6905C61F8FC1EAC910C20EE1A8CB1BC9D0CD1FA4B1DFE62
        2F112DE4E740382ED4D46CA2FBD218AE4EE6926E139E5921D5C0755D1C84AFFF
        180A721413733A0151E42A857135565BB4B1C45DBDB0813106506CA928E2F3F8
        04E19DDB89C42D4B721D5617787833F68192C2309158145FD972DC2F6631C051
        8AB6961ACE5E19C4EB51EC4CD4CE4DF8FBB149869E3CE7544D88A9A11B94AF69
        A2F5C4B8D3FF73954D03B405E538097A315D1DFBB93E3042F7D00B442CABD43C
        ED953334057E62E2E53B66DFDD06283CDCFBF1BF85BF00388E97D275459C3BB9
        0FAD355A1B62B138A3372FF3FCC19F045A7661A39FB8D0BCBEF3B7C73367D200
        ABCD1FED1DF71BAD80A34044B0024A099B7D455456B531F2B80773EF190DCDD5
        BC1A7E7F12389706DCEA3ED04446F41DAD6274641A0166510467FE778065872A
        965E7C78F7ED8998C7B906B85EB20B3766D0A270E306048E94E6D9BE50ECFBC0
        B7F069E077200E447F00A25E564DD5AF61520000000049454E44AE426082}
    end
    object dxBarButton8: TdxBarButton
      Action = actionUndo
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000016744558745469746C6500556E646F3B4172726F773B456469743BCB
        5CF1270000025C49444154785EB5936F48535F18C7BF77D7692BB1561626134D
        63B319230D86FDF951146868BEB0041B2EFB372367D01FEA45FBC1828CD4CA32
        FF65D60BC3B48282B0863175B6B5325A83A814B360D82B11493252B776EFD3BD
        372FAC17BD920E7C38700E9F2F0FCF730E4344F8574B21C00A307F15E4CBB3F7
        FC20020840A569BD28B0275A7A8A793EBA36F0DEADCDD8B0638A40206204688E
        8814DB1D1FBEF03C44F1E0F9CE55A76E3D7FDCDA3F4296DA5E02B04840295733
        323B0399A3D75FFC5189D2DAE0B4DA3B7C537D81090A703C992A9F5045B367F4
        485D6FCFFEEAAEE359397B97018892832C75EEDF7269E57DBDB5D1EDBDE11AA1
        E199107D0C71F46186A3518EA3B7933FC8F9699CEA9F0E51D9D5FEB1DD673A4A
        01C4889EB9BA4FEA015B56F76CACB8401F9F90A8C66C9891BA1726603AC44B84
        391E6A9560D14F74BB3EE3B56FD8EEB85C720940500C5064159DD3A996689A75
        86B4ADD9DB32A18C66C18BD982480078024004250B68E315E8720C62C0EBDFE7
        B9597E9789189932A3B0C6A25E9150BD29D7189BA2D3C0D1E1C1F4F76F58BC34
        0E49698948D5A740C13058AD2634B478C7075D6D99929D5E784D6E649426DBA2
        D3EEACE9CEB33DA2ED27BB2839BBD4B826DF5E6228AAEDDC7CF876F058E73B6A
        F47F255BFB1B5A676AF85F0A48CDBB288F46AE46A5D972FAD0C6F287130016CE
        755E956434AFCDD875C579A0E9253579026430B7BD92029273AB2459461EABC0
        020136E29C15884B2FA86AB3B60E50FA9EF609460AC8A99294C8474102B1CB93
        C10427E557279D0D3DA8508857DAFC0BF59C6AE57FCC3CFE498CB8CFFB372A30
        CFF50B153D09AADDB2FD980000000049454E44AE426082}
    end
    object dxBarButton9: TdxBarButton
      Caption = 'GUID'
      Category = 0
      Hint = 'GUID'
      Visible = ivAlways
    end
    object dxBarButton10: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarButton11: TdxBarButton
      Action = actionPreviewHint
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000013744558745469746C6500507265766965773B5072696E749891A1F3
        0000028649444154785E6D915D48D45918C67FE73FA3A649F481F6A1F831D137
        A34562D85E440EA8A0D48D15425DB4D2D645D04D52BB4B73951521E145484448
        A44412151951BB62178991659F1F7AA146C85C2CACB84BECEC7CFCCF396FC330
        CC20CE73F13EE7C0737E3C2FC72B220074F58E0F23520F200822A0002BC98988
        02B1080A30CF7E3DBE3B00E025256B6C7DC7B15A001052926C573A7B5ED4032C
        0068B1E9802099736A480687D19645001337E9C791A8414430364511050EE4E7
        7900D0C66401680308D60A7FCF7FE7CED30F4CCDCE6145F095ACE4606335656B
        57E02887989BA5816B6C321CFAEB5F823D43F8FC1BD9DBBA030798FA3243F0EA
        1382C70394971663B4CEDE4044E87F344EA57F03B575DBD8BE3A87E97943D4BF
        152596BEC1D79C39D6905C61F8FC1EAC910C20EE1A8CB1BC9D0CD1FA4B1DFE62
        2F112DE4E740382ED4D46CA2FBD218AE4EE6926E139E5921D5C0755D1C84AFFF
        180A721413733A0151E42A857135565BB4B1C45DBDB0813106506CA928E2F3F8
        04E19DDB89C42D4B721D5617787833F68192C2309158145FD972DC2F6631C051
        8AB6961ACE5E19C4EB51EC4CD4CE4DF8FBB149869E3CE7544D88A9A11B94AF69
        A2F5C4B8D3FF73954D03B405E538097A315D1DFBB93E3042F7D00B442CABD43C
        ED953334057E62E2E53B66DFDD06283CDCFBF1BF85BF00388E97D275459C3BB9
        0FAD355A1B62B138A3372FF3FCC19F045A7661A39FB8D0BCBEF3B7C73367D200
        ABCD1FED1DF71BAD80A34044B0024A099B7D455456B531F2B80773EF190DCDD5
        BC1A7E7F12389706DCEA3ED04446F41DAD6274641A0166510467FE778065872A
        965E7C78F7ED8998C7B906B85EB20B3766D0A270E306048E94E6D9BE50ECFBC0
        B7F069E077200E447F00A25E564DD5AF61520000000049454E44AE426082}
    end
    object dxBarButton12: TdxBarButton
      Action = actionShowFindWindow
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001B744558745469746C650046696E643B49643B4C6F636174653B5365
        61726368C8CDD75F0000030649444154785E7D917D48956718C6AFF77C7276CC
        CF91D91F4EC630883119B431DBFA10E940DB6A30A441E8C650823111B2F6E138
        466C8A68D4C6CC1811AB34B659C3CA5C9B6EC23A9BA1B639AC88697E24C7E6D9
        396AE9793FCFF3BCEFBDE71CE478FEA85DF7F3E37EFEB8B92E9EE796B02A4960
        17D8042430059620758690222282ADEEE80F10B295BFDFF2746D53D7950F9ABB
        E59A86F33DBBCB3F2EAC3974A6E0D0177DDDFE2FFBF4BA6357BBABFDEDF900A4
        DAA64B491387C118849C99B9051D6FEE282A7E32D383EB7F4EF84CFEFC687A76
        06BDB6ADD09D95EE41FFC0ADD7C7B4B40C0025319D994891ADECDDC692CF5A7F
        A250244A81A1316A6EEDA4AA8F3AE8C3233D149C7B486353613AFD6D3F55D77F
        4315FBDB8A0148C927C4D3DDE959951B9F59074D6750748E60308CF1F109C114
        54838320C1E57183330516B9AA00D893E98545A56B9DAE35651B84C103594754
        D5B1ACE86006038B19C2D004B7086E8F174EBB056E626FD1CB65D9C9CFDDF5CE
        91F68E8BC3B4B8AC11E3268DDF0BD3270D5FD3AE8A463A7AB287EE879729AAE8
        64C418CDCECD53E7E5DFA8A5ED42AFCFE7F302B0496F541ED7DB1A2BDCD70667
        D0F5CBDF287B7523729E3070F3CE0C466FDFC1FCFC42727B92A8D45D1259D71C
        86CEED99E96E5C0D4C20A7740B7E1CBA8196EAAD989C0E21140A61F34B2F8228
        310D1224CA1210F0C7C8C8369B66C8C1C999084A5F29C0D2F020B66FCAC7DDA9
        39C8D1A507266350550DF507CA31F76F04FE8315F01F781B5E8F0B8AAA807306
        DB42E8EEE7A73B7FC553B9AE44729E9770E6BB5ECCCE4E7771C6A1280A4CCB82
        AE69B044AFAA6E40CD7B6F419565F01887E3F6EF67DB754571FEDC3FB08F206D
        88694BE3F2C3FBA79E2D705E34B1BE5295E30648189826A069AAE80459159D0B
        0300D189BFBEFF0AC0398153C005726C31DB5EF8DC5A31A82492755D17468453
        273EC5E1A6135015159C73484484474992A4B417B6EC89E6E6AD07630CA0F859
        FD48BBCD8EC548040E3C5E66CC3002FF0467B7AE2C0E04098943144F80C959E0
        FF0C8CD1E1CB3B579E2521A9E49D04B1FF0031E8A6CDA38412D6000000004945
        4E44AE426082}
    end
  end
  object ActionList1: TActionList
    Left = 72
    Top = 80
    object actionNewRecord: TAction
      Caption = #1057#1086#1079#1076#1072#1090#1100
      ShortCut = 16462
      OnExecute = actionNewRecordExecute
    end
    object actionSave: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      OnExecute = actionSaveExecute
    end
    object actionDeleteRecord: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100
      OnExecute = actionDeleteRecordExecute
    end
    object actionReload: TAction
      Caption = #1054#1073#1085#1086#1074#1080#1090#1100
      OnExecute = actionReloadExecute
    end
    object actionPreviewHint: TAction
      Caption = 'actionPreview'
      OnExecute = actionPreviewHintExecute
    end
    object actionExit: TFileExit
      Category = 'File'
      Caption = 'E&xit'
      Hint = 'Exit|Quits the application'
      ImageIndex = 43
    end
    object actionUndo: TAction
      Caption = 'actionUndo'
      OnExecute = actionUndoExecute
    end
    object actionShowSettings: TAction
      Caption = #1053#1072#1089#1090#1088#1086#1081#1082#1080
      OnExecute = actionShowSettingsExecute
    end
    object actionShowBuffer: TAction
      Caption = #1041#1091#1092#1077#1088
      OnExecute = actionShowBufferExecute
    end
    object actionPreviewHelp: TAction
      Caption = 'actionPreviewHelp'
      OnExecute = actionPreviewHelpExecute
    end
    object actionShowFindWindow: TAction
      Caption = #1053#1072#1081#1090#1080
      ShortCut = 16454
      OnExecute = actionShowFindWindowExecute
    end
    object actionRestartApp: TAction
      Caption = 'actionRestartApp'
      OnExecute = actionRestartAppExecute
    end
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    OnDataChange = DataSource1DataChange
    Left = 626
    Top = 427
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 706
    Top = 419
  end
  object dxDockingManager1: TdxDockingManager
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
    DefaultTabContainerSiteProperties.TabsProperties.TabPosition = tpTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    Left = 496
    Top = 440
    PixelsPerInch = 96
  end
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svColor]
      Color = clCream
    end
    object cxStyle2: TcxStyle
      AssignedValues = [svColor]
      Color = clBtnFace
    end
    object cxStyle3: TcxStyle
      AssignedValues = [svColor]
      Color = clBtnFace
    end
  end
  object cxImageList1: TcxImageList
    SourceDPI = 96
    FormatVersion = 1
    DesignInfo = 22217641
    ImageInfo = <
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000DE0000
          006A000000000000000000000000000000000000000000000000000000000000
          00000000008B000000F70000008B000000000000000000000000000000BD0000
          00DF000000030000000000000000000000000000000000000000000000000000
          0000000000F7000000FF000000F7000000000000000000000000000000500000
          00FF000000500000000000000000000000000000000000000000000000000000
          00000000008B000000F70000008B000000000000000000000000000000030000
          00DF000000BD0000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0075000000FE0000002C00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0011000000F40000009900000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000099000000F400000011000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000002C000000FE00000075000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000BD000000DF000000030000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000050000000FF000000500000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00000000000000000003000000DF000000BD0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000006A000000DE0000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end
      item
        Image.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF000000FF000000FF000000FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF0000000000000000000000FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000FF0000
          00FF000000FF000000FF0000000000000000000000FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000FF0000
          000000000000000000FF000000FF000000FF000000FF00000000000000000000
          0000000000000000000000000000000000000000000000000000000000FF0000
          0000000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FF000000FF000000FF0000
          00FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000000000000000000000000000FF000000FF000000FF0000
          00FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF0000000000000000000000FF000000FF000000FF0000
          00FF000000000000000000000000000000000000000000000000000000000000
          000000000000000000FF00000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          00FF000000FF000000FF000000FF000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000000000000000
          0000000000000000000000000000000000000000000000000000}
      end>
  end
end
