object OPPGuideForm: TOPPGuideForm
  Left = 0
  Top = 0
  Caption = 'OPPGuideForm'
  ClientHeight = 1041
  ClientWidth = 1417
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 3
    Width = 1417
    Height = 1017
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    ManagerFont = False
    ParentFont = True
    Align = alClient
    DockingType = 5
    OriginalWidth = 1417
    OriginalHeight = 1017
    object dxLayoutDockSite2: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 1417
      Height = 521
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      ManagerFont = False
      ParentFont = True
      DockingType = 0
      OriginalWidth = 600
      OriginalHeight = 400
      object dxLayoutDockSite3: TdxLayoutDockSite
        Left = 642
        Top = 0
        Width = 775
        Height = 521
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        ManagerFont = False
        ParentFont = True
        DockingType = 0
        OriginalWidth = 600
        OriginalHeight = 400
        object dxLayoutDockSite1: TdxLayoutDockSite
          Left = 0
          Top = 0
          Width = 133
          Height = 521
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          ManagerFont = False
          ParentFont = True
          DockingType = 0
          OriginalWidth = 600
          OriginalHeight = 400
        end
        object dxVertContainerDockSite2: TdxVertContainerDockSite
          Left = 133
          Top = 0
          Width = 642
          Height = 521
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          ManagerFont = False
          ParentFont = True
          ActiveChildIndex = -1
          AllowFloating = True
          AutoHide = False
          CustomCaptionButtons.Buttons = <>
          DockingType = 3
          OriginalWidth = 642
          OriginalHeight = 280
          object dxDockPanel5: TdxDockPanel
            Left = 0
            Top = 0
            Width = 642
            Height = 261
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            ManagerFont = False
            ParentFont = True
            AllowFloating = True
            AutoHide = False
            Caption = 'dxDockPanel5'
            CustomCaptionButtons.Buttons = <>
            TabsProperties.CustomButtons.Buttons = <>
            DockingType = 2
            OriginalWidth = 642
            OriginalHeight = 280
          end
          object dxDockPanel6: TdxDockPanel
            Left = 0
            Top = 261
            Width = 642
            Height = 260
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            ManagerFont = False
            ParentFont = True
            AllowFloating = True
            AutoHide = False
            Caption = 'dxDockPanel6'
            CustomCaptionButtons.Buttons = <>
            TabsProperties.CustomButtons.Buttons = <>
            DockingType = 2
            OriginalWidth = 642
            OriginalHeight = 280
          end
        end
      end
      object dxVertContainerDockSite1: TdxVertContainerDockSite
        Left = 0
        Top = 0
        Width = 642
        Height = 521
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        ManagerFont = False
        ParentFont = True
        ActiveChildIndex = -1
        AllowFloating = True
        AutoHide = False
        CustomCaptionButtons.Buttons = <>
        DockingType = 1
        OriginalWidth = 642
        OriginalHeight = 280
        object dxDockPanelTreeView: TdxDockPanel
          Left = 0
          Top = 0
          Width = 642
          Height = 174
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Color = clBtnFace
          ManagerColor = False
          ManagerFont = False
          ParentFont = True
          AllowFloating = True
          AutoHide = False
          Caption = 'Hierarchy'
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 2
          OriginalWidth = 642
          OriginalHeight = 280
          object cxDBTreeList1: TcxDBTreeList
            Left = 0
            Top = 0
            Width = 638
            Height = 150
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            BorderStyle = cxcbsNone
            Align = alClient
            Bands = <
              item
                MinWidth = 40
              end>
            DataController.DataSource = DataSourceTreeView
            DataController.ImageIndexField = 'NodeType'
            DataController.ParentField = 'PIdentifier'
            DataController.KeyField = 'Identifier'
            DragMode = dmAutomatic
            Images = ImageList1
            LookAndFeel.Kind = lfFlat
            LookAndFeel.SkinName = ''
            Navigator.Buttons.CustomButtons = <>
            OptionsBehavior.ImmediateEditor = False
            OptionsSelection.InvertSelect = False
            OptionsView.FixedSeparatorWidth = 4
            OptionsView.Headers = False
            OptionsView.IndicatorWidth = 16
            OptionsView.NavigatorOffset = 100
            PopupMenu = PopupMenu1
            Preview.LeftIndent = 10
            Preview.RightIndent = 10
            RootValue = -1
            TabOrder = 0
            OnDragDrop = cxDBTreeList1DragDrop
            OnDragOver = cxDBTreeList1DragOver
            OnFocusedNodeChanged = cxDBTreeList1FocusedNodeChanged
            OnInitInsertingRecord = cxDBTreeList1InitInsertingRecord
            OnKeyDown = cxDBTreeList1KeyDown
            object cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn
              DataBinding.FieldName = 'Caption'
              MinWidth = 40
              Options.Sorting = False
              Width = 590
              Position.ColIndex = 0
              Position.RowIndex = 0
              Position.BandIndex = 0
              Summary.FooterSummaryItems = <>
              Summary.GroupFooterSummaryItems = <>
            end
            object cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn
              Visible = False
              DataBinding.FieldName = 'Order'
              MinWidth = 40
              Width = 200
              Position.ColIndex = 1
              Position.RowIndex = 0
              Position.BandIndex = 0
              SortOrder = soAscending
              SortIndex = 0
              Summary.FooterSummaryItems = <>
              Summary.GroupFooterSummaryItems = <>
            end
          end
        end
        object dxDockPanelScript: TdxDockPanel
          Left = 0
          Top = 174
          Width = 642
          Height = 174
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          ManagerFont = False
          ParentFont = True
          AllowFloating = True
          AutoHide = False
          Caption = 'Script'
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 2
          OriginalWidth = 642
          OriginalHeight = 280
          object dxBarDockControl2: TdxBarDockControl
            Left = 0
            Top = 0
            Width = 638
            Height = 28
            Align = dalTop
            BarManager = dxBarManager1
          end
          object ScrMemo1: TScrMemo
            Left = 0
            Top = 28
            Width = 638
            Height = 102
            Cursor = crIBeam
            ActiveLineSettings.ShowActiveLine = False
            ActiveLineSettings.ShowActiveLineIndicator = True
            Align = alClient
            AutoCompletion.Active = False
            AutoCompletion.AutoDisplay = False
            AutoCompletion.AutoWidth = False
            AutoCompletion.Font.Charset = DEFAULT_CHARSET
            AutoCompletion.Font.Color = clWindowText
            AutoCompletion.Font.Height = -11
            AutoCompletion.Font.Name = 'Tahoma'
            AutoCompletion.Font.Style = []
            AutoCompletion.StartToken = '(.'
            AutoCorrect.Active = True
            AutoHintParameterPosition = hpBelowCode
            BlockBrackets = True
            BlockColor = clWhite
            BlockLineColor = clTeal
            BookmarkGlyph.Data = {
              36050000424D3605000000000000360400002800000010000000100000000100
              0800000000000001000000000000000000000001000000000000000000000000
              80000080000000808000800000008000800080800000C0C0C000C0DCC000F0CA
              A6000020400000206000002080000020A0000020C0000020E000004000000040
              20000040400000406000004080000040A0000040C0000040E000006000000060
              20000060400000606000006080000060A0000060C0000060E000008000000080
              20000080400000806000008080000080A0000080C0000080E00000A0000000A0
              200000A0400000A0600000A0800000A0A00000A0C00000A0E00000C0000000C0
              200000C0400000C0600000C0800000C0A00000C0C00000C0E00000E0000000E0
              200000E0400000E0600000E0800000E0A00000E0C00000E0E000400000004000
              20004000400040006000400080004000A0004000C0004000E000402000004020
              20004020400040206000402080004020A0004020C0004020E000404000004040
              20004040400040406000404080004040A0004040C0004040E000406000004060
              20004060400040606000406080004060A0004060C0004060E000408000004080
              20004080400040806000408080004080A0004080C0004080E00040A0000040A0
              200040A0400040A0600040A0800040A0A00040A0C00040A0E00040C0000040C0
              200040C0400040C0600040C0800040C0A00040C0C00040C0E00040E0000040E0
              200040E0400040E0600040E0800040E0A00040E0C00040E0E000800000008000
              20008000400080006000800080008000A0008000C0008000E000802000008020
              20008020400080206000802080008020A0008020C0008020E000804000008040
              20008040400080406000804080008040A0008040C0008040E000806000008060
              20008060400080606000806080008060A0008060C0008060E000808000008080
              20008080400080806000808080008080A0008080C0008080E00080A0000080A0
              200080A0400080A0600080A0800080A0A00080A0C00080A0E00080C0000080C0
              200080C0400080C0600080C0800080C0A00080C0C00080C0E00080E0000080E0
              200080E0400080E0600080E0800080E0A00080E0C00080E0E000C0000000C000
              2000C0004000C0006000C0008000C000A000C000C000C000E000C0200000C020
              2000C0204000C0206000C0208000C020A000C020C000C020E000C0400000C040
              2000C0404000C0406000C0408000C040A000C040C000C040E000C0600000C060
              2000C0604000C0606000C0608000C060A000C060C000C060E000C0800000C080
              2000C0804000C0806000C0808000C080A000C080C000C080E000C0A00000C0A0
              2000C0A04000C0A06000C0A08000C0A0A000C0A0C000C0A0E000C0C00000C0C0
              2000C0C04000C0C06000C0C08000C0C0A000F0FBFF00A4A0A000808080000000
              FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00FDFD25252525
              2525252525252525FDFDFD2E25FFFFFFFFFFFFFFFFFFFF25FDFDFD2525252525
              2525252525252525FDFD9A9AB7B7B7B7B7B7B7B7B7B72525FDFDFD25B7B7B7B7
              B7B7B7B7B7B72525FDFD9A9AB7B7B7B7B7B7B7B7B7B72525FDFDFD25BFB7BFBF
              B7B7B7B7B7B72525FDFD9A9ABFBFBFB7BFBFB7B7B7B72525FDFDFD25BFBFBFBF
              BFB7BFBFB7B72525FDFD9A9ABFBFBFB7BFBFBFB7BFB72525FDFDFD25BFBFBFBF
              BFBFBFBFBFB72525FDFD9A9ABFBFBFBFBFB7BFBFB7B72525FDFDFD25BFBFBFBF
              BFBFBFBFBFB72525FDFD9A9ABFBFBFBFBFBFBFBFBFB725FDFDFDFD2525252525
              25252525252525FDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFDFD}
            BorderColor = 15000546
            BorderStyle = bsSingle
            ClipboardFormats = [cfText]
            CodeFolding.Enabled = False
            CodeFolding.LineColor = clGray
            Ctl3D = False
            DelErase = True
            EnhancedHomeKey = False
            Gutter.Font.Charset = DEFAULT_CHARSET
            Gutter.Font.Color = clWindowText
            Gutter.Font.Height = -13
            Gutter.Font.Name = 'Courier New'
            Gutter.Font.Style = []
            Gutter.BorderColor = 15000546
            Gutter.GutterColor = 16250613
            Gutter.GutterColorTo = 16250613
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clBlack
            Font.Height = -13
            Font.Name = 'COURIER NEW'
            Font.Style = []
            HiddenCaret = False
            Lines.Strings = (
              '')
            MarkerList.UseDefaultMarkerImageIndex = False
            MarkerList.DefaultMarkerImageIndex = -1
            MarkerList.ImageTransparentColor = 33554432
            MemoSource = ScrMemoSource1
            OleDropTarget = []
            PrintOptions.MarginLeft = 0
            PrintOptions.MarginRight = 0
            PrintOptions.MarginTop = 0
            PrintOptions.MarginBottom = 0
            PrintOptions.PageNr = False
            PrintOptions.PrintLineNumbers = False
            RightMarginColor = 14869218
            ScrollHint = False
            SelColor = clWhite
            SelBkColor = clNavy
            ShowRightMargin = True
            SmartTabs = False
            SyntaxStyles = ScrPascalMemoStyler1
            TabOrder = 1
            TabSize = 2
            TabStop = True
            TrimTrailingSpaces = True
            UILanguage.ScrollHint = 'Row'
            UILanguage.Undo = 'Undo'
            UILanguage.Redo = 'Redo'
            UILanguage.Copy = 'Copy'
            UILanguage.Cut = 'Cut'
            UILanguage.Paste = 'Paste'
            UILanguage.Delete = 'Delete'
            UILanguage.SelectAll = 'Select All'
            UIStyle = tsWindows8
            UrlStyle.TextColor = clBlue
            UrlStyle.BkColor = clWhite
            UrlStyle.Style = [fsUnderline]
            UseStyler = True
            Version = '3.8.1.0'
            WordWrap = wwNone
            OnCursorChange = ScrMemo1CursorChange
            OnChange = ScrMemo1Change
          end
          object dxStatusBar2: TdxStatusBar
            Left = 0
            Top = 130
            Width = 638
            Height = 20
            Panels = <
              item
                PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
                Width = 70
              end
              item
                PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
              end>
            Font.Charset = RUSSIAN_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Courier'
            Font.Style = []
          end
        end
        object dxDockPanelProperties: TdxDockPanel
          Left = 0
          Top = 348
          Width = 642
          Height = 173
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          ManagerFont = False
          ParentFont = True
          AllowFloating = True
          AutoHide = False
          Caption = 'Properties'
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          DockingType = 2
          OriginalWidth = 642
          OriginalHeight = 280
          object cxDBVerticalGrid1: TcxDBVerticalGrid
            Left = 0
            Top = 0
            Width = 638
            Height = 149
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            BorderStyle = cxcbsNone
            Align = alClient
            LookAndFeel.Kind = lfFlat
            LookAndFeel.NativeStyle = False
            LookAndFeel.SkinName = ''
            OptionsView.ScrollBars = ssVertical
            OptionsView.RowHeaderMinWidth = 48
            OptionsView.RowHeaderWidth = 150
            OptionsView.ValueWidth = 200
            OptionsView.ValueMinWidth = 80
            OptionsView.NavigatorOffset = 100
            OptionsBehavior.GoToNextCellOnEnter = True
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.FocusCellOnCycle = True
            OptionsBehavior.AllowChangeRecord = False
            OptionsData.Appending = False
            OptionsData.Deleting = False
            OptionsData.DeletingConfirmation = False
            OptionsData.Inserting = False
            Navigator.Buttons.CustomButtons = <>
            Navigator.InfoPanel.Visible = True
            TabOrder = 0
            DataController.DataSource = DataSourceTreeView
            Version = 1
            object cxDBVerticalGrid1DBEditorRow6: TcxDBEditorRow
              Properties.EditPropertiesClassName = 'TcxLookupComboBoxProperties'
              Properties.EditProperties.ImmediatePost = True
              Properties.EditProperties.KeyFieldNames = 'id'
              Properties.EditProperties.ListColumns = <
                item
                  FieldName = 'caption'
                end>
              Properties.EditProperties.ListOptions.ShowHeader = False
              Properties.EditProperties.ListSource = DataSourceNodeType
              Properties.EditProperties.OnEditValueChanged = cxDBVerticalGrid1DBEditorRow6EditPropertiesEditValueChanged
              Properties.DataBinding.FieldName = 'NodeType'
              ID = 0
              ParentID = -1
              Index = 0
              Version = 1
            end
            object cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow
              Properties.EditPropertiesClassName = 'TcxTextEditProperties'
              Properties.DataBinding.FieldName = 'Caption'
              ID = 1
              ParentID = -1
              Index = 1
              Version = 1
            end
            object cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow
              Properties.DataBinding.FieldName = 'Identifier'
              Properties.Options.Editing = False
              Properties.Options.Filtering = False
              Properties.Options.IncSearch = False
              Properties.Options.ShowEditButtons = eisbNever
              ID = 2
              ParentID = -1
              Index = 2
              Version = 1
            end
          end
        end
      end
    end
    object dxDockPanelOutputLog: TdxDockPanel
      Left = 0
      Top = 521
      Width = 1417
      Height = 496
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      ManagerFont = False
      ParentFont = True
      AllowFloating = True
      AutoHide = False
      Caption = 'Log'
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      DockingType = 4
      OriginalWidth = 370
      OriginalHeight = 496
      object cxMemo1: TcxMemo
        Left = 0
        Top = 0
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Align = alClient
        Properties.ReadOnly = True
        Properties.ScrollBars = ssBoth
        Style.BorderStyle = ebsNone
        Style.LookAndFeel.Kind = lfFlat
        Style.LookAndFeel.SkinName = ''
        StyleDisabled.LookAndFeel.Kind = lfFlat
        StyleDisabled.LookAndFeel.SkinName = ''
        StyleFocused.LookAndFeel.Kind = lfFlat
        StyleFocused.LookAndFeel.SkinName = ''
        StyleHot.LookAndFeel.Kind = lfFlat
        StyleHot.LookAndFeel.SkinName = ''
        TabOrder = 0
        Height = 472
        Width = 1413
      end
    end
  end
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 1020
    Width = 1417
    Height = 21
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
        Width = 200
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    ParentFont = True
    OnResize = dxStatusBar1Resize
  end
  object dxBarDockControl3: TdxBarDockControl
    Left = 0
    Top = 0
    Width = 1417
    Align = dalTop
    BarManager = dxBarManager1
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
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Segoe UI'
    Font.Style = []
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    LookAndFeel.SkinName = ''
    Left = 736
    Top = 304
    PixelsPerInch = 96
  end
  object DataSourceTreeView: TDataSource
    DataSet = DataSetTreeView
    Left = 829
    Top = 352
  end
  object DataSetTreeView: TClientDataSet
    PersistDataPacket.Data = {
      DD0000009619E0BD010000001800000007000000000003000000DD000A496465
      6E74696669657201004900000001000557494454480200020022000B50496465
      6E7469666965720100490000000100055749445448020002002200084E6F6465
      547970650400010000000000054F726465720400010000000000074361707469
      6F6E02004A000000010005574944544802000200FE0110416374696F6E496465
      6E746966696572020049000000010005574944544802000200FF000653637269
      707404004B0000000100075355425459504502004900070042696E6172790000
      00}
    Active = True
    Aggregates = <>
    Params = <>
    AfterOpen = DataSetTreeViewAfterOpen
    BeforeEdit = DataSetTreeViewBeforeEdit
    AfterPost = DataSetTreeViewAfterPost
    AfterScroll = DataSetTreeViewAfterScroll
    AfterApplyUpdates = DataSetTreeViewAfterApplyUpdates
    Left = 824
    Top = 416
    object DataSetTreeViewIdentifier: TStringField
      DisplayWidth = 40
      FieldName = 'Identifier'
      Size = 34
    end
    object DataSetTreeViewPIdentifier: TStringField
      DisplayWidth = 40
      FieldName = 'PIdentifier'
      Size = 34
    end
    object DataSetTreeViewNodeType: TIntegerField
      FieldName = 'NodeType'
    end
    object DataSetTreeViewOrder: TIntegerField
      FieldName = 'Order'
    end
    object DataSetTreeViewCaption: TWideStringField
      FieldName = 'Caption'
      Size = 255
    end
    object DataSetTreeViewActionText: TStringField
      FieldName = 'ActionIdentifier'
      Size = 255
    end
    object DataSetTreeViewScript: TBlobField
      FieldName = 'Script'
    end
  end
  object dxBarManager1: TdxBarManager
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Categories.Strings = (
      'Default')
    Categories.ItemsVisibles = (
      2)
    Categories.Visibles = (
      True)
    LookAndFeel.Kind = lfFlat
    LookAndFeel.SkinName = ''
    PopupMenuLinks = <>
    UseSystemFont = False
    Left = 728
    Top = 192
    PixelsPerInch = 96
    object dxBarManager1Bar2: TdxBar
      Caption = 'script'
      CaptionButtons = <>
      DockControl = dxBarDockControl2
      DockedDockControl = dxBarDockControl2
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 2276
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -3
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton9'
        end
        item
          Visible = True
          ItemName = 'dxBarButton12'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = True
      Visible = True
      WholeRow = False
    end
    object dxBarButton1: TdxBarButton
      Action = actionAddRecord
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002A744558745469746C6500466F6F7465723B4164642047726F757020
        466F6F7465723B466F6F7465723B47726F7570951B9BC9000002174944415478
        5E7D92CB4B555118C57FFB9EAB41F507D4A04913C16A1422DD8A4CB1E7631064
        39087B4A45616426346910CD9C2544AF8960282464F888204332082B1A685160
        4490504DA4EE95CA7BF65EC947E7205E680DF6FA061FEBB7367B3B2073BBF7CD
        139771350008642E73911E084728C6A3670E57D549F20059C0FD9C2DD65C385A
        45BA9B0EA5EAB83BBE0570004900DE0B09BA07DE19270814440842C8E642E137
        2D4DD5781F581CE0BCF700ECAFAF00C9F225074E981293F0A134001F04889EE1
        B7469320482011CC205FF845EBB10D4903163508063EB0ADD2A822910301C84C
        021F9736C002807B03132951923952DAA0ED640E1FFED3A061F75A5C4A74E609
        2C9B812872B437E7B8D65ADAC088DDFD1329D1E041E64834EEA9A4F7E945A6A6
        5FD3D6991B6EBF91DB8AC01A04BB021CDABBC67808E4C016A20C4B974464B38E
        F79FC739D1709C5B3D776AF6EDCAD1FFF0F9827F10E0FAC834A600C970BA6E15
        5D0F5AF8F8E515C2F169E605C539CFFDBE672268C41AC4211010F2DED0429810
        CBE6E91FE6C9E79A4E31F97590FC9FEF6CAA5F8157CCE8C0B75A6B605710E4F3
        73E6C81B2D48FC982DB27A65351D373B9160E3CEE58C0DE64120E9910584205B
        BE7AB0020089545104E71B3BC938C7912BEB989D9B218E33ACDF51742F87221C
        50DE7CA96F382A2BAF95606142FA92382E9FDD4CD7501B93536304B9C70E6D4F
        561C5096FC094AE548858018284A0A007F01D3EF47C1BE4D3BFF000000004945
        4E44AE426082}
    end
    object dxBarButton2: TdxBarButton
      Action = actionAddChildRecord
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002E744558745469746C6500416464506172616772617068546F546162
        6C654F66436F6E74656E74733B5061726167726170683B03D2CAD80000021B49
        444154785EA5534D68134114FED6A6AD1E95825441A468150B9ED422551035A2
        82288878D1831E5ADA7A926E51243D5468D49C02ADD19EEC45F0A65E0C858A46
        410CE8418B62F1070F2D3D88825AB3C9CE9B31F3CD6012BC087EF078F3D837DF
        FBDE7EBB81310657EF7F9A31C05EA30D6CAD8DCD7067ED6B66D60FD3A7B6EC83
        079BD2773F9A7FC5F0D4AC4DF0810400325B44B106E0A6F36458FE51548E0522
        1A752001E5592C450AE3D30B968204B59534C223EB217EA53A342860F3E08176
        E6D6E6042880D303DBC3E8DEBC0A41102C03DCE3460200D9FCBC7F79DA65BFEB
        F0D10E8C4DF7E3CD6211BB339B669E84EFF6EFBCB2411C81DB8B8DE7926BB8C2
        F2966647E05588D2BC3CD997C699F1700F80D66717DEC73505CCC0CFD4104409
        BE8A4044A13D7B13A30F7A31BB50841683B9EF055ADA3DD6B1A42AFAA9575023
        58319A81315641822E88082F4FF6A631F7A38072FC0B03277A10C525E4EE3CDF
        4502D1B515BE0C0D4029A9868216C1BA1BB7D0B57A07CE4E84B470F0640FB2B7
        0B744345F2E8AF15566626989D0243EBC2E47554CA114E4F6D47542941B4C1CB
        91CF6DD679A740BC02185CBBF781BED77DCE8C8BC736A2B36D9B950DA94EB697
        B7A6D6464E8106D152F5FED2F14E771160D660415BCF277328BE5D44DFE1AE43
        002AAF2ECFA3E11D94CACA7B0F06A77380CB4D4D093C7EFDCDF644F0F00426DF
        9F7B71D068408CA64D9653F34C155400479A471DF83BFF0F7E03C130D63A7821
        85870000000049454E44AE426082}
    end
    object dxBarButton3: TdxBarButton
      Action = actionGuideExportAs
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000011744558745469746C65004578706F72743B5361766546D49E310000
        02A849444154785E75935D48145D1CC69F597752C3B24C5E83EAA58B280C5E2A
        56892EB22E22C88BA4ABBC780D0BCA2ECCD0A88B500991288A85C2A8E8830A82
        E0C525FB80B20431B008D74273D12CECA2CD6D5BF755576767E77CFC3B737649
        43FA313F660F67FFCF3C676040440B74A9BC50A05598FBCF2F9FDA5E67068ACB
        B38A0064298DB206133B941EFC012125AE1F7F09858709324E55B5EC5BBB29FF
        9DEF80778F1BDAE36746FB59BD0F43E9D1C9739ACA5C659EB2B0A265E974D80A
        507FB88D8E5EF689AD47CC4B797F19F9EE8C1E3E78C2DF05183B133FA2902450
        BE6B1B5E472E226A0F00A42FF8EBCF2162BDC712F36F74F77E40A0F3E160F433
        AF7603BCD50D7E56EAFB072F9E76E2E6D516907050D3B601574EDE01490E490C
        F15408E19920B864C8C95A86443C1777DB3B925E008694029C7108CE202561D6
        72C0B944D4EA5343FD1092E94121B9FE9D901388C46D4C4FA5900E10028C7325
        0317128280352BB6A0A6F90CD21838565B0A41025230F4F68EE34D772C141F13
        877580105237E09CEB06D39640EDDE7B906A3D339340C30D9F3A7B2146232378
        FE388CB1A1E4AD6F6F65D36C9462BF1A389C4328FFADAAD34F2229414458BF6E
        353808C1C14F78F6E83B56792A30FAE4FE690013ED1F778B7403B73EE328DE5C
        A2872408B393FF23168BA1A070251033D015209414D5C3B6090A6B63A5573436
        77410770B7418A213AFE15FFDD6EC53CF4B12CEB109A5AAF810903C3C3437009
        3DE070D101EE5939635A97D8940D0DB9170130111A194376F662388E03858788
        E602DC41E69ADED4C7A04C403AC2004B39F09AB99042426120C3FC77A0FFD4D8
        31022E094220735712A970068F9D849442A7CE0FE089C978CF40B0AF6C91E9C5
        60F00BB83054558694CD95496838834824C058EA95BB52FEF621E528CDCC7A6E
        6F21A4644A9B8874959FB191A9A729553D740000000049454E44AE426082}
    end
    object dxBarButton4: TdxBarButton
      Action = actionGuideReload
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001D744558745469746C65004C6F61643B53656C6563743B466F6C6465
        723B4F70656E96333E78000002AB49444154785EA5934F685C5514C67F6FF226
        A4D4A6D408420DD56ED2560B0A9A164349ED42DC48692B2EAA74E94690521AA4
        442A4141DD0522B4B6A894104D2BA6ED4263379A48FE2C12D48CD32668666286
        4E32D34966924966E6FDBDF7F4F166120864E7818FC3399CEFC7C7856B8808DB
        956118DBAE6B7D8B2902D4D564D67A04A0ABFFCFD0549BA39DBD13F2E137A33F
        9EFBE8EB676A3B885D6B1F8A5F6F97F8B5AAFEBEDA2EE3DD6D6737C0EF770F9E
        3C7F6578E2FCD511F7832B23F2533C271DD7470BE7BA7E7817A833D1F2DA0BEF
        7DB799CCB7D698E9EFF8F2ABCE53134372A6B57E4763FF89B6169EDDF7247591
        08590B4EBFB96BCFC79FFDD7070C98CAD3A08AE03E44B4C24471E8AD0B4DBBA7
        D389FB633BD9FFDCD3F40E8C9359580A0026274E1DE3DEC0AFAB85C5D4254047
        94AF4114281B4359E057A8D3799A9E3FCAFC32DCBD3BC43BAF3672EBF211CA96
        CDECBD3B24466E1E4FFCF2F90DC03395ABAA00DF4602B3F62D501E44B31CDEEB
        F0FAE1273872A08419F5F8EDD366527FCCD2DF339C034C0063F4935669BBD883
        5E9D62697A9295856CC86B6C7903C7F1318D0895C5299C621AD1C2726A091102
        095AC992E9B90AAB90C55E986335BBCE81B7BFC0084C688D8807DA43D4CB815C
        C47702D9A10AA924E3DFF7A54CE5288AE97956FEF9975DCD2F214E9172E26744
        FB88AABE8F681DCE288568850E948E059EA23F1826589E4F52CAE4D9FBCA7EEC
        4C0C7FBDB079189A4483F243900E419A5CF211F174E9B6E93A3EF96482866803
        F5A64F25F517CA5A03AD36534835456DA728ADD8140AD674CF586ECEB4CA2E92
        C9F0D48B07B1161FE0E41F82D2E1A1C886494208A243502E5D225FF20601CF2C
        97BD49FCB5D699DF279919061101A97601D0008208208216707D49C7162BDF02
        AE01EC00A24064DBDFB7B504F000BBF764B30208BFF3FFA9C7E0B9B422AAA90C
        AE0000000049454E44AE426082}
    end
    object dxBarButton5: TdxBarButton
      Action = actionGuideNew
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000003D744558745469746C65004E65773B506167653B426172733B526962
        626F6E3B5374616E646172643B4974656D3B426C616E6B3B44656661756C743B
        456D7074793B130452ED0000016349444154785E6D51314E04310C1CEF1E0289
        B7D0D3C035347C006A2A3AC413A8A9111D15E20B743420D1DE6310E2F612DBC8
        B11372DC269BC4BBF1CC8EC70B5505118D8F2FABB7611C970A850D3BEC2EC2B6
        D9A79CA68FDBABE33355DD2CE06300D1F2FAF2C8E21D10407EC6FEF0BC3A0130
        1AB01210B3946093045A49D4379BFE10F6F706E45C72879E002CCE2E1DDAF1F1
        1ACBE859B8C27A05A9D61DA0DE030A02BBA3AA80FE1178AA4803951C356564D0
        CEC4CCBB04393114A180C84F4F6B31A99794D2AE026CC2448181B7DB286122DB
        1245E61905927301899802448CE8405352569E333145FB44B5815BDD2EA3DDE7
        AC331E30438BC46656AB5B10C362CB9933910B525D41DF3E9BBD1A19C0336D44
        628163156182CF4AA643FB01F30C8164F7E0F060E11D28C904F5DF7B77C44B10
        F13A7B82BC9EBEDF6FEE5E4F15CD441069B09707146AA6F5D7A775BE27989EEE
        2FCE01EC0118AA2FFDD9C51AE01F03FE022E9833255C6BD8490000000049454E
        44AE426082}
    end
    object dxBarButton6: TdxBarButton
      Action = actionGuideRunSelected
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001B744558745469746C65004E6578743B506C61793B4172726F773B52
        6967687416E40EAE000002AF49444154785EA5925D6895751CC73FCFD14D46BE
        B41AF6C20A870425152C2F82A08B0ABD295D05416F572184045D2644082A425A
        831A62128E2E448C59108D9430D84A1C81DBD4D6B6D05C3B739B5B3BB1EDAC73
        CEF33CFFDF4BCFE1807076DB173E7CF95FFC3E7C2FFE91BB136501D600D556C0
        3F3839EC8262062A86A9A3E2881B224AE9DF949E032F90A396B5877A46C3BEAF
        0687DF3DDABB0B683CBAE7A99C064352458212AA484053218943F55D3BA4969C
        A8B2E7C5C71E3FF7EBFAEFDEEBFAA56FE1D6C4FECFF73E7D1908638BC1821A6A
        10D439DCD58FA8D7092255A3A5B9898EE71F213FBBF9B99F06365D7CFBE0F9AF
        676E0C1FDED6DC70030803B3154B122388616AF582204A2A30B562DCD5BC8137
        3AB6716DEC9ED7FBD76F7C75C3E6B3DD7F0EF67EFCCC834D7380EC78BFD7D457
        2D90E0A46A9412A5583688E081875A78B3F5DEC6CBD75AF636346D7AAB65EBB3
        9DBF5F387EE242D7AE2520D0E59EBBB320552AC1A8A4423938C58A905F4C3302
        AD5BEE67F72BDB373EB1BDFDC0932F7D34D4FE5AE73B4063DD822408A5D4AB20
        6224197195580966AC6B8878B47D0B0F6FBDAFB5E754F205701A48EE082AA540
        A124148A29E6206A04ABC9725184C6C6D0C84D4606AF2FAE14663E03A47E811A
        CB15214E0D7343CC897211EBD6464C8C4F73F5D278BCFCF7ADEEF93F7A3B97A7
        06E680A44E90A6CA526CC4C18822674D2EC7FCC43C570646AD3033F5CD3F13FD
        870AD7CFFD05C480B5ED3CEEAB048195B262EE141796B87A7194DBF97CDFCAF4
        9583B77F3B35045400FDF0C7593F7DE45B4484D50B982B14B3C33126C76F8E94
        1646F74F0F7ED9079401D9F7C3AC03A880AA60AB7EA2A6B1F27DF7F9A9CAE2E4
        91C94B9F9C014A4068DB71CCCD9C339F9EA5DAA686BB634E9DA0FCF38997EFA6
        961848DB761E7337C7CC5071DC0C73C7AB4419A60044EECEFFC97FFDEAC21326
        FC988F0000000049454E44AE426082}
    end
    object dxBarButton7: TdxBarButton
      Action = actionGuideRunAll
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002C744558745469746C65004E6578743B446F75626C653B4172726F77
        3B466173743B526577696E643B496E637265617365C977401F000002A4494441
        54785EA5937F2C947F00C73F59FA4145B1564C368DFAA3560B49E12F27B34EAD
        E5BEFB2E6B5487B14493CC8F6A213BD250A11885EE34CDF97147256EA2E3B843
        E28E76A8CC6EDCA8DCE122DE3DCFB3E30FFACF677BFFF1BCB6F7EBD93EEFE721
        00D695750BE8634265933126E1057D6B98CFCD26E215F996B85D1191801429F1
        4F6C21CB8C3E9BE3042A843F5548CEDF2A75A3C0162A5BEFBC1E5CCD4C5C2E8B
        C885E48F841D2F21AC9846E219F1861198C594F4A37D588724FEC06220AFB9FC
        C869EED1EAF65148D5BF5698FB7F4907E897D1A26EED0291690CE454683D2330
        8F2AFA8CEFB3C0F0F412848A0970B3E47309056D908F6831A09D43A57C1CDC6C
        F9AC6F7445A6BDF3393BA368C389103123D816F1B407CAA92588BF2DA255B388
        4ECD3C1ED40C8173EF03E29ECB51AFD2423AAA4746B51A67121A275D2FE6C553
        BD9DC63B223BB88F145068FFA07C701E2F9506F0550688870DA856E9702DBF07
        ACEB75887DA18050F573851D0FAAF8E2E89312440B2C02D365681E5B4071DF1C
        0A7B0DC8ED9A4176870E8F3B7528E9D5234B328EC3FF578227195BC368816540
        722BC443BFF1A47B06399D7A64CAF4784809D29A26C1E175C09D5B85E0EC56F0
        5A2656D8218EE0AB9D67E2555AB08B9D2081A07F16E96DD35459878C962904E7
        F5C2234C044EDA7BDCAD53E37EA306C1B99FE01C24FCB19F957AD7D4CCDAC638
        2FB1624537E0591755944E23AC7800EE212203275688941A2552A96268919261
        4E67728AB6DBBAD0739AD3733AB0F9CC0AD62743EA70E3D508BC221BE0E49F5F
        6B61EFE99DC557208AAFA6D83B86591DF477A52F9CCA467AC2DBEFA7C83EBF32
        46B0F3D8A52A38B20B657B9C43CF1AE7D9EDC1AD5DCD4CE9A2035B40ECFDF8C4
        CEB78CECF52E6504A6C692E5F207F22F66E35D42A830455B5629313EAFFF6FFC
        0B0C0C183FEAAD166E0000000049454E44AE426082}
    end
    object dxBarButton8: TdxBarButton
      Caption = 'actionSaveScript'
      Category = 0
      Enabled = False
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000000B744558745469746C6500536176653BF9E8F9090000020349444154
        785E7D52316B145110FEDEE6EE9A03EF2E57284A6C6DAC42C0262185829D0A36
        16820AFE010BADC47F60AB2022585A588875B01041822158D858081204956872
        DC5DD6DD3733CF99D93DD713CCC0F7BEF776E7FB66E6ED0654912982633EFCFC
        9F67A2603B2CAC9DBFB2810CEB4B274EE1E4D271B0303832A24450A94C113112
        2846ECED8F301A7DB59CD76F379E9F6D9901425A5F3DB38C4F9FC7B87BE70690
        60E1E49C80BC886049B87EF33656964F6373EBFD1A8096190466017352B0ABA6
        BF22C4D5C9C5972E5CD632010F1F3F0553C4CECE17088B8F9ED99292B898884C
        EFE2548B270711D942867E7F114717BB9E0308582A83962D2C6EE0EE2925EFA6
        245671899205ED561B9D4EA77EC7CA6D887063904CCCE29735C923BEEF4F91C4
        A771DC7FF008C7865DEF88896B23FA6310BC03226FEFEAB55B9A203E63823143
        5490445C3883D05F23F8FCC228F26F180C06B3CB730059BD57866F20229EDF18
        D42358BC78F60487C5EAB98B5A6408E63903B1F6BD358B9FE302F06D729610BC
        B2AE20B63CF9D780602072579FB9F98B6082E6DF8D044932FF156CA648EC89F7
        5E7E844802299851B3C26667F30BCAF31D043B3091DFC3917E0F5C895C1C4D3C
        03E095C79303A4CA20984111CBFCDDE6D6F6CA70D0C39B571F2A7164AF129589
        14C65E59B0FB631765596C03C8030033E929BA8A0C4D8443F6A2982AF67E037E
        F6AC9379188DF20000000049454E44AE426082}
    end
    object dxBarButton9: TdxBarButton
      Action = actionScriptRun
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C00000011744558745469746C6500506C61793B53746172743B5D
        61407A000001F049444154785EAD904B6B13511886DF938C4E5A31C560A5174C
        68E3056C91D07A5D98B6D616BC54105CC4DB527F801050BA511411A550D14271
        2314ACC5955D14A90B770A820B15A9422B0DB69149C64E9C2493CB3973267E84
        5884566DC0071E38B3380FEF1CFC3746A74FAA2353273A50252E54D035AB160C
        6F1F4C1D7F32FCEC58A0EA407231EB9692B160735784B9F07978F2E8D0ED89FE
        4D6B0E64CCA28B0B81B6C03E74EF8978B636745E56143677F7697FF4EAC821F5
        9F01291D0821912E7C43412CC1DF588F03A1015F53C3EE3B5E9F3A736BBCF7EC
        91D3ADAE3F066CDB814D811C37A0673EA168E7A0281C417F13426D875B37FB82
        8FC30381D7D71F75F7ACBE409420B8036E5BC88B3434F3030C2B46A13C366E50
        B06BFB36ECDC71707F9DB7F9E5E068F81C2A28CB0B245D1692E667E82C40C0CC
        C7CBAE73D7C0343D48680652BAF62A9DE2EF0030B2A4FCFE0B8253C0CE4250E0
        17564E85AE99588ACF7E492C583726EE7D9C0460AD5820CB6FE0A0C0CB0BE8DB
        83F822C7F785989ED20B43CFC7E6C68C643E0D809336595A1110E547CC607E5E
        428F7DCD657FF0876F5EC4EFCFBE3792008AA4241DAC465FA465CBA59B1DA58B
        D742F6F968FBF8DEDEC610002FB9BEF2D80C7FA3EF4C4BFD852BEDD35DA7FC3D
        00EA489574930C6B44216BC99ACA99A14AD8F2D42AF809500AE0796F04355100
        00000049454E44AE426082}
    end
    object dxBarButton10: TdxBarButton
      Caption = 'Compile'
      Category = 0
      Visible = ivAlways
    end
    object dxBarButton11: TdxBarButton
      Caption = #1042#1080#1076
      Category = 0
      Hint = #1042#1080#1076
      Visible = ivAlways
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = #1042#1080#1076
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton12: TdxBarButton
      Action = actionScriptCompile
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000021744558745469746C65004170706C793B4F4B3B436865636B3B4261
        72733B526962626F6E3B6463C8680000037D49444154785E4D8E7F4C94051CC6
        3FEF7B77E02073EA92742577579B684891E62AB6C4526B6013696BC9DC5A0B33
        D0322DA716D3CA94A21A8E96E976AEB654688E409B46B859CC249500E3F81588
        234EE0F875DC1DF7A3BB7BDFF7DBC16AEBD9F3D9BEFF3CCFF7C13555CB58A801
        40014CC5E5696BF638D24FBEF7EDF2D683550F7B0E5666B4969C5A5EBBEBCB65
        2F0209803A116E6438F82377A66A60385007A0E4EFB2A5BC51B1B4AEF4EC5AB9
        D476583A87AA642C7055BA47CE4A43F72752713157F67D93DE54B0DFBE04308D
        867E9E290050725F4BBDB7F8E8B29EAA86B7C4E5BF203DDEE3D23E71585AC6F6
        48E7E4C7D2E777C870A05E7E68DE277B4F668C6EDE6BCF00D4017F350A607EF5
        48DAB99CECBC9CF4343BC3E1264CAA60C282AAA8288A028A30313E852DE509EE
        0C4D72EEF26967CD17FD4F0EDE0A064DF9BBEDEB6CD6C51F3C9DF5382EFF1540
        104014216E500C2ED6DDA4F67C3BEDB79BC9C95EC3E8F8784AD28288BBADC1D3
        6C4E98652A7C2C7D2543816674430304C4885B0755E1CC99EBCC51D750F14E35
        DBCB32E91DF98DCCA5ABE8FCB36733E0500D3132EF9EAB108C7AE9ED1BA6B4AC
        969F2E39896A11CE5F68212529975D5B4A395A59C40B79CF6049D0489AAD81AA
        3C0A9854436741140FE148809AEA16CA8AAEA34C65F1E9E7F524EBEBD99A7F80
        53751FB2707118EB836642311F31C63174497C286BEE6C55D3F48971DF2088C1
        A60D6BF9BAB6849D0547D8FD520D2F3F5F822FD8C7AFCEEF58B16A11FEC82831
        3DC6A87F8868C488745C9D0C9AF5A8D2E51EF15BE72FD248B127E2F5FE8DE3FB
        FDEC28280755E1FDCFB691BF310B6FC48566C4C030F08D458984B40E4057837E
        ADAAA7CB87A0E2090EB2E491594C1A4DD45C2EC779AB0E53B287C4399384A353
        718288A8F4767B09F8F4F380069094BBDD7AB3E474869CB8B1428E5DCB90AAB6
        0DB2E59055B2B621C72EAF93134D99723C8EE3F79572A83A5336EEB439EF9A67
        990FA82A1071F7855EF9E35AC0D3EB0C010A9EF000799B56F1EEDBAFC7BF87D0
        0D411185BEEE30AD8DFE88AB2B501CF0C4FC5706DE34CC0D7F15E9AB53BF6A17
        784ED78C4AB72BF6803DDD82B6B013D5A420064CB875FABB628CB8A21DEEDBA1
        A2D6FAB11B8066480C7EE92F045000737CD6BCA736DFB77F7D616A63EE769BCC
        B0C326CF6E4D6D5B5D70FF47C9732CF700164099CE4D3373FCA76CAB43052CFF
        62065440001D884E130F19FC4FFF00FE20CB5D5DF1FFF30000000049454E44AE
        426082}
    end
  end
  object ActionList1: TActionList
    Left = 56
    Top = 64
    object actionAddRecord: TAction
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      OnExecute = actionAddRecordExecute
    end
    object actionAddChildRecord: TAction
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100' '#1087#1086#1076#1095#1080#1085#1105#1085#1085#1099#1081
      Enabled = False
      OnExecute = actionAddChildRecordExecute
    end
    object actionRemoveRecord: TAction
      Caption = #1059#1076#1072#1083#1080#1090#1100
      OnExecute = actionRemoveRecordExecute
    end
    object actionGuideExport: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
      Enabled = False
      OnExecute = actionGuideExportExecute
    end
    object actionGuideExportAs: TAction
      Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1082#1072#1082'...'
      OnExecute = actionGuideExportAsExecute
    end
    object actionGuideReload: TAction
      Caption = 'actionReload'
      OnExecute = actionGuideReloadExecute
    end
    object actionGuideOpen: TAction
      Caption = #1054#1090#1082#1088#1099#1090#1100
      OnExecute = actionGuideOpenExecute
    end
    object actionGuideNew: TAction
      Caption = #1057#1086#1079#1076#1072#1090#1100
      OnExecute = actionGuideNewExecute
    end
    object actionScriptCompile: TAction
      Caption = #1055#1088#1086#1074#1077#1088#1080#1090#1100' '#1085#1072' '#1086#1096#1080#1073#1082#1080
      OnExecute = actionScriptCompileExecute
    end
    object actionScriptSave: TAction
      Caption = 'actionScriptSave'
      OnExecute = actionScriptSaveExecute
    end
    object actionScriptRun: TAction
      Caption = #1042#1099#1073#1088#1072#1085#1085#1099#1081' ('#1090#1086#1083#1100#1082#1086' '#1089#1082#1088#1080#1087#1090')'
      OnExecute = actionScriptRunExecute
    end
    object actionGuideRunAll: TAction
      Caption = #1042#1089#1077
      OnExecute = actionGuideRunAllExecute
    end
    object actionGuideRunSelected: TAction
      Caption = #1042#1099#1073#1088#1072#1085#1085#1099#1081
      OnExecute = actionGuideRunSelectedExecute
    end
    object actionFindPanelShow: TAction
      Caption = 'actionFindPanelShow'
      ShortCut = 16454
      OnExecute = actionFindPanelShowExecute
    end
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = actionCloseExecute
    end
    object actionHelp: TAction
      Caption = #1054' '#1087#1088#1086#1075#1088#1072#1084#1084#1077
      OnExecute = actionHelpExecute
    end
    object actionClearRecentList: TAction
      Caption = #1054#1095#1080#1089#1090#1080#1090#1100
      OnExecute = actionClearRecentListExecute
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.xml'
    Filter = 'XML|*.xml'
    Left = 728
    Top = 416
  end
  object ImageList1: TImageList
    Left = 272
    Top = 168
    Bitmap = {
      494C010103000800680010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FFCACACA35FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFF
      FF00FFFFFF00000000FF000000FFFFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000FFC5C5C53A0000000000000000000000000000
      00FF000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF000000
      00FFFFFFFF00FFFFFF00FFFFFF00000000FF0000000000000000000000000000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF000000
      00FF000000FFFFFFFF00FFFFFF00000000FF0000000000000000000000000000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00FFFFFF00FFFFFF000000
      00FFFFFFFF00FFFFFF00FFFFFF00000000FF0000000000000000000000000000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00000000FF494949B60000000000000000000000000000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FFFFFFFF00FFFF
      FF00FFFFFF00000000FF000000FFFFFFFF000000000000000000000000000000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF0000
      00FF000000FF000000FFFFFFFF00FFFFFF000000000000000000000000000000
      00FF00000000000000FF000000FF000000FF000000FF000000FF000000FF0000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000FF0000
      00FF000000FF000000FF000000FF000000FF0000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00FFFFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000000000000000000000000000FF0000000000000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00FFFFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF0000000000000000000000000000000000000000000000FF000000FF0000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF00000000000000000000000000000000000000FF00000000000000000000
      000000000000000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000
      00FFFFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF0000000000000000000000000000000000000000000000FF000000000000
      00FF000000FF0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF00000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF0000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF000000FF0000
      00FFFFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF0000000000000000000000000000000000000000000000FF000000FF0000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00FF000000000000000000000000000000000000000000000000000000000000
      0000000000FF000000000000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FFFFFFFF00000000FF0000
      00FFFFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF0000000000000000000000000000000000000000000000FF000000FF0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDFD
      FD02000000FF000000FF000000FF000000FF000000FF000000FF000000FF0000
      00FFFDFDFD02000000000000000000000000000000FFFFFFFF00FFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00000000FF000000FF000000FFFFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      00FF000000FF000000FF000000FF000000FF000000FF0D0D0DF2000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000FF000000FF000000FF0000
      00FF000000FF000000FF000000FF000000FF000000FF000000FFFFFFFF00FFFF
      FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF0000FFFF0000FFFF0000FFFF0000
      C0030000E0070000DFFB0000EFF70000DFFB0000E8170000DFFB0000EFF70000
      DFFB0000E8170000DFFB0000EFF70000DFFB0000E8170000C0FB0000EFF70000
      EEFB0000EF870000EF7B0000EFA70000EF030000EF8F0000EFF70000EF9F0000
      E0070000E03F0000FFFF0000FFFF000000000000000000000000000000000000
      000000000000}
  end
  object cxStyleRepository1: TcxStyleRepository
    Left = 728
    Top = 80
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clMenuHighlight
    end
    object cxStyle2: TcxStyle
    end
    object cxStyle3: TcxStyle
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 733
    Top = 543
  end
  object DataSourceNodeType: TDataSource
    DataSet = DataSetNodeType
    Left = 960
    Top = 360
  end
  object DataSetNodeType: TClientDataSet
    PersistDataPacket.Data = {
      870000009619E0BD010000001800000002000300000003000000410002696404
      000100100000000763617074696F6E02004A0010000100055749445448020002
      00FE0100000000000000000C0066006F006C006400650072000000010000000C
      0073006300720069007000740000000200000016006400650073006300720069
      007000740069006F006E00}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 968
    Top = 416
    object DataSetNodeTypeid: TIntegerField
      FieldName = 'id'
    end
    object DataSetNodeTypecaption: TWideStringField
      FieldName = 'caption'
      Size = 255
    end
  end
  object ScrPascalMemoStyler1: TScrPascalMemoStyler
    AutoBlockEnd = True
    BlockStart = 'begin,try,case,class,record,interface,implementation'
    BlockEnd = 'end'
    LineComment = '//'
    MultiCommentLeft = '{'
    MultiCommentRight = '}'
    CommentStyle.TextColor = clNavy
    CommentStyle.BkColor = clNone
    CommentStyle.Style = [fsItalic]
    NumberStyle.TextColor = clFuchsia
    NumberStyle.BkColor = clNone
    NumberStyle.Style = [fsBold]
    HighlightStyle.TextColor = clWhite
    HighlightStyle.BkColor = clRed
    HighlightStyle.Style = [fsBold]
    AllStyles = <
      item
        KeyWords.Strings = (
          'UNIT'
          'INTERFACE'
          'IMPLEMENTATION'
          'USES'
          'CONST'
          'PROGRAM'
          'PRIVATE'
          'PUBLIC'
          'PUBLISHED'
          'PROTECTED'
          'PROPERTY'
          'FUNCTION'
          'FINALISE'
          'INITIALISE'
          'VAR'
          'BEGIN'
          'WITH'
          'END'
          'FOR'
          'TO'
          'DO'
          'NOT'
          'IF'
          'THEN'
          'ELSE'
          'TYPE'
          'WHILE'
          'REPEAT'
          'UNTIL'
          'BREAK'
          'CONTINUE'
          'VIRTUAL'
          'OVERRIDE'
          'DEFAULT'
          'CLASS'
          'STORED'
          'INHERITED'
          'PROCEDURE'
          'CONSTRUCTOR'
          'DESTRUCTOR'
          'FINALLY'
          'RAISE'
          'STRING'
          'TRY'
          'EXCEPT'
          'STDCALL'
          'CDECL'
          'PASCAL'
          'NIL'
          'CASE'
          'REINTRODUCE'
          'PACKED'
          'RECORD'
          'MESSAGE'
          'IN'
          'IS'
          'SHL'
          'SHR'
          'MOD'
          'DIV'
          'XOR'
          'OR'
          'AND'
          'OF'
          'SET'
          'DOWNTO'
          'EXPORTS'
          'LIBRARY'
          'AS'
          'ASM'
          'DYNAMIC'
          'OBJECT'
          'THREADVAR'
          'FILE'
          'ABSTRACT'
          'OVERLOAD'
          'ASSEMBLER'
          'ABSOLUTE'
          'AUTOMATED'
          'EXTERNAL'
          'REGISTER'
          'DISPINTERFACE'
          'RESOURCESTRING'
          'NEAR'
          'FAR'
          'LABEL'
          'OUT'
          'SAFECALL'
          'DISPID'
          'ARRAY'
          'INLINE'
          'FORWARD'
          'PLATFORM'
          'DEPRECATED')
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGreen
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = [fsBold]
        BGColor = clNone
        StyleType = stKeyword
        BracketStart = #0
        BracketEnd = #0
        Info = 'Pascal Standard Default'
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        BGColor = clNone
        StyleType = stBracket
        BracketStart = #39
        BracketEnd = #39
        Info = 'Simple Quote'
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clBlue
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        BGColor = clNone
        StyleType = stBracket
        BracketStart = '"'
        BracketEnd = '"'
        Info = 'Double Quote'
      end
      item
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        BGColor = clNone
        StyleType = stSymbol
        BracketStart = #0
        BracketEnd = #0
        Symbols = ' ,;:.(){}[]=+-*/^%<>#'#13#10
        Info = 'Symbols Delimiters'
      end
      item
        CommentLeft = '(*'
        CommentRight = '*)'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clGray
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = [fsItalic]
        BGColor = clNone
        StyleType = stComment
        BracketStart = #0
        BracketEnd = #0
        Info = 'Multi line comment'
      end>
    AutoCompletion.Strings = (
      'ShowMessage'
      'MessageDlg')
    HintParameter.TextColor = clBlack
    HintParameter.BkColor = clInfoBk
    HintParameter.HintCharStart = '('
    HintParameter.HintCharEnd = ')'
    HintParameter.HintCharDelimiter = ';'
    HintParameter.HintClassDelimiter = '.'
    HintParameter.HintCharWriteDelimiter = ','
    HintParameter.Parameters.Strings = (
      'ShowMessage(const Msg: string);'
      
        'MessageDlg(const Msg: string; DlgType: TMsgDlgType; Buttons: TMs' +
        'gDlgButtons; HelpCtx: Longint): Integer);')
    HexIdentifier = '$'
    Description = 'Pascal'
    Filter = 'Pascal Files (*.pas,*.dpr,*.dpk,*.inc)|*.pas;*.dpr;*.dpk;*.inc'
    DefaultExtension = '.pas'
    StylerName = 'Pascal'
    Extensions = 'pas;dpr;dpk;inc'
    RegionDefinitions = <
      item
        Identifier = 'procedure'
        RegionStart = 'begin'
        RegionEnd = 'end'
        RegionType = rtClosed
        ShowComments = False
      end
      item
        Identifier = 'procedure'
        RegionEnd = 'forward'
        RegionType = rtClosed
        ShowComments = False
      end
      item
        Identifier = 'constructor'
        RegionStart = 'begin'
        RegionEnd = 'end'
        RegionType = rtClosed
        ShowComments = False
      end
      item
        Identifier = 'destructor'
        RegionStart = 'begin'
        RegionEnd = 'end'
        RegionType = rtClosed
        ShowComments = False
      end
      item
        Identifier = 'interface'
        RegionStart = 'interface'
        RegionType = rtOpen
        ShowComments = False
      end
      item
        Identifier = 'unit'
        RegionStart = 'unit'
        RegionType = rtFile
        ShowComments = False
      end
      item
        Identifier = 'implementation'
        RegionStart = 'implementation'
        RegionType = rtOpen
        ShowComments = False
      end
      item
        Identifier = 'case'
        RegionStart = 'case'
        RegionEnd = 'end'
        RegionType = rtIgnore
        ShowComments = False
      end
      item
        Identifier = 'try'
        RegionStart = 'try'
        RegionEnd = 'end'
        RegionType = rtIgnore
        ShowComments = False
      end
      item
        Identifier = 'function'
        RegionStart = 'begin'
        RegionEnd = 'end'
        RegionType = rtClosed
        ShowComments = False
      end
      item
        Identifier = '{$region'
        RegionStart = '{$region'
        RegionEnd = '{$endregion'
        RegionType = rtClosed
        ShowComments = False
      end>
    Left = 656
    Top = 276
  end
  object ScrMemoSource1: TScrMemoSource
    Lines.Strings = (
      '')
    ReadOnly = False
    Left = 656
    Top = 220
  end
  object MainMenu1: TMainMenu
    Left = 176
    Top = 67
    object N3: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088' '#1089#1094#1077#1085#1072#1088#1080#1077#1074
      object actionHelp1: TMenuItem
        Action = actionHelp
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object actionClose1: TMenuItem
        Action = actionClose
      end
    end
    object N8: TMenuItem
      Caption = #1044#1086#1082#1091#1084#1077#1085#1090
      object actionNew1: TMenuItem
        Action = actionGuideNew
      end
      object actionOpen1: TMenuItem
        Action = actionGuideOpen
      end
      object actionSave1: TMenuItem
        Action = actionGuideExport
      end
      object actionExport1: TMenuItem
        Action = actionGuideExportAs
      end
      object N11: TMenuItem
        Caption = '-'
      end
      object N10: TMenuItem
        Caption = #1053#1077#1076#1072#1074#1085#1080#1077
        object N12: TMenuItem
          Caption = '-'
        end
        object N13: TMenuItem
          Action = actionClearRecentList
        end
      end
    end
    object N5: TMenuItem
      Caption = #1057#1094#1077#1085#1072#1088#1080#1080
      object actionCompileScript1: TMenuItem
        Action = actionScriptCompile
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object actionAddRecord1: TMenuItem
        Action = actionAddRecord
      end
      object actionAddChildRecord1: TMenuItem
        Action = actionAddChildRecord
      end
      object actionRemoveRecord1: TMenuItem
        Action = actionRemoveRecord
      end
      object N7: TMenuItem
        Caption = '-'
      end
    end
    object N14: TMenuItem
      Caption = #1042#1099#1087#1086#1083#1085#1080#1090#1100
      object actionRunScript1: TMenuItem
        Action = actionScriptRun
      end
      object actionRunSelected1: TMenuItem
        Action = actionGuideRunSelected
      end
      object actionRunAll1: TMenuItem
        Action = actionGuideRunAll
      end
    end
    object N1: TMenuItem
      Caption = #1042#1080#1076
      object N2: TMenuItem
        Caption = #1055#1072#1085#1077#1083#1080
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 304
    Top = 51
    object actionAddRecord2: TMenuItem
      Action = actionAddRecord
    end
    object actionAddChildRecord2: TMenuItem
      Action = actionAddChildRecord
    end
    object actionRemoveRecord2: TMenuItem
      Action = actionRemoveRecord
    end
    object N9: TMenuItem
      Caption = '-'
    end
    object actionCompileScript2: TMenuItem
      Action = actionScriptCompile
    end
    object N15: TMenuItem
      Caption = '-'
    end
    object N16: TMenuItem
      Action = actionGuideRunSelected
    end
    object actionRunScript2: TMenuItem
      Action = actionScriptRun
    end
  end
end
