object OPPGuideForm: TOPPGuideForm
  Left = 0
  Top = 0
  Caption = 'OPPGuideForm'
  ClientHeight = 1061
  ClientWidth = 2242
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Segoe UI'
  Font.Style = []
  WindowState = wsMaximized
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 30
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 44
    Width = 2242
    Height = 937
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ManagerFont = False
    Align = alClient
    DockingType = 5
    OriginalWidth = 2242
    OriginalHeight = 937
    object dxLayoutDockSite2: TdxLayoutDockSite
      Left = 0
      Top = 0
      Width = 2242
      Height = 441
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
        Width = 1600
        Height = 441
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
          Width = 958
          Height = 441
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          ManagerFont = False
          ParentFont = True
          ExplicitWidth = 944
          ExplicitHeight = 440
          DockingType = 0
          OriginalWidth = 600
          OriginalHeight = 400
        end
        object dxVertContainerDockSite2: TdxVertContainerDockSite
          Left = 958
          Top = 0
          Width = 642
          Height = 441
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
          ExplicitLeft = 944
          ExplicitHeight = 440
          DockingType = 3
          OriginalWidth = 642
          OriginalHeight = 280
          object dxDockPanel5: TdxDockPanel
            Left = 0
            Top = 0
            Width = 642
            Height = 221
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Segoe UI'
            Font.Style = []
            ManagerFont = False
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
            Top = 221
            Width = 642
            Height = 220
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Segoe UI'
            Font.Style = []
            ManagerFont = False
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
        Height = 441
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
          Height = 147
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ManagerColor = False
          ManagerFont = False
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
            Height = 107
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
            FindPanel.DisplayMode = fpdmManual
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Segoe UI'
            Font.Style = []
            Images = ImageList1
            LookAndFeel.Kind = lfFlat
            LookAndFeel.SkinName = ''
            Navigator.Buttons.CustomButtons = <>
            OptionsBehavior.ImmediateEditor = False
            OptionsBehavior.CopyCaptionsToClipboard = False
            OptionsSelection.InvertSelect = False
            OptionsView.FixedSeparatorWidth = 4
            OptionsView.Headers = False
            OptionsView.IndicatorWidth = 16
            OptionsView.NavigatorOffset = 100
            ParentFont = False
            Preview.LeftIndent = 10
            Preview.RightIndent = 10
            RootValue = -1
            ScrollbarAnnotations.CustomAnnotations = <>
            TabOrder = 0
            OnDragDrop = cxDBTreeList1DragDrop
            OnDragOver = cxDBTreeList1DragOver
            OnFindCriteriaChanged = cxDBTreeList1FindCriteriaChanged
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
          Top = 147
          Width = 642
          Height = 147
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ManagerFont = False
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
          object cxDBMemo1: TcxDBMemo
            Left = 0
            Top = 28
            Align = alClient
            DataBinding.DataField = 'Script'
            DataBinding.DataSource = DataSourceTreeView
            ParentFont = False
            Properties.ScrollBars = ssBoth
            Style.BorderStyle = ebsNone
            Style.Font.Charset = DEFAULT_CHARSET
            Style.Font.Color = clWindowText
            Style.Font.Height = -21
            Style.Font.Name = 'Segoe UI'
            Style.Font.Style = []
            Style.LookAndFeel.Kind = lfFlat
            Style.LookAndFeel.NativeStyle = False
            Style.IsFontAssigned = True
            StyleDisabled.LookAndFeel.Kind = lfFlat
            StyleDisabled.LookAndFeel.NativeStyle = False
            StyleFocused.BorderStyle = ebsNone
            StyleFocused.LookAndFeel.Kind = lfFlat
            StyleFocused.LookAndFeel.NativeStyle = False
            StyleHot.BorderStyle = ebsNone
            StyleHot.LookAndFeel.Kind = lfFlat
            StyleHot.LookAndFeel.NativeStyle = False
            TabOrder = 1
            Height = 79
            Width = 638
          end
        end
        object dxDockPanelProperties: TdxDockPanel
          Left = 0
          Top = 294
          Width = 642
          Height = 147
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -21
          Font.Name = 'Segoe UI'
          Font.Style = []
          ManagerFont = False
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
            Height = 107
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            BorderStyle = cxcbsNone
            Align = alClient
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -21
            Font.Name = 'Segoe UI'
            Font.Style = []
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
            OptionsData.Appending = False
            OptionsData.Deleting = False
            OptionsData.DeletingConfirmation = False
            OptionsData.Inserting = False
            Navigator.Buttons.CustomButtons = <>
            Navigator.InfoPanel.Visible = True
            ParentFont = False
            ScrollbarAnnotations.CustomAnnotations = <>
            TabOrder = 0
            DataController.DataSource = DataSourceTreeView
            ExplicitHeight = 106
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
            object cxDBVerticalGrid1DBEditorRow3: TcxDBEditorRow
              Properties.DataBinding.FieldName = 'ReactionIdentifier'
              ID = 2
              ParentID = -1
              Index = 2
              Version = 1
            end
            object cxDBVerticalGrid1DBEditorRow4: TcxDBEditorRow
              Properties.DataBinding.FieldName = 'ActualResultIdentifier'
              ID = 3
              ParentID = -1
              Index = 3
              Version = 1
            end
            object cxDBVerticalGrid1DBEditorRow2: TcxDBEditorRow
              Properties.DataBinding.FieldName = 'ActionIdentifier'
              ID = 4
              ParentID = -1
              Index = 4
              Version = 1
            end
            object cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow
              Properties.DataBinding.FieldName = 'Identifier'
              Properties.Options.Editing = False
              Properties.Options.Filtering = False
              Properties.Options.FilteringWithFindPanel = False
              Properties.Options.IncSearch = False
              Properties.Options.ShowEditButtons = eisbNever
              ID = 5
              ParentID = -1
              Index = 5
              Version = 1
            end
          end
        end
      end
    end
    object dxDockPanelOutputLog: TdxDockPanel
      Left = 0
      Top = 441
      Width = 2242
      Height = 496
      Margins.Left = 6
      Margins.Top = 6
      Margins.Right = 6
      Margins.Bottom = 6
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ManagerFont = False
      AllowFloating = True
      AutoHide = False
      Caption = 'Log'
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      ExplicitTop = 440
      ExplicitWidth = 2228
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
        ParentFont = False
        Properties.ReadOnly = True
        Properties.ScrollBars = ssBoth
        Style.BorderStyle = ebsNone
        Style.Font.Charset = DEFAULT_CHARSET
        Style.Font.Color = clWindowText
        Style.Font.Height = -21
        Style.Font.Name = 'Segoe UI'
        Style.Font.Style = []
        Style.LookAndFeel.Kind = lfFlat
        Style.LookAndFeel.SkinName = ''
        Style.IsFontAssigned = True
        StyleDisabled.LookAndFeel.Kind = lfFlat
        StyleDisabled.LookAndFeel.SkinName = ''
        StyleFocused.LookAndFeel.Kind = lfFlat
        StyleFocused.LookAndFeel.SkinName = ''
        StyleHot.LookAndFeel.Kind = lfFlat
        StyleHot.LookAndFeel.SkinName = ''
        TabOrder = 0
        ExplicitWidth = 2224
        Height = 456
        Width = 2238
      end
    end
  end
  object dxBarDockControl1: TdxBarDockControl
    Left = 0
    Top = 0
    Width = 2242
    Height = 44
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = dalTop
    BarManager = dxBarManager1
  end
  object dxStatusBar1: TdxStatusBar
    Left = 0
    Top = 981
    Width = 2242
    Height = 40
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <
      item
        PanelStyleClassName = 'TdxStatusBarTextPanelStyle'
      end>
    ParentFont = True
    ExplicitTop = 980
    ExplicitWidth = 2228
  end
  object dxStatusBar2: TdxStatusBar
    Left = 0
    Top = 1021
    Width = 2242
    Height = 40
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Panels = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    ExplicitTop = 1020
    ExplicitWidth = 2228
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
    Font.Height = -21
    Font.Name = 'Segoe UI'
    Font.Style = []
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    LookAndFeel.SkinName = ''
    ResizeZonesWidth = 6
    SelectionFrameWidth = 8
    Left = 736
    Top = 304
    PixelsPerInch = 192
  end
  object DataSourceTreeView: TDataSource
    DataSet = DataSetTreeView
    OnDataChange = DataSourceTreeViewDataChange
    Left = 957
    Top = 104
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
    AfterApplyUpdates = DataSetTreeViewAfterApplyUpdates
    Left = 960
    Top = 216
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
    PixelsPerInch = 192
    object dxBarManager1Bar1: TdxBar
      Caption = 'default'
      CaptionButtons = <>
      DockControl = dxBarDockControl1
      DockedDockControl = dxBarDockControl1
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 608
      FloatTop = 377
      FloatClientWidth = 51
      FloatClientHeight = 295
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -21
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton5'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton1'
        end
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end
        item
          BeginGroup = True
          Visible = True
          ItemName = 'dxBarButton6'
        end
        item
          Visible = True
          ItemName = 'dxBarButton7'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = True
      Visible = True
      WholeRow = False
    end
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
      Font.Height = -6
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton8'
        end
        item
          Visible = True
          ItemName = 'dxBarButton9'
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
      Action = actionExport
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
      Action = actionReload
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
      Action = actionNew
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
      Action = actionRunSelected
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
      Action = actionRunAll
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
      Action = actionSaveScript
      Category = 0
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
      Action = actionRunScript
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
  end
  object ActionList1: TActionList
    Left = 56
    Top = 152
    object actionAddRecord: TAction
      Caption = 'actionAddRecord'
      OnExecute = actionAddRecordExecute
    end
    object actionAddChildRecord: TAction
      Caption = 'actionAddChildRecord'
      Enabled = False
      OnExecute = actionAddChildRecordExecute
    end
    object actionRemoveRecord: TAction
      Caption = 'actionRemoveRecord'
    end
    object actionExport: TAction
      Caption = 'actionExport'
      OnExecute = actionExportExecute
    end
    object actionReload: TAction
      Caption = 'actionReload'
      OnExecute = actionReloadExecute
    end
    object actionOpen: TAction
      Caption = 'actionOpen'
    end
    object actionNew: TAction
      Caption = 'actionNew'
      OnExecute = actionNewExecute
    end
    object actionRunSelected: TAction
      Caption = 'actionRunSelected'
      OnExecute = actionRunSelectedExecute
    end
    object actionRunAll: TAction
      Caption = 'actionRunAll'
      OnExecute = actionRunAllExecute
    end
    object actionSaveScript: TAction
      Caption = 'actionSaveScript'
      Enabled = False
      OnExecute = actionSaveScriptExecute
    end
    object actionRunScript: TAction
      Caption = 'actionRunScript'
      OnExecute = actionRunScriptExecute
    end
    object actionShowFindPanel: TAction
      Caption = 'actionShowFindPanel'
      ShortCut = 16454
      OnExecute = actionShowFindPanelExecute
    end
  end
  object SaveDialog1: TSaveDialog
    Left = 728
    Top = 416
  end
  object ImageList1: TImageList
    Left = 272
    Top = 168
    Bitmap = {
      494C010103000800040010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
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
    PixelsPerInch = 192
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clMenuHighlight
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
    Left = 1240
    Top = 96
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
    Left = 1240
    Top = 208
    object DataSetNodeTypeid: TIntegerField
      FieldName = 'id'
    end
    object DataSetNodeTypecaption: TWideStringField
      FieldName = 'caption'
      Size = 255
    end
  end
end
