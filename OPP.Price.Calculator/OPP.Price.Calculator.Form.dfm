object OPPPriceCalculator: TOPPPriceCalculator
  Left = 0
  Top = 0
  Caption = #1043#1054#1051#1068#1060#1057#1058#1056#1048#1052': '#1050#1072#1083#1100#1082#1091#1083#1103#1094#1080#1080' '#1085#1072' '#1079#1072#1082#1072#1079
  ClientHeight = 634
  ClientWidth = 963
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 0
    Width = 963
    Height = 614
    Align = alClient
    ExplicitHeight = 592
    DockingType = 5
    OriginalWidth = 963
    OriginalHeight = 614
    object dxLayoutDockSite1: TdxLayoutDockSite
      Left = 185
      Top = 0
      Width = 778
      Height = 614
      ExplicitHeight = 592
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
      object dxLayoutDockSite2: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 593
        Height = 614
        DockingType = 0
        OriginalWidth = 300
        OriginalHeight = 200
        object dxLayoutDockSite4: TdxLayoutDockSite
          Left = 0
          Top = 140
          Width = 593
          Height = 474
          DockingType = 0
          OriginalWidth = 300
          OriginalHeight = 200
          object dxLayoutDockSite5: TdxLayoutDockSite
            Left = 0
            Top = 0
            Width = 593
            Height = 474
            ExplicitWidth = 300
            ExplicitHeight = 200
            DockingType = 0
            OriginalWidth = 300
            OriginalHeight = 200
          end
          object dxDockPanel3: TdxDockPanel
            Left = 0
            Top = 0
            Width = 593
            Height = 474
            AllowFloating = True
            AutoHide = False
            Caption = #1047#1072#1082#1072#1079
            CustomCaptionButtons.Buttons = <>
            TabsProperties.CustomButtons.Buttons = <>
            ExplicitTop = 72
            ExplicitHeight = 402
            DockingType = 0
            OriginalWidth = 185
            OriginalHeight = 140
            object cxGrid2: TcxGrid
              Left = 0
              Top = 3
              Width = 589
              Height = 447
              Align = alClient
              TabOrder = 0
              OnEnter = cxGrid2Enter
              OnResize = cxGrid2Resize
              LookAndFeel.Kind = lfFlat
              LookAndFeel.NativeStyle = False
              ExplicitLeft = 2
              object cxGrid2DBTableView1: TcxGridDBTableView
                Navigator.Buttons.CustomButtons = <>
                DataController.DataSource = OrderDatasource
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
                OptionsSelection.InvertSelect = False
                OptionsView.GroupByBox = False
                OnColumnSizeChanged = cxGrid2DBTableView1ColumnSizeChanged
                object cxGrid2DBTableView1Column1: TcxGridDBColumn
                  DataBinding.FieldName = 'PriceItemName'
                  Width = 35
                  IsCaptionAssigned = True
                end
                object cxGrid2DBTableView1Column3: TcxGridDBColumn
                  DataBinding.FieldName = 'ItemPrice'
                  Width = 77
                end
                object cxGrid2DBTableView1Column4: TcxGridDBColumn
                  DataBinding.FieldName = 'PredictedPrice'
                  Width = 105
                end
              end
              object cxGrid2Level1: TcxGridLevel
                GridView = cxGrid2DBTableView1
              end
            end
            object dxBarDockControl1: TdxBarDockControl
              Left = 0
              Top = 0
              Width = 589
              Align = dalTop
              BarManager = dxBarManager1
            end
          end
        end
        object dxDockPanel2: TdxDockPanel
          Left = 0
          Top = 0
          Width = 593
          Height = 140
          AllowFloating = True
          AutoHide = False
          Caption = #1052#1077#1090#1086#1076#1080#1082#1072
          CustomCaptionButtons.Buttons = <>
          TabsProperties.CustomButtons.Buttons = <>
          ExplicitWidth = 185
          DockingType = 2
          OriginalWidth = 185
          OriginalHeight = 140
          object cxGrid1: TcxGrid
            Left = 0
            Top = 3
            Width = 589
            Height = 113
            Align = alClient
            TabOrder = 0
            OnEnter = cxGrid1Enter
            OnResize = cxGrid1Resize
            LookAndFeel.Kind = lfFlat
            LookAndFeel.NativeStyle = False
            object cxGrid1DBTableView1: TcxGridDBTableView
              PopupMenu = PopupMenu1
              Navigator.Buttons.CustomButtons = <>
              DataController.DataSource = MethologyDatasource
              DataController.Summary.DefaultGroupSummaryItems = <>
              DataController.Summary.FooterSummaryItems = <>
              DataController.Summary.SummaryGroups = <>
              OptionsSelection.InvertSelect = False
              OptionsView.GroupByBox = False
              OnColumnSizeChanged = cxGrid1DBTableView1ColumnSizeChanged
              object cxGrid1DBTableView1Column1: TcxGridDBColumn
                DataBinding.FieldName = 'PriceItemName'
                Width = 34
                IsCaptionAssigned = True
              end
              object cxGrid1DBTableView1Column2: TcxGridDBColumn
                DataBinding.FieldName = 'Formula'
                Width = 220
              end
              object cxGrid1DBTableView1Column3: TcxGridDBColumn
                DataBinding.FieldName = 'KoefP'
                Width = 40
              end
              object cxGrid1DBTableView1Column4: TcxGridDBColumn
                DataBinding.FieldName = 'KoefR'
                Width = 40
              end
            end
            object cxGrid1Level1: TcxGridLevel
              GridView = cxGrid1DBTableView1
            end
          end
          object dxBarDockControl2: TdxBarDockControl
            Left = 0
            Top = 0
            Width = 589
            Align = dalTop
            BarManager = dxBarManager1
          end
        end
      end
      object dxDockPanel5: TdxDockPanel
        Left = 593
        Top = 0
        Width = 185
        Height = 614
        AllowFloating = True
        AutoHide = False
        Caption = 'dxDockPanel5'
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        DockingType = 3
        OriginalWidth = 185
        OriginalHeight = 140
        object cxDBVerticalGrid1: TcxDBVerticalGrid
          Left = 0
          Top = 0
          Width = 181
          Height = 570
          Align = alClient
          LookAndFeel.Kind = lfFlat
          LookAndFeel.NativeStyle = False
          Navigator.Buttons.CustomButtons = <>
          TabOrder = 0
          OnEdited = cxDBVerticalGrid1Edited
          DataController.DataSource = PropertiesDatasource
          DataController.GridMode = True
          ExplicitWidth = 185
          ExplicitHeight = 120
          Version = 1
          object cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow
            ID = 1
            ParentID = -1
            Index = 0
            Version = 1
          end
        end
        object dxStatusBar1: TdxStatusBar
          Left = 0
          Top = 570
          Width = 181
          Height = 20
          Panels = <>
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ExplicitTop = 120
          ExplicitWidth = 185
        end
      end
    end
    object dxVertContainerDockSite2: TdxVertContainerDockSite
      Left = 0
      Top = 0
      Width = 185
      Height = 614
      ActiveChildIndex = -1
      AllowFloating = True
      AutoHide = False
      CustomCaptionButtons.Buttons = <>
      ExplicitHeight = 592
      DockingType = 1
      OriginalWidth = 185
      OriginalHeight = 140
      object dxDockPanel1: TdxDockPanel
        Left = 0
        Top = 0
        Width = 185
        Height = 382
        AllowFloating = True
        AutoHide = False
        Caption = #1043#1088#1091#1087#1087#1099' '#1084#1077#1090#1086#1076#1080#1082
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        DockingType = 2
        OriginalWidth = 185
        OriginalHeight = 368
        object cxDBTreeList1: TcxDBTreeList
          Left = 0
          Top = 3
          Width = 181
          Height = 355
          Align = alClient
          Bands = <
            item
            end>
          DataController.DataSource = TreeDatasource
          DataController.ParentField = 'ParentMethologyName'
          DataController.KeyField = 'MethologyName'
          LookAndFeel.Kind = lfFlat
          LookAndFeel.NativeStyle = False
          Navigator.Buttons.CustomButtons = <>
          OptionsData.Editing = False
          OptionsSelection.InvertSelect = False
          OptionsView.Headers = False
          RootValue = -1
          TabOrder = 0
          OnEnter = cxDBTreeList1Enter
          OnResize = cxDBTreeList1Resize
          ExplicitHeight = 565
          object cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn
            DataBinding.FieldName = 'CurrentMethologyName'
            Width = 240
            Position.ColIndex = 0
            Position.RowIndex = 0
            Position.BandIndex = 0
            Summary.FooterSummaryItems = <>
            Summary.GroupFooterSummaryItems = <>
          end
        end
        object dxBarDockControl3: TdxBarDockControl
          Left = 0
          Top = 0
          Width = 181
          Align = dalTop
          BarManager = dxBarManager1
        end
      end
      object dxDockPanel4: TdxDockPanel
        Left = 0
        Top = 382
        Width = 185
        Height = 232
        AllowFloating = True
        AutoHide = False
        Caption = #1057#1090#1072#1090#1100#1080' '#1079#1072#1090#1088#1072#1090
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        DockingType = 2
        OriginalWidth = 185
        OriginalHeight = 224
        object dxBarDockControl4: TdxBarDockControl
          Left = 0
          Top = 0
          Width = 181
          Align = dalTop
          BarManager = dxBarManager1
        end
        object cxGrid3: TcxGrid
          Left = 0
          Top = 3
          Width = 181
          Height = 205
          Align = alClient
          TabOrder = 1
          OnEnter = cxGrid3Enter
          OnResize = cxGrid3Resize
          LookAndFeel.Kind = lfFlat
          LookAndFeel.NativeStyle = False
          ExplicitHeight = 565
          object cxGrid3DBTableView1: TcxGridDBTableView
            Navigator.Buttons.CustomButtons = <>
            DataController.DataSource = PriceItemDatasource
            DataController.Summary.DefaultGroupSummaryItems = <>
            DataController.Summary.FooterSummaryItems = <>
            DataController.Summary.SummaryGroups = <>
            OptionsSelection.InvertSelect = False
            OptionsView.GroupByBox = False
            OnColumnSizeChanged = cxGrid3DBTableView1ColumnSizeChanged
            object cxGrid3DBTableView1Column1: TcxGridDBColumn
              DataBinding.FieldName = 'PriceItemName'
              Width = 50
            end
            object cxGrid3DBTableView1Column2: TcxGridDBColumn
              DataBinding.FieldName = 'PriceItemDescription'
              Width = 121
            end
          end
          object cxGrid3Level1: TcxGridLevel
            GridView = cxGrid3DBTableView1
          end
        end
      end
    end
  end
  object dxStatusBar2: TdxStatusBar
    Left = 0
    Top = 614
    Width = 963
    Height = 20
    Panels = <>
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ExplicitLeft = 632
    ExplicitTop = 608
    ExplicitWidth = 0
  end
  object TreeDatasource: TDataSource
    DataSet = TreeDataset
    OnDataChange = TreeDatasourceDataChange
    Left = 120
    Top = 256
  end
  object MethologyDatasource: TDataSource
    DataSet = MethologyDataset
    Left = 232
    Top = 104
  end
  object PriceItemDataset: TClientDataSet
    PersistDataPacket.Data = {
      850000009619E0BD01000000180000000300000000000300000085000B507269
      63654974656D4944020049000000010005574944544802000200FF000D507269
      63654974656D4E616D65020049000000010005574944544802000200FF001450
      726963654974656D4465736372697074696F6E02004900000001000557494454
      4802000200FF000000}
    Active = True
    Aggregates = <>
    Params = <>
    AfterInsert = PriceItemDatasetAfterInsert
    AfterPost = PriceItemDatasetAfterPost
    Left = 72
    Top = 424
    object PriceItemDatasetPriceItemID: TStringField
      FieldName = 'PriceItemID'
      Required = True
      Size = 255
    end
    object PriceItemDatasetPriceItemName: TStringField
      FieldName = 'PriceItemName'
      Size = 255
    end
    object PriceItemDatasetPriceItemDescription: TStringField
      FieldName = 'PriceItemDescription'
      Size = 255
    end
  end
  object TreeDataset: TClientDataSet
    PersistDataPacket.Data = {
      5E0000009619E0BD0100000018000000020000000000030000005E000D4D6574
      686F6C6F67794E616D6502004900000001000557494454480200020000040D49
      64656E746966696361746F72020049000000010005574944544802000200FF00
      0000}
    Active = True
    Aggregates = <>
    Params = <>
    AfterPost = TreeDatasetAfterPost
    OnCalcFields = ClientDataSet1CalcFields
    Left = 120
    Top = 208
    object TreeDatasetMethologyName: TStringField
      FieldName = 'MethologyName'
      Size = 1024
    end
    object TreeDatasetParentMethologyName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'ParentMethologyName'
      Size = 1024
    end
    object TreeDatasetCurrentMethologyName: TStringField
      FieldKind = fkInternalCalc
      FieldName = 'CurrentMethologyName'
      Size = 1024
    end
    object TreeDatasetIdentificator: TStringField
      FieldName = 'Identificator'
      Size = 255
    end
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
    Font.Name = 'Tahoma'
    Font.Style = []
    LookAndFeel.Kind = lfFlat
    LookAndFeel.NativeStyle = False
    Left = 24
    Top = 200
    PixelsPerInch = 96
  end
  object OrderDatasource: TDataSource
    DataSet = OrderDataset
    Left = 241
    Top = 448
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
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 25
    Top = 144
    PixelsPerInch = 96
    object dxBarButton1: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
      OnClick = dxBarButton1Click
    end
    object dxBarButton2: TdxBarButton
      Action = actionAddMethology
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000023744558745469746C65004164643B46696C653B41646446696C653B
        426172733B526962626F6E3B15DCAA0A000002E049444154785E65927F689555
        18C73FE7BDEFBDCDFD52BBCAC6AC3FC67E68615830DB168DB491DBAC086566B8
        612B35D655D141121B42E48FFE501B28669BD860FB4316CE864284DA2854C28A
        A9685413993437595DC7D6DDDAEEDEF73DE7E9EECD3B821D78CEF71C9E733E7C
        9F874701EAD0A99F7A14AC061004115080117F4766540C82C218F7FBBD91B272
        11310036A0C4C8EA0FB63D0F0002B387B9570E9EF86115A00066019E18109876
        34028800BE26210A0152420AED19E600B4A301D02290FC0C042C0502CA02E3E7
        66201680DADF550180E5033C0D08227E801226E27F71E9D6715A7BDEE6F0F9B5
        B47E5BC737D78FF1F4B22040A0697D37C6083EC0D506010C0AA5147F446FD0F6
        5D84901DA0BAE4431AD79D65434283B6CDCDE801229F9656D98134CB73CDFF1D
        8018884DFE49F7B583BCF94223E52B6A387FB593C6935B3877A583179757B0B1
        AC81F9E194F6773E2ECA3B50DBA32CC0725C8D08584AB8FA6B27CFE5AE21279C
        C368FC37EEDEEF636BE58984DE233A758B507A8CA2C2F28CB405A13D40C0F64B
        987560B8DD7F859AAA77F9BCFB10030F1E20C092AC4C3C2D34B79D263B2B8392
        E26C8C31AF033BFF2B41FB0E40C1586C84096ED27F7F906DAFB5B06FEB693C23
        EC4FE8F186AF181A1C6378FA471C47870165270188F85DB55506A3B128E90B27
        3976760B087C12F992A6CF3622C0FCC57146C727884FE991E41C589E0101DF66
        414E31BFDFF985C295634C7B717ABF7E824599F3F05CCDCA37061081A1BE2013
        31E722602C00ED6904782C6853595C4B6FEF20EEDFA96871487B7C92CD1F5591
        16FE07573B8C465DFAAE3B93C3F7C69B010D90FAD6FB1D976B779F914DBBCEC8
        919397E5687B8754EF79561ABE7846F65DCC97E69FF3129A27DB5B0A645DC372
        5DB2FEC95A20F4EA8EC2D9694C0516000B813090BDB464D14B2FD7E57655D6E7
        3FAC8C14C89AFAFC91559B73BB9E2A5B5C0ACC0354457D3E4A4448AEC44310B8
        D072573D0207937D0204701F8579E5BD3C01F817E1E75F4F0B44B2A300000000
        49454E44AE426082}
    end
    object dxBarButton3: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarButton4: TdxBarButton
      Action = actionRemoveMethology
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000005A744558745469746C650044656C6574653B44656C6574654974656D
        3B52656D6F76653B52656D6F76654974656D3B44656C6574654C6973743B4C69
        73743B52656D6F76654C6973743B4974656D3B4C6973743B436C6561723B4572
        61736558E2170B0000022349444154785E75924D6813411886DFD95DB349A458
        31C86A548AA2448F815E146AAA823FF877100F5215AF5A0D5AEDC18B97569016
        11ECA128BD28281E4568C552C1A306AA050F82A0509A06D69C0C4A92DD9971BE
        D91F12ECBEF0ED97ECF7CEBBB3CF2C93528231663C7DF5798119AC049204D47D
        6A54F14539213CFFC3D58BFD87D59CD3C84220F6FB8F57BA75A51F1DFE353539
        F3E920F941EA0800E7125202CD96AFE7324E61A081542D9BB6944F60AD00C639
        47AC6071D8A8CBF8B5B84808687B5C1B04ADA2DE1D42163D6BB7FC8480B6AF0D
        9C2E248100621C22D45C827C090C44403E0808B7CDA8474CA82206F88F81A000
        2030B2F828915A67C032E97700F3DCF10226EEC2A2A307208CAE53D0DBA6AE19
        E8C5DEB7AFA80D5F43FDC934BCE65F64DFBCC097A327BECF970E1D01604401CC
        1702027A610C8E9EEC4E4DC1B97C12E9B4879FC3D791DB04ECBA39E4647CCC50
        40CC40BF42404DC322F95CC2DFB603F5F977C8150BE83B3F08A3E1A2363B87AA
        FB6BA10BA2D0DB96B06D13A4E843CA97CB58BC7009195BCD3664D1A8D651995D
        AC3E5C5D1EE96420BC56F3FDE8D85BDC096B746C0E95A5652CDDBB8FBEFDFB60
        A54C347EAC209536513C3D90BF91DB3E09C024D2D1B9A6006401ACEFA88DAFF7
        145D77BC2C2B6707E5B4B373F5E3B10372E5F6907CD49BAF01B02920B194CCC7
        5B769F7A99DFEB4EF46E7D56B0ECCDE33DCEF3073D8E3B92C99D21048C8C4962
        4A443A6445464EA1E13D9FFEFF03AFC13AA078710D3D0000000049454E44AE42
        6082}
    end
    object dxBarButton5: TdxBarButton
      Action = actionSaveMethologyCollection
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
    object dxBarButton6: TdxBarButton
      Action = actionLoadMethologyCollection
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001974455874536F6674776172650041646F626520496D616765526561
        647971C9653C00000013744558745469746C65004C6F61644D61703B4C6F6164
        3B3C1583BE000002AB49444154785EA5934F685C5514C67F6FF226A4D4A6D408
        420DD56ED2560B0A9A164349ED42DC48692B2EAA74E94690521AA4442A4141DD
        0522B4B6A894104D2BA6ED4263379A48FE2C12D48CD326686662864E32D34966
        924966E6FDBDF7F4F166120864E7818FC3399CEFC7C7856B8808DB956118DBAE
        6B7D8B2902D4D564D67A04A0ABFFCFD0549BA39DBD13F2E137A33F9EFBE8EB67
        6A3B885D6B1F8A5F6F97F8B5AAFEBEDA2EE3DD6D6737C0EF770F9E3C7F6578E2
        FCD511F7832B23F2533C271DD7470BE7BA7E7817A833D1F2DA0BEF7DB799CCB7
        D698E9EFF8F2ABCE53134372A6B57E4763FF89B6169EDDF724759108590B4EBF
        B96BCFC79FFDD7070C98CAD3A08AE03E44B4C24471E8AD0B4DBBA7D389FB633B
        D9FFDCD3F40E8C9359580A0026274E1DE3DEC0AFAB85C5D425404794AF411428
        1B4359E057A8D3799A9E3FCAFC32DCBD3BC43BAF3672EBF211CA96CDECBD3B24
        466E1E4FFCF2F90DC03395ABAA00DF4602B3F62D501E44B31CDEEBF0FAE12738
        72A08419F5F8EDD366527FCCD2DF339C034C0063F4935669BBD8835E9D62697A
        9295856CC86B6C7903C7F1318D0895C5299C621AD1C2726A091102095AC992E9
        B90AAB90C55E986335BBCE81B7BFC0084C688D8807DA43D4CB815CC47702D9A1
        0AA924E3DFF7A54CE5288AE97956FEF9975DCD2F214E9172E26744FB88AABE8F
        681DCE288568850E948E059EA23F1826589E4F52CAE4D9FBCA7EEC4C0C7FBDB0
        79189A4483F243900E419A5CF211F174E9B6E93A3EF96482866803F5A64F25F5
        17CA5A03AD36534835456DA728ADD8140AD674CF586ECEB4CA2E92C9F0D48B07
        B1161FE0E41F82D2E1A1C886494208A243502E5D225FF20601CF2C97BD49FCB5
        D699DF279919061101A97601D0008208208216707D49C7162BDF02AE01EC00A2
        4064DBDFB7B504F000BBF764B30208BFF3FFA9C7E0B9B422AAA90CAE00000000
        49454E44AE426082}
    end
    object dxBarSubItem1: TdxBarSubItem
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton7: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarButton8: TdxBarButton
      Action = actionClose
      Category = 0
    end
    object dxBarSubItem2: TdxBarSubItem
      Caption = #1043#1088#1091#1087#1087#1099' '#1084#1077#1090#1086#1076#1080#1082
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton9: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarSubItem3: TdxBarSubItem
      Caption = #1052#1077#1090#1086#1076#1080#1082#1072
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton10: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarSubItem4: TdxBarSubItem
      Caption = #1057#1090#1072#1090#1100#1080' '#1079#1072#1090#1088#1072#1090
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton11: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
    object dxBarSubItem5: TdxBarSubItem
      Caption = #1047#1072#1082#1072#1079
      Category = 0
      Visible = ivAlways
      ItemLinks = <>
    end
    object dxBarButton12: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = 'New Button'
      Visible = ivAlways
    end
  end
  object ActionList1: TActionList
    Left = 25
    Top = 40
    object actionAddMethology: TAction
      Caption = 'actionAddMethology'
      OnExecute = actionAddMethologyExecute
    end
    object actionRemoveMethology: TAction
      Caption = 'actionRemoveMethology'
      OnExecute = actionRemoveMethologyExecute
    end
    object actionSaveMethologyCollection: TAction
      Caption = 'actionSaveMehologyCollection'
      OnExecute = actionSaveMethologyCollectionExecute
    end
    object actionLoadMethologyCollection: TAction
      Caption = 'load'
      OnExecute = actionLoadMethologyCollectionExecute
    end
    object actionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = actionCloseExecute
    end
    object actionRestartApp: TAction
      Caption = 'actionRestartApp'
      OnExecute = actionRestartAppExecute
    end
    object actionAddMethologyGroup: TAction
      Caption = 'actionAddMethologyGroup'
      OnExecute = actionAddMethologyGroupExecute
    end
  end
  object MethologyDataset: TClientDataSet
    PersistDataPacket.Data = {
      AC0000009619E0BD010000001800000005000000000003000000AC0007466F72
      6D756C61020049000000010005574944544802000200FF00054B6F6566500200
      49000000010005574944544802000200FF00054B6F6566520200490000000100
      05574944544802000200FF000B50726963654974656D49440200490000000100
      05574944544802000200FF000D4964656E746966696361746F72010049000000
      01000557494454480200020032000000}
    Active = True
    Aggregates = <>
    Params = <>
    AfterInsert = MethologyDatasetAfterInsert
    AfterPost = MethologyDatasetAfterPost
    BeforeDelete = MethologyDatasetBeforeDelete
    Left = 233
    Top = 56
    object MethologyDatasetFormula: TStringField
      FieldName = 'Formula'
      Size = 255
    end
    object MethologyDatasetKoefP: TStringField
      FieldName = 'KoefP'
      Size = 255
    end
    object MethologyDatasetKoefR: TStringField
      FieldName = 'KoefR'
      Size = 255
    end
    object MethologyDatasetPriceItemID: TStringField
      FieldName = 'PriceItemID'
      Size = 255
    end
    object MethologyDatasetPriceItemName: TStringField
      FieldKind = fkLookup
      FieldName = 'PriceItemName'
      LookupDataSet = PriceItemDataset
      LookupKeyFields = 'PriceItemID'
      LookupResultField = 'PriceItemName'
      KeyFields = 'PriceItemID'
      Size = 255
      Lookup = True
    end
    object MethologyDatasetPriceItemDescription: TStringField
      FieldKind = fkLookup
      FieldName = 'PriceItemDescription'
      LookupDataSet = PriceItemDataset
      LookupKeyFields = 'PriceItemID'
      LookupResultField = 'PriceItemDescription'
      KeyFields = 'PriceItemID'
      Size = 255
      Lookup = True
    end
    object MethologyDatasetIdentificator: TStringField
      FieldName = 'Identificator'
      Size = 50
    end
  end
  object FileSaveDialog1: TFileSaveDialog
    DefaultExtension = '*.opp.price'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'opp price project (*.opp.price)'
        FileMask = '*.opp.price'
      end>
    Options = []
    Left = 121
    Top = 88
  end
  object FileOpenDialog1: TFileOpenDialog
    DefaultExtension = '*.opp.price'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'opp price project (*.opp.price)'
        FileMask = '*.opp.price'
      end>
    Options = []
    Left = 121
    Top = 40
  end
  object PriceItemDatasource: TDataSource
    DataSet = PriceItemDataset
    Left = 74
    Top = 480
  end
  object MainMenu1: TMainMenu
    Left = 25
    Top = 88
    object N1: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088
      object About1: TMenuItem
        Caption = 'About'
      end
      object N12: TMenuItem
        Caption = '-'
      end
      object load2: TMenuItem
        Action = actionLoadMethologyCollection
        Caption = #1054#1090#1082#1088#1099#1090#1100' '#1087#1088#1086#1077#1082#1090
      end
      object N13: TMenuItem
        Action = actionSaveMethologyCollection
        Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100' '#1087#1088#1086#1077#1082#1090' '#1082#1072#1082
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object N2: TMenuItem
        Action = actionClose
      end
    end
    object N4: TMenuItem
      Caption = #1043#1088#1091#1087#1087#1072' '#1084#1077#1090#1086#1076#1080#1082
      object N14: TMenuItem
        Action = actionAddMethologyGroup
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      end
      object N15: TMenuItem
        Caption = #1059#1076#1072#1083#1080#1090#1100
      end
    end
    object N5: TMenuItem
      Caption = #1052#1077#1090#1086#1076#1080#1082#1072
      object actionAddMethology1: TMenuItem
        Action = actionAddMethology
        Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      end
      object actionRemoveMethology1: TMenuItem
        Action = actionRemoveMethology
        Caption = #1059#1076#1072#1083#1080#1090#1100
      end
    end
    object N6: TMenuItem
      Caption = #1057#1090#1072#1090#1100#1080' '#1079#1072#1090#1088#1072#1090
    end
    object N7: TMenuItem
      Caption = #1047#1072#1082#1072#1079
    end
    object N8: TMenuItem
      Caption = #1042#1080#1076
      object N9: TMenuItem
        Caption = #1055#1072#1085#1077#1083#1080
        object N10: TMenuItem
          Action = actionRestartApp
          Caption = #1057#1073#1088#1086#1089
        end
        object N11: TMenuItem
          Caption = '-'
        end
      end
    end
  end
  object PopupMenu1: TPopupMenu
    Left = 121
    Top = 152
    object actionAddMethology2: TMenuItem
      Action = actionAddMethology
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
    end
    object actionRemoveMethology2: TMenuItem
      Action = actionRemoveMethology
      Caption = #1059#1076#1072#1083#1080#1090#1100
    end
  end
  object OrderDataset: TClientDataSet
    PersistDataPacket.Data = {
      700000009619E0BD010000001800000003000000000003000000700009497465
      6D50726963650A000400000000000D4964656E746966696361746F7201004900
      000001000557494454480200020032000D50726963654974656D4E616D650200
      4900000001000557494454480200020000040000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 241
    Top = 392
    object OrderDatasetItemPrice: TExtendedField
      FieldName = 'ItemPrice'
      Precision = 19
    end
    object OrderDatasetPredictedPrice: TExtendedField
      FieldKind = fkCalculated
      FieldName = 'PredictedPrice'
      Precision = 19
      Calculated = True
    end
    object OrderDatasetIdentificator: TStringField
      FieldName = 'Identificator'
      Size = 50
    end
    object OrderDatasetPriceItemName: TStringField
      FieldName = 'PriceItemName'
      Size = 1024
    end
  end
  object PropertiesDatasource: TDataSource
    DataSet = TreeDataset
    Left = 842
    Top = 144
  end
end
