object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 599
  ClientWidth = 974
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object dxDockSite1: TdxDockSite
    Left = 0
    Top = 31
    Width = 974
    Height = 568
    Align = alClient
    DockingType = 5
    OriginalWidth = 974
    OriginalHeight = 568
    object dxLayoutDockSite3: TdxLayoutDockSite
      Left = 321
      Top = 0
      Width = 653
      Height = 568
      DockingType = 0
      OriginalWidth = 300
      OriginalHeight = 200
      object dxLayoutDockSite1: TdxLayoutDockSite
        Left = 0
        Top = 0
        Width = 332
        Height = 568
        DockingType = 0
        OriginalWidth = 300
        OriginalHeight = 200
      end
      object dxDockPanel2: TdxDockPanel
        Left = 332
        Top = 0
        Width = 321
        Height = 568
        AllowFloating = True
        AutoHide = False
        Caption = 'dxDockPanel2'
        CustomCaptionButtons.Buttons = <>
        TabsProperties.CustomButtons.Buttons = <>
        TabsProperties.Style = 9
        DockingType = 3
        OriginalWidth = 321
        OriginalHeight = 140
        object cxDBVerticalGrid1: TcxDBVerticalGrid
          Left = 0
          Top = 0
          Width = 317
          Height = 540
          BorderStyle = cxcbsNone
          Align = alClient
          Navigator.Buttons.CustomButtons = <>
          TabOrder = 0
          DataController.DataSource = DataSource1
          Version = 1
          object cxDBVerticalGrid1DBEditorRow1: TcxDBEditorRow
            Properties.DataBinding.FieldName = 'Caption'
            ID = 0
            ParentID = -1
            Index = 0
            Version = 1
          end
          object cxDBVerticalGrid1DBEditorRow2: TcxDBEditorRow
            Properties.DataBinding.FieldName = 'ActionText'
            ID = 1
            ParentID = -1
            Index = 1
            Version = 1
          end
          object cxDBVerticalGrid1DBEditorRow3: TcxDBEditorRow
            Properties.DataBinding.FieldName = 'ReactionText'
            ID = 2
            ParentID = -1
            Index = 2
            Version = 1
          end
          object cxDBVerticalGrid1DBEditorRow4: TcxDBEditorRow
            Properties.DataBinding.FieldName = 'ActualResultText'
            ID = 3
            ParentID = -1
            Index = 3
            Version = 1
          end
          object cxDBVerticalGrid1DBEditorRow5: TcxDBEditorRow
            Properties.DataBinding.FieldName = 'Order'
            ID = 4
            ParentID = -1
            Index = 4
            Version = 1
          end
        end
      end
    end
    object dxDockPanel1: TdxDockPanel
      Left = 0
      Top = 0
      Width = 321
      Height = 568
      Color = clBtnFace
      ManagerColor = False
      AllowFloating = True
      AutoHide = False
      Caption = 'dxDockPanel1'
      CustomCaptionButtons.Buttons = <>
      TabsProperties.CustomButtons.Buttons = <>
      TabsProperties.Style = 9
      DockingType = 1
      OriginalWidth = 321
      OriginalHeight = 140
      object cxDBTreeList1: TcxDBTreeList
        Left = 0
        Top = 0
        Width = 317
        Height = 540
        BorderStyle = cxcbsNone
        Align = alClient
        Bands = <
          item
          end>
        DataController.DataSource = DataSource1
        DataController.ImageIndexField = 'NodeType'
        DataController.ParentField = 'PIdentifier'
        DataController.KeyField = 'Identifier'
        DragMode = dmAutomatic
        Images = ImageList1
        Navigator.Buttons.CustomButtons = <>
        OptionsBehavior.ImmediateEditor = False
        OptionsBehavior.DragDropText = True
        OptionsSelection.InvertSelect = False
        OptionsView.Indicator = True
        RootValue = -1
        TabOrder = 0
        OnDragDrop = cxDBTreeList1DragDrop
        OnDragOver = cxDBTreeList1DragOver
        OnEndDrag = cxDBTreeList1EndDrag
        OnInitInsertingRecord = cxDBTreeList1InitInsertingRecord
        object cxDBTreeList1cxDBTreeListColumn1: TcxDBTreeListColumn
          DataBinding.FieldName = 'Caption'
          Options.Sorting = False
          Width = 295
          Position.ColIndex = 0
          Position.RowIndex = 0
          Position.BandIndex = 0
          Summary.FooterSummaryItems = <>
          Summary.GroupFooterSummaryItems = <>
        end
        object cxDBTreeList1cxDBTreeListColumn2: TcxDBTreeListColumn
          Visible = False
          DataBinding.FieldName = 'Order'
          Width = 100
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
  end
  object dxBarDockControl1: TdxBarDockControl
    Left = 0
    Top = 0
    Width = 974
    Align = dalTop
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
    Left = 440
    Top = 112
    PixelsPerInch = 96
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    OnDataChange = DataSource1DataChange
    Left = 397
    Top = 240
  end
  object ClientDataSet1: TClientDataSet
    PersistDataPacket.Data = {
      F90000009619E0BD010000001800000008000000000003000000F90007436170
      74696F6E020049000000010005574944544802000200FF000A416374696F6E54
      657874020049000000010005574944544802000200FF00084E6F646554797065
      04000100000000000C5265616374696F6E546578740200490000000100055749
      44544802000200FF001041637475616C526573756C7454657874010049000000
      01000557494454480200020014000A4964656E74696669657201004900000001
      000557494454480200020022000B504964656E74696669657201004900000001
      00055749445448020002002200054F7264657204000100000000000000}
    Active = True
    Aggregates = <>
    Params = <>
    Left = 480
    Top = 304
    object ClientDataSet1Caption: TStringField
      FieldName = 'Caption'
      Size = 255
    end
    object ClientDataSet1ActionText: TStringField
      FieldName = 'ActionText'
      Size = 255
    end
    object ClientDataSet1NodeType: TIntegerField
      FieldName = 'NodeType'
    end
    object ClientDataSet1ReactionText: TStringField
      FieldName = 'ReactionText'
      Size = 255
    end
    object ClientDataSet1ActualResultText: TStringField
      FieldName = 'ActualResultText'
    end
    object ClientDataSet1Identifier: TStringField
      DisplayWidth = 40
      FieldName = 'Identifier'
      Size = 34
    end
    object ClientDataSet1PIdentifier: TStringField
      DisplayWidth = 40
      FieldName = 'PIdentifier'
      Size = 34
    end
    object ClientDataSet1Order: TIntegerField
      FieldName = 'Order'
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
    PopupMenuLinks = <>
    UseSystemFont = True
    Left = 536
    Top = 200
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      28
      0)
    object dxBarManager1Bar1: TdxBar
      Caption = 'default'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1008
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
          ItemName = 'dxBarButton2'
        end
        item
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end>
      OneOnRow = True
      Row = 0
      UseOwnFont = False
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
  end
  object ActionList1: TActionList
    Left = 440
    Top = 216
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
  end
  object SaveDialog1: TSaveDialog
    Left = 432
    Top = 496
  end
  object ImageList1: TImageList
    Left = 360
    Top = 88
    Bitmap = {
      494C010102000C004C0010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCFCFD0DFBFCFD0DFBFCFD0DFBFC
      FD0DFBFCFD0DFBFCFD0DFBFCFD0DFBFCFD0DFBFCFD0DFBFCFD0DFBFCFD0DFBFC
      FD0DFBFCFD0DFBFCFD0DFDFEFE04000000000000000000000000000000000000
      0000F8FAF8070000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000030BDF1FE2EBEF7FF2EBEF7FF2EBE
      F7FF2EBEF7FF2EBEF7FF2EBEF7FF2EBEF7FF2EBEF7FF2EBEF7FF2EBEF7FF2EBE
      F7FF2EBEF7FF2EBEF7FEF0F3F43F000000000000000000000000000000000000
      000019961BFF168C11F5FEFEFE01000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003BC2F2FF12BBFCFF12BBFCFF12BB
      FCFF12BBFCFF12BBFCFF12BBFCFF12BBFCFF12BBFCFF12BBFCFF12BBFCFF12BB
      FCFF12BBFCFF12BBFCFFB5E7F966000000000000000000000000000000000000
      000010900BFF00A000FF189515FFE6EFE6190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003CC4F5FF1DC3FCFF1DC3FCFF1DC3
      FCFF1DC3FCFF1DC3FCFF1DC3FCFF1DC3FCFF1DC3FCFF1DC3FCFF1DC3FCFF1DC3
      FCFF1DC3FCFF1DC3FCFF78D5F5CB000000000000000000000000000000000000
      00000D8F09FF00BA00FF00AA00FF189B16FF9EC59E6100000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000003EC9FAFF4AD3FDFF4AD3FDFF4DD4
      FDFF52D5FDFF56D6FDFF5AD7FDFF54D6FDFF46D2FDFF30CDFDFF2ACCFDFF2ACC
      FDFF2ACCFDFF2ACCFDFF41CBF7FD000000000000000000000000000000000000
      00000B8D07FF00CD00FF00B500FF009E00FF0A8D0AFF429340BF000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000043CEFBFF68DDFDFF61DCFEFF66DD
      FEFF6BDFFEFF71E0FEFF78E1FEFF7EE3FEFF85E4FEFF8CE6FEFF93E7FEFF99E9
      FEFF64DDFEFF37D4FEFF4CD7FDFF000000000000000000000000000000000000
      0000098906FF00C300FF00B200FF009D00FF008700FF017201FF10790DF4FEFE
      FE01000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000089E1FCFF5ED5F7FF7BE5FEFF81E6
      FEFF87E8FEFF8DE9FEFF94EAFEFF9AEBFEFFA1EDFEFFA8EEFEFFAEEFFEFFB4F0
      FFFFBAF1FFFFC0F2FFFF97EAFEFFFBFDFE080000000000000000000000000000
      0000068404FF00B000FF00A500FF009500FF008100FF006D00FF005700FF0A69
      08FFEAF1EA1C0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000097E6FCFF5ACFF4FF95ECFEFF9BED
      FEFFA1EEFEFFA7EFFEFFADF1FEFFB3F2FEFFB9F3FFFFBFF4FFFFC4F4FFFFC9F5
      FFFFCEF6FFFFD2F7FFFFD5F7FFFFCAEFFA610000000000000000000000000000
      0000057D03FF2AAF2AFF2CAB2CFF29A029FF299429FF2C892CFF317E31FF3574
      35FF5A955ADA0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A2EAFCFF7FDCF8FFA6F0FDFFAAF0
      FDFFAEF1FDFFB2F2FEFFB6F2FEFFB9F3FEFFBDF3FEFFC0F3FEFFC3F4FEFFC5F4
      FEFFC7F5FEFFC8F5FEFFC8F5FEFF8EDDF6B50000000000000000000000000000
      0000027401FF31A731FF36A536FF3AA13AFF3F9B3FFF439443FF3C853BFF6E9E
      6E91000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000AAECFCFFC2F4FFFFBCF3FFFFB6F1
      FFFFB0F0FEFFA9EFFEFFA2EEFEFF9BEDFEFF95EBFEFF8EEAFEFF87E9FEFF81E8
      FEFF81E8FEFF0000000000000000000000000000000000000000000000000000
      0000006900FF4AAA4AFF4EAA4EFF53A953FF58A658FF2A782AFFC9D9C9360000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000ACEDFCFFCEF6FFFFC9F6FFFFC4F5
      FFFFBFF4FFFFB9F3FEFF73D9F6F775D8F5EC74D8F5EC73D8F5EC72D8F5EC71D7
      F5EC70D5F5E90000000000000000000000000000000000000000000000000000
      0000005B00FF63B263FF67B467FF6CB56CFF0F5A0FFDF8FAF807000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A3EDFCFFD7F9FFFFD4F8FFFFD0F7
      FFFFCBF7FFFFC6F6FFFFBDEBF96C000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000004E00FF7CBF7CFF82C282FF206220DF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000073D5F4DF7DDAF6EC7DDAF6EC7DDA
      F6EC7CDAF6EC7BDAF6ECE3F6FC36000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000004C00FF81BB81FF6E996E91000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00006B986BD5C9D9C93600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFF000000000001F7FF00000000
      0001F1FF000000000001F0FF000000000001F07F000000000001F03F00000000
      0001F00F000000000000F007000000000000F007000000000000F00F00000000
      0007F01F000000000007F03F0000000001FFF0FF0000000001FFF1FF00000000
      FFFFF3FF00000000FFFFFFFF0000000000000000000000000000000000000000
      000000000000}
  end
  object cxStyleRepository1: TcxStyleRepository
    PixelsPerInch = 96
    object cxStyle1: TcxStyle
      AssignedValues = [svTextColor]
      TextColor = clMenuHighlight
    end
  end
  object FileOpenDialog1: TFileOpenDialog
    FavoriteLinks = <>
    FileTypes = <>
    Options = []
    Left = 349
    Top = 503
  end
end
