object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 829
  ClientWidth = 1552
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 27
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 1552
    Height = 829
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object PageControl1: TPageControl
      AlignWithMargins = True
      Left = 12
      Top = 12
      Width = 1528
      Height = 805
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      ActivePage = TabSheet1
      Align = alClient
      Style = tsFlatButtons
      TabOrder = 0
      object TabSheet1: TTabSheet
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'TabSheet1'
        object GroupBox1: TGroupBox
          Left = 16
          Top = 16
          Width = 850
          Height = 226
          Hint = 'Kod_OKWED wrong hint'
          HelpType = htKeyword
          HelpKeyword = 'Kod_KWED2'
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'Help && Hints'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          object Kod_OKWED: TCheckBox
            Left = 48
            Top = 56
            Width = 194
            Height = 34
            HelpType = htKeyword
            HelpKeyword = 'Kod_OKWED'
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = '[Kod_OKWED]'
            ParentShowHint = False
            ShowHint = True
            TabOrder = 0
          end
          object Kod_MKC: TEdit
            Left = 48
            Top = 130
            Width = 242
            Height = 24
            Hint = 'cxHint test'
            HelpType = htKeyword
            HelpKeyword = '!'#1058#1077#1084#1072
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            ParentCustomHint = False
            ParentShowHint = False
            ShowHint = True
            TabOrder = 1
            Text = '[!'#1058#1077#1084#1072']'
          end
          object internalHelpViewerButton: TButton
            Left = 604
            Top = 126
            Width = 150
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Internal'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 3
            OnClick = internalHelpViewerButtonClick
          end
          object externalHelpViewerButton: TButton
            Left = 604
            Top = 29
            Width = 150
            Height = 50
            HelpType = htKeyword
            HelpKeyword = 'Kod_OKWED'
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'External [Kod_OKWED]'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = externalHelpViewerButtonClick
          end
        end
        object GroupBox2: TGroupBox
          Left = 912
          Top = 16
          Width = 626
          Height = 226
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'GroupBox2'
          TabOrder = 1
          object Label1: TLabel
            Left = 32
            Top = 48
            Width = 37
            Height = 16
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Label2'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
          end
          object CheckBox1: TCheckBox
            Left = 32
            Top = 130
            Width = 194
            Height = 34
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'CheckBox1'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 0
          end
          object Edit1: TEdit
            Left = 320
            Top = 42
            Width = 242
            Height = 24
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 1
            Text = 'Edit1'
          end
          object generateHintMappingButton: TButton
            Left = 412
            Top = 122
            Width = 150
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Generate mapping'
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -13
            Font.Name = 'Tahoma'
            Font.Style = []
            ParentFont = False
            TabOrder = 2
            OnClick = generateHintMappingButtonClick
          end
        end
      end
      object TabSheet2: TTabSheet
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'TabSheet2'
        ImageIndex = 1
        object GroupBox3: TGroupBox
          Left = 6
          Top = 6
          Width = 850
          Height = 467
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Caption = 'TOPPHelpPredicate'
          TabOrder = 0
          object Button1: TButton
            Left = 32
            Top = 48
            Width = 210
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Save to Stream'
            TabOrder = 0
            OnClick = Button1Click
          end
          object Memo1: TMemo
            Left = 304
            Top = 48
            Width = 513
            Height = 225
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Lines.Strings = (
              'Memo1')
            TabOrder = 1
          end
          object Button4: TButton
            Left = 32
            Top = 320
            Width = 210
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Copy'
            TabOrder = 2
            OnClick = Button4Click
          end
          object Button2: TButton
            Left = 32
            Top = 114
            Width = 210
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Save to file'
            TabOrder = 3
            OnClick = Button2Click
          end
          object Button5: TButton
            Left = 32
            Top = 176
            Width = 210
            Height = 50
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            Caption = 'Read from file'
            TabOrder = 4
            OnClick = Button5Click
          end
        end
      end
      object TabSheet3: TTabSheet
        Margins.Left = 6
        Margins.Top = 6
        Margins.Right = 6
        Margins.Bottom = 6
        Caption = 'TabSheet3'
        ImageIndex = 2
        object dxDockSite1: TdxDockSite
          Left = 920
          Top = 0
          Width = 600
          Height = 760
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alRight
          DockingType = 5
          OriginalWidth = 600
          OriginalHeight = 760
          object dxLayoutDockSite1: TdxLayoutDockSite
            Left = 0
            Top = 0
            Width = 600
            Height = 760
            Margins.Left = 6
            Margins.Top = 6
            Margins.Right = 6
            Margins.Bottom = 6
            ExplicitHeight = 759
            DockingType = 0
            OriginalWidth = 600
            OriginalHeight = 400
          end
          object dxDockPanel1: TdxDockPanel
            Left = 0
            Top = 0
            Width = 600
            Height = 760
            AllowClosing = False
            AllowFloating = False
            AutoHide = False
            Caption = #1059#1089#1083#1086#1074#1080#1103' '#1087#1086#1080#1089#1082#1072
            CustomCaptionButtons.Buttons = <>
            TabsProperties.CustomButtons.Buttons = <>
            DockingType = 0
            OriginalWidth = 203
            OriginalHeight = 161
            object cxGrid2: TcxGrid
              Left = 0
              Top = 28
              Width = 596
              Height = 690
              Align = alClient
              TabOrder = 0
              ExplicitHeight = 689
              object cxGrid2DBTableView1: TcxGridDBTableView
                Navigator.Buttons.CustomButtons = <>
                ScrollbarAnnotations.CustomAnnotations = <>
                DataController.DataSource = DataSource2
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
              end
              object cxGrid2Level1: TcxGridLevel
                GridView = cxGrid2DBTableView1
              end
            end
            object dxBarDockControl2: TdxBarDockControl
              Left = 0
              Top = 0
              Width = 596
              Height = 28
              Margins.Left = 6
              Margins.Top = 6
              Margins.Right = 6
              Margins.Bottom = 6
              Align = dalTop
              BarManager = dxBarManager1
            end
          end
        end
        object dxDockSite2: TdxDockSite
          Left = 0
          Top = 0
          Width = 920
          Height = 760
          Margins.Left = 6
          Margins.Top = 6
          Margins.Right = 6
          Margins.Bottom = 6
          Align = alClient
          DockingType = 5
          OriginalWidth = 920
          OriginalHeight = 760
          object dxLayoutDockSite2: TdxLayoutDockSite
            Left = 0
            Top = 0
            Width = 920
            Height = 760
            ExplicitWidth = 906
            ExplicitHeight = 759
            DockingType = 0
            OriginalWidth = 300
            OriginalHeight = 200
          end
          object dxDockPanel2: TdxDockPanel
            Left = 0
            Top = 0
            Width = 920
            Height = 760
            AllowClosing = False
            AllowFloating = False
            AutoHide = False
            Caption = #1047#1072#1087#1080#1089#1080' ('#1087#1086#1076#1089#1082#1072#1079#1082#1080')'
            CustomCaptionButtons.Buttons = <>
            TabsProperties.CustomButtons.Buttons = <>
            DockingType = 0
            OriginalWidth = 185
            OriginalHeight = 140
            object cxGrid1: TcxGrid
              Left = 0
              Top = 28
              Width = 916
              Height = 690
              Align = alClient
              TabOrder = 0
              ExplicitWidth = 902
              ExplicitHeight = 689
              object cxGrid1DBTableView1: TcxGridDBTableView
                Navigator.Buttons.CustomButtons = <>
                ScrollbarAnnotations.CustomAnnotations = <>
                OnFocusedRecordChanged = cxGrid1DBTableView1FocusedRecordChanged
                DataController.DataSource = DataSource1
                DataController.Summary.DefaultGroupSummaryItems = <>
                DataController.Summary.FooterSummaryItems = <>
                DataController.Summary.SummaryGroups = <>
              end
              object cxGrid1Level1: TcxGridLevel
                GridView = cxGrid1DBTableView1
              end
            end
            object dxBarDockControl1: TdxBarDockControl
              Left = 0
              Top = 0
              Width = 916
              Height = 28
              Margins.Left = 6
              Margins.Top = 6
              Margins.Right = 6
              Margins.Bottom = 6
              Align = dalTop
              BarManager = dxBarManager1
            end
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
    Left = 120
    Top = 592
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 128
    Top = 488
    PixelsPerInch = 192
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
    Left = 120
    Top = 696
    PixelsPerInch = 192
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Custom 1'
      CaptionButtons = <>
      DockControl = dxBarDockControl1
      DockedDockControl = dxBarDockControl1
      DockedLeft = 0
      DockedTop = 0
      FloatLeft = 1576
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -1
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton8'
        end
        item
          Visible = True
          ItemName = 'dxBarButton3'
        end
        item
          Visible = True
          ItemName = 'dxBarButton1'
        end
        item
          Visible = True
          ItemName = 'dxBarButton2'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = True
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar2: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Custom 2'
      CaptionButtons = <>
      DockControl = dxBarDockControl1
      DockedDockControl = dxBarDockControl1
      DockedLeft = 107
      DockedTop = 0
      FloatLeft = 1576
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -1
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButton4'
        end
        item
          Visible = True
          ItemName = 'dxBarButton5'
        end>
      OneOnRow = False
      Row = 0
      UseOwnFont = True
      Visible = True
      WholeRow = False
    end
    object dxBarManager1Bar3: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Custom 3'
      CaptionButtons = <>
      DockControl = dxBarDockControl2
      DockedDockControl = dxBarDockControl2
      DockedLeft = 1
      DockedTop = 0
      FloatLeft = 1576
      FloatTop = 2
      FloatClientWidth = 0
      FloatClientHeight = 0
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -1
      Font.Name = 'Segoe UI'
      Font.Style = []
      ItemLinks = <
        item
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
    object dxBarButton1: TdxBarButton
      Caption = '&Open'
      Category = 0
      Hint = 'Open'
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002B744558745469746C65004F70656E3B466F6C6465723B426172733B
        526962626F6E3B5374616E646172643B4C6F6164F1C3C4630000022249444154
        785EA593BD6B545110C57F6FF3242AA9B412144B3FB1500CA20663FC032C6C44
        B0B010041194147616366295805A88A058898D8D106C444D6C44123426120959
        36B8316FD9F8F2B15F6FDFBD7746B9EFAD82019B1CB81C6698397366E006AACA
        461000859CC959F327FC079DC1E1C4C381D785807EB21815A827E642DF8DB1E7
        E30F4E8BA2A0CA1F12A5F7FA281D8488F61FBAFC940C8A6DD5987A76F3DE9D4B
        FB468F5C7D53653D0470B94B426704DC2AA4DF517184380E9EBBB6DD99E1F2A9
        03DB4072BB0AA2608C1B1BB8F5E10C6032012BA00E5C422016D4D2250D0E5F1C
        24E8DE0104A00208AA8ED1BB57FA80AEBF02A9CB046C82DA26625BE00CD4CBA8
        CF1B54F21A1556AB75809E20080A800BDEDD3EA67D8343C8CA272AD3E3C40B15
        54404510C92EA7A28888E7A5F92AEAD38A38AD862675B4E28864A1C872D460FF
        F961BC786E1B71A85A108B3A8B8A0149894BDF78FB6868DEAFB05A2E11CFCCD2
        B3AB174D221AB32F51DF90DD47457C8C737E1D114769B2CCCA9A19094DDBB154
        9A632D5A66E7F1BDB42BD3D85ADC29CC9A54C04F17C40B0951B1C29772FD4598
        B62D4B73737477F7B0796B487D6602DB58F6D63B2E347391E71CB59536F1CFC6
        D7FBEFA362D86AA4B018B1E7C44992C529926AD14F50BF7BA749BD082A5EA8BA
        9010D7CD0860C24633FD88AB1D9D1C79C5647E5D72560001505401554421B55A
        FEFCA3F9184803600BB00928B01EC13FB10206489E9CDDED0036FC9D7F01FAB6
        A14B22EE620A0000000049454E44AE426082}
    end
    object dxBarButton2: TdxBarButton
      Caption = '&Save'
      Category = 0
      Hint = 'Save'
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
    object dxBarButton3: TdxBarButton
      Caption = '&New'
      Category = 0
      Hint = 'New'
      Visible = ivAlways
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
      OnClick = dxBarButton3Click
    end
    object dxBarButton4: TdxBarButton
      Caption = 'Add record'
      Category = 0
      Hint = 'Add record'
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000001B744558745469746C65004164643B506C75733B426172733B526962
        626F6E3B9506332F0000036349444154785E35927D6C535518C69F73EE6DEB64
        63A3AEFB60A3A36E33B8C56581E0D8707E21CC1A43A2A22304FE3001512A86C4
        E900132451FF503367420043B244364C483031465C248B4441C0980C45B4D065
        CDBA4ECAE82AAC5DBBDE8FF3E1BD27F1397973DE9C3CBFF7233964226FC2D543
        A53E0280443E3FD752525AB14323FA06685A3381E492F329C6ADF39954E2F8C9
        C3DBA6018858DE940A9C2C5870C1D51BB6FAF61DBB327860F81A1BFE25297FB8
        3127C7EFE4E5D5745E9EBB9991239766E481937FE4DE1818DB0DC0EB322EABBA
        B63FD5EB7D6CCBBE6F1B83FE9E67BA82E084C0E4123697CAE0D109BC94805B0C
        E7AFCC606A66EEECF75FBCBB753AFAEB2201A0BD3E7861B02914D8DBF34408A9
        AC0D2181D3672E23319D81AB950D016CEBED824E809A722FC62E4CE17A343130
        D4DF73507FB9FFAB551E9F6FCF93EB82B879BB088D52504A14FCC9CE4E95F79D
        B80CD396284A8179C7D3DD1144F29FEC5BE1D73E1BA6BEB2C09BEDCD955A7CCE
        44D1744C1687C9045C05EBFC686F0DAADCB08413D2098E89B4E1BC5779965687
        5ED585D03ACBFDA548E7197EFA711C776EDFC5FF12200A7075F4E85975D7D4FA
        F1F4A635A82C5F02A2956CD46D2EEB1D160B455BC19FEE5E0F4A885A45828071
        81137D1B61DB0C1E5D43E4C8CF5858E4D0A1810BBA5CB76DEEBDB768C1E604AE
        EA6B1F40D9121F0A265385BC0E5457530109404A8010E27805EEE60598CDA15B
        8699C8E7CD4784EEC3F2BA00767C340A4AA9327E79300CE1505BDEFF0E9AA681
        5082150DD5604CA26858282E1693D428E42F6666B3909068EF68C5E6171FC7E6
        17BA611A260C93A9029C713CF7FC3A3C1BEE404B5B2398E0989FCBA190FD774C
        CFA46243B11B4B77ADADF67BB236478E10500AA5D2121D5C48354D3A674108A1
        56114C201E4BB1D9F86FA70880FB1EDD3E34B0A229B4E7E1350FC2E22E2011BF
        16C3FCBD050557562DC3CA964608B8B4C4E49F4924A27F1F193F1DD9AF03B0FE
        1AFDE03D113EDC6431B1A96575089212B4AD6D555F581280D902398343308EC9
        EB49DC9A981A75E043000CA46D09005A49457059DB4BC78E77EDFCDAEAFDF892
        DC3B1295EF7C13977D4E444E45E52BCE5BE7AE338555E10FDF0650EE32B30E4B
        D24C0212A8F210EAAED3D01969BB3FD0BCDDE32BEB06D56AD5D09CCDDA66EE62
        EED6EF43A9AB2331008603ABCEFF019D3AAD15CCD8D2E00000000049454E44AE
        426082}
    end
    object dxBarButton5: TdxBarButton
      Caption = '&Remove'
      Category = 0
      Hint = 'Remove'
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000029744558745469746C650052656D6F76653B44656C6574653B426172
        733B526962626F6E3B5374616E646172643B635648300000026449444154785E
        A551494C5351146568194A2B583746627F458902118892AAB811369246043462
        8556D032A82D831664486D8C3FB40B8128E3C68998A289625720314469022591
        26566BC484681C22C6012B5A1556C7F73EBF507FDCF193F3DFBBE7DC7BDE7BF7
        86005811B820E80B2310F16B6880E4F7E10462AAF3F1B2014F889E961FBB307D
        AAE2EBC8FEDC137C72280FB1AB546BA0DAE4F11296C491940F36089F2CD5593F
        DFE8C682771453E71B31909D6D207C044D76166B8C339D1789E6C4A76B5D18D7
        15D9A869B04184A744E7FBED72C03FD08EF9B13BF09CADC1F50C55CDE08182DA
        B7ED562CB807E1BFDB06FF701F3C47753E52131D6C20B2EFCA343C31EAF1D3D1
        89B93E167F1EF5E37155255ED92C989F70E0C74D96E3C734F9B89498749A1E2A
        EC41644F4A6AF5B8F6107C7D2D98ED6D847FE80A7EDDBF8A6FBD4D98ED69C088
        3A0BD678A589E44A843D089844DB141B4D0FF3D5F8D251878FAC9EA08C5B87F6
        EC44B37C6D23C991F2530AF99F81E472BAAA7E42AFC307B612EFEA0E2FC1A52D
        446B5A4633C991090D968A5B93D24D4EAD06336DF5982E5363BA9C07D9BF674F
        62B4F020AC9B53976EF1CF145A36249B1EE4E5E28DA5022F8AB23045706B1383
        DB498974CFE1F599620CAB736061B63409A720ED5624CFBD341E81B760379EE5
        65A23F410183487ACE208EB1F42728E1CDCFE4F0BC641FBA9894395213176C10
        655EA3EC706CDF06778E0A76C57A5447C8E87B63298C91B166BB92817BEF0EDC
        4BDB8A0639D34DF8986083309A58278BEFB0C631BEAAC5E255940F6886A8D566
        A27DAF95ADEB22B15CD883808984208E209A8F859A9C6F6078F0145684BF98E8
        BFC080A205F60000000049454E44AE426082}
    end
    object dxBarButton6: TdxBarButton
      Caption = 'New Button'
      Category = 0
      Hint = '&New Predicate'
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        610000002C744558745469746C65004164643B4974656D3B4164644974656D3B
        426172733B526962626F6E3B4974656D3B506C75734E32EF8100000286494441
        54785E5D915D48545B14C77FFB9C199D11CDA264A2C828B2A44891B844491924
        A441915020611045742F04D14B11D1439015193E28652541057D125CEFBDF8D0
        43D017D8BDE0EDC1BC57ED8331106AC6907072C673CEDEAB98738686D9ECBDD8
        9BCDFFBF7E6B2D05D8B7FF1C7962DB768320002020E2DF252F8880E7382F0EEE
        A9691411072004584AA986BDDB5700148A0005FC34BBD537BA09B00172064A6B
        01C0714D8E01113F0882BF15C5610BCF330056BE01DA8000264F1DE803A2E01F
        411B0D4001819BAB3B10E5F74005068288CA11A8020300C1989F225028019409
        5E0631B0A8A204C06ABBF88AD9B4830528CFD501B2600013D4EE89C7B3A1043D
        8FE3B4FF1EA7AB3F4E32AD299BBF387AE7C4068504CD7082261A14C677427B2E
        97FF7ACF6832C3968D0BF9AD75395BEA63A4EC302DC7EFFFB1B8BA3EFAE84C83
        CA1218CF030163C4CF2E86FE810954B14D63C312929922B6ED7F80A722346FAD
        64CDCA8AF5EB769E3C05D85903D71F9F2F3682D6FA077A92BADA1803EF1C1229
        8D339B617246F37ADCA5B6268656E17D40289435D006117F9C205862484CCEF0
        E1AB45C7B9BB686310ED71F8C8358CD6F474FD8AEB780B013B9882017C7CB225
        0891B0E1E3C4341DED6D84141C3A7A8587D78F90CA0883EFA648CFA43FE7C628
        3982D2888D88C218F8A56A0EFF0CC50947A3D8B6C24967F8EFB3CB976F9A91D7
        1F989EFAD20778214067BEA59E9FEE7CB9591044A0BC2CCCD2452564129FF8F7
        E9104B5757D2DD7D8CBFDF24181F1E67622C3EFC71F0DE59C005B08028500ECC
        05E60567412852BEACBAE94267CDEE1B6375AD77646D4BEFDB554DE72F1595C6
        2A8050F58E5E948850B856EFBA094AF8BFEF80058481A22091011CC0AD6ABE6A
        44E03B805C64CDB4C3E1300000000049454E44AE426082}
    end
    object dxBarButton7: TdxBarButton
      Caption = '&Remove predicate'
      Category = 0
      Hint = 'Remove predicate'
      Visible = ivAlways
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
    object dxBarButton8: TdxBarButton
      Caption = 'Re&fresh'
      Category = 0
      Hint = 'Refresh'
      Visible = ivAlways
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000027744558745469746C6500526566726573683B5265706561743B4261
        72733B526962626F6E3B52656C6F6164CD4DF6E90000037749444154785E6593
        6B4C537718C69F73E885622F4069C9B02DA1ADB60C2133D471DDD451D8257C02
        2661B3719912A3092163898BDBF8B06422DB74612612CDE64CDCE2A698CC2DA2
        0C541C9918645BB701EB3414B99536E08022A597737ADE71927E58C693BCFFBC
        F9E5799E2FFFBC0C364A64FFE794988DE6CF6F55032034567433EF7596AB37E7
        A85AC0302FB22C1C448020D02F3C47BD8F3C4B9F76BC3314FCE44A05AD330844
        585F80B3BDAF00007BB2CBE9BC70C73533E0F998BCF33F5030FA9B38343EFF3D
        F58F1EA7737D0DB347CF943A456F44F06085BF87F7BF28054470A2AB72CFE5C1
        43F4C07F999EC4876989FB891E477B6921DA438BDC6D5A37D398EF22755EDD4B
        CD271DF5626689BB83B74E3920693AB643AF516BCFE7659723439B0E6F6008D7
        6F0F60E46F0F44E5DB6D28DABE1DF9D66750F8F4F3989B0F7C597DC0329026DD
        153878BC80D8CCEC4D2DD9FADC145D8606EE078338FBF5377CFFDDC10FA7C717
        AC33138FAD3F0F0D1FB976EBC798D7EF86C1900AAB292F45A393BE0D800DADC5
        20E1E3F4B22E3D13ABDC1CDC7F8D61666EAEADEFCCEC31001C44011DCA237266
        F88FB18F5277CB91AE27C438FE2500EF46C25C9CE5B9B84DA10462F12798F64D
        63C5C79D17C3CBDC5D4A7C27EBEE5BF86ABD18E3C11E90F2112251CE044071E5
        84572E0947F8C8C44A8F54A94843B24C0685542E88414E0893B3DEACCA2FD6FB
        08B4492661118AFD038A876032EA5587DB75CB828065361CE2A67D8100FC2137
        AC66234C76CD7E00529DDCC9DCBC34115E5E8C746CCBCD455DCD0B880B1C1809
        8FC6FDB5D0E9B4181B9D6D4FB239B45B55AA9462953E0C8D3C0B6BAB5462B029
        42AA34B94FFB540A3B726FFE7E7246AC9E74BF6B92A4045EE0F170E657F47C37
        35E5BEBE788029ADDE6CDE569239BA65475461B66402410BA6BC8B989CF46375
        758DF38E2E952B53E5B6AD858A0BF93BA31075B32B04CFF08ACBDD1DBC240259
        952BA7F1607B01B575DBE9DB87657463B281067CCD7471701FD535E7F603D0D7
        34D9FBDB6ED8A8F5AA899EAD4D1599120083C4935C5663A8AF6BD9E26FEAB4D0
        07D72C74EABE95CEB94BE8707B1155BC96E372546595B85AF3B8CA43199CA558
        510440E278550D54BD99932881D468571BCA6A0D9FED6C308EECDE6BA25DAF9B
        E8B93DC63F2BDFB0B4025057ED339F2EA8569E06A0103385752A301BCF1832B1
        1D4052820B006200E2FF615C82E35FC02B8FD5CBC3AEEB0000000049454E44AE
        426082}
    end
  end
  object recordsDataset: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 136
    Top = 211
  end
  object DataSource1: TDataSource
    DataSet = recordsDataset
    Left = 336
    Top = 195
  end
  object DataSource2: TDataSource
    DataSet = predicatesDataset
    Left = 1167
    Top = 308
  end
  object predicatesDataset: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 1100
    Top = 443
  end
end
