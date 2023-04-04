object SampleForm: TSampleForm
  Left = 0
  Top = 0
  BorderStyle = bsSingle
  Caption = 'SampleForm'
  ClientHeight = 510
  ClientWidth = 763
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  Visible = True
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 27
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 763
    Height = 510
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitHeight = 377
    object Splitter1: TSplitter
      Left = 265
      Top = 0
      Height = 510
      ExplicitLeft = 384
      ExplicitTop = 208
      ExplicitHeight = 100
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 265
      Height = 510
      Align = alLeft
      BevelOuter = bvNone
      Caption = 'Panel1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
      ExplicitHeight = 377
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 265
        Height = 24
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object SpeedButton1: TSpeedButton
          Left = 0
          Top = 0
          Width = 23
          Height = 24
          Align = alLeft
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000000000000000
            0000000000020000000A170D0738542D1894814626D193502AEA924F2AE87F45
            25D0522C17931209053000000009000000010000000000000000000000000000
            00030201011159311B97A96239FAC58957FFD6A36DFFDDAF75FFDDAF74FFD6A4
            6BFFC58956FFA46137F53C2112730000000F0000000300000000000000020201
            0110744226B9BC7C4DFFDDAE77FFDEB076FFE2B782FFE3BB87FFE3BC86FFE1B7
            82FFDEAF74FFDBAB72FFBD7E4EFF6F3E24B50000001000000002000000085C36
            2095BE8053FFE0B37CFFDFB076FFDEB177FFB78254FFAA7144FFAB7245FFBC88
            59FFDFB279FFDFB277FFDEB077FFC08253FF55321D920000000A190F0932B070
            47FADFB27DFFDFB27AFFE0B37BFFE0B57DFFA56B3FFFF5EFEAFFF8F3EEFFAB72
            45FFE2B67EFFE0B47CFFE0B47BFFDEB079FFB3734AFB130B072F613C2795CD9B
            6FFFE2B780FFE5BD89FFE7C291FFE8C393FFA56B3FFFF1E6DEFFF9F5F1FFAA71
            44FFE8C494FFE8C393FFE5BF8CFFE1B77FFFD09C6EFF5434218B935E3DD2DCB3
            83FFE3B781FFBA8659FFA97043FFAB7245FFAC7346FFF5EDE6FFFAF6F3FFAD75
            47FFB0784AFFB17A4BFFC29162FFE4B983FFDEB17EFF8E5B3BD0B0744CF2E3BF
            8FFFE4BB84FFA56B3FFFF3EBE6FFFAF6F3FFF6EFE8FFF7F0EAFFFBF7F5FFFAF7
            F4FFFAF7F3FFFAF6F2FFAB7245FFE5BD87FFE5BE8BFFAB714CEEAE764FECE9C9
            A0FFE5BE89FFA56B3FFFE0D2CAFFE1D3CCFFE3D5CFFFF2EAE4FFF8F3EFFFEADF
            D9FFE6DAD4FFE9DED9FFAA7144FFE7C08CFFEACA9DFFAE764FEE9A6A49D0E9CD
            ACFFEAC796FFB78456FFA56B3FFFA56B3FFFA56B3FFFF1EAE5FFFAF6F3FFA56B
            3FFFA56B3FFFA56B3FFFB78457FFEACA99FFEBD1ADFF996A49D46E4E3697DDBB
            9DFFEED3A9FFEECFA2FFEED2A5FFF0D6A9FFA56B3FFFF0EAE7FFFDFCFBFFA56B
            3FFFF1D6AAFFF0D5A8FFEED2A5FFEFD4A7FFE0C2A2FF6246318F1C140E2BC794
            6CFCF5E8CCFFEFD6ABFFF1D8AEFFF2DAB0FFA56B3FFFDECFC9FFDFD1CBFFA56B
            3FFFF3DCB2FFF1DBB0FFF1D8ADFFF7EACDFFC69470FA1A120D2E000000036F52
            3C92D7B08CFFF8EFD3FFF3E0B9FFF3DFB7FFB98A5FFFA56B3FFFA56B3FFFBA8A
            5FFFF4E1B9FFF4E2BDFFFAF1D5FFD9B390FF664B368C00000006000000010202
            0107906C4EB8D9B38FFFF7EDD3FFF8EED0FFF7EBC9FFF6E8C4FFF6E8C5FFF7EC
            CAFFF8EED0FFF4E8CDFFD7AF8BFF88664AB30202010B00000001000000000000
            00010202010770543F8FCFA078FCE2C4A2FFEBD7B8FFF4E9CDFFF4EACEFFECD8
            B9FFE3C5A3FFC59973F24C392A67000000060000000100000000000000000000
            000000000001000000022019122C6C543E89A47E5FCCC59770F1C19570EEA47E
            60CD6C543F8B16110D2200000003000000010000000000000000}
          OnClick = SpeedButton1Click
          ExplicitLeft = 24
          ExplicitHeight = 22
        end
        object SpeedButton2: TSpeedButton
          Left = 23
          Top = 0
          Width = 23
          Height = 24
          Align = alLeft
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000000000000000
            0000000000020000000C05031A46110852AB190C76E31D0E89FF1C0E89FF190C
            76E4120852AD06031B4D0000000E000000030000000000000000000000000000
            000301010519130A55A9211593FF2225AEFF2430C2FF2535CBFF2535CCFF2430
            C3FF2225AFFF211594FF140B58B20101051E0000000400000000000000020101
            03151C1270CD2522A6FF2D3DCCFF394BD3FF3445D1FF2939CDFF2839CDFF3344
            D0FF394AD4FF2D3CCDFF2523A8FF1C1270D20101051D00000003000000091912
            5BA72A27AAFF2F41D0FF3541C7FF2726ABFF3137BCFF384AD3FF384BD3FF3137
            BCFF2726ABFF3540C7FF2E40D0FF2927ACFF1A115EB10000000D08061C3D3129
            A2FD2C3CCCFF3842C6FF5F5DBDFFEDEDF8FF8B89CEFF3337B9FF3437B9FF8B89
            CEFFEDEDF8FF5F5DBDFF3741C6FF2B3ACDFF3028A4FF0907204A1E185F9F373B
            BCFF3042D0FF2621A5FFECE7ECFFF5EBE4FFF8F2EEFF9491D1FF9491D1FFF8F1
            EDFFF3E9E2FFECE6EBFF2621A5FF2E3FCFFF343ABEFF201A66B0312A92E03542
            CBFF3446D1FF2C2FB5FF8070ADFFEBDBD3FFF4EAE4FFF7F2EDFFF8F1EDFFF4E9
            E2FFEADAD1FF7F6FACFF2B2EB5FF3144D0FF3040CBFF312A95E53E37AEFA3648
            D0FF374AD3FF3A4ED5FF3234B4FF8A7FB9FFF6ECE7FFF5ECE6FFF4EBE5FFF6EB
            E5FF897DB8FF3233B4FF384BD3FF3547D2FF3446D1FF3E37AEFA453FB4FA4557
            D7FF3B50D5FF4C5FDAFF4343B7FF9189C7FFF7EFE9FFF6EEE9FFF6EFE8FFF7ED
            E8FF9087C5FF4242B7FF495DD8FF394CD4FF3F52D4FF443FB3FA403DA1DC5967
            DAFF5B6EDDFF4F4DBAFF8F89CAFFFBF6F4FFF7F1ECFFEDE1D9FFEDE0D9FFF7F0
            EAFFFAF5F2FF8F89CAFF4E4DB9FF576ADCFF5765D9FF403EA4E12E2D70987C85
            DDFF8798E8FF291D9BFFE5DADEFFF6EEEBFFEDDFDAFF816EA9FF816EA9FFEDDF
            D8FFF4ECE7FFE5D9DCFF291D9BFF8494E7FF7A81DDFF33317BAC111125356768
            D0FC9EACEDFF686FCEFF5646A1FFCCB6BCFF7A68A8FF4C4AB6FF4D4BB7FF7A68
            A8FFCBB5BCFF5646A1FF666DCCFF9BAAEEFF696CD0FD1212273F000000043B3B
            79977D84DFFFA5B6F1FF6D74D0FF2D219BFF5151B9FF8EA2ECFF8EA1ECFF5252
            BBFF2D219BFF6B72D0FFA2B3F0FF8086E0FF404183A700000008000000010303
            050C4E509DBC8087E2FFAEBDF3FFA3B6F1FF9DAFF0FF95A9EEFF95A8EEFF9BAD
            EFFFA2B3F0FFACBCF3FF838AE3FF4F52A0C10303051100000002000000000000
            000100000005323464797378D9F8929CEAFFA1AEEFFFB0BFF3FFB0BFF4FFA2AE
            EFFF939DE9FF7479DAF83234647D000000080000000200000000000000000000
            000000000000000000031213232D40437D935D61B5D07378DFFC7378DFFC5D61
            B5D040437D951212223000000004000000010000000000000000}
          OnClick = SpeedButton2Click
          ExplicitLeft = 64
          ExplicitTop = 8
          ExplicitHeight = 22
        end
        object SpeedButton3: TSpeedButton
          Left = 48
          Top = 0
          Width = 23
          Height = 22
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            00000000000000000000000000030000000B00000013000000190000001A0000
            00140000000B0000000300000000000000000000000000000000000000000000
            000000000000000000060402011C4827118B7C431ED2A65927FFA55927FF7E44
            1ED442230F7B0100000F0000000E000000070000000000000000000000000000
            000000000005120A05348A4F26DDC58A53FFDCB37CFFEFD298FFEFD198FFB676
            43FF2E1A0C62100904398F5127E10E05013A0000000600000000000000000000
            0002040201198D552BDCD1A169FFF1D6A5FFCE9E6EFFC08656FFBD8251FF613A
            1DA6000000227D4B26CBE2B97BFF5F290FCF0101001900000003000000000000
            00074C2F1B82C99765FFECD2A3FFB98154FB5238238A120C07300F0A06270201
            01194C2F1B88CE9D66FFF6DC9BFFBA8657FF3F1C0C910000000D000000000000
            000A8C5B36D0E3C598FFCB9D75FF573B258C0000000C00000003000000062014
            0C43BD875AFBF8E5BCFFF8DFA5FFF7E4BAFFA16540FC1C0E074C000000080000
            0014B37A4BFAF5E6BDFFBC8356FF0D0704300000000C00000003000000079666
            3FD5B87D4DFFBB8153FFF2D9A1FFB87D4DFFB87C4DFF9C6941DE845331D3A263
            3BFFBB8557FFF6E7BFFFBF8B5EFFA06238FF87522FDC00000006000000020000
            000B0D08042FA1653CFFF4DEAEFFB68155FA000000180000000A1F170F34C79D
            75FBFBF5DCFFFCF3CCFFFAF4DAFFB3855FFB21150C4100000004000000020000
            0009492C1886BA8B5EFFE7CEA7FF926B48CB0000000900000000000000045540
            2D77DDC1A2FFFDF7D9FFD4B598FF5037227F0202010C0D08041F110A05274B2D
            1986A1683EFAF3E4C3FFD8B692FF533F2C780000000400000000000000000000
            00058F6F50BCEFE1CDFF886343C20202010D58382091A3693CFFA66F43FFBE94
            6DFFF4E9D1FFE3CAADFFA47E5BD60504030E0000000100000000000000000000
            0001130F0B1DAB8863DA18130E242C1E1248B78B63FDF8F3E2FFF9F3E4FFEDDE
            C7FFDCC1A1FFA3815ED215110C22000000020000000000000000000000000000
            000000000001000000010101000342301E629A7B5CC2C6A078F9C6A078F9997B
            5DC3564634710504030A00000001000000000000000000000000000000000000
            0000000000000000000000000000000000010000000200000002000000020000
            0002000000010000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000000000000000
            0000000000000000000000000000000000000000000000000000}
          OnClick = SpeedButton3Click
        end
      end
      object cxListView1: TcxListView
        Left = 0
        Top = 24
        Width = 265
        Height = 486
        Align = alClient
        ColumnClick = False
        Columns = <
          item
          end>
        HideSelection = False
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 1
        ViewStyle = vsReport
        OnSelectItem = cxListView1SelectItem
        ExplicitLeft = 48
        ExplicitTop = 112
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
    object panelAddContaner: TPanel
      Left = 268
      Top = 0
      Width = 495
      Height = 510
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitHeight = 377
      object panelAddBorder: TPanel
        Left = 0
        Top = 0
        Width = 495
        Height = 510
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitHeight = 377
        object PageControl1: TPageControl
          AlignWithMargins = True
          Left = 3
          Top = 76
          Width = 489
          Height = 390
          ActivePage = TabSheet2
          Align = alClient
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          Style = tsFlatButtons
          TabOrder = 0
          ExplicitTop = 88
          ExplicitHeight = 286
          object TabSheet1: TTabSheet
            Caption = #1055#1086#1084#1086#1097#1100
            ExplicitHeight = 340
            object Panel5: TPanel
              Left = 0
              Top = 0
              Width = 481
              Height = 359
              Align = alClient
              BevelOuter = bvNone
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWindowText
              Font.Height = -11
              Font.Name = 'Tahoma'
              Font.Style = []
              ParentFont = False
              TabOrder = 0
              ExplicitLeft = -1
              ExplicitHeight = 255
              object cxLabel6: TcxLabel
                Left = 8
                Top = 9
                Caption = #1048#1089#1090#1086#1095#1085#1080#1082
              end
              object cxEditShortcutPredicateFilename: TcxButtonEdit
                Left = 112
                Top = 8
                Properties.Buttons = <
                  item
                    Default = True
                    Kind = bkEllipsis
                  end>
                Properties.ReadOnly = True
                Properties.OnButtonClick = cxEditShortcutPredicateFilenamePropertiesButtonClick
                TabOrder = 1
                Width = 337
              end
              object cxLabel7: TcxLabel
                Left = 8
                Top = 36
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
              end
              object cxComboBoxShortcutKeywordType: TcxComboBox
                Left = 112
                Top = 35
                Properties.Items.Strings = (
                  'ktBookmark'
                  'ktSearch'
                  'ktPage')
                TabOrder = 3
                Text = 'ktSearch'
                Width = 97
              end
              object cxLabel8: TcxLabel
                Left = 8
                Top = 63
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1080#1089#1082#1072
              end
              object cxTextEditShortcutPredicateValue: TcxTextEdit
                Left = 112
                Top = 62
                TabOrder = 5
                Width = 337
              end
              object cxLabel10: TcxLabel
                Left = 8
                Top = 126
                Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
              end
              object cxComboBoxShortcutDetailsKeywordType: TcxComboBox
                Left = 112
                Top = 125
                Properties.Items.Strings = (
                  'ktBookmark'
                  'ktPage'
                  'ktSearch')
                TabOrder = 7
                Text = 'ktSearch'
                Width = 97
              end
              object cxLabel9: TcxLabel
                Left = 8
                Top = 153
                Caption = #1047#1085#1072#1095#1077#1085#1080#1077
              end
              object cxTextEditShortcutDetailsPredicateValue: TcxTextEdit
                Left = 112
                Top = 152
                TabOrder = 9
                Width = 337
              end
              object cxButton4: TcxButton
                Left = 376
                Top = 374
                Width = 75
                Height = 25
                Caption = #1054#1095#1080#1089#1090#1080#1090#1100
                TabOrder = 10
              end
              object cxButton3: TcxButton
                Left = 280
                Top = 374
                Width = 75
                Height = 25
                Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
                TabOrder = 11
              end
              object cxLabel13: TcxLabel
                Left = 8
                Top = 103
                Caption = #1059#1090#1086#1095#1085#1077#1085#1080#1077
              end
            end
          end
          object TabSheet2: TTabSheet
            Caption = #1055#1086#1076#1089#1082#1072#1079#1082#1080
            ImageIndex = 1
            ExplicitHeight = 415
            object Panel4: TPanel
              Left = 0
              Top = 0
              Width = 481
              Height = 359
              Align = alClient
              BevelOuter = bvNone
              TabOrder = 0
              ExplicitHeight = 415
              object Panel7: TPanel
                Left = 0
                Top = 0
                Width = 481
                Height = 359
                Align = alClient
                BevelOuter = bvNone
                Font.Charset = DEFAULT_CHARSET
                Font.Color = clWindowText
                Font.Height = -11
                Font.Name = 'Tahoma'
                Font.Style = []
                ParentFont = False
                TabOrder = 0
                ExplicitLeft = 1
                ExplicitTop = 1
                ExplicitWidth = 479
                ExplicitHeight = 372
                object cxComboBoxHintDetailsKeywordType: TcxComboBox
                  Left = 112
                  Top = 125
                  Properties.Items.Strings = (
                    'ktBookmark'
                    'ktPage'
                    'ktSearch')
                  TabOrder = 0
                  Text = 'ktSearch'
                  Width = 97
                end
                object cxLabel12: TcxLabel
                  Left = 8
                  Top = 126
                  Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                end
                object cxTextEditHintDetailsPredicateValue: TcxTextEdit
                  Left = 112
                  Top = 152
                  TabOrder = 2
                  Width = 337
                end
                object cxLabel11: TcxLabel
                  Left = 8
                  Top = 153
                  Caption = #1047#1085#1072#1095#1077#1085#1080#1077
                end
                object cxTextEditHintPredicateValue: TcxTextEdit
                  Left = 112
                  Top = 62
                  TabOrder = 4
                  Width = 337
                end
                object cxLabel4: TcxLabel
                  Left = 8
                  Top = 63
                  Caption = #1047#1085#1072#1095#1077#1085#1080#1077' '#1087#1086#1080#1089#1082#1072
                end
                object cxComboBoxHintKeywordType: TcxComboBox
                  Left = 112
                  Top = 35
                  Properties.Items.Strings = (
                    'ktBookmark'
                    'ktSearch'
                    'ktPage')
                  TabOrder = 6
                  Text = 'ktSearch'
                  Width = 97
                end
                object cxLabel3: TcxLabel
                  Left = 8
                  Top = 36
                  Caption = #1058#1080#1087' '#1087#1086#1080#1089#1082#1072
                end
                object cxEditHintPredicateFilename: TcxButtonEdit
                  Left = 112
                  Top = 8
                  Properties.Buttons = <
                    item
                      Default = True
                      Kind = bkEllipsis
                    end>
                  Properties.ReadOnly = True
                  Properties.OnButtonClick = cxButtonEdit1PropertiesButtonClick
                  TabOrder = 8
                  Width = 337
                end
                object cxLabel2: TcxLabel
                  Left = 8
                  Top = 9
                  Caption = #1048#1089#1090#1086#1095#1085#1080#1082
                end
                object cxLabel1: TcxLabel
                  Left = 8
                  Top = 103
                  Caption = #1059#1090#1086#1095#1085#1077#1085#1080#1077
                end
              end
            end
          end
        end
        object Panel8: TPanel
          Left = 0
          Top = 0
          Width = 495
          Height = 73
          Align = alTop
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 1
          object cxLabel5: TcxLabel
            Left = 16
            Top = 16
            Caption = #1048#1076#1077#1085#1090#1080#1092#1080#1082#1072#1090#1086#1088' '#1082#1086#1084#1087#1086#1085#1077#1085#1090#1072
          end
          object cxEditIdentifierName: TcxTextEdit
            Left = 16
            Top = 40
            TabOrder = 1
            Text = 'KWOD12'
            Width = 441
          end
        end
        object Panel9: TPanel
          Left = 0
          Top = 469
          Width = 495
          Height = 41
          Align = alBottom
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          TabOrder = 2
          ExplicitLeft = 152
          ExplicitTop = 232
          ExplicitWidth = 273
          object cxButton1: TcxButton
            Left = 302
            Top = 9
            Width = 75
            Height = 25
            Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
            TabOrder = 0
            OnClick = cxButton1Click
          end
          object cxButton2: TcxButton
            Left = 398
            Top = 9
            Width = 75
            Height = 25
            Caption = #1054#1095#1080#1089#1090#1080#1090#1100
            TabOrder = 1
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
    Left = 112
    Top = 200
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 88
    Top = 128
    PixelsPerInch = 96
  end
  object OpenTextFileDialog1: TOpenTextFileDialog
    Filter = 'RTF|*.rtf|PDF|*.pdf'
    Left = 169
    Top = 96
  end
  object MainMenu1: TMainMenu
    Left = 40
    Top = 32
    object N1: TMenuItem
      Caption = #1056#1077#1076#1072#1082#1090#1086#1088
      object N2: TMenuItem
        Caption = '-'
      end
      object N3: TMenuItem
        Caption = #1047#1072#1082#1088#1099#1090#1100
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
    end
  end
end
