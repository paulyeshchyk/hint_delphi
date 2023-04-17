object OPPHintAttributeSchemeEditorForm: TOPPHintAttributeSchemeEditorForm
  Left = 0
  Top = 0
  Caption = 'OPPHintAttributeSchemeEditorForm'
  ClientHeight = 491
  ClientWidth = 1065
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
  object dxBarDockControl1: TdxBarDockControl
    Left = 0
    Top = 0
    Width = 1065
    Align = dalTop
  end
  object Panel1: TPanel
    Left = 0
    Top = 450
    Width = 1065
    Height = 41
    Align = alBottom
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 944
      Top = 4
      Width = 117
      Height = 33
      Action = ActionClose
      Align = alRight
      TabOrder = 0
    end
  end
  object cxGrid1: TcxGrid
    Left = 0
    Top = 31
    Width = 1065
    Height = 419
    Align = alClient
    TabOrder = 2
    ExplicitLeft = -8
    ExplicitTop = 29
    object cxGrid1DBTableView1: TcxGridDBTableView
      Navigator.Buttons.CustomButtons = <>
      DataController.DataSource = SchemeDataSource
      DataController.Summary.DefaultGroupSummaryItems = <>
      DataController.Summary.FooterSummaryItems = <>
      DataController.Summary.SummaryGroups = <>
      OptionsBehavior.EditMode = emInplaceEditForm
      OptionsSelection.InvertSelect = False
      object cxGrid1DBTableView1ClassName1: TcxGridDBColumn
        DataBinding.FieldName = 'ClassName'
        PropertiesClassName = 'TcxLookupComboBoxProperties'
        Properties.ListColumns = <>
        Width = 103
      end
      object cxGrid1DBTableView1PropertyName1: TcxGridDBColumn
        DataBinding.FieldName = 'PropertyName'
        Width = 128
      end
    end
    object cxGrid1Level1: TcxGridLevel
      GridView = cxGrid1DBTableView1
    end
  end
  object ActionList1: TActionList
    Left = 272
    Top = 96
    object ActionClose: TAction
      Caption = #1047#1072#1082#1088#1099#1090#1100
      OnExecute = ActionCloseExecute
    end
    object actionAddRecord: TAction
      Caption = 'actionAddRecord'
      OnExecute = actionAddRecordExecute
    end
    object actionSaveAs: TAction
      Caption = 'actionSave'
      OnExecute = actionSaveAsExecute
    end
    object actionReloadDataset: TAction
      Caption = 'actionReloadDataset'
      OnExecute = actionReloadDatasetExecute
    end
    object actionReloadRTTIDataset: TAction
      Caption = 'actionReloadRTTIDataset'
    end
    object actionSave: TAction
      Caption = 'actionSave'
      OnExecute = actionSaveExecute
    end
  end
  object SchemeDataSet: TClientDataSet
    PersistDataPacket.Data = {
      D10000009619E0BD0100000018000000020004000000030000009C0009436C61
      73734E616D65020049001000010005574944544802000200FF000C50726F7065
      7274794E616D65020049001000010005574944544802000200FF0001000A4348
      414E47455F4C4F47040082000C00000001000000000000000400000002000000
      0000000004000000030000000200000008000000040000000100000008000000
      050401003105040100310C041000546F70704F626A4174747269627574650C00
      06005450616E656C0B0048656C704B6579776F7264}
    Active = True
    Aggregates = <>
    FieldDefs = <
      item
        Name = 'ClassName'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 255
      end
      item
        Name = 'PropertyName'
        Attributes = [faUnNamed]
        DataType = ftString
        Size = 255
      end>
    IndexDefs = <>
    Params = <>
    StoreDefs = True
    Left = 32
    Top = 160
    object SchemeDataSetClassName: TStringField
      FieldName = 'ClassName'
      Size = 255
    end
    object SchemeDataSetPropertyName: TStringField
      FieldName = 'PropertyName'
      Size = 255
    end
  end
  object SchemeDataSource: TDataSource
    DataSet = SchemeDataSet
    Left = 40
    Top = 224
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
    Left = 368
    Top = 96
    PixelsPerInch = 96
    DockControlHeights = (
      0
      0
      28
      0)
    object dxBarManager1Bar1: TdxBar
      AllowClose = False
      AllowCustomizing = False
      AllowQuickCustomizing = False
      AllowReset = False
      Caption = 'Default'
      CaptionButtons = <>
      DockedDockingStyle = dsTop
      DockedLeft = 0
      DockedTop = 0
      DockingStyle = dsTop
      FloatLeft = 1099
      FloatTop = 8
      FloatClientWidth = 0
      FloatClientHeight = 0
      IsMainMenu = True
      ItemLinks = <
        item
          Visible = True
          ItemName = 'dxBarButtonNewRecord'
        end
        item
          Visible = True
          ItemName = 'dxBarButtonSave'
        end>
      MultiLine = True
      OneOnRow = True
      Row = 0
      UseOwnFont = False
      Visible = True
      WholeRow = True
    end
    object dxBarButtonSave: TdxBarButton
      Action = actionSaveAs
      Category = 0
      Glyph.SourceDPI = 96
      Glyph.Data = {
        89504E470D0A1A0A0000000D49484452000000100000001008060000001FF3FF
        6100000017744558745469746C65005361766520416C6C3B536176653B416C6C
        6E14F5A10000022749444154785E7D933D6B14511885CF9D9D240B2BBA88D5B6
        362A8A22D649616B2B361A6C2C6C640B1B4152596AAD36F90356B1B08A4DAC14
        D318502C048B40D84DB2EE6EB23B73E7BE1FCE7B6707B2063CCB993B2C9C33CF
        3B2FE35415B53A9D8E03604E302F87D312738A7925771FAD7D740E2B2A0A2B97
        780AA4B48A4058E0F31C4AB4B5F9FEEDED530580AE5CBB7209BDC3612CB01FB8
        2A1BF4FBF09CA3B1B484A3109601345257AAC66EB55A49E1FDD36936419E4F9D
        51D424C17B4C8EC7EA5CA20EEE155180E5520BAE765F46EC51AF8754053F76BE
        830A4241040A01FDDE3E9A67DB70CABF2E9C3FF7BAD1486D94AAA0C6BE79FD2A
        363FEC62EDD9E338B7A819117DF56117ADE61246E3A38B8142E2D00033D50570
        C28C40543A8058B07738018BD645600A1011909DCC9044C0C4B120B10BB38002
        8188622890805863892A40CCB18099C124AEBE9F23B079B9F4FD074FC0CA5091
        D91AA51AC70AE303D81999CA89020B8640B87CE35615826232FC83ECF838864D
        2215B6904017759E808CC007F4F776F16EFD05FED59D7B5D349205301358D879
        9FD75B403542B5AE68D3C1284794020A452802B060AB630C0EF671A65C2991FF
        048062810583B928AA9C5A0CA82FF67F9A3623F697CF1BEBED76FB0D8000C0CF
        BD83E00B3CDFF809129B11B3B32270491609B22CCBA7D3E9183359011D0D075B
        DFB6BF2E2F96983BDBBF41EC5094219F53E90CAE2408C4A05061A3D6894FB709
        60C168FEF309EB0C3B5755C64C7F016522C355A0F7E6060000000049454E44AE
        426082}
    end
    object dxBarButtonNewRecord: TdxBarButton
      Action = actionAddRecord
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
  end
end
