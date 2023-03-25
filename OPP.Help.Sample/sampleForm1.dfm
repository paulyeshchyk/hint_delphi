object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 409
  ClientWidth = 802
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnClose = FormClose
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 368
    Width = 802
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 335
    ExplicitWidth = 788
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 8
    Width = 425
    Height = 113
    Caption = 'Help && Hints'
    TabOrder = 1
    object Kod_OKWED: TCheckBox
      Left = 24
      Top = 28
      Width = 97
      Height = 17
      HelpType = htKeyword
      HelpKeyword = 'Kod_OKWED'
      Caption = '1Kod_OKWED'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object Kod_MKC: TEdit
      Left = 24
      Top = 65
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
    object Button1: TButton
      Left = 302
      Top = 63
      Width = 75
      Height = 25
      Caption = #1055#1086#1084#1086#1097#1100
      TabOrder = 3
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 302
      Top = 24
      Width = 75
      Height = 25
      Caption = 'External'
      TabOrder = 2
      OnClick = Button2Click
    end
  end
  object GroupBox2: TGroupBox
    Left = 456
    Top = 8
    Width = 313
    Height = 105
    Caption = 'GroupBox2'
    TabOrder = 2
    object Label1: TLabel
      Left = 16
      Top = 24
      Width = 31
      Height = 13
      Caption = 'Label2'
    end
    object CheckBox1: TCheckBox
      Left = 16
      Top = 65
      Width = 97
      Height = 17
      Caption = 'CheckBox1'
      TabOrder = 0
    end
    object Edit1: TEdit
      Left = 160
      Top = 21
      Width = 121
      Height = 21
      TabOrder = 1
      Text = 'Edit1'
    end
    object Button3: TButton
      Left = 206
      Top = 61
      Width = 75
      Height = 25
      Caption = 'Button3'
      TabOrder = 2
    end
  end
  object cxHintController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    UseHintControlLookAndFeel = True
    OnShowHint = cxHintControllerShowHint
    OnShowHintEx = cxHintControllerShowHintEx
    Left = 1096
    Top = 48
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 1088
    Top = 168
    PixelsPerInch = 96
  end
end
