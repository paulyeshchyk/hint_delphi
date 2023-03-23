object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 755
  ClientWidth = 1198
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poDesigned
  OnCreate = FormCreate
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 714
    Width = 1198
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitTop = 681
    ExplicitWidth = 1184
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
  object Edit1: TEdit
    Left = 498
    Top = 58
    Width = 359
    Height = 21
    TabOrder = 1
    Text = 'opphint://Kod_MKC'
    OnKeyPress = Edit1KeyPress
  end
  object Button2: TButton
    Left = 640
    Top = 256
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 736
    Top = 344
    Width = 75
    Height = 25
    Caption = 'send message'
    TabOrder = 3
  end
  object Kod_MKC: TEdit
    Left = 159
    Top = 1
    Width = 121
    Height = 21
    Hint = 'cxHint test'
    HelpType = htKeyword
    HelpKeyword = 'Kod_MKC'
    ParentCustomHint = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 4
    Text = 'Kod_MKC'
  end
  object Kod_OKWED: TCheckBox
    Left = 8
    Top = 12
    Width = 97
    Height = 17
    HelpType = htKeyword
    HelpKeyword = 'Kod_OKWED'
    Caption = '1Kod_OKWED'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 5
  end
  object cxHintController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    UseHintControlLookAndFeel = True
    OnShowHint = cxHintControllerShowHint
    OnShowHintEx = cxHintControllerShowHintEx
    Left = 952
    Top = 40
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 560
    Top = 88
    PixelsPerInch = 96
  end
end
