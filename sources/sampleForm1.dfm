object SampleForm: TSampleForm
  Left = 0
  Top = 0
  Caption = 'SampleForm'
  ClientHeight = 676
  ClientWidth = 1002
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LinkToText: TCheckBox
    Left = 16
    Top = 24
    Width = 97
    Height = 17
    Caption = 'LinkToText'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
  end
  object Text1BookmarkFixed: TEdit
    Left = 16
    Top = 64
    Width = 121
    Height = 21
    Hint = 'cxHint test'
    ParentCustomHint = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Text = 'Text1BookmarkFixed'
  end
  object Button2: TButton
    Left = 345
    Top = 20
    Width = 75
    Height = 25
    Caption = #1089#1090#1080#1083#1080
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    OnClick = Button2Click
  end
  object cxHintController: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    UseHintControlLookAndFeel = True
    Left = 560
    Top = 32
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 560
    Top = 88
    PixelsPerInch = 96
  end
end
