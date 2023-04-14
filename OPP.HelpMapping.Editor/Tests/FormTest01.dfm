object FormTest1: TFormTest1
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu]
  Caption = 'FormTest1'
  ClientHeight = 186
  ClientWidth = 416
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 192
  TextHeight = 13
  object GroupBox11: TGroupBox
    AlignWithMargins = True
    Left = 12
    Top = 12
    Width = 392
    Height = 162
    Hint = 'Kod_OKWED wrong hint'
    HelpType = htKeyword
    HelpKeyword = 'Kod_KWED2'
    Margins.Left = 12
    Margins.Top = 12
    Margins.Right = 12
    Margins.Bottom = 12
    Align = alClient
    Caption = 'Help && Hints'
    ParentShowHint = False
    ShowHint = True
    TabOrder = 0
    object Kod_OKWED: TCheckBox
      Left = 32
      Top = 32
      Width = 153
      Height = 17
      HelpType = htKeyword
      HelpKeyword = 'Kod_OKWED'
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = '[Kod_OKWED]'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object Kod_MKC: TEdit
      Left = 32
      Top = 96
      Width = 153
      Height = 21
      Hint = 'cxHint test'
      HelpType = htKeyword
      HelpKeyword = '!'#1054#1089#1085#1086#1074#1072#1085#1080#1077
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      ParentCustomHint = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Text = '[!'#1054#1089#1085#1086#1074#1072#1085#1080#1077']'
    end
    object internalHelpViewerButton: TButton
      Left = 209
      Top = 94
      Width = 129
      Height = 25
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'Internal'
      TabOrder = 3
      OnClick = internalHelpViewerButtonClick
    end
    object externalHelpViewerButton: TButton
      Left = 209
      Top = 28
      Width = 129
      Height = 25
      HelpType = htKeyword
      HelpKeyword = 'Kod_OKWED'
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'External [Kod_OKWED]'
      TabOrder = 2
      OnClick = externalHelpViewerButtonClick
    end
  end
  object tipsRepo: TdxScreenTipRepository
    Left = 128
    Top = 136
    PixelsPerInch = 96
  end
  object cxHintStyleController1: TcxHintStyleController
    HintStyleClassName = 'TdxScreenTipStyle'
    HintStyle.ScreenTipLinks = <>
    HintStyle.ScreenTipActionLinks = <>
    Left = 40
    Top = 136
  end
end
