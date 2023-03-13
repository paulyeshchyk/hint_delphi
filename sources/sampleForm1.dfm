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
  object Button1: TButton
    Left = 264
    Top = 20
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 0
    OnClick = Button1Click
  end
  object LinkToText: TCheckBox
    Left = 16
    Top = 24
    Width = 97
    Height = 17
    Caption = 'LinkToText'
    TabOrder = 1
  end
  object Edit1: TEdit
    Left = 16
    Top = 64
    Width = 121
    Height = 21
    Hint = 'cxHint test'
    ParentCustomHint = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    Text = 'Text1BookmarkFixed'
  end
  object cxRichEdit1: TcxRichEdit
    Left = 16
    Top = 128
    Lines.Strings = (
      'cxRichEdit1')
    TabOrder = 3
    Height = 89
    Width = 889
  end
  object cxHintStyleController1: TcxHintStyleController
    HintStyleClassName = 'TcxHintStyle'
    HintStyle.CallOutPosition = cxbpAuto
    HintStyle.CaptionFont.Charset = DEFAULT_CHARSET
    HintStyle.CaptionFont.Color = clWindowText
    HintStyle.CaptionFont.Height = -11
    HintStyle.CaptionFont.Name = 'Tahoma'
    HintStyle.CaptionFont.Style = []
    HintStyle.Font.Charset = DEFAULT_CHARSET
    HintStyle.Font.Color = clWindowText
    HintStyle.Font.Height = -11
    HintStyle.Font.Name = 'Tahoma'
    HintStyle.Font.Style = []
    HintStyle.IconSize = cxisSmall
    HintStyle.IconType = cxhiQuestion
    OnShowHintEx = cxHintStyleController1ShowHintEx
    Left = 568
    Top = 64
  end
end
