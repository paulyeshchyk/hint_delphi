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
  OnCreate = FormCreate
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
  object RichEdit1: TRichEdit
    Left = 40
    Top = 376
    Width = 889
    Height = 89
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    Lines.Strings = (
      'RichEdit1')
    ParentFont = False
    TabOrder = 3
  end
  object docServer: TdxRichEditDocumentServer
    Left = 400
    Top = 112
  end
end
