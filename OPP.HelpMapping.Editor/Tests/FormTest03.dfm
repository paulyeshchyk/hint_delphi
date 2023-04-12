object FormTest3: TFormTest3
  Left = 0
  Top = 0
  Caption = 'FormTest3'
  ClientHeight = 932
  ClientWidth = 1994
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -22
  Font.Name = 'Tahoma'
  Font.Style = []
  PixelsPerInch = 192
  TextHeight = 27
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 12
    Top = 12
    Width = 1970
    Height = 908
    Margins.Left = 12
    Margins.Top = 12
    Margins.Right = 12
    Margins.Bottom = 12
    Align = alClient
    Caption = 'TOPPHelpPredicate'
    TabOrder = 0
    object Button1: TButton
      Left = 64
      Top = 96
      Width = 210
      Height = 50
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'Save to Stream'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Memo1: TMemo
      Left = 608
      Top = 96
      Width = 1026
      Height = 450
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Lines.Strings = (
        'Memo1')
      TabOrder = 1
    end
    object Button4: TButton
      Left = 64
      Top = 640
      Width = 210
      Height = 50
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'Copy'
      TabOrder = 2
    end
    object Button2: TButton
      Left = 64
      Top = 228
      Width = 210
      Height = 50
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'Save to file'
      TabOrder = 3
      OnClick = Button2Click
    end
    object Button5: TButton
      Left = 64
      Top = 352
      Width = 210
      Height = 50
      Margins.Left = 12
      Margins.Top = 12
      Margins.Right = 12
      Margins.Bottom = 12
      Caption = 'Read from file'
      TabOrder = 4
      OnClick = Button5Click
    end
  end
end
