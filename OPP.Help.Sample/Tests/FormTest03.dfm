object FormTest3: TFormTest3
  Left = 0
  Top = 0
  Caption = 'FormTest3'
  ClientHeight = 467
  ClientWidth = 997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 6
    Top = 6
    Width = 985
    Height = 455
    Margins.Left = 6
    Margins.Top = 6
    Margins.Right = 6
    Margins.Bottom = 6
    Align = alClient
    Caption = 'TOPPHelpPredicate'
    TabOrder = 0
    object Button1: TButton
      Left = 32
      Top = 48
      Width = 105
      Height = 25
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
      Width = 105
      Height = 25
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
      Width = 105
      Height = 25
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
      Width = 105
      Height = 25
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
