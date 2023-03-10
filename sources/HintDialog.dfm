object OPPHintDialog: TOPPHintDialog
  Left = 0
  Top = 0
  Anchors = [akLeft, akTop, akRight, akBottom]
  AutoSize = True
  BorderIcons = []
  Caption = 'OPPHintDialog'
  ClientHeight = 182
  ClientWidth = 264
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    AlignWithMargins = True
    Left = 8
    Top = 8
    Width = 248
    Height = 166
    Margins.Left = 8
    Margins.Top = 8
    Margins.Right = 8
    Margins.Bottom = 8
    Align = alClient
    Alignment = taCenter
    AutoSize = False
    Caption = 'Lorem ipsum dolor sit'
    Constraints.MaxWidth = 800
    WordWrap = True
    ExplicitWidth = 105
    ExplicitHeight = 57
  end
end
