object FrmMsg: TFrmMsg
  Left = 375
  Top = 333
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = ' App Setup Demo'
  ClientHeight = 127
  ClientWidth = 482
  Color = clMedGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object lbMsg: TLabel
    Left = 24
    Top = 24
    Width = 433
    Height = 33
    Alignment = taCenter
    AutoSize = False
    Caption = 'lbMsg'
    Color = clWindow
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
    WordWrap = True
  end
  object btOk: TButton
    Left = 264
    Top = 80
    Width = 89
    Height = 25
    Caption = 'Yes'
    ModalResult = 1
    TabOrder = 0
    TabStop = False
  end
  object btKo: TButton
    Left = 368
    Top = 80
    Width = 89
    Height = 25
    Caption = 'No'
    ModalResult = 2
    TabOrder = 1
    TabStop = False
  end
end
