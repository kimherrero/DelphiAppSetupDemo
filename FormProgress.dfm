object FrmProgress: TFrmProgress
  Left = 594
  Top = 351
  BorderIcons = []
  BorderStyle = bsNone
  ClientHeight = 77
  ClientWidth = 301
  Color = clBtnShadow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Meter: TAbMeter
    Left = 24
    Top = 32
    Width = 257
    Height = 25
    Ctl3D = False
    Orientation = moHorizontal
    UnusedColor = clSilver
    UsedColor = clNavy
  end
  object lbText: TLabel
    Left = 32
    Top = 16
    Width = 242
    Height = 13
    Caption = 'Updater is working on requested operation'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
end
