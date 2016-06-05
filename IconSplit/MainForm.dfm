object ISPMainForm: TISPMainForm
  Left = 0
  Top = 0
  Caption = 'ISPMainForm'
  ClientHeight = 473
  ClientWidth = 860
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    860
    473)
  PixelsPerInch = 96
  TextHeight = 13
  object imgSource: TImage
    Left = 8
    Top = 8
    Width = 844
    Height = 433
  end
  object imgDest: TImage
    Left = 464
    Top = 8
    Width = 377
    Height = 121
  end
  object btnLoad: TButton
    Left = 8
    Top = 447
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Load'
    TabOrder = 0
    OnClick = btnLoadClick
  end
  object btnSplit: TButton
    Left = 89
    Top = 447
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Split'
    TabOrder = 1
    OnClick = btnSplitClick
  end
end
