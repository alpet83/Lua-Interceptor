object NewProfileDialog: TNewProfileDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1057#1086#1079#1076#1072#1085#1080#1077' '#1087#1088#1086#1092#1080#1083#1103'.'
  ClientHeight = 102
  ClientWidth = 224
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    224
    102)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 13
    Width = 23
    Height = 13
    Caption = #1048#1084#1103':'
  end
  object Label2: TLabel
    Left = 8
    Top = 38
    Width = 21
    Height = 13
    Caption = 'PIN:'
  end
  object edtName: TMaskEdit
    Left = 95
    Top = 8
    Width = 121
    Height = 21
    Hint = #1048#1084#1103' '#1087#1088#1086#1092#1080#1083#1103', '#1085#1077' '#1084#1077#1085#1077#1077' 5 '#1089#1080#1084#1074#1086#1083#1086#1074
    TabOrder = 0
    OnChange = edtNameChange
  end
  object edtPIN: TMaskEdit
    Left = 95
    Top = 35
    Width = 115
    Height = 21
    Hint = #1053#1077#1086#1073#1103#1079#1072#1090#1077#1083#1100#1085#1099#1081' '#1082#1086#1076' '#1079#1072#1097#1080#1090#1099' '#1076#1086#1089#1090#1091#1087#1072
    EditMask = '0000;1;_'
    MaxLength = 4
    TabOrder = 1
    Text = '0000'
  end
  object btnOK: TButton
    Left = 8
    Top = 69
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1054#1050
    Enabled = False
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 141
    Top = 69
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1054#1090#1084#1077#1085#1072
    ModalResult = 2
    TabOrder = 3
  end
end
