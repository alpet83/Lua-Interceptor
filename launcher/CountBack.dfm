object BreakoutForm: TBreakoutForm
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = #1054#1073#1088#1072#1073#1086#1090#1082#1072' '#1089#1073#1086#1103
  ClientHeight = 79
  ClientWidth = 318
  Color = clBtnFace
  DefaultMonitor = dmMainForm
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  DesignSize = (
    318
    79)
  PixelsPerInch = 96
  TextHeight = 13
  object lbInfo: TLabel
    Left = 48
    Top = 16
    Width = 212
    Height = 13
    Caption = #1048#1075#1088#1072' '#1073#1091#1076#1077#1090' '#1087#1077#1088#1077#1079#1072#1075#1088#1091#1078#1077#1085#1072' '#1095#1077#1088#1077#1079' 3 '#1089#1077#1082'...'
  end
  object btnCancel: TButton
    Left = 112
    Top = 46
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = #1054#1090#1084#1077#1085#1080#1090#1100
    TabOrder = 0
    OnClick = btnCancelClick
    ExplicitTop = 135
  end
  object tmrDownCount: TTimer
    OnTimer = tmrDownCountTimer
    Left = 280
    Top = 40
  end
end
