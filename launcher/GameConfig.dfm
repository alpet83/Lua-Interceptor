object ConfigDialog: TConfigDialog
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'NLC7 '#1053#1072#1089#1090#1088#1086#1081#1082#1080
  ClientHeight = 335
  ClientWidth = 394
  Color = clBtnFace
  Constraints.MaxHeight = 360
  Constraints.MaxWidth = 400
  Constraints.MinHeight = 360
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    394
    335)
  PixelsPerInch = 96
  TextHeight = 13
  object lbPatches: TLabel
    Left = 176
    Top = 58
    Width = 35
    Height = 13
    Caption = #1055#1072#1090#1095#1080':'
  end
  object Label2: TLabel
    Left = 8
    Top = 11
    Width = 103
    Height = 13
    Caption = #1056#1072#1079#1088#1077#1096#1077#1085#1080#1077' '#1101#1082#1088#1072#1085#1072':'
  end
  object clbPatches: TCheckListBox
    Left = 176
    Top = 80
    Width = 213
    Height = 229
    OnClickCheck = clbPatchesClickCheck
    ItemHeight = 13
    TabOrder = 0
  end
  object cbxResolution: TComboBox
    Left = 136
    Top = 8
    Width = 161
    Height = 21
    ItemIndex = 5
    TabOrder = 1
    Text = '1280x1024'
    Items.Strings = (
      '800x480'
      '800x600'
      '1024x768'
      '1280x768'
      '1280x800'
      '1280x1024'
      '1366x768'
      '1400x900'
      '1600x900'
      '1600x1200'
      '1680x1050'
      '1920x1080'
      '1920x1200'
      '2048x1080'
      '2048x1152'
      '2048x1536'
      '2560x1440'
      '2560x1600'
      '3656x2664'
      '3996x2160'
      '4096x1714'
      '4096x3112'
      ''
      '')
  end
  object btnSave: TButton
    Left = 305
    Top = 6
    Width = 81
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1057#1086#1093#1088#1072#1085#1080#1090#1100
    TabOrder = 2
    OnClick = btnSaveClick
  end
  object btnCancel: TButton
    Left = 303
    Top = 37
    Width = 83
    Height = 25
    Anchors = [akTop, akRight]
    Caption = #1054#1090#1084#1077#1085#1072
    TabOrder = 3
    OnClick = btnCancelClick
  end
  object chxFullScreen: TCheckBox
    Left = 136
    Top = 35
    Width = 161
    Height = 17
    Caption = #1055#1086#1083#1085#1086#1101#1082#1088#1072#1085#1085#1099#1081' '#1088#1077#1078#1080#1084
    TabOrder = 4
  end
  object sbMain: TStatusBar
    Left = 0
    Top = 316
    Width = 394
    Height = 19
    Panels = <
      item
        Text = '!'
        Width = 50
      end>
  end
  object gbProfiles: TGroupBox
    Left = 8
    Top = 64
    Width = 162
    Height = 245
    Caption = ' '#1055#1088#1086#1092#1080#1083#1100' '#1080#1075#1088#1086#1082#1072' '
    TabOrder = 6
    DesignSize = (
      162
      245)
    object lbxProfiles: TListBox
      Left = 9
      Top = 14
      Width = 141
      Height = 192
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 13
      TabOrder = 0
      OnClick = lbxProfilesClick
    end
    object btnAddProfile: TButton
      Left = 10
      Top = 212
      Width = 63
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = #1044#1086#1073#1072#1074#1080#1090#1100
      TabOrder = 1
      OnClick = btnAddProfileClick
    end
    object btnDelProfile: TButton
      Left = 88
      Top = 212
      Width = 63
      Height = 25
      Anchors = [akRight, akBottom]
      Caption = #1059#1076#1072#1083#1080#1090#1100
      TabOrder = 2
      OnClick = btnDelProfileClick
    end
  end
end
