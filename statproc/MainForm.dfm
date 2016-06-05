object MForm: TMForm
  Left = 0
  Top = 0
  Caption = 'MForm'
  ClientHeight = 805
  ClientWidth = 1462
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    1462
    805)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 776
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Progress 0%'
  end
  object Label2: TLabel
    Left = 8
    Top = 752
    Width = 101
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Volume in mebibytes:'
  end
  object Label3: TLabel
    Left = 8
    Top = 726
    Width = 84
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Source files path:'
  end
  object ggProgress: TGauge
    Left = 207
    Top = 749
    Width = 555
    Height = 21
    Anchors = [akLeft, akBottom]
    Progress = 0
  end
  object Label4: TLabel
    Left = 790
    Top = 725
    Width = 75
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'User src ROOT:'
  end
  object sgFiles: TStringGridEx
    Left = 8
    Top = 8
    Width = 1446
    Height = 706
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultRowHeight = 16
    FixedCols = 0
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    ColWidths = (
      129
      425
      139
      50
      160)
  end
  object btnAnalyse: TButton
    Left = 687
    Top = 720
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Analyse'
    TabOrder = 1
    OnClick = btnAnalyseClick
  end
  object edtSrcPath: TEdit
    Left = 126
    Top = 722
    Width = 555
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    OnDblClick = edtSrcPathDblClick
  end
  object edtVolume: TEdit
    Left = 126
    Top = 749
    Width = 75
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '1300'
  end
  object btnExit: TButton
    Left = 1379
    Top = 772
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Exit'
    TabOrder = 4
    OnClick = btnExitClick
  end
  object lbxRoot: TListBox
    Left = 880
    Top = 720
    Width = 493
    Height = 77
    Anchors = [akLeft, akRight, akBottom]
    ItemHeight = 13
    TabOrder = 5
  end
  object btnAddRoot: TButton
    Left = 1379
    Top = 721
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'Add ROOT'
    TabOrder = 6
    OnClick = btnAddRootClick
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofPathMustExist, ofEnableSizing]
    Left = 832
    Top = 736
  end
end
