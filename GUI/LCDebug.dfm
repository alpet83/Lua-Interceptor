object DebugMonitor: TDebugMonitor
  Left = 0
  Top = 0
  Caption = 'DebugMonitor'
  ClientHeight = 510
  ClientWidth = 846
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    846
    510)
  PixelsPerInch = 96
  TextHeight = 13
  object pgctl: TPageControl
    Left = 8
    Top = 8
    Width = 833
    Height = 457
    ActivePage = tsMap
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object tsMap: TTabSheet
      Caption = 'tsMap'
      DesignSize = (
        825
        429)
      object imgMap: TImage
        Left = 3
        Top = 3
        Width = 819
        Height = 390
        Anchors = [akLeft, akTop, akRight, akBottom]
        OnMouseMove = imgMapMouseMove
      end
      object lbInfo: TLabel
        Left = 518
        Top = 413
        Width = 28
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'lbInfo'
      end
      object btnDrawMap: TButton
        Left = 3
        Top = 399
        Width = 75
        Height = 25
        Anchors = [akLeft, akBottom]
        Caption = 'Draw Map'
        TabOrder = 0
        OnClick = btnDrawMapClick
      end
      object edtTest: TEdit
        Left = 84
        Top = 405
        Width = 325
        Height = 21
        Anchors = [akLeft, akBottom]
        TabOrder = 1
        Text = '0'
      end
      object chxVertexDump: TCheckBox
        Left = 415
        Top = 409
        Width = 97
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Vertex dump'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
    end
  end
  object updTimer: TTimer
    OnTimer = updTimerTimer
    Left = 24
    Top = 472
  end
end
