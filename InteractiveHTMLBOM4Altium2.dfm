object MainFrm: TMainFrm
  Left = 0
  Top = 0
  Caption = 'MainFrm'
  ClientHeight = 247
  ClientWidth = 661
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 24
    Top = 64
    Width = 27
    Height = 13
    Caption = 'Layer'
  end
  object Label1: TLabel
    Left = 24
    Top = 88
    Width = 34
    Height = 13
    Caption = 'Format'
  end
  object Panel1: TPanel
    Left = 0
    Top = 206
    Width = 661
    Height = 41
    Align = alBottom
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      661
      41)
    object OKBtn: TButton
      Left = 466
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 570
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object DarkModeChk: TCheckBox
    Left = 24
    Top = 16
    Width = 97
    Height = 17
    Caption = 'Dark mode'
    TabOrder = 1
  end
  object Highlighting1PinChk: TCheckBox
    Left = 184
    Top = 16
    Width = 136
    Height = 17
    Caption = 'Highlighting first pin'
    TabOrder = 2
  end
  object FabLayerChk: TCheckBox
    Left = 24
    Top = 32
    Width = 97
    Height = 17
    Caption = 'Fab layer'
    TabOrder = 3
  end
  object LayerFilterCb: TComboBox
    Left = 72
    Top = 56
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 4
  end
  object FormatCb: TComboBox
    Left = 72
    Top = 88
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 5
  end
  object AddNetsChk: TCheckBox
    Left = 184
    Top = 32
    Width = 136
    Height = 17
    Caption = 'Add Nets'
    TabOrder = 6
  end
  object AddTracksChk: TCheckBox
    Left = 344
    Top = 32
    Width = 136
    Height = 17
    Caption = 'Add Tracks'
    TabOrder = 7
  end
end
