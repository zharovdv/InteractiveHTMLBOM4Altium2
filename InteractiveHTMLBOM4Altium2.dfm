object MainFrm: TMainFrm
  Left = 0
  Top = 0
  Caption = 'InteractiveHTMLBOM4Altium'
  ClientHeight = 624
  ClientWidth = 639
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -17
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = OnFormShow
  PixelsPerInch = 144
  TextHeight = 21
  object Label2: TLabel
    Left = 24
    Top = 216
    Width = 40
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Layer'
  end
  object Label1: TLabel
    Left = 24
    Top = 264
    Width = 53
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Format'
  end
  object Label3: TLabel
    Left = 24
    Top = 24
    Width = 33
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Title'
  end
  object Label4: TLabel
    Left = 24
    Top = 60
    Width = 68
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Company'
  end
  object Label5: TLabel
    Left = 24
    Top = 96
    Width = 62
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Revision'
  end
  object Label6: TLabel
    Left = 24
    Top = 348
    Width = 222
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Component Fields as Columns'
  end
  object Label7: TLabel
    Left = 336
    Top = 348
    Width = 134
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Group parameters'
  end
  object Label8: TLabel
    Left = 24
    Top = 312
    Width = 154
    Height = 21
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Override '#39'Value'#39' with'
  end
  object Panel1: TPanel
    Left = 0
    Top = 563
    Width = 639
    Height = 61
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      639
      61)
    object OKBtn: TButton
      Left = 347
      Top = 12
      Width = 112
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 503
      Top = 12
      Width = 112
      Height = 38
      Margins.Left = 5
      Margins.Top = 5
      Margins.Right = 5
      Margins.Bottom = 5
      Anchors = [akTop, akRight]
      Caption = 'Cancel'
      TabOrder = 1
      OnClick = CancelBtnClick
    end
  end
  object DarkModeChk: TCheckBox
    Left = 24
    Top = 144
    Width = 146
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Dark mode'
    TabOrder = 1
  end
  object Highlighting1PinChk: TCheckBox
    Left = 264
    Top = 144
    Width = 204
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Highlighting first pin'
    TabOrder = 2
  end
  object FabLayerChk: TCheckBox
    Left = 24
    Top = 168
    Width = 146
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Fab layer'
    TabOrder = 3
  end
  object LayerFilterCb: TComboBox
    Left = 180
    Top = 204
    Width = 218
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    TabOrder = 4
  end
  object FormatCb: TComboBox
    Left = 180
    Top = 252
    Width = 218
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    TabOrder = 5
  end
  object AddNetsChk: TCheckBox
    Left = 264
    Top = 168
    Width = 204
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Add Nets'
    TabOrder = 6
  end
  object AddTracksChk: TCheckBox
    Left = 504
    Top = 168
    Width = 204
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = 'Add Tracks'
    TabOrder = 7
  end
  object TitleEdt: TEdit
    Left = 96
    Top = 12
    Width = 182
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 8
  end
  object CompanyEdt: TEdit
    Left = 96
    Top = 48
    Width = 182
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 9
  end
  object RevisionEdt: TEdit
    Left = 96
    Top = 84
    Width = 182
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    TabOrder = 10
  end
  object ValueParameterCb: TComboBox
    Left = 180
    Top = 300
    Width = 218
    Height = 29
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Style = csDropDownList
    TabOrder = 11
  end
  object ColumnsParametersClb: TCheckListBox
    Left = 24
    Top = 384
    Width = 264
    Height = 144
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ItemHeight = 21
    TabOrder = 12
  end
  object GroupParametersClb: TCheckListBox
    Left = 336
    Top = 384
    Width = 264
    Height = 144
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    ItemHeight = 21
    TabOrder = 13
  end
end
