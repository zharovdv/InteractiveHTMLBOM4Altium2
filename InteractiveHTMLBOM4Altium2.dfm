object MainFrm: TMainFrm
  Left = 0
  Top = 0
  Caption = 'InteractiveHTMLBOM4Altium'
  ClientHeight = 416
  ClientWidth = 426
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = OnFormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label2: TLabel
    Left = 16
    Top = 144
    Width = 27
    Height = 13
    Caption = 'Layer'
  end
  object Label1: TLabel
    Left = 16
    Top = 176
    Width = 34
    Height = 13
    Caption = 'Format'
  end
  object Label3: TLabel
    Left = 16
    Top = 16
    Width = 20
    Height = 13
    Caption = 'Title'
  end
  object Label4: TLabel
    Left = 16
    Top = 40
    Width = 45
    Height = 13
    Caption = 'Company'
  end
  object Label5: TLabel
    Left = 16
    Top = 64
    Width = 40
    Height = 13
    Caption = 'Revision'
  end
  object Label6: TLabel
    Left = 16
    Top = 232
    Width = 142
    Height = 13
    Caption = 'Component Fields as Columns'
  end
  object Label7: TLabel
    Left = 224
    Top = 232
    Width = 87
    Height = 13
    Caption = 'Group parameters'
  end
  object Label8: TLabel
    Left = 16
    Top = 208
    Width = 98
    Height = 13
    Caption = 'Override '#39'Value'#39' with'
  end
  object Panel1: TPanel
    Left = 0
    Top = 375
    Width = 426
    Height = 41
    Align = alBottom
    ParentBackground = False
    TabOrder = 0
    DesignSize = (
      426
      41)
    object OKBtn: TButton
      Left = 231
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'OK'
      TabOrder = 0
      OnClick = OKBtnClick
    end
    object CancelBtn: TButton
      Left = 335
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
    Left = 16
    Top = 96
    Width = 97
    Height = 17
    Caption = 'Dark mode'
    TabOrder = 1
  end
  object Highlighting1PinChk: TCheckBox
    Left = 176
    Top = 96
    Width = 136
    Height = 17
    Caption = 'Highlighting first pin'
    TabOrder = 2
  end
  object FabLayerChk: TCheckBox
    Left = 16
    Top = 112
    Width = 97
    Height = 17
    Caption = 'Fab layer'
    TabOrder = 3
  end
  object LayerFilterCb: TComboBox
    Left = 120
    Top = 136
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 4
  end
  object FormatCb: TComboBox
    Left = 120
    Top = 168
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 5
  end
  object AddNetsChk: TCheckBox
    Left = 176
    Top = 112
    Width = 136
    Height = 17
    Caption = 'Add Nets'
    TabOrder = 6
  end
  object AddTracksChk: TCheckBox
    Left = 336
    Top = 112
    Width = 136
    Height = 17
    Caption = 'Add Tracks'
    TabOrder = 7
  end
  object TitleEdt: TEdit
    Left = 64
    Top = 8
    Width = 121
    Height = 21
    TabOrder = 8
  end
  object CompanyEdt: TEdit
    Left = 64
    Top = 32
    Width = 121
    Height = 21
    TabOrder = 9
  end
  object RevisionEdt: TEdit
    Left = 64
    Top = 56
    Width = 121
    Height = 21
    TabOrder = 10
  end
  object ValueParameterCb: TComboBox
    Left = 120
    Top = 200
    Width = 145
    Height = 21
    Style = csDropDownList
    TabOrder = 11
  end
  object ColumnsParametersClb: TCheckListBox
    Left = 16
    Top = 256
    Width = 176
    Height = 96
    ItemHeight = 13
    TabOrder = 12
  end
  object GroupParametersClb: TCheckListBox
    Left = 224
    Top = 256
    Width = 176
    Height = 96
    ItemHeight = 13
    TabOrder = 13
  end
end
