object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'frmMain'
  ClientHeight = 481
  ClientWidth = 641
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 209
    Top = 41
    Width = 4
    Height = 421
    ExplicitHeight = 440
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 641
    Height = 41
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 280
      Top = 16
      Width = 31
      Height = 19
      Caption = 'Label1'
      Layout = tlCenter
    end
    object Button1: TButton
      Left = 8
      Top = 10
      Width = 75
      Height = 25
      Caption = '&Connect'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object ListView1: TListView
    Left = 0
    Top = 41
    Width = 209
    Height = 421
    Align = alLeft
    Columns = <
      item
        Caption = '#'
        Width = 0
      end
      item
        Caption = 'Opponent'
        Width = 80
      end
      item
        Caption = 'Word'
        Width = 70
      end
      item
        Caption = 'Pts'
        Width = 30
      end>
    OwnerData = True
    RowSelect = True
    TabOrder = 1
    ViewStyle = vsReport
    OnData = ListView1Data
    OnDblClick = ListView1DblClick
    ExplicitLeft = -1
  end
  object Panel4: TPanel
    Left = 213
    Top = 41
    Width = 428
    Height = 421
    Align = alClient
    TabOrder = 2
    ExplicitLeft = 304
    ExplicitTop = 128
    ExplicitWidth = 185
    ExplicitHeight = 249
    object Panel1: TPanel
      Left = 1
      Top = 1
      Width = 426
      Height = 375
      Align = alClient
      Color = 7648919
      ParentBackground = False
      TabOrder = 0
      OnResize = Panel1Resize
      ExplicitLeft = 0
      ExplicitTop = 41
      ExplicitWidth = 641
      ExplicitHeight = 440
    end
    object pnlRack: TPanel
      Left = 1
      Top = 376
      Width = 426
      Height = 44
      Align = alBottom
      ParentBackground = False
      TabOrder = 1
    end
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 462
    Width = 641
    Height = 19
    Panels = <>
    ExplicitLeft = 72
    ExplicitTop = 256
    ExplicitWidth = 0
  end
end
