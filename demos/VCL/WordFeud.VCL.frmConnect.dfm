object frmConnect: TfrmConnect
  Left = 0
  Top = 0
  ActiveControl = edEmail
  Caption = 'Connect'
  ClientHeight = 146
  ClientWidth = 318
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poOwnerFormCenter
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 25
    Top = 27
    Width = 24
    Height = 13
    Caption = 'Email'
  end
  object Label2: TLabel
    Left = 25
    Top = 54
    Width = 46
    Height = 13
    Caption = 'Password'
  end
  object edEmail: TEdit
    Left = 88
    Top = 24
    Width = 209
    Height = 21
    TabOrder = 0
  end
  object edPassword: TEdit
    Left = 88
    Top = 51
    Width = 209
    Height = 21
    TabOrder = 1
  end
  object Button1: TButton
    Left = 141
    Top = 95
    Width = 75
    Height = 25
    Caption = '&Connect'
    Default = True
    ModalResult = 1
    TabOrder = 2
  end
  object Button2: TButton
    Left = 222
    Top = 95
    Width = 75
    Height = 25
    Caption = 'Cancel'
    ModalResult = 3
    TabOrder = 3
  end
end
