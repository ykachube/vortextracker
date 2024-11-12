object SpectrumAnalyzerForm: TSpectrumAnalyzerForm
  Left = 1114
  Top = 566
  BorderStyle = bsSingle
  Caption = 'SpectrumAnalyzer'
  ClientHeight = 304
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object pb1: TPaintBox
    Left = 0
    Top = 0
    Width = 513
    Height = 289
    Hint = 'wow'
  end
  object tmr1: TTimer
    Interval = 10
    OnTimer = tmr1Timer
    Left = 128
    Top = 104
  end
end
