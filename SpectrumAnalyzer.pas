unit SpectrumAnalyzer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls;
type   TBarData =  record
    Frequency: Double;
    Level: Integer;
  end;
   TBarDataArray = array[0..32] of TBarData;


  TSpectrumAnalyzerForm = class(TForm)

   
     pb1: TPaintBox;



    procedure UpdateDisplay;
  public
    procedure SetAudioData(Buffer: PByte; Size: Integer);
  end;

var
  SpectrumAnalyzerForm: TSpectrumAnalyzerForm;
     FBuffer: array of Byte;
    FSize: Integer;
    BarData: TBarDataArray;

implementation

procedure TSpectrumAnalyzerForm.SetAudioData(Buffer: PByte; Size: Integer);
  const
    NumberOfChannels = 16;
    SampleRate = 3;
    NumberOfBars = 32 ;
var
  Channels: array[0..NumberOfChannels - 1] of Double;
  WindowSize: Integer;
  BinSize: Double;
  OldBuffer: PByte;

  I, J: Integer;

begin

   OldBuffer := Buffer ;
  //SetLength(FBuffer,Size);
  //System.Move(Buffer^,Pointer(FBuffer),Size);
 /// FreeMemory(Buffer);

  FSize := Size ;
  WindowSize := Size div NumberOfChannels;
  BinSize := SampleRate * 1.0 / WindowSize;

  // Initialize the bar data
  for I := 0 to NumberOfBars-1 do
  begin
    BarData[I].Frequency := I * BinSize;
    BarData[I].Level := 0;
  end;

  // Calculate the level for each bar
  for J := 0 to NumberOfChannels - 2 do
  begin
    for I := 0 to WindowSize - 2 do
    begin
      Inc (Buffer,I+J*(WindowSize - 1));
      Channels[J] := Channels[J] + Buffer^ * 1.0;
      Buffer := OldBuffer;
    end;

    // Calculate the RMS value for each channel
    Channels[J] := Sqrt(Channels[J] / WindowSize);

    // Calculate the level for each bar
    for I := 0 to 31 do
    begin
      if (BarData[I].Frequency >= Channels[J]) then
        BarData[I].Level := BarData[I].Level + Round(Channels[J] * Channels[J]);
    end;
  end;

  // Normalize the bar data
  for I := 0 to 31 do
  begin
    BarData[I].Level := Round( Sqrt(BarData[I].Level));
  end;




  UpdateDisplay;
end;

procedure TSpectrumAnalyzerForm.UpdateDisplay;

  var
  
  BarWidth: Integer;
  BarHeight: Integer;
  BarX, BarY: Integer;
  ColorLevel: Integer;
  Brush: TBrush;
  I: Integer;
begin
 // BarData := TBarDataArray(Self.Tag);
  BarWidth := pb1.Width div 32;
  BarHeight := pb1.Height;
  BarX := 0;
  BarY := 0;
  ColorLevel := 0;

  // Draw the bars
  for I := 0 to 31 do
  begin
    // Calculate the height of the bar
    BarHeight := Round(BarData[I].Level * pb1.Height);

    // Set the color of the bar
    Brush := TBrush.Create;
    try
      ColorLevel := Round((BarData[I].Level * 16) / 255);
     Brush.Color := RGB(255 - (ColorLevel * 255) div 16, (ColorLevel * 255) div 16, 0);
      pb1.Canvas.Brush := Brush;
      pb1.Canvas.FillRect(Rect(BarX, BarY, BarX + BarWidth, BarY + BarHeight));
    finally
      Brush.Free;
    end;

    // Move to the next bar
    BarX := BarX + BarWidth;
  end;
end;



{$R *.dfm}

end.
