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
    tmr1: TTimer;

    procedure UpdateDisplay;
    procedure tmr1Timer(Sender: TObject);
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
   UpdateDisplay;
   Exit;
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
      //Inc (Buffer,I+J*(WindowSize - 1));
    //  Channels[J] := Channels[J] + Buffer^ * 1.0;
     // Buffer := OldBuffer;
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
  BarWidth, BarHeight, BarX, BarY, ColorLevel, I: Integer;
  Brush: TBrush;
begin
  // Generate random test data
  Randomize; // Initialize random number generator
  for I := 0 to High(BarData) do
  begin
    BarData[I].Frequency := 20.0 * I; // Assuming each bar represents an increasing frequency step
    BarData[I].Level := Random(100); // Generate a random level between 0 and 100
  end;

  BarWidth := pb1.Width div 32;
  BarHeight := pb1.Height;
  BarX := 0;
  BarY := 0;
  ColorLevel := 0;

  // Draw the bars
  for I := 0 to 31 do
  begin
    // Calculate the height of the bar
    BarHeight := Round((BarData[I].Level / 100) * pb1.Height);  // Adjust height calculation

    // Set the color of the bar
    Brush := TBrush.Create;
    Brush.Style := bsHorizontal;
    try
      ColorLevel := Round((BarData[I].Level * 16) / 100);   // Adjust color calculation for max level 100
      Brush.Color := RGB(255 - (ColorLevel * 255) div 16, (ColorLevel * 255) div 16, 0);
      pb1.Canvas.Brush := Brush;
      pb1.Canvas.FillRect(Rect(BarX, pb1.Height - BarHeight, BarX + BarWidth, pb1.Height));  // Draw from bottom up
    finally
      Brush.Free;
    end;

    // Move to the next bar
    BarX := BarX + BarWidth;
  end;

  pb1.Canvas.Refresh; // Ensure the canvas is refreshed to show updates
end;


{$R *.dfm}

procedure TSpectrumAnalyzerForm.tmr1Timer(Sender: TObject);
begin
 UpdateDisplay;
end;

end.
