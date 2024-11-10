unit SpectrumAnalyzer;

interface

uses
  Windows, Math, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, AudioQueue, IntFFT;
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

   
  end;

var
  SpectrumAnalyzerForm: TSpectrumAnalyzerForm;
     FBuffer: array of Byte;
    FSize: Integer;
    BarData: TBarDataArray;
    PatternBitmap: TBitmap;


implementation





function NextPowerOf2(Value: Integer): Integer;
begin
Result := 1;
while Result < Value do
Result := Result * 2;

//Result := Result * 2;
end;





procedure TSpectrumAnalyzerForm.UpdateDisplay;
var
BarWidth, BarHeight, BarX, I, J, K: Integer;
Brush: TBrush;
Buf: TBufferData;
P: PSmallInt;
ComplexData: array of TIntComplex;
WindowSize, PaddedSize: Integer;
FFT: TIntFFT;
BandSize: Integer;
MaxLevel, AvgLevel: Double;
LastBarData: array[0..32] of Integer;
IntermediateBarData: array[0..32] of Integer;
frameCount: Integer;
LogFrequency: Double;
LogBins: array[0..32] of Integer;
TotalLevel: Double;
begin
MaxLevel := 1;
TotalLevel := 0;
P := nil;
FFT := TIntFFT.Create;

// Save the current bar levels
for I := 0 to 31 do
LastBarData[I] := BarData[I].Level;

try
while daAudioQueue.Dequeue(Buf) do
begin
P := PSmallInt(Buf.Buffer);  // Cast to PSmallInt to work with 16-bit audio data
if P = nil then
Exit;

WindowSize := Buf.Size div SizeOf(SmallInt);
PaddedSize := NextPowerOf2(WindowSize);
SetLength(ComplexData, PaddedSize);

// Convert audio data to complex numbers and pad with zeros
for I := 0 to WindowSize - 1 do
begin
ComplexData[I].Re := P^;
ComplexData[I].Im := 0;
Inc(P);  // Move to the next sample
end;
for I := WindowSize to PaddedSize - 1 do
begin
ComplexData[I].Re := 0;
ComplexData[I].Im := 0;
end;

// Perform FFT
FFT.PerformFFT(ComplexData, False);

// Calculate the logarithmic frequency bins
for I := 0 to 32 do  
LogBins[I] := Round(PaddedSize * (Log10(I + 2) / Log10(32 + 1)));

// Calculate the magnitude of each frequency bin
for I := 0 to 31 do      //mem corruption
begin
BarData[I].Frequency := LogBins[I] * (44100 / PaddedSize); // Assuming a sample rate of 44100 Hz
BarData[I].Level := 0;
for J := LogBins[I] to LogBins[I + 1] - 1 do
begin
if J < PaddedSize then
BarData[I].Level := BarData[I].Level + Round(Sqrt(Sqr(ComplexData[J].Re) + Sqr(ComplexData[J].Im)));
end;
TotalLevel := TotalLevel + BarData[I].Level;
if BarData[I].Level > MaxLevel then MaxLevel := BarData[I].Level;
end;

// Calculate the average level
AvgLevel := TotalLevel / 31+0.01;

// Normalize the levels using adaptive scaling
for I := 0 to 30 do
begin
BarData[I].Level := Round((BarData[I].Level / AvgLevel) * 50); // Scale relative to average level
if BarData[I].Level > 100 then BarData[I].Level := 100; // Cap at 100
end;

// Free the buffer memory after processing
FreeMem(Buf.Buffer);
Buf.Buffer := nil; // Set pointer to nil after freeing memory
end;

// Draw the intermediate frames
frameCount := 4;

for K := 1 to frameCount do
begin
BarWidth := pb1.Width div 32;
BarX := 0;

for I := 0 to 31 do
begin
IntermediateBarData[I] := LastBarData[I] + (BarData[I].Level - LastBarData[I]) * K div frameCount;
BarHeight := Round((IntermediateBarData[I] / 100) * pb1.Height);  // Adjust height calculation

Brush := TBrush.Create;
try
//Brush.Style := bsHorizontal;
Brush.Color := RGB(
Min(255, 255 - Round(Abs(IntermediateBarData[I] - 50) * 5.1)), // Red component
Min(255, Round(IntermediateBarData[I] * 2.55)), // Green component
Max(0, 255 - Round((IntermediateBarData[I] * 4.55))) // Blue component
);

pb1.Canvas.Brush := Brush;
//SetBKColor(pb1.Canvas.Handle, Brush.Color);
pb1.Canvas.FillRect(Rect(BarX, pb1.Height - BarHeight, BarX + BarWidth - 3, pb1.Height));  // Draw from bottom up
Brush.Color := clWhite;
pb1.Canvas.Brush := Brush;
pb1.Canvas.FillRect(Rect(BarX, 0 , BarX + BarWidth - 3, pb1.Height - BarHeight));  // fill white
finally
Brush.Free;
end;

BarX := BarX + BarWidth;
end;

pb1.Canvas.Refresh; // Ensure the canvas is refreshed to show updates
Sleep(10); // Small delay to make the transition visible
end;
finally
FFT.Free;
end;
end;

{$R *.dfm}

procedure TSpectrumAnalyzerForm.tmr1Timer(Sender: TObject);
begin
 UpdateDisplay;
end;




end.
