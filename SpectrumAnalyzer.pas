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
MaxLevel: Integer;
LastBarData: array[0..31] of Integer;
IntermediateBarData: array[0..31] of Integer;
frameCount: Integer;
LogFrequency: Double;
LogBarPositions: array[0..31] of Integer;
begin
MaxLevel := 1;
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

// Calculate the magnitude of each frequency bin
BandSize := PaddedSize div 64;
for I := 0 to 31 do
begin
LogFrequency := Log10(I + 1); // Use logarithmic scale for frequency
BarData[I].Frequency := LogFrequency * (44100 / PaddedSize); // Assuming a sample rate of 44100 Hz
BarData[I].Level := 0;
for J := I * BandSize to (I + 1) * BandSize - 1 do
begin
if J < PaddedSize then
BarData[I].Level := BarData[I].Level + Round(Sqrt(Sqr(ComplexData[J].Re) + Sqr(ComplexData[J].Im)));
if BarData[I].Level > MaxLevel then MaxLevel := BarData[I].Level;
end;
BarData[I].Level := Round(BarData[I].Level / MaxLevel * 100); // Average the levels
end;

// Free the buffer memory after processing
FreeMem(Buf.Buffer);
Buf.Buffer := nil; // Set pointer to nil after freeing memory
end;

// Calculate logarithmic positions for the bars
for I := 0 to 31 do
LogBarPositions[I] := Round(Log10(I + 2) / Log10(32 + 1) * pb1.Width);

// Draw the intermediate frames
frameCount := 4;

for K := 1 to frameCount do
begin
BarX := 0;

for I := 0 to 31 do
begin
IntermediateBarData[I] := LastBarData[I] + (BarData[I].Level - LastBarData[I]) * K div frameCount;
BarHeight := Round((IntermediateBarData[I] / 100) * pb1.Height);  // Adjust height calculation

Brush := TBrush.Create;
try
Brush.Style := bsHorizontal;
Brush.Color := RGB(
Min(255, 255 - Round(Abs(IntermediateBarData[I] - 50) * 5.1)), // Red component
Min(255, Round(IntermediateBarData[I] * 2.55)), // Green component
Max(0, 255 - Round((IntermediateBarData[I] * 4.55))) // Blue component
);

pb1.Canvas.Brush := Brush;
SetBKColor(pb1.Canvas.Handle, Brush.Color);
pb1.Canvas.FillRect(Rect(LogBarPositions[I], pb1.Height - BarHeight, LogBarPositions[I] + BarWidth - 3, pb1.Height));  // Draw from bottom up
finally
Brush.Free;
end;

BarX := LogBarPositions[I];
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
