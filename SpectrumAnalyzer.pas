unit SpectrumAnalyzer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
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

implementation




function NextPowerOf2(Value: Integer): Integer;
begin
Result := 1;
while Result < Value do
Result := Result * 2;
end;

procedure TSpectrumAnalyzerForm.UpdateDisplay;
var
BarWidth, BarHeight, BarX, I, J: Integer;
Brush: TBrush;
Buf: TBufferData;
P: PSmallInt;
ComplexData: array of TIntComplex;
WindowSize, PaddedSize: Integer;
FFT: TIntFFT;
BandSize: Integer;
begin
P := nil;
FFT := TIntFFT.Create;

WriteLn('dequene');

try
while daAudioQueue.Dequeue(Buf) do
begin
  WriteLn('dequene begin');
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
    WriteLn('fft');
// Perform FFT
FFT.PerformFFT(ComplexData, False);
     WriteLn('calc');
// Calculate the magnitude of each frequency bin
BandSize := PaddedSize div 32;
for I := 0 to 31 do
begin
BarData[I].Frequency := I * (44100 / PaddedSize); // Assuming a sample rate of 44100 Hz
BarData[I].Level := 0;
for J := I * BandSize to (I + 1) * BandSize - 1 do
begin
BarData[I].Level := BarData[I].Level + Round(Sqrt(Sqr(ComplexData[J].Re) + Sqr(ComplexData[J].Im)));
end;
BarData[I].Level := BarData[I].Level div (BandSize*5); // Average the levels
end;
   WriteLn('free buf');
// Free the buffer memory after processing
FreeMem(Buf.Buffer);
Buf.Buffer := nil; // Set pointer to nil after freeing memory
 WriteLn('end ');
end;

// Draw the bars
BarWidth := pb1.Width div 32;
BarX := 0;

for I := 0 to 31 do
begin
BarHeight := Round((BarData[I].Level / 100) * pb1.Height);  // Adjust height calculation

Brush := TBrush.Create;
Brush.Style := bsHorizontal;
try
Brush.Color := RGB(255 - (BarData[I].Level * 255) div 100, (BarData[I].Level * 255) div 100, 0);
pb1.Canvas.Brush := Brush;
pb1.Canvas.FillRect(Rect(BarX, pb1.Height - BarHeight, BarX + BarWidth, pb1.Height));  // Draw from bottom up
finally
Brush.Free;
end;

BarX := BarX + BarWidth;
end;

pb1.Canvas.Refresh; // Ensure the canvas is refreshed to show updates
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
