unit IntFFT;

interface

uses SysUtils;

type
TIntComplex = record
Re, Im: Integer;
end;

TTwiddleFactors = record
WPR, WPI: Integer;
end;

TIntFFT = class
private


procedure CalcTwiddleFactors(M: Integer; var TwiddleFactors: TTwiddleFactors; Inverse: Boolean);
public
procedure PerformFFT(var Data: array of TIntComplex; Inverse: Boolean);
end;

implementation

uses
Math;

procedure TIntFFT.CalcTwiddleFactors(M: Integer; var TwiddleFactors: TTwiddleFactors; Inverse: Boolean);
var
Theta: Double;
begin
Theta := -2 * Pi / M;
if Inverse then
Theta := -Theta;
TwiddleFactors.WPR := -Round(2 * Sin(0.5 * Theta) * Sin(0.5 * Theta) * 32768);
TwiddleFactors.WPI := -Round(Sin(Theta) * 32768);
end;

procedure TIntFFT.PerformFFT(var Data: array of TIntComplex; Inverse: Boolean);
var
N, I, J, K, M, Size: Integer;
TwiddleFactors: TTwiddleFactors;
Temp: TIntComplex;
WTemp: Integer;
begin
Size := Length(Data);
if Size < 2 then
Exit;
if (Size and (Size - 1)) <> 0 then
raise Exception.Create('Array length not a power of 2');

N := 2;
while N <= Size do
begin
TwiddleFactors.WPR := 0;
TwiddleFactors.WPI := 0;

M := Size div N;
for K := 0 to N - 1 do
begin
//WriteLn('K: ', K);  // Debug information
for I := K to Size - 1 do
begin
J := I + M;
if (I >= Size) or (J >= Size) then
begin
//WriteLn('Index out of bounds: I = ', I, ', J = ', J, ', Size = ', Size);
Exit;
end;
//WriteLn('I: ', I, ', J: ', J);  // Debug information
WTemp := Round(Data[J].Re * TwiddleFactors.WPR - Data[J].Im * TwiddleFactors.WPI) + Data[I].Re;
Data[I].Im := Round(Data[J].Re * TwiddleFactors.WPI + Data[J].Im * TwiddleFactors.WPR) + Data[I].Im;
Data[J].Re := WTemp;

Data[J].Im := Round(Data[J].Im); // Discard fractional part
Data[I].Im := Round(Data[I].Im); // Discard fractional part
end;

CalcTwiddleFactors(N, TwiddleFactors, Inverse);
end;

N := N * 2;
end;

if Inverse then
for I := 0 to Size - 1 do
begin
Data[I].Re := Data[I].Re div Size;
Data[I].Im := Data[I].Im div Size;
end;
end;

end.
