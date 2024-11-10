unit AudioQueue;

interface

uses  Classes,SyncObjs;


type
  TBufferData = record
    Buffer: Pointer;
    Size: Integer;

  end;

  TAudioQueue = class
  private
    FList: TList;
    FLock: TCriticalSection;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Enqueue(const Item: TBufferData);
    function Dequeue(var Item: TBufferData): Boolean;
  end;

var

  daAudioQueue: TAudioQueue;

implementation




constructor TAudioQueue.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FList := TList.Create;
end;

destructor TAudioQueue.Destroy;
begin
  FLock.Free;
  FList.Free;
  inherited;
end;

procedure TAudioQueue.Enqueue(const Item: TBufferData);
var
NewItem: ^TBufferData;
begin
FLock.Enter;
try
New(NewItem);
NewItem^.Size := Item.Size;
GetMem(NewItem^.Buffer, Item.Size);
Move(Item.Buffer^, NewItem^.Buffer^, Item.Size);
FList.Add(NewItem);
//WriteLn('Enqueued buffer of size: ', Item.Size); // Logging
FreeMem(Item.Buffer);
finally

FLock.Leave;
end;
end;

function TAudioQueue.Dequeue(var Item: TBufferData): Boolean;
var
TempItem: ^TBufferData;
begin
Result := False;
FLock.Enter;
try
if FList.Count > 0 then
begin
TempItem := FList[0];
Item := TempItem^;
FList.Delete(0);
//WriteLn('Dequeued buffer of size: ', Item.Size); // Logging
Dispose(TempItem);
Result := True;
end;
finally

FLock.Leave;
end;
end;

initialization
   daAudioQueue := TAudioQueue.Create

finalization

  daAudioQueue.Free;

end.



