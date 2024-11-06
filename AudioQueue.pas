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
begin
  FLock.Enter;
  try
    FList.Add(@Item);
  finally
    FLock.Leave;
  end;
end;

function TAudioQueue.Dequeue(var Item: TBufferData): Boolean;
begin
  Result := False;
  FLock.Enter;
  try
    if FList.Count > 0 then
    begin
      Item := TBufferData(FList[0]^);
      FList.Delete(0);
      Result := True;
    end;
  finally
    FLock.Leave;
  end;
end;

end.



