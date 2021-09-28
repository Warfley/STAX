unit stax.asyncio;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, stax;

type

  EReadError = class(Exception);
  EWriteError = class(Exception);

  { TIOReader }

  TIOReader = class
  private
    FAsyncSleepTime: Integer;
  public
    constructor Create(AsyncSleepTime: Integer);

    function TryRead(ABuffer: Pointer; ACount: SizeInt): SizeInt; virtual; abstract;
    function AsyncRead(AExecutor: TExecutor; ABuffer: Pointer; ACount: SizeInt; AwaitFullData: Boolean): SizeInt;
  end;

  { TIOWriter }

  TIOWriter = class
  private
    FAsyncSleepTime: Integer;
  public
    constructor Create(AsyncSleepTime: Integer);

    function TryWrite(ABuffer: Pointer; ACount: SizeInt): SizeInt; virtual; abstract;
    function AsyncWrite(AExecutor: TExecutor; ABuffer: Pointer; ACount: SizeInt; AwaitFullData: Boolean): SizeInt;
  end;

  { TIOReadTask }

  generic TIOReadTask<T> = class(specialize TRVTask<T>)
  private
    FReader: TIOReader;
    FOwnsReader: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; AOwnsReader: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOBufferReadTask }

  TIOBufferReadTask = class(specialize TRVTask<SizeInt>)
  private
    FReader: TIOReader;
    FBuffer: Pointer;
    FCount: SizeInt;
    FOwnsReader: Boolean;
    FAwaitFullData: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; ABuffer: Pointer; ACount: SizeInt;
                       AAwaitFullData: Boolean;
                       AOwnsReader: Boolean = True);
    destructor Destroy; override;
  end;                                                                           

  { TIOArrayReadTask }

  generic TIOArrayReadTask<T> = class(specialize TRVTask<specialize TArray<T>>)
  private
    FReader: TIOReader;
    FCount: SizeInt;
    FOwnsReader: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; ACount: SizeInt; AOwnsReader: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOStringReadTask }

  TIOStringReadTask  = class(specialize TRVTask<String>)
  private
    FReader: TIOReader;
    FCount: SizeInt;
    FPattern: String;
    FOwnsReader: Boolean;
    FAwaitFullData: Boolean;

    function ReadTo: String;
    function ReadCount: String;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; ACount: SizeInt;
                       AAwaitFullData: Boolean;
                       AOwnsReader: Boolean = True);
    constructor Create(AReader: TIOReader; APattern: String;
                       AMaxCount: SizeInt = 0; AAwaitFullData: Boolean = True;
                       AOwnsReader: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOWriteTask }

  generic TIOWriteTask<T> = class(TTask)
  private
    FWriter: TIOWriter;
    FOwnsWriter: Boolean;
    FData: T;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; const AData: T; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOBufferWriteTask }

  TIOBufferWriteTask = class(specialize TRVTask<SizeInt>)
  private
    FWriter: TIOWriter;
    FBuffer: Pointer;
    FCount: SizeInt;
    FOwnsWriter: Boolean;
    FAwaitFullData: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; ABuffer: Pointer; ACount: SizeInt;
      AAwaitFullData: Boolean; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOStringWriteTask }

  TIOStringWriteTask = class(specialize TRVTask<SizeInt>)
  private
    FWriter: TIOWriter;
    FData: String;
    FOwnsWriter: Boolean;
    FAwaitFullData: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; const AData: String;
      AAwaitFullData: Boolean; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOArrayWriteTask }

  generic TIOArrayWriteTask<T> = class(specialize TRVTask<SizeInt>)
  public type
    TDataArray = array of T;
  private
    FWriter: TIOWriter;
    FData: TDataArray;
    FOwnsWriter: Boolean;
    FAwaitFullData: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; const AData: TDataArray;
      AAwaitFullData: Boolean; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

  { TAsyncStream }

  TAsyncStream = class(TStream)
  private
    FReader: TIOReader;
    FWriter: TIOWriter;
    FExecutor: TExecutor;
  public
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;

    constructor Create(AReader: TIOReader; AWriter: TIOWriter; AExecutor: TExecutor);
  end;

implementation

{ TAsyncStream }

function TAsyncStream.Read(var Buffer; Count: Longint): Longint;
begin
  if not Assigned(FReader) then
    ReadNotImplemented;
  Result := FReader.AsyncRead(FExecutor, @Buffer, Count, False);
end;

function TAsyncStream.Write(const Buffer; Count: Longint): Longint;
begin
  if not Assigned(FWriter) then
    WriteNotImplemented;
  Result := FWriter.AsyncWrite(FExecutor, @Buffer, Count, False);
end;

constructor TAsyncStream.Create(AReader: TIOReader; AWriter: TIOWriter;
  AExecutor: TExecutor);
begin
  inherited Create;
  FReader := AReader;
  FWriter := AWriter;
  FExecutor := AExecutor;
end;

{ TIOStringWriteTask }

procedure TIOStringWriteTask.Execute;
begin
  FResult := FWriter.AsyncWrite(Executor, @FData[1], FData.Length, FAwaitFullData);
end;

constructor TIOStringWriteTask.Create(AWriter: TIOWriter; const AData: String; AAwaitFullData: Boolean;
  AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FData := AData;
  FAwaitFullData := AAwaitFullData;
end;

destructor TIOStringWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

{ TIOArrayWriteTask }

procedure TIOArrayWriteTask.Execute;
begin
  FResult := FWriter.AsyncWrite(Executor, @FData[0], Length(FData) * SizeOf(T), FAwaitFullData);
end;

constructor TIOArrayWriteTask.Create(AWriter: TIOWriter;
  const AData: TDataArray; AAwaitFullData: Boolean; AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FData := AData;
  FAwaitFullData := AAwaitFullData;
end;

destructor TIOArrayWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

{ TIOBufferWriteTask }

procedure TIOBufferWriteTask.Execute;
begin
  FResult := FWriter.AsyncWrite(Executor, FBuffer, FCount, FAwaitFullData);
end;

constructor TIOBufferWriteTask.Create(AWriter: TIOWriter; ABuffer: Pointer;
  ACount: SizeInt; AAwaitFullData: Boolean; AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FBuffer := ABuffer;
  FCount := ACount;
  FAwaitFullData := AAwaitFullData;
end;

destructor TIOBufferWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

{ TIOWriteTask }

procedure TIOWriteTask.Execute;
begin
  FWriter.AsyncWrite(Executor, @FData, SizeOf(T), True);
end;

constructor TIOWriteTask.Create(AWriter: TIOWriter; const AData: T;
  AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FData := AData;
end;

destructor TIOWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

constructor TIOReader.Create(AsyncSleepTime: Integer);
begin
  FAsyncSleepTime := AsyncSleepTime;
end;

function TIOReader.AsyncRead(AExecutor: TExecutor; ABuffer: Pointer;
  ACount: SizeInt; AwaitFullData: Boolean): SizeInt;
begin
  Result := 0;
  while (AwaitFullData and (Result < ACount)) or (Result = 0) do
  begin
    Result += TryRead(@PByte(ABuffer)[Result], ACount - Result);
    if (Result = 0) or (AwaitFullData and (Result < ACount)) then
      AExecutor.Sleep(FAsyncSleepTime);
  end;
end;

constructor TIOWriter.Create(AsyncSleepTime: Integer);
begin
  FAsyncSleepTime := AsyncSleepTime;
end;

function TIOWriter.AsyncWrite(AExecutor: TExecutor; ABuffer: Pointer;
  ACount: SizeInt; AwaitFullData: Boolean): SizeInt;
begin
  Result := 0;
  while (Result = 0) or (AwaitFullData and (Result < ACount)) do
  begin
    Result += TryWrite(@PByte(ABuffer)[Result], ACount - Result);
    if (Result = 0) or (AwaitFullData and (Result < ACount)) then
      AExecutor.Sleep(FAsyncSleepTime);
  end;
end;

{ TIOStringReadTask }

function TIOStringReadTask.ReadTo: String;
var
  c: Char;
  len: integer;
  pLen: integer;
  backtrack: integer;
begin
  Result := '';
  SetLength(Result, 128);
  len := 0;
  plen := 0;
  backtrack := 0;
  try
    while (FCount <= 0) or (len < FCount) do
    begin
      FReader.AsyncRead(Executor, @c, SizeOf(c), True);
      Result[len + 1] := c;
      Inc(len);
      if len = Result.Length then
        SetLength(Result, Result.Length * 2);
      if FPattern[pLen + 1] = c then
      begin
        if plen = 0 then
        begin
          backtrack := len;
        end;
        Inc(pLen);
        if pLen = FPattern.Length then
          Exit;
      end
      else if plen > 0 then
      begin
        pLen := 0;
        while backtrack + pLen < len do
        begin
          if FPattern[pLen + 1] = Result[backtrack + pLen + 1] then
            Inc(pLen)
          else
          begin
            pLen := 0;
            Inc(backtrack);
          end;
        end;
      end;
    end;
  finally
    SetLength(Result, len);
  end;
end;

function TIOStringReadTask.ReadCount: String;
var
  Size: SizeInt;
begin
  Result := '';
  SetLength(Result, FCount);
  Size := FReader.AsyncRead(Executor, @Result[1], FCount, FAwaitFullData);
  SetLength(Result, Size);
end;

procedure TIOStringReadTask.Execute;
begin
  if Length(FPattern) > 0 then
    FResult := ReadTo
  else
    FResult := ReadCount;
end;

destructor TIOStringReadTask.Destroy;
begin
  if FOwnsReader then
    FReader.Free;
  inherited Destroy;
end;

constructor TIOStringReadTask.Create(AReader: TIOReader; ACount: SizeInt;
  AAwaitFullData: Boolean; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FPattern := '';
  FCount := ACount;
  FOwnsReader := AOwnsReader;
  FAwaitFullData := AAwaitFullData;
end;

constructor TIOStringReadTask.Create(AReader: TIOReader; APattern: String;
  AMaxCount: SizeInt; AAwaitFullData: Boolean; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FPattern := APattern;
  FCount := AMaxCount;
  FOwnsReader := AOwnsReader;
  FAwaitFullData := AAwaitFullData;
end;

{ TIOArrayReadTask }

procedure TIOArrayReadTask.Execute;
begin
  SetLength(FResult, FCount);
  FReader.AsyncRead(Executor, PByte(@FResult[0]), FCount * SizeOf(T), True);
end;

constructor TIOArrayReadTask.Create(AReader: TIOReader; ACount: SizeInt;
  AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FCount := ACount;
  FOwnsReader := AOwnsReader;
end;

destructor TIOArrayReadTask.Destroy;
begin
  if FOwnsReader then
    FReader.Free;
  inherited Destroy;
end;

{ TIOBufferReadTask }

procedure TIOBufferReadTask.Execute;
begin
  FResult := FReader.AsyncRead(Executor, FBuffer, FCount, FAwaitFullData);
end;

constructor TIOBufferReadTask.Create(AReader: TIOReader; ABuffer: Pointer;
  ACount: SizeInt; AAwaitFullData: Boolean; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FBuffer := ABuffer;
  FCount := ACount;
  FOwnsReader := AOwnsReader;
  FAwaitFullData := AAwaitFullData;
end;

destructor TIOBufferReadTask.Destroy;
begin
  if FOwnsReader then
    FReader.Free;
  inherited Destroy;
end;

{ TIOReadTask }

procedure TIOReadTask.Execute;
begin
  FReader.AsyncRead(Executor, @FResult, SizeOf(FResult), True);
end;

constructor TIOReadTask.Create(AReader: TIOReader; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FOwnsReader := AOwnsReader;
end;

destructor TIOReadTask.Destroy;
begin
  if FOwnsReader then
    FReader.Free;
  inherited Destroy;
end;

end.

