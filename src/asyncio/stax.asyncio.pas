unit stax.asyncio;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, stax;

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
    procedure AsyncRead(AExecutor: TExecutor; ABuffer: Pointer; ACount: SizeInt);
  end;

  { TIOWriter }

  TIOWriter = class
  private
    FAsyncSleepTime: Integer;
  public
    constructor Create(AsyncSleepTime: Integer);

    function TryWrite(ABuffer: Pointer; ACount: SizeInt): SizeInt; virtual; abstract;
    procedure AsyncWrite(AExecutor: TExecutor; ABuffer: Pointer; ACount: SizeInt);
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

  TIOBufferReadTask = class(TTask)
  private
    FReader: TIOReader;
    FBuffer: Pointer;
    FCount: SizeInt;
    FOwnsReader: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; ABuffer: Pointer; ACount: SizeInt; AOwnsReader: Boolean = True);
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

    function ReadTo: String;
    function ReadCount: String;
  protected
    procedure Execute; override;
  public
    constructor Create(AReader: TIOReader; ACount: SizeInt; AOwnsReader: Boolean = True);
    constructor Create(AReader: TIOReader; APattern: String; AMaxCount: SizeInt = 0; AOwnsReader: Boolean = True);
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

  TIOBufferWriteTask = class(TTask)
  private
    FWriter: TIOWriter;
    FBuffer: Pointer;
    FCount: SizeInt;
    FOwnsWriter: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; ABuffer: Pointer; ACount: SizeInt; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

  { TIOStringWriteTask }

  TIOStringWriteTask = class(TTask)
  private
    FWriter: TIOWriter;
    FData: String;
    FOwnsWriter: Boolean;
  protected
    procedure Execute; override;
  public
    constructor Create(AWriter: TIOWriter; const AData: String; AOwnsWriter: Boolean = True);
    destructor Destroy; override;
  end;

implementation

{ TIOStringWriteTask }

procedure TIOStringWriteTask.Execute;
begin
  FWriter.AsyncWrite(Executor, @FData[1], FData.Length);
end;

constructor TIOStringWriteTask.Create(AWriter: TIOWriter; const AData: String;
  AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FData := AData;
end;

destructor TIOStringWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

{ TIOBufferWriteTask }

procedure TIOBufferWriteTask.Execute;
begin
  FWriter.AsyncWrite(Executor, FBuffer, FCount);
end;

constructor TIOBufferWriteTask.Create(AWriter: TIOWriter; ABuffer: Pointer;
  ACount: SizeInt; AOwnsWriter: Boolean);
begin
  inherited Create;
  FWriter := AWriter;
  FOwnsWriter := AOwnsWriter;
  FBuffer := ABuffer;
  FCount := ACount;
end;

destructor TIOBufferWriteTask.Destroy;
begin
  if FOwnsWriter then FWriter.Free;
  inherited Destroy;
end;

{ TIOWriteTask }

procedure TIOWriteTask.Execute;
begin
  FWriter.AsyncWrite(Executor, @FData, SizeOf(T));
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

procedure TIOReader.AsyncRead(AExecutor: TExecutor; ABuffer: Pointer;
  ACount: SizeInt);
var
  BytesRead: SizeInt;
begin
  BytesRead := 0;
  while BytesRead < ACount do
  begin
    BytesRead += TryRead(@PByte(ABuffer)[BytesRead], ACount - BytesRead);
    if BytesRead < ACount then
      AExecutor.Sleep(FAsyncSleepTime);
  end;
end;

constructor TIOWriter.Create(AsyncSleepTime: Integer);
begin
  FAsyncSleepTime := AsyncSleepTime;
end;

procedure TIOWriter.AsyncWrite(AExecutor: TExecutor; ABuffer: Pointer;
  ACount: SizeInt);
var
  BytesWritten: SizeInt;
begin
  BytesWritten := 0;
  while BytesWritten < ACount do
  begin
    BytesWritten += TryWrite(@PByte(ABuffer)[BytesWritten], ACount - BytesWritten);
    if BytesWritten < ACount then
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
  Success: SizeInt;
begin
  Result := '';
  SetLength(Result, 128);
  len := 0;
  plen := 0;
  backtrack := 0;
  try
    while (FCount <= 0) or (len < FCount) do
    begin
      FReader.AsyncRead(Executor, @c, SizeOf(c));
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
begin
  SetLength(Result, FCount);
  FReader.AsyncRead(Executor, @FResult[1], FCount);
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
  AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FPattern := '';
  FCount := ACount;
  FOwnsReader := AOwnsReader;
end;

constructor TIOStringReadTask.Create(AReader: TIOReader; APattern: String;
  AMaxCount: SizeInt; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FPattern := APattern;
  FCount := AMaxCount;
  FOwnsReader := AOwnsReader;
end;

{ TIOArrayReadTask }

procedure TIOArrayReadTask.Execute;
begin
  SetLength(FResult, FCount);
  FReader.AsyncRead(Executor, PByte(@FResult[0]), FCount * SizeOf(T));
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
  FReader.AsyncRead(Executor, FBuffer, FCount);
end;

constructor TIOBufferReadTask.Create(AReader: TIOReader; ABuffer: Pointer;
  ACount: SizeInt; AOwnsReader: Boolean);
begin
  inherited Create;
  FReader := AReader;
  FBuffer := ABuffer;
  FCount := ACount;
  FOwnsReader := AOwnsReader;
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
  FReader.AsyncRead(Executor, @FResult, SizeOf(FResult));
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

