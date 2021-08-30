unit stax.tasks.io.tcp;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, stax, stax.tasks.io, Sockets, Compatibility.sockets;

type
  TSocket = Compatibility.sockets.TSocket;

  ESocketError = class(Exception);
  EConnectionClosedException = class(Exception);

  { TAcceptTask }

  TAcceptTask = class(specialize TRVTask<TSocket>)
  private
    FServerSocket: TSocket;
  protected
    procedure Execute; override;
  public
    constructor Create(AServerSocket: Tsocket);
  end;

  { TConnectTask }

  TConnectTask = class(TTask)
  private
    FSocket: Tsocket;
    FHost: String;
    FPort: Integer;

  protected
    procedure Execute; override;
    constructor Create(ASocket: Tsocket; const AHost: String; APort: Integer);
  end;

  { TNonBlockingTCPReceiver }

  TNonBlockingTCPReceiver = class(TIOReader)
  private
    FSocket: TSocket;
  protected
    function TryRead(ABuffer: Pointer; ACount: SizeInt): SizeInt; override;
  public
    constructor Create(ASocket: TSocket);
  end;

  { TNonBlockingTCPSender }

  TNonBlockingTCPSender = class(TIOWriter)
  private
    FSocket: TSocket;
  protected
    function TryWrite(ABuffer: Pointer; ACount: SizeInt): SizeInt; override;
  public
    constructor Create(ASocket: TSocket);
  end;


const
  SocketSleepingTime = 10;

function AsyncAccept(AServerSocket: TSocket): specialize TRVTask<TSocket>; inline;
function AsyncConnect(ASocket: TSocket; const AHost: string; APort: Integer): TTask; inline;

function AsyncReceive(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt):  TTask; overload; inline;
generic function AsyncReceive<T>(ASocket: Tsocket): specialize TRVTask<T>; overload; inline;
generic function AsyncReceive<T>(ASocket: Tsocket; ACount: SizeInt): specialize TRVTask<specialize TArray<T>>; overload; inline;
function AsyncReceiveLn(ASocket: Tsocket): specialize TRVTask<String>; inline;
function AsyncReceiveStr(ASocket: Tsocket; ALength: SizeInt): specialize TRVTask<String>; inline;

function AsyncSend(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt): TTask; overload; inline;
generic function AsyncSend<T>(ASocket: Tsocket; const AData: T): TTask; overload; inline;
function AsyncSendLn(ASocket: Tsocket; const AData: String): TTask; inline;
function AsyncSendStr(ASocket: Tsocket; const AData: String): TTask; inline;

function TCPSocket: TSocket; inline;
function TCPServerSocket(AHost: String; APort: Integer): TSocket;
procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer); inline;
procedure TCPSocketClose(ASocket: TSocket); inline;
implementation

// Helper

procedure WaitForHandshake(ATask: TTask; ASocket: Tsocket); inline;
var
  success: LongInt;
  fs: TFDSet;
  timeout: TTimeVal;
begin
  FD_ZERO(fs);
  FD_SET(ASocket, fs);
  timeout.tv_sec:=0;
  timeout.tv_usec:=0;
  repeat
    success := select(ASocket + 1, Nil, @fs, nil, @timeout);
    if success = 0 then
      ATask.Sleep(SocketSleepingTime)
    else if success < 0 then
      raise ESocketError.Create('Socket error ' + socketerror.ToString);
  until success = 1;
end;

function AsyncAccept(AServerSocket: TSocket): specialize TRVTask<TSocket>;
begin
  Result := TAcceptTask.Create(AServerSocket);
end;

function AsyncConnect(ASocket: TSocket; const AHost: string;
  APort: Integer): TTask;
begin
  Result := TConnectTask.Create(ASocket, AHost, APort);
end;

function AsyncReceive(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt):  TTask;
begin
  Result := TIOBufferReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), ABuffer, ACount);
end;

generic function AsyncReceive<T>(ASocket: Tsocket): specialize TRVTask<T>;
begin
  Result := specialize TIOReadTask<T>.Create(TNonBlockingTCPReceiver.Create(ASocket));
end;

generic function AsyncReceive<T>(ASocket: Tsocket; ACount: SizeInt): specialize TRVTask<specialize TArray<T>>;
begin
  Result := specialize TIOArrayReadTask<T>.Create(TNonBlockingTCPReceiver.Create(ASocket), ACount);
end;

function AsyncReceiveLn(ASocket: Tsocket): specialize TRVTask<String>;
begin
  Result := TIOStringReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), #10);
end;

function AsyncReceiveStr(ASocket: Tsocket; ALength: SizeInt): specialize TRVTask<String>;
begin
  Result := TIOStringReadTask.Create(TNonBlockingTCPReceiver.Create(ASocket), ALength);
end;

function AsyncSend(ASocket: Tsocket; ABuffer: Pointer; ACount: SizeInt
  ): TTask;
begin
  Result := TIOBufferWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), ABuffer, ACount);
end;

generic function AsyncSend<T>(ASocket: Tsocket; const AData: T): TTask; overload; inline;
begin
  Result := specialize TIOWriteTask<T>.Create(TNonBlockingTCPSender.Create(ASocket), AData);
end;

function AsyncSendLn(ASocket: Tsocket; const AData: String): TTask;
begin
  Result := TIOStringWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), AData.Trim + #10);
end;

function AsyncSendStr(ASocket: Tsocket; const AData: String): TTask;
begin
  Result := TIOStringWriteTask.Create(TNonBlockingTCPSender.Create(ASocket), AData);
end;

function TCPSocket: TSocket;
begin
  Result := fpsocket(AF_INET, SOCK_STREAM, 0);
  if SocketInvalid(Result) then
    raise ESocketError.Create('Error creating socket ' + socketerror.ToString);
end;

function TCPServerSocket(AHost: String; APort: Integer): TSocket;
var
  addr: TSockAddr;
begin
  Result := TCPSocket;
  addr.sin_family := AF_INET;
  addr.sin_port := ShortHostToNet(APort);
  addr.sin_addr.s_addr := LongWord(StrToNetAddr(AHost));
  if fpbind(Result, @addr, SizeOf(addr)) <> 0 then raise
    ESocketError.Create('Error binding to socket to ' + AHost + ':' + APort.ToString + ' ' + socketerror.ToString);
end;

procedure TCPServerListen(AServerSocket: TSocket; Backlog: Integer);
begin
  if fplisten(AServerSocket, Backlog) <> 0 then raise
    ESocketError.Create('Error starting listening ' + socketerror.ToString);
end;

procedure TCPSocketClose(ASocket: TSocket);
begin
  Close(ASocket);
end;

{ TNonBlockingTCPSender }

function TNonBlockingTCPSender.TryWrite(ABuffer: Pointer; ACount: SizeInt
  ): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(FSocket);
  try
    Result := fpsend(FSocket, ABuffer, ACount, 0);
    if Result = 0 then
      raise EConnectionClosedException.Create('The connection closed')
    else if Result < 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        Result := 0
      else
        raise ESocketError.Create('Socket error ' + err.ToString);
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TNonBlockingTCPSender.Create(ASocket: TSocket);
begin
  inherited Create(SocketSleepingTime);
  FSocket := ASocket;
end;

{ TNonBlockingTCPReceiver }

function TNonBlockingTCPReceiver.TryRead(ABuffer: Pointer; ACount: SizeInt
  ): SizeInt;
var
  OldState, err: LongInt;
begin
  OldState := SetNonBlocking(FSocket);
  try
    Result := fprecv(FSocket, ABuffer, ACount, 0);
    if Result = 0 then
      raise EConnectionClosedException.Create('The connection closed')
    else if Result < 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        Result := 0
      else
        raise ESocketError.Create('Socket error ' + err.ToString);
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TNonBlockingTCPReceiver.Create(ASocket: TSocket);
begin
  inherited Create(SocketSleepingTime);
  FSocket := ASocket;
end;

{ TConnectTask }

procedure TConnectTask.Execute;
var
  OldState, err, success: LongInt;
  addr: TSockAddr;
begin
  OldState := SetNonBlocking(FSocket);
  try
    addr.sin_family := AF_INET;
    addr.sin_port := ShortHostToNet(FPort);
    addr.sin_addr.s_addr := LongWord(StrToNetAddr(FHost));
    success := fpconnect(FSocket, @addr, SizeOf(addr));
    if success <> 0 then
    begin
      err := socketerror;
      if WasBlockingError(err) then
        WaitForHandshake(self, FSocket)
      else
        raise ESocketError.Create('Socket error ' + err.ToString);
    end;
  finally
    RestoreBlocking(FSocket, OldState);
  end;
end;

constructor TConnectTask.Create(ASocket: Tsocket; const AHost: String;
  APort: Integer);
begin
  inherited Create;
  FSocket := ASocket;
  FHost := AHost;
  FPort := APort;
end;

{ TAcceptTask }

procedure TAcceptTask.Execute;
var
  OldState, blocking, err: LongInt;
  Conn: TSocket;
begin
  OldState := SetNonBlocking(FServerSocket);
  try
    repeat
      Conn := fpaccept(FServerSocket, nil, nil);
      if SocketInvalid(Conn) then
      begin
        err := socketerror;
        if WasBlockingError(err) then
          Sleep(SocketSleepingTime)
        else
          raise ESocketError.Create('Socket error ' + err.ToString);
      end;
    until not SocketInvalid(Conn);
    WaitForHandshake(Self, Conn);
  finally
    RestoreBlocking(FServerSocket, OldState);
  end;
  FResult := Conn;
end;

constructor TAcceptTask.Create(AServerSocket: Tsocket);
begin
  inherited Create;
  FServerSocket := AServerSocket;
end;

end.

