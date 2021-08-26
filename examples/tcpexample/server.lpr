program server;

{$mode objfpc}{$H+}

uses
  Sockets, stax, stax.tasks.io.tcp, stax.tasks.functional;

// simple tcp echo server
procedure HandleConnection(AExecutor: TExecutor; AConnection: TSocket);
var
  c: Char;
begin
  while True do
  begin
    c := specialize Await<Char>(specialize AsyncReceive<Char>(AConnection));
    Write(c);
    AExecutor.RunAsync(specialize AsyncSend<Char>(AConnection, c));
  end;
end;

procedure RunServer(AExecutor: TExecutor; AHost: string; APort: Integer);
var
  Sock: Tsocket;
  addr: TSockAddr;
  Conn: TSocket;
begin
  Sock := fpsocket(AF_INET, SOCK_STREAM, 0);
  addr.sin_family := AF_INET;
  addr.sin_port := ShortHostToNet(APort);
  addr.sin_addr.s_addr := LongWord(StrToNetAddr(AHost));
  if fpbind(Sock, @addr, SizeOf(addr)) <> 0 then raise
    ESocketError.Create('Error binding to socket');
  if fplisten(Sock, 10) <> 0 then raise
    ESocketError.Create('Error binding to socket');
  while True do
  begin
    Conn := specialize Await<TSocket>(AsyncAccept(Sock));
    AExecutor.RunAsync(specialize AsyncProcedure<Tsocket>(@HandleConnection, Conn));
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(specialize AsyncProcedure<String, Integer>(@RunServer, '0.0.0.0', 1337));
  try
    exec.Run;
  except on E: EUnhandledError do
    WriteLn('Unhandled error: ', E.Message);
  end;
  exec.Free;
  ReadLn;
end.

