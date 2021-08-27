program server;

{$mode objfpc}{$H+}

uses
  stax, stax.tasks.io.tcp, stax.tasks.functional;

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
  Conn: TSocket;
begin
  Sock := TCPServerSocket(AHost, APort);
  TCPServerListen(Sock, 10);
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

