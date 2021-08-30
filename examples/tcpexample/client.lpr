program client;

{$mode objfpc}{$H+}

uses
  stax, stax.functional, stax.asynctcp, stax.asyncconsole;

procedure SendUserInput(AExecutor: TExecutor; ASocket: Tsocket);
var
  c: Char;
begin
  while True do
  begin
    c := specialize Await<Char>(AsyncConsoleRead(True));
    if c = #13 then c := #10;
    Await(specialize AsyncSend<Char>(ASocket, c));
  end;
end;

procedure HandleReceive(AExecutor: TExecutor; ASocket: Tsocket);
var
  c: Char;
begin
  while True do
  begin
    c := specialize Await<Char>(specialize AsyncReceive<Char>(ASocket));
    Write(c);
  end;
end;

procedure ConnectToServer(AExecutor: TExecutor; AHost: string; APort: Integer);
var
  Sock: Tsocket;
begin
  Sock := TCPSocket;
  Await(AsyncConnect(Sock, AHost, APort));
  AExecutor.RunAsync(specialize AsyncProcedure<TSocket>(@SendUserInput, Sock));
  AExecutor.RunAsync(specialize AsyncProcedure<TSocket>(@HandleReceive, Sock));
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(specialize AsyncProcedure<String, Integer>(@ConnectToServer, '127.0.0.1', 1337));
  try
    exec.Run;
  except on E: EUnhandledError do
    WriteLn('Unhandled Error: ', E.Error.Message);
  end;
  exec.Free;
  ReadLn;
end.

