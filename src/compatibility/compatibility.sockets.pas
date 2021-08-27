unit compatibility.sockets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sockets, WinSock2;

type
  TSocket = WinSock2.TSocket;

const EAGAIN = EsockEWOULDBLOCK;

function SocketSelectWrite(ASocket: Tsocket): Boolean;
function SetNonBlocking(ASocket: Tsocket): Integer;
procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);

implementation
uses stax.tasks.io.tcp;

function SocketSelectWrite(ASocket: Tsocket): Boolean;
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
    if success = 1 then
      Result := True
    else if success = 0 then
      Result := False
    else
      raise ESocketError.Create('Socket error ' + socketerror.ToString);
  until success = 1;
end;

function SetNonBlocking(ASocket: Tsocket): Integer;
var
  nonblock: u_long;
begin
  nonblock := 1;
  ioctlsocket(ASocket, FIONBIO, @nonblock);
  Result := 0;
end;

procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);
var
  nonblock: u_long;
begin
  nonblock := OldState;
  ioctlsocket(ASocket, FIONBIO, @nonblock);
end;

end.

