unit compatibility.sockets;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Sockets {$IfDef Windows}, WinSock2{$Else}, BaseUnix{$EndIf};

type
  TSocket = {$IfDef Windows}WinSock2.TSocket{$Else}Sockets.Tsocket{$EndIf};
  TFDSet = {$IfDef Windows}WinSock2.TFDSet{$Else}BaseUnix.TFDSet{$EndIf};
  TTimeVal = {$IfDef Windows}WinSock2.TTimeVal{$Else}BaseUnix.TTimeVal{$EndIf};

function SetNonBlocking(ASocket: Tsocket): Integer;
procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);

function SocketInvalid(ASocket: TSocket): Boolean; inline;

procedure Close(ASocket: TSocket);

procedure FD_CLR(ASocket: TSocket; var FDSet: TFDSet); inline;
function FD_ISSET(ASocket: TSocket; var FDSet: TFDSet): Boolean; inline;
procedure FD_SET(ASocket: TSocket; var FDSet: TFDSet); inline;
procedure FD_ZERO(var FDSet: TFDSet); inline;
function select(nfds: Integer; ReadFDs,  WriteFDs, ExceptFDs: PFDSet; Timeout: PTimeVal): Integer; inline;

function WasBlockingError(AError: Integer): Boolean; inline;

implementation

procedure Close(ASocket: TSocket);
begin
  {$IfDef Windows}
  WinSock2.closesocket(ASocket);
  {$Else}
  fpClose(ASocket);
  {$EndIf}
end;

procedure FD_CLR(ASocket: TSocket; var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_CLR(ASocket, FDSet);
  {$Else}
  fpFD_CLR(ASocket, FDSet);
  {$EndIf}
end;

function FD_ISSET(ASocket: TSocket; var FDSet: TFDSet): Boolean;
begin
  {$IfDef Windows}
  Result := WinSock2.FD_ISSET(ASocket, FDSet);
  {$Else}
  Result := fpFD_ISSET(ASocket, FDSet) <> 0;
  {$EndIf}
end;

procedure FD_SET(ASocket: TSocket; var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_SET(ASocket, FDSet);
  {$Else}
  fpFD_SET(ASocket, FDSet);
  {$EndIf}
end;

procedure FD_ZERO(var FDSet: TFDSet);
begin
  {$IfDef Windows}
  WinSock2.FD_ZERO(FDSet);
  {$Else}
  fpFD_ZERO(FDSet);
  {$EndIf}
end;

function select(nfds: Integer; ReadFDs,  WriteFDs, ExceptFDs: PFDSet; Timeout: PTimeVal): Integer;
begin
  {$IfDef Windows}
  Result := WinSock2.select(nfds, ReadFDs, WriteFds, ExceptFDs, Timeout);
  {$Else}
  Result := fpSelect(nfds, ReadFDs, WriteFds, ExceptFDs, Timeout);
  {$EndIf}
end;

function WasBlockingError(AError: Integer): Boolean;
begin
  {$IfDef Windows}
  Result := AError = EsockEWOULDBLOCK;
  {$Else}
  Result := (AError = EsockEWOULDBLOCK) or (AError = ESysEAGAIN) or (AError = ESysEINPROGRESS);
  {$EndIf}
end;

function SetNonBlocking(ASocket: Tsocket): Integer;
{$IfDef Windows}
var
  nonblock: u_long;
begin
  nonblock := 1;
  ioctlsocket(ASocket, LongInt(FIONBIO), @nonblock);
  Result := 0;
end;
{$Else}
begin
  Result := FpFcntl(ASocket, F_GetFl);
  FpFcntl(ASocket, F_SetFL, Result Or O_NONBLOCK);
end;
{$EndIf}

procedure RestoreBlocking(ASocket: Tsocket; OldState: Integer);
{$IfDef Windows}
var
  nonblock: u_long;
begin
  nonblock := OldState;
  ioctlsocket(ASocket, LongInt(FIONBIO), @nonblock);
end;
{$Else}
begin
  FpFcntl(ASocket, F_SetFL, OldState);
end;
{$EndIf}

function SocketInvalid(ASocket: TSocket): Boolean;
begin
  {$IfDef Windows}
  Result := ASocket = TSocket(INVALID_SOCKET);
  {$Else}
  Result := ASocket < 0;
  {$EndIf}
end;

end.

