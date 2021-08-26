unit Compatibility.Console;

{$mode objfpc}{$H+}

interface

uses
  SysUtils,
  {$IfDef UNIX}BaseUnix, termio
  {$Else}Windows
  {$Endif};

type
  TDirectReadState = {$IfDef UNIX}Termios{$Else}Cardinal{$EndIf};

procedure GetWindowSize(Handle: THandle; out Rows: Integer; out Columns: Integer);
function isATTY(Handle: THandle): Boolean; inline;
function OpenTerminalFile(const FileName: String; OpenWrite: Boolean): THandle;

function StandardIn: THANDLE; inline;
function StandardOut: THANDLE; inline;
function StandardErr: THANDLE; inline;

function InitOutputConsole(Handle: THANDLE): Cardinal;
function InitInputConsole(Handle: THANDLE): Cardinal;

procedure ResetConsole(Handle: THANDLE; OrigState: Cardinal);

function EnableDirectRead(Handle: THandle): TDirectReadState;
procedure RestoreDirectRead(Handle: THandle; oldState: TDirectReadState);

function CharAvailable(Handle: THandle): Boolean;

implementation

procedure GetWindowSize(Handle: THandle; out Rows: Integer; out
   Columns: Integer);
{$IfDef Unix}
var
  sz: TWinSize;
begin
  FpIOCtl(Handle, TIOCGWINSZ, @sz);
  Rows := sz.ws_row;
  Columns := sz.ws_col;
end;
{$else}
var
  csbi: CONSOLE_SCREEN_BUFFER_INFO;
begin
  FillChar(csbi, SizeOf(csbi), 0);
  GetConsoleScreenBufferInfo(Handle, csbi);
  Columns := csbi.srWindow.Right - csbi.srWindow.Left + 1;
  Rows := csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
end;
{$EndIf}

function isATTY(Handle: THandle): Boolean;
{$IfDef Windows}
var
  dummy: TByHandleFileInformation;
{$EndIf}
begin
 {$IfDef UNIX}
 Result := termio.IsATTY(Handle) <> 0;
 {$Else}
 // dirty hack but seems to work
 Result := not GetFileInformationByHandle(Handle, dummy);
 {$EndIf}
end;

function OpenTerminalFile(const FileName: String; OpenWrite: Boolean): THandle;
begin
 if OpenWrite then
 begin
  {$IfDef UNIX}
  Result := FpOpen(FileName, O_WRONLY);
  {$Else}
  Result := SysUtils.FileOpen(FileName, fmOpenRead);
  {$EndIf}
 end
 else
 begin
  {$IfDef UNIX}
  Result := FpOpen(FileName, O_RDONLY);
  {$Else}
  Result := SysUtils.FileOpen(FileName, fmOpenRead);
  {$EndIf}
 end;
end;

function StandardIn: THANDLE;
begin
 {$IfDef WINDOWS}
 Result := GetStdHandle(STD_INPUT_HANDLE);
 {$Else}
 Result := StdInputHandle;
 {$EndIf}
end;

function StandardOut: THANDLE;
begin
 {$IfDef WINDOWS}
 Result := GetStdHandle(STD_OUTPUT_HANDLE);
 {$Else}
 Result := StdOutputHandle;
 {$EndIf}
end;

function StandardErr: THANDLE;
begin
 {$IfDef WINDOWS}
 Result := GetStdHandle(STD_ERROR_HANDLE);
 {$Else}
 Result := StdErrorHandle;
 {$EndIf}
end;

function InitOutputConsole(Handle: THANDLE): Cardinal;
begin
 Result := 0;
{$IfDef WINDOWS}
  GetConsoleMode(Handle, Result);
  SetConsoleMode(Handle, Result Or ENABLE_VIRTUAL_TERMINAL_PROCESSING);
{$EndIf}
end;

function InitInputConsole(Handle: THANDLE): Cardinal;
begin
 Result := 0;
{$IfDef WINDOWS}
  GetConsoleMode(Handle, Result);
  SetConsoleMode(Handle, Result Or ENABLE_VIRTUAL_TERMINAL_INPUT);
{$EndIf}
end;

procedure ResetConsole(Handle: THANDLE; OrigState: Cardinal);
begin
{$IfDef WINDOWS}
  SetConsoleMode(Handle, OrigState);
{$EndIf}
end;

function EnableDirectRead(Handle: THandle): TDirectReadState;
{$IfDef WINDOWS}
begin
  GetConsoleMode(Handle, Result);
  SetConsoleMode(Handle, Result And Not (ENABLE_ECHO_INPUT Or ENABLE_LINE_INPUT or ENABLE_PROCESSED_INPUT));
end;
{$Else}
var
  nTIO: Termios;
  i: SizeInt;
begin
  TCGetAttr(Handle, Result);
  nTIO := Result;
  // Raw to not echo the characters inputted
  CFMakeRaw(nTIO);
  // Directly read and don't line buffer
  TCSetAttr(Handle, TCSANOW, nTIO);
end;
{$EndIf}

procedure RestoreDirectRead(Handle: THandle; oldState: TDirectReadState);
{$IfDef WINDOWS}
begin
  SetConsoleMode(Handle, oldState);
end;
{$Else}
begin
  TCSetAttr(Handle, TCSANOW, oldState);
end;
{$EndIf}


function CharAvailable(Handle: THandle): Boolean;
{$IfDef WINDOWS}
var
  Num, read: Cardinal;
  inps: array of TINPUTRECORD;
  i: Integer;
begin
 Result := False;
 if not GetNumberOfConsoleInputEvents(Handle, num) then
   Exit(True);
 if Num = 0 then Exit;
 SetLength(inps, num);
 PeekConsoleInput(Handle, @inps[0], num, @read);
 for i := 0 to read - 1 do
   if (inps[i].EventType = KEY_EVENT) and (inps[i].Event.KeyEvent.bKeyDown) then
     Exit(True)
end;
{$Else}
var
  fdSet: TFDSet;
  timeout: TTimeVal;
begin
  fpFD_ZERO(fdSet);
  fpFD_SET(Handle, fdSet);
  timeout.tv_sec:=0;
  timeout.tv_usec:=0;
  Result := fpSelect(Handle + 1, @fdSet, nil, nil, @timeout) = 1;
end;
{$EndIf}


end.

