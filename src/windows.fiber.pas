unit windows.fiber;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, windows;

type
  EFiberError = class(Exception);
  TFiber = LPVOID;

// see WinBase.h:316
  PFIBER_START_ROUTINE = procedure (lpFiberParameter: LPVOID); stdcall;
  LPFIBER_START_ROUTINE = PFIBER_START_ROUTINE;

// See WinBase.h:1461
procedure SwitchToFiber(lpFiber: TFiber); stdcall; external 'kernel32' name 'SwitchToFiber';
procedure DeleteFiber(lpFiber: TFiber); stdcall; external 'kernel32' name 'DeleteFiber';
function ConvertFiberToThread: BOOL; stdcall; external 'kernel32' name 'ConvertFiberToThread';
function CreateFiber(dwStackSize: SIZE_T; lpStartAddress: LPFIBER_START_ROUTINE; lpParameter: LPVOID): TFiber; stdcall; external 'kernel32' name 'CreateFiber';
function ConvertThreadToFiber(lpParameter: LPVOID): TFiber; stdcall; external 'kernel32' name 'ConvertThreadToFiber';

function LastFiberError: DWORD; inline;
implementation

function LastFiberError: DWORD;
begin
  Result := GetLastError;
end;

end.

