unit stax.asyncconsole;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, stax, stax.asyncio, stax.internal.console;

type

  { TConsoleReader }

  TConsoleReader = class(TIOReader)
  private
    FHandle: THandle;
    FDirectRead: Boolean;
  public
    function TryRead(ABuffer: Pointer; ACount: SizeInt): SizeInt; override;

    constructor Create(ADirectRead: Boolean = False);
  end;

const
  ConsoleSleepTime = 50;

function AsyncConsoleRead(DirectRead: Boolean = False): specialize TRVTask<Char>; overload;
function AsyncConsoleRead(Count: SizeInt; DirectRead: Boolean = False; AwaitFullData: Boolean = True): specialize TRVTask<String>; overload;
function AsyncConsoleReadLn(DirectRead: Boolean = False): specialize TRVTask<String>; overload;
function AsyncConsoleReadLn(MaxLength: SizeInt;DirectRead: Boolean = False): specialize TRVTask<String>; overload;

implementation

function AsyncConsoleRead(DirectRead: Boolean = False): specialize TRVTask<Char>;
begin
  Result := (specialize TIOReadTask<Char>).Create(TConsoleReader.Create(DirectRead));
end;

function AsyncConsoleRead(Count: SizeInt; DirectRead: Boolean = False; AwaitFullData: Boolean = True): specialize TRVTask<String>;
begin
  Result := TIOStringReadTask.Create(TConsoleReader.Create(DirectRead), Count, AwaitFullData);
end;

function AsyncConsoleReadLn(DirectRead: Boolean = False): specialize TRVTask<String>;
var
  ending: String;
begin
  if DirectRead then
    ending := #13
  else
    ending := LineEnding;
  Result := TIOStringReadTask.Create(TConsoleReader.Create(DirectRead), ending);
end;

function AsyncConsoleReadLn(MaxLength: SizeInt; DirectRead: Boolean = False): specialize TRVTask<String>;
var
  ending: String;
begin
  if DirectRead then
    ending := #13
  else
    ending := LineEnding;
  Result := TIOStringReadTask.Create(TConsoleReader.Create(DirectRead), ending, MaxLength);
end;

{ TConsoleReader }

function TConsoleReader.TryRead(ABuffer: Pointer; ACount: SizeInt): SizeInt;
var
  success: LongInt;
  oldState: TDirectReadState;
  c: Char;
begin
  Result := 0;
  {$IfDef Windows}
  if not FDirectRead then
    raise Exception.Create('Non blocking read without direct read not possible on windows');
  {$EndIf}
  if FDirectRead then
    oldState := EnableDirectRead(FHandle);
  try
    while (Result < ACount) and CharAvailable(FHandle) do
    begin
      success := FileRead(FHandle, c, sizeof(c));
      if Success <> 1 then
        raise EReadError.Create('Error reading from stdin');
      PChar(ABuffer)[Result] := c;
      Inc(Result);
    end;
  finally
    if FDirectRead then
      RestoreDirectRead(FHandle, oldState);
  end;
end;

constructor TConsoleReader.Create(ADirectRead: Boolean);
begin
  inherited Create(ConsoleSleepTime);
  FDirectRead:=ADirectRead;
  FHandle := StdInputHandle;
end;


end.

