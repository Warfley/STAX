unit stax.generators;

{$mode objfpc}{$H+}

interface

uses
  stax;

type
  { TGenerator }

  generic TGenerator<TResult> = class(TExecutable, specialize IGenerator<TResult>)
  public type
    IInterfaceType = specialize IGenerator<TResult>;
    TEnumerator = specialize TGeneratorEnumerator<TResult>;
  private
    FRefCount: LongInt;
    FHasResult: Boolean;
    FGeneratedValue: TResult;
    FGeneratedIndex: SizeInt;
  protected
     procedure FinalizeExecution; override;

     procedure Yield(const AResult: TResult); {$IFDEF inlining}inline;{$ENDIF}

  public
    function _AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function QueryInterface(constref iid: tguid; out obj): longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};

    constructor Create(AStackSize: SizeInt = DefaultTaskStackSize);

    procedure StartNextGeneration; {$IFDEF inlining}inline;{$ENDIF}
    function HasResult: Boolean; {$IFDEF inlining}inline;{$ENDIF}
    function GetIndex: SizeInt; {$IFDEF inlining}inline;{$ENDIF}
    function GetValue: TResult; {$IFDEF inlining}inline;{$ENDIF}

    function GetEnumerator: TEnumerator; {$IFDEF inlining}inline;{$ENDIF}

    property Index: SizeInt read FGeneratedIndex;
  end;

implementation

{ TGenerator }

procedure TGenerator.FinalizeExecution;
begin
  try
    inherited FinalizeExecution;
  finally
    // if no references exist we can free it
    if FRefCount = 0 then
      Free;
  end;
end;

procedure TGenerator.Yield(const AResult: TResult);
begin
  // When we generate a result, we store that result
  FGeneratedValue := AResult;
  // Set the status
  FHasResult := True;
  // Count the epoch
  Inc(FGeneratedIndex);
  // And notify waiting tasks
  ResolveDependencies;
  // Then we go to sleep until we are needed again
  Wait;
end;

function TGenerator._AddRef: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterlockedIncrement(FRefCount);
end;

function TGenerator._Release: longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  Result := InterlockedDecrement(FRefCount);
  if Result = 0 then
    // if havent scheduled, or are already finished, simply free
    if Status in [esNone, esFinished,esError] then
      Free
    else // otherwise let it run to termination and let the scheduler free it
      Terminate;
end;

function TGenerator.QueryInterface(constref iid: tguid; out obj): longint;
 {$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
begin
  if getinterface(iid,obj) then
    result:=S_OK
  else
    result:=longint(E_NOINTERFACE);
end;

constructor TGenerator.Create(AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FRefCount := 0;
  FHasResult := False;
  FGeneratedIndex := -1;
end;

procedure TGenerator.StartNextGeneration;
begin
  // if already scheduled or running do nothing
  if not (Status in [esNone, esWaiting]) then
    Exit;
  // If not assigned to an executor, do so now
  if not Assigned(Executor) then
    SetExecutor(GetExecutor);
  // Set as pending
  FHasResult := False;
  // Schedule
  if Status = esNone then
    Executor.ScheduleForExecution(Self)
  else
    Executor.Wakeup(Self);
end;

function TGenerator.HasResult: Boolean;
begin
  Result := FHasResult;
end;

function TGenerator.GetIndex: SizeInt;
begin
  Result := FGeneratedIndex;
end;

function TGenerator.GetValue: TResult;
begin
  Result := FGeneratedValue;
end;

function TGenerator.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(IInterfaceType(Self));
end;

end.

