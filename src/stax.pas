unit stax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, stax.helpertypes, barrier;

type
  TExecutor = class;

  ETaskTerminatedException = class(Exception);
  ETaskNotActiveException = class(Exception);

  TTaskStatus = (tsNone=0, tsScheduled, tsRescheduled, tsRunning, tsFinished, tsError);

  { TTask }

  TTask = class
  // Maybe this is too much?
  public const DefaultTaskStackSize = DefaultStackSize;
  private type
    TStackData = record
      Size: SizeInt;
      Memory: Pointer;
    end;
    TStackCache = specialize TList<TStackData>;
  public
    class var StackCache: TStackCache;
    class constructor InitStatics;
    class destructor CleanupStatics;
    class function AllocStack(ASize: SizeInt): TStackData; static; {$IFDEF inlining}inline;{$ENDIF}
    class procedure FreeStack(constref AStackData: TStackData); static; {$IFDEF inlining}inline;{$ENDIF}
  private
    FError: Exception;
    FStatus: TTaskStatus;
    FExecutor: TExecutor;
    FStack: TStackData;
    FTaskEnv: jmp_buf;
    FTerminated: Boolean;
  protected
    procedure Execute; virtual; abstract;
  public
    constructor Create(AStackSize: SizeInt = DefaultTaskStackSize);
    destructor Destroy; override;

    procedure Terminate; {$IFDEF inlining}inline;{$ENDIF}

    procedure Run(AExecutor: TExecutor);
    procedure Resume;
    procedure Yield;
    procedure Sleep(Time: QWord);
    procedure Stop; {$IFDEF inlining}inline;{$ENDIF}

    property Error: Exception read FError;
    property Status: TTaskStatus read FStatus;
    property Executor: TExecutor read FExecutor;
    property Terminated: Boolean read FTerminated;
  end;

  { TRVTask }

  generic TRVTask<T> = class(TTask)
  protected
    FResult: T;
  protected
    procedure Execute; override;
  public
    property TaskResult: T read FResult;
  end;

  TErrorFunction = procedure(Task: TTask; E: Exception);
  TErrorMethod = procedure(Task: TTask; E: Exception) of object;
  TErrorHandler = specialize TUnion<TErrorFunction, TErrorMethod>;

  { EUnhandledError }

  EUnhandledError = class(Exception)
  public
    Error: Exception;
    Task: TTask;

    constructor Create(AError: Exception; ATask: TTask);
  end;

  TTaskQueueEntry = record
    Task: TTask;
    FreeTask: Boolean;
    RaiseError: Boolean;
  end;

  TTaskQueue = specialize TQueue<TTaskQueueEntry>;

  ETaskAlreadyScheduledException = class(Exception);
  ENotATaskException = class(Exception);

  { TExecutor }

  TExecutor = class
  private type
    TThreadExecutorMap = specialize TDictionary<TThreadID, TExecutor>;
  private
    class var ThreadExecutorMap: TThreadExecutorMap;
    class constructor InitStatics;
    class destructor CleanupStatics;
    class procedure RegisterExecutor(ThreadID: TThreadID; Executor: TExecutor); static; {$IFDEF inlining}inline;{$ENDIF}
    class procedure RemoveExecutor(Executor: TExecutor); static; {$IFDEF inlining}inline;{$ENDIF}
  public
    class function GetExecutor(ThreadID: Integer): TExecutor; static; {$IFDEF inlining}inline;{$ENDIF}
  private
    FErrorHandler: TErrorHandler;
    FTaskQueue: TTaskQueue;
    FCurrentTask: TTask;
    FSchedulerEnv: jmp_buf;
    FThread: TThread;

    procedure RaiseErrorHandler(Task: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure ScheduleTask(constref QueueEntry: TTaskQueueEntry);
    procedure ExecTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure FinalizeTask(ATask: TTaskQueueEntry);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True);
    procedure Await(ATask: TTask; FreeTask: Boolean = True);
    // For some reason this breaks fpc, so we added this as a global function
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
    procedure Yield;
    procedure Sleep(time: QWord);

    procedure Run;

    property OnError: TErrorHandler read FErrorHandler write FErrorHandler;
    property Thread: TThread read FThread;
  end;


function GetExecutor: TExecutor; {$IFDEF inlining}inline;{$ENDIF}
procedure Await(ATask: TTask; FreeTask: Boolean = True);
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
procedure Yield;
procedure AsyncSleep(time: QWord);

implementation

function GetExecutor: TExecutor;
begin
  Result := TExecutor.GetExecutor(TThread.CurrentThread.ThreadID);
end;

procedure Await(ATask: TTask; FreeTask: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('Await can only be called from within a Task');
  Executor.Await(ATask, FreeTask);
end;

generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('Await can only be called from within a Task');
  try
    Executor.Await(ATask, False);
    Result := ATask.TaskResult;
  finally
    if FreeTask then ATask.Free;
  end;
end;

procedure Yield;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('Yield can only be called from within a Task');
  Executor.Yield;
end;

procedure AsyncSleep(time: QWord);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('AsyncSleep can only be called from within a Task');
  Executor.Sleep(time);
end;

{ EUnhandledError }

constructor EUnhandledError.Create(AError: Exception; ATask: TTask);
begin
  Error := AError;
  Task := ATask;
end;

{ TExecutor }

class constructor TExecutor.InitStatics;
begin
  ThreadExecutorMap := TThreadExecutorMap.Create;
end;

class destructor TExecutor.CleanupStatics;
begin
  ThreadExecutorMap.Free;
end;

class procedure TExecutor.RegisterExecutor(ThreadID: TThreadID;
  Executor: TExecutor);
begin
  ThreadExecutorMap.Add(ThreadID, Executor);
end;

class procedure TExecutor.RemoveExecutor(Executor: TExecutor);
begin
  ThreadExecutorMap.Remove(Executor.Thread.ThreadID)
end;

class function TExecutor.GetExecutor(ThreadID: Integer): TExecutor;
begin
  if not ThreadExecutorMap.TryGetValue(ThreadID, Result) then
    Result := nil;
end;

procedure TExecutor.RaiseErrorHandler(Task: TTask);
begin
  if FErrorHandler.isFirst then
    FErrorHandler.First()(Task, Task.Error)
  else if FErrorHandler.isSecond then
    FErrorHandler.Second()(Task, Task.Error)
  else
    raise EUnhandledError.Create(Task.Error, Task);
end;

procedure TExecutor.ScheduleTask(constref QueueEntry: TTaskQueueEntry);
begin
  FTaskQueue.Enqueue(QueueEntry);
  if QueueEntry.Task.Status = tsNone then
    QueueEntry.Task.FStatus := tsScheduled
  else
    QueueEntry.Task.FStatus := tsRescheduled;
end;

procedure TExecutor.ExecTask(ATask: TTask);
begin
  if setjmp(FSchedulerEnv) <> 0 then
  begin
    // when the scheduler is called we end up here
    FCurrentTask := nil;
    Exit;
  end;
  FCurrentTask := ATask;
  if ATask.Status = tsRescheduled then
    ATask.Resume
  else
    ATask.Run(Self);
end;

procedure TExecutor.FinalizeTask(ATask: TTaskQueueEntry);
begin
  try
    // handle errors
    if ATask.RaiseError and (ATask.Task.Status = tsError) then
      RaiseErrorHandler(ATask.Task);
  finally
    if ATask.FreeTask then
      ATask.Task.Free;
  end;
end;

constructor TExecutor.Create;
begin
  FTaskQueue := TTaskQueue.Create;
  FCurrentTask := nil;
  FThread := nil;
end;

destructor TExecutor.Destroy;
begin
  // TODO: handle currently active and scheduled tasks
  FTaskQueue.Free;
  inherited Destroy;
end;

procedure TExecutor.RunAsync(ATask: TTask; FreeTask: Boolean;
  RaiseErrors: Boolean);
var
  NewEntry: TTaskQueueEntry;
begin
  if ATask.Status <> tsNone then
    raise ETaskAlreadyScheduledException.Create('Task already scheduled');
  ATask.FExecutor := Self;
  NewEntry.Task := ATask;
  NewEntry.FreeTask := FreeTask;
  NewEntry.RaiseError := RaiseErrors;
  ScheduleTask(NewEntry);
end;

procedure TExecutor.Await(ATask: TTask; FreeTask: Boolean);
begin
  if (TThread.CurrentThread <> FThread) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  try
    // schedule task to await
    RunAsync(ATask, False, False);
    // while not finished, yield to the scheduler to continue the q
    while ATask.Status < tsFinished do
      FCurrentTask.Yield;
    if ATask.Status = tsError then
      raise ATask.Error;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;

{generic function TExecutor.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  try
    Self.Await(ATask, False);
    Result := ATask.TaskResult;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;}

procedure TExecutor.Yield;
begin
  if (TThread.CurrentThread <> FThread) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only yield inside an asynchronous job');
  FCurrentTask.Yield;
end;

procedure TExecutor.Sleep(time: QWord);
begin
  if (TThread.CurrentThread <> FThread) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only sleep inside an asynchronous job');
  FCurrentTask.Sleep(time);
end;

procedure TExecutor.Run;
var
  NextTask: TTaskQueueEntry;
begin
  FThread := TThread.CurrentThread;
  RegisterExecutor(FThread.ThreadID, self);
  while FTaskQueue.Count > 0 do
  begin
    NextTask := FTaskQueue.Extract;
    ExecTask(NextTask.Task);
    // if task is not finished, reschedule
    if NextTask.Task.Status < tsFinished then
      ScheduleTask(NextTask)
    else
      FinalizeTask(NextTask);
  end;
  RemoveExecutor(Self);
end;

{ TRVTask }

procedure TRVTask.Execute;
begin
  FResult := Default(T);
end;

{ TTask }

class constructor TTask.InitStatics;
begin
  TTask.StackCache := TStackCache.Create;
end;

class destructor TTask.CleanupStatics;
var
  st: TStackData;
begin
  for st in TTask.StackCache do
    Freemem(st.Memory);
  TTask.StackCache.Free;
end;

class function TTask.AllocStack(ASize: SizeInt): TStackData;
begin
  if (ASize = DefaultTaskStackSize) and (TTask.StackCache.Count > 0) then
    Result := TTask.StackCache.ExtractIndex(TTask.StackCache.Count - 1)
  else
  begin
    Result.Size := ASize;
    Result.Memory := GetMem(ASize);
  end;
end;

class procedure TTask.FreeStack(constref AStackData: TStackData);
begin
  if not Assigned(AStackData.Memory) then Exit;
  if AStackData.Size = DefaultTaskStackSize then
    TTask.StackCache.Add(AStackData)
  else
    FreeMem(AStackData.Memory, AStackData.Size);
end;

constructor TTask.Create(AStackSize: SizeInt);
begin
  FTerminated := False;
  FError := nil;
  FStatus := tsNone;
  FExecutor := nil;
  // We don't allocate the stack now, so if another task before this one finishes
  // we might be able to reuse it's memory before allocating new one
  FStack.Size := AStackSize;
  FStack.Memory := nil;
end;

destructor TTask.Destroy;
begin
  TTask.FreeStack(FStack);
  inherited Destroy;
end;

procedure TTask.Run(AExecutor: TExecutor);
var
  StackPtr, BasePtr, NewStackPtr, NewBasePtr: Pointer;
  FrameSize: SizeInt;
begin
  FExecutor := AExecutor;
  // setup stack
  if not Assigned(FStack.Memory) then
    FStack := TTask.AllocStack(FStack.Size);
  // copy current frame so we can use local variables and arguments
  {$AsmMode intel}
  asm
  MOV StackPtr, RSP
  MOV BasePtr, RBP
  end;
  FrameSize := BasePtr - StackPtr;
  NewBasePtr := FStack.Memory + FStack.Size;
  NewStackPtr := NewBasePtr - FrameSize;
  Move(PByte(StackPtr)^, PByte(NewStackPtr)^, FrameSize);
  // move to new stack
  asm
  MOV RSP, NewStackPtr
  MOV RBP, NewBasePtr
  end;
  // Start the execution
  FStatus := tsRunning;
  try
    Execute;  
    FStatus := tsFinished;
  except on E: Exception do begin
    FError := E;
    FStatus := tsError;
  end
  end;
  longjmp(AExecutor.FSchedulerEnv, 1);
end;

procedure TTask.Resume;
begin
  FStatus := tsRunning;
  longjmp(FTaskEnv, 1);
end;

procedure TTask.Yield;
begin
  if FExecutor.FCurrentTask <> Self then
    raise ETaskNotActiveException.Create('Only an active task can yield');
  // store current state:
  if setjmp(FTaskEnv) = 0 then
    // go to scheduler
    longjmp(FExecutor.FSchedulerEnv, 1);
  // On return, check if we got terminated, if so notify task via exception
  if Terminated then
    raise ETaskTerminatedException.Create('Task terminated during waiting');
end;

procedure TTask.Sleep(Time: QWord);
var
  Start: QWord;
begin
  Start := GetTickCount64;
  repeat
    Yield;
  until GetTickCount64 - Start > Time;
end;

procedure TTask.Stop;
begin
  Terminate;
  Yield;
end;

procedure TTask.Terminate;
begin
  FTerminated := True;
end;

end.

