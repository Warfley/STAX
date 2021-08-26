unit stax;

{$mode objfpc}{$H+}
{$IfDef RELEASE}
{$define inlining}
{$EndIf}

interface

uses
  Classes, SysUtils, math, gpriorityqueue, Generics.Collections, stax.helpertypes
  {$IfDef WINDOWS}, windows.fiber{$EndIf};

type
  // Forward definitions
  TExecutor = class;
  TTask = class;

  // Exceptions
  ETaskTerminatedException = class(Exception);
  ETaskNotActiveException = class(Exception);
  EForeignTaskException = class(Exception);
  ETaskAlreadyScheduledException = class(Exception);
  ENotATaskException = class(Exception);

  { EUnhandledError }

  EUnhandledError = class(Exception)
  public
    Error: Exception;
    Task: TTask;

    constructor Create(AError: Exception; ATask: TTask);
    destructor Destroy; override;
  end;

  // Error handler
  TErrorFunction = procedure(Task: TTask; E: Exception);
  TErrorMethod = procedure(Task: TTask; E: Exception) of object;
  TErrorHandler = specialize TUnion<TErrorFunction, TErrorMethod>;

  { TDependable }

  TDependable = class
  private type
    TTaskList = specialize TList<TTask>;
  private
    FDependingTasks: TTaskList;
  protected
    procedure AddDependingTask(ATask: TTask); inline;
    procedure ResolveDependencies; // maybe virtual?
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TTaskStatus = (tsNone=0, tsScheduled, tsRescheduled, tsWaiting, tsRunning, tsFinished, tsError);

  { TTask }

  TTask = class(TDependable)
  // Maybe this is too much?
  public const DefaultTaskStackSize = DefaultStackSize;
  private type
  {$IfDef Windows}
    TFiberData = record
      Fiber: TFiber;
      StackSize: SizeInt;
    end;
  {$Else}
    TStackData = record
      Size: SizeInt;
      Memory: Pointer;
    end;
    TStackCache = specialize TList<TStackData>;
  private
    class var StackCache: TStackCache;
    class var CacheCriticalSection: TRTLCriticalSection;
    class constructor InitStatics;
    class destructor CleanupStatics;
    class function AllocStack(ASize: SizeInt): TStackData; static; {$IFDEF inlining}inline;{$ENDIF}
    class procedure FreeStack(constref AStackData: TStackData); static; {$IFDEF inlining}inline;{$ENDIF}
  {$EndIf}
  private
    FAutoFree: Boolean;
    FRaiseExceptions: Boolean;
    FError: Exception;
    FStatus: TTaskStatus;
    FExecutor: TExecutor;
    FTerminated: Boolean;
    {$IfDef Windows}
    FFiber: TFiberData;
    {$Else}
    FStack: TStackData;
    FTaskEnv: jmp_buf;
    {$EndIf}
    procedure DoExecute;
    procedure Schedule(AExecutor: TExecutor; AutoFree: Boolean;
      RaiseExceptions: Boolean);
    procedure Wait; {$IFDEF inlining}inline;{$ENDIF}
    procedure Reschedule; {$IFDEF inlining}inline;{$ENDIF}
    procedure DependencyResolved(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
  protected
    procedure Execute; virtual; abstract;
  public
    constructor Create(AStackSize: SizeInt = DefaultTaskStackSize);
    destructor Destroy; override;

    procedure Terminate; {$IFDEF inlining}inline;{$ENDIF}

    procedure Run;
    procedure Resume;
    procedure Yield;
    // because forward declarations of generics make fpc crash, this is put in a helper
    //procedure Await(ATask: TTask; FreeTask: Boolean = True); overload;
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload;
    procedure Sleep(Time: QWord);
    procedure Stop; {$IFDEF inlining}inline;{$ENDIF}

    function ExtractError: Exception; {$IFDEF inlining}inline;{$ENDIF}

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

  TTaskHelper = class helper for TTask
  public
    procedure Await(ATask: TTask; FreeTask: Boolean = True); overload;
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload;
  end;

  { TExecutor }

  TExecutor = class
  private
  const MaxWaitingTime = 50;
  private type
    TTaskQueue = specialize TQueue<TTask>;
    TSleepQueueEntry = specialize TPair<QWord, TTask>;

    { TSleepQueueComparator }

    TSleepQueueComparator = class
      public class function c(constref A, B: TSleepQueueEntry): Boolean; static; inline;
    end;
    TSleepQueue = specialize TPriorityQueue<TSleepQueueEntry, TSleepQueueComparator>;
    TThreadExecutorMap = specialize TDictionary<TThreadID, TExecutor>;
  private
    class var ThreadExecutorMap: TThreadExecutorMap;
    class var ExecutorMapCriticalSection: TRTLCriticalSection;
    class constructor InitStatics;
    class destructor CleanupStatics;
    class procedure RegisterExecutor(ThreadID: TThreadID; Executor: TExecutor); static; {$IFDEF inlining}inline;{$ENDIF}
    class procedure RemoveExecutor(Executor: TExecutor); static; {$IFDEF inlining}inline;{$ENDIF}
  public
    class function GetExecutor(ThreadID: Integer): TExecutor; static; {$IFDEF inlining}inline;{$ENDIF}
  private
    FErrorHandler: TErrorHandler;
    FTaskQueue: TTaskQueue;
    FSleepQueue: TSleepQueue;
    FCurrentTask: TTask;
    {$IfDef Windows}
    FFiber: TFiber;
    {$Else}
    FSchedulerEnv: jmp_buf;
    {$EndIf}
    FThread: TThread;
    FWaitingTasks: Integer;

    procedure RaiseErrorHandler(Task: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure ScheduleTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure ExecTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure FinalizeTask(ATask: TTask);
    function HandleSleepQueue: QWord;
    procedure QueueSleep(ATask: TTask; ATime: QWord); {$IFDEF inlining}inline;{$ENDIF}
    procedure AddWaitingTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure RemoveWaitingTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    function TasksWaiting: Boolean; {$IFDEF inlining}inline;{$ENDIF}

    procedure SetupExecution;
    procedure ExecutionLoop;
    procedure TeardownExecution;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True);
    procedure ScheduleForAwait(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    // For some reason this breaks fpc, so we added this as a type helper
    //procedure Await(ATask: TTask; FreeTask: Boolean = True);
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
    procedure Yield; {$IFDEF inlining}inline;{$ENDIF}
    procedure Sleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

    procedure Run;

    property OnError: TErrorHandler read FErrorHandler write FErrorHandler;
    property Thread: TThread read FThread;
    property CurrentTask: TTask read FCurrentTask;
  end;

  { TExecutorHelper }

  TExecutorHelper = class helper for TExecutor
  public
    procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
  end;


function GetExecutor: TExecutor; {$IFDEF inlining}inline;{$ENDIF}
procedure RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True); {$IFDEF inlining}inline;{$ENDIF}
procedure ScheduleForAwait(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
procedure Yield; {$IFDEF inlining}inline;{$ENDIF}
procedure AsyncSleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

implementation

{$Region TDependable}

{ TDependable }

procedure TDependable.AddDependingTask(ATask: TTask);
begin
  FDependingTasks.Add(ATask);
end;

procedure TDependable.ResolveDependencies;
var
  ATask: TTask;
begin
  for ATask in FDependingTasks do
    ATask.DependencyResolved(Self);
end;

constructor TDependable.Create;
begin
  FDependingTasks := TTaskList.Create;
end;

destructor TDependable.Destroy;
begin
  FDependingTasks.Free;
  inherited Destroy;
end;

{$EndRegion TDependable}

{$Region TTask}

{ TTask }

{$IfDef WINDOWS}
procedure FiberEntryPoint(lpFiberParameter: Pointer); stdcall;
begin
  TTask(lpFiberParameter).DoExecute;
  // return to scheduler
  SwitchToFiber(TTask(lpFiberParameter).Executor.FFiber);
end;

{$Else}

class constructor TTask.InitStatics;
begin
  TTask.StackCache := TStackCache.Create;
  InitCriticalSection(CacheCriticalSection);
end;

class destructor TTask.CleanupStatics;
var
  st: TStackData;
begin
  for st in TTask.StackCache do
    Freemem(st.Memory);
  DoneCriticalSection(CacheCriticalSection);
  TTask.StackCache.Free;
end;

class function TTask.AllocStack(ASize: SizeInt): TStackData;
begin
  EnterCriticalSection(CacheCriticalSection);
  try
    if (ASize = DefaultTaskStackSize) and (TTask.StackCache.Count > 0) then
      Result := TTask.StackCache.ExtractIndex(TTask.StackCache.Count - 1)
    else
    begin
      Result.Size := ASize;
      Result.Memory := GetMem(ASize);
    end;
  finally
    LeaveCriticalSection(CacheCriticalSection);
  end;
end;

class procedure TTask.FreeStack(constref AStackData: TStackData);
begin
  if not Assigned(AStackData.Memory) then Exit;
  EnterCriticalSection(CacheCriticalSection);
  try
    if AStackData.Size = DefaultTaskStackSize then
      TTask.StackCache.Add(AStackData)
    else
      FreeMem(AStackData.Memory, AStackData.Size);
  finally
    LeaveCriticalSection(CacheCriticalSection);
  end;
end;
{$EndIf}

procedure TTask.DoExecute;
begin
  try
  // Start the execution
  FStatus := tsRunning;
  try
      Execute;
      FStatus := tsFinished;
    except on E: Exception do begin
      FError := Exception(AcquireExceptionObject);
      FStatus := tsError;
    end
    end;
  finally
    // After we finished execution, notify all tasks depending on us
    ResolveDependencies;
  end;
end;

procedure TTask.Schedule(AExecutor: TExecutor; AutoFree: Boolean; RaiseExceptions: Boolean);
begin
  FExecutor := AExecutor;
  FAutoFree := AutoFree;
  FRaiseExceptions := RaiseExceptions;
  FStatus := tsScheduled;
  AExecutor.ScheduleTask(Self);
end;

procedure TTask.Wait;
begin
  FExecutor.AddWaitingTask(Self);
  FStatus := tsWaiting;
  Yield;
end;

procedure TTask.Reschedule;
begin
  if FStatus = tsWaiting then
    FExecutor.RemoveWaitingTask(Self);
  FStatus := tsRescheduled;
  FExecutor.ScheduleTask(Self);
end;

procedure TTask.DependencyResolved(ADependency: TDependable);
begin
  // currently the only dependency we have is awaiting some other task, therefore
  // if we finished awaiting, we can simply reschedule ourselves
  if FStatus <> tsWaiting then
    raise ETaskAlreadyScheduledException.Create('Can only reschedule a waiting task');
  Reschedule;
end;

constructor TTask.Create(AStackSize: SizeInt);
begin
  inherited Create;
  FTerminated := False;
  FError := nil;
  FStatus := tsNone;
  FExecutor := nil;
  {$IfDef Windows}
  FFiber.Fiber:=nil;
  FFiber.StackSize := AStackSize;
  {$Else}
  // We don't allocate the stack now, so if another task before this one finishes
  // we might be able to reuse it's memory before allocating new one
  FStack.Size := AStackSize;
  FStack.Memory := nil;
  {$EndIf}
end;

destructor TTask.Destroy;
begin
  {$IfDef WINDOWS}
  if Assigned(FFiber.Fiber) then
    DeleteFiber(FFiber.Fiber);
  {$Else}
  TTask.FreeStack(FStack);
  {$EndIf}
  if Assigned(FError) then
    FError.Free;
  inherited Destroy;
end;

procedure TTask.Run;
{$IfNDef Windows}
var
  StackPtr, BasePtr, NewStackPtr, NewBasePtr: Pointer;
  FrameSize: SizeInt;
{$EndIf}
begin
  {$IfDef WINDOWS}
  FFiber.Fiber := CreateFiber(FFiber.StackSize, @FiberEntryPoint, Self);
  if not Assigned(FFiber.Fiber) then
    raise EFiberError.Create('Cannot create fiber: ' + LastFiberError.ToString);
  SwitchToFiber(FFiber.Fiber);
  {$Else}
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
  DoExecute;
  longjmp(AExecutor.FSchedulerEnv, 1);
  {$EndIf}
end;

procedure TTask.Resume;
begin
  FStatus := tsRunning;
  {$IfDef WINDOWS}
  SwitchToFiber(FFiber.Fiber);
  {$Else}
  longjmp(FTaskEnv, 1);
  {$EndIf}
end;

procedure TTask.Yield;
begin
  if FExecutor.FCurrentTask <> Self then
    raise ETaskNotActiveException.Create('Only an active task can yield');
  {$IfDef Windows}
  SwitchToFiber(Executor.FFiber);
  {$Else}
  // store current state:
  if setjmp(FTaskEnv) = 0 then
    // go to scheduler
    longjmp(FExecutor.FSchedulerEnv, 1);
  {$EndIf}
  // On return, check if we got terminated, if so notify task via exception
  if Terminated then
    raise ETaskTerminatedException.Create('Task terminated during waiting');
end;

procedure TTaskHelper.Await(ATask: TTask; FreeTask: Boolean);
begin
  try
    // schedule task to await
    if ATask.Status = tsNone then
      ScheduleForAwait(ATask)
    else if ATask.Executor <> FExecutor then
      raise EForeignTaskException.Create('Can''t await a task from another executor');
    // if the task was already earlier it might already be finished
    // but we only need to wait if it wasnt finished
    if ATask.Status < tsFinished then
    begin
      // add as dependency to that task in order to be notified when it finishes
      ATask.AddDependingTask(Self);
      // then wait until we are woken up
      Wait;
    end;
    // Check for errors
    if ATask.Status = tsError then
      raise ATask.ExtractError;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;

generic function TTaskHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  try
    Self.Await(ATask, False);
    Result := ATask.TaskResult;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;

procedure TTask.Sleep(Time: QWord);
begin
  if FExecutor.FCurrentTask <> Self then
    raise ETaskNotActiveException.Create('Only an active task can go to sleep');
  FExecutor.QueueSleep(Self, Time);
  Wait;
end;

procedure TTask.Stop;
begin
  Terminate;
  Yield;
end;

function TTask.ExtractError: Exception;
begin
  Result := FError;
  FError := nil;
end;

procedure TTask.Terminate;
begin
  FTerminated := True;
end;

{ TRVTask }

procedure TRVTask.Execute;
begin
  FResult := Default(T);
end;

{$EndRegion TTask}

{$Region TExecutor}

{ TExecutor.TSleepQueueComparator }

class function TExecutor.TSleepQueueComparator.c(constref A, B: TSleepQueueEntry
  ): Boolean;
begin
  Result := A.First > B.First;
end;

{ EUnhandledError }

constructor EUnhandledError.Create(AError: Exception; ATask: TTask);
begin
  inherited Create(AError.Message);
  Error := AError;
  Task := ATask;
end;

destructor EUnhandledError.Destroy;
begin
  Error.Free;
  inherited Destroy;
end;

{ TExecutor }

class constructor TExecutor.InitStatics;
begin
  ThreadExecutorMap := TThreadExecutorMap.Create;
  InitCriticalSection(ExecutorMapCriticalSection);
end;

class destructor TExecutor.CleanupStatics;
begin
  DoneCriticalSection(ExecutorMapCriticalSection);
  ThreadExecutorMap.Free;
end;

class procedure TExecutor.RegisterExecutor(ThreadID: TThreadID;
  Executor: TExecutor);
begin
  EnterCriticalSection(ExecutorMapCriticalSection);
  try
    ThreadExecutorMap.Add(ThreadID, Executor);
  finally
    LeaveCriticalSection(ExecutorMapCriticalSection);
  end;
end;

class procedure TExecutor.RemoveExecutor(Executor: TExecutor);
begin
  EnterCriticalSection(ExecutorMapCriticalSection);
  try
    ThreadExecutorMap.Remove(Executor.Thread.ThreadID);
  finally
    LeaveCriticalSection(ExecutorMapCriticalSection);
  end;
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
    raise EUnhandledError.Create(Task.ExtractError, Task);
end;

procedure TExecutor.ScheduleTask(ATask: TTask);
begin
  FTaskQueue.Enqueue(ATask);
end;

procedure TExecutor.ExecTask(ATask: TTask);
begin
  {$IfNDef Windows}
  if setjmp(FSchedulerEnv) <> 0 then
  begin
    // when the scheduler is called we end up here
    FCurrentTask := nil;
    Exit;
  end;
  {$EndIf}
  FCurrentTask := ATask;
  if ATask.Status = tsRescheduled then
    ATask.Resume
  else
    ATask.Run;
  FCurrentTask := nil;
end;

procedure TExecutor.FinalizeTask(ATask: TTask);
begin
  try
    // handle errors
    if ATask.FRaiseExceptions and (ATask.Status = tsError) then
      RaiseErrorHandler(ATask);
  finally
    if ATask.FAutoFree then
      ATask.Free;
  end;
end;

function TExecutor.HandleSleepQueue: QWord;
var
  CurrTime: QWord;
begin
  Result := MaxWaitingTime;
  CurrTime := GetTickCount64;
  while not FSleepQueue.IsEmpty and (FSleepQueue.Top.First < CurrTime) do
  begin
    FSleepQueue.Top.Second.Reschedule;
    FSleepQueue.Pop;
  end;
  if not FSleepQueue.IsEmpty then
    Result := Min(FSleepQueue.Top.First - CurrTime, MaxWaitingTime);
end;

procedure TExecutor.QueueSleep(ATask: TTask; ATime: QWord);
var
  EndTime: QWord;
begin
  EndTime := GetTickCount64 + ATime;
  FSleepQueue.Push(specialize Pair<QWord, TTask>(EndTime, ATask));
end;

procedure TExecutor.AddWaitingTask(ATask: TTask);
begin
  // at the moment just count how many are waiting
  // maybe we need to keep log of the waiting tasks in the future
  Inc(FWaitingTasks);
end;

procedure TExecutor.RemoveWaitingTask(ATask: TTask);
begin
  Dec(FWaitingTasks);
end;

function TExecutor.TasksWaiting: Boolean;
begin
  Result := FWaitingTasks > 0;
end;

procedure TExecutor.SetupExecution;
begin
  FThread := TThread.CurrentThread;
  RegisterExecutor(FThread.ThreadID, self);
  FWaitingTasks:=0;
  {$IfDef WINDOWS}
  FFiber := ConvertThreadToFiber(Self);
  if not Assigned(FFiber) then
    raise EFiberError.Create('Error calling ConvertThreadToFiber: ' + LastFiberError.ToString);
  {$EndIf}
end;

procedure TExecutor.ExecutionLoop;
var
  NextTask: TTask;
  SleepTime: QWord;
begin
  while (FTaskQueue.Count > 0) or not FSleepQueue.IsEmpty or TasksWaiting do
  begin
    SleepTime := HandleSleepQueue;
    if FTaskQueue.Count = 0 then
      // wait at most 50 ms
      SysUtils.Sleep(SleepTime)
    else
    begin
      NextTask := FTaskQueue.Extract;
      ExecTask(NextTask);
      // if task is not finished, reschedule
      if NextTask.Status < tsFinished then
      begin
        // but only if the task is not set to waiting
        // if its waiting we wait for it to be "woken up"
        if NextTask.Status <> tsWaiting then
          NextTask.Reschedule;
      end
      else
        FinalizeTask(NextTask);
    end;
  end;
end;

procedure TExecutor.TeardownExecution;
begin
  {$IfDef WINDOWS}
  if not ConvertFiberToThread then
    raise EFiberError.Create('Error calling ConvertFiberToThread: ' + LastFiberError.ToString);
  {$EndIf}
  RemoveExecutor(Self);
end;

constructor TExecutor.Create;
begin
  FTaskQueue := TTaskQueue.Create;
  FSleepQueue := TSleepQueue.Create;
  FCurrentTask := nil;
  FThread := nil;
end;

destructor TExecutor.Destroy;
begin
  // TODO: handle currently active and scheduled tasks
  FSleepQueue.Free;
  FTaskQueue.Free;
  inherited Destroy;
end;

procedure TExecutor.RunAsync(ATask: TTask; FreeTask: Boolean;
  RaiseErrors: Boolean);
begin
  if ATask.Status <> tsNone then
    raise ETaskAlreadyScheduledException.Create('Task already scheduled');
  ATask.FExecutor := Self;
  ATask.Schedule(self, FreeTask, RaiseErrors);
end;

procedure TExecutor.ScheduleForAwait(ATask: TTask);
begin
  RunAsync(ATask, False, False);
end;

procedure TExecutorHelper.Await(ATask: TTask; FreeTask: Boolean);
begin
  if (TThread.CurrentThread <> FThread) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  FCurrentTask.Await(ATask, FreeTask);
end;

generic function TExecutorHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  if (TThread.CurrentThread <> FThread) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  Result := FCurrentTask.specialize Await<TResult>(ATask, FreeTask);
end;

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
begin
  SetupExecution;
  ExecutionLoop;
  TeardownExecution;
end;

{$EndRegion TExecutor}

{$Region HelperFunctions}

function GetExecutor: TExecutor;
begin
  Result := TExecutor.GetExecutor(TThread.CurrentThread.ThreadID);
end;

procedure RunAsync(ATask: TTask; FreeTask: Boolean; RaiseErrors: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('RunAsync can only be called from within a Task');
  Executor.RunAsync(ATask, FreeTask, RaiseErrors);
end;

procedure ScheduleForAwait(ATask: TTask);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('ScheduleForAwait can only be called from within a Task');
  Executor.ScheduleForAwait(ATask);
end;

procedure Await(ATask: TTask; FreeTask: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotATaskException.Create('Await can only be called from within a Task');
  Executor.CurrentTask.Await(ATask, FreeTask);
end;

generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotATaskException.Create('Await can only be called from within a Task');
  Result := Executor.CurrentTask.specialize Await<TResult>(ATask, FreeTask);
end;

procedure Yield;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotATaskException.Create('Yield can only be called from within a Task');
  Executor.CurrentTask.Yield;
end;

procedure AsyncSleep(time: QWord);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotATaskException.Create('AsyncSleep can only be called from within a Task');
  Executor.CurrentTask.Sleep(time);
end;

{$EndRegion HelperFunctions}

end.

