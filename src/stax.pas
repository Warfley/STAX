unit stax;

{$mode objfpc}{$H+}
{$IfDef RELEASE}
{$define inlining}
{$EndIf}

interface

uses
  Classes, SysUtils, math, Generics.Collections, stax.helpertypes,
  fibers;

type
  // Forward definitions
  TExecutor = class;
  TTask = class;

  // Exceptions
  ETaskTerminatedException = class(Exception);
  EAwaitedTaskTerminatedException = class(Exception);
  ETaskAlreadyTerminatedException = class(Exception);
  ETaskNotActiveException = class(Exception);
  EForeignTaskException = class(Exception);
  ETaskAlreadyScheduledException = class(Exception);
  ENotATaskException = class(Exception);
  ESomethingWentHorriblyWrongException = class(Exception);
  EAlreadyOneExecutorActiveException = class(Exception);

  { EUnhandledError }

  EUnhandledError = class(Exception)
  public
    Error: Exception;
    Task: TTask;

    constructor Create(AError: Exception; ATask: TTask);
    destructor Destroy; override;
  end;

  { EMultiTaskException }

  EMultiTaskException = class(Exception)
  public type
    TTaskExceptionMap = specialize TDictionary<TTask, Exception>;
  private
    FExceptions: TTaskExceptionMap;
    FOwnsTasks: Boolean;
  public
    constructor Create(ATask: TTask; AException: Exception; OwnsTasks: Boolean);
    destructor Destroy; override;

    procedure AddException(ATask: TTask; AException: Exception); inline;

    property Exceptions: TTaskExceptionMap read FExceptions;
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

  TTaskStatus = (tsNone=0, tsScheduled, tsRescheduled, tsWaiting, tsSleeping, tsRunning, tsFinished, tsError);

  { TTask }

  TTask = class(TDependable)
  // 4kb is the default page size. Maybe this is too little?
  public const DefaultTaskStackSize = 4 * 1024;
  private
    FAutoFree: Boolean;
    FRaiseExceptions: Boolean;
    FError: Exception;
    FStatus: TTaskStatus;
    FExecutor: TExecutor;
    FTerminated: Boolean;
    FFiber: TFiber;
    FNumDependencies: Integer;
  public
    function HasDependencies: Boolean; inline;
  private
    procedure DoExecute;
    procedure Schedule(AExecutor: TExecutor; AutoFree: Boolean;
      RaiseExceptions: Boolean);
    procedure Wait; {$IFDEF inlining}inline;{$ENDIF}
    procedure Reschedule; {$IFDEF inlining}inline;{$ENDIF}
    procedure DependencyResolved(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
    procedure AddDependency(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
  protected
    procedure Execute; virtual; abstract;
  public
    constructor Create(AStackSize: SizeInt = DefaultTaskStackSize);
    destructor Destroy; override;

    procedure Terminate; {$IFDEF inlining}inline;{$ENDIF}

    procedure Resume;
    procedure Yield;
    // because forward declarations of generics make fpc crash, this is put in a helper
    //procedure Await(ATask: TTask; FreeTask: Boolean = True); overload;
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload;
    procedure Sleep(Time: QWord);

    function ExtractError: Exception; {$IFDEF inlining}inline;{$ENDIF}

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

  TExceptionBehavior = (ebIgnore, ebRaiseAndTerminate, ebAccumulate);

  { TTaskHelper }

  TTaskHelper = class helper for TTask
  public
    procedure Await(ATask: TTask; FreeTask: Boolean = True); overload;
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload;
    procedure AwaitAll(const Tasks: Array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; FreeTasks: Boolean = True);
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
    TSleepQueue = specialize TMinHeap<TSleepQueueEntry, TSleepQueueComparator>;
  private
    FErrorHandler: TErrorHandler;
    FTaskQueue: TTaskQueue;
    FSleepQueue: TSleepQueue;
    FCurrentTask: TTask;
    FFiber: TFiber;
    FWaitingTasks: Integer;
    FTerminated: Boolean;

    procedure RaiseErrorHandler(Task: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure ScheduleTask(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}
    procedure RemoveSleepingTask(ATask: TTask);
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

    procedure TerminateTaskAndFinish(ATask: TTask); {$IfDef inlining}inline;{$endif}
    procedure StopRemainingTasks;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True);
    function ScheduleForAwait(ATask: TTask): TTask; {$IFDEF inlining}inline;{$ENDIF}
    // For some reason this breaks fpc, so we added this as a type helper
    //procedure Await(ATask: TTask; FreeTask: Boolean = True);
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
    procedure Yield; {$IFDEF inlining}inline;{$ENDIF}
    procedure Sleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

    procedure Run;
    procedure Terminate; inline;

    property OnError: TErrorHandler read FErrorHandler write FErrorHandler;
    property CurrentTask: TTask read FCurrentTask;
    property Terminated: Boolean read FTerminated;
  end;

  { TExecutorHelper }

  TExecutorHelper = class helper for TExecutor
  public
    procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
    procedure AwaitAll(const Tasks: Array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; FreeTasks: Boolean = True);
  end;

  { TTaskWorkerFiber }

  TTaskWorkerFiber = class(TExecutableFiber)
  private
    FTask: TTask;
  protected
    procedure Execute; override;
  public
    constructor Create(StackSize: SizeInt; ATask: TTask);
    destructor Destroy; override;
  end;


function GetExecutor: TExecutor; {$IFDEF inlining}inline;{$ENDIF}
function GetCurrentTask: TTask; {$IFDEF inlining}inline;{$ENDIF}
procedure RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True); {$IFDEF inlining}inline;{$ENDIF}
function ScheduleForAwait(ATask: TTask): TTask; {$IFDEF inlining}inline;{$ENDIF}
procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
procedure AwaitAll(const Tasks: array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; FreeTasks: Boolean = True); {$IFDEF inlining}inline;{$ENDIF}
procedure Yield; {$IFDEF inlining}inline;{$ENDIF}
procedure AsyncSleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

implementation

// global variables
threadvar CurrentExecutor: TExecutor;

{ EMultiTaskException }

constructor EMultiTaskException.Create(ATask: TTask; AException: Exception;
  OwnsTasks: Boolean);
begin
  inherited Create('One or more tasks threw an exception');
  FExceptions := TTaskExceptionMap.Create;
  FExceptions.Add(ATask, AException);
  FOwnsTasks := OwnsTasks;
end;

destructor EMultiTaskException.Destroy;
var
  E: Exception;
  ATask: TTask;
begin
  for E in FExceptions.Values do
    E.Free;
  if FOwnsTasks then
    for ATask in FExceptions.Keys do
      ATask.Free;
  FExceptions.Free;
  inherited Destroy;
end;

procedure EMultiTaskException.AddException(ATask: TTask; AException: Exception
  );
begin
  FExceptions.Add(ATask, AException);
end;

{ TTaskWorkerFiber }

procedure TTaskWorkerFiber.Execute;
begin
  FTask.DoExecute;
end;

constructor TTaskWorkerFiber.Create(StackSize: SizeInt; ATask: TTask);
begin
  inherited Create(StackSize);
  FTask := ATask;
end;

destructor TTaskWorkerFiber.Destroy;
begin
  inherited Destroy;
end;

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

function TTask.HasDependencies: Boolean;
begin
  Result := FNumDependencies > 0;
end;

procedure TTask.DoExecute;
begin
  try
    // Start the execution
    FStatus := tsRunning;
    try
      // will be cought directly by the except below
      if Terminated then
        raise ETaskTerminatedException.Create('Task got terminated before execution started');
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
  if not (FStatus in [tsWaiting, tsSleeping, tsRunning]) then
    Exit;
  if FStatus = tsWaiting then
    FExecutor.RemoveWaitingTask(Self);
  FStatus := tsRescheduled;
  FExecutor.ScheduleTask(Self);
end;

procedure TTask.DependencyResolved(ADependency: TDependable);
begin
  if FNumDependencies <= 0 then
    raise ETaskAlreadyScheduledException.Create('No dependencies left');
  Dec(FNumDependencies);
  Reschedule;
end;

procedure TTask.AddDependency(ADependency: TDependable);
begin
  Inc(FNumDependencies);
  ADependency.AddDependingTask(Self);
end;

constructor TTask.Create(AStackSize: SizeInt);
begin
  inherited Create;
  FTerminated := False;
  FError := nil;
  FStatus := tsNone;
  FExecutor := nil;
  FFiber := TTaskWorkerFiber.Create(AStackSize, Self);
end;

destructor TTask.Destroy;
begin
  FFiber.Free;
  if Assigned(FError) then
    FError.Free;
  inherited Destroy;
end;

procedure TTask.Resume;
begin
  FStatus := tsRunning;
  FExecutor.FFiber.SwitchTo(FFiber);
end;

procedure TTask.Yield;
begin
  if FExecutor.FCurrentTask <> Self then
    raise ETaskNotActiveException.Create('Only an active task can yield');
  if FTerminated then
    raise  ETaskAlreadyTerminatedException.Create('Can''t yield in an already terminated task. Just finish up!');
  FFiber.Return;
  // On return, check if we got terminated, if so notify task via exception
  if Terminated then
    raise ETaskTerminatedException.Create('Task terminated during waiting');
end;

procedure TTaskHelper.Await(ATask: TTask; FreeTask: Boolean);
var
  Err: Exception;
  TaskTerminated: ETaskTerminatedException;
begin
  TaskTerminated := nil;
  try
    if FExecutor.FCurrentTask <> Self then
      raise ETaskNotActiveException.Create('Only an active task can await');
    if FTerminated then
      raise  ETaskAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
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
      AddDependency(ATask);
      // if we are woken up due to a terminate, we need to still wait for the other task to perform memory management
      while ATask.Status < tsFinished do
        try
          Wait;
        except on E: ETaskTerminatedException do
          if not Assigned(TaskTerminated) then
          begin
          // so we can wait agian
          FTerminated := False;
          // remember to re-raise
          TaskTerminated := ETaskTerminatedException(AcquireExceptionObject);
          // Terminate task we are awaiting
          ATask.Terminate;
          end;
        end;
    end;
    // If terminated, restore termination state
    if Assigned(TaskTerminated) then
    begin
      FTerminated := True;
      raise TaskTerminated;
    end;
    // Check for errors
    if ATask.Status = tsError then
    begin
      Err := ATask.ExtractError;
      if Err is ETaskTerminatedException then
      begin
        Err.Free;
        raise EAwaitedTaskTerminatedException.Create('Awaited task got terminated before finishing');
      end
      else
        raise err;
    end;
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

procedure TTaskHelper.AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; FreeTasks: Boolean);

procedure FreeAllTasks;
var
  i: Integer;
begin
  for i:=Low(Tasks) to High(Tasks) do
    Tasks[i].Free;
end;

// checks all the assumptions and raises exception if not met
procedure CheckAssumptions;
var
  i: Integer;
begin
  // check if we are active
  if FExecutor.FCurrentTask <> Self then
  begin
    if FreeTasks then FreeAllTasks;
    raise ETaskNotActiveException.Create('Only an active task can await');
  end;
  // check if we are alive
  if FTerminated then
  begin
    if FreeTasks then FreeAllTasks;
    raise ETaskAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
  end;
  // check all tasks if we can await all of them
  for i:=Low(Tasks) to High(Tasks) do
    if (Tasks[i].Status > tsNone) and (Tasks[i].Executor <> FExecutor) then
    begin
      if FreeTasks then FreeAllTasks;
      raise EForeignTaskException.Create('Can''t await a task from another executor');
    end;
end;

// schedules tasks and adds self to dependency
// returns number of depending tasks
procedure ScheduleTasks;
var
  i: Integer;
begin
  for i:=Low(Tasks) to High(Tasks) do
  begin
    // schedule if not already done
    if Tasks[i].Status = tsNone then
      ScheduleForAwait(Tasks[i]);
    //  If not finished we add it as dependency
    if Tasks[i].Status < tsFinished then
    begin
      AddDependency(Tasks[i]);
    end;
  end;
end;

// returns the combined exception for all failed tasks
function GetMultiException: EMultiTaskException;
var
  i: Integer;
begin
  Result := nil;
  for i:=Low(Tasks) to High(Tasks) do
    if  Tasks[i].Status = tsError then
      if Assigned(Result) then
        Result.AddException(Tasks[i], Tasks[i].ExtractError)
      else
        Result := EMultiTaskException.Create(Tasks[i], Tasks[i].ExtractError, FreeTasks);
end;

// terminate all still active tasks
procedure TerminateRunningTasks;
var
  i: Integer;
begin
  for i:=Low(Tasks) to High(Tasks) do
    if Tasks[i].Status < tsFinished then
      Tasks[i].Terminate;
end;

// Free all tasks not referenced by the exception
procedure FreeNonExceptionTasks(NotInclude: EMultiTaskException);
var
  i: Integer;
begin
  for i:=Low(Tasks) to High(Tasks) do
    if not NotInclude.FExceptions.ContainsKey(Tasks[i]) then
      Tasks[i].Free;
end;

var
  AError: EMultiTaskException;
  TaskTerminated: ETaskTerminatedException;
begin
  TaskTerminated := nil;
  CheckAssumptions; 
  // If already one task raised an exception
  // we might not need to start working in the first place
  AError := nil;
  try
    ScheduleTasks;
    while HasDependencies do
    begin
      // check for an exception but only if this is the first we find
      // all others would be termination exceptions, we therefore only do that once
      if (ExceptionBehavior = ebRaiseAndTerminate) and not Assigned(AError) then
      begin
        AError := GetMultiException;
        // if we just discovered an exception by a task, terminate all others
        if Assigned(AError) then
          TerminateRunningTasks;
      end;
      // Wait until we are woken up by one of the tasks finishing or by termination
      try
        Wait;
      except on E: ETaskTerminatedException do
        if not Assigned(TaskTerminated) then
        begin
          // We don't handle errors as we are going to raise ETaskTerminatedException
          ExceptionBehavior := ebIgnore;
          FreeAndNil(AError);
          // so we can wait agian
          FTerminated := False;
          // remember to re-raise
          TaskTerminated := ETaskTerminatedException(AcquireExceptionObject);
          // terminate all tasks we are awaiting
          TerminateRunningTasks;
        end;
      end;
    end;
    // If terminated, restore termination state and raise error
    if Assigned(TaskTerminated) then
    begin
      FTerminated := True;
      raise TaskTerminated;
    end;
    // if we accumulate errors, we do so at the end
    // also if we want to report the first error but haven't found one yet,
    // check for errors during the last wait
    if (ExceptionBehavior = ebAccumulate) or
       ((ExceptionBehavior = ebRaiseAndTerminate) and not Assigned(AError)) then
       AError := GetMultiException;
  finally
    if FreeTasks then
      if not Assigned(AError) then
        FreeAllTasks
      else
        FreeNonExceptionTasks(AError);
    // If we encountered an error during execution, raise it now
    if Assigned(AError) then
      raise AError;
  end;
end;

procedure TTask.Sleep(Time: QWord);
begin
  if FExecutor.FCurrentTask <> Self then
    raise ETaskNotActiveException.Create('Only an active task can go to sleep');
  if FTerminated then
    raise ETaskAlreadyTerminatedException.Create('Can''t sleep in an already terminated task. Just finish up!');
  FExecutor.QueueSleep(Self, Time);
  FStatus := tsSleeping;
  Yield;
end;

function TTask.ExtractError: Exception;
begin
  Result := FError;
  FError := nil;
end;

procedure TTask.Terminate;
begin
  // Set terminated
  FTerminated := True;
  // Wake up if sleeping or waiting
  if FStatus = tsSleeping then
    FExecutor.RemoveSleepingTask(Self);
  Reschedule;
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
  Result := A.First < B.First;
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

procedure TExecutor.RaiseErrorHandler(Task: TTask);
var
  Err: Exception;
begin
  Err := Task.ExtractError;
  if Err is ETaskTerminatedException then
  begin
    Err.Free;
    Exit;
  end;
  if FErrorHandler.isFirst then
    FErrorHandler.First()(Task, Err)
  else if FErrorHandler.isSecond then
    FErrorHandler.Second()(Task, Err)
  else
    raise EUnhandledError.Create(Err, Task);
  Err.Free;
end;

procedure TExecutor.ScheduleTask(ATask: TTask);
begin
  FTaskQueue.Enqueue(ATask);
end;

procedure TExecutor.RemoveSleepingTask(ATask: TTask);
var
  i: SizeInt;
begin
  for i := 0 to SizeInt(FSleepQueue.Elements.Size) - 1 do
    if FSleepQueue.Elements.Mutable[i]^.Second = ATask then
    begin
      FSleepQueue.Delete(i);
      Break;
    end;
end;

procedure TExecutor.ExecTask(ATask: TTask);
begin
  FCurrentTask := ATask;
  ATask.Resume;
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
  while not FSleepQueue.Empty and (FSleepQueue.First^.First < CurrTime) do
    FSleepQueue.ExtractFirst.Second.Reschedule;
  if not FSleepQueue.Empty then
    Result := Min(FSleepQueue.First^.First - CurrTime, MaxWaitingTime);
end;

procedure TExecutor.QueueSleep(ATask: TTask; ATime: QWord);
var
  EndTime: QWord;
begin
  EndTime := GetTickCount64 + ATime;
  FSleepQueue.Insert(specialize Pair<QWord, TTask>(EndTime, ATask));
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
  if Assigned(CurrentExecutor) then
    raise EAlreadyOneExecutorActiveException.Create('Can only run one executor per thread');
  CurrentExecutor := Self;
  FWaitingTasks:=0;
  FTerminated := False;
  FFiber := TMainFiber.Create;
end;

procedure TExecutor.ExecutionLoop;
var
  NextTask: TTask;
  SleepTime: QWord;
begin
  while not FTerminated and ((FTaskQueue.Count > 0) or not FSleepQueue.Empty or TasksWaiting) do
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
        // but only if the task is running
        // if its waiting or sleeping we wait for it to be "woken up"
        if NextTask.Status = tsRunning then
          NextTask.Reschedule;
      end
      else
        FinalizeTask(NextTask);
    end;
  end;
end;

procedure TExecutor.TeardownExecution;
begin
  StopRemainingTasks;
  if TasksWaiting then
    raise ESomethingWentHorriblyWrongException.Create('Stopped execution while some tasks still waiting... This should never happen');
  FreeAndNil(FFiber);
  CurrentExecutor := nil;
end;

procedure TExecutor.TerminateTaskAndFinish(ATask: TTask);
begin
  // Only call this on tasks waiting or scheduled
  ATask.Terminate;
  ATask.Resume;
end;

procedure TExecutor.StopRemainingTasks;
var
  ATask: TTask;
begin
  // wakeup all sleeping tasks
  while not FSleepQueue.Empty do
    FSleepQueue.ExtractFirst.Second.Reschedule;
  // stop all scheduled tasks
  while FTaskQueue.Count > 0 do
  begin
    ATask := FTaskQueue.Extract;
    if ATask.FStatus = tsScheduled then
    begin
      // not started yet, simply kill it off:
      // first set the error
      ATask.FError := ETaskTerminatedException.Create('Task terminated by executor');
      ATask.FStatus := tsError;
      // but also notify all waiting tasks
      ATask.ResolveDependencies;
      // then Free
      if ATask.FAutoFree then
        ATask.Free;
    end
    else if ATask.FStatus = tsRescheduled then
    begin
      // task is yielded, set the terminated flag and continue to let it raise an exception
      TerminateTaskAndFinish(ATask);
      if ATask.FAutoFree then
        ATask.Free;
    end
    else
      raise ESomethingWentHorriblyWrongException.Create('Only scheduled and rescheduled tasks should be in queue');
  end;
end;

constructor TExecutor.Create;
begin
  FTaskQueue := TTaskQueue.Create;
  FSleepQueue := TSleepQueue.Create;
  FCurrentTask := nil;
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

function TExecutor.ScheduleForAwait(ATask: TTask): TTask;
begin
  RunAsync(ATask, False, False);
  Result := ATask;
end;

procedure TExecutorHelper.Await(ATask: TTask; FreeTask: Boolean);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  FCurrentTask.Await(ATask, FreeTask);
end;

generic function TExecutorHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  Result := FCurrentTask.specialize Await<TResult>(ATask, FreeTask);
end;

procedure TExecutorHelper.AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; FreeTasks: Boolean);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only await inside a task');
  FCurrentTask.AwaitAll(Tasks, ExceptionBehavior, FreeTasks);
end;

procedure TExecutor.Yield;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only yield inside an asynchronous job');
  FCurrentTask.Yield;
end;

procedure TExecutor.Sleep(time: QWord);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentTask) then
    raise ENotATaskException.Create('Can only sleep inside an asynchronous job');
  FCurrentTask.Sleep(time);
end;

procedure TExecutor.Run;
begin
  SetupExecution;
  ExecutionLoop;
  TeardownExecution;
end;

procedure TExecutor.Terminate;
begin
  FTerminated := True;
end;

{$EndRegion TExecutor}

{$Region HelperFunctions}

function GetExecutor: TExecutor;
begin
  Result := CurrentExecutor;
end;

function GetCurrentTask: TTask;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('No active executor on this thread');
  Result := Executor.CurrentTask;
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

function ScheduleForAwait(ATask: TTask): TTask;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('ScheduleForAwait can only be called from within a Task');
  Result := Executor.ScheduleForAwait(ATask);
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

procedure AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; FreeTasks: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotATaskException.Create('Await can only be called from within a Task');
  Executor.CurrentTask.AwaitAll(Tasks, ExceptionBehavior, FreeTasks);
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

