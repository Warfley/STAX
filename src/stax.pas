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
  TExecutable = class;
  TTask = class;

  // Exceptions
  ETaskTerminatedException = class(Exception);
  EAwaitedTaskTerminatedException = class(Exception);
  ETaskAlreadyTerminatedException = class(Exception);
  ETaskNotActiveException = class(Exception);
  EForeignTaskException = class(Exception);
  EAlreadyAssignedExecutorException = class(Exception);
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
    TExecutableList = specialize TList<TExecutable>;
  private
    FDepending: TExecutableList;
  protected
    procedure AddDepending(AExecutable: TExecutable); inline;
    procedure ResolveDependencies; // maybe virtual?
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TExecutionState = (esNone=0, esScheduled, esRescheduled, esWaiting, esSleeping, esRunning, esFinished, esError);

  { TExecutable }

  TExecutable = class(TDependable)
  // 4kb is the default page size. Maybe this is too little?
  public const DefaultTaskStackSize = 4 * 1024;
  private
    FError: Exception;
    FStatus: TExecutionState;
    FExecutor: TExecutor;
    FTerminated: Boolean;
    FFiber: TFiber;
    FNumDependencies: Integer;
  public
    function HasDependencies: Boolean; inline;
  private
    // Scheduling
    procedure SetExecutor(AExecutor: TExecutor); {$IFDEF inlining}inline;{$ENDIF}
    procedure MarkScheduled;
    procedure YieldToScheduler; {$IFDEF inlining}inline;{$ENDIF}
    procedure Wait; {$IFDEF inlining}inline;{$ENDIF}

    // Dependency management
    procedure DependencyResolved(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
    procedure AddDependency(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}

    // Execution
    procedure DoExecute;
  protected
    procedure Execute; virtual; abstract;

    // called by scheduler after execution finished
    procedure FinalizeExecution; virtual;
  public
    constructor Create(AStackSize: SizeInt = DefaultTaskStackSize);
    destructor Destroy; override;

    procedure Terminate; {$IFDEF inlining}inline;{$ENDIF}

    // because forward declarations of generics make fpc crash, this is put in a helper
    //procedure Await(ATask: TTask; FreeTask: Boolean = True); overload;
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload;
    procedure Sleep(Time: QWord);

    function ExtractError: Exception; {$IFDEF inlining}inline;{$ENDIF}

    property Status: TExecutionState read FStatus;
    property Executor: TExecutor read FExecutor;
    property Terminated: Boolean read FTerminated;
  end;

  { TTask }

  TTask = class(TExecutable)
  private
    FAutoFree: Boolean;
    FRaiseExceptions: Boolean;

    procedure SetSchedulingParameter(AExecutor: TExecutor; AutoFree: Boolean; RaiseExceptions: Boolean);
  protected
    procedure FinalizeExecution; override;
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

  { TExecutableHelper }

  TExecutableHelper = class helper for TExecutable
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
    TSchedulingQueue = specialize TQueue<TExecutable>;
    TSleepQueueEntry = specialize TPair<QWord, TExecutable>;

    { TSleepQueueComparator }

    TSleepQueueComparator = class
      public class function c(constref A, B: TSleepQueueEntry): Boolean; static; inline;
    end;
    TSleepQueue = specialize TMinHeap<TSleepQueueEntry, TSleepQueueComparator>;
  private
    FErrorHandler: TErrorHandler;
    FSchedulingQueue: TSchedulingQueue;
    FSleepQueue: TSleepQueue;
    FCurrentExecution: TExecutable;
    FFiber: TFiber;
    FWaitingCount: Integer;
    FTerminated: Boolean;

    // error handling
    procedure RaiseErrorHandler(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}

    // scheduling
    procedure ScheduleForExecution(AExecutable: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    procedure ContinueExecutable(AExecutable: TExecutable); {$IFDEF inlining}inline;{$ENDIF}

    // Sleeping
    procedure QueueSleep(ATask: TExecutable; ATime: QWord); {$IFDEF inlining}inline;{$ENDIF}
    procedure RemoveSleeping(AExecutable: TExecutable);
    function HandleSleepQueue: QWord;

    // Waiting
    procedure AddWaitingTask(ATask: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    procedure RemoveWaitingTask(ATask: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    function TasksWaiting: Boolean; {$IFDEF inlining}inline;{$ENDIF}

    // execution loop
    procedure SetupExecution;
    procedure ExecutionLoop;
    procedure TeardownExecution;

    // termination
    procedure TerminateTaskAndFinish(AExecution: TExecutable); {$IfDef inlining}inline;{$endif}
    procedure StopRemainingExecutions;
  public
    constructor Create;
    destructor Destroy; override;

    function RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True): TTask;
    function ScheduleForAwait(ATask: TTask): TTask; {$IFDEF inlining}inline;{$ENDIF}
    // For some reason this breaks fpc, so we added this as a type helper
    //procedure Await(ATask: TExecutable; FreeTask: Boolean = True);
    //generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
    procedure Sleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

    procedure Run;
    procedure Terminate; inline;

    property OnError: TErrorHandler read FErrorHandler write FErrorHandler;
    property CurrentTask: TExecutable read FCurrentExecution;
    property Terminated: Boolean read FTerminated;
  end;

  { TExecutorHelper }

  TExecutorHelper = class helper for TExecutor
  public
    procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
    procedure AwaitAll(const Tasks: Array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; FreeTasks: Boolean = True);
  end;

  { TExecutionWorkFiber }

  TExecutionWorkFiber = class(TExecutableFiber)
  private
    FExecution: TExecutable;
  protected
    procedure Execute; override;
  public
    constructor Create(StackSize: SizeInt; AExecution: TExecutable);
    destructor Destroy; override;
  end;


function GetExecutor: TExecutor; {$IFDEF inlining}inline;{$ENDIF}
function GetCurrentExecution: TExecutable; {$IFDEF inlining}inline;{$ENDIF}
function RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True): TTask; {$IFDEF inlining}inline;{$ENDIF}
function ScheduleForAwait(ATask: TTask): TTask; {$IFDEF inlining}inline;{$ENDIF}
procedure Await(ATask: TTask; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
procedure AwaitAll(const Tasks: array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; FreeTasks: Boolean = True); {$IFDEF inlining}inline;{$ENDIF}
procedure AsyncSleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

implementation

// global variables
threadvar CurrentExecutor: TExecutor;

{ TTask }

procedure TTask.SetSchedulingParameter(AExecutor: TExecutor; AutoFree: Boolean;
  RaiseExceptions: Boolean);
begin
  SetExecutor(AExecutor);
  FAutoFree := AutoFree;
  FRaiseExceptions := RaiseExceptions;
end;

procedure TTask.FinalizeExecution;
begin
  try
    inherited FinalizeExecution;
    if FRaiseExceptions and (FStatus = esError) then
      FExecutor.RaiseErrorHandler(Self);
  finally
    if FAutoFree then
      Free;
  end;
end;

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

{ TExecutionWorkFiber }

procedure TExecutionWorkFiber.Execute;
begin
  FExecution.DoExecute;
end;

constructor TExecutionWorkFiber.Create(StackSize: SizeInt; AExecution: TExecutable);
begin
  inherited Create(StackSize);
  FExecution := AExecution;
end;

destructor TExecutionWorkFiber.Destroy;
begin
  inherited Destroy;
end;

{$Region TDependable}

{ TDependable }

procedure TDependable.AddDepending(AExecutable: TExecutable);
begin
  FDepending.Add(AExecutable);
end;

procedure TDependable.ResolveDependencies;
var
  ADepending: TExecutable;
begin
  for ADepending in FDepending do
    ADepending.DependencyResolved(Self);
end;

constructor TDependable.Create;
begin
  FDepending := TExecutableList.Create;
end;

destructor TDependable.Destroy;
begin
  FDepending.Free;
  inherited Destroy;
end;

{$EndRegion TDependable}

{$Region TExecutable}

{ TExecutable }

function TExecutable.HasDependencies: Boolean;
begin
  Result := FNumDependencies > 0;
end;

procedure TExecutable.SetExecutor(AExecutor: TExecutor);
begin
  if Assigned(FExecutor) then
    raise EAlreadyAssignedExecutorException.Create('Can only execute on one executor');
  FExecutor := AExecutor;
end;

procedure TExecutable.MarkScheduled;
begin
  if not (FStatus in [esNone, esWaiting, esSleeping, esRunning]) then
    Exit;
  if FStatus = esWaiting then
    FExecutor.RemoveWaitingTask(Self);
  if FStatus = esNone then
    FStatus := esScheduled
  else
    FStatus := esRescheduled;
end;

procedure TExecutable.YieldToScheduler;
begin
  FFiber.Return;
  // After we are called again by the scheduler, we set ourselves to running
  FStatus := esRunning;
  // If terminated during waiting raise an exception
  if FTerminated then
    raise ETaskTerminatedException.Create('Task got terminated');
end;

procedure TExecutable.Wait;
begin
  FExecutor.AddWaitingTask(Self);
  FStatus := esWaiting;
  YieldToScheduler;
end;

procedure TExecutable.DependencyResolved(ADependency: TDependable);
begin
  if FNumDependencies <= 0 then
    raise ESomethingWentHorriblyWrongException.Create('No dependencies left');
  Dec(FNumDependencies);
  FExecutor.ScheduleForExecution(Self);
end;

procedure TExecutable.AddDependency(ADependency: TDependable);
begin
  Inc(FNumDependencies);
  ADependency.AddDepending(Self);
end;

procedure TExecutable.DoExecute;
begin
  // Start the execution
  FStatus := esRunning;
  try
    // will be cought directly by the except below
    if Terminated then
      raise ETaskTerminatedException.Create('Task got terminated before execution started');
    Execute;
    FStatus := esFinished;
  except on E: Exception do begin
    FError := Exception(AcquireExceptionObject);
    FStatus := esError;
  end
  end;
end;

procedure TExecutable.FinalizeExecution;
begin
  ResolveDependencies;
end;

constructor TExecutable.Create(AStackSize: SizeInt);
begin
  inherited Create;
  FTerminated := False;
  FError := nil;
  FStatus := esNone;
  FExecutor := nil;
  FFiber := TExecutionWorkFiber.Create(AStackSize, Self);
end;

destructor TExecutable.Destroy;
begin
  FFiber.Free;
  if Assigned(FError) then
    FError.Free;
  inherited Destroy;
end;

procedure TExecutableHelper.Await(ATask: TTask; FreeTask: Boolean);
var
  Err: Exception;
  TaskTerminated: ETaskTerminatedException;
begin
  TaskTerminated := nil;
  try
    if FExecutor.FCurrentExecution <> Self then
      raise ETaskNotActiveException.Create('Only an active task can await');
    if FTerminated then
      raise  ETaskAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
    // SetSchedulingParameter task to await
    if ATask.Status = esNone then
      ScheduleForAwait(ATask)
    else if ATask.Executor <> FExecutor then
      raise EForeignTaskException.Create('Can''t await a task from another executor');
    // if the task was already earlier it might already be finished
    // but we only need to wait if it wasnt finished
    if ATask.Status < esFinished then
    begin
      // add as dependency to that task in order to be notified when it finishes
      AddDependency(ATask);
      // if we are woken up due to a terminate, we need to still wait for the other task to perform memory management
      while ATask.Status < esFinished do
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
    if ATask.Status = esError then
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

generic function TExecutableHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  try
    Self.Await(ATask, False);
    Result := ATask.TaskResult;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;

procedure TExecutableHelper.AwaitAll(const Tasks: array of TTask;
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
  if FExecutor.FCurrentExecution <> Self then
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
    if (Tasks[i].Status > esNone) and (Tasks[i].Executor <> FExecutor) then
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
    // SetSchedulingParameter if not already done
    if Tasks[i].Status = esNone then
      ScheduleForAwait(Tasks[i]);
    //  If not finished we add it as dependency
    if Tasks[i].Status < esFinished then
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
    if  Tasks[i].Status = esError then
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
    if Tasks[i].Status < esFinished then
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

procedure TExecutable.Sleep(Time: QWord);
begin
  if FExecutor.FCurrentExecution <> Self then
    raise ETaskNotActiveException.Create('Only an active task can go to sleep');
  if FTerminated then
    raise ETaskAlreadyTerminatedException.Create('Can''t sleep in an already terminated task. Just finish up!');
  if Time = 0 then
    FExecutor.ScheduleForExecution(Self)
  else
  begin
    FExecutor.QueueSleep(Self, Time);
    FStatus := esSleeping;
  end;
  YieldToScheduler;
end;

function TExecutable.ExtractError: Exception;
begin
  Result := FError;
  FError := nil;
end;

procedure TExecutable.Terminate;
begin
  // Set terminated
  FTerminated := True;
  // Wake up if sleeping or waiting
  if FStatus = esSleeping then
    FExecutor.RemoveSleeping(Self);
  FExecutor.ScheduleForExecution(Self);
end;

{ TRVTask }

procedure TRVTask.Execute;
begin
  FResult := Default(T);
end;

{$EndRegion TExecutable}

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

procedure TExecutor.RaiseErrorHandler(ATask: TTask);
var
  Err: Exception;
begin
  Err := ATask.ExtractError;
  if Err is ETaskTerminatedException then
  begin
    Err.Free;
    Exit;
  end;
  if FErrorHandler.isFirst then
    FErrorHandler.First()(ATask, Err)
  else if FErrorHandler.isSecond then
    FErrorHandler.Second()(ATask, Err)
  else
    raise EUnhandledError.Create(Err, ATask);
  Err.Free;
end;

procedure TExecutor.ScheduleForExecution(AExecutable: TExecutable);
begin
  if not (AExecutable.FStatus in [esNone, esWaiting, esSleeping, esRunning]) then
    Exit;
  AExecutable.MarkScheduled;
  FSchedulingQueue.Enqueue(AExecutable);
end;

procedure TExecutor.ContinueExecutable(AExecutable: TExecutable);
begin
  FCurrentExecution := AExecutable;
  FFiber.SwitchTo(AExecutable.FFiber);
  FCurrentExecution := nil;
end;

procedure TExecutor.QueueSleep(ATask: TExecutable; ATime: QWord);
var
  EndTime: QWord;
begin
  EndTime := GetTickCount64 + ATime;
  FSleepQueue.Insert(specialize Pair<QWord, TExecutable>(EndTime, ATask));
end;

procedure TExecutor.RemoveSleeping(AExecutable: TExecutable);
var
  i: SizeInt;
begin
  for i := 0 to SizeInt(FSleepQueue.Elements.Size) - 1 do
    if FSleepQueue.Elements.Mutable[i]^.Second = AExecutable then
    begin
      FSleepQueue.Delete(i);
      Break;
    end;
end;

function TExecutor.HandleSleepQueue: QWord;
var
  CurrTime: QWord;
begin
  Result := MaxWaitingTime;
  CurrTime := GetTickCount64;
  while not FSleepQueue.Empty and (FSleepQueue.First^.First < CurrTime) do
    ScheduleForExecution(FSleepQueue.ExtractFirst.Second);
  if not FSleepQueue.Empty then
    Result := Min(FSleepQueue.First^.First - CurrTime, MaxWaitingTime);
end;

procedure TExecutor.AddWaitingTask(ATask: TExecutable);
begin
  // at the moment just count how many are waiting
  // maybe we need to keep log of the waiting tasks in the future
  Inc(FWaitingCount);
end;

procedure TExecutor.RemoveWaitingTask(ATask: TExecutable);
begin
  Dec(FWaitingCount);
end;

function TExecutor.TasksWaiting: Boolean;
begin
  Result := FWaitingCount > 0;
end;

procedure TExecutor.SetupExecution;
begin
  if Assigned(CurrentExecutor) then
    raise EAlreadyOneExecutorActiveException.Create('Can only run one executor per thread');
  CurrentExecutor := Self;
  FWaitingCount:=0;
  FTerminated := False;
  FFiber := TMainFiber.Create;
end;

procedure TExecutor.ExecutionLoop;
var
  NextExecution: TExecutable;
  SleepTime: QWord;
begin
  while not FTerminated and ((FSchedulingQueue.Count > 0) or not FSleepQueue.Empty or TasksWaiting) do
  begin
    SleepTime := HandleSleepQueue;
    if FSchedulingQueue.Count = 0 then
      // wait at most 50 ms
      SysUtils.Sleep(SleepTime)
    else
    begin
      NextExecution := FSchedulingQueue.Extract;
      ContinueExecutable(NextExecution);
      // if task finished, call the finish within the executors stack
      if NextExecution.Status >= esFinished then
        NextExecution.FinalizeExecution;
    end;
  end;
end;

procedure TExecutor.TeardownExecution;
begin
  StopRemainingExecutions;
  if TasksWaiting then
    raise ESomethingWentHorriblyWrongException.Create('Stopped execution while some tasks still waiting... This should never happen');
  FreeAndNil(FFiber);
  CurrentExecutor := nil;
end;

procedure TExecutor.TerminateTaskAndFinish(AExecution: TExecutable);
begin
  // Only call this on tasks waiting or scheduled
  AExecution.Terminate;
  FFiber.SwitchTo(AExecution.FFiber);
end;

procedure TExecutor.StopRemainingExecutions;
var
  AExecution: TExecutable;
begin
  // wakeup all sleeping tasks
  while not FSleepQueue.Empty do
    ScheduleForExecution(FSleepQueue.ExtractFirst.Second);
  // stop all scheduled tasks
  while FSchedulingQueue.Count > 0 do
  begin
    AExecution := FSchedulingQueue.Extract;
    if AExecution.FStatus = esScheduled then
    begin
      // not started yet, simply kill it off:
      // first set the error
      AExecution.FError := ETaskTerminatedException.Create('Task terminated by executor');
      AExecution.FStatus := esError;
      // Finalize execution
      AExecution.FinalizeExecution;
    end
    else if AExecution.FStatus = esRescheduled then
    begin
      // task is yielded, set the terminated flag and continue to let it raise an exception
      TerminateTaskAndFinish(AExecution);
      if AExecution.Status < esFinished then
        raise ESomethingWentHorriblyWrongException.Create('Execution didn''t finish when it should');
      // finalize task
      AExecution.FinalizeExecution;
    end
    else
      raise ESomethingWentHorriblyWrongException.Create('Only scheduled and rescheduled tasks should be in queue');
  end;
end;

constructor TExecutor.Create;
begin
  FSchedulingQueue := TSchedulingQueue.Create;
  FSleepQueue := TSleepQueue.Create;
  FCurrentExecution := nil;
end;

destructor TExecutor.Destroy;
begin
  // TODO: handle currently active and scheduled tasks
  FSleepQueue.Free;
  FSchedulingQueue.Free;
  inherited Destroy;
end;

function TExecutor.RunAsync(ATask: TTask; FreeTask: Boolean;
  RaiseErrors: Boolean): TTask;
begin
  ATask.SetSchedulingParameter(Self, FreeTask, RaiseErrors);
  ScheduleForExecution(ATask);
  Result := ATask;
end;

function TExecutor.ScheduleForAwait(ATask: TTask): TTask;
begin
  Result := RunAsync(ATask, False, False);
end;

procedure TExecutorHelper.Await(ATask: TTask; FreeTask: Boolean);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotATaskException.Create('Can only await inside a task');
  FCurrentExecution.Await(ATask, FreeTask);
end;

generic function TExecutorHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotATaskException.Create('Can only await inside a task');
  Result := FCurrentExecution.specialize Await<TResult>(ATask, FreeTask);
end;

procedure TExecutorHelper.AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; FreeTasks: Boolean);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotATaskException.Create('Can only await inside a task');
  FCurrentExecution.AwaitAll(Tasks, ExceptionBehavior, FreeTasks);
end;

procedure TExecutor.Sleep(time: QWord);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotATaskException.Create('Can only sleep inside an asynchronous job');
  FCurrentExecution.Sleep(time);
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

function GetCurrentExecution: TExecutable;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('No active executor on this thread');
  Result := Executor.CurrentTask;
end;

function RunAsync(ATask: TTask; FreeTask: Boolean; RaiseErrors: Boolean): TTask;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotATaskException.Create('RunAsync can only be called from within a Task');
  Result := Executor.RunAsync(ATask, FreeTask, RaiseErrors);
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

