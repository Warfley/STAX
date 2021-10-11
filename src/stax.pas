unit stax;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}

{$IfDef RELEASE}
{$define inlining}
{$EndIf}

interface

uses
  Classes, SysUtils, math, Generics.Collections, stax.helpertypes,
  fibers;

// 16kb (4 pages). Maybe this is too little?
const DefaultTaskStackSize = 16 * 1024;

type
  // Forward definitions
  TExecutor = class;
  TExecutable = class;
  TTask = class;

  // Exceptions
  ETaskTerminatedException = class(Exception);
  EAwaitedTaskTerminatedException = class(Exception);
  EAwaitedGeneratorTerminatedException = class(Exception);
  EAwaitTimeoutException = class(Exception);
  EExecutionAlreadyTerminatedException = class(Exception);
  EExecutionNotActiveException = class(Exception);
  EForeignTaskException = class(Exception);
  EAlreadyAssignedExecutorException = class(Exception);
  ENotAnExecutionException = class(Exception);
  ESomethingWentHorriblyWrongException = class(Exception);
  EAlreadyOneExecutorActiveException = class(Exception);
  EGeneratorFinishedException = class(Exception);

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
    constructor Create(OwnsTasks: Boolean);
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
  private
    FError: Exception;
    FStatus: TExecutionState;
    FExecutor: TExecutor;
    FTerminated: Boolean;
    FFiber: TFiber;
    FNumDependencies: Integer;
    FRequiredStackSize: SizeInt;
  public
    // Dependency management
    function HasDependencies: Boolean; inline;
    procedure DependencyResolved(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
    procedure AddDependency(ADependency: TDependable); {$IFDEF inlining}inline;{$ENDIF}
  protected
    // Scheduling
    procedure SetExecutor(AExecutor: TExecutor); {$IFDEF inlining}inline;{$ENDIF}
    procedure MarkScheduled;
    procedure YieldToScheduler; {$IFDEF inlining}inline;{$ENDIF}
    procedure Wait; {$IFDEF inlining}inline;{$ENDIF}

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
    property RequiredStackSize: SizeInt read FRequiredStackSize;
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

  generic TYieldFunction<TResult> = procedure(const AResult: TResult) of object;

  { TGeneratorEnumerator }

  generic TGeneratorEnumerator<TResult> = record
  private
    FGenerator: IUnknown;
    FCurrent: TResult;
  public
    // use IUnknown because generic forward references make FPC sad
    constructor Create(AGenerator: IUnknown);

    function MoveNext: Boolean; inline;
    property Current: TResult read FCurrent;
  end;

  generic IGenerator<TResult> = interface
    function HasResult: Boolean;
    function ExtractError: Exception;
    procedure StartNextGeneration;
    function GetIndex: SizeInt;
    function GetValue: TResult;

    function GetEnumerator: specialize TGeneratorEnumerator<TResult>;
  end;

  TExceptionBehavior = (ebIgnore, ebRaiseAndTerminate, ebAccumulate);

  { TExecutableHelper }

  TExecutableHelper = class helper for TExecutable
  public
    procedure Await(ATask: TTask; TimeOut: Int64 = -1; FreeTask: Boolean = True); overload;
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult; overload;
    procedure AwaitAll(const Tasks: Array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; TimeOut: Int64 = -1; FreeTasks: Boolean = True);
    generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean; overload;
    generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult; overload; inline;
  end;

  { TExecutionWorkFiber }

  TExecutionWorkFiber = class(TExecutableFiber)
  private
    FExecution: TExecutable;
    FStackSize: SizeInt;
  protected
    procedure Execute; override;
  public
    constructor Create(StackSize: SizeInt);
    destructor Destroy; override;

    property StackSize: SizeInt read FStackSize;
    property Execution: TExecutable read FExecution write FExecution;
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

    TFiberCache = specialize TList<TExecutionWorkFiber>;
    TFiberCacheMap = specialize TDictionary<SizeInt, TFiberCache>;
  private
    FCacheFibers: Boolean;
    FFiberCache: TFiberCacheMap;
    FErrorHandler: TErrorHandler;
    FSchedulingQueue: TSchedulingQueue;
    FSleepQueue: TSleepQueue;
    FCurrentExecution: TExecutable;
    FFiber: TFiber;
    FWaitingCount: Integer;
    FTerminated: Boolean;

    // Fiber cache
    procedure AssignFiber(AExecution: TExecutable); //inline;
    procedure CacheFiber(AExecution: TExecutable); //inline;

    // error handling
    procedure RaiseErrorHandler(ATask: TTask); {$IFDEF inlining}inline;{$ENDIF}

    // Execution
    procedure ContinueExecutable(AExecutable: TExecutable); {$IFDEF inlining}inline;{$ENDIF}

    // Sleeping
    procedure QueueSleep(ATask: TExecutable; ATime: QWord); {$IFDEF inlining}inline;{$ENDIF}
    function HandleSleepQueue: QWord;

    // Waiting
    procedure AddWaitingTask(ATask: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    procedure RemoveWaitingTask(ATask: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    function TasksWaiting: Boolean; {$IFDEF inlining}inline;{$ENDIF}

    // execution loop
    procedure SetupExecution;
    procedure ExecutionLoop;
    procedure TeardownExecution;

  public
    constructor Create(CacheFibers: Boolean = True);
    destructor Destroy; override;

    procedure ScheduleForExecution(AExecutable: TExecutable); {$IFDEF inlining}inline;{$ENDIF}
    procedure Wakeup(AExecutable: TExecutable);

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
    procedure Await(ATask: TTask; TimeOut: Int64 = -1; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
    generic function Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
    procedure AwaitAll(const Tasks: Array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; TimeOut: Int64 = -1; FreeTasks: Boolean = True);
    generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean; overload; inline;
    generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult; overload; inline;
  end;


function GetExecutor: TExecutor; {$IFDEF inlining}inline;{$ENDIF}
function GetCurrentExecution: TExecutable; {$IFDEF inlining}inline;{$ENDIF}
function RunAsync(ATask: TTask; FreeTask: Boolean = True; RaiseErrors: Boolean = True): TTask; {$IFDEF inlining}inline;{$ENDIF}
function ScheduleForAwait(ATask: TTask): TTask; {$IFDEF inlining}inline;{$ENDIF}
procedure Await(ATask: TTask; TimeOut: Int64 = -1; FreeTask: Boolean = True); overload; {$IFDEF inlining}inline;{$ENDIF}
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult; overload; {$IFDEF inlining}inline;{$ENDIF}
procedure AwaitAll(const Tasks: array of TTask; ExceptionBehavior: TExceptionBehavior = ebAccumulate; TimeOut: Int64 = -1; FreeTasks: Boolean = True); {$IFDEF inlining}inline;{$ENDIF}
generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean; overload; inline;
generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult; overload; inline;
procedure AsyncSleep(time: QWord); {$IFDEF inlining}inline;{$ENDIF}

implementation
uses
  stax.generators;

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

constructor EMultiTaskException.Create(OwnsTasks: Boolean);
begin
  inherited Create('One or more tasks threw an exception');
  FExceptions := TTaskExceptionMap.Create;
  FOwnsTasks := OwnsTasks;
end;

constructor EMultiTaskException.Create(ATask: TTask; AException: Exception;
  OwnsTasks: Boolean);
begin
  Create(OwnsTasks);
  FExceptions.Add(ATask, AException);
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
  while True do
  begin
    if Assigned(FExecution) then
      FExecution.DoExecute;
    Return;
  end;
end;

constructor TExecutionWorkFiber.Create(StackSize: SizeInt);
begin
  inherited Create(StackSize);
  FStackSize := StackSize;
  FExecution := nil;
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
  FDepending.Clear;
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

procedure TExecutable.DependencyResolved(ADependency: TDependable);
begin
  if FNumDependencies <= 0 then
    raise ESomethingWentHorriblyWrongException.Create('No dependencies left');
  Dec(FNumDependencies);
  FExecutor.Wakeup(Self);
end;

procedure TExecutable.AddDependency(ADependency: TDependable);
begin
  Inc(FNumDependencies);
  ADependency.AddDepending(Self);
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
  if Assigned(FExecutor) and Assigned(FFiber) then
    FExecutor.CacheFiber(Self);
end;

constructor TExecutable.Create(AStackSize: SizeInt);
begin
  inherited Create;
  FTerminated := False;
  FError := nil;
  FStatus := esNone;
  FExecutor := nil;
  FFiber := nil;
  FRequiredStackSize := AStackSize;
end;

destructor TExecutable.Destroy;
begin
  if Assigned(FFiber) then
    FFiber.Free;
  if Assigned(FError) then
    FError.Free;
  inherited Destroy;
end;

procedure TExecutableHelper.Await(ATask: TTask; TimeOut: Int64;
  FreeTask: Boolean);
var
  EndTime, CurrTime: QWord;
  Err: Exception;
  TimeoutEncountered: Boolean;
  TaskTerminated: ETaskTerminatedException;
begin
  EndTime := GetTickCount64 + TimeOut;
  TaskTerminated := nil;
  TimeoutEncountered := False;
  try
    if FExecutor.FCurrentExecution <> Self then
      raise EExecutionNotActiveException.Create('Only an active task can await');
    if FTerminated then
      raise  EExecutionAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
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
          // We simply wait to be woken up if either the task has already been terminated
          // and we wait for it to finish up, or if we don't have a timeout and wait for it
          // to run
          if (ATask.Terminated) or (TimeOut < 0) then
            Wait
          else
          begin // timeout set: check if we surpassed timeout
            CurrTime := GetTickCount64;
            if CurrTime <= EndTime then // if we have time left
              Self.Sleep(EndTime - CurrTime) // Sleep until woken up or timeout
            else // otherwise terminate task
            begin
              TimeoutEncountered := True;
              ATask.Terminate;
            end;
          end;
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
    // If timeout raise error
    if TimeoutEncountered then
      raise EAwaitTimeoutException.Create('Task timed out during await');
    // Error handling
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

generic function TExecutableHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult;
begin
  try
    Self.Await(ATask, TimeOut, False);
    Result := ATask.TaskResult;
  finally
    if FreeTask then
      ATask.Free;
  end;
end;

procedure TExecutableHelper.AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; TimeOut: Int64; FreeTasks: Boolean);

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
    raise EExecutionNotActiveException.Create('Only an active task can await');
  end;
  // check if we are alive
  if FTerminated then
  begin
    if FreeTasks then FreeAllTasks;
    raise EExecutionAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
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

// terminate all still active tasks and add them to timeout exception
function TerminateTasksAndCreateTimeoutException: EMultiTaskException;
var
  i: Integer;
begin
  Result := EMultiTaskException.Create(FreeTasks);
  for i:=Low(Tasks) to High(Tasks) do
    if (ExceptionBehavior = ebAccumulate) and (Tasks[i].Status = esError) then
      Result.AddException(Tasks[i], Tasks[i].ExtractError)
    else if Tasks[i].Status < esFinished then
    begin
      Tasks[i].Terminate;
      Result.AddException(Tasks[i], EAwaitTimeoutException.Create('Task timed out during await'));
    end;
  if Result.FExceptions.Count = 0 then
    FreeAndNil(Result);
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
  EndTime, CurrTime: QWord;
begin
  TaskTerminated := nil;
  EndTime := GetTickCount64 + TimeOut;
  CheckAssumptions;
  // If already one task raised an exception
  // we might not need to start working in the first place
  AError := nil;
  try
    ScheduleTasks;
    while HasDependencies do
    begin
      // check for timeout
      CurrTime := GetTickCount64;
      if (TimeOut >= 0) and (CurrTime > EndTime) then // timeout encountered, terminate tasks
        AError := TerminateTasksAndCreateTimeoutException;
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
        // We wait if we got terminated or we reached exception/timeout and wait for terminated tasks to finish
        // or we have no timeout set
        if Assigned(TaskTerminated) or Assigned(AError) or (TimeOut < 0) then
          Wait
        else // Timeout set: sleep to timeout or be woken up before
          Self.Sleep(EndTime - CurrTime);
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
    if not Assigned(AError) and
       ((ExceptionBehavior = ebAccumulate) or
       (ExceptionBehavior = ebRaiseAndTerminate)) then
       AError := GetMultiException;

    // If we encountered an error during execution, raise it now
    if Assigned(AError) then
      if ExceptionBehavior = ebIgnore then // unless we ignore all errors
        AError.Free
      else
        raise AError;
  finally
    if FreeTasks then
      if not Assigned(AError) then
        FreeAllTasks
      else
        FreeNonExceptionTasks(AError);
  end;
end;

generic function TExecutableHelper.AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean;
var
  Gen: specialize TGenerator<TResult>;
  Err: Exception;
begin
  Gen := AGenerator as specialize TGenerator<TResult>;
  if Gen.Status >= esFinished then
    Exit(False);
  // Error checks
  if FExecutor.FCurrentExecution <> Self then
    raise EExecutionNotActiveException.Create('Only an active task can await');
  if FTerminated then
    raise  EExecutionAlreadyTerminatedException.Create('Can''t await in an already terminated task. Just finish up!');
  if Assigned(Gen.Executor) and (Gen.Executor <> FExecutor) then
    raise EForeignTaskException.Create('Can''t await a generator from another executor');
  // Schedule generation
  Gen.StartNextGeneration;
  // add as dependency to that task in order to be notified when it finishes
  AddDependency(Gen);
  try
    // Wait until the generator wakes us up or we are terminated
    Wait;
  except on E: ETaskTerminatedException do
  begin
    // If we got terminated, remove ourselves from the dependencies
    // and reraise
    raise E;
  end;
  end;
  // check if we produced a result
  Result := Gen.FHasResult;
  Value := Gen.FGeneratedValue;
  // Error handling
  if Gen.Status = esError then
  begin
    Err := Gen.ExtractError;
    if Err is ETaskTerminatedException then
    begin
      Err.Free;
      raise EAwaitedGeneratorTerminatedException.Create('Awaited generator got terminated before finishing');
    end
    else
      raise err;
  end;
end;

generic function TExecutableHelper.AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult;
begin
  if not specialize AwaitNext<TResult>(AGenerator, Result) then
    raise EGeneratorFinishedException.Create('Generator finished, no next element');
end;

procedure TExecutable.Sleep(Time: QWord);
begin
  if FExecutor.FCurrentExecution <> Self then
    raise EExecutionNotActiveException.Create('Only an active task can go to sleep');
  if FTerminated then
    raise EExecutionAlreadyTerminatedException.Create('Can''t sleep in an already terminated task. Just finish up!');
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
  FExecutor.Wakeup(Self);
end;

{ TRVTask }

procedure TRVTask.Execute;
begin
  FResult := Default(T);
end;

{ TGeneratorEnumerator }

constructor TGeneratorEnumerator.Create(AGenerator: IUnknown);
begin
  FGenerator := AGenerator;
end;

function TGeneratorEnumerator.MoveNext: Boolean;
begin
  Result := specialize AwaitNext<TResult>(specialize IGenerator<TResult>(FGenerator), FCurrent);
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

procedure TExecutor.AssignFiber(AExecution: TExecutable);
var
  Cache: TFiberCache;
  Fiber: TExecutionWorkFiber;
begin
  if FCacheFibers and // if caching is enabled
     FFiberCache.TryGetValue(AExecution.RequiredStackSize, Cache) and // there is a cache for this size
     (Cache.Count > 0) then // and there is a cached element
    Fiber := Cache.ExtractIndex(Cache.Count - 1)
  else
    Fiber := TExecutionWorkFiber.Create(AExecution.RequiredStackSize);
  Fiber.Execution := AExecution;
  AExecution.FFiber := Fiber;
end;

procedure TExecutor.CacheFiber(AExecution: TExecutable);
var
  Cache: TFiberCache;
  Fiber: TExecutionWorkFiber;
begin
  if not FCacheFibers then
    Exit; // will be cleaned up by destructor
  Fiber := TExecutionWorkFiber(AExecution.FFiber);
  if not FFiberCache.TryGetValue(Fiber.StackSize, Cache) then
  begin
    Cache := TFiberCache.Create;
    FFiberCache.Add(Fiber.StackSize, Cache);
  end;
  Cache.Add(Fiber);
  Fiber.Execution := nil;
  AExecution.FFiber := nil;
end;

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

procedure TExecutor.ContinueExecutable(AExecutable: TExecutable);
begin
  if not Assigned(AExecutable.FFiber) then
    AssignFiber(AExecutable);
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

function TExecutor.HandleSleepQueue: QWord;
var
  CurrTime: QWord;
begin
  Result := MaxWaitingTime;
  CurrTime := GetTickCount64;
  while not FSleepQueue.Empty and (FTerminated or (FSleepQueue.First^.First < CurrTime)) do
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
  while (FSchedulingQueue.Count > 0) or not FSleepQueue.Empty or TasksWaiting do
  begin
    SleepTime := HandleSleepQueue;
    if FSchedulingQueue.Count = 0 then
      // wait at most 50 ms
      SysUtils.Sleep(SleepTime)
    else
    begin
      NextExecution := FSchedulingQueue.Extract;
      if FTerminated then
        NextExecution.Terminate;
      ContinueExecutable(NextExecution);
      // if task finished, call the finish within the executors stack
      if NextExecution.Status >= esFinished then
        NextExecution.FinalizeExecution; // will also cache the fiber
    end;
  end;
end;

procedure TExecutor.TeardownExecution;
begin
  if TasksWaiting then
    raise ESomethingWentHorriblyWrongException.Create('Stopped execution while some tasks still waiting... This should never happen');
  FreeAndNil(FFiber);
  CurrentExecutor := nil;
end;

constructor TExecutor.Create(CacheFibers: Boolean);
begin
  FFiberCache := TFiberCacheMap.Create;
  FSchedulingQueue := TSchedulingQueue.Create;
  FSleepQueue := TSleepQueue.Create;
  FCurrentExecution := nil;
  FCacheFibers := CacheFibers;
end;

destructor TExecutor.Destroy;
var
  cache: TFiberCache;
  i: SizeInt;
begin
  // TODO: handle currently active and scheduled tasks
  FSleepQueue.Free;
  FSchedulingQueue.Free;
  // Clear cached items
  for cache in FFiberCache.Values do
  begin
    for i:=0 to Cache.Count - 1 do
      cache[i].Free;
    cache.Free;
  end;
  FFiberCache.Free;
  inherited Destroy;
end;

procedure TExecutor.ScheduleForExecution(AExecutable: TExecutable);
begin
  if not (AExecutable.FStatus in [esNone, esWaiting, esSleeping, esRunning]) then
    Exit;
  AExecutable.MarkScheduled;
  FSchedulingQueue.Enqueue(AExecutable);
end;

procedure TExecutor.Wakeup(AExecutable: TExecutable);
var
  i: SizeInt;
begin
  if not (AExecutable.Status in [esSleeping, esWaiting]) then
    Exit;
  if AExecutable.Status = esSleeping then
    for i := 0 to SizeInt(FSleepQueue.Elements.Size) - 1 do
      if FSleepQueue.Elements.Mutable[i]^.Second = AExecutable then
      begin
        FSleepQueue.Delete(i);
        Break;
      end;
  ScheduleForExecution(AExecutable);
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

procedure TExecutorHelper.Await(ATask: TTask; TimeOut: Int64; FreeTask: Boolean
  );
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only await inside a task');
  FCurrentExecution.Await(ATask, TimeOut, FreeTask);
end;

generic function TExecutorHelper.Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only await inside a task');
  Result := FCurrentExecution.specialize Await<TResult>(ATask, TimeOut, FreeTask);
end;

procedure TExecutorHelper.AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; TimeOut: Int64; FreeTasks: Boolean);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only await inside a task');
  FCurrentExecution.AwaitAll(Tasks, ExceptionBehavior, TimeOut, FreeTasks);
end;

generic function TExecutorHelper.AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only await inside a task');
  Result := FCurrentExecution.specialize AwaitNext<TResult>(AGenerator, Value);
end;

generic function TExecutorHelper.AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult;
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only await inside a task');
  Result := FCurrentExecution.specialize AwaitNext<TResult>(AGenerator);
end;

procedure TExecutor.Sleep(time: QWord);
begin
  if (GetExecutor <> Self) or not Assigned(FCurrentExecution) then
    raise ENotAnExecutionException.Create('Can only sleep inside an asynchronous job');
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
    raise ENotAnExecutionException.Create('No active executor on this thread');
  Result := Executor.CurrentTask;
end;

function RunAsync(ATask: TTask; FreeTask: Boolean; RaiseErrors: Boolean): TTask;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotAnExecutionException.Create('RunAsync can only be called from within a Task');
  Result := Executor.RunAsync(ATask, FreeTask, RaiseErrors);
end;

function ScheduleForAwait(ATask: TTask): TTask;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) then
    raise ENotAnExecutionException.Create('ScheduleForAwait can only be called from within a Task');
  Result := Executor.ScheduleForAwait(ATask);
end;

procedure Await(ATask: TTask; TimeOut: Int64; FreeTask: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('Await can only be called from within a Task');
  Executor.CurrentTask.Await(ATask, TimeOut, FreeTask);
end;

generic function Await<TResult>(ATask: specialize TRVTask<TResult>; TimeOut: Int64 = -1; FreeTask: Boolean = True): TResult;
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('Await can only be called from within a Task');
  Result := Executor.CurrentTask.specialize Await<TResult>(ATask, TimeOut, FreeTask);
end;

procedure AwaitAll(const Tasks: array of TTask;
  ExceptionBehavior: TExceptionBehavior; TimeOut: Int64; FreeTasks: Boolean);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('Await can only be called from within a Task');
  Executor.CurrentTask.AwaitAll(Tasks, ExceptionBehavior, TimeOut, FreeTasks);
end;

generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>; out Value: TResult): Boolean;
 var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('Await can only be called from within a Task');
  Result := Executor.CurrentTask.specialize AwaitNext<TResult>(AGenerator, Value);
end;

generic function AwaitNext<TResult>(AGenerator: specialize IGenerator<TResult>): TResult;
 var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('Await can only be called from within a Task');
  Result := Executor.CurrentTask.specialize AwaitNext<TResult>(AGenerator);
end;

procedure AsyncSleep(time: QWord);
var
  Executor: TExecutor;
begin
  Executor := GetExecutor;
  if not Assigned(Executor) or not Assigned(Executor.CurrentTask) then
    raise ENotAnExecutionException.Create('AsyncSleep can only be called from within a Task');
  Executor.CurrentTask.Sleep(time);
end;

{$EndRegion HelperFunctions}

end.

