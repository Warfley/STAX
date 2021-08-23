unit stax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, stax.helpertypes, barrier;

type

  TWorkerThread = class;
  TExecutor = class;

  TTaskStatus = (tsNone=0, tsScheduled, tsRunning, tsFinished, tsError);

  { TTask }

  TTask = class
  private
    FError: Exception;
    FStatus: TTaskStatus;
    FExecutor: TExecutor;
    FWorkerThread: TWorkerThread;
  protected
    procedure Execute; virtual; abstract;
  public
    constructor Create;

    procedure Run(WorkerThread: TWorkerThread);

    property Error: Exception read FError;
    property Status: TTaskStatus read FStatus;
    property Executor: TExecutor read FExecutor;
    property WorkerThread: TWorkerThread read FWorkerThread;
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

  TWorkerStatus = (wsIdle, wsRunning, wsPaused);
  EWorkerError = class(Exception);
  EWorkerTerminatedException = class(Exception);

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FBarrier: TBarrier;
    FStatus: TWorkerStatus;
    FTask: TTask;
    FExecutor: TExecutor;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecutor: TExecutor);
    destructor Destroy; override;

    procedure RunTask(ATask: TTask);
    procedure Yield;
    procedure Sleep(time: QWord);
    procedure ResumeTask;

    procedure Stop;

    property Status: TWorkerStatus read FStatus;
    property Task: TTask read FTask;
    property Executor: TExecutor read FExecutor;
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

  TWorkerList = specialize TList<TWorkerThread>;

  ETaskAlreadyScheduledException = class(Exception);
  ENotATaskException = class(Exception);

  { TExecutor }

  TExecutor = class
  private
    FErrorHandler: TErrorHandler;
    FTaskQueue: TTaskQueue;
    FWorkers: TWorkerList;

    procedure RaiseErrorHandler(Task: TTask); inline;
    procedure ScheduleTask(constref QueueEntry: TTaskQueueEntry);
    function GetFreeWorker: TWorkerThread;
    procedure ExecTask(ATask: TTask);
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
  end;


function GetExecutor: TExecutor; inline;
procedure Await(ATask: TTask; FreeTask: Boolean = True);
generic function Await<TResult>(ATask: specialize TRVTask<TResult>; FreeTask: Boolean = True): TResult;
procedure Yield;
procedure AsyncSleep(time: QWord);

implementation

generic

function GetExecutor: TExecutor;
begin
  Result := nil;
  if TThread.CurrentThread is TWorkerThread then
    Result := TWorkerThread(TThread.CurrentThread).Executor;
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

{ TWorkerThread }

procedure TWorkerThread.Execute;
begin
  while not Terminated do
  begin
    // wait for next task
    FStatus := wsIdle;
    FBarrier.Enter;
    // check if terminated during waiting
    if Terminated then
      break;
    // Work task
    FStatus := wsRunning;
    FTask.Run(Self);
    FTask := nil;
    // After finishing release scheduler
    FBarrier.Enter;
  end;
end;

constructor TWorkerThread.Create(AExecutor: TExecutor);
begin
  inherited Create(True);
  FExecutor := AExecutor;
  FBarrier.Initialize(2);
  FTask := nil;
  FreeOnTerminate:=True;
  Start;
end;

destructor TWorkerThread.Destroy;
begin
  FBarrier.Destroy;
  inherited Destroy;
end;

procedure TWorkerThread.RunTask(ATask: TTask);
begin
  if Assigned(FTask) or (FStatus <> wsIdle) then
    raise EWorkerError.Create('Can''t schedule new task before last one finished');
  if TThread.CurrentThread.ThreadID = Self.ThreadID then
    raise EWorkerError.Create('Only the scheduler is allowed to schedule tasks');
  // setup task
  FTask := ATask;
  // Release worker
  FBarrier.Enter;
  // Lock scheduler
  FBarrier.Enter;
end;

procedure TWorkerThread.Yield;
begin
  if TThread.CurrentThread.ThreadID <> Self.ThreadID then
    raise EWorkerError.Create('Can only yield from within the task executed on this thread');
  FStatus := wsPaused;
  // Release scheduler
  FBarrier.Enter;
  // Lock until rescheduled
  FBarrier.Enter;
  // check if terminated during the waiting
  if Terminated then
    raise EWorkerTerminatedException.Create('Worker terminated during yield');
end;

procedure TWorkerThread.Sleep(time: QWord);
var
  StartTime: QWord;
begin
  StartTime := GetTickCount64;
  repeat
    Yield;
  until (GetTickCount64 - StartTime) > time;
end;

procedure TWorkerThread.ResumeTask;
begin
  if not Assigned(FTask) or (FStatus <> wsPaused) then
    raise EWorkerError.Create('No paused task to be resumed');
  if TThread.CurrentThread.ThreadID = Self.ThreadID then
    raise EWorkerError.Create('Only the scheduler is allowed to resume tasks');
  FStatus := wsRunning;
  // Release worker
  FBarrier.Enter;
  // Lock scheduler
  FBarrier.Enter;
end;

procedure TWorkerThread.Stop;
begin
  Terminate;
  if FStatus = wsIdle then
    // Release worker
    FBarrier.Enter;
end;

{ EUnhandledError }

constructor EUnhandledError.Create(AError: Exception; ATask: TTask);
begin
  Error := AError;
  Task := ATask;
end;

{ TExecutor }

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
  QueueEntry.Task.FStatus := tsScheduled;
end;

function TExecutor.GetFreeWorker: TWorkerThread;
begin
  if FWorkers.Count > 0 then
    Result := FWorkers.ExtractIndex(FWorkers.Count - 1)
  else
    Result := TWorkerThread.Create(Self);
end;

procedure TExecutor.ExecTask(ATask: TTask);
begin
  if Assigned(ATask.WorkerThread) then
    ATask.WorkerThread.ResumeTask
  else
    GetFreeWorker.RunTask(ATask);
end;

procedure TExecutor.FinalizeTask(ATask: TTaskQueueEntry);
begin
  // Reuse worker
  FWorkers.Add(ATask.Task.WorkerThread);
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
  FWorkers := TWorkerList.Create;
end;

destructor TExecutor.Destroy;
var
  Worker: TWorkerThread;
begin
  FTaskQueue.Free;
  for worker in FWorkers do
  begin
    Worker.Stop;
  end;
  FWorkers.Free;
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
var
  Worker: TWorkerThread;
begin
  if not (TThread.CurrentThread is TWorkerThread) then
    raise ENotATaskException.Create('Can only await inside a task');
  try
    Worker := TWorkerThread(TThread.CurrentThread);
    // schedule task to await
    RunAsync(ATask, False, False);
    // while not finished, yield to the scheduler to continue the q
    while ATask.Status < tsFinished do
      Worker.Yield;
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
  if not (TThread.CurrentThread is TWorkerThread) then
    raise EWorkerError.Create('Can only yield inside an asynchronous job');
  TWorkerThread(TThread.CurrentThread).Yield;
end;

procedure TExecutor.Sleep(time: QWord);
begin
  if not (TThread.CurrentThread is TWorkerThread) then
    raise EWorkerError.Create('Can only sleep inside an asynchronous job');
  TWorkerThread(TThread.CurrentThread).Sleep(time);
end;

procedure TExecutor.Run;
var
  NextTask: TTaskQueueEntry;
begin
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
end;

{ TRVTask }

procedure TRVTask.Execute;
begin
  FResult := Default(T);
end;

{ TTask }

constructor TTask.Create;
begin
  FError := nil;
  FStatus := tsNone;
  FExecutor := nil;
end;

procedure TTask.Run(WorkerThread: TWorkerThread);
begin
  FWorkerThread := WorkerThread;
  FStatus := tsRunning;
  try
    Execute;  
    FStatus := tsFinished;
  except on E: Exception do begin
    FError := E;
    FStatus := tsError;
  end
  end;
end;

end.

