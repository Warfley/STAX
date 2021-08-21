unit stax;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Generics.Collections, stax.helpertypes, barrier;

type

  TWorkerThread = class;
  TExecutor = class;

  TTaskStatus = (stNone, stScheduled, stRunning, stFinished, stError);

  { TTask }

  TTask = class
  private
    FError: Exception;
    FStatus: TTaskStatus;
    FExecutor: TExecutor;
    FWorkerThread: TWorkerThread;
  public
    constructor Create(AExecutor: TExecutor);

    procedure Run(WorkerThread: TWorkerThread);
    procedure Execute; virtual; abstract;

    property Error: Exception read FError;
    property Status: TTaskStatus read FStatus;
    property Executor: TExecutor read FExecutor;
  end;

  { TRVTask }

  generic TRVTask<T> = class(TTask)
  protected
    Result: T;
  public
    procedure Execute; override;
    property TaskResult: T read Result;
  end;

  TWorkerStatus = (wsIdle, wsRunning, wsPaused);
  EWorkerError = class(Exception);

  { TWorkerThread }

  TWorkerThread = class(TThread)
  private
    FBarrier: TBarrier;
    FStatus: TWorkerStatus;
    FTask: TTask;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;

    procedure RunTask(ATask: TTask);
    procedure Yield;
    procedure ResumeTask;

    procedure Stop;

    property Status: TWorkerStatus read FStatus;
    property Task: TTask read FTask;
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
  ETaskAlreadyScheduledException = class(Exception);

  { TExecutor }

  TExecutor = class
  private
    FErrorHandler: TErrorHandler;

    procedure RaiseErrorHandler(Task: TTask); inline;
  public
    property OnError: TErrorHandler read FErrorHandler write FErrorHandler;
  end;

implementation

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

constructor TWorkerThread.Create;
begin
  FBarrier.Initialize(2);
  FTask := nil;
  inherited Create(False);
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

{ TRVTask }

procedure TRVTask.Execute;
begin
  Self.Result := Default(T);
end;

{ TTask }

constructor TTask.Create(AExecutor: TExecutor);
begin
  FError := nil;
  FStatus := stNone;
  FExecutor := AExecutor;
end;

procedure TTask.Run(WorkerThread: TWorkerThread);
begin
  FWorkerThread := WorkerThread;
  FStatus := stRunning;
  try
    Execute;  
    FStatus := stFinished;
  except on E: Exception do begin
    FError := E;
    FStatus := stError;
  end
  end;
end;

end.

