program awaitalltest;

{$mode objfpc}{$H+}

uses
  SysUtils, stax, stax.functional;

procedure ExceptionThrowingTask(AExecutor: TExecutor);
begin
  raise Exception.Create('Test');
end;

procedure SleepAndExceptionThrowingTask(AExecutor: TExecutor);
begin
  AsyncSleep(1000);
  raise Exception.Create('Test');
end;

procedure CountingTask(AExecutor: TExecutor);
var
  i: Integer;
begin
  for i := 1 to 10 do
  begin
    WriteLn(i);
    AsyncSleep(1000);
  end;
end;

procedure AwaitAllTask(AExecutor: TExecutor);
var
  Ex: Exception;
begin
  WriteLn('Accumulate');
  try
    AwaitAll([
      AsyncProcedure(@ExceptionThrowingTask),
      AsyncProcedure(@SleepAndExceptionThrowingTask),
      AsyncProcedure(@CountingTask)
    ]);
  except on E: EMultiTaskException do
    for Ex in E.Exceptions.Values do
      WriteLn(Ex.Message);
  end;
  WriteLn('RaiseAndTerminate');
  try
    AwaitAll([
      AsyncProcedure(@ExceptionThrowingTask),
      AsyncProcedure(@SleepAndExceptionThrowingTask),
      AsyncProcedure(@CountingTask)
    ], ebRaiseAndTerminate);
  except on E: EMultiTaskException do
    for Ex in E.Exceptions.Values do
      WriteLn(Ex.Message);
  end;
  WriteLn('Ignore');
  AwaitAll([
    AsyncProcedure(@ExceptionThrowingTask),
    AsyncProcedure(@SleepAndExceptionThrowingTask),
    AsyncProcedure(@CountingTask)
  ], ebIgnore);
  WriteLn('Timeout');
  try
    AwaitAll([
      AsyncProcedure(@ExceptionThrowingTask),
      AsyncProcedure(@SleepAndExceptionThrowingTask),
      AsyncProcedure(@CountingTask)
    ], ebAccumulate, 500);
  except on E: EMultiTaskException do
    for Ex in E.Exceptions.Values do
      WriteLn(Ex.Message);
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(AsyncProcedure(@AwaitAllTask));
  exec.Run;
  exec.Free;
  ReadLn;
end.

