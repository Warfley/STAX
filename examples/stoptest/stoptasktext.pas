program stoptasktext;

{$mode objfpc}{$H+}

uses
  stax, stax.functional;

procedure SleepingTask(AExecutor: TExecutor);
begin
  WriteLn('Start sleeping');
  AsyncSleep(1000);
  WriteLn('Sleeped');
end;

procedure AwaitingTask(AExecutor: TExecutor);
begin
  WriteLn('Start Awaiting');
  Await(AsyncProcedure(@SleepingTask));
  WriteLn('Awaited');
end;

procedure YieldTask(AExecutor: TExecutor);
begin
  WriteLn('Start Yield');
  AsyncSleep(0);
  WriteLn('Yielded');
end;

procedure AwaitingAllTask(AExecutor: TExecutor);
begin  
  WriteLn('Start Awaiting all');
  AwaitAll([AsyncProcedure(@AwaitingTask), AsyncProcedure(@YieldTask)]);
  WriteLn('Awaited all');
end;

procedure KillTask(AExecutor: TExecutor; ATask: TTask);
begin
  ATask.Terminate;
end;

var
  exec: TExecutor;
  t: TTask;
begin
  exec := TExecutor.Create;
  t := AsyncProcedure(@AwaitingAllTask);
  exec.RunAsync(t);
  exec.RunAsync(specialize AsyncProcedure<TTask>(@KillTask, t));
  exec.Run;
  exec.Free;
  ReadLn;
end.

