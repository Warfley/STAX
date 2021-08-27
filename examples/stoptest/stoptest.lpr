program stoptest;

{$mode objfpc}{$H+}

uses
  stax, stax.tasks.functional;

procedure SleepingFunc(AExecutor: TExecutor);
begin
  AExecutor.Sleep(1000);
end;

procedure Stub(AExecutor: TExecutor);
begin
  WriteLn('Just chilling, doing nothing');
end;

procedure AwaitingFunc(AExecutor: TExecutor);
begin
  AExecutor.Await(AsyncProcedure(@Stub));
end;

procedure StopFunc(AExecutor: TExecutor);
begin
  AExecutor.Terminate;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(AsyncProcedure(@SleepingFunc));
  exec.RunAsync(AsyncProcedure(@AwaitingFunc));
  exec.RunAsync(AsyncProcedure(@StopFunc));
  exec.Run;
  exec.Free;
  ReadLn;
end.

