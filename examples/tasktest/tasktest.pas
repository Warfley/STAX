program tasktest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils, stax, stax.tasks.functional, stax.tasks.io.console;

procedure AsyncWrite(AExecutor: TExecutor; ALine: String);
begin
  WriteLn(ALine);
end;

procedure Echo(AExecutor: TExecutor);
var
  line: String;
begin
  While True do
  begin
    line := specialize Await<String>(AsyncConsoleReadLn(True));
    Await(specialize ProcedureTask<String>(@AsyncWrite, line));
  end;
end;

procedure Counter(AExecutor: TExecutor);
var
  i: Integer;
begin
  i := 0;
  while True do
  begin
    Await(specialize ProcedureTask<String>(@AsyncWrite, i.ToString));
    AExecutor.Sleep(1000);
    Inc(i);
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(ProcedureTask(@Echo));
  exec.RunAsync(ProcedureTask(@Counter));
  exec.Run;
  exec.Free;
  ReadLn;
end.

