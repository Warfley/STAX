program exceptiontest;

{$mode objfpc}{$H+}

uses
  SysUtils, stax, stax.functional;

type
  ETestException = class(Exception);

procedure ExceptionTest(AExecutor: TExecutor);
begin
  raise ETestException.Create('Test');
end;

procedure ExceptTest(AExecutor: TExecutor);
begin
  try
    Await(AsyncProcedure(@ExceptionTest));
  except on e: ETestException do
    WriteLn('Exception cought: ', E.Message)
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(AsyncProcedure(@ExceptTest));
  exec.Run;
  exec.Free;
  ReadLn;
end.

