program timeout;

{$mode objfpc}{$H+}

uses
  SysUtils, stax, stax.functional;

procedure LongSleep(AExecutor: TExecutor);
begin
  AsyncSleep(10000);
  WriteLn('Sleeped');
end;

procedure TimeoutTest(AExecutor: TExecutor);
var
  StartTime: QWord;
begin
  StartTime := GetTickCount64;
  try
    Await(AsyncProcedure(@LongSleep), 500);
  except on E: EAwaitTimeoutException do
    WriteLn(E.Message);
  end;
  WriteLn(GetTickCount64 - StartTime);
end;

var
  Exec: TExecutor;
begin
  Exec := TExecutor.Create;
  try
    Exec.RunAsync(AsyncProcedure(@TimeoutTest));
    Exec.Run;
  finally
    Exec.Free;
  end;
  ReadLn;
end.

