program performancetest;

{$mode objfpc}{$H+}

uses
  SysUtils, stax, stax.functional;

function AddFunc(AExecutor: TExecutor; A, B: Integer): Integer;
begin
  Result := A + B;
end;

procedure MeasureFunc(AExecutor: TExecutor);
var
  X: Integer;
  start: QWord;
begin
  start := GetTickCount64;
  X := 0;
  while X < 1000000 do
    X := specialize Await<Integer>(specialize AsyncFunction<Integer, Integer, Integer>(@AddFunc, X, 1));
  WriteLn(GetTickCount64-Start, ' ms');
end;

var
  exec: TExecutor;
begin
  Write('Cache enabled: ');
  exec := TExecutor.Create;
  try
    exec.RunAsync(AsyncProcedure(@MeasureFunc));
    exec.Run;
  finally
    exec.Free;
  end;
  Write('Cache disabled: ');
  exec := TExecutor.Create(False);
  try
    exec.RunAsync(AsyncProcedure(@MeasureFunc));
    exec.Run;
  finally
    exec.Free;
  end;
  ReadLn;
end.

