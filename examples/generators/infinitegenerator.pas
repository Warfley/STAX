program infinitegenerator;

{$mode objfpc}{$H+}

uses
  stax, stax.functional;

procedure InfiniteGenerator(Yield: specialize TYieldFunction<String>);
begin
  while True do
    Yield('Hello World!');
end;

procedure InfiniteGeneratorTest(AExecutor: TExecutor);
var
  Msg: String;
  i: Integer;
begin
  i := 0;
  for Msg in specialize AsyncGenerator<String>(@InfiniteGenerator) do
  begin
    WriteLn(Msg);
    Inc(i);
    if i >= 3 then
      Exit;
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  try
    exec.RunAsync(AsyncProcedure(@InfiniteGeneratorTest));
    exec.Run;
  finally
    exec.Free;
  end;
  ReadLn;
end.


