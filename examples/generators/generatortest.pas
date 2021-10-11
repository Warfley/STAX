program generatortest;

{$mode objfpc}{$H+}

uses
  stax, stax.functional, stax.generators;

type

  { TTestGenerator }

  TTestGenerator = class(specialize TGenerator<Integer>)
  protected
    procedure Execute; override;
  end;

{ TTestGenerator }

procedure TTestGenerator.Execute;
begin
  Yield(1);
  Yield(2);
  Yield(3);
end;

procedure GeneratorTest(AExecutor: TExecutor);
var
  i: Integer;
  gen: specialize IGenerator<Integer>;
begin
  gen := TTestGenerator.Create;
  while specialize AwaitNext<Integer>(gen, i) do
    WriteLn('Next: ', i);
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  try
    exec.RunAsync(AsyncProcedure(@GeneratorTest));
    exec.Run;
  finally
    exec.Free;
  end;
  ReadLn;
end.

