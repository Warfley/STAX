program tasktest;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,{$ENDIF}
  Classes, SysUtils, stax;

type

  { TWritingTask }

  TWritingTask = class(TTask)
  private
    FLine: String;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecutor: TExecutor; ALine: String);
  end;

  { TCountingTask }

  TCountingTask = class(TTask)
  private
    FName: String;
  protected
    procedure Execute; override;
  public
    constructor Create(AExecutor: TExecutor; AName: String);
  end;

{ TCountingTask }

procedure TCountingTask.Execute;
var
  i: Integer;
begin
  for i:=0 to 10 do
    Executor.Await(TWritingTask.Create(Executor, '%s: %d'.Format([FName, i])));
end;

constructor TCountingTask.Create(AExecutor: TExecutor; AName: String);
begin
  inherited Create(AExecutor);
  FName := AName;
end;

{ TWritingTask }

procedure TWritingTask.Execute;
begin
  WriteLn(FLine);
end;

constructor TWritingTask.Create(AExecutor: TExecutor; ALine: String);
begin
  inherited Create(AExecutor);
  FLine := ALine;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(TCountingTask.Create(exec, 'C1'));
  exec.RunAsync(TCountingTask.Create(exec, 'C2'));
  exec.Run;
  exec.Free;
  ReadLn;
end.

