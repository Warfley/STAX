program tasktest;

{$mode objfpc}{$H+}

uses
  SysUtils, Stax, Stax.tasks.functional;

procedure AsyncWrite(AExecutor: TExecutor; ALine: String);
begin
  WriteLn(ALine);
end;

function AsyncConcat(AExecutor: TExecutor; AName: String; ANumber: Integer): String;
begin
  Result := '%s: %d'.Format([AName, ANumber]);
end;

procedure AsyncCount(AExecutor: TExecutor; AName: String);
var
  i: Integer;
  line: String;
begin
  for i:=0 to 10 do
  begin
    Line := specialize Await<String>(specialize AsyncFunction<String, String, Integer>(@AsyncConcat, AName, i));
    Await(specialize AsyncProcedure<String>(@AsyncWrite, Line));
  end;
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  exec.RunAsync(specialize AsyncProcedure<String>(@AsyncCount, 'C1'));
  exec.RunAsync(specialize AsyncProcedure<String>(@AsyncCount, 'C2'));
  exec.Run;
  exec.Free;
  ReadLn;
end.
