program generatoriterationtest;

{$mode objfpc}{$H+}

uses
  SysUtils, stax, stax.functional;

procedure IterateDirectory(Yield: specialize TYieldFunction<String>; ADirectory: String);
var
  SearchRec: TSearchRec;
  Entry, SubEntry: string;
begin
  if FindFirst(ADirectory + PathDelim + '*', faAnyFile, SearchRec) = 0 then
  try
    repeat
      if (SearchRec.Name = '.') or (SearchRec.Name = '..') then
        Continue;
      Entry := ADirectory + PathDelim + SearchRec.Name;
      Yield(Entry);
      // recursive descent into directories
      if (SearchRec.Attr and faDirectory) = faDirectory then
        for SubEntry in specialize AsyncGenerator<String, String>(@IterateDirectory, Entry) do
          Yield(SubEntry);
    until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

procedure GeneratorIteratorTest(AExecutor: TExecutor);
var
  dir: String;
begin
  for dir in specialize AsyncGenerator<String, String>(@IterateDirectory, '.') do
    WriteLn(dir);
end;

var
  exec: TExecutor;
begin
  exec := TExecutor.Create;
  try
    exec.RunAsync(AsyncProcedure(@GeneratorIteratorTest));
    exec.Run;
  finally
    exec.Free;
  end;
  ReadLn;
end.

