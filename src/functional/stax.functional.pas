unit stax.functional;

{$mode objfpc}{$H+}

interface

uses
  Classes, stax, stax.functional.procedures, stax.functional.functions;

function AsyncProcedure(AProcedure: TTaskProcedure): TTask; overload;
function AsyncProcedure(AProcedure: TTaskMethodProcedure): TTask; overload;
generic function AsyncProcedure<TParam1>(
  AProcedure: specialize TSingleParamTaskProcedure<TParam1>;
  AParam1: TParam1): TTask; overload;
generic function AsyncProcedure<TParam1>(
  AProcedure: specialize TSingleParamTaskMethodProcedure<TParam1>;
  AParam1: TParam1): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize TDoubleParamTaskProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize TDoubleParamTaskMethodProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize TTripleParamTaskProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize TTripleParamTaskMethodProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize TQuadrupleParamTaskProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TTask; overload;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize TQuadrupleParamTaskMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TTask; overload;

generic function AsyncFunction<TResult>(
  AFunction: specialize TTaskFunction<TResult>): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult>(
  AFunction: specialize TTaskMethodFunction<TResult>): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize TSingleParamTaskFunction<TResult, TParam1>;
  AParam1: TParam1): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize TSingleParamTaskMethodFunction<TResult, TParam1>;
  AParam1: TParam1): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize TDoubleParamTaskFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize TDoubleParamTaskMethodFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize TTripleParamTaskFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize TTripleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize TQuadrupleParamTaskFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): specialize TRVTask<TResult>; overload;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize TQuadrupleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): specialize TRVTask<TResult>; overload;
implementation

function AsyncProcedure(AProcedure: TTaskProcedure): TTask;
begin
  Result := TProcedureTask.Create(AProcedure);
end;

function AsyncProcedure(AProcedure: TTaskMethodProcedure): TTask;
begin
  Result := TMethodProcedureTask.Create(AProcedure);
end;
generic function AsyncProcedure<TParam1>(
  AProcedure: specialize TSingleParamTaskProcedure<TParam1>;
  AParam1: TParam1): TTask;
begin
  Result := (specialize TSingleParamProcedureTask<TParam1>).Create(AProcedure, AParam1);
end;

generic function AsyncProcedure<TParam1>(
 AProcedure: specialize TSingleParamTaskMethodProcedure<TParam1>;
 AParam1: TParam1): TTask;
begin
  Result := (specialize TSingleParamMethodProcedureTask<TParam1>).Create(AProcedure, AParam1);
end;

generic function AsyncProcedure<TParam1, TParam2>(
AProcedure: specialize TDoubleParamTaskProcedure<TParam1, TParam2>;
AParam1: TParam1; AParam2: TParam2): TTask;
begin
  Result := (specialize TDoubleParamProcedureTask<TParam1, TParam2>).Create(AProcedure, AParam1, AParam2);
end;

generic function AsyncProcedure<TParam1, TParam2>(
AProcedure: specialize TDoubleParamTaskMethodProcedure<TParam1, TParam2>;
AParam1: TParam1; AParam2: TParam2): TTask;
begin
  Result := (specialize TDoubleParamMethodProcedureTask<TParam1, TParam2>).Create(AProcedure, AParam1, AParam2);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3>(
AProcedure: specialize TTripleParamTaskProcedure<TParam1, TParam2, TParam3>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TTask;
begin
  Result := (specialize TTripleParamProcedureTask<TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3);
end;
   
generic function AsyncProcedure<TParam1, TParam2, TParam3>(
AProcedure: specialize TTripleParamTaskMethodProcedure<TParam1, TParam2, TParam3>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TTask;
begin
  Result := (specialize TTripleParamMethodProcedureTask<TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3);
end;
   
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
AProcedure: specialize TQuadrupleParamTaskProcedure<TParam1, TParam2, TParam3, TParam4>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TTask;
begin
  Result := (specialize TQuadrupleParamProcedureTask<TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
AProcedure: specialize TQuadrupleParamTaskMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TTask;
begin
  Result := (specialize TQuadrupleParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4);
end;

generic function AsyncFunction<TResult>(
  AFunction: specialize TTaskFunction<TResult>): specialize TRVTask<TResult>;
begin
  Result := (specialize TFunctionTask<TResult>).Create(AFunction);
end;

generic function AsyncFunction<TResult>(
  AFunction: specialize TTaskMethodFunction<TResult>): specialize TRVTask<TResult>;
begin
  Result := (specialize TMethodFunctionTask<TResult>).Create(AFunction);
end;
generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize TSingleParamTaskFunction<TResult, TParam1>;
  AParam1: TParam1):  specialize TRVTask<TResult>;
begin
  Result := (specialize TSingleParamFunctionTask<TResult, TParam1>).Create(AFunction, AParam1);
end;

generic function AsyncFunction<TResult, TParam1>(
 AFunction: specialize TSingleParamTaskMethodFunction<TResult, TParam1>;
 AParam1: TParam1):  specialize TRVTask<TResult>;
begin
  Result := (specialize TSingleParamMethodFunctionTask<TResult, TParam1>).Create(AFunction, AParam1);
end;

generic function AsyncFunction<TResult, TParam1, TParam2>(
AFunction: specialize TDoubleParamTaskFunction<TResult, TParam1, TParam2>;
AParam1: TParam1; AParam2: TParam2):  specialize TRVTask<TResult>;
begin
  Result := (specialize TDoubleParamFunctionTask<TResult, TParam1, TParam2>).Create(AFunction, AParam1, AParam2);
end;

generic function AsyncFunction<TResult, TParam1, TParam2>(
AFunction: specialize TDoubleParamTaskMethodFunction<TResult, TParam1, TParam2>;
AParam1: TParam1; AParam2: TParam2):  specialize TRVTask<TResult>;
begin
  Result := (specialize TDoubleParamMethodFunctionTask<TResult, TParam1, TParam2>).Create(AFunction, AParam1, AParam2);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
AFunction: specialize TTripleParamTaskFunction<TResult, TParam1, TParam2, TParam3>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3):  specialize TRVTask<TResult>;
begin
  Result := (specialize TTripleParamFunctionTask<TResult, TParam1, TParam2, TParam3>).Create(AFunction, AParam1, AParam2, AParam3);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
AFunction: specialize TTripleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3):  specialize TRVTask<TResult>;
begin
  Result := (specialize TTripleParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3>).Create(AFunction, AParam1, AParam2, AParam3);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
AFunction: specialize TQuadrupleParamTaskFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4):  specialize TRVTask<TResult>;
begin
  Result := (specialize TQuadrupleParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AFunction, AParam1, AParam2, AParam3, AParam4);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
AFunction: specialize TQuadrupleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4):  specialize TRVTask<TResult>;
begin
  Result := (specialize TQuadrupleParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AFunction, AParam1, AParam2, AParam3, AParam4);
end;

end.

