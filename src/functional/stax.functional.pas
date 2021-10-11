unit stax.functional;

{$MODE ObjFpc}{$H+}

interface

uses stax, stax.functional.procedures, stax.functional.functions, stax.functional.generators;

function AsyncProcedure(
  AProcedure: T0ParamProcedure;
    AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
function AsyncProcedure(
  AProcedure: T0ParamMethodProcedure;
    AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1>(
  AProcedure: specialize T1ParamProcedure<TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1>(
  AProcedure: specialize T1ParamMethodProcedure<TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize T2ParamProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize T2ParamMethodProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamMethodProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;
generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; inline;

generic function AsyncFunction<TResult>(
  AFunction: specialize T0ParamFunction<TResult>;
  
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult>(
  AFunction: specialize T0ParamMethodFunction<TResult>;
  
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize T1ParamFunction<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize T1ParamMethodFunction<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize T2ParamFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize T2ParamMethodFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize T3ParamFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize T3ParamMethodFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize T4ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize T4ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AFunction: specialize T5ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;
generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AFunction: specialize T5ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; inline;

generic function AsyncGenerator<TResult>(
  AProcedure: specialize T0ParamGeneratorProcedure<TResult>;
    AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult>(
  AProcedure: specialize T0ParamGeneratorMethodProcedure<TResult>;
    AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1>(
  AProcedure: specialize T1ParamGeneratorProcedure<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1>(
  AProcedure: specialize T1ParamGeneratorMethodProcedure<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2>(
  AProcedure: specialize T2ParamGeneratorProcedure<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2>(
  AProcedure: specialize T2ParamGeneratorMethodProcedure<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; inline;
implementation

function AsyncProcedure(
  AProcedure: T0ParamProcedure;
    AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := T0ParamProcedureTask.Create(AProcedure, AStackSize);
end;

function AsyncProcedure(
  AProcedure: T0ParamMethodProcedure;
    AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := T0ParamMethodProcedureTask.Create(AProcedure, AStackSize);
end;

generic function AsyncProcedure<TParam1>(
  AProcedure: specialize T1ParamProcedure<TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T1ParamProcedureTask<TParam1>).Create(AProcedure, AParam1, AStackSize);
end;

generic function AsyncProcedure<TParam1>(
  AProcedure: specialize T1ParamMethodProcedure<TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T1ParamMethodProcedureTask<TParam1>).Create(AProcedure, AParam1, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize T2ParamProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T2ParamProcedureTask<TParam1, TParam2>).Create(AProcedure, AParam1, AParam2, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2>(
  AProcedure: specialize T2ParamMethodProcedure<TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T2ParamMethodProcedureTask<TParam1, TParam2>).Create(AProcedure, AParam1, AParam2, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T3ParamProcedureTask<TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamMethodProcedure<TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T3ParamMethodProcedureTask<TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T4ParamProcedureTask<TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T4ParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T5ParamProcedureTask<TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;

generic function AsyncProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): TTask; 
begin
  Result := (specialize T5ParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;


generic function AsyncFunction<TResult>(
  AFunction: specialize T0ParamFunction<TResult>;
  
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T0ParamFunctionTask<TResult>).Create(AFunction, AStackSize);
end;

generic function AsyncFunction<TResult>(
  AFunction: specialize T0ParamMethodFunction<TResult>;
  
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T0ParamMethodFunctionTask<TResult>).Create(AFunction, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize T1ParamFunction<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T1ParamFunctionTask<TResult, TParam1>).Create(AFunction, AParam1, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1>(
  AFunction: specialize T1ParamMethodFunction<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T1ParamMethodFunctionTask<TResult, TParam1>).Create(AFunction, AParam1, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize T2ParamFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T2ParamFunctionTask<TResult, TParam1, TParam2>).Create(AFunction, AParam1, AParam2, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2>(
  AFunction: specialize T2ParamMethodFunction<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T2ParamMethodFunctionTask<TResult, TParam1, TParam2>).Create(AFunction, AParam1, AParam2, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize T3ParamFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T3ParamFunctionTask<TResult, TParam1, TParam2, TParam3>).Create(AFunction, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3>(
  AFunction: specialize T3ParamMethodFunction<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T3ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3>).Create(AFunction, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize T4ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T4ParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AFunction, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4>(
  AFunction: specialize T4ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T4ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AFunction, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AFunction: specialize T5ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T5ParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AFunction, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;

generic function AsyncFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AFunction: specialize T5ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; 
begin
  Result := (specialize T5ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AFunction, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;


generic function AsyncGenerator<TResult>(
  AProcedure: specialize T0ParamGeneratorProcedure<TResult>;
    AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T0ParamGeneratorProcedureTask<TResult>).Create(AProcedure, AStackSize);
end;

generic function AsyncGenerator<TResult>(
  AProcedure: specialize T0ParamGeneratorMethodProcedure<TResult>;
    AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T0ParamGeneratorMethodProcedureTask<TResult>).Create(AProcedure, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1>(
  AProcedure: specialize T1ParamGeneratorProcedure<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T1ParamGeneratorProcedureTask<TResult, TParam1>).Create(AProcedure, AParam1, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1>(
  AProcedure: specialize T1ParamGeneratorMethodProcedure<TResult, TParam1>;
  AParam1: TParam1; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T1ParamGeneratorMethodProcedureTask<TResult, TParam1>).Create(AProcedure, AParam1, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2>(
  AProcedure: specialize T2ParamGeneratorProcedure<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T2ParamGeneratorProcedureTask<TResult, TParam1, TParam2>).Create(AProcedure, AParam1, AParam2, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2>(
  AProcedure: specialize T2ParamGeneratorMethodProcedure<TResult, TParam1, TParam2>;
  AParam1: TParam1; AParam2: TParam2; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T2ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2>).Create(AProcedure, AParam1, AParam2, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T3ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3>(
  AProcedure: specialize T3ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T3ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3>).Create(AProcedure, AParam1, AParam2, AParam3, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T4ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4>(
  AProcedure: specialize T4ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T4ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T5ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;

generic function AsyncGenerator<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>(
  AProcedure: specialize T5ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; 
  AStackSize: SizeInt = DefaultTaskStackSize): specialize IGenerator<TResult>; 
begin
  Result := (specialize T5ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>).Create(AProcedure, AParam1, AParam2, AParam3, AParam4, AParam5, AStackSize);
end;

end.