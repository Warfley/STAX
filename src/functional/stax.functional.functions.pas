unit stax.functional.functions;

{$MODE ObjFpc}{$H+}

interface

uses stax;

type
  generic T0ParamFunction<TResult> = function(AExecutor: TExecutor): TResult;
  generic T0ParamMethodFunction<TResult> = function(AExecutor: TExecutor): TResult of object;
  generic T1ParamFunction<TResult, TParam1> = function(AExecutor: TExecutor; AParam1: TParam1): TResult;
  generic T1ParamMethodFunction<TResult, TParam1> = function(AExecutor: TExecutor; AParam1: TParam1): TResult of object;
  generic T2ParamFunction<TResult, TParam1, TParam2> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2): TResult;
  generic T2ParamMethodFunction<TResult, TParam1, TParam2> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2): TResult of object;
  generic T3ParamFunction<TResult, TParam1, TParam2, TParam3> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult;
  generic T3ParamMethodFunction<TResult, TParam1, TParam2, TParam3> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult of object;
  generic T4ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TResult;
  generic T4ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TResult of object;
  generic T5ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5): TResult;
  generic T5ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5): TResult of object;

  generic T0ParamFunctionTask<TResult> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T0ParamFunction<TResult>;
  private
    FFunPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T0ParamMethodFunctionTask<TResult> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T0ParamMethodFunction<TResult>;
  private
    FFunPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamFunctionTask<TResult, TParam1> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T1ParamFunction<TResult, TParam1>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamMethodFunctionTask<TResult, TParam1> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T1ParamMethodFunction<TResult, TParam1>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamFunctionTask<TResult, TParam1, TParam2> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T2ParamFunction<TResult, TParam1, TParam2>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamMethodFunctionTask<TResult, TParam1, TParam2> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T2ParamMethodFunction<TResult, TParam1, TParam2>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T3ParamFunctionTask<TResult, TParam1, TParam2, TParam3> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T3ParamFunction<TResult, TParam1, TParam2, TParam3>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T3ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T3ParamMethodFunction<TResult, TParam1, TParam2, TParam3>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T4ParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T4ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T4ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T4ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T5ParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T5ParamFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
    FParam5: TParam5;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T5ParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize T5ParamMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
    FParam5: TParam5;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

implementation

procedure T0ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor);
end;

constructor T0ParamFunctionTask.Create(AFunction: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
end;

procedure T0ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor);
end;

constructor T0ParamMethodFunctionTask.Create(AFunction: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
end;

procedure T1ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1);
end;

constructor T1ParamFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
end;

procedure T1ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1);
end;

constructor T1ParamMethodFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
end;

procedure T2ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2);
end;

constructor T2ParamFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T2ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2);
end;

constructor T2ParamMethodFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T3ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor T3ParamFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T3ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor T3ParamMethodFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T4ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T4ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamMethodFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T5ParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
  FParam5 := AParam5;
end;

procedure T5ParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamMethodFunctionTask.Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
  FParam5 := AParam5;
end;

end.