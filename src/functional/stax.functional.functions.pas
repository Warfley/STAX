unit stax.functional.functions;

{$mode objfpc}{$H+}

interface

uses
  stax;

type
  generic TTaskFunction<TResult> =
    function(AExecutor: TExecutor): TResult;
  generic TTaskMethodFunction<TResult> =
    function(AExecutor: TExecutor): TResult of object;
  generic TSingleParamTaskFunction<TResult, TParam1> =
    function(AExecutor: TExecutor; AParam1: TParam1): TResult;
  generic TSingleParamTaskMethodFunction<TResult, TParam1> =
    function(AExecutor: TExecutor; AParam1: TParam1): TResult of object;
  generic TDoubleParamTaskFunction<TResult, TParam1, TParam2> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2): TResult;
  generic TDoubleParamTaskMethodFunction<TResult, TParam1, TParam2> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2): TResult of object;
  generic TTripleParamTaskFunction<TResult, TParam1, TParam2, TParam3> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult;
  generic TTripleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3): TResult of object;
  generic TQuadrupleParamTaskFunction<TResult, TParam1, TParam2, TParam3, TParam4> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TResult;
  generic TQuadrupleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4> =
    function(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4): TResult of object;

  { TFunctionTask }

  generic TFunctionTask<TResult> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TTaskFunction<TResult>;
  private
    FFunPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType);
  end;

  { TMethodFunctionTask }

  generic TMethodFunctionTask<TResult> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TTaskMethodFunction<TResult>;
  private
    FFunPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType);
  end;

  { TSingleParamFunctionTask }

  generic TSingleParamFunctionTask<TResult, TParam1> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TSingleParamTaskFunction<TResult, TParam1>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1);
  end;

  { TSingleParamMethodFunctionTask }

  generic TSingleParamMethodFunctionTask<TResult, TParam1> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TSingleParamTaskMethodFunction<TResult, TParam1>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1);
  end;

  { TDoubleParamFunctionTask }

  generic TDoubleParamFunctionTask<TResult, TParam1, TParam2> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TDoubleParamTaskFunction<TResult, TParam1, TParam2>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2);
  end;

  { TDoubleParamMethodFunctionTask }

  generic TDoubleParamMethodFunctionTask<TResult, TParam1, TParam2> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TDoubleParamTaskMethodFunction<TResult, TParam1, TParam2>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2);
  end;

  { TTripleParamFunctionTask }

  generic TTripleParamFunctionTask<TResult, TParam1, TParam2, TParam3> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TTripleParamTaskFunction<TResult, TParam1, TParam2, TParam3>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  end;

  { TTripleParamMethodFunctionTask }

  generic TTripleParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TTripleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  end;

  { TQuadrupleParamFunctionTask }

  generic TQuadrupleParamFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TQuadrupleParamTaskFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  end;

  { TQuadrupleParamMethodFunctionTask }

  generic TQuadrupleParamMethodFunctionTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TRVTask<TResult>)
  public type
    TFunctionType = specialize TQuadrupleParamTaskMethodFunction<TResult, TParam1, TParam2, TParam3, TParam4>;
  private
    FFunPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  end;

implementation

{ TQuadrupleParamMethodFunctionTask }

procedure TQuadrupleParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor TQuadrupleParamMethodFunctionTask.Create(
  AFunction: TFunctionType; AParam1: TParam1; AParam2: TParam2;
  AParam3: TParam3; AParam4: TParam4);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
  FParam4 := AParam4;
end;

{ TQuadrupleParamFunctionTask }

procedure TQuadrupleParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor TQuadrupleParamFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
  FParam4 := AParam4;
end;

{ TTripleParamMethodFunctionTask }

procedure TTripleParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor TTripleParamMethodFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
end;

{ TTripleParamFunctionTask }

procedure TTripleParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor TTripleParamFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
end;

{ TDoubleParamMethodFunctionTask }

procedure TDoubleParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2);
end;

constructor TDoubleParamMethodFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1; AParam2: TParam2);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

{ TDoubleParamFunctionTask }

procedure TDoubleParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1, FParam2);
end;

constructor TDoubleParamFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1; AParam2: TParam2);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

{ TSingleParamMethodFunctionTask }

procedure TSingleParamMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1);
end;

constructor TSingleParamMethodFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
end;

{ TSingleParamFunctionTask }

procedure TSingleParamFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor, FParam1);
end;

constructor TSingleParamFunctionTask.Create(AFunction: TFunctionType;
  AParam1: TParam1);
begin
  inherited Create;
  FFunPtr := AFunction;
  FParam1 := AParam1;
end;

{ TMethodFunctionTask }

procedure TMethodFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor);
end;

constructor TMethodFunctionTask.Create(AFunction: TFunctionType);
begin
  inherited Create;
  FFunPtr := AFunction;
end;

{ TFunctionTask }

procedure TFunctionTask.Execute;
begin
  FResult := FFunPtr(Executor);
end;

constructor TFunctionTask.Create(AFunction: TFunctionType);
begin
  inherited Create;
  FFunPtr := AFunction;
end;

end.

