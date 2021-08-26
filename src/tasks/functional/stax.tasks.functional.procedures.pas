unit stax.tasks.functional.procedures;

{$mode ObjFPC}{$H+}

interface

uses
  stax;

type
  TTaskProcedure =
    procedure(AExecutor: TExecutor);
  TTaskMethodProcedure =
    procedure(AExecutor: TExecutor) of object;
  generic TSingleParamTaskProcedure<TParam1> =
    procedure(AExecutor: TExecutor; AParam1: TParam1);
  generic TSingleParamTaskMethodProcedure<TParam1> =
    procedure(AExecutor: TExecutor; AParam1: TParam1) of object;
  generic TDoubleParamTaskProcedure<TParam1, TParam2> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2);
  generic TDoubleParamTaskMethodProcedure<TParam1, TParam2> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2) of object;
  generic TTripleParamTaskProcedure<TParam1, TParam2, TParam3> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  generic TTripleParamTaskMethodProcedure<TParam1, TParam2, TParam3> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3) of object;
  generic TQuadrupleParamTaskProcedure<TParam1, TParam2, TParam3, TParam4> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  generic TQuadrupleParamTaskMethodProcedure<TParam1, TParam2, TParam3, TParam4> =
    procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4) of object;

  { TProcedureTask }

  TProcedureTask = class(TTask)
  private
    FProcPtr: TTaskProcedure;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TTaskProcedure);
  end;

  { TMethodProcedureTask }

  TMethodProcedureTask = class(TTask)
  private
    FProcPtr: TTaskMethodProcedure;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TTaskMethodProcedure);
  end;

  { TSingleParamProcedureTask }

  generic TSingleParamProcedureTask<TParam1> = class(TTask)
  public type
    TFunctionType = specialize TSingleParamTaskProcedure<TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1);
  end;

  { TSingleParamMethodProcedureTask }

  generic TSingleParamMethodProcedureTask<TParam1> = class(TTask)
  public type
    TFunctionType = specialize TSingleParamTaskMethodProcedure<TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1);
  end;

  { TDoubleParamProcedureTask }

  generic TDoubleParamProcedureTask<TParam1, TParam2> = class(TTask)
  public type
    TFunctionType = specialize TDoubleParamTaskProcedure<TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2);
  end;

  { TDoubleParamMethodProcedureTask }

  generic TDoubleParamMethodProcedureTask<TParam1, TParam2> = class(TTask)
  public type
    TFunctionType = specialize TDoubleParamTaskMethodProcedure<TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2);
  end;

  { TTripleParamProcedureTask }

  generic TTripleParamProcedureTask<TParam1, TParam2, TParam3> = class(TTask)
  public type
    TFunctionType = specialize TTripleParamTaskProcedure<TParam1, TParam2, TParam3>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  end;

  { TTripleParamMethodProcedureTask }

  generic TTripleParamMethodProcedureTask<TParam1, TParam2, TParam3> = class(TTask)
  public type
    TFunctionType = specialize TTripleParamTaskMethodProcedure<TParam1, TParam2, TParam3>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  end;

  { TQuadrupleParamProcedureTask }

  generic TQuadrupleParamProcedureTask<TParam1, TParam2, TParam3, TParam4> = class(TTask)
  public type
    TFunctionType = specialize TQuadrupleParamTaskProcedure<TParam1, TParam2, TParam3, TParam4>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  end;

  { TQuadrupleParamMethodProcedureTask }

  generic TQuadrupleParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4> = class(TTask)
  public type
    TFunctionType = specialize TQuadrupleParamTaskMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  end;

implementation

{ TQuadrupleParamMethodProcedureTask }

procedure TQuadrupleParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor TQuadrupleParamMethodProcedureTask.Create(
  AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2;
  AParam3: TParam3; AParam4: TParam4);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
  FParam4 := AParam4;
end;

{ TQuadrupleParamProcedureTask }

procedure TQuadrupleParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor TQuadrupleParamProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
  FParam4 := AParam4;
end;

{ TTripleParamMethodProcedureTask }

procedure TTripleParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor TTripleParamMethodProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
end;

{ TTripleParamProcedureTask }

procedure TTripleParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor TTripleParamProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam2 := AParam3;
end;

{ TDoubleParamMethodProcedureTask }

procedure TDoubleParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2);
end;

constructor TDoubleParamMethodProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1; AParam2: TParam2);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

{ TDoubleParamProcedureTask }

procedure TDoubleParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2);
end;

constructor TDoubleParamProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1; AParam2: TParam2);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

{ TSingleParamMethodProcedureTask }

procedure TSingleParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1);
end;

constructor TSingleParamMethodProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

{ TSingleParamProcedureTask }

procedure TSingleParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1);
end;

constructor TSingleParamProcedureTask.Create(AProcedure: TFunctionType;
  AParam1: TParam1);
begin
  inherited Create;
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

{ TMethodProcedureTask }

procedure TMethodProcedureTask.Execute;
begin
  FProcPtr(Executor);
end;

constructor TMethodProcedureTask.Create(AProcedure: TTaskMethodProcedure);
begin
  inherited Create;
  FProcPtr := AProcedure;
end;

{ TProcedureTask }

procedure TProcedureTask.Execute;
begin
  FProcPtr(Executor);
end;

constructor TProcedureTask.Create(AProcedure: TTaskProcedure);
begin
  inherited Create;
  FProcPtr := AProcedure;
end;

end.

