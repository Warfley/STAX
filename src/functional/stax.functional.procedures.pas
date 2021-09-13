unit stax.functional.procedures;

{$MODE ObjFpc}{$H+}

interface

uses stax;

type
  T0ParamProcedure = procedure(AExecutor: TExecutor);
  T0ParamMethodProcedure = procedure(AExecutor: TExecutor) of object;
  generic T1ParamProcedure<TParam1> = procedure(AExecutor: TExecutor; AParam1: TParam1);
  generic T1ParamMethodProcedure<TParam1> = procedure(AExecutor: TExecutor; AParam1: TParam1) of object;
  generic T2ParamProcedure<TParam1, TParam2> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2);
  generic T2ParamMethodProcedure<TParam1, TParam2> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2) of object;
  generic T3ParamProcedure<TParam1, TParam2, TParam3> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  generic T3ParamMethodProcedure<TParam1, TParam2, TParam3> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3) of object;
  generic T4ParamProcedure<TParam1, TParam2, TParam3, TParam4> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  generic T4ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4) of object;
  generic T5ParamProcedure<TParam1, TParam2, TParam3, TParam4, TParam5> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5);
  generic T5ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4, TParam5> = procedure(AExecutor: TExecutor; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5) of object;

  T0ParamProcedureTask = class(TTask)
  public type
    TFunctionType = T0ParamProcedure;
  private
    FProcPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  T0ParamMethodProcedureTask = class(TTask)
  public type
    TFunctionType = T0ParamMethodProcedure;
  private
    FProcPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamProcedureTask<TParam1> = class(TTask)
  public type
    TFunctionType = specialize T1ParamProcedure<TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamMethodProcedureTask<TParam1> = class(TTask)
  public type
    TFunctionType = specialize T1ParamMethodProcedure<TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamProcedureTask<TParam1, TParam2> = class(TTask)
  public type
    TFunctionType = specialize T2ParamProcedure<TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamMethodProcedureTask<TParam1, TParam2> = class(TTask)
  public type
    TFunctionType = specialize T2ParamMethodProcedure<TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T3ParamProcedureTask<TParam1, TParam2, TParam3> = class(TTask)
  public type
    TFunctionType = specialize T3ParamProcedure<TParam1, TParam2, TParam3>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T3ParamMethodProcedureTask<TParam1, TParam2, TParam3> = class(TTask)
  public type
    TFunctionType = specialize T3ParamMethodProcedure<TParam1, TParam2, TParam3>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T4ParamProcedureTask<TParam1, TParam2, TParam3, TParam4> = class(TTask)
  public type
    TFunctionType = specialize T4ParamProcedure<TParam1, TParam2, TParam3, TParam4>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T4ParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4> = class(TTask)
  public type
    TFunctionType = specialize T4ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T5ParamProcedureTask<TParam1, TParam2, TParam3, TParam4, TParam5> = class(TTask)
  public type
    TFunctionType = specialize T5ParamProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
    FParam5: TParam5;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T5ParamMethodProcedureTask<TParam1, TParam2, TParam3, TParam4, TParam5> = class(TTask)
  public type
    TFunctionType = specialize T5ParamMethodProcedure<TParam1, TParam2, TParam3, TParam4, TParam5>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
    FParam3: TParam3;
    FParam4: TParam4;
    FParam5: TParam5;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

implementation

procedure T0ParamProcedureTask.Execute;
begin
  FProcPtr(Executor);
end;

constructor T0ParamProcedureTask.Create(AProcedure: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
end;

procedure T0ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor);
end;

constructor T0ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
end;

procedure T1ParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1);
end;

constructor T1ParamProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

procedure T1ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1);
end;

constructor T1ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

procedure T2ParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2);
end;

constructor T2ParamProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T2ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2);
end;

constructor T2ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T3ParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor T3ParamProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T3ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3);
end;

constructor T3ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T4ParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T4ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T5ParamProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
  FParam5 := AParam5;
end;

procedure T5ParamMethodProcedureTask.Execute;
begin
  FProcPtr(Executor, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
  FParam5 := AParam5;
end;

end.