unit stax.functional.generators;

{$MODE ObjFpc}{$H+}

interface

uses stax, stax.generators;

type
  generic T0ParamGeneratorProcedure<TResult> = procedure(Yield: specialize TYieldFunction<TResult>);
  generic T0ParamGeneratorMethodProcedure<TResult> = procedure(Yield: specialize TYieldFunction<TResult>) of object;
  generic T1ParamGeneratorProcedure<TResult, TParam1> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1);
  generic T1ParamGeneratorMethodProcedure<TResult, TParam1> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1) of object;
  generic T2ParamGeneratorProcedure<TResult, TParam1, TParam2> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2);
  generic T2ParamGeneratorMethodProcedure<TResult, TParam1, TParam2> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2) of object;
  generic T3ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3);
  generic T3ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3) of object;
  generic T4ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4);
  generic T4ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4) of object;
  generic T5ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5);
  generic T5ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = procedure(Yield: specialize TYieldFunction<TResult>; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5) of object;

  generic T0ParamGeneratorProcedureTask<TResult> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T0ParamGeneratorProcedure<TResult>;
  private
    FProcPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T0ParamGeneratorMethodProcedureTask<TResult> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T0ParamGeneratorMethodProcedure<TResult>;
  private
    FProcPtr: TFunctionType;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamGeneratorProcedureTask<TResult, TParam1> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T1ParamGeneratorProcedure<TResult, TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T1ParamGeneratorMethodProcedureTask<TResult, TParam1> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T1ParamGeneratorMethodProcedure<TResult, TParam1>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamGeneratorProcedureTask<TResult, TParam1, TParam2> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T2ParamGeneratorProcedure<TResult, TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T2ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T2ParamGeneratorMethodProcedure<TResult, TParam1, TParam2>;
  private
    FProcPtr: TFunctionType;
    FParam1: TParam1;
    FParam2: TParam2;
  protected
    procedure Execute; override;
  public
    constructor Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt = DefaultTaskStackSize);
  end;

  generic T3ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T3ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3>;
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

  generic T3ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T3ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3>;
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

  generic T4ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T4ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
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

  generic T4ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T4ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4>;
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

  generic T5ParamGeneratorProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T5ParamGeneratorProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
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

  generic T5ParamGeneratorMethodProcedureTask<TResult, TParam1, TParam2, TParam3, TParam4, TParam5> = class(specialize TGenerator<TResult>)
  public type
    TFunctionType = specialize T5ParamGeneratorMethodProcedure<TResult, TParam1, TParam2, TParam3, TParam4, TParam5>;
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

procedure T0ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield);
end;

constructor T0ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
end;

procedure T0ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield);
end;

constructor T0ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
end;

procedure T1ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1);
end;

constructor T1ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

procedure T1ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1);
end;

constructor T1ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
end;

procedure T2ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2);
end;

constructor T2ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T2ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2);
end;

constructor T2ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
end;

procedure T3ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3);
end;

constructor T3ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T3ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3);
end;

constructor T3ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
end;

procedure T4ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T4ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3, FParam4);
end;

constructor T4ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
end;

procedure T5ParamGeneratorProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamGeneratorProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
begin
  inherited Create(AStackSize);
  FProcPtr := AProcedure;
  FParam1 := AParam1;
  FParam2 := AParam2;
  FParam3 := AParam3;
  FParam4 := AParam4;
  FParam5 := AParam5;
end;

procedure T5ParamGeneratorMethodProcedureTask.Execute;
begin
  FProcPtr(@Yield, FParam1, FParam2, FParam3, FParam4, FParam5);
end;

constructor T5ParamGeneratorMethodProcedureTask.Create(AProcedure: TFunctionType; AParam1: TParam1; AParam2: TParam2; AParam3: TParam3; AParam4: TParam4; AParam5: TParam5; AStackSize: SizeInt);
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