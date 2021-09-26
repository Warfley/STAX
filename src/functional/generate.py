#!/usr/bin/env python3
from argparse import ArgumentParser
from io import TextIOBase
from sys import stdout
from typing import TYPE_CHECKING

def generate_fun_types(paramcount: int, file: TextIOBase) -> None:
    gen_params = ", ".join([f"TParam{i}" for i in range(1, paramcount + 1)])
    params = "; " + "; ".join([f"AParam{i}: TParam{i}" for i in range(1, paramcount + 1)]) if paramcount else ""

    param_line = f"(AExecutor: TExecutor{params})"
    gen_line = f"<TResult, {gen_params}>" if paramcount else "<TResult>"

    fun_line = f"  generic T{paramcount}ParamFunction{gen_line} = function{param_line}: TResult;\n"
    fun_meth_line = f"  generic T{paramcount}ParamMethodFunction{gen_line} = function{param_line}: TResult of object;\n"

    file.writelines([fun_line, fun_meth_line])

def generate_proc_types(paramcount: int, file: TextIOBase) -> None:
    gen_params = ", ".join([f"TParam{i}" for i in range(1, paramcount + 1)])
    params = "; " + "; ".join([f"AParam{i}: TParam{i}" for i in range(1, paramcount + 1)]) if paramcount else ""

    param_line = f"(AExecutor: TExecutor{params})"
    gen_line = f"<{gen_params}>" if  paramcount else ""

    proc_line = f"  {'generic ' if paramcount else ''}T{paramcount}ParamProcedure{gen_line} = procedure{param_line};\n"
    proc_meth_line = f"  {'generic ' if paramcount else ''}T{paramcount}ParamMethodProcedure{gen_line} = procedure{param_line} of object;\n"

    file.writelines([proc_line, proc_meth_line])


def generate_proc_task_header(paramcount: int, method: bool, file: TextIOBase) -> None:
    file.write("  generic " if paramcount else "  ")
    gen_params = f"<{', '.join([f'TParam{i}' for i in range(1, paramcount + 1)])}>" if paramcount else ""
    file.write(f"T{paramcount}Param{'Method' if method  else''}ProcedureTask{gen_params} = class(TTask)\n")
    file.write("  public type\n")
    if paramcount:
        file.write(f"    TFunctionType = specialize T{paramcount}Param{'Method' if method else ''}Procedure{gen_params};\n")
    else:
        file.write(f"    TFunctionType = T0Param{'Method' if method  else ''}Procedure;\n")
    file.write("  private\n")
    file.write("    FProcPtr: TFunctionType;\n")
    for i in range(1, paramcount + 1):
        file.write(f"    FParam{i}: TParam{i};\n")
    file.write("  protected\n")
    file.write("    procedure Execute; override;\n")
    file.write("  public\n")
    create_params = "; ".join([f"AParam{i}: TParam{i}" for i in range(1, paramcount + 1)]) + "; " if paramcount else ""
    file.write(f"    constructor Create(AProcedure: TFunctionType; {create_params}AStackSize: SizeInt = DefaultTaskStackSize);\n")
    file.write("  end;\n")
    
def generate_fun_task_header(paramcount: int, method: bool, file: TextIOBase) -> None:
    gen_params = f"<TResult, {', '.join([f'TParam{i}' for i in range(1, paramcount + 1)])}>" if paramcount else "<TResult>"
    file.write(f"  generic T{paramcount}Param{'Method' if method  else ''}FunctionTask{gen_params} = class(specialize TRVTask<TResult>)\n")
    file.write("  public type\n")
    file.write(f"    TFunctionType = specialize T{paramcount}Param{'Method' if method else ''}Function{gen_params};\n")
    file.write("  private\n")
    file.write("    FFunPtr: TFunctionType;\n")
    for i in range(1, paramcount + 1):
        file.write(f"    FParam{i}: TParam{i};\n")
    file.write("  protected\n")
    file.write("    procedure Execute; override;\n")
    file.write("  public\n")
    create_params = "; ".join([f"AParam{i}: TParam{i}" for i in range(1, paramcount + 1)]) + "; " if paramcount else ""
    file.write(f"    constructor Create(AFunction: TFunctionType; {create_params}AStackSize: SizeInt = DefaultTaskStackSize);\n")
    file.write("  end;\n")

def generate_task_constructor(paramcount: int, method: bool, function: bool, file: TextIOBase) -> None:
    type_name = f"T{paramcount}Param{'Method' if method  else ''}{'Function' if function else 'Procedure'}Task"
    create_params = "; ".join([f"AParam{i}: TParam{i}" for i in range(1, paramcount + 1)]) + "; " if paramcount else ""
    file.write(f"constructor {type_name}.Create({'AFunction' if function else 'AProcedure'}: TFunctionType; {create_params}AStackSize: SizeInt);\n")
    file.write("begin\n")
    file.write("  inherited Create(AStackSize);\n")
    if function:
        file.write("  FFunPtr := AFunction;\n")
    else:
        file.write("  FProcPtr := AProcedure;\n")
    for i in range(1, paramcount + 1):
        file.write(f"  FParam{i} := AParam{i};\n")
    file.write("end;\n")

def generate_task_execute(paramcount: int, method: bool, function: bool, file: TextIOBase) -> None:
    type_name = f"T{paramcount}Param{'Method' if method  else ''}{'Function' if function else 'Procedure'}Task"
    param_list = ", " + ", ".join([f"FParam{i}" for i in range(1, paramcount + 1)]) if paramcount else ""
    file.write(f"procedure {type_name}.Execute;\n")
    file.write("begin\n")
    if function:
        file.write("  FResult := FFunPtr")
    else:
        file.write("  FProcPtr")
    file.write(f"(Executor{param_list});\n")
    file.write("end;\n")

def generate_async_proc(paramcount: int, method: bool, body: bool, file: TextIOBase):
    fun_type_name = f"T{paramcount}Param{'Method' if method  else ''}Procedure"
    gen_params = f"<{', '.join([f'TParam{i}' for i in range(1, paramcount + 1)])}>" if paramcount else ""
    if paramcount:
        file.write("generic ")
    file.write(f"function AsyncProcedure{gen_params}(\n  ")
    if paramcount:
        file.write(f"AProcedure: specialize {fun_type_name}{gen_params};\n  ")
    else:
        file.write(f"AProcedure: {fun_type_name};\n  ")
    for i in range(1, paramcount + 1):
        file.write(f"AParam{i}: TParam{i}; ")
    if paramcount:
        file.write("\n")
    file.write(f"  AStackSize: SizeInt = DefaultTaskStackSize): TTask; {'inline;' if not body else ''}\n")
    if not body:
        return
    file.write("begin\n")
    task_name = f"(specialize {fun_type_name}Task{gen_params})" if paramcount else f"{fun_type_name}Task"
    file.write(f"  Result := {task_name}.Create(AProcedure, ")
    for i in range(1, paramcount + 1):
        file.write(f"AParam{i}, ")
    file.write("AStackSize);\n")
    file.write("end;\n")

def generate_async_fun(paramcount: int, method: bool, body: bool, file: TextIOBase):
    fun_type_name = f"T{paramcount}Param{'Method' if method  else ''}Function"
    gen_params = f"<TResult, {', '.join([f'TParam{i}' for i in range(1, paramcount + 1)])}>" if paramcount else "<TResult>"
    file.write(f"generic function AsyncFunction{gen_params}(\n  ")
    file.write(f"AFunction: specialize {fun_type_name}{gen_params};\n  ")
    for i in range(1, paramcount + 1):
        file.write(f"AParam{i}: TParam{i}; ")
    file.write("\n")
    file.write(f"  AStackSize: SizeInt = DefaultTaskStackSize): specialize TRVTask<TResult>; {'inline;' if not body else ''}\n")
    if not body:
        return
    file.write("begin\n")
    task_name = f"(specialize {fun_type_name}Task{gen_params})"
    file.write(f"  Result := {task_name}.Create(AFunction, ")
    for i in range(1, paramcount + 1):
        file.write(f"AParam{i}, ")
    file.write("AStackSize);\n")
    file.write("end;\n")

def generate_fun_file(paramcount: int, functions: bool) -> None:
    with open(f"stax.functional.{'functions' if functions else 'procedures'}.pas", "w") as fl:
        fl.write(f"unit stax.functional.{'functions' if functions else 'procedures'};\n\n")
        fl.write("{$MODE ObjFpc}{$H+}\n\n")
        fl.write("interface\n\n")
        fl.write("uses stax;\n\n")
        fl.write("type\n")
        for i in range(paramcount+1):
            if functions:
                generate_fun_types(i, fl)
            else:
                generate_proc_types(i, fl)
        fl.write("\n")
        for i in range(paramcount+1):
            if functions:
                generate_fun_task_header(i, False, fl)
                fl.write("\n")
                generate_fun_task_header(i, True, fl)
                fl.write("\n")
            else:
                generate_proc_task_header(i, False, fl)
                fl.write("\n")
                generate_proc_task_header(i, True, fl)
                fl.write("\n")
        fl.write("implementation\n\n")
        for i in range(paramcount + 1):
            generate_task_execute(i, False, functions, fl)
            fl.write("\n")
            generate_task_constructor(i, False, functions, fl)
            fl.write("\n")
            generate_task_execute(i, True, functions, fl)
            fl.write("\n")
            generate_task_constructor(i, True, functions, fl)
            fl.write("\n")
        fl.write("end.")

def generate_main_file(paramcount: int) -> None:
    with open("stax.functional.pas", "w") as fl:
        fl.write(f"unit stax.functional;\n\n")
        fl.write("{$MODE ObjFpc}{$H+}\n\n")
        fl.write("interface\n\n")
        fl.write("uses stax, stax.functional.procedures, stax.functional.functions;\n\n")
        for i in range(paramcount+1):
            generate_async_proc(i, False, False, fl)
            generate_async_proc(i, True, False, fl)
        fl.write("\n")
        for i in range(paramcount+1):
            generate_async_fun(i, False, False, fl)
            generate_async_fun(i, True, False, fl)
        fl.write("implementation\n\n")
        for i in range(paramcount+1):
            generate_async_proc(i, False, True, fl)
            fl.write("\n")
            generate_async_proc(i, True, True, fl)
            fl.write("\n")
        fl.write("\n")
        for i in range(paramcount+1):
            generate_async_fun(i, False, True, fl)
            fl.write("\n")
            generate_async_fun(i, True, True, fl)
            fl.write("\n")
        fl.write("end.")

def main():
    arg_parser = ArgumentParser()
    arg_parser.add_argument("paramcount")

    args = arg_parser.parse_args()

    generate_fun_file(int(args.paramcount), False)
    generate_fun_file(int(args.paramcount), True)
    generate_main_file(int(args.paramcount))

if __name__=="__main__":
    main()
