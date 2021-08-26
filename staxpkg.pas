{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit StaxPKG;

{$warn 5023 off : no warning about unused units}
interface

uses
  stax, stax.helpertypes, stax.tasks.functional, 
  stax.tasks.functional.functions, stax.tasks.functional.procedures, 
  stax.tasks.io, stax.tasks.io.console, stax.tasks.io.tcp, windows.fiber, 
  Compatibility.Console, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('StaxPKG', @Register);
end.
