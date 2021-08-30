{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit StaxPKG;

{$warn 5023 off : no warning about unused units}
interface

uses
  stax, stax.helpertypes, stax.functional, stax.functional.functions, 
  stax.functional.procedures, stax.asyncio, stax.asyncconsole, stax.asynctcp, 
  stax.internal.console, stax.internal.tcp, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('StaxPKG', @Register);
end.
