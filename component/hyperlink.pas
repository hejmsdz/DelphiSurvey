{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit hyperlink;

interface

uses
  MyHyperlink, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('MyHyperlink', @MyHyperlink.Register);
end;

initialization
  RegisterPackage('hyperlink', @Register);
end.
