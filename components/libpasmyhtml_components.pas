{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit libPasMyHTML_Components;

{$warn 5023 off : no warning about unused units}
interface

uses
  TagTreeView, pasmyhtml, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('TagTreeView', @TagTreeView.Register);
end;

initialization
  RegisterPackage('libPasMyHTML_Components', @Register);
end.
