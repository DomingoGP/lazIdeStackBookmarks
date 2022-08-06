{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lazstackbookmarks;

{$warn 5023 off : no warning about unused units}
interface

uses
  LazViewStackBookmarks, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazViewStackBookmarks', @LazViewStackBookmarks.Register);
end;

initialization
  RegisterPackage('lazstackbookmarks', @Register);
end.
