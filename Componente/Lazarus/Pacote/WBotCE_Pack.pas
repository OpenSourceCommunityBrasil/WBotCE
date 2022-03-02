{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit WBotCE_Pack;

{$warn 5023 off : no warning about unused units}
interface

uses
  WBotCE_Config, WBotCE_Const, WBotCE_Core, WBotCE_Utils, WBotCE_UtilsImg, 
  WBotCE_Model, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('WBotCE_Core', @WBotCE_Core.Register);
end;

initialization
  RegisterPackage('WBotCE_Pack', @Register);
end.
