{
 _    _  _         _____  _____  _____
| |  | || |       |_   _|| ____||  ___|
| |  | || |___  ___ | |  | |    | |
| |/\| ||  _  |/ _ \| |  | |    |  _|
| /  \ || (_) | (_) | |  | |___ | |___
|__/\__||_____|\___/|_|  |_____||_____|

}

{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit wbotce_pack;

{$warn 5023 off : no warning about unused units}
interface

uses
  wbotce_core, WBotce_Form, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('wbotce_core', @wbotce_core.Register);
end;

initialization
  RegisterPackage('wbotce_pack', @Register);
end.
