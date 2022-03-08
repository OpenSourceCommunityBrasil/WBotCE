program Demo;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, WBotce_Config,
  unit_main
  { you can add units after this };

{$R *.res}

begin
  try
    if (CreateGlobalCEFApp) then
    begin
      RequireDerivedFormResource:=True;
  Application.Scaled:=True;
      Application.Initialize;
      Application.CreateForm(TForm1, Form1);
      Application.Run;
    end;
  finally
    DestroyGlobalCEFApp;
  end;
end.

