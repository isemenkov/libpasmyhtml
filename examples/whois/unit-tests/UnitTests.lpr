program whois_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, RootZoneDatabaseTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.
