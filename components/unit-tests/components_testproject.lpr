program components_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, components_testcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

