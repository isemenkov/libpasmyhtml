program libpasmyhtml_testproject;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, myhtmltestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

