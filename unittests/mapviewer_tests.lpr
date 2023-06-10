program mapviewer_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner,
  mvtests_engine, mvtests_types;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

