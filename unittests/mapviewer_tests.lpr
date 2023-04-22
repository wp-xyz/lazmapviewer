program mapviewer_tests;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, mvMiscTests_Engine, mvmisctests_types;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

