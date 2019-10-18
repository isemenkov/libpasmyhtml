unit myhtmltestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, libpasmyhtml;

type

  TMyHTMLTestCase= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TMyHTMLTestCase.TestHookUp;
begin
  Fail('Напишите ваш тест');
end;



initialization

  RegisterTest(TMyHTMLTestCase);
end.

