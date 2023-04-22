unit mvMiscTests_Types;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type

  TAreaTests= class(TTestCase)
  published
    procedure Test_PointInArea;
    procedure Test_Union;
    procedure Test_Intersection;
  end;

implementation

uses
  mvTypes;

function AreaToStr(Area: TRealArea): String;
begin
  Result := Format('L=%.6f T=%.6f R=%.6f B=%.6f', [
    Area.TopLeft.Lon, Area.TopLeft.Lat, Area.BottomRight.Lon, Area.BottomRight.Lat
  ]);
end;

procedure TAreaTests.Test_PointInArea;
var
  counter: Integer;
  a: TRealArea;
  p: TRealPoint;
begin
  // Regular area, point inside
  counter := 1;
  a.Init(0, 10, 10, 0);
  p.Init(5, 5);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    true,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Regular area, point's longitude outside
  inc(counter);
  p.Init(15, 5);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    false,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Regular area, point's latitude outside
  inc(counter);
  p.Init(5, 15);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    false,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point inside in the eastern part (left of dateline)
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(175, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    true,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point inside in the western part (right of dateline)
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(-175, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    true,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point at dateline (east)
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(180, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    true,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point at dateline (west)
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(-180, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    true,                // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point's longitude outside, eastern part
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(160, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    false,               // expected
    a.ContainsPoint(p)   // actual
  );

  // Area crossing dateline, point's longitude outside, western part
  inc(counter);
  a.Init(170, 40, -170, 30);
  p.Init(-160, 35);
  AssertEquals(
    'Point in area test #' + IntToStr(counter) + ' mismatch',
    false,               // expected
    a.ContainsPoint(p)   // actual
  );
end;

procedure TAreaTests.Test_Union;
var
  counter: Integer;
  a, b, expected, actual: TRealArea;
begin
  // Regular areas, separated
  counter := 1;
  a.Init(0, 10, 10, 0);
  b.Init(20, 40, 30, 20);
  expected.Init(0, 40, 30, 0);
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // Regular areas, partly overlapping
  inc(counter);
  a.Init(0, 10, 10, 0);
  b.Init(5, 40, 30, 20);
  expected.Init(0, 40, 30, 0);
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // Regular areas, partly overlapping
  inc(counter);
  a.Init(5, 10, 10, 0);
  b.Init(0, 40, 30, 20);
  expected.Init(0, 40, 30, 0);
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // Regular areas, partly overlapping
  inc(counter);
  a.Init(5, 10, 10, 0);
  b.Init(0, 40, 30, 20);
  expected.Init(0, 40, 30, 0);
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // First area crossing dateline
  inc(counter);
  a.Init(160, 30, -160, 10);         // --> 160...200
  b.Init(-170, 60, -150, 50);        // --> -170..-160
  expected.Init(-170, 60, -160, 10); // --> -170...200
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // Second area crossing dateline
  inc(counter);
  a.Init(-170, 60, -150, 50);        // --> -170..-160
  b.Init(160, 30, -160, 10);         // --> 160...200
  expected.Init(-170, 60, -160, 10); // --> -170...200
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

  // Both areas crossing dateline
  inc(counter);
  a.Init(170, 60, -150, 50);         // --> 170..210
  b.Init(160, 30, -160, 10);         // --> 160...200
  expected.Init(160, 60, -150, 10);  // --> 160...210
  actual := a.Union(b);
  AssertEquals(
    'Area union test #' + IntToStr(counter) + ' mismatch',
    true,                 // expected
    (actual = expected)   // actual
  );

end;


procedure TAreaTests.Test_Intersection;
var
  counter: Integer;
  a, b, expected, actual: TRealArea;
  intersects: Boolean;
begin
  // Regular areas, separated
  counter := 1;
  a.Init(0, 10, 10, 0);
  b.Init(20, 40, 30, 20);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    false,
    intersects
  );

  // Regular areas, partly overlapping
  inc(counter);
  a.Init(0, 30, 20, 0);
  b.Init(5, 40, 30, 20);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    true,
    intersects
  );
  expected.Init(5, 30, 20, 20);
  actual := a.Intersection(b);
  AssertEquals(
    'Area intersection test #' + IntToStr(counter) + ' mismatch',
    AreaToStr(expected),
    AreaToStr(actual)
  );

  // Regular areas, partly overlapping, reverse order
  inc(counter);
  a.Init(5, 40, 30, 20);
  b.Init(0, 30, 20, 0);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    true,
    intersects
  );
  expected.Init(5, 30, 20, 20);
  actual := a.Intersection(b);
  AssertEquals(
    'Area intersection test #' + IntToStr(counter) + ' mismatch',
    AreaToStr(expected),
    AreaToStr(actual)
  );

  // First area crossing date line, no overlaps
  inc(counter);
  a.Init(160, 40, -170, 20);
  b.Init(-160, 30, -150, 0);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    false,
    intersects
  );

  // First area crossing date line, overlaps on the left side of date lie
  inc(counter);
  a.Init(160, 40, -170, 20);
  b.Init(165, 30, 170, 0);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    true,
    intersects
  );
  expected.Init(165, 30, 170, 20);
  actual := a.Intersection(b);
  AssertEquals(
    'Area intersection test #' + IntToStr(counter) + ' mismatch',
    AreaToStr(expected),
    AreaToStr(actual)
  );

  // First area crossing date line, overlaps on the right side of date lie
  inc(counter);
  a.Init(160, 40, -160, 20);
  b.Init(-170, 30, -165, 0);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    true,
    intersects
  );
  expected.Init(-170, 30, -165, 20);
  actual := a.Intersection(b);
  AssertEquals(
    'Area intersection test #' + IntToStr(counter) + ' mismatch',
    AreaToStr(expected),
    AreaToStr(actual)
  );

  // Second area crossing date line, no overlaps
  inc(counter);
  a.Init(-160, 30, -150, 0);
  b.Init(160, 40, -170, 20);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    false,
    intersects
  );

  // Second area crossing date line, overlaps on the left side of dateline
  inc(counter);
  a.Init(165, 30, 170, 0);
  b.Init(160, 40, -170, 20);
  intersects := a.Intersects(b);
  AssertEquals(
    'Area intersection detection test #' + IntToStr(counter) + ' mismatch',
    true,
    intersects
  );
  expected.Init(165, 30, 170, 20);
  actual := a.Intersection(b);
  AssertEquals(
    'Area intersection test #' + IntToStr(counter) + ' mismatch',
    AreaToStr(expected),
    AreaToStr(actual)
  );
end;


initialization

  RegisterTest(TAreaTests);
end.

