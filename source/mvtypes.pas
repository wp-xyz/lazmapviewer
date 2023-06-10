{
  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvTypes;

{$mode objfpc}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

interface

uses
  Classes, SysUtils, Math;

const
  TILE_SIZE = 256;
  PALETTE_PAGE = 'Misc';
  DEFAULT_POI_TEXT_WIDTH = 300;

Type
  { TArea }
  TArea = record
    top, left, bottom, right: Int64;
  end;

  { TRealPoint }
  TRealPoint = Record
    public
      Lon: Double;
      Lat: Double;
    private
      function GetLonRad: Extended;
      procedure SetLonRad(AValue: Extended);
      function GetLatRad: Extended;
      procedure SetLatRad(AValue: Extended);
    public
      procedure Init(ALon, ALat: Double);
      property LonRad: Extended read GetLonRad write SetLonRad;
      property LatRad: Extended read GetLatRad write SetLatRad;
  end;

  { TRealArea }
  TRealArea = Record
  public
    TopLeft : TRealPoint;
    BottomRight : TRealPoint;
  public
    procedure Init(ALeft, ATop, ARight, ABottom: Extended);
    procedure Init(ATopLeft, ABottomRight: TRealPoint);
    function ContainsPoint(APoint: TRealPoint): boolean;
    function Equal(Area: TRealArea): Boolean;
    function Intersection(const Area: TRealArea): TRealArea;
    function Intersects(const Area: TRealArea): boolean;
    function Union(const Area: TRealArea): TRealArea;
  end;


implementation

{ Helper functions to simplify using the cyclic coordinates }

{ Checks whether x is between x1 and x2 (where x1 < x2) }
function LinearInRange(x, x1, x2: Extended): Boolean;
begin
  Result := InRange(x, x1, x2);
end;

{ Checks whether x is between x1 and x2 where x1 and x2 can be in any order.
  When x1 > x2 it is assumed that the interval crosses the dateline. }
function CyclicInRange(x, x1, x2: Extended): Boolean;
begin
  if x1 <= x2 then
    Result := inRange(x, x1, x2)
  else
    Result := (x > x1) or (x < x2);
end;


{ Checks whether the line segment between A1 and A2 intersects the line segment
  between B1 and B2. It is assumed that A1 < A2 and B1 < B2. }
function LinearIntersects(A1, A2, B1, B2: Extended): Boolean;
begin
  Result := InRange(A1, B1, B2) or InRange(A2, B1, B2) or
            InRange(B1, A1, A2) or InRange(B2, A1, A2);
end;

{ Checks whether the line segment between A1 and A2 intersects the line segment
  between B1 and B2. A1 and A2, as well as B1 and B2 can be in any order.
  When the coordinate with index 2 is greater than the coordinate with index 1
  it is assumed that the segment crosses the dateline. }
function CyclicIntersects(L1, R1, L2, R2: Extended): Boolean;
begin
  if (L1 <= R1) and (L2 <= R2) then
    Result := LinearIntersects(L1, R1, L2, R2)
  else
  if (L1 <= R1) and (L2 > R2) then
    Result := (L2 <= R1) or (R2 >= L1)
  else
  if (L1 > R1) and (L2 <= R2) then
    Result := (R1 >= L2) or (L1 <= R2)
  else
    Result := true;
end;

{ Calculates in Res1 and Res2 the endpoints of the overlap of the segments
  between A1 and A2 and between B1 and B2. A1 and A2, and B1 and B2 must be
  in ascending order.
  The function returns false if the segments do not overlap. }
function LinearIntersection(A1, A2, B1, B2: Extended; out Res1, Res2: Extended): Boolean;
begin
  Result := false;
  if (A2 < B1) or (B2 < A1) then
    exit;
  Res1 := A1;
  Res2 := A2;
  if B1 > Res1 then Res1 := B1;
  if B2 < Res2 then Res2 := B2;
  Result := true;
end;

{ Calculates in L and R the endpoints of the overlap of the segments
  between L1 and R1 and between L2 and R2. L1 and R1, and L2 and R2 can be in
  any order. If L1 > R1 it is assumed that this segment crosses the dateline.
  Likewise with L2/R2.
  The function returns false if the segments do not overlap. }
function CyclicIntersection(L1, R1, L2, R2: Extended; out L, R: Extended): Boolean;
begin
  Result := false;
  if (L1 <= R1) and (L2 <= R2) then
    Result := LinearIntersection(L1, R1, L2, R2, L, R)
  else
  if (L1 <= R1) and (L2 > R2) then
  begin
    if (L2 > R1) and (L1 > R2) then
      exit;
    Result := true;
    L := L1;
    R := R1;
    if (L2 <= L1) or (R2 >= R1) then exit;
    if L2 < R1 then R := L2;
    if R2 > L1 then L := R2;
  end else
  if (L1 > R1) and (L2 <= R2) then
  begin
    if (L1 > R2) and (R1 < L2) then exit;
    L := L2;
    R := R2;
    Result := true;
    if (L1 <= L2) or (R1 >= R2) then exit;
    if L1 < R2 then R := L1;
    if R1 > L2 then L := R1;
  end else
  begin
    Result := true;
    L := L1;
    R := R1;
    if L2 > L1 then L := L2;
    if R2 < R1 then R := R2;
  end;
end;

{ Calculates the union of the sements between A1/A2 and between B1/B2 and
  returns the endpoints of the union in Res1 and Res2. It is assumed then
  A1/A2 and B1/B2 are in ascending order. }
procedure LinearUnion(A1, A2, B1, B2: Extended; out Res1, Res2: Extended);
begin
  Res1 := A1;
  Res2 := A2;
  if B1 < Res1 then Res1 := B1;
  if B2 > Res2 then Res2 := B2;
end;

{ Calculates the union of the sements between L1/R1 and between L2/R2 and
  returns the endpoints of the union in L and R. L1/R1 and L2/R2 can be in any
  order. When L1 > R1 then it is assumed that this segment crosses the dateline.
  Likewise with L2/R2. }
procedure CyclicUnion(L1, R1, L2, R2: Extended; out L, R: Extended);
  procedure SetLimits(aL, aR: Extended);
  begin
    L := aL;
    R := aR;
  end;
begin
  // Both between -180 and 180 deg
  if (L1 <= R1) and (L2 <= R2) then
    LinearUnion(L1, R1, L2, R2, L, R)
  else
  // 2nd crossing dateline               //-180                         180
  if (L1 <= R1) and (L2 > R2) then       //  |        L1-----R1          |
  begin
    if L2 <= L1 then                     //  |-R2  L2--------------------|
      SetLimits(L2, R2)
    else
    if L2 <= R1 then
    begin
      if R2 < L1 then                    //  |-R2        L2--------------|
        SetLimits(L1, R2)
      else                               //  |----------R2 L2------------|  // Complete overlap
        SetLimits(-180.0, 180.0);
    end else
    begin   // L2 > R1
      if R2 < L1 then                    //  |-R2                    L2--|  // No overlap here. Since we want to "extend", we keep L1R1
        SetLimits(L1, R1)
      else                               //  |----------R2           L2--|
      if R2 < R1 then
        SetLimits(L2, R1)
      else  // R2 > R                    //  |-------------------R2  L2--|
        SetLimits(L2, R2);
    end;
  end else
  // 1st crossing dateline
  if (L1 > R1) and (L2 <= R2) then
  begin
    CyclicUnion(L2, R2, L1, R1, L, R);
  end else
  // both crossing dateline
  begin
    if L2 < R1 then  // complete overlap
      SetLimits(-180, 180)
    else
    begin
      SetLimits(L1, R1);
      if L2 < L then L := L2;
      if R2 > R then R := R2;
    end;
  end;
end;


{ TRealPoint }

procedure TRealPoint.Init(ALon, ALat: Double);
begin
  Lon := ALon;
  Lat := ALat;
end;

function TRealPoint.GetLonRad: Extended;
begin
  Result := DegToRad(Self.Lon);
end;

procedure TRealPoint.SetLonRad(AValue: Extended);
begin
  Self.Lon := RadToDeg(AValue);
end;

function TRealPoint.GetLatRad: Extended;
begin
  Result := DegToRad(Self.Lat);
end;

procedure TRealPoint.SetLatRad(AValue: Extended);
begin
  Self.Lat := RadToDeg(AValue);
end;


{ TRealArea

  It is assumed (and not checked) that ATop and ABottom are between -90 and 90
  and ALeft and ARight between -180 and 180. When ALeft and ARight are in
  reverse order it is assumed that the dateline is crossed.
}
procedure TRealArea.Init(ALeft, ATop, ARight, ABottom: Extended);
begin
  TopLeft.Lon := ALeft;
  TopLeft.Lat := ATop;
  BottomRight.Lon := ARight;
  BottomRight.Lat := ABottom;
end;

procedure TRealArea.Init(ATopLeft, ABottomRight: TRealPoint);
begin
  TopLeft := ATopLeft;
  BottomRight := ABottomRight;
end;

{ Checks whether the given point is inside the area (including borders). }
function TRealArea.ContainsPoint(APoint: TRealPoint): boolean;
begin
  Result :=
    LinearInRange(APoint.Lat, BottomRight.Lat, TopLeft.Lat) and
    CyclicInRange(APoint.Lon, TopLeft.Lon, BottomRight.Lon);
end;

function TRealArea.Equal(Area: TRealArea): Boolean;
begin
  Result :=
    (TopLeft.Lon = Area.TopLeft.Lon) and
    (TopLeft.Lat = Area.TopLeft.Lat) and
    (BottomRight.Lon = Area.BottomRight.Lon) and
    (BottomRight.Lat = Area.BottomRight.Lat);
end;

function TRealArea.Intersection(const Area: TRealArea): TRealArea;
var
  B, T, L, R: Extended;
begin
  LinearIntersection(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat, B, T);
  CyclicIntersection(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon, L, R);
  Result.Init(L, T, R, B);
end;

function TRealArea.Intersects(const Area: TRealArea): boolean;
var
  A1, A2: TRealArea;
begin
  Result :=
    LinearIntersects(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat) and
    CyclicIntersects(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon);
end;

{ Calculates the union with the other area. When the date line is crossed the
  right longitude becomes smaller than the left longitude! }
function TRealArea.Union(const Area: TRealArea): TRealArea;
var
  B, T, L, R: Extended;
begin
  LinearUnion(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat, B, T);
  CyclicUnion(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon, L, R);
  Result.Init(L, T, R, B);
end;

end.

