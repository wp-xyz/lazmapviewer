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
      class operator = (const P1, P2: TRealPoint): Boolean;
      property LonRad: Extended read GetLonRad write SetLonRad;
      property LatRad: Extended read GetLatRad write SetLatRad;
  end;
  TRealPointArray = array of TRealPoint;

  { TRealArea }
  TRealArea = Record
    TopLeft : TRealPoint;
    BottomRight : TRealPoint;
    procedure Init(ALeft, ATop, ARight, ABottom: Extended);
    procedure Init(ATopLeft, ABottomRight: TRealPoint);
    function ContainsPoint(APoint: TRealPoint): Boolean;
    function CrossesDateLine: boolean;
    function Normalize: TRealArea;
    function NormalizeLeft: TRealArea;
    function UnNormalize: TRealArea;
    function Union(const Area: TRealArea): TRealArea;
    function Intersection(const Area: TRealArea): TRealArea;
    function Intersects(const Area: TRealArea): boolean;
//    procedure IntersectionWithLine(A, B: TRealPoint; var P: TRealPointArray);
    class operator = (const Area1, Area2: TRealArea): Boolean;
  end;


implementation

function LinearInRange(x, x1, x2: Extended): Boolean;
begin
  Result := InRange(x, x1, x2);
end;

function CyclicInRange(x, x1, x2: Extended): Boolean;
begin
  if x1 <= x2 then
    Result := inRange(x, x1, x2)
  else
    Result := (x > x1) or (x < x2);
end;
(*
function InLongitudeRange(Lon, Lon1, Lon2: Extended): boolean;
begin
  if Lon1 <= Lon2 then
    Result := InRange(Lon, Lon1, Lon2)
  else
    // Crossing the date-line
    Result := InRange(Lon, Lon1, 180) or InRange(Lon, -180, Lon2);
end;
  *)
// It is assumed that A1 < A2 and B1 < B2.
function LinearIntersects(A1, A2, B1, B2: Extended): Boolean;
begin
  Result := InRange(A1, B1, B2) or InRange(A2, B1, B2) or
            InRange(B1, A1, A2) or InRange(B2, A1, A2);
end;

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

procedure LinearUnion(A1, A2, B1, B2: Extended; out Res1, Res2: Extended);
begin
  Res1 := A1;
  Res2 := A2;
  if B1 < Res1 then Res1 := B1;
  if B2 > Res2 then Res2 := B2;
end;

procedure CyclicUnion(L1, R1, L2, R2: Extended; out L, R: Extended);
begin
  if (L1 <= R1) and (L2 <= R2) then
    LinearUnion(L1, R1, L2, R2, L, R)
  else
  if (L1 <= R1) and (L2 > R2) then
  begin
    L := L1;
    R := R1;
    if L2 < L then L := L2;
    if R2 > R then R := R2;
  end else
  if (L1 > R1) and (L2 <= R2) then
  begin
    L := L2;
    R := R2;
    if L1 < L then L := L1;
    if R1 > R then R := R1;
  end else
  begin
    L := L1;
    R := R1;
    if L2 < L then L := L2;
    if R2 > R then R := R2;
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

class operator TRealPoint.= (const P1, P2: TRealPoint): Boolean;
begin
  Result := (P1.Lon = P2.Lon) and (P1.Lat = P2.Lat);
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
  Normalize;
end;

procedure TRealArea.Init(ATopLeft, ABottomRight: TRealPoint);
begin
  TopLeft := ATopLeft;
  BottomRight := ABottomRight;
  Normalize;
end;

function TRealArea.ContainsPoint(APoint: TRealPoint): boolean;
begin
  Result :=
    LinearInRange(APoint.Lat, BottomRight.Lat, TopLeft.Lat) and
    CyclicInRange(APoint.Lon, TopLeft.Lon, BottomRight.Lon);
end;

function TRealArea.CrossesDateLine: boolean;
begin
  Result := BottomRight.Lon < TopLeft.Lon;
end;

{ Makes sure that the left and right coordinates are in ascending order. }
function TRealArea.Normalize: TRealArea;
begin
  Result := Self;
  if Result.CrossesDateLine then
    Result.BottomRight.Lon := Result.BottomRight.Lon + 360;
end;

function TRealArea.NormalizeLeft: TRealArea;
begin
  Result := Self;
  if Result.CrossesDateLine then
    Result.TopLeft.Lon := Result.TopLeft.Lon - 360.0;
end;

function TRealArea.UnNormalize: TRealArea;
begin
  Result := Self;
  if (Result.TopLeft.Lon < -180.0) then
    Result.TopLeft.Lon := Result.TopLeft.Lon + 360.0;
  if (Result.BottomRight.Lon > 180.0) then
    Result.BottomRight.Lon := Result.BottomRight.Lon - 360.0;
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
{
var
  A: TRealArea;
begin
  Result := Self.Normalize;
  A := Area.Normalize;

  if A.TopLeft.Lon < Result.TopLeft.Lon then
    Result.TopLeft.Lon := A.TopLeft.Lon;
  if A.BottomRight.Lon > Result.BottomRight.Lon then
    Result.BottomRight.Lon := A.BottomRight.Lon;

  if A.TopLeft.Lat > Result.TopLeft.Lat then
    Result.TopLeft.Lat := A.TopLeft.Lat;
  if A.BottomRight.Lat < Result.BottomRight.Lat then
    Result.BottomRight.Lat := A.BottomRight.Lat;

  Result := Result.Unnormalize;
end;
 }
{ Calculates the intersection with the other area. When the date line is crossed
  the right longitude becomes smaller than the left longitude! }
function TRealArea.Intersection(const Area: TRealArea): TRealArea;
var
  B, T, L, R: Extended;
begin
  LinearIntersection(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat, B, T);
  CyclicIntersection(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon, L, R);
  Result.Init(L, T, R, B);
  {
  Result.TopLeft.Lon := L;
  Result.TopLeft.Lat := T;
  Result.BottomRight.Lon := R;
  Result.BottomRight.Lat := B;
  }
end;

function TRealArea.Intersects(const Area: TRealArea): boolean;
var
  A1, A2: TRealArea;
begin
  Result :=
    LinearIntersects(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat) and
    CyclicIntersects(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon);
end;
                     (*
{ Calculates the intersection point(s) of the area borders with the straight line
  between A and B, and returns the intersection points in the array P which has
  either 0, 1, or 2 points.
  NOTE: This routine does not take care of crossing the dateline.
  Acknowledgement: Modified code from TAChart. }
procedure TRealArea.IntersectionWithLine(A, B: TRealPoint;
  var P: TRealPointArray);

  procedure AdjustX(var Pt: TRealPoint; NewLon: Double);
  var
    dx: Double;
  begin
    dx := B.Lon - A.Lon;
    if not IsInfinite(dx) and not IsInfinite(Pt.Lat) then
      Pt.Lat := Pt.Lat + (B.Lat - A.Lat) / dx * (NewLon - Pt.Lon);
    Pt.Lon := NewLon;
  end;

  procedure AdjustY(var Pt: TRealPoint; NewLat: Double);
  var
    dy: Double;
  begin
    dy := B.Lat - A.Lat;
    if not IsInfinite(dy) and not IsInfinite(Pt.Lon) then
      Pt.Lon := Pt.Lon + (B.Lon - A.Lon) / dy * (NewLat - Pt.Lat);
    Pt.Lat := NewLat;
  end;

type
  TCaseOfTwo = (cotNone, cotFirst, cotSecond, cotBoth);
const
  CASE_OF_TWO: array [Boolean, Boolean] of TCaseOfTwo =
    ((cotNone, cotSecond), (cotFirst, cotBoth));
var
  oldA, oldB: TRealPoint;
  n: Integer;
begin
  oldA := A;
  oldB := B;
  case CASE_OF_TWO[A.Lon < TopLeft.Lon, B.Lon < TopLeft.Lon] of
    cotFirst: AdjustX(A, TopLeft.Lon);
    cotSecond: AdjustX(B, TopLeft.Lon);
    cotBoth: exit;
    cotNone: ;
  end;
  case CASE_OF_TWO[A.Lon > BottomRight.Lon, B.Lon > BottomRight.Lon] of
    cotFirst: AdjustX(A, BottomRight.Lon);
    cotSecond: AdjustX(B, BottomRight.Lon);
    cotBoth: exit;
    cotNone: ;
  end;
  case CASE_OF_TWO[A.Lat < BottomRight.Lat, B.Lat < BottomRight.Lat] of
    cotFirst: AdjustY(A, BottomRight.Lat);
    cotSecond: AdjustY(B, BottomRight.Lat);
    cotBoth: exit;
    cotNone: ;
  end;
  case CASE_OF_TWO[A.Lat > TopLeft.Lat, B.Lat > TopLeft.Lat] of
    cotFirst: AdjustY(A, TopLeft.Lat);
    cotSecond: AdjustY(B, TopLeft.Lat);
    cotBoth: exit;
    cotNone: ;
  end;
  n := 0;
  SetLength(P, 2);
  if (A <> oldA) then
  begin
    P[n] := A;
    inc(n);
  end;
  if (B <> oldB) then
  begin
    P[n] := B;
    inc(n);
  end;
  SetLength(P, n);
end;      *)

class operator TRealArea.= (const Area1, Area2: TRealArea): Boolean;
begin
  Result := (Area1.TopLeft = Area2.TopLeft) and (Area1.BottomRight = Area2.BottomRight);
end;

end.

