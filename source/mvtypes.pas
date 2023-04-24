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
    procedure IntersectionWithLine(A, B: TRealPoint; var P: TRealPointArray);
    class operator = (const Area1, Area2: TRealArea): Boolean;
  end;


implementation

function InLongitudeRange(Lon, Lon1, Lon2: Extended): boolean;
begin
  if Lon1 <= Lon2 then
    Result := InRange(Lon, Lon1, Lon2)
  else
    // Crossing the date-line
    Result := InRange(Lon, Lon1, 180) or InRange(Lon, -180, Lon2);
end;

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
    InRange(APoint.Lat, BottomRight.Lat, TopLeft.Lat) and
    InLongitudeRange(APoint.Lon, TopLeft.Lon, BottomRight.Lon);
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

{ Calculates the intersection with the other area. When the date line is crossed
  the right longitude becomes smaller than the left longitude! }
function TRealArea.Intersection(const Area: TRealArea): TRealArea;
var
  B, T, L, R: Extended;
begin
  LinearIntersection(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat, B, T);
  CyclicIntersection(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon, L, R);
  Result.TopLeft.Lon := L;
  Result.TopLeft.Lat := T;
  Result.BottomRight.Lon := R;
  Result.BottomRight.Lat := B;
end;
(*

var
  A1, A2: TRealArea;
begin
  A1 := Self.Normalize;
  A2 := Area.Normalize;

  if A1.TopLeft.Lon < A2.TopLeft.Lon then
    A1.TopLeft.Lon := A2.TopLeft.Lon;

  if A1.TopLeft.Lat > A2.TopLeft.Lat then
    A1.TopLeft.Lat := A2.TopLeft.Lat;

  if A1.BottomRight.Lon > A2.BottomRight.Lon then
    A1.BottomRight.Lon := A2.BottomRight.Lon;

  if A1.BottomRight.Lat < A2.BottomRight.Lat then
    A1.BottomRight.Lat := A2.BottomRight.Lat;

  Result := A1.Unnormalize;
end;     *)

function TRealArea.Intersects(const Area: TRealArea): boolean;
var
  A1, A2: TRealArea;
begin
  Result := LinearIntersects(BottomRight.Lat, TopLeft.Lat, Area.BottomRight.Lat, Area.TopLeft.Lat)
    and CyclicIntersects(TopLeft.Lon, BottomRight.Lon, Area.TopLeft.Lon, Area.BottomRight.Lon);
  (*
  if A1.CrossesDateLine then
    A1 := Self.NormalizeLeft;
  if A2.CrossesDateLine then
    A2 := Area.Normalize;
  Result :=
  (
    (A1.TopLeft.Lon <= A2.BottomRight.Lon) and
    (A1.BottomRight.Lon >= A2.TopLeft.Lon) and
    (A1.TopLeft.Lat >= A2.BottomRight.Lat) and
    (A1.BottomRight.Lat <= A2.TopLeft.Lat)
  ) or (
    (A2.TopLeft.Lon <= A1.BottomRight.Lon) and
    (A2.BottomRight.Lon >= A1.TopLeft.Lon) and
    (A2.TopLeft.Lat >= A1.BottomRight.Lat) and
    (A2.BottomRight.Lat <= A1.TopLeft.Lat)
  );
    *)
(*
  Result :=
    (InRange(A1.TopLeft.Lon, A2.TopLeft.Lon, A2.BottomRight.Lon) and
     InRange(A1.TopLeft.Lat, A2.TopLeft.Lat, A2.BottomRight.Lat))
    or
    (InRange(A2.TopLeft.Lon, A1.TopLeft.Lon, A1.BottomRight.Lon) and
     InRange(A2.TopLeft.Lat, A1.TopLeft.Lat, A1.BottomRight.Lat));
     *)
end;

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
end;
             (*
procedure TRealArea.IntersectionWithLine(A, B: TRealPoint; var P: TRealPointArray);

  // Intersection of PA...PB with vertical edge between x,y1 and x,y2
  function IntersectionVertEdge(PA, PB: TRealPoint; x, y1, y2: Double): TRealPoint;
  begin
    Result.Lon := x;
    Result.Lat := (PB.Lat - PA.Lat) / (PB.Lon - PA.Lon) * (x - PA.Lon) + PA.Lat;
    // Divide by zero handled outside
  end;

  // Intersection of PA..PB with horizontal edge between (x1,y) and (x2,y)
  function IntersectionHorEdge(PA, PB: TRealPoint; y, x1, x2: Double): TRealPoint;
  begin
    Result.Lat := y;
    Result.Lon := (PB.Lon - PA.Lon) / (PB.Lat - PA.Lat) * (y - PA.Lat) + PA.Lon;
    // Divide by zero handled outside
  end;

  procedure OneInside(Inside, OutSide: TRealPoint);
  var
    pt: TRealPoint;
  begin
    // Vertical line  intersects vertical line
    if Inside.Lon = Outside.Lon then
    begin
      if (Outside.Lon = TopLeft.Lon) or (Outside.Lon = BottomRight.Lon) then
      begin
        SetLength(P, 1);
        if Outside.Lat > TopLeft.Lat then
        begin
          P[0].Init(OutSide.Lon, TopLeft.Lat);
          exit;
        end;
        if Outside.lat < BottomRight.Lat then
        begin
          P[0].Init(OutSide.Lon, BottomRight.Lat);
          exit;
        end;
      end;
    end;
    // Horizontal line intersects horizontal line
    if Inside.Lat = Outside.Lat then
    begin
      if (Outside.Lat = TopLeft.Lat) or (Outside.Lat = BottomRight.Lat) then
      begin
        SetLength(P, 1);
        if Outside.Lon < TopLeft.Lon then
        begin
          P[0].Init(TopLeft.Lon, Outside.Lat);
          exit;
        end;
        if Outside.Lon > BottomRight.Lon then
        begin
          P[0].Init(BottomRight.Lon, Outside.lat);
          exit;
        end;
      end;
    end;

    // Intersection with top edge of area
    if Outside.Lat > TopLeft.Lat then
    begin
      pt := IntersectionHorEdge(Inside, Outside, TopLeft.Lat, TopLeft.Lon, BottomRight.Lon);
      if InRange(pt.Lon, TopLeft.Lon, BottomRight.Lon) then
      begin
        SetLength(P, 1);
        P[0] := pt;
        exit;
      end;
    end;

    // Intersection with bottom edge of area
    if Outside.Lat < BottomRight.Lat then
    begin
      pt := IntersectionHorEdge(Inside, Outside, BottomRight.Lat, TopLeft.Lon, BottomRight.Lon);
      if InRange(pt.Lon, TopLeft.Lon, BottomRight.Lon) then
      begin
        SetLength(P, 1);
        P[0] := pt;
        exit;
      end;
    end;

    // Intersection with left edge of area
    if Outside.Lon < TopLeft.Lon then
    begin
      pt := IntersectionVertEdge(Inside, Outside, TopLeft.Lon, BottomRight.Lat, TopLeft.Lat);
      if InRange(pt.Lat, BottomRight.Lat, TopLeft.Lat) then
      begin
        SetLength(P, 1);
        P[0] := pt;
        exit;
      end;
    end;

    // Intersection with right edge of area
    if Outside.Lon > BottomRight.Lon then
    begin
      pt := IntersectionVertEdge(Inside, Outside, BottomRight.Lon, BottomRight.Lat, TopLeft.Lat);
      if InRange(pt.Lat, BottomRight.Lat, TopLeft.Lat) then
      begin
        SetLength(P, 1);
        P[0] := pt;
        exit;
      end;
    end;
  end;

  procedure BothOutside;
  var
    pt: TRealPoint;
    n: Integer;
    d0, d1: Double;
  begin
    SetLength(P, 2);
    n := 0;

    // Vertical line (avoid division by zero)
    if A.Lon = B.Lon then
    begin
      if InRange(A.Lon, TopLeft.Lon, BottomRight.Lon) then
      begin
        if A.Lat < B.Lat then
        begin
          P[0].Init(A.Lon, BottomRight.Lat);
          P[1].Init(A.Lon, TopLeft.Lat);
        end else
        begin
          P[0].Init(A.Lon, TopLeft.Lat);
          P[1].Init(A.Lon, BottomRight.Lat);
        end;
      end else
        SetLength(P, 0);
      exit;
    end;

    // Horizontal line (avoid division by zero)
    if A.Lat = B.Lat then
    begin
      if InRange(A.Lat, BottomRight.Lat, TopLeft.Lat) then
      begin
        if A.Lon < B.Lon then
        begin
          P[0].Init(TopLeft.Lon, A.Lat);
          P[1].Init(BottomRight.Lon, A.Lat);
        end else
        begin
          P[0].Init(BottomRight.Lon, A.Lat);
          P[1].Init(TopLeft.Lon, A.Lat);
        end;
      end else
        SetLength(P, 0);
      exit;
    end;

    // Intersection with top edge of area
    pt := IntersectionHorEdge(A, B, TopLeft.Lat, TopLeft.Lon, BottomRight.Lon);
    if InRange(pt.Lon, TopLeft.Lon, BottomRight.Lon) then
    begin
      P[n] := pt;
      inc(n);
    end;

    // Intersection with bottom edge of area
    pt := IntersectionHorEdge(A, B, BottomRight.Lat, TopLeft.Lon, BottomRight.Lon);
    if InRange(pt.Lon, TopLeft.Lon, BottomRight.Lon) then
    begin
      P[n] := pt;
      inc(n);
    end;

    // Intersection with left edge of area
    if A.Lat = B.Lat then
      pt.Init(TopLeft.Lon, A.Lat)
    else
      pt := IntersectionVertEdge(A, B, TopLeft.Lon, BottomRight.Lat, TopLeft.Lat);
    if InRange(pt.Lat, BottomRight.Lat, TopLeft.Lat) then
    begin
      P[n] := pt;
      inc(n);
    end;

    // Intersection with right edge of area
    if A.Lat = B.Lat then
      pt.Init(BottomRight.Lon, A.Lat)
    else
      pt := IntersectionVertEdge(A, B, BottomRight.Lon, BottomRight.Lat, TopLeft.Lat);
    if InRange(pt.Lat, BottomRight.Lat, TopLeft.Lat) then
    begin
      P[n] := pt;
      inc(n);
    end;

    if n = 0 then
    begin
      SetLength(P, 0);
      exit;
    end;

    // Order intersection points in ascending distance from A
    d0 := sqr(A.Lon - P[0].Lon) + sqr(A.Lat - P[0].Lat);
    d1 := sqr(A.Lon - P[1].Lon) + sqr(A.Lat - P[1].Lat);
    if d0 > d1 then
    begin
      pt := P[0];
      P[0] := P[1];
      P[1] := pt;
    end;
  end;

var
  isInsideA, isInsideB: Boolean;
begin
  isInsideA := ContainsPoint(A);
  isInsideB := ContainsPoint(B);

  if isInsideA and isInsideB then
  begin
    SetLength(P, 0);
    exit;
  end;

  if isInsideA and not isInsideB then
  begin
    OneInside(A, B);
    exit;
  end;

  if isInsideB and not isInsideA then
  begin
    OneInside(B, A);
    exit;
  end;

  BothOutside;
end;
          *)
class operator TRealArea.= (const Area1, Area2: TRealArea): Boolean;
begin
  Result := (Area1.TopLeft = Area2.TopLeft) and (Area1.BottomRight = Area2.BottomRight);
end;

end.

