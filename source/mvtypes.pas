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
end;

function TRealArea.Intersects(const Area: TRealArea): boolean;
var
  A1, A2: TRealArea;
begin
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

(*
  Result :=
    (InRange(A1.TopLeft.Lon, A2.TopLeft.Lon, A2.BottomRight.Lon) and
     InRange(A1.TopLeft.Lat, A2.TopLeft.Lat, A2.BottomRight.Lat))
    or
    (InRange(A2.TopLeft.Lon, A1.TopLeft.Lon, A1.BottomRight.Lon) and
     InRange(A2.TopLeft.Lat, A1.TopLeft.Lat, A1.BottomRight.Lat));
     *)
end;

class operator TRealArea.= (const Area1, Area2: TRealArea): Boolean;
begin
  Result := (Area1.TopLeft = Area2.TopLeft) and (Area1.BottomRight = Area2.BottomRight);
end;

end.

