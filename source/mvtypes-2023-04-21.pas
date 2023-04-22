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
      property LonRad: Extended read GetLonRad write SetLonRad;
      property LatRad: Extended read GetLatRad write SetLatRad;
  end;

  { TRealArea }
  TRealArea = record
  private
    FTopLeft : TRealPoint;
    FBottomRight : TRealPoint;
  public
    procedure Create(ALeft, ATop, ARight, ABottom: Extended);
    procedure Create(ATopLeft, ABottomRight: TRealPoint);
    function Intersection(const Area: TRealArea; out Common: TRealArea): Boolean;
    function Intersects(const Area: TRealArea): Boolean;
    procedure Normalize;
    function PointInside(const Pt: TRealPoint): Boolean;
    function Union(const Area: TRealArea): TRealArea;
    property TopLeft: TRealPoint read FTopLeft;
    property BottomRight: TRealPoint read FBottomRight;
  end;


implementation

{ TRealPoint }

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

{ TRealArea }

procedure TRealArea.Create(ALeft, ATop, ARight, ABottom: Extended);
begin
  FTopLeft.Lon := ALeft;
  FTopLeft.Lat := ATop;
  FBottomRight.Lon := ARight;
  FBottomRight.Lat := ABottom;
  Normalize;
end;

procedure TRealArea.Create(ATopLeft, ABottomRight: TRealPoint);
begin
  FTopLeft := ATopLeft;
  FBottomRight := ABottomRight;
  Normalize;
end;

{ Calculates the intersection with the given Area.
  NOTE: Function Intersects() must be called before in order to make sure that
  an intersection exists at all. }
function TRealArea.Intersection(const Area: TRealArea; out Common: TRealArea): Boolean;
begin
  if Intersects(Area) then
  begin
    Common := self;
    if Common.TopLeft.Lon < Area.TopLeft.Lon then
      Common.FTopLeft.Lon := Area.TopLeft.Lon;
    if Common.TopLeft.Lat > Area.TopLeft.Lat then
      Common.FTopLeft.Lat := Area.TopLeft.Lat;
    if Common.BottomRight.Lon > Area.BottomRight.Lon then
      Common.FBottomRight.Lon := Area.BottomRight.Lon;
    if Common.BottomRight.Lat < Area.BottomRight.Lat then
      Common.FBottomRight.Lat := Area.BottomRight.Lat;
    Result := true;
  end else
  begin
    Common.Create(0, 0, 0, 0);
    Result := false;
  end;
end;

function TRealArea.Intersects(const Area: TRealArea): Boolean;
begin
  Result := (FTopLeft.Lon <= Area.BottomRight.Lon) and
            (FBottomRight.Lon >= Area.TopLeft.Lon) and
            (FTopLeft.Lat >= Area.BottomRight.Lat) and
            (FBottomRight.Lat <= Area.TopLeft.Lat);
end;

procedure TRealArea.Normalize;
begin
  if (FTopLeft.Lon > 0) and (FBottomRight.Lon < 0) then
  begin
    while FTopLeft.Lon > FBottomRight.Lon do
      FTopLeft.Lon := FTopLeft.Lon - 360.0;
  end else
  begin
    while FBottomRight.Lon < FTopLeft.Lon do
      FBottomRight.Lon := FBottomRight.Lon + 360.0;
  end;
end;

{ Returns true when the point Pt is inside the area. }
function TRealArea.PointInside(const Pt: TRealPoint): Boolean;
begin
  Result := InRange(Pt.Lon, FTopLeft.Lon, FBottomRight.Lon) and
            InRange(Pt.Lat, FTopLeft.Lat, FBottomRight.Lat);
end;

function TRealArea.Union(const Area: TRealArea): TRealArea;
begin
  Result := Self;

  if Result.TopLeft.Lon > Area.TopLeft.Lon then
    Result.FTopLeft.Lon := Area.TopLeft.Lon;
  if Result.BottomRight.Lon < Area.BottomRight.Lon then
    Result.FBottomRight.Lon := Area.BottomRight.Lon;

  if Result.TopLeft.Lat < Area.TopLeft.Lat then
    Result.FTopLeft.Lat := Area.TopLeft.Lat;
  if Result.BottomRight.Lat > Area.BottomRight.Lat then
    Result.FBottomRight.Lat := Area.BottomRight.Lat;
end;


end.

