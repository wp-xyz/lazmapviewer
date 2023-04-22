unit mvMiscTests_Engine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TMiscTests_Engine= class(TTestCase)
  published
    procedure Test_Distance;
    procedure Test_LatToStr_DMS;
    procedure Test_LatToStr_Deg;
    procedure Test_LonToStr_DMS;
    procedure Test_LonToStr_Deg;
    procedure Test_SplitGPS;
    procedure Test_ZoomFactor;
  end;

implementation

uses
  Math, mvEngine;

type
  TDistanceRec = record
    Name1: String;
    Lat1, Lon1: Double;
    Name2: String;
    Lat2, Lon2: Double;
    Distance_km: Double;
  end;

const
  Distance_TestData: array[0..2] of TDistanceRec = (
    // Calculated on https://keisan.casio.com/exec/system/1224587128 for R=6378km
    (Name1: 'Sydney'; Lat1:-33.865143; Lon1:151.209900;
     Name2: 'San Francisco'; Lat2:37.828724; Lon2:-122.355537;
     Distance_km: 11968),
    (Name1: 'London'; Lat1: 51.503368; Lon1: -0.127721;
     Name2: 'Istanbul'; Lat2: 41.276901; Lon2: 28.729324;
     Distance_km: 2468.6),
    (Name1: 'Tokyo'; Lat1:35.652832; Lon1:139.839478;
     Name2: 'Singapore'; Lat2:1.290270; Lon2:103.851959;
     Distance_km: 5331.97)
  );

type
  TLatLonRec = record
    Name: String;
    Lat: Double;
    Lat_Deg: String;
    Lat_DMS: String;
    Lat_D, Lat_M: Integer;
    Lat_S: Double;
    Lon: Double;
    Lon_Deg: String;
    Lon_DMS: String;
    Lon_D, Lon_M: Integer;
    Lon_S: Double;
  end;

const
  LatLon_TestData: array[0..7] of TLatLonRec = (
    (Name:'Sydney';   // https://www.latlong.net/place/sydney-nsw-australia-700.html
      Lat:-33.865143; Lat_Deg:'33.865143° S'; Lat_DMS:'33° 51'' 54.5148" S';  Lat_D:-33; Lat_M:51; Lat_S:54.5148;
      Lon:151.209900; Lon_Deg:'151.209900° E'; Lon_DMS:'151° 12'' 35.6400" E'; Lon_D:151; Lon_M:12; Lon_S:35.64),
    (Name:'San Francisco';  // https://www.latlong.net/place/san-francisco-bay-area-ca-usa-32614.html
      Lat:37.828724; Lat_Deg:'37.828724° N'; Lat_DMS:'37° 49'' 43.4064" N'; Lat_D:37; Lat_M:49; Lat_S:43.4064;
      Lon:-122.355537; Lon_Deg:'122.355537° W'; Lon_DMS:'122° 21'' 19.9332" W'; Lon_D:-122; Lon_M:21; Lon_S:19.9332),
    (Name:'London';  // https://www.latlong.net/place/10-downing-street-london-uk-32612.html
      Lat:51.503368; Lat_Deg:'51.503368° N'; Lat_DMS:'51° 30'' 12.1248" N'; Lat_D:51; lat_M:30; Lat_S:12.1248;
      Lon:-0.127721; Lon_Deg:'0.127721° W'; Lon_DMS:'0° 7'' 39.7956" W'; Lon_D:0; Lon_M:7; Lon_S:39.7956),
    (Name:'Istanbul';  // https://www.latlong.net/place/istanbul-airport-turkey-32591.html
      Lat:41.276901; Lat_Deg:'41.276901° N'; Lat_DMS:'41° 16'' 36.8436" N'; Lat_D:41; Lat_M:16; Lat_S:36.8436;
      Lon:28.729324; Lon_Deg:'28.729324° E'; Lon_DMS:'28° 43'' 45.5664" E'; Lon_D:28; Lon_M:43; Lon_S:45.5664),
    (Name:'Tokyo';    // https://www.latlong.net/place/tokyo-japan-8040.html
      Lat:35.652832; Lat_Deg:'35.652832° N'; Lat_DMS:'35° 39'' 10.1952" N'; Lat_D:35; Lat_M:39; Lat_S:10.1952;
      Lon:139.839478; Lon_Deg:'139.839478° E'; Lon_DMS:'139° 50'' 22.1208" E'; Lon_D:139; Lon_M:50; Lon_S:22.1208),
    (Name:'Singapore';  // https://www.latlong.net/place/singapore-788.html
      Lat:1.290270; Lat_Deg:'1.290270° N'; Lat_DMS:'1° 17'' 24.9720" N'; Lat_D:1; Lat_M:17; Lat_S:24.9720;
      Lon:103.851959; Lon_Deg:'103.851959° E'; Lon_DMS:'103° 51'' 7.0524" E'; Lon_D:103; Lon_M:51; Lon_S:7.0524),
    (Name:'Lima';   // https://www.latlong.net/place/lima-city-lima-province-peru-6919.html
      Lat:-12.046374; Lat_Deg:'12.046374° S'; Lat_DMS:'12° 2'' 46.9464" S'; Lat_D:-12; Lat_M:2; Lat_S:46.9464;
      Lon:-77.042793; Lon_Deg:'77.042793° W'; Lon_DMS:'77° 2'' 34.0548" W'; Lon_D:-77; Lon_M:2; Lon_S:34.0548),
    (Name: 'Johannesburg';  // https://www.latlong.net/place/johannesburg-south-africa-1083.html
      Lat:-26.195246; Lat_Deg:'26.195246° S'; Lat_DMS:'26° 11'' 42.8856" S'; Lat_D:-26; Lat_M:11; Lat_S:42.8856;
      Lon:28.034088; Lon_Deg:'28.034088° E'; Lon_DMS:'28° 2'' 2.7168" E'; Lon_D:28; Lon_M:2; Lon_S:2.7168)
  );


var
  PointFormatsettings: TFormatSettings;

procedure TMiscTests_Engine.Test_Distance;
const
  TOLERANCE = 2;
  RADIUS = 6378; // Earth radius in km, as used by the references
var
  i: Integer;
begin
  for i := 0 to High(Distance_TestData) do
    with Distance_TestData[i] do
      AssertEquals(
        'Distance mismatch between ' + Name1 + ' and ' + Name2,
        Distance_km,
        HaverSineDist(Lat1, Lon1, Lat2, Lon2, RADIUS), TOLERANCE
      );
end;

procedure TMiscTests_Engine.Test_LatToStr_Deg;
const
  NO_DMS = false;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lat_Deg,                                     // expected
        LatToStr(Lat, NO_DMS, PointFormatSettings)   // actual
      );
end;

procedure TMiscTests_Engine.Test_LatToStr_DMS;
const
  NEED_DMS = true;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lat_DMS,                                      // expected
        LatToStr(Lat, NEED_DMS, PointFormatSettings)  // actual
      );
end;

procedure TMiscTests_Engine.Test_LonToStr_Deg;
const
  NO_DMS = false;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lon_Deg,                                    // expected
        LonToStr(Lon, NO_DMS, PointFormatSettings)   // actual
      );
end;

procedure TMiscTests_Engine.Test_LonToStr_DMS;
const
  NEED_DMS = true;
var
  i: Integer;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
      AssertEquals(
        'Latitude string mismatch for ' + Name,
        Lon_DMS,                                       // expected
        LonToStr(Lon, NEED_DMS, PointFormatSettings)   // actual
      );
end;

procedure TMiscTests_Engine.Test_SplitGPS;
const
  TOLERANCE = 1e-5;
var
  i: Integer;
  D, M, S: double;
begin
  for i := 0 to High(LatLon_TestData) do
    with LatLon_TestData[i] do
    begin
      SplitGPS(Lat, D, M, S);
      AssertEquals(
        'Latitude degrees mismatch for ' + Name,
        Lat_D,       // expected
        round(D)     // actual
      );
      AssertEquals(
        'Latitude minutes mismatch for ' + Name,
        Lat_M,       // expected
        round(M)     // actual
      );
      AssertEquals(
        'Latitude seconds mismatch for ' + Name,
        Lat_S,
        S,
        TOLERANCE
      );

      SplitGPS(Lon, D, M, S);
      AssertEquals(
        'Longitude degrees mismatch for ' + Name,
        Lon_D,       // expected
        round(D)     // actual
      );
      AssertEquals(
        'Longitude minutes mismatch for ' + Name,
        Lon_M,       // expected
        round(M)     // actual
      );
      AssertEquals(
        'Longitude seconds mismatch for ' + Name,
        Lon_S,
        S,
        TOLERANCE
      );
    end;
end;

procedure TMiscTests_Engine.Test_ZoomFactor;
var
  z: Integer;
  f: Extended;
begin
  for z := 0 to 32 do
  begin
    f := ZoomFactor(z);
    AssertEquals('Zoomlevel lookup failure at ' + IntToStr(z), f, IntPower(2, z))
  end;
end;


initialization
  PointFormatSettings := DefaultFormatSettings;
  PointFormatSettings.DecimalSeparator := '.';
  DMS_Decimals := 4;

  RegisterTest(TMiscTests_Engine);
end.

