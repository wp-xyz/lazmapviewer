{
  (c) 2014 ti_dic@hotmail.com

  Parts of this component are based on :
    Map Viewer Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}


unit mvEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, Controls, Math, GraphType, FPImage,
  mvTypes, mvJobQueue, mvMapProvider, mvDownloadEngine, mvCache, mvDragObj;

const
  EARTH_EQUATORIAL_RADIUS = 6378137;
  EARTH_POLAR_RADIUS = 6356752.3142;
  EARTH_CIRCUMFERENCE = 2 * pi * EARTH_EQUATORIAL_RADIUS;
  EARTH_ECCENTRICITY = sqrt(1 - sqr(EARTH_POLAR_RADIUS / EARTH_EQUATORIAL_RADIUS));

type
  TDrawTileEvent = procedure (const TileId: TTileId; X,Y: integer;
    TileImg: TLazIntfImage) of object;

  TDrawStretchedTileEvent = procedure (const TileId: TTileId; X,Y: Integer;
    TileImg: TLazIntfImage; const R: TRect) of object;

  TTileIdArray = Array of TTileId;

  TDistanceUnits = (duMeters, duKilometers, duMiles);

  { TMapWindow }

  TMapWindow = Record
    MapProvider: TMapProvider;
    X: Int64;
    Y: Int64;
    Center: TRealPoint;
    Zoom: integer;
    ZoomCenter: TRealPoint;
    ZoomOffset: TPoint;
    Height: integer;
    Width: integer;
  end;


  { TMapViewerEngine }

  TMapViewerEngine = Class(TComponent)
    private
      DragObj : TDragObj;
      Cache : TPictureCache;
      FActive: boolean;
      FBkColor: TFPColor;
      FCyclic: Boolean;
      FDownloadEngine: TMvCustomDownloadEngine;
      FDrawPreviewTiles: Boolean;
      FDrawTitleInGuiThread: boolean;
      FOnCenterMove: TNotifyEvent;
      FOnChange: TNotifyEvent;
      FOnDrawTile: TDrawTileEvent;
      FOnDrawStretchedTile: TDrawStretchedTileEvent;
      FOnZoomChange: TNotifyEvent;
      lstProvider : TStringList;
      Queue : TJobQueue;
      MapWin : TMapWindow;
      FZoomToCursor: Boolean;
      function GetCacheOnDisk: Boolean;
      function GetCachePath: String;
      function GetCenter: TRealPoint;
      function GetHeight: integer;
      function GetMapProvider: String;
      function GetUseThreads: Boolean;
      function GetWidth: integer;
      function GetZoom: integer;
      function IsValidTile(const aWin: TMapWindow; const aTile: TTIleId): boolean;
      procedure MoveMapCenter(Sender: TDragObj);
      procedure SetActive(AValue: boolean);
      procedure SetBkColor(AValue: TFPColor);
      procedure SetCacheOnDisk(AValue: Boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCenter(ACenter: TRealPoint);
      procedure SetCyclic(AValue: Boolean);
      procedure SetDownloadEngine(AValue: TMvCustomDownloadEngine);
      procedure SetHeight(AValue: integer);
      procedure SetMapProvider(AValue: String);
      procedure SetUseThreads(AValue: Boolean);
      procedure SetWidth(AValue: integer);
      procedure SetZoom(AValue: Integer); overload;
      procedure SetZoom(AValue: integer; AZoomToCursor: Boolean); overload;
      function DegreesToMapPixels(const AWin: TMapWindow; ALonLat: TRealPoint): TPoint;
      function MapPixelsToDegrees(const AWin: TMapWindow; APoint: TPoint): TRealPoint;
      function PixelsToDegreesEPSG3395(APoint: TPoint; Zoom: Integer): TRealPoint;
      function PixelsToDegreesEPSG3857(APoint: TPoint; Zoom: Integer): TRealPoint;
      procedure CalculateWin(var AWin: TMapWindow);
      function DegreesToPixelsEPSG3395(const AWin: TMapWindow; ALonLat: TRealPoint): TPoint;
      function DegreesToPixelsEPSG3857(const AWin: TMapWindow; ALonLat: TRealPoint): TPoint;
      procedure Redraw(const aWin: TMapWindow);
      function CalculateVisibleTiles(const aWin: TMapWindow) : TArea;
      function IsCurrentWin(const aWin: TMapWindow) : boolean;
    protected
      procedure AdjustZoomCenter(var AWin: TMapWindow);
      procedure ConstraintZoom(var aWin: TMapWindow);
      function GetTileName(const Id: TTileId): String;
      procedure evDownload(Data: TObject; Job: TJob);
      procedure TileDownloaded(Data: PtrInt);
      procedure DrawStretchedTile(const TileId: TTileID; X, Y: Integer; TileImg: TLazIntfImage; const R: TRect);
      Procedure DrawTile(const TileId: TTileId; X,Y: integer; TileImg: TLazIntfImage);
      Procedure DoDrag(Sender: TDragObj);
    public
      constructor Create(aOwner: TComponent); override;
      destructor Destroy; override;

      function AddMapProvider(OpeName: String; ProjectionType: TProjectionType; Url: String;
        MinZoom, MaxZoom, NbSvr: integer; GetSvrStr: TGetSvrStr = nil;
        GetXStr: TGetValStr = nil; GetYStr: TGetValStr = nil;
        GetZStr: TGetValStr = nil): TMapProvider;
      procedure CancelCurrentDrawing;
      procedure ClearMapProviders;
      function CrossesDateline: Boolean;
      procedure GetMapProviders(AList: TStrings);
      function LonLatToScreen(ALonLat: TRealPoint): TPoint;
      function LonLatToWorldScreen(ALonLat: TRealPoint): TPoint;
      function ReadProvidersFromXML(AFileName: String; out AMsg: String): Boolean;
      procedure Redraw;
      Procedure RegisterProviders;
      function ScreenToLonLat(aPt: TPoint): TRealPoint;
      procedure SetSize(aWidth, aHeight: integer);
      function WorldScreenToLonLat(aPt: TPoint): TRealPoint;
      procedure WriteProvidersToXML(AFileName: String);

      procedure DblClick(Sender: TObject);
      procedure MouseDown(Sender: TObject; Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer);
      procedure MouseMove(Sender: TObject; {%H-}Shift: TShiftState;
        X, Y: Integer);
      procedure MouseUp(Sender: TObject; Button: TMouseButton;
        {%H-}Shift: TShiftState; X, Y: Integer);
      procedure MouseWheel(Sender: TObject; {%H-}Shift: TShiftState;
        WheelDelta: Integer; {%H-}MousePos: TPoint; var Handled: Boolean);
      procedure ZoomOnArea(const aArea: TRealArea);

      property BkColor: TFPColor read FBkColor write SetBkColor;
      property Center: TRealPoint read GetCenter write SetCenter;
      property DrawPreviewTiles : Boolean read FDrawPreviewTiles write FDrawPreviewTiles;

    published
      property Active: Boolean read FActive write SetActive default false;
      property CacheOnDisk: Boolean read GetCacheOnDisk write SetCacheOnDisk;
      property CachePath: String read GetCachePath write SetCachePath;
      property Cyclic: Boolean read FCyclic write SetCyclic default false;
      property DownloadEngine: TMvCustomDownloadEngine
        read FDownloadEngine write SetDownloadEngine;
      property DrawTitleInGuiThread: boolean
        read FDrawTitleInGuiThread write FDrawTitleInGuiThread;
      property Height: integer read GetHeight write SetHeight;
      property JobQueue: TJobQueue read Queue;
      property MapProvider: String read GetMapProvider write SetMapProvider;
      property UseThreads: Boolean read GetUseThreads write SetUseThreads;
      property Width: integer read GetWidth write SetWidth;
      property Zoom: integer read GetZoom write SetZoom;
      property ZoomToCursor: Boolean read FZoomToCursor write FZoomToCursor default True;

      property OnCenterMove: TNotifyEvent read FOnCenterMove write FOnCenterMove;
      property OnChange: TNotifyEvent Read FOnChange write FOnchange; //called when visiable area change
      property OnDrawStretchedTile: TDrawStretchedTileEvent read FOnDrawStretchedTile write FOnDrawStretchedTile;
      property OnDrawTile: TDrawTileEvent read FOnDrawTile write FOnDrawTile;
      property OnZoomChange: TNotifyEvent read FOnZoomChange write FOnZoomChange;
  end;

function RealPoint(Lat, Lon: Double): TRealPoint;

function HaversineDist(Lat1, Lon1, Lat2, Lon2, Radius: Double): Double;
function CalcGeoDistance(Lat1, Lon1, Lat2, Lon2: double;
  AUnits: TDistanceUnits = duKilometers): double;

function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
function GPSToDMS(Angle: Double): string;
function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string;

function LatToStr(ALatitude: Double; DMS: Boolean): String;
function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
function LonToStr(ALongitude: Double; DMS: Boolean): String;
function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;

procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double);

function ZoomFactor(AZoomLevel: Integer): Int64;

var
  HERE_AppID: String = '';
  HERE_AppCode: String = '';
  OpenWeatherMap_ApiKey: String = '';
  ThunderForest_ApiKey: String = '';

  DMS_Decimals: Integer = 1;


implementation

uses
  Forms, TypInfo, laz2_xmlread, laz2_xmlwrite, laz2_dom,
  mvJobs, mvGpsObj;

const
  _K = 1024;
  _M = _K*_K;
  _G = _K*_M;
  ZOOM_FACTOR: array[0..32] of Int64 = (
     1, 2, 4, 8, 16, 32, 64, 128, 256, 512,                             //  0..9
    _K, 2*_K, 4*_K, 8*_K, 16*_K, 32*_K, 64*_K, 128*_K, 256*_K, 512*_K,  // 10..19
    _M, 2*_M, 4*_M, 8*_M, 16*_M, 32*_M, 64*_M, 128*_M, 256*_M, 512*_M,  // 20..29
    _G, 2*_G, 4*_G                                                      // 30..32
  );

function ZoomFactor(AZoomLevel: Integer): Int64;
begin
  if (AZoomLevel >= Low(ZOOM_FACTOR)) and (AZoomLevel < High(ZOOM_FACTOR)) then
    Result := ZOOM_FACTOR[AZoomLevel]
  else
    Result := round(IntPower(2, AZoomLevel));
end;


type

  { TLaunchDownloadJob }

  TLaunchDownloadJob = class(TJob)
  private
    AllRun: boolean;
    Win: TMapWindow;
    Engine: TMapViewerEngine;
    FRunning: boolean;
    FTiles: TTileIdArray;
    FStates: Array of integer;
  protected
    function pGetTask: integer; override;
    procedure pTaskStarted(aTask: integer); override;
    procedure pTaskEnded(aTask: integer; aExcept: Exception); override;
  public
    procedure ExecuteTask(aTask: integer; FromWaiting: boolean); override;
    function Running: boolean; override;
  public
    constructor Create(Eng: TMapViewerEngine; const Tiles: TTileIdArray;
      const aWin: TMapWindow);
  end;


  { TEnvTile }

  TEnvTile = Class(TBaseTile)
  private
    Tile: TTileId;
    Win: TMapWindow;
  public
    constructor Create(const aTile: TTileId; const aWin: TMapWindow);reintroduce;
  end;


  { TMemObj }

  TMemObj = Class
  private
    FWin: TMapWindow;
  public
    constructor Create(const aWin: TMapWindow);
  end;

  constructor TMemObj.Create(const aWin: TMapWindow);
  begin
    FWin := aWin;
  end;


{ TLaunchDownloadJob }

function TLaunchDownloadJob.pGetTask: integer;
var
  i: integer;
begin
  if not AllRun and not Cancelled then
  begin
    for i:=Low(FStates) to High(FStates) do
      if FStates[i] = 0 then
      begin
        Result := i + 1;
        Exit;
      end;
    AllRun := True;
  end;
  Result := ALL_TASK_COMPLETED;
  for i := Low(FStates) to High(FStates) do
    if FStates[i] = 1 then
    begin
      Result := NO_MORE_TASK;
      Exit;
    end;
end;

procedure TLaunchDownloadJob.pTaskStarted(aTask: integer);
begin
  FRunning := True;
  FStates[aTask-1] := 1;
end;

procedure TLaunchDownloadJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask - 1] := 3
  Else
    FStates[aTask - 1] := 2;
end;

procedure TLaunchDownloadJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var
  iTile: integer;
  lJob: TEventJob;
  lTile: TEnvTile;
begin
  iTile := aTask - 1;
  lTile:=TEnvTile.Create(FTiles[iTile], Win);
  lJob := TEventJob.Create
    (
      @Engine.evDownload,
      lTile,
      false,                                    // owns data
      Engine.GetTileName(FTiles[iTile])
    );
  if not Queue.AddUniqueJob(lJob  ,
    Launcher
  ) then
     begin
       FreeAndNil(lJob);
       FreeAndNil(lTile);
     end;
end;

function TLaunchDownloadJob.Running: boolean;
begin
  Result := FRunning;
end;

constructor TLaunchDownloadJob.Create(Eng: TMapViewerEngine;
  const Tiles: TTileIdArray; const aWin: TMapWindow);
var
  i: integer;
begin
  Engine := Eng;
  SetLength(FTiles, Length(Tiles));
  For i:=Low(FTiles) to High(FTiles) do
    FTiles[i] := Tiles[i];
  SetLength(FStates, Length(Tiles));
  AllRun := false;
  Name := 'LaunchDownload';
  Win := aWin;
end;


{ TEnvTile }

constructor TEnvTile.Create(const aTile: TTileId; const aWin: TMapWindow);
begin
  inherited Create(aWin.MapProvider);
  Tile := aTile;
  Win := aWin;
end;


{ TMapViewerEngine }

constructor TMapViewerEngine.Create(aOwner: TComponent);
begin
  DrawTitleInGuiThread := true;
  DrawPreviewTiles := true;
  DragObj := TDragObj.Create;
  DragObj.OnDrag := @DoDrag;
  Cache := TPictureCache.Create(self);
  lstProvider := TStringList.Create;
  FBkColor := colWhite;
  RegisterProviders;
  Queue := TJobQueue.Create(8);
  Queue.OnIdle := @Cache.CheckCacheSize;

  inherited Create(aOwner);

  FZoomToCursor := true;
  ConstraintZoom(MapWin);
  CalculateWin(mapWin);
end;

destructor TMapViewerEngine.Destroy;
begin
  ClearMapProviders;
  FreeAndNil(DragObj);
  FreeAndNil(lstProvider);
  FreeAndNil(Cache);
  FreeAndNil(Queue);
  inherited Destroy;
end;

function TMapViewerEngine.AddMapProvider(OpeName: String; ProjectionType: TProjectionType;
  Url: String; MinZoom, MaxZoom, NbSvr: integer; GetSvrStr: TGetSvrStr;
  GetXStr: TGetValStr; GetYStr: TGetValStr; GetZStr: TGetValStr): TMapProvider;
var
  idx :integer;
Begin
  idx := lstProvider.IndexOf(OpeName);
  if idx = -1 then
  begin
    Result := TMapProvider.Create(OpeName);
    lstProvider.AddObject(OpeName, Result);
  end
  else
    Result := TMapProvider(lstProvider.Objects[idx]);
  Result.AddUrl(Url, ProjectionType, NbSvr, MinZoom, MaxZoom, GetSvrStr, GetXStr, GetYStr, GetZStr);
end;

procedure TMapViewerEngine.AdjustZoomCenter(var AWin: TMapWindow);
var
  ptMouseCursor: TPoint;
  rPtAdjustedCenter: TRealPoint;
begin
  ptMouseCursor := LonLatToScreen(AWin.ZoomCenter);
  rPtAdjustedCenter := ScreenToLonLat(ptMouseCursor.Add(AWin.ZoomOffset));
  AWin.Center := rPtAdjustedCenter;
  CalculateWin(AWin);
end;

function TMapViewerEngine.CalculateVisibleTiles(const aWin: TMapWindow): TArea;
var
  MaxX, MaxY, startX, startY: int64;
begin
  MaxX := (Int64(aWin.Width) div TILE_SIZE) + 1;
  MaxY := (Int64(aWin.Height) div TILE_SIZE) + 1;
  startX := -aWin.X div TILE_SIZE;
  startY := -aWin.Y div TILE_SIZE;
  Result.Left := startX - 1;
  Result.Right := startX + MaxX;
  Result.Top := startY - 1;
  Result.Bottom := startY + MaxY;
end;

procedure TMapViewerEngine.CalculateWin(var AWin: TMapWindow);
var
  PixelLocation: TPoint; // review: coth: Should it use Int64?
begin
  case AWin.MapProvider.ProjectionType of
    ptEPSG3857: PixelLocation := DegreesToPixelsEPSG3857(AWin, AWin.Center);
    ptEPSG3395: PixelLocation := DegreesToPixelsEPSG3395(AWin, AWin.Center);
    else PixelLocation := DegreesToPixelsEPSG3857(AWin, AWin.Center);
  end;

  AWin.X := Int64(AWin.Width div 2) - PixelLocation.x;
  AWin.Y := Int64(AWin.Height div 2) - PixelLocation.y;
end;

procedure TMapViewerEngine.CancelCurrentDrawing;
var
  Jobs: TJobArray;
begin
  Jobs := Queue.CancelAllJob(self);
  Queue.WaitForTerminate(Jobs);
end;

procedure TMapViewerEngine.ClearMapProviders;
var
  i: Integer;
begin
  for i:=0 to lstProvider.Count-1 do
    TObject(lstProvider.Objects[i]).Free;
  lstProvider.Clear;
end;

procedure TMapViewerEngine.ConstraintZoom(var aWin: TMapWindow);
var
  zMin, zMax: integer;
begin
  if Assigned(aWin.MapProvider) then
  begin
    aWin.MapProvider.GetZoomInfos(zMin, zMax);
    if aWin.Zoom < zMin then
      aWin.Zoom := zMin;
    if aWin.Zoom > zMax then
      aWin.Zoom := zMax;
  end;
end;

{ Returns true when the visible window crosses the date line, i.e. the longitudes
  at the left of the window are > 0, and those at the right are < 0. }
function TMapViewerEngine.CrossesDateline: Boolean;
var
  visArea: TRealArea;
  mapWidth: Int64;
begin
  // Catch the case, that the screen is wider than the whole world
  mapWidth := ZoomFactor(MapWin.Zoom) * TILE_SIZE;
  Result := (MapWin.Width > mapWidth);
  if not Result then
  begin
    visArea.TopLeft := ScreenToLonLat(Point(0, 0));
    visArea.BottomRight := ScreenToLonLat(Point(Width, Height));
    Result := (visArea.TopLeft.Lon > 0) and (visArea.BottomRight.Lon < 0);
  end;
end;

procedure TMapViewerEngine.DblClick(Sender: TObject);
var
  pt: TPoint;
begin
  pt.X := DragObj.MouseX;
  pt.Y := DragObj.MouseY;
  SetCenter(ScreenToLonLat(pt));
end;

procedure TMapViewerEngine.DoDrag(Sender: TDragObj);
begin
  if Sender.DragSrc = self then
    MoveMapCenter(Sender);
end;

procedure TMapViewerEngine.DrawStretchedTile(const TileID: TTileID; X, Y: Integer;
  TileImg: TLazIntfImage; const R: TRect);
begin
  if Assigned(FOnDrawStretchedTile) then
    FOnDrawStretchedTile(TileId, X, Y, TileImg, R);
end;

procedure TMapViewerEngine.DrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TLazIntfImage);
begin
  if Assigned(FOnDrawTile) then
    FOnDrawTile(TileId, X, Y, TileImg);
end;

procedure TMapViewerEngine.evDownload(Data: TObject; Job: TJob);
var
  Id: TTileId;
  Url: String;
  Env: TEnvTile;
  MapO: TMapProvider;
  lStream: TMemoryStream;
begin
  Env := TEnvTile(Data);
  Id := Env.Tile;
  MapO := Env.Win.MapProvider;
  if Assigned(MapO) and Assigned(Cache) then
  begin
    if not Cache.InCache(MapO, Id) then
    begin
      if Assigned(FDownloadEngine) then
      begin
        Url := MapO.GetUrlForTile(Id);
        if Url <> '' then
        begin
          lStream := TMemoryStream.Create;
          try
            try
              FDownloadEngine.DownloadFile(Url, lStream);
              if Assigned(Cache) then Cache.Add(MapO, Id, lStream);
            except
            end;
          finally
            FreeAndNil(lStream);
          end;
        end;
      end;
    end;
  end;

  if Job.Cancelled then
    Exit;

  if DrawTitleInGuiThread then
    Queue.QueueAsyncCall(@TileDownloaded, PtrInt(Env))
  else
    TileDownloaded(PtrInt(Env));
end;

function TMapViewerEngine.GetCacheOnDisk: Boolean;
begin
  Result := Cache.UseDisk;
end;

function TMapViewerEngine.GetCachePath: String;
begin
  Result := Cache.BasePath;
end;

function TMapViewerEngine.GetCenter: TRealPoint;
begin
  Result := MapWin.Center;
end;

function TMapViewerEngine.GetHeight: integer;
begin
  Result := MapWin.Height
end;

function TMapViewerEngine.GetMapProvider: String;
begin
  if Assigned(MapWin.MapProvider) then
    Result := MapWin.MapProvider.Name
  else
    Result := '';
end;

procedure TMapViewerEngine.GetMapProviders(AList: TStrings);
begin
  AList.Assign(lstProvider);
end;

function TMapViewerEngine.GetTileName(const Id: TTileId): String;
begin
  Result := IntToStr(Id.X) + '.' + IntToStr(Id.Y) + '.' + IntToStr(Id.Z);
end;

function TMapViewerEngine.GetUseThreads: Boolean;
begin
  Result := Queue.UseThreads;
end;

function TMapViewerEngine.GetWidth: integer;
begin
  Result := MapWin.Width;
end;

function TMapViewerEngine.GetZoom: integer;
begin
  Result := MapWin.Zoom;
end;

function TMapViewerEngine.IsCurrentWin(const aWin: TMapWindow): boolean;
begin
  Result := (aWin.Zoom = MapWin.Zoom) and
            (aWin.Center.Lat = MapWin.Center.Lat) and
            (aWin.Center.Lon = MapWin.Center.Lon) and
            (aWin.Width = MapWin.Width) and
            (aWin.Height = MapWin.Height);
end;

function TMapViewerEngine.IsValidTile(const aWin: TMapWindow;
  const aTile: TTileId): boolean;
var
  tiles: int64;
begin
  tiles := 1 shl aWin.Zoom;
  Result := (aTile.X >= 0) and (aTile.X <= tiles-1) and
            (aTile.Y >= 0) and (aTile.Y <= tiles-1);
end;

function TMapViewerEngine.DegreesToMapPixels(const AWin: TMapWindow;
  ALonLat: TRealPoint): TPoint;
var
  pixelLocation: TPoint;
  mapWidth: Int64;
begin
  case AWin.MapProvider.ProjectionType of
    ptEPSG3395: pixelLocation := DegreesToPixelsEPSG3395(AWin, ALonLat);
    ptEPSG3857: pixelLocation := DegreesToPixelsEPSG3857(AWin, ALonLat);
    else pixelLocation := DegreesToPixelsEPSG3857(AWin, ALonLat);
  end;
  Result.X := pixelLocation.x + AWin.X;
  if FCyclic and CrossesDateline then
  begin
    mapWidth := ZoomFactor(AWin.Zoom) * TILE_SIZE;
    while (Result.X < 0) do
      Result.X := Result.X + mapWidth;
    while (Result.X > AWin.Width) do
      Result.X := Result.X - mapWidth;
  end;
  Result.Y := pixelLocation.y + AWin.Y;
end;

// review: coth: Should it use Int64?
function TMapViewerEngine.DegreesToPixelsEPSG3857(const AWin: TMapWindow;
  ALonLat: TRealPoint): TPoint;
const
  MIN_LATITUDE = -85.05112878;
  MAX_LATITUDE = 85.05112878;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
  TWO_PI = 2.0 * pi;
var
  factor, px, py: Extended;
  pt: TRealPoint;
begin
  // https://epsg.io/3857
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 41
  // https://en.wikipedia.org/wiki/Web_Mercator_projection
  pt.Lat := Math.EnsureRange(ALonLat.Lat, MIN_LATITUDE, MAX_LATITUDE);
  pt.Lon := Math.EnsureRange(ALonLat.Lon, MIN_LONGITUDE, MAX_LONGITUDE);

  factor := TILE_SIZE / TWO_PI * ZoomFactor(AWin.Zoom);
  px := factor * (pt.LonRad + pi);
  py := factor * (pi - ln( tan(pi/4 + pt.LatRad/2) ));

  Result.x := Round(px);
  Result.y := Round(py);
end;

// review: coth: Should it use Int64?
function TMapViewerEngine.DegreesToPixelsEPSG3395(const AWin: TMapWindow;
  ALonLat: TRealPoint): TPoint;
const
  MIN_LATITUDE = -80;
  MAX_LATITUDE = 84;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
var
  px, py, lny, sny: Extended;
  pt: TRealPoint;
  cfmpx, cfmpm: Extended;
  Z: Integer;
  zoomfac: Extended;  // 2**Z
begin
  // https://epsg.io/3395
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 44
  pt.Lat := Math.EnsureRange(ALonLat.Lat, MIN_LATITUDE, MAX_LATITUDE);
  pt.Lon := Math.EnsureRange(ALonLat.Lon, MIN_LONGITUDE, MAX_LONGITUDE);

  Z := 23 - AWin.Zoom;
  zoomfac := ZoomFactor(Z);
  cfmpx := IntPower(2, 31);
  cfmpm := cfmpx / EARTH_CIRCUMFERENCE;
  px := (EARTH_CIRCUMFERENCE/2 + EARTH_EQUATORIAL_RADIUS * pt.LonRad) * cfmpm / zoomfac;

  sny := EARTH_ECCENTRICITY * sin(pt.LatRad);
  lny := tan(pi/4 + pt.LatRad/2) * power((1-sny)/(1+sny), EARTH_ECCENTRICITY/2);
  py := (EARTH_CIRCUMFERENCE/2 - EARTH_EQUATORIAL_RADIUS * ln(lny)) * cfmpm / zoomfac;

  Result.x := Round(px);
  Result.y := Round(py);
end;

function TMapViewerEngine.LonLatToScreen(ALonLat: TRealPoint): TPoint;
Begin
  Result := DegreesToMapPixels(MapWin, ALonLat);
end;

function TMapViewerEngine.LonLatToWorldScreen(ALonLat: TRealPoint): TPoint;
begin
  Result := LonLatToScreen(ALonLat);
  Result.X := Result.X + MapWin.X;
  Result.Y := Result.Y + MapWin.Y;
end;

function TMapViewerEngine.MapPixelsToDegrees(const AWin: TMapWindow;
  APoint: TPoint): TRealPoint;
var
  mapWidth: Int64;
  mPoint : TPoint;
begin
  mapWidth := round(ZoomFactor(AWin.Zoom)) * TILE_SIZE;

  if FCyclic then
  begin
    mPoint.X := (APoint.X - AWin.X) mod mapWidth;
    while mPoint.X < 0 do
      mPoint.X := mPoint.X + mapWidth;
    while mPoint.X >= mapWidth do
      mPoint.X := mPoint.X - mapWidth;
  end else
    mPoint.X := EnsureRange(APoint.X - AWin.X, 0, mapWidth);
  mPoint.Y := EnsureRange(APoint.Y - AWin.Y, 0, mapWidth);

  case aWin.MapProvider.ProjectionType of
    ptEPSG3857: Result := PixelsToDegreesEPSG3857(mPoint, AWin.Zoom);
    ptEPSG3395: Result := PixelsToDegreesEPSG3395(mPoint, AWin.Zoom);
    else        Result := PixelsToDegreesEPSG3857(mPoint, AWin.Zoom);
  end;
end;

function TMapViewerEngine.PixelsToDegreesEPSG3857(APoint: TPoint; Zoom: Integer): TRealPoint;
const
  MIN_LATITUDE = -85.05112878;
  MAX_LATITUDE = 85.05112878;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
var
  zoomfac: Int64;
begin
  // https://epsg.io/3857
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 41

  // note: coth: ** for better readability, but breaking OmniPascal in VSCode
  // Result.LonRad := ( APoints.X / (( TILE_SIZE / (2*pi)) * 2**Zoom) ) - pi;
  // Result.LatRad := arctan( sinh(pi - (APoints.Y/TILE_SIZE) / 2**Zoom * pi*2) );
  zoomFac := ZoomFactor(Zoom);
  Result.LonRad := ( APoint.X / (( TILE_SIZE / (2*pi)) * zoomFac) ) - pi;
  Result.LatRad := arctan( sinh(pi - (APoint.Y/TILE_SIZE) / zoomFac * pi*2) );

  Result.Lat := Math.EnsureRange(Result.Lat, MIN_LATITUDE, MAX_LATITUDE);
  Result.Lon := Math.EnsureRange(Result.Lon, MIN_LONGITUDE, MAX_LONGITUDE);
end;

Function TMapViewerEngine.PixelsToDegreesEPSG3395(APoint: TPoint; Zoom: Integer): TRealPoint;

  function PhiIteration(y, phi: Extended): Extended;
  var
    t: Extended;
    sin_phi: Extended;
    arg: Extended;
  begin
    t := exp(y/EARTH_EQUATORIAL_RADIUS);
    sin_phi := sin(phi);
    arg := (1 - EARTH_ECCENTRICITY * sin_phi) / (1 + EARTH_ECCENTRICITY * sin_phi);
    Result := pi/2 - 2*arctan( t * Math.power(arg, EARTH_ECCENTRICITY/2) );
  end;

const
  MIN_LATITUDE = -80;
  MAX_LATITUDE = 84;
  MIN_LONGITUDE = -180;
  MAX_LONGITUDE = 180;
  EPS = 1e-8;
var
  LonRad, LatRad: Extended;
  WorldSize: Int64;
  Cpm: Extended;
  Z: Integer;
  t, phi:  Extended;
  zoomFac: Int64;
  i: Integer;
begin
  // https://epsg.io/3395
  // https://pubs.usgs.gov/pp/1395/report.pdf, page 44

  Z := 23 - Zoom;
  zoomFac := ZoomFactor(Z);
  WorldSize := ZoomFactor(31);
  Cpm :=  WorldSize / EARTH_CIRCUMFERENCE;

  LonRad := (APoint.x / (Cpm/zoomFac) - EARTH_CIRCUMFERENCE/2) / EARTH_EQUATORIAL_RADIUS;
  LatRad := (APoint.y / (Cpm/zoomFac) - EARTH_CIRCUMFERENCE/2);

  t := pi/2 - 2*arctan(exp(-LatRad/EARTH_EQUATORIAL_RADIUS));

  i := 0;
  repeat
    phi := t;
    t := PhiIteration(LatRad, phi);
    inc(i);
    if i>10 then
      Break;
      //raise Exception.Create('Phi iteration takes too long.');
  until (abs(phi - t) < EPS);

  LatRad := t;

  Result.LonRad := LonRad;
  Result.LatRad := LatRad;

  Result.Lat := Math.EnsureRange(Result.Lat, MIN_LATITUDE, MAX_LATITUDE);
  Result.Lon := Math.EnsureRange(Result.Lon, MIN_LONGITUDE, MAX_LONGITUDE);
end;

procedure TMapViewerEngine.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DragObj.MouseDown(self,X,Y);
end;

procedure TMapViewerEngine.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  DragObj.MouseMove(X,Y);
end;

procedure TMapViewerEngine.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
    DragObj.MouseUp(X,Y);
end;

procedure TMapViewerEngine.MouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint;
  var Handled: Boolean);
var
  Val: Integer;
  nZoom: integer;
  bZoomToCursor: Boolean;
begin
  bZoomToCursor := False;
  Val := 0;
  if WheelDelta > 0 then
    Val := 1;
  if WheelDelta < 0 then
    Val := -1;
  nZoom := Zoom + Val;
  if (nZoom > 0) and (nZoom < 20) then
  begin
    if ZoomToCursor then
    begin
      MapWin.ZoomCenter := ScreenToLonLat(MousePos);
      MapWin.ZoomOffset := LonLatToScreen(Center).Subtract(MousePos);
      bZoomToCursor := True;
    end;
    SetZoom(nZoom, bZoomToCursor);
  end;
  Handled := true;
end;

procedure TMapViewerEngine.MoveMapCenter(Sender: TDragObj);
var
  old: TMemObj;
  nCenter: TRealPoint;
  aPt: TPoint;
Begin
  if Sender.LnkObj = nil then
    Sender.LnkObj := TMemObj.Create(MapWin);
  old := TMemObj(Sender.LnkObj);
  aPt.X := old.FWin.Width DIV 2-Sender.OfsX;
  aPt.Y := old.FWin.Height DIV 2-Sender.OfsY;
  nCenter := MapPixelsToDegrees(old.FWin,aPt);
  SetCenter(nCenter);
end;

function TMapViewerEngine.ReadProvidersFromXML(AFileName: String;
  out AMsg: String): Boolean;

  function GetSvrStr(AName: String): TGetSvrStr;
  var
    lcName: String;
  begin
    lcName := LowerCase(AName);
    if lcName = LowerCase(SVR_LETTER) then
      Result := @GetSvrLetter
    else if lcName = LowerCase(SVR_BASE1) then
      Result := @GetSvrBase1
    else
      Result := nil;
  end;

  function GetValStr(AName: String): TGetValStr;
  var
    lcName: String;
  begin
    lcName := Lowercase(AName);
    if lcName = LowerCase(STR_QUADKEY) then
      Result := @GetStrQuadKey
    else if lcName = LowerCase(STR_YAHOOY) then
      Result := @GetStrYahooY
    else if lcName = LowerCase(STR_YAHOOZ) then
      Result := @GetStrYahooZ
    else
      Result := nil;
  end;

  function GetAttrValue(ANode: TDOMNode; AttrName: String): String;
  var
    node: TDOMNode;
  begin
    Result := '';
    if ANode.HasAttributes then begin
      node := ANode.Attributes.GetNamedItem(AttrName);
      if Assigned(node) then Result := node.NodeValue;
    end;
  end;

var
  stream: TFileStream;
  doc: TXMLDocument = nil;
  node, layerNode: TDOMNode;
  providerName: String;
  projectionType: TProjectionType;
  url: String;
  minZoom: Integer;
  maxZoom: Integer;
  svrCount: Integer;
  s: String;
  svrProc: String;
  xProc: String;
  yProc: String;
  zProc: String;
  first: Boolean;
begin
  Result := false;
  AMsg := '';
  stream := TFileStream.Create(AFileName, fmOpenread or fmShareDenyWrite);
  try
    ReadXMLFile(doc, stream, [xrfAllowSpecialCharsInAttributeValue, xrfAllowLowerThanInAttributeValue]);
    node := doc.FindNode('map_providers');
    if node = nil then begin
      AMsg := 'No map providers in file.';
      exit;
    end;

    first := true;
    node := node.FirstChild;
    while node <> nil do begin
      providerName := GetAttrValue(node, 'name');
      layerNode := node.FirstChild;
      while layerNode <> nil do begin
        url := GetAttrValue(layerNode, 'url');
        if url = '' then
          continue;
        s := GetAttrValue(layerNode, 'minZom');
        if s = '' then minZoom := 0
          else minZoom := StrToInt(s);
        s := GetAttrValue(layerNode, 'maxZoom');
        if s = '' then maxzoom := 9
          else maxZoom := StrToInt(s);
        s := GetAttrValue(layerNode, 'serverCount');
        if s = '' then svrCount := 1
          else svrCount := StrToInt(s);
        s := Concat('pt', GetAttrValue(layerNode, 'projection'));
        projectionType := TProjectionType(GetEnumValue(TypeInfo(TProjectionType), s)); //-1 will default to ptEPSG3857
        svrProc := GetAttrValue(layerNode, 'serverProc');
        xProc := GetAttrValue(layerNode, 'xProc');
        yProc := GetAttrValue(layerNode, 'yProc');
        zProc := GetAttrValue(layerNode, 'zProc');
        layerNode := layerNode.NextSibling;
      end;
      if first then begin
        ClearMapProviders;
        first := false;
      end;
      AddMapProvider(providerName, projectionType,
        url, minZoom, maxZoom, svrCount,
        GetSvrStr(svrProc), GetValStr(xProc), GetValStr(yProc), GetValStr(zProc)
      );
      node := node.NextSibling;
    end;
    Result := true;
  finally
    stream.Free;
    doc.Free;
  end;
end;

procedure TMapViewerEngine.Redraw;
begin
  Redraw(MapWin);
end;

procedure TMapViewerEngine.Redraw(const AWin: TMapWindow);
var
  TilesVis: TArea;
  x, y : Integer; //int64;
  Tiles: TTileIdArray = nil;
  iTile: Integer;
  tile: TTileID;
  numTiles: Integer;
  px, py: Integer;
  previewDrawn: Boolean;
  previewImg: TLazIntfImage;
  R: TRect;
begin
  if not(Active) then
    Exit;
  Queue.CancelAllJob(self);

  TilesVis := CalculateVisibleTiles(AWin);
  SetLength(Tiles, (TilesVis.Bottom - TilesVis.Top + 1) * (TilesVis.Right - TilesVis.Left + 1));
  iTile := Low(Tiles);
  numTiles := 1 shl AWin.Zoom;
  for y := TilesVis.Top to TilesVis.Bottom do
    for X := TilesVis.Left to TilesVis.Right do
    begin
      if FCyclic then
      begin
        Tiles[iTile].X := X mod numTiles;
        if Tiles[iTile].X < 0 then
          Tiles[iTile].X := Tiles[iTile].X + numTiles;
      end else
        Tiles[iTile].X := X;
      Tiles[iTile].Y := Y;
      Tiles[iTile].Z := AWin.Zoom;

      // Avoid tiling artefacts when a tile does not exist (lowest zoom) or
      // is not valid
      if not Cache.InCache(AWin.MapProvider, Tiles[iTile]) then
      begin
        previewdrawn := False;
        py := AWin.Y + Y * TILE_SIZE;
        px := AWin.X + X * TILE_SIZE;
        if FDrawPreviewTiles then
        begin
          if IsValidTile(AWin, Tiles[iTile]) then  // Invalid tiles probably will not be found in the cache
          begin
            tile := Tiles[iTile];
            if Cache.GetPreviewFromCache(AWin.MapProvider, tile, R) then
            begin
              Cache.GetFromCache(AWin.MapProvider, tile, previewImg);
              DrawStretchedTile(Tiles[iTile], px, py, previewImg, R);
              previewDrawn := true;
            end;
          end;
        end;
        if not previewDrawn then
          DrawTile(Tiles[iTile], px, py, nil);  // Draw blank tile if preview cannot be generated
      end;

      if IsValidTile(AWin, Tiles[iTile]) then
        inc(iTile);
    end;
  SetLength(Tiles, iTile);
  if Length(Tiles) > 0 then
    Queue.AddJob(TLaunchDownloadJob.Create(self, Tiles, AWin), self);
end;

procedure TMapViewerEngine.RegisterProviders;
var
  HERE1, HERE2: String;      // do not remove: needed by the inc file
begin
  {$I mvengine_mapreg.inc}
end;

function TMapViewerEngine.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result := MapPixelsToDegrees(MapWin, aPt);
end;

procedure TMapViewerEngine.SetActive(AValue: boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;

  // One problem at designtime is that the control is creating cache directories
  // at unexpected places when Cache.BasePath is relative
  if csDesigning in ComponentState then
    exit;

  if not FActive then
    Queue.CancelAllJob(self)
  else begin
    if Cache.UseDisk then ForceDirectories(Cache.BasePath);
    Redraw(MapWin);
  end;
end;

procedure TMapViewerEngine.SetBkColor(AValue: TFPColor);
begin
  if FBkColor = AValue then Exit;
  FBkColor := AValue;
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetCacheOnDisk(AValue: Boolean);
begin
  if Cache.UseDisk = AValue then Exit;
  Cache.UseDisk := AValue;
end;

procedure TMapViewerEngine.SetCachePath(AValue: String);
begin
  Cache.BasePath := aValue;
end;

procedure TMapViewerEngine.SetCenter(aCenter: TRealPoint);
begin
  if (MapWin.Center.Lon <> aCenter.Lon) or (MapWin.Center.Lat <> aCenter.Lat) then
  begin
    Mapwin.Center := aCenter;
    CalculateWin(MapWin);
    Redraw(MapWin);
    if assigned(OnCenterMove) then
      OnCenterMove(Self);
    if Assigned(OnChange) then
      OnChange(Self);
  end;
end;

procedure TMapViewerEngine.SetCyclic(AValue: Boolean);
begin
  if FCyclic = AValue then exit;
  FCyclic := AValue;
  if CrossesDateLine then
    Redraw;
end;

procedure TMapViewerEngine.SetDownloadEngine(AValue: TMvCustomDownloadEngine);
begin
  if FDownloadEngine = AValue then Exit;
  FDownloadEngine := AValue;
  if Assigned(FDownloadEngine) then
    FDownloadEngine.FreeNotification(self);
end;

procedure TMapViewerEngine.SetHeight(AValue: integer);
begin
  if MapWin.Height = AValue then Exit;
  MapWin.Height := AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetMapProvider(AValue: String);
var
  idx: integer;
begin
  idx := lstProvider.IndexOf(aValue);
  if not ((aValue = '') or (idx <> -1)) then
    raise Exception.Create('Unknow Provider: ' + aValue);
  if Assigned(MapWin.MapProvider) and (MapWin.MapProvider.Name = AValue) then Exit;
  if idx <> -1 then
  begin
    MapWin.MapProvider := TMapProvider(lstProvider.Objects[idx]);
    ConstraintZoom(MapWin);
    CalculateWin(MapWin);
  end
  else
    MapWin.MapProvider := nil;
  if Assigned(MapWin.MapProvider) then
    Redraw(MapWin);
end;

procedure TMapViewerEngine.SetSize(aWidth, aHeight: integer);
begin
  if (MapWin.Width = aWidth) and (MapWin.Height = aHeight) then Exit;
  CancelCurrentDrawing;
  MapWin.Width := aWidth;
  MapWin.Height := aHeight;
  CalculateWin(MapWin);
  Redraw(MapWin);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.SetUseThreads(AValue: Boolean);
begin
  if Queue.UseThreads = AValue then Exit;
  Queue.UseThreads := AValue;
  Cache.UseThreads := AValue;
end;

procedure TMapViewerEngine.SetWidth(AValue: integer);
begin
  if MapWin.Width = AValue then Exit;
  MapWin.Width := AValue;
  CalculateWin(MapWin);
  Redraw(MapWin);
end;

procedure TMapViewerEngine.SetZoom(AValue: Integer);
begin
  SetZoom(AValue, false);
end;

procedure TMapViewerEngine.SetZoom(AValue: integer; AZoomToCursor: Boolean);
begin
  if MapWin.Zoom = AValue then Exit;
  MapWin.Zoom := AValue;
  ConstraintZoom(MapWin);
  CalculateWin(MapWin);
  if AZoomToCursor then
    AdjustZoomCenter(MapWin);
  Redraw(MapWin);
  if Assigned(OnZoomChange) then
    OnZoomChange(Self);
  if Assigned(OnChange) then
    OnChange(Self);
end;

procedure TMapViewerEngine.TileDownloaded(Data: PtrInt);
var
  EnvTile: TEnvTile;
  img: TLazIntfImage;
  X, Y: integer;
  worldWidth : Integer;
  numTiles : Integer;
  baseX : Integer;
begin
  EnvTile := TEnvTile(Data);
  try
    if IsCurrentWin(EnvTile.Win)then
    begin
       Cache.GetFromCache(EnvTile.Win.MapProvider, EnvTile.Tile, img);
       Y := EnvTile.Win.Y + EnvTile.Tile.Y * TILE_SIZE; // begin of Y
       if Cyclic then
       begin
         baseX := EnvTile.Win.X + EnvTile.Tile.X * TILE_SIZE; // begin of X
         numTiles := 1 shl EnvTile.Win.Zoom;
         worldWidth := numTiles * TILE_SIZE;
         // From the center to the left (western) hemisphere
         X := baseX;
         while (X+TILE_SIZE >= 0) do
         begin
           DrawTile(EnvTile.Tile, X, Y, img);
           X := X - worldWidth;
         end;
         // From the center to the right (eastern) hemisphere
         X := baseX + worldWidth;
         while ((X-TILE_SIZE) <= EnvTile.Win.Width) do
         begin
           DrawTile(EnvTile.Tile, X, Y, img);
           X := X + worldWidth;
         end;
       end else
       begin
         X := EnvTile.Win.X + EnvTile.Tile.X * TILE_SIZE; // begin of X
         DrawTile(EnvTile.Tile, X, Y, img);
       end;
    end;
  finally
    FreeAndNil(EnvTile);
  end;
end;

function TMapViewerEngine.WorldScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  aPt.X := aPt.X - MapWin.X;
  aPt.Y := aPt.Y - MapWin.Y;
  Result := ScreenToLonLat(aPt);
end;

procedure TMapViewerEngine.WriteProvidersToXML(AFileName: String);
var
  doc: TXMLDocument;
  root: TDOMNode;
  i: Integer;
  prov: TMapProvider;
begin
  doc := TXMLDocument.Create;
  try
    root := doc.CreateElement('map_providers');
    doc.AppendChild(root);
    for i := 0 to lstProvider.Count - 1 do begin
      prov := TMapProvider(lstProvider.Objects[i]);
      prov.ToXML(doc, root);
    end;
    WriteXMLFile(doc, AFileName);
  finally
    doc.Free;
  end;
end;

procedure TMapViewerEngine.ZoomOnArea(const aArea: TRealArea);
var
  tmpWin: TMapWindow;
  visArea: TRealArea;
  TopLeft, BottomRight: TPoint;
begin
  tmpWin := MapWin;
  tmpWin.Center.Lon := (aArea.TopLeft.Lon + aArea.BottomRight.Lon) / 2;
  tmpWin.Center.Lat := (aArea.TopLeft.Lat + aArea.BottomRight.Lat) / 2;
  tmpWin.Zoom := 18;
  TopLeft.X := 0;
  TopLeft.Y := 0;
  BottomRight.X := tmpWin.Width;
  BottomRight.Y := tmpWin.Height;
  Repeat
    CalculateWin(tmpWin);
    visArea.TopLeft := MapPixelsToDegrees(tmpWin, TopLeft);
    visArea.BottomRight := MapPixelsToDegrees(tmpWin, BottomRight);
    if AreaInsideArea(aArea, visArea) then
      break;
    dec(tmpWin.Zoom);
  until (tmpWin.Zoom = 2);
  MapWin := tmpWin;
  Redraw(MapWin);
end;


//------------------------------------------------------------------------------

function RealPoint(Lat, Lon: Double): TRealPoint;
begin
  Result.Lon := Lon;
  Result.Lat := Lat;
end;

procedure SplitGps(AValue: Double; out ADegs, AMins: Double);
begin
  AValue := abs(AValue);
  AMins := frac(AValue) * 60;
  if abs(AMins - 60) < 1E-3 then
  begin
    AMins := 0;
    ADegs := trunc(AValue) + 1;
  end else
    ADegs := trunc(AValue);
  if AValue < 0 then
    ADegs := -ADegs;
end;

procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double);
begin
  SplitGps(AValue, ADegs, AMins);
  ASecs := frac(AMins) * 60;
  AMins := trunc(AMins);
  if abs(ASecs - 60) < 1E-3 then
  begin
    ASecs := 0;
    AMins := AMins + 1;
    if abs(AMins - 60) < 1e-3 then
    begin
      AMins := 0;
      ADegs := ADegs + 1;
    end;
  end;
  if AValue < 0 then
    ADegs := -ADegs;
end;

function GPSToDMS(Angle: Double): string;
begin
  Result := GPSToDMS(Angle, DefaultFormatSettings);
end;

function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string;
var
  deg, min, sec: Double;
begin
  SplitGPS(Angle, deg, min, sec);
  Result := Format('%.0f° %.0f'' %.*f"', [deg, min, DMS_Decimals, sec], AFormatSettings);
end;

function LatToStr(ALatitude: Double; DMS: Boolean): String;
begin
  Result := LatToStr(ALatitude, DMS, DefaultFormatSettings);
end;

function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  if DMS then
    Result := GPSToDMS(abs(ALatitude), AFormatSettings)
  else
    Result := Format('%.6f°',[abs(ALatitude)], AFormatSettings);
  if ALatitude > 0 then
    Result := Result + ' N'
  else
  if ALatitude < 0 then
    Result := Result + ' S';
end;

function LonToStr(ALongitude: Double; DMS: Boolean): String;
begin
  Result := LonToStr(ALongitude, DMS, DefaultFormatSettings);
end;

function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  if DMS then
    Result := GPSToDMS(abs(ALongitude), AFormatSettings)
  else
    Result := Format('%.6f°', [abs(ALongitude)], AFormatSettings);
  if ALongitude > 0 then
    Result := Result + ' E'
  else if ALongitude < 0 then
    Result := Result + ' W';
end;

{ Combines up to three parts of a GPS coordinate string (degrees, minutes, seconds)
  to a floating-point degree value. The parts are separated by non-numeric
  characters:

  three parts ---> d m s ---> d and m must be integer, s can be float
  two parts   ---> d m   ---> d must be integer, s can be float
  one part    ---> d     ---> d can be float

  Each part can exhibit a unit identifier, such as °, ', or ". BUT: they are
  ignored. This means that an input string 50°30" results in the output value 50.5
  although the second part is marked as seconds, not minutes!

  Hemisphere suffixes ('N', 'S', 'E', 'W') are supported at the end of the input string.
}
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
const
  NUMERIC_CHARS = ['0'..'9', '.', ',', '-', '+'];
var
  mins, secs: Double;
  i, j, len: Integer;
  n: Integer;
  s: String = '';
  res: Integer;
  sgn: Double;
begin
  Result := false;

  ADeg := NaN;
  mins := 0;
  secs := 0;

  if AValue = '' then
    exit;

  len := Length(AValue);
  i := len;
  while (i >= 1) and (AValue[i] = ' ') do dec(i);
  sgn := 1.0;
  if (AValue[i] in ['S', 's', 'W', 'w']) then sgn := -1;

  // skip leading non-numeric characters
  i := 1;
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract first value: degrees
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, ADeg, res);
    if res <> 0 then
      exit;
  end;

  // skip non-numeric characters between degrees and minutes
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract second value: minutes
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, mins, res);
    if (res <> 0) or (mins < 0) then
      exit;
  end;

  // skip non-numeric characters between minutes and seconds
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract third value: seconds
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, secs, res);
    if (res <> 0) or (secs < 0) then
      exit;
  end;

  // If the string contains seconds then minutes and deegrees must be integers
  if (secs <> 0) and ((frac(ADeg) > 0) or (frac(mins) > 0)) then
    exit;
  // If the string does not contain seconds then degrees must be integer.
  if (secs = 0) and (mins <> 0) and (frac(ADeg) > 0) then
    exit;

  // If the string contains minutes, but no seconds, then the degrees must be integer.
  Result := (mins >= 0) and (mins < 60) and (secs >= 0) and (secs < 60);

  // A similar check should be made for the degrees range, but since this is
  // different for latitude and longitude the check is skipped here.
  if Result then
    ADeg := sgn * (abs(ADeg) + mins / 60 + secs / 3600);
end;

// https://stackoverflow.com/questions/73608975/pascal-delphi-11-formula-for-distance-in-meters-between-two-decimal-gps-point
function HaversineDist(Lat1, Lon1, Lat2, Lon2, Radius: Double): Double;
var
  latFrom, latTo, lonDiff: Double;
  dx, dy, dz: Double;
begin
  lonDiff := DegToRad(Lon1 - Lon2);
  latFrom := DegToRad(Lat1);
  latTo := DegToRad(Lat2);

  dz := sin(latFrom) - sin(latTo);
  dx := cos(lonDiff) * cos(latFrom) - cos(latTo);
  dy := sin(lonDiff) * cos(latFrom);

  Result := arcsin(sqrt(sqr(dx) + sqr(dy) + sqr(dz)) / 2) * Radius * 2;
end;


{ Returns the direct distance (air-line) between two geo coordinates
 If latitude NOT between -90°..+90° and longitude NOT between -180°..+180°
 the function returns NaN.
 Usage: CalcGeoDistance(51.53323, -2.90130, 51.29442, -2.27275, duKilometers);
}
function CalcGeoDistance(Lat1, Lon1, Lat2, Lon2: double;
  AUnits: TDistanceUnits = duKilometers): double;
begin
  // Validate
  if (Lat1 < -90.0) or (Lat1 > 90.0) then exit(NaN);
  if (Lat2 < -90.0) or (Lat2 > 90.0) then exit(NaN);

  Result := HaversineDist(Lat1, Lon1, Lat2, Lon2, EARTH_EQUATORIAL_RADIUS);
  case AUnits of
    duMeters: ;
    duKilometers: Result := Result * 1E-3;
    duMiles: Result := Result * 0.62137E-3;
  end;
end;

{ Converts an angle given as degrees, minutes and seconds to a single
  floating point degrees value. }
function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
begin
  Result := Deg + Min/60.0 + Sec/3600.0;
end;

end.

