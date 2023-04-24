{ Initial map viewer library:
    Copyright (C) 2011 Maciej Kaczkowski / keit.co

  Extensions:
    (C) 2014 ti_dic@hotmail.com
    (C) 2019 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

// ToDo: Make Active work at designtime.

unit mvMapViewer;

{$MODE objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Controls, Graphics, IntfGraphics, Forms, ImgList, LCLVersion,
  MvTypes, MvGPSObj, MvEngine, MvMapProvider, MvDownloadEngine, MvDrawingEngine;

Type

  TDrawGpsPointEvent = procedure (Sender: TObject;
    ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint) of object;

  { TMapView }

  TMapView = class(TCustomControl)
    private
      FDownloadEngine: TMvCustomDownloadEngine;
      FBuiltinDownloadEngine: TMvCustomDownloadEngine;
      FEngine: TMapViewerEngine;
      FBuiltinDrawingEngine: TMvCustomDrawingEngine;
      FDrawingEngine: TMvCustomDrawingEngine;
      FActive: boolean;
      FGPSItems: TGPSObjectList;
      FInactiveColor: TColor;
      FPOIImage: TBitmap;
      FPOITextBgColor: TColor;
      FOnDrawGpsPoint: TDrawGpsPointEvent;
      FDebugTiles: Boolean;
      FDefaultTrackColor: TColor;
      FDefaultTrackWidth: Integer;
      FFont: TFont;
      FPOIImages: TCustomImageList;
      FPOIImagesWidth: Integer;
      procedure CallAsyncInvalidate;
      procedure DoAsyncInvalidate({%H-}Data: PtrInt);
      procedure DrawObjects(const {%H-}TileId: TTileId; aLeft, aTop, aRight,aBottom: integer);
      procedure DrawPointOfInterest(const {%H-}Area: TRealArea; APt: TGPSPointOfInterest);
      procedure DrawPt(const {%H-}Area: TRealArea; APt: TGPSPoint);
      procedure DrawTrack(const Area: TRealArea; trk: TGPSTrack);
      function GetCacheOnDisk: boolean;
      function GetCachePath: String;
      function GetCenter: TRealPoint;
      function GetDownloadEngine: TMvCustomDownloadEngine;
      function GetDrawingEngine: TMvCustomDrawingEngine;
      function GetMapProvider: String;
      function GetOnCenterMove: TNotifyEvent;
      function GetOnChange: TNotifyEvent;
      function GetOnZoomChange: TNotifyEvent;
      function GetUseThreads: boolean;
      function GetZoom: integer;
      function GetZoomToCursor: Boolean;
      function IsCachePathStored: Boolean;
      function IsFontStored: Boolean;
      procedure SetActive(AValue: boolean);
      procedure SetCacheOnDisk(AValue: boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCenter(AValue: TRealPoint);
      procedure SetDebugTiles(AValue: Boolean);
      procedure SetDefaultTrackColor(AValue: TColor);
      procedure SetDefaultTrackWidth(AValue: Integer);
      procedure SetDownloadEngine(AValue: TMvCustomDownloadEngine);
      procedure SetDrawingEngine(AValue: TMvCustomDrawingEngine);
      procedure SetFont(AValue: TFont);
      procedure SetInactiveColor(AValue: TColor);
      procedure SetMapProvider(AValue: String);
      procedure SetOnCenterMove(AValue: TNotifyEvent);
      procedure SetOnChange(AValue: TNotifyEvent);
      procedure SetOnZoomChange(AValue: TNotifyEvent);
      procedure SetPOIImage(const AValue: TBitmap);
      procedure SetPOIImages(const AValue: TCustomImageList);
      procedure SetPOIImagesWidth(AValue: Integer);
      procedure SetPOITextBgColor(AValue: TColor);
      procedure SetUseThreads(AValue: boolean);
      procedure SetZoom(AValue: integer);
      procedure SetZoomToCursor(AValue: Boolean);
      procedure UpdateFont(Sender: TObject);
      procedure UpdateImage(Sender: TObject);

    protected
      AsyncInvalidate : boolean;
      procedure ActivateEngine;
      procedure DblClick; override;
      procedure DoDrawTile(const TileId: TTileId; X,Y: integer; TileImg: TLazIntfImage);
      procedure DoDrawTileInfo(const {%H-}TileID: TTileID; X,Y: Integer);
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
      procedure DoOnResize; override;
      function IsActive: Boolean;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Paint; override;
      procedure OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;
        Adding: boolean);
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure ClearBuffer;
      procedure GetMapProviders(lstProviders: TStrings);
      function GetVisibleArea: TRealArea;
      function LonLatToScreen(aPt: TRealPoint): TPoint;
      function ObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjarray;
      procedure SaveToFile(AClass: TRasterImageClass; const AFileName: String);
      function SaveToImage(AClass: TRasterImageClass): TRasterImage;
      procedure SaveToStream(AClass: TRasterImageClass; AStream: TStream);
      function ScreenToLonLat(aPt: TPoint): TRealPoint;
      procedure CenterOnObj(obj: TGPSObj);
      procedure Redraw; inline;
      procedure ZoomOnArea(const aArea: TRealArea);
      procedure ZoomOnObj(obj: TGPSObj);
      procedure WaitEndOfRendering;
      property Center: TRealPoint read GetCenter write SetCenter;
      property Engine: TMapViewerEngine read FEngine;
      property GPSItems: TGPSObjectList read FGPSItems;
    published
      property Active: boolean read FActive write SetActive default false;
      property Align;
      property CacheOnDisk: boolean read GetCacheOnDisk write SetCacheOnDisk default true;
      property CachePath: String read GetCachePath write SetCachePath stored IsCachePathStored;
      property DebugTiles: Boolean read FDebugTiles write SetDebugTiles default false;
      property DefaultTrackColor: TColor read FDefaultTrackColor write SetDefaultTrackColor default clRed;
      property DefaultTrackWidth: Integer read FDefaultTrackWidth write SetDefaultTrackWidth default 1;
      property DownloadEngine: TMvCustomDownloadEngine read GetDownloadEngine write SetDownloadEngine;
      property DrawingEngine: TMvCustomDrawingEngine read GetDrawingEngine write SetDrawingEngine;
      property Font: TFont read FFont write SetFont stored IsFontStored;
      property Height default 150;
      property InactiveColor: TColor read FInactiveColor write SetInactiveColor default clWhite;
      property MapProvider: String read GetMapProvider write SetMapProvider;
      property POIImage: TBitmap read FPOIImage write SetPOIImage;
      property POIImages: TCustomImageList read FPOIImages write SetPOIImages;
      property POIImagesWidth: Integer read FPOIImagesWidth write SetPOIImagesWidth default 0;
      property POITextBgColor: TColor read FPOITextBgColor write SetPOITextBgColor default clNone;
      property PopupMenu;
      property UseThreads: boolean read GetUseThreads write SetUseThreads default false;
      property Width default 150;
      property Zoom: integer read GetZoom write SetZoom default 0;
      property ZoomToCursor: Boolean read GetZoomToCursor write SetZoomToCursor default True;
      property OnCenterMove: TNotifyEvent read GetOnCenterMove write SetOnCenterMove;
      property OnZoomChange: TNotifyEvent read GetOnZoomChange write SetOnZoomChange;
      property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
      property OnDrawGpsPoint: TDrawGpsPointEvent read FOnDrawGpsPoint write FOnDrawGpsPoint;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;
  end;


implementation

uses                syncobjs,
  GraphType, Types,
  mvJobQueue, mvExtraData, mvDLEFpc,
  {$IFDEF MSWINDOWS}
  mvDLEWin,
  {$ENDIF}
  mvDE_IntfGraphics;

{ Converts a length given in millimeters to screen pixels }
function mmToPx(AValue: Double): Integer;
begin
  Result := round(AValue / 25.4 * ScreenInfo.PixelsPerInchX);
end;


type

  { TDrawObjJob }

  TDrawObjJob = class(TJob)
  private
    AllRun: boolean;
    Viewer: TMapView;
    FRunning: boolean;
    FLst: TGPSObjList;
    FStates: Array of integer;
    FArea: TRealArea;
  protected
    function pGetTask: integer; override;
    procedure pTaskStarted(aTask: integer); override;
    procedure pTaskEnded(aTask: integer; aExcept: Exception); override;
  public
    procedure ExecuteTask(aTask: integer; FromWaiting: boolean); override;
    function Running: boolean;override;
  public
    constructor Create(aViewer: TMapView; aLst: TGPSObjList; const aArea: TRealArea);
    destructor Destroy; override;
  end;

{ TDrawObjJob }

function TDrawObjJob.pGetTask: integer;
var
  i: integer;
begin
  if not(AllRun) and not(Cancelled) then
  begin
    for i := Low(FStates) to High(FStates) do
      if FStates[i]=0 then
      begin
        result := i+1;
        Exit;
      end;
    AllRun:=True;
  end;

  Result := ALL_TASK_COMPLETED;
  for i := Low(FStates) to High(FStates) do
    if FStates[i]=1 then
    begin
      Result := NO_MORE_TASK;
      Exit;
    end;
end;

procedure TDrawObjJob.pTaskStarted(aTask: integer);
begin
  FRunning := True;
  FStates[aTask-1] := 1;
end;

procedure TDrawObjJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask-1] := 3
  else
    FStates[aTask-1] := 2;
end;

procedure TDrawObjJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var
  iObj: integer;
  Obj: TGpsObj;
begin
  iObj := aTask-1;
  Obj := FLst[iObj];
  if Obj.InheritsFrom(TGPSTrack) then
    Viewer.DrawTrack(FArea, TGPSTrack(Obj));
  if Obj.InheritsFrom(TGPSPointOfInterest) then
    Viewer.DrawPointOfInterest(FArea, TGPSPointOfInterest(Obj))
  else
  if Obj.InheritsFrom(TGPSPoint) then
    Viewer.DrawPt(FArea, TGPSPoint(Obj));
end;

function TDrawObjJob.Running: boolean;
begin
  Result := FRunning;
end;

constructor TDrawObjJob.Create(aViewer: TMapView; aLst: TGPSObjList;
  const aArea: TRealArea);
begin
  FArea := aArea;
  FLst := aLst;
  SetLEngth(FStates,FLst.Count);
  Viewer := aViewer;
  AllRun := false;
  Name := 'DrawObj';
end;

destructor TDrawObjJob.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLst);
  if not(Cancelled) then
    Viewer.CallAsyncInvalidate;
end;


{ TMapView }

procedure TMapView.SetActive(AValue: boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if FActive then
    ActivateEngine
  else
    Engine.Active := false;
end;

function TMapView.GetCacheOnDisk: boolean;
begin
  Result := Engine.CacheOnDisk;
end;

function TMapView.GetCachePath: String;
begin
  Result := Engine.CachePath;
end;

function TMapView.GetCenter: TRealPoint;
begin
  Result := Engine.Center;
end;

function TMapView.GetDownloadEngine: TMvCustomDownloadEngine;
begin
  if FDownloadEngine = nil then
    Result := FBuiltinDownloadEngine
  else
    Result := FDownloadEngine;
end;

function TMapView.GetDrawingEngine: TMvCustomDrawingEngine;
begin
  if FDrawingEngine = nil then
    Result := FBuiltinDrawingEngine
  else
    Result := FDrawingEngine;
end;

function TMapView.GetMapProvider: String;
begin
  result := Engine.MapProvider;
end;

function TMapView.GetOnCenterMove: TNotifyEvent;
begin
  result := Engine.OnCenterMove;
end;

function TMapView.GetOnChange: TNotifyEvent;
begin
  Result := Engine.OnChange;
end;

function TMapView.GetOnZoomChange: TNotifyEvent;
begin
  Result := Engine.OnZoomChange;
end;

function TMapView.GetUseThreads: boolean;
begin
  Result := Engine.UseThreads;
end;

function TMapView.GetZoom: integer;
begin
  result := Engine.Zoom;
end;

function TMapView.GetZoomToCursor: Boolean;
begin
  Result := Engine.ZoomToCursor;
end;

function TMapView.IsCachePathStored: Boolean;
begin
  Result := not SameText(CachePath, 'cache/');
end;

function TMapView.IsFontStored: Boolean;
begin
  Result := SameText(FFont.Name, 'default') and (FFont.Size = 0) and
    (FFont.Style = []) and (FFont.Color = clBlack);
end;

procedure TMapView.SetCacheOnDisk(AValue: boolean);
begin
  Engine.CacheOnDisk := AValue;
end;

procedure TMapView.SetCachePath(AValue: String);
begin
  Engine.CachePath := AValue;
end;

procedure TMapView.SetCenter(AValue: TRealPoint);
begin
  Engine.Center := AValue;
end;

procedure TMapView.SetDebugTiles(AValue: Boolean);
begin
  if FDebugTiles = AValue then exit;
  FDebugTiles := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetDefaultTrackColor(AValue: TColor);
begin
  if FDefaultTrackColor = AValue then exit;
  FDefaultTrackColor := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetDefaultTrackWidth(AValue: Integer);
begin
  if FDefaultTrackWidth = AValue then exit;
  FDefaultTrackWidth := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetDownloadEngine(AValue: TMvCustomDownloadEngine);
begin
  FDownloadEngine := AValue;
  FEngine.DownloadEngine := GetDownloadEngine;
end;

procedure TMapView.SetDrawingEngine(AValue: TMvCustomDrawingEngine);
begin
  FDrawingEngine := AValue;
  if AValue = nil then
    FBuiltinDrawingEngine.CreateBuffer(ClientWidth, ClientHeight)
  else begin
    FBuiltinDrawingEngine.CreateBuffer(0, 0);
    FDrawingEngine.CreateBuffer(ClientWidth, ClientHeight);
  end;
  UpdateFont(nil);
end;

procedure TMapView.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  UpdateFont(nil);
end;

procedure TMapView.SetInactiveColor(AValue: TColor);
begin
  if FInactiveColor = AValue then
    exit;
  FInactiveColor := AValue;
  if not IsActive then
    Invalidate;
end;

procedure TMapView.ActivateEngine;
begin
  Engine.SetSize(ClientWidth,ClientHeight);
  Engine.Active := IsActive;
end;

procedure TMapView.SetMapProvider(AValue: String);
begin
  Engine.MapProvider := AValue;
end;

procedure TMapView.SetOnCenterMove(AValue: TNotifyEvent);
begin
  Engine.OnCenterMove := AValue;
end;

procedure TMapView.SetOnChange(AValue: TNotifyEvent);
begin
  Engine.OnChange := AValue;
end;

procedure TMapView.SetOnZoomChange(AValue: TNotifyEvent);
begin
  Engine.OnZoomChange := AValue;
end;

procedure TMapView.SetPOIImage(const AValue: TBitmap);
begin
  if FPOIImage = AValue then exit;
  FPOIImage := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetPOIImages(const AValue: TCustomImageList);
begin
  if FPOIImages = AValue then exit;
  FPOIImages := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetPOIImagesWidth(AValue: Integer);
begin
  if FPOIImagesWidth = AValue then exit;
  FPOIImagesWidth := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetPOITextBgColor(AValue: TColor);
begin
  if FPOITextBgColor = AValue then exit;
  FPOITextBgColor := AValue;
  Engine.Redraw;
end;

procedure TMapView.SetUseThreads(AValue: boolean);
begin
  Engine.UseThreads := aValue;
end;

procedure TMapView.SetZoom(AValue: integer);
begin
  Engine.Zoom := AValue;
end;

procedure TMapView.SetZoomToCursor(AValue: Boolean);
begin
  Engine.ZoomToCursor := AValue;
end;

function TMapView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if IsActive then
    Engine.MouseWheel(self,Shift,WheelDelta,MousePos,Result);
end;

procedure TMapView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseDown(self,Button,Shift,X,Y);
end;

procedure TMapView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if IsActive then
    Engine.MouseUp(self,Button,Shift,X,Y);
end;

procedure TMapView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  if IsActive then
    Engine.MouseMove(self,Shift,X,Y);
end;

procedure TMapView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (AComponent = FPOIImages) and (Operation = opRemove) then
    FPOIImages := nil;
end;

procedure TMapView.DblClick;
begin
  inherited DblClick;
  if IsActive then
    Engine.DblClick(self);
end;

procedure TMapView.DoOnResize;
begin
  inherited DoOnResize;
  //cancel all rendering threads
  Engine.CancelCurrentDrawing;
  DrawingEngine.CreateBuffer(ClientWidth, ClientHeight);
  if IsActive then
    Engine.SetSize(ClientWidth, ClientHeight);
end;

procedure TMapView.Paint;
begin
  inherited Paint;
  if IsActive then
    DrawingEngine.PaintToCanvas(Canvas)
  else
  begin
    Canvas.Brush.Color := InactiveColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(0, 0, ClientWidth, ClientHeight);
  end;
end;

procedure TMapView.OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;
  Adding: boolean);
var
  Area, objArea, visArea: TRealArea;
begin
  if Adding and Assigned(Objs) then
  begin
    objArea := GetAreaOf(Objs);
    visArea := GetVisibleArea;
    if hasIntersectArea(objArea, visArea) then
    begin
      Area := IntersectArea(objArea, visArea);
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self, Objs, Area), Engine);
    end
    else
      objs.Free;
  end
  else
  begin
    Engine.Redraw;
    Objs.free;
  end;
end;

procedure TMapView.DrawTrack(const Area: TRealArea; trk: TGPSTrack);
var
  Old,New: TPoint;
  i: integer;
  pt: TRealPointArray;
  iPt1, iPt2: TPoint;
  pt1, pt2: TRealPoint;
  isInside1, isInside2: boolean;
  trkColor: TColor;
  trkWidth: Integer;
begin
  if not trk.Visible or (trk.Points.Count = 0) then
    exit;

  GPSItems.Lock;
  try
    // Determine track color
    if trk.LineColor = clDefault then
    begin
      trkColor := ColorToRGB(FDefaultTrackColor);
      if (trk.ExtraData <> nil) and trk.ExtraData.InheritsFrom(TDrawingExtraData) then
        trkColor := TDrawingExtraData(trk.ExtraData).Color;
    end else
      trkColor := ColorToRGB(trk.LineColor);

    // Determine track width
    if trk.LineWidth = -1 then
    begin
      trkWidth := FDefaultTrackWidth;
      if (trk.ExtraData <> nil) and trk.ExtraData.InheritsFrom(TTrackExtraData) then
        trkWidth := mmToPx(TTrackExtraData(trk.ExtraData).Width);
    end else
      trkWidth := mmToPx(trk.LineWidth);
    if trkWidth < 1 then trkWidth := 1;

    DrawingEngine.PenColor := trkColor;
    DrawingEngine.PenWidth := trkWidth;
    pt1 := trk.Points[0].RealPoint;
    iPt1 := Engine.LonLatToScreen(pt1);
    isInside1 := PtInsideArea(pt1, Area);
    for i:=1 to pred(trk.Points.Count) do
    begin
      pt2 := trk.Points[i].RealPoint;
      isInside2 := PtInsideArea(pt2, Area);
      if isInside1 and isInside2 then
      begin
        // Totally inside
        iPt2 := Engine.LonLatToScreen(pt2);
        DrawingEngine.Line(iPt1.X, iPt1.Y, iPt2.X, iPt2.Y);
        iPt1 := iPt2;
      end else
      begin
        Area.InterSectionWithLine(pt1, pt2, pt);
        if Length(pt) = 1 then
        begin
          // Coming in
          if isInside2 then
          begin
            iPt1 := Engine.LonLatToScreen(pt[0]);
            iPt2 := Engine.LonLatToScreen(pt2);
            DrawingEngine.Line(iPt1.X, iPt1.Y, iPt2.X, iPt2.Y);
            iPt1 := iPt2;
          end else
          // Going out
          if isInside1 then
          begin
            iPt2 := Engine.LonLatToScreen(pt[0]);
            DrawingEngine.Line(iPt1.X, iPt1.Y, iPt2.X, iPt2.Y);
          end;
        end else
        if length(pt) = 2 then
        begin
          // Going through
          iPt1 := Engine.LonLatToScreen(pt[0]);
          iPt2 := Engine.LonLatToScreen(pt[1]);
          DrawingEngine.Line(iPt1.X, iPt1.Y, iPt2.X, iPt2.Y);
        end else
          ;  // no intersection;
      end;
      pt1 := pt2;
      isInside1 := isInside2;
    end;
  finally
    GPSItems.Unlock;
  end;
end;

procedure TMapView.DrawPointOfInterest(const Area: TRealArea; APt: TGPSPointOfInterest);
var
  pt: TPoint;
  ptColor: TColor;
  extent: TSize;
  s: String;
  bmp: TBitmap;
  w, h: Integer;
begin
  GPSItems.Lock;
  try
    pt := Engine.LonLatToScreen(APt.RealPoint);

    // Draw point as symbol from image list ...
    if Assigned(FPOIImages) and (APt.ImageIndex <> -1) and (APt.ImageIndex < FPOIImages.Count) then
    begin
      bmp := TBitmap.Create;
      try
        FPOIImages.GetBitmap(APt.ImageIndex, bmp);
        {$IF LCL_FullVersion >= 2000000}
        w := FPOIImages.WidthForPPI[FPOIImagesWidth, Font.PixelsPerInch];
        h := FPOIImages.HeightForPPI[FPOIImagesWidth, Font.PixelsPerInch];
        {$ELSE}
        w := FPOIImages.Width;
        h := FPOIImages.Height;
        {$IFEND}
        DrawingEngine.DrawBitmap(pt.X - w div 2, pt.Y - h, bmp, true);
      finally
        bmp.Free;
      end;
    end else
    begin
      // ... or as cross
      ptColor := clRed;
      if (APt.ExtraData <> nil) and APt.ExtraData.InheritsFrom(TDrawingExtraData) then
        ptColor := TDrawingExtraData(APt.ExtraData).Color;
      DrawingEngine.PenColor := ptColor;
      DrawingEngine.Line(pt.X, pt.Y - 5, pt.X, pt.Y + 5);
      DrawingEngine.Line(pt.X - 5, pt.Y, pt.X + 5, pt.Y);
      pt.Y := pt.Y + 5;
    end;

    // Draw point text
    s := APt.Name;
    if FPOITextBgColor = clNone then
      DrawingEngine.BrushStyle := bsClear
    else begin
      DrawingEngine.BrushStyle := bsSolid;
      DrawingEngine.BrushColor := FPOITextBgColor;
      s := ' ' + s + ' ';
    end;
    extent := DrawingEngine.TextExtent(s);
    DrawingEngine.Textout(pt.X - extent.CX div 2, pt.Y + 5, s);
  finally
    GPSItems.Unlock;
  end;
end;

procedure TMapView.DrawPt(const Area: TRealArea; APt: TGPSPoint);
var
  Pt: TPoint;
  PtColor: TColor;
  extent: TSize;
  s: String;
begin
  GPSItems.Lock;
  try
    if Assigned(FOnDrawGpsPoint) then begin
      FOnDrawGpsPoint(Self, DrawingEngine, APt);
      exit;
    end;

    Pt := Engine.LonLatToScreen(APt.RealPoint);
    PtColor := clRed;
    if APt.ExtraData <> nil then
    begin
      if APt.ExtraData.inheritsFrom(TDrawingExtraData) then
        PtColor := TDrawingExtraData(APt.ExtraData).Color;
    end;

    // Draw point marker
    if Assigned(FPOIImage) and not (FPOIImage.Empty) then
      DrawingEngine.DrawBitmap(Pt.X - FPOIImage.Width div 2, Pt.Y - FPOIImage.Height, FPOIImage, true)
    else begin
      DrawingEngine.PenColor := ptColor;
      DrawingEngine.PenWidth := 3;
      DrawingEngine.Line(Pt.X, Pt.Y - 5, Pt.X, Pt.Y + 5);
      DrawingEngine.Line(Pt.X - 5, Pt.Y, Pt.X + 5, Pt.Y);
      Pt.Y := Pt.Y + 5;
    end;

    // Draw point text
    s := APt.Name;
    if FPOITextBgColor = clNone then
      DrawingEngine.BrushStyle := bsClear
    else begin
      DrawingEngine.BrushStyle := bsSolid;
      DrawingEngine.BrushColor := FPOITextBgColor;
      s := ' ' + s + ' ';
    end;
    extent := DrawingEngine.TextExtent(s);
    DrawingEngine.Textout(Pt.X - extent.CX div 2, Pt.Y + 5, s);

  finally
    GPSItems.Unlock;
  end;
end;

procedure TMapView.CallAsyncInvalidate;
Begin
  if not(AsyncInvalidate) then
  begin
    AsyncInvalidate := true;
    Engine.Jobqueue.QueueAsyncCall(@DoAsyncInvalidate, 0);
  end;
end;

procedure TMapView.DrawObjects(const TileId: TTileId;
  aLeft, aTop,aRight,aBottom: integer);
var
  Area: TRealArea;
  lst: TGPSObjList;
begin
  Area.TopLeft := Engine.ScreenToLonLat(Point(aLeft, aTop));
  Area.BottomRight := Engine.ScreenToLonLat(Point(aRight, aBottom));
  //Area.Normalize;

  if GPSItems.Count > 0 then
  begin
    lst := GPSItems.GetObjectsInArea(Area);
    if lst.Count > 0 then
      Engine.Jobqueue.AddJob(TDrawObjJob.Create(self, lst, Area), Engine)
    else
    begin
      FreeAndNil(Lst);
      CallAsyncInvalidate;
    end;
  end
  else
    CallAsyncInvalidate;
end;

procedure TMapView.DoAsyncInvalidate(Data: PtrInt);
Begin
  Invalidate;
  AsyncInvalidate := false;
end;

procedure TMapView.DoDrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TLazIntfImage);
begin
  if Assigned(TileImg) then begin
    DrawingEngine.DrawLazIntfImage(X, Y, TileImg);
  end
  else begin
    DrawingEngine.BrushColor := clWhite;
    DrawingEngine.BrushStyle := bsSolid;
    DrawingEngine.FillRect(X, Y, X + TILE_SIZE, Y + TILE_SIZE);
  end;

  if FDebugTiles then
    DoDrawTileInfo(TileID, X, Y);

  DrawObjects(TileId, X, Y, X + TILE_SIZE, Y + TILE_SIZE);
end;

procedure TMapView.DoDrawTileInfo(const TileID: TTileID; X, Y: Integer);
begin
  DrawingEngine.PenColor := clGray;
  DrawingEngine.PenWidth := 1;
  DrawingEngine.Line(X, Y, X, Y + TILE_SIZE);
  DrawingEngine.Line(X, Y, X + TILE_SIZE, Y);
  DrawingEngine.Line(X + TILE_SIZE, Y, X + TILE_SIZE, Y + TILE_SIZE);
  DrawingEngine.Line(X, Y + TILE_SIZE, X + TILE_SIZE, Y + TILE_SIZE);
end;

function TMapView.IsActive: Boolean;
begin
  if not(csDesigning in ComponentState) then
    Result := FActive
  else
    Result := false;
end;

constructor TMapView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 150;
  Height := 150;

  FActive := false;
  FDefaultTrackColor := clRed;
  FDefaultTrackWidth := 1;
  FInactiveColor := clWhite;

  FGPSItems := TGPSObjectList.Create;
  FGPSItems.OnModified := @OnGPSItemsModified;

  {$IFDEF MSWindows}
  FBuiltinDownloadEngine := TMvDEWin.Create(self);
  {$ELSE}
  FBuiltinDownloadEngine := TMvDEFpc.Create(self);
  {$ENDIF}
  FBuiltinDownloadEngine.Name := 'BuiltInDLE';

  FEngine := TMapViewerEngine.Create(self);
  FEngine.CachePath := 'cache/';
  FEngine.CacheOnDisk := true;
  FEngine.OnDrawTile := @DoDrawTile;
  FEngine.DrawTitleInGuiThread := false;
  FEngine.DownloadEngine := FBuiltinDownloadEngine;
  FEngine.ZoomToCursor := True;

  FBuiltinDrawingEngine := TMvIntfGraphicsDrawingEngine.Create(self);
  FBuiltinDrawingEngine.Name := 'BuiltInDE';
  FBuiltinDrawingEngine.CreateBuffer(Width, Height);

  FFont := TFont.Create;
  FFont.Name := 'default';
  FFont.Size := 0;
  FFont.Style := [];
  FFont.Color := clBlack;
  FFont.OnChange := @UpdateFont;

  FPOIImage := TBitmap.Create;
  FPOIImage.OnChange := @UpdateImage;
  FPOITextBgColor := clNone;
end;

destructor TMapView.Destroy;
begin
  FFont.Free;
  FreeAndNil(FPOIImage);
  FreeAndNil(FGPSItems);
  inherited Destroy;
end;

procedure TMapView.SaveToFile(AClass: TRasterImageClass; const AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmCreate + fmShareDenyNone);
  try
    SaveToStream(AClass, stream);
  finally
    stream.Free;
  end;
end;

function TMapView.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := DrawingEngine.SaveToImage(AClass);
end;

procedure TMapView.SaveToStream(AClass: TRasterImageClass; AStream: TStream);
var
  img: TRasterImage;
begin
  img := SaveToImage(AClass);
  try
    img.SaveToStream(AStream);
  finally
    img.Free;
  end;
end;

function TMapView.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result:=Engine.ScreenToLonLat(aPt);
end;

function TMapView.LonLatToScreen(aPt: TRealPoint): TPoint;
begin
  Result:=Engine.LonLatToScreen(aPt);
end;

procedure TMapView.GetMapProviders(lstProviders: TStrings);
begin
  Engine.GetMapProviders(lstProviders);
end;

procedure TMapView.WaitEndOfRendering;
begin
  Engine.Jobqueue.WaitAllJobTerminated(Engine);
end;

function TMapView.ObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjarray;
const
  DELTA = 3;
var
  rArea: TRealArea;
  gpsList: TGPSObjList;
  i: Integer;
begin
  Result := nil;
  
  if ATolerance = -1 then
    ATolerance := DELTA;

  // Define area of +/-ATolerance pixels around the screen point
  rArea.TopLeft := ScreenToLonLat(Point(X-ATolerance, Y-ATolerance));
  rArea.BottomRight := ScreenToLonLat(Point(X+ATolerance, Y+ATolerance));

  // Collect Objects in this are
  gpsList := FGPSItems.GetObjectsInArea(rArea);
  try
    SetLength(Result, gpsList.Count);
    for i := 0 to gpsList.Count-1 do
      if gpsList[i] is TGPSPoint then
        Result[i] := gpsList[i];
  finally
    gpsList.Free;
  end;
end;

procedure TMapView.CenterOnObj(obj: TGPSObj);
var
  Area: TRealArea;
  Pt: TRealPoint;
begin
  obj.GetArea(Area);
  Pt.Lon := (Area.TopLeft.Lon + Area.BottomRight.Lon) /2;
  Pt.Lat := (Area.TopLeft.Lat + Area.BottomRight.Lat) /2;
  Center := Pt;
end;

procedure TMapView.ZoomOnObj(obj: TGPSObj);
var
  Area: TRealArea;
begin
  obj.GetArea(Area);
  Engine.ZoomOnArea(Area);
end;

procedure TMapView.ZoomOnArea(const aArea: TRealArea);
begin
  Engine.ZoomOnArea(aArea);
end;

procedure TMapView.Redraw;
begin
  Engine.Redraw;
end;

function TMapView.GetVisibleArea: TRealArea;
var
  mapWidth: Int64;
begin
  Result.TopLeft := Engine.ScreenToLonLat(Point(0, 0));
  Result.BottomRight := Engine.ScreenToLonLat(Point(Width, Height));

  mapWidth := ZoomFactor(Engine.Zoom) * TILE_SIZE;
  if Width >= mapWidth then
  begin
    Result.TopLeft.Lon := -180;
    Result.BottomRight.Lon := 180;
  end;
end;

procedure TMapView.ClearBuffer;
begin
  DrawingEngine.CreateBuffer(ClientWidth, ClientHeight);       // ???
end;

procedure TMapView.UpdateFont(Sender: TObject);
begin
  if SameText(FFont.Name, 'default') then
    DrawingEngine.FontName := Screen.SystemFont.Name
  else
    DrawingEngine.FontName := FFont.Name;
  if FFont.Size = 0 then
    DrawingEngine.FontSize := Screen.SystemFont.Size
  else
    DrawingEngine.FontSize := FFont.Size;
  DrawingEngine.FontStyle := FFont.Style;
  DrawingEngine.FontColor := ColorToRGB(FFont.Color);
  Engine.Redraw;
end;

procedure TMapView.UpdateImage(Sender: TObject);
begin
  Engine.Redraw;
end;

end.

