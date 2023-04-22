unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, IntfGraphics, ColorBox, PrintersDlgs,
  mvGeoNames, mvMapViewer, mvTypes, mvGpsObj, mvDrawingEngine;

type

  { TMainForm }

  TMainForm = class(TForm)
    Bevel1: TBevel;
    BtnSearch: TButton;
    BtnGoTo: TButton;
    BtnGPSPoints: TButton;
    BtnSaveToFile: TButton;
    BtnLoadGPXFile: TButton;
    BtnPOITextFont: TButton;
    BtnPrintMap: TButton;
    CbDoubleBuffer: TCheckBox;
    CbFoundLocations: TComboBox;
    CbLocations: TComboBox;
    CbProviders: TComboBox;
    CbUseThreads: TCheckBox;
    CbMouseCoords: TGroupBox;
    CbDistanceUnits: TComboBox;
    CbDebugTiles: TCheckBox;
    cbPOITextBgColor: TColorBox;
    CbZoomToCursor: TCheckBox;
    FontDialog: TFontDialog;
    GbCenterCoords: TGroupBox;
    GbScreenSize: TGroupBox;
    GbSearch: TGroupBox;
    GbGPS: TGroupBox;
    InfoCenterLatitude: TLabel;
    InfoViewportHeight: TLabel;
    InfoCenterLongitude: TLabel;
    InfoBtnGPSPoints: TLabel;
    GPSPointInfo: TLabel;
    InfoViewportWidth: TLabel;
    LblPOITextBgColor: TLabel;
    LblSelectLocation: TLabel;
    LblCenterLatitude: TLabel;
    LblViewportHeight: TLabel;
    LblViewportWidth: TLabel;
    LblPositionLongitude: TLabel;
    LblPositionLatitude: TLabel;
    InfoPositionLongitude: TLabel;
    InfoPositionLatitude: TLabel;
    LblCenterLongitude: TLabel;
    LblProviders: TLabel;
    LblZoom: TLabel;
    MapView: TMapView;
    GeoNames: TMVGeoNames;
    BtnLoadMapProviders: TSpeedButton;
    BtnSaveMapProviders: TSpeedButton;
    OpenDialog: TOpenDialog;
    PageControl: TPageControl;
    PgData: TTabSheet;
    PgConfig: TTabSheet;
    POIImages: TImageList;
    PrintDialog1: TPrintDialog;
    rgPOIMode: TRadioGroup;
    ZoomTrackBar: TTrackBar;
    procedure BtnGoToClick(Sender: TObject);
    procedure BtnLoadGPXFileClick(Sender: TObject);
    procedure BtnPrintMapClick(Sender: TObject);
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnGPSPointsClick(Sender: TObject);
    procedure BtnSaveToFileClick(Sender: TObject);
    procedure BtnPOITextFontClick(Sender: TObject);
    procedure CbDebugTilesChange(Sender: TObject);
    procedure CbDoubleBufferChange(Sender: TObject);
    procedure CbFoundLocationsDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure cbPOITextBgColorChange(Sender: TObject);
    procedure CbProvidersChange(Sender: TObject);
    procedure CbShowPOIImageChange(Sender: TObject);
    procedure CbUseThreadsChange(Sender: TObject);
    procedure CbDistanceUnitsChange(Sender: TObject);
    procedure CbZoomToCursorChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure GeoNamesNameFound(const AName: string; const ADescr: String;
      const ALoc: TRealPoint);
    procedure MapViewChange(Sender: TObject);
    procedure MapViewDrawGpsPoint(Sender: TObject;
      ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure MapViewMouseLeave(Sender: TObject);
    procedure MapViewMouseMove(Sender: TObject; {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      {%H-}Shift: TShiftState; X, Y: Integer);
    procedure MapViewZoomChange(Sender: TObject);
    procedure BtnLoadMapProvidersClick(Sender: TObject);
    procedure BtnSaveMapProvidersClick(Sender: TObject);
    procedure rgPOIModeClick(Sender: TObject);
    procedure ZoomTrackBarChange(Sender: TObject);

  private
    POIImage: TCustomBitmap;
    procedure ClearFoundLocations;
    procedure UpdateCoords(X, Y: Integer);
    procedure UpdateDropdownWidth(ACombobox: TCombobox);
    procedure UpdateLocationHistory(ALocation: String);
    procedure UpdateViewportSize;

  public
    procedure ReadFromIni;
    procedure WriteToIni;

  end;

var
  MainForm: TMainForm;


implementation

{$R *.lfm}

uses
  LCLType, IniFiles, Math, FPCanvas, FPImage, GraphType,
  Printers, OSPrinters,
  mvEngine, mvGPX,
  globals, gpsPtForm, gpslistform;

type
  TLocationParam = class
    Descr: String;
    Loc: TRealPoint;
  end;

const
  MAX_LOCATIONS_HISTORY = 50;
  MAP_PROVIDER_FILENAME = 'map-providers.xml';
  HOMEDIR = '../../../../';   
  USE_DMS = true;

var
  PointFormatSettings: TFormatsettings;
  CacheDir: String = HOMEDIR + 'cache/';   // share the cache in both example projects

function CalcIniName: String;
begin
  Result := ChangeFileExt(Application.ExeName, '.ini');
end;


{ TMainForm }

procedure TMainForm.BtnLoadMapProvidersClick(Sender: TObject);
var
  fn: String;
  msg: String;
begin
  fn := Application.Location + MAP_PROVIDER_FILENAME;
  if FileExists(fn) then begin
    if MapView.Engine.ReadProvidersFromXML(fn, msg) then begin
      MapView.GetMapProviders(CbProviders.Items);
      CbProviders.ItemIndex := 0;
      MapView.MapProvider := CbProviders.Text;
    end else
      ShowMessage(msg);
  end;
end;

procedure TMainForm.BtnSaveMapProvidersClick(Sender: TObject);
begin
  MapView.Engine.WriteProvidersToXML(Application.Location + MAP_PROVIDER_FILENAME);
end;

procedure TMainForm.BtnSearchClick(Sender: TObject);
begin
  ClearFoundLocations;
  GeoNames.Search(CbLocations.Text, MapView.DownloadEngine);
  UpdateDropdownWidth(CbFoundLocations);
  UpdateLocationHistory(CbLocations.Text);
  if CbFoundLocations.Items.Count > 0 then CbFoundLocations.ItemIndex := 0;
end;

procedure TMainForm.BtnGPSPointsClick(Sender: TObject);
var
  F: TGpsListViewer;
begin
  F := TGpsListViewer.Create(nil);
  try
    F.MapViewer := MapView;
    F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TMainForm.BtnGoToClick(Sender: TObject);
var
  s: String;
  P: TLocationParam;
begin
  if CbFoundLocations.ItemIndex = -1 then
    exit;

  // Extract parameters of found locations. We need that to get the coordinates.
  s := CbFoundLocations.Items.Strings[CbFoundLocations.ItemIndex];
  P := TLocationParam(CbFoundLocations.Items.Objects[CbFoundLocations.ItemIndex]);
  if P = nil then
    exit;
  CbFoundLocations.Text := s;

  // Show location in center of mapview
  MapView.Zoom := 12;
  MapView.Center := P.Loc;
  MapView.Invalidate;
end;

procedure TMainForm.BtnLoadGPXFileClick(Sender: TObject);
var
  reader: TGpxReader;
  b: TRealArea;
begin
  if OpenDialog.FileName <> '' then
    OpenDialog.InitialDir := ExtractFileDir(OpenDialog.Filename);
  if OpenDialog.Execute then begin
    reader := TGpxReader.Create;
    try
      reader.LoadFromFile(OpenDialog.FileName, MapView.GPSItems, b);
      MapView.Engine.ZoomOnArea(b);
      MapViewZoomChange(nil);
    finally
      reader.Free;
    end;
  end;
end;

procedure TMainForm.BtnPrintMapClick(Sender: TObject);
const
  INCH = 25.4;         // 1" = 25.4 mm
  HOR_MARGIN = 10.0;   // mm
  VERT_MARGIN = 10.0;  // mm
  
  function px2mm(px, ppi: Integer): Double;
  begin
    Result := px / ppi * INCH;
  end;
  
  function mm2px(mm: Double; ppi: Integer): Integer;
  begin
    Result := round(mm / INCH * ppi);
  end;
  
var
  bmp: TBitmap;
  PrintedRect: TRect;
  aspectRatio_map: Double;
  aspectRatio_page: Double;
  mapWidth_mm: Double;
  mapHeight_mm: Double;
  pageWidth_mm: Double;
  pageHeight_mm: Double;
  printedWidth_mm: Double;
  printedHeight_mm: Double;
  factor: Double;
begin
  if PrintDialog1.Execute then
  begin
    Printer.Orientation := poPortrait;
    
    // Scale map such that it fits into the width of the printer paper with
    // a margin at all sides
    mapWidth_mm := px2mm(MapView.Width, ScreenInfo.PixelsPerInchX);
    mapHeight_mm := px2mm(MapView.Height, ScreenInfo.PixelsPerInchY);
    pageWidth_mm := px2mm(Printer.PageWidth, Printer.XDPI) - 2*HOR_MARGIN;
    pageHeight_mm := px2mm(Printer.PageHeight, Printer.YDPI) - 2*VERT_MARGIN;
    aspectRatio_map := mapHeight_mm / mapWidth_mm;
    aspectRatio_page := pageHeight_mm / pageWidth_mm;
    if aspectRatio_Map > aspectRatio_page then
    begin
      factor := pageHeight_mm / mapHeight_mm;
      printedWidth_mm := mapWidth_mm * factor;
      printedHeight_mm := pageHeight_mm;
    end else 
    begin
      factor := pageWidth_mm / mapWidth_mm;
      printedWidth_mm := pageWidth_mm;
      printedHeight_mm := mapHeight_mm * factor;
    end;
    // Calculate size of the output rectangle (on paper).
    PrintedRect := Rect(0, 0, mm2px(printedWidth_mm, Printer.XDPI), mm2px(printedHeight_mm, Printer.YDPI));
    // Center output rectangle horizontally on page
    OffsetRect(PrintedRect, mm2px(HOR_MARGIN + (pageWidth_mm-printedWidth_mm) / 2 , Printer.XDPI), mm2px(VERT_MARGIN, Printer.YDPI));

    // Begin printing
    Printer.BeginDoc;
    try
      // Catch the displayed images as a bitmap.
      bmp := TBitmap.Create;
      try
        bmp.SetSize(MapView.Width, MapView.Height);
        MapView.DrawingEngine.PaintToCanvas(bmp.Canvas);
  
        // Stretch-draw the map image to the output rectangle on the printer. 
        Printer.Canvas.StretchDraw(PrintedRect, bmp);
      finally
        bmp.Free;
      end;
        
    finally
      Printer.EndDoc;
    end;
  end;
end;

procedure TMainForm.BtnSaveToFileClick(Sender: TObject);
begin
  MapView.SaveToFile(TPortableNetworkGraphic, 'mapview.png');
  ShowMessage('Map saved to "mapview.png".');
end;

procedure TMainForm.BtnPOITextFontClick(Sender: TObject);
begin
  FontDialog.Font.Assign(MapView.Font);
  if FontDialog.Execute then
    MapView.Font.Assign(FontDialog.Font);
end;

procedure TMainForm.CbDebugTilesChange(Sender: TObject);
begin
  MapView.DebugTiles := CbDebugTiles.Checked;
end;

procedure TMainForm.CbDoubleBufferChange(Sender: TObject);
begin
  MapView.DoubleBuffered := CbDoubleBuffer.Checked;
end;

procedure TMainForm.CbFoundLocationsDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  s: String;
  P: TLocationParam;
  combo: TCombobox;
  x, y: Integer;
begin
  combo := TCombobox(Control);
  if (State * [odSelected, odFocused] <> []) then begin
    combo.Canvas.Brush.Color := clHighlight;
    combo.Canvas.Font.Color := clHighlightText;
  end else begin
    combo.Canvas.Brush.Color := clWindow;
    combo.Canvas.Font.Color := clWindowText;
  end;
  combo.Canvas.FillRect(ARect);
  combo.Canvas.Brush.Style := bsClear;
  s := combo.Items.Strings[Index];
  P := TLocationParam(combo.Items.Objects[Index]);
  x := ARect.Left + 2;
  y := ARect.Top + 2;
  combo.Canvas.Font.Style := [fsBold];
  combo.Canvas.TextOut(x, y, s);
  inc(y, combo.Canvas.TextHeight('Tg'));
  combo.Canvas.Font.Style := [];
  combo.Canvas.TextOut(x, y, P.Descr);
end;

procedure TMainForm.cbPOITextBgColorChange(Sender: TObject);
begin
  MapView.POITextBgColor := cbPOITextBgColor.Selected;
end;

procedure TMainForm.CbProvidersChange(Sender: TObject);
begin
  MapView.MapProvider := CbProviders.Text;
end;

procedure TMainForm.CbShowPOIImageChange(Sender: TObject);
begin
  {
  if CbShowPOIImage.Checked then
    MapView.POIImage.Assign(POIImage)
  else
    MapView.POIImage.Clear;
    }
end;

procedure TMainForm.CbUseThreadsChange(Sender: TObject);
begin
  MapView.UseThreads := CbUseThreads.Checked;
end;

procedure TMainForm.CbDistanceUnitsChange(Sender: TObject);
begin
  DistanceUnit := TDistanceUnits(CbDistanceUnits.ItemIndex);
  UpdateViewPortSize;
end;

procedure TMainForm.CbZoomToCursorChange(Sender: TObject);
begin
  MapView.ZoomToCursor := CbZoomToCursor.Checked;
end;

procedure TMainForm.ClearFoundLocations;
var
  i: Integer;
  P: TLocationParam;
begin
  for i:=0 to CbFoundLocations.Items.Count-1 do begin
    P := TLocationParam(CbFoundLocations.Items.Objects[i]);
    P.Free;
  end;
  CbFoundLocations.Items.Clear;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  fn: String;
begin
  cInputQueryEditSizePercents := 0;

  fn := Application.Location + 'mapmarker.png';
  if not FileExists(fn) then
    MessageDlg('Copy the file "mapmarker.png" from the source folder to the folder with the executable.',
      mtError, [mbOK], 0)
  else
  begin
    //  FMapMarker := CreateMapMarker(32, clRed, clBlack);
    POIImage := TPortableNetworkGraphic.Create;
    POIImage.PixelFormat := pf32bit;
    POIImage.LoadFromFile(fn);
  end;

  CacheDir := HOMEDIR + 'cache/';
  if not ForceDirectories(CacheDir) then
  begin
    CacheDir := GetAppConfigDir(false) + 'cache/';
    ForceDirectories(CacheDir);
  end;
  MapView.CachePath := CacheDir;
  MapView.GetMapProviders(CbProviders.Items);
  CbProviders.ItemIndex := CbProviders.Items.Indexof(MapView.MapProvider);
  MapView.DoubleBuffered := true;
  MapView.Zoom := 1;
  CbZoomToCursor.Checked := MapView.ZoomToCursor;
  CbUseThreads.Checked := MapView.UseThreads;
  CbDoubleBuffer.Checked := MapView.DoubleBuffered;
  CbPOITextBgColor.Selected := MapView.POITextBgColor;

  InfoPositionLongitude.Caption := '';
  InfoPositionLatitude.Caption := '';
  InfoCenterLongitude.Caption := '';
  InfoCenterLatitude.Caption := '';
  InfoViewportWidth.Caption := '';
  InfoViewportHeight.Caption := '';
  GPSPointInfo.caption := '';

  ReadFromIni;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WriteToIni;
  ClearFoundLocations;
  FreeAndNil(POIImage)
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  MapView.Active := true;
end;

procedure TMainForm.GeoNamesNameFound(const AName: string;
  const ADescr: String; const ALoc: TRealPoint);
var
  P: TLocationParam;
begin
  P := TLocationParam.Create;
  P.Descr := ADescr;
  P.Loc := ALoc;
  CbFoundLocations.Items.AddObject(AName, P);
end;

procedure TMainForm.MapViewChange(Sender: TObject);
begin
  UpdateViewportSize;
end;

procedure TMainForm.MapViewDrawGpsPoint(Sender: TObject;
  ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
var
  P: TPoint;
  d: Integer;
  sym: TGPSSymbol;
  clr: TColor;
  extent: TSize;
begin
  // Screen coordinates of the GPS point
  P := TMapView(Sender).LonLatToScreen(APoint.RealPoint);

  // Draw the GPS point as specified by the data in the point's ExtraData
  if not (APoint.ExtraData is TGPSExtraData) then
    exit;

  // Get the POI attributes
  with TGPSExtraData(APoint.ExtraData) do
  begin
    clr := Color;
    sym := Symbol;
    d := Size div 2;
  end;

  // Draw the POI symbol
  ADrawer.PenColor := clr;
  case sym of
    gpsPlus:
      begin
        ADrawer.Line(P.X - d, P.Y, P.X + d, P.Y);
        ADrawer.Line(P.X, P.Y - d, P.X , P.Y + d);
      end;
    gpsCross:
      begin
        ADrawer.Line(P.x - d, P.Y - d, P.X + d, P.Y + d);
        ADrawer.Line(P.x - d, P.Y + d, P.X + d, P.Y - d);
      end;
    gpsFilledCircle:
      begin
        ADrawer.BrushStyle := bsSolid;
        ADrawer.BrushColor := clr;
        ADrawer.Ellipse(P.X - d, P.Y - d, P.X + d, P.Y + d);
      end;
    gpsOpenCircle:
      begin
        ADrawer.BrushStyle := bsClear;
        ADrawer.Ellipse(P.X - d, P.Y - d, P.X + d, P.Y + d);
      end;
    gpsFilledRect:
      begin
        ADrawer.BrushStyle := bsSolid;
        ADrawer.BrushColor := clr;
        ADrawer.Rectangle(P.X - d, P.Y - d, P.X + d, P.Y + d);
      end;
    gpsOpenRect:
      begin
        ADrawer.BrushStyle := bsClear;
        ADrawer.Rectangle(P.X - d, P.Y - d, P.X + d, P.Y + d);
      end;
  end;

  // Prepare text output: background color...
  inc(P.Y, d + 4);
  extent := ADrawer.TextExtent(APoint.Name);
  if cbPOITextBgColor.selected = clNone then
    ADrawer.BrushStyle := bsClear
  else
  begin
    ADrawer.BrushStyle := bsSolid;
    ADrawer.BrushColor := cbPOITextBgColor.Selected;
  end;

  // ... and font
  ADrawer.FontColor := MapView.Font.Color;
  ADrawer.FontName := MapView.Font.Name;
  ADrawer.FontSize := MapView.Font.Size;
  ADrawer.FontStyle := MapView.Font.Style;

  // Write the POI text
  ADrawer.TextOut(P.X - extent.CX div 2, P.Y, APoint.Name);
end;

{  ADrawer.BrushColor := clRed;
  ADrawer.BrushStyle := bsSolid;
  ADrawer.Ellipse(P.X - R, P.Y - R, P.X + R, P.Y + R);
end;
 }
procedure TMainForm.MapViewMouseLeave(Sender: TObject);
begin
  UpdateCoords(MaxInt, MaxInt);
end;

procedure TMainForm.MapViewMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var
  objs: TGpsObjArray;
  L: TStrings;
  i: Integer;
begin
  UpdateCoords(X, Y);

  objs := MapView.ObjsAtScreenPt(X, Y);
  if Length(objs) > 0 then
  begin
    L := TStringList.Create;
    try
      for i := 0 to High(objs) do
        if objs[i] is TGpsPoint then
          with TGpsPoint(objs[i]) do
            L.Add(Format('%s (%s / %s)', [
              Name, LatToStr(Lat, USE_DMS), LonToStr(Lon, USE_DMS)
          ]));
      GPSPointInfo.Caption := L.Text;
    finally
      L.Free;
    end;
  end else
    GPSPointInfo.Caption := '';
end;

procedure TMainForm.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  rPt: TRealPoint;
  gpsPt: TGpsPoint;
  gpsName: String = '';
  gpsSize: Integer = 0;
  gpsSymbol: TGPSSymbol = gpsPlus;
  gpsColor: TColor = clBlack;
  gpsImageIndex: Integer = -1;
begin
  if (Button = mbRight) then begin
    rPt := MapView.ScreenToLonLat(Point(X, Y));

    case rgPOIMode.ItemIndex of
      0, 1:
        begin
          if not InputQuery('Name of GPS location', 'Please enter name', gpsName) then
            exit;
          gpsPt := TGpsPoint.CreateFrom(rPt);
        end;
      2:
        begin
          if GPSPointForm = nil then
          begin
            GPSPointForm := TGPSPointForm.Create(Application);
            GPSPointForm.Position := poMainFormCenter;
          end;
          GPSPointForm.SetImageIndexData('', POIImages, -1);
          if GPSPointForm.ShowModal <> mrOK then
            exit;
          gpsPt := TGpsPointOfInterest.CreateFrom(rPt);
          GPSPointForm.GetImageIndexData(gpsName, gpsImageIndex);
          TGPSPointOfInterest(gpsPt).ImageIndex := gpsImageIndex;
        end;
      3:
        begin
          if GPSPointForm = nil then
          begin
            GPSPointForm := TGPSPointForm.Create(Application);
            GPSPointForm.Position := poMainFormCenter;
          end;
          GPSPointForm.SetSymbolData('', clRed, gpsPlus, 10);
          if GPSPointForm.ShowModal <> mrOK then
            exit;
          gpsPt := TGpsPoint.CreateFrom(rPt);
          GPSPointForm.GetSymbolData(gpsName, gpsColor, gpsSymbol, gpsSize);
          gpsPt.ExtraData := TGPSExtraData.Create(_CLICKED_POINTS_);
          with TGPSExtraData(gpsPt.ExtraData) do
          begin
            Color := gpsColor;
            Symbol := gpsSymbol;
            Size := gpsSize;
          end;
        end;
    end;
    gpsPt.Name := gpsName;

    MapView.GpsItems.Add(gpsPt, _CLICKED_POINTS_);
  end;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  ZoomTrackbar.Position := MapView.Zoom;
end;

procedure TMainForm.ReadFromIni;
var
  ini: TCustomIniFile;
  List: TStringList;
  L, T, W, H: Integer;
  R: TRect;
  i: Integer;
  s: String;
  pt: TRealPoint;
  du: TDistanceUnits;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    HERE_AppID := ini.ReadString('HERE', 'APP_ID', '');
    HERE_AppCode := ini.ReadString('HERE', 'APP_CODE', '');
    OpenWeatherMap_ApiKey := ini.ReadString('OpenWeatherMap', 'API_Key', '');
    ThunderForest_ApiKey := ini.ReadString('ThunderForest', 'API_Key', '');

    if ((HERE_AppID <> '') and (HERE_AppCode <> '')) or
       (OpenWeatherMap_ApiKey <> '') or
       (ThunderForest_ApiKey <> '') then
    begin
      MapView.Engine.ClearMapProviders;
      MapView.Engine.RegisterProviders;
      MapView.GetMapProviders(CbProviders.Items);
    end;

    R := Screen.DesktopRect;
    L := ini.ReadInteger('MainForm', 'Left', Left);
    T := ini.ReadInteger('MainForm', 'Top', Top);
    W := ini.ReadInteger('MainForm', 'Width', Width);
    H := ini.ReadInteger('MainForm', 'Height', Height);
    if L + W > R.Right then L := R.Right - W;
    if L < R.Left then L := R.Left;
    if T + H > R.Bottom then T := R.Bottom - H;
    if T < R.Top then T := R.Top;
    SetBounds(L, T, W, H);

    s := ini.ReadString('MapView', 'Provider', MapView.MapProvider);
    if CbProviders.Items.IndexOf(s) = -1 then begin
      MessageDlg('Map provider "' + s + '" not found.', mtError, [mbOK], 0);
      s := CbProviders.Items[0];
    end;
    MapView.MapProvider := s;
    CbProviders.Text := MapView.MapProvider;

    MapView.Zoom := ini.ReadInteger('MapView', 'Zoom', MapView.Zoom);
    pt.Lon := StrToFloatDef(ini.ReadString('MapView', 'Center.Longitude', ''), 0.0, PointFormatSettings);
    pt.Lat := StrToFloatDef(ini.ReadString('MapView', 'Center.Latitude', ''), 0.0, PointFormatSettings);
    MapView.Center := pt;

    s := ini.ReadString('MapView', 'DistanceUnits', '');
    if s <> '' then begin
      for du in TDistanceUnits do
        if DistanceUnit_Names[du] = s then begin
          DistanceUnit := du;
          CbDistanceUnits.ItemIndex := ord(du);
          break;
        end;
    end;

    List := TStringList.Create;
    try
      ini.ReadSection('Locations', List);
      for i:=0 to List.Count-1 do begin
        s := ini.ReadString('Locations', List[i], '');
        if s <> '' then
          CbLocations.Items.Add(s);
      end;
    finally
      List.Free;
    end;

  finally
    ini.Free;
  end;
end;

procedure TMainForm.rgPOIModeClick(Sender: TObject);
begin
  case rgPOIMode.ItemIndex of
    0: begin  // default
         MapView.POIImage.Clear;
         MapView.OnDrawGPSPoint := nil;
       end;
    1: begin  // default POI image
         MapView.POIImage.Assign(POIImage);
         MapView.OnDrawGPSPoint := nil;
       end;
    2: ;      // image from image list
    3: begin  // symbol
         MapView.POIImage.Clear;
         MapView.OnDrawGPSPoint := @MapViewDrawGpsPoint;
       end;
  end;
end;

procedure TMainForm.UpdateCoords(X, Y: Integer);
var
  rPt: TRealPoint;
begin
  rPt := MapView.Center;
  InfoCenterLongitude.Caption := LonToStr(rPt.Lon, USE_DMS);
  InfoCenterLatitude.Caption := LatToStr(rPt.Lat, USE_DMS);

  if (X <> MaxInt) and (Y <> MaxInt) then begin
    rPt := MapView.ScreenToLonLat(Point(X, Y));
    InfoPositionLongitude.Caption := LonToStr(rPt.Lon, USE_DMS);
    InfoPositionLatitude.Caption := LatToStr(rPt.Lat, USE_DMS);
  end else begin
    InfoPositionLongitude.Caption := '-';
    InfoPositionLatitude.Caption := '-';
  end;
end;

procedure TMainForm.UpdateDropdownWidth(ACombobox: TCombobox);
var
  cnv: TControlCanvas;
  i, w: Integer;
  s: String;
  P: TLocationParam;
begin
  w := 0;
  cnv := TControlCanvas.Create;
  try
    cnv.Control := ACombobox;
    cnv.Font.Assign(ACombobox.Font);
    for i:=0 to ACombobox.Items.Count-1 do begin
      cnv.Font.Style := [fsBold];
      s := ACombobox.Items.Strings[i];
      w := Max(w, cnv.TextWidth(s));
      P := TLocationParam(ACombobox.Items.Objects[i]);
      cnv.Font.Style := [];
      w := Max(w, cnv.TextWidth(P.Descr));
    end;
    ACombobox.ItemWidth := w + 16;
    ACombobox.ItemHeight := 2 * cnv.TextHeight('Tg') + 6;
  finally
    cnv.Free;
  end;
end;

procedure TMainForm.UpdateLocationHistory(ALocation: String);
var
  idx: Integer;
begin
  idx := CbLocations.Items.IndexOf(ALocation);
  if idx <> -1 then
    CbLocations.Items.Delete(idx);
  CbLocations.Items.Insert(0, ALocation);
  while CbLocations.Items.Count > MAX_LOCATIONS_HISTORY do
    CbLocations.Items.Delete(Cblocations.items.Count-1);
  CbLocations.Text := ALocation;
end;

procedure TMainForm.UpdateViewportSize;
begin
  InfoViewportWidth.Caption := Format('%.2n %s', [
    CalcGeoDistance(
      MapView.GetVisibleArea.TopLeft.Lat,
      MapView.GetVisibleArea.TopLeft.Lon,
      MapView.GetVisibleArea.TopLeft.Lat,
      MapView.GetVisibleArea.BottomRight.Lon,
      DistanceUnit
    ),
    DistanceUnit_Names[DistanceUnit]
  ]);
  InfoViewportHeight.Caption := Format('%.2n %s', [
    CalcGeoDistance(
      MapView.GetVisibleArea.TopLeft.Lat,
      MapView.GetVisibleArea.TopLeft.Lon,
      MapView.GetVisibleArea.BottomRight.Lat,
      MapView.GetVisibleArea.TopLeft.Lon,
      DistanceUnit
    ),
    DistanceUnit_Names[DistanceUnit]
  ]);
end;

procedure TMainForm.WriteToIni;
var
  ini: TCustomIniFile;
  i: Integer;
begin
  ini := TMemIniFile.Create(CalcIniName);
  try
    ini.WriteInteger('MainForm', 'Left', Left);
    ini.WriteInteger('MainForm', 'Top', Top);
    ini.WriteInteger('MainForm', 'Width', Width);
    ini.WriteInteger('MainForm', 'Height', Height);

    ini.WriteString('MapView', 'Provider', MapView.MapProvider);
    ini.WriteInteger('MapView', 'Zoom', MapView.Zoom);
    ini.WriteString('MapView', 'Center.Longitude', FloatToStr(MapView.Center.Lon, PointFormatSettings));
    ini.WriteString('MapView', 'Center.Latitude', FloatToStr(MapView.Center.Lat, PointFormatSettings));

    ini.WriteString('MapView', 'DistanceUnits', DistanceUnit_Names[DistanceUnit]);

    if HERE_AppID <> '' then
      ini.WriteString('HERE', 'APP_ID', HERE_AppID);
    if HERE_AppCode <> '' then
      ini.WriteString('HERE', 'APP_CODE', HERE_AppCode);

    if OpenWeatherMap_ApiKey <> '' then
      ini.WriteString('OpenWeatherMap', 'API_Key', OpenWeatherMap_ApiKey);

    if ThunderForest_ApiKey <> '' then
      ini.WriteString('ThunderForest', 'API_Key', ThunderForest_ApiKey);

    ini.EraseSection('Locations');
    for i := 0 to CbLocations.Items.Count-1 do
      ini.WriteString('Locations', 'Item'+IntToStr(i), CbLocations.Items[i]);

  finally
    ini.Free;
  end;
end;

procedure TMainForm.ZoomTrackBarChange(Sender: TObject);
begin
  MapView.Zoom := ZoomTrackBar.Position;
  LblZoom.Caption := Format('Zoom (%d):', [ZoomTrackbar.Position]);
end;


initialization
  PointFormatSettings.DecimalSeparator := '.';

end.

