{ Drawing engine based on Lazarus IntfGraphics routines
  (C) 2014 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDE_IntfGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Types, LclVersion,
  FPImage, FPCanvas, IntfGraphics, LazCanvas,
  mvDrawingEngine;

type
  TMvIntfGraphicsLayer = class(TMvLayer)
  private
    FBuffer: TLazIntfImage;
    FCanvas: TFPCustomCanvas;
    FFontName: String;
    FFontColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
  public
    destructor Destroy; override;
    property Buffer: TLazIntfImage read FBuffer;
    property Canvas: TFPCustomCanvas read FCanvas;
    property FontName: String read FFontName write FFontName;
    property FontColor: TColor read FFontColor write FFontColor;
    property FontSize: Integer read FFontSize write FFontSize;
    property FontStyle: TFontStyles read FFontStyle write FFontStyle;
  end;

  { TMvIntfGraphicsDrawingEngine }

  TMvIntfGraphicsDrawingEngine = class(TMvCustomDrawingEngine)
  private
    procedure CreateLazIntfImageAndCanvas(out ABuffer: TLazIntfImage;
      out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
    function GetLockActiveBuffer(var ALockedLayer : TMvIntfGraphicsLayer): TLazIntfImage;
    function GetLockActiveCanvas(var ALockedLayer : TMvIntfGraphicsLayer): TFpCustomCanvas;
    function GetLockActiveLayer: TMvIntfGraphicsLayer;
    function GetLayer(AIndex: Integer): TMvIntfGraphicsLayer;
    procedure MergeLayers;
  protected
    function GetBrushColor: TColor; override;
    function GetBrushStyle: TBrushStyle; override;
    function GetFontColor: TColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TFontStyles; override;
    function GetLayerClass: TMvLayerClass; override;
    function GetPenColor: TColor; override;
    function GetPenWidth: Integer; override;
    procedure SetBrushColor(AValue: TColor); override;
    procedure SetBrushStyle(AValue: TBrushStyle); override;
    procedure SetFontColor(AValue: TColor); override;
    procedure SetFontName(AValue: String); override;
    procedure SetFontSize(AValue: Integer); override;
    procedure SetFontStyle(AValue: TFontStyles); override;
    procedure SetPenColor(AValue: TColor); override;
    procedure SetPenWidth(AValue: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateBuffer(AWidth, AHeight: Integer); override;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      UseAlphaChannel: Boolean); override;
    procedure DrawLazIntfImage(X, Y: Integer; AImg: TLazIntfImage); override;
    procedure DrawScaledLazIntfImage(DestRect, SrcRect: TRect; ASrcImg: TLazIntfImage); override;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); override;
    procedure FillPixels(X1, Y1, X2, Y2: Integer; AColor: TColor); override;
    procedure FillRect(X1, Y1, X2, Y2: Integer); override;
    procedure Line(X1, Y1, X2, Y2: Integer); override;
    procedure PaintToCanvas(ACanvas: TCanvas); override;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); override;
    function SaveToImage(AClass: TRasterImageClass): TRasterImage; override;
    function TextExtent(const AText: String): TSize; override;
    procedure TextOut(X, Y: Integer; const AText: String); override;
  end;


implementation

uses
  LCLType, LCLIntf,
  FPImgCanv, GraphType,
  mvTypes;

function InRange(x, min, max: Integer): Boolean;
begin
  Result := (x >= min) and (x <= max);
end;


{$IF Lcl_FullVersion < 1090000}

function IfThen(ACondition: Boolean; a, b: Integer): Integer;
begin
  if ACondition then Result := a else Result := b;
end;

// Workaround for http://mantis.freepascal.org/view.php?id=27144
procedure CopyPixels(ASource, ADest: TLazIntfImage;
  XDst: Integer = 0; YDst: Integer = 0;
  AlphaMask: Boolean = False; AlphaTreshold: Word = 0);
var
  SrcHasMask, DstHasMask: Boolean;
  x, y, xStart, yStart, xStop, yStop: Integer;
  c: TFPColor;
  SrcRawImage, DestRawImage: TRawImage;
begin
  ASource.GetRawImage(SrcRawImage);
  ADest.GetRawImage(DestRawImage);

  if DestRawImage.Description.IsEqual(SrcRawImage.Description) and (XDst =  0) and (YDst = 0) then
  begin
    // same description -> copy
    if DestRawImage.Data <> nil then
      System.Move(SrcRawImage.Data^, DestRawImage.Data^, DestRawImage.DataSize);
    if DestRawImage.Mask <> nil then
      System.Move(SrcRawImage.Mask^, DestRawImage.Mask^, DestRawImage.MaskSize);
    Exit;
  end;

  // copy pixels
  XStart := IfThen(XDst < 0, -XDst, 0);
  YStart := IfThen(YDst < 0, -YDst, 0);
  XStop := IfThen(ADest.Width - XDst < ASource.Width, ADest.Width - XDst, ASource.Width) - 1;
  YStop := IfTHen(ADest.Height - YDst < ASource.Height, ADest.Height - YDst, ASource.Height) - 1;

  SrcHasMask := SrcRawImage.Description.MaskBitsPerPixel > 0;
  DstHasMask := DestRawImage.Description.MaskBitsPerPixel > 0;

  if DstHasMask then begin
    for y:= yStart to yStop do
      for x:=xStart to xStop do
        ADest.Masked[x+XDst,y+YDst] := SrcHasMask and ASource.Masked[x,y];
  end;

  for y:=yStart to yStop do
    for x:=xStart to xStop do
    begin
      c := ASource.Colors[x,y];
      if not DstHasMask and SrcHasMask and (c.alpha = $FFFF) then // copy mask to alpha channel
        if ASource.Masked[x,y] then
          c.alpha := 0;

      ADest.Colors[x+XDst,y+YDst] := c;
      if AlphaMask and (c.alpha < AlphaTreshold) then
        ADest.Masked[x+XDst,y+YDst] := True;
    end;
end;

{$IFEND}


{ TMvIntfGraphicsLayer }

destructor TMvIntfGraphicsLayer.Destroy;
begin
  FBuffer.Free;
  FCanvas.Free;
  inherited;
end;


{  TMvIntfGraphicsDrawingengine  }

constructor TMvIntfGraphicsDrawingEngine.Create(AOwner: TComponent);
begin
  inherited;
  AddLayer(OUTPUT_LAYER);
  //SetActiveLayer(OUTPUT_LAYER);
  AddLayer(MAP_LAYER);
  SetActiveLayer(MAP_LAYER);
end;

procedure TMvIntfGraphicsDrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
var
  i: Integer;
  layer: TMvIntfGraphicsLayer;
begin
  LayerListEnter;
  try
    for i := 0 to FLayerList.Count-1 do
    begin
      layer := TMvIntfGraphicsLayer(GetLayer(i));
      layer.LayerEnter;
      try
        layer.FCanvas.Free;
        layer.FBuffer.Free;
        CreateLazIntfImageAndCanvas(layer.FBuffer, layer.FCanvas, AWidth, AHeight);
      finally
        layer.LayerLeave;
      end;
    end;
  finally
    LayerListLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.CreateLazIntfImageAndCanvas(
  out ABuffer: TLazIntfImage;
  out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
var
  rawImg: TRawImage;
begin
  rawImg.Init;
  {$IFDEF DARWIN}
  rawImg.Description.Init_BPP32_A8R8G8B8_BIO_TTB(AWidth, AHeight);
  {$ELSE}
  rawImg.Description.Init_BPP32_B8G8R8_BIO_TTB(AWidth, AHeight);
//  rawImg.Description.Init_BPP32_B8G8R8A8_BIO_TTB(AWidth, AHeight);
  {$ENDIF}
  rawImg.CreateData(True);
  ABuffer := TLazIntfImage.Create(rawImg, true);
//  ACanvas := TFPImageCanvas.Create(ABuffer);
  ACanvas := TLazCanvas.Create(ABuffer);
  ACanvas.Brush.FPColor := colWhite;
  ACanvas.FillRect(0, 0, AWidth, AHeight);
end;

{ Draws the specified bitmap on the buffer of the active layer. }
procedure TMvIntfGraphicsDrawingEngine.DrawBitmap(X, Y: Integer;
  ABitmap: TCustomBitmap; UseAlphaChannel: Boolean);
var
  intfImg: TLazIntfImage;
  i, j: Integer;
  cimg, cbuf: TFPColor;
  alpha: Double;
  buf: TLazIntfImage;
  layer: TMvIntfGraphicsLayer;
begin
  { To make the CriticalSection of the List access as short as possible
    the list is not blocked for the whole operation but only for the part of getting
    the active layer and locking it.
    To prevent a hole between the two try...finally blocks a variable is holding the state
    of the LayerLock.
  }
  try
    layer := GetLockActiveLayer;
    buf := layer.Buffer;
    intfImg := ABitmap.CreateIntfImage;
    try
      if UseAlphaChannel then
      begin
        for j := 0 to intfImg.Height - 1 do
          if InRange(j + Y, 0, buf.Height - 1) then
            for i := 0 to intfImg.Width - 1 do
            begin
              cimg := intfImg.Colors[i, j];
              alpha := cimg.Alpha / word($FFFF);
              if InRange(i + X, 0, buf.Width-1) then
              begin
                cbuf := buf.Colors[i + X, j + Y];
                cbuf.Red := Round(alpha * cimg.Red + (1 - alpha) * cbuf.Red);
                cbuf.Green := Round(alpha * cimg.Green + (1 - alpha) * cbuf.Green);
                cbuf.Blue := Round(alpha * cimg.Blue + (1 - alpha) * cbuf.Blue);
                buf.Colors[i + X, j + Y] := cbuf;
              end;
            end;
      end
      else
        for j := 0 to intfImg.Height - 1 do
          if InRange(j + Y, 0, buf.Height - 1) then
            for i := 0 to intfImg.Width - 1 do
              if InRange(i + x, 0, buf.Width-1) then
                buf.Colors[i + X, j + Y] := intfImg.Colors[i, j];
    finally
      intfimg.Free;
    end;
  finally
    layer.LayerLeave;
  end;
end;

{ Draws the specified LazIntfImage on the active buffer layer }
procedure TMvIntfGraphicsDrawingEngine.DrawLazIntfImage(X, Y: Integer;
  AImg: TLazIntfImage);
var
  layer: TMvIntfGraphicsLayer;
  buf: TLazIntfImage;
begin
  try
    layer := GetLockActiveLayer;
    buf := layer.Buffer;
    {$IF Lcl_FullVersion < 1090000}
    { Workaround for //http://mantis.freepascal.org/view.php?id=27144 }
    CopyPixels(AImg, buf, X, Y);
    {$ELSE}
    buf.CopyPixels(AImg, X, Y);
    {$IFEND}
  finally
    layer.LayerLeave;
  end;
end;

{ Scales the rectangle SrcRect of the specified source image (ASrcImg) such
  that it fits into the rectangle DestRect of the Buffer image in the active
  layer. }
procedure TMvIntfGraphicsDrawingEngine.DrawScaledLazIntfImage(
  DestRect, SrcRect: TRect; ASrcImg: TLazIntfImage);
var
  img: TLazIntfImage;
  w, h, x, y: Integer;
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if not Assigned(canv) then
      exit;

    w := SrcRect.Right - SrcRect.Left;
    h := SrcRect.Bottom - SrcRect.Top;

    img := TLazIntfImage.Create(0, 0);
    try
      img.DataDescription := ASrcImg.DataDescription;
      img.SetSize(w, h);
      for y := 0 to h-1 do
        for x := 0 to w-1 do
          img.Colors[x, y] := ASrcImg.Colors[SrcRect.Left + x, SrcRect.Top + y];;
      canv.Interpolation := TFPSharpInterpolation.Create;
      try
        canv.StretchDraw(DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, img);
      finally
        canv.Interpolation.Free;
        canv.Interpolation := nil;
      end;
    finally
      img.Free;
    end;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Ellipse(X1,Y1, X2, Y2);
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer;
  AColor: TColor);
var
  buf: TLazIntfImage;
  c: TFPColor;
  x, y: Integer;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  buf := GetLockActiveBuffer(layer);
  try
    if (X1 >= buf.Width) or (X2 < 0) or (Y1 >= buf.Height) or (Y2 < 0) then
      exit;

    if X1 < 0 then X1 := 0;
    if Y1 < 0 then Y1 := 0;
    if X2 >= buf.Width then X2 := buf.Width - 1;
    if Y2 >= buf.Height then Y2 := buf.Height - 1;

    c := TColorToFPColor(ColorToRGB(AColor));
    for y := Y1 to Y2 do
      for x := X1 to X2 do
        buf.Colors[x, y] := c;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.FillRect(X1,Y1, X2, Y2);
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetLockActiveBuffer(
  var ALockedLayer: TMvIntfGraphicsLayer): TLazIntfImage;
begin
  ALockedLayer := GetLockActiveLayer;
  Result := ALockedLayer.Buffer;
end;

function TMvIntfGraphicsDrawingEngine.GetLockActiveCanvas(
  var ALockedLayer: TMvIntfGraphicsLayer): TFpCustomCanvas;
begin
  ALockedLayer := GetLockActiveLayer;
  Result := ALockedLayer.Canvas;
end;

function TMvIntfGraphicsDrawingEngine.GetLockActiveLayer: TMvIntfGraphicsLayer;
begin
  LayerListEnter;
  try
    Result := TMvIntfGraphicsLayer(GetActiveLayer);
    Result.LayerEnter;
  finally
    LayerListLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetBrushColor: TColor;
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      Result := FPColorToTColor(canv.Brush.FPColor)
    else
      Result := 0;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetBrushStyle: TBrushStyle;
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      Result := canv.Brush.Style
    else
      Result := bsSolid;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetFontColor: TColor;
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    Result := layer.FontColor;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetFontName: String;
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    Result := layer.FontName;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetFontSize: Integer;
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    Result := layer.FontSize;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetFontStyle: TFontStyles;
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    Result := layer.FontStyle;
  finally
    layer.LayerLeave;
  end;
end;


function TMvIntfGraphicsDrawingEngine.GetLayer(AIndex: Integer): TMvIntfGraphicsLayer;
begin
  Result := TMvIntfGraphicsLayer(inherited GetLayer(AIndex));
end;

function TMvIntfGraphicsDrawingEngine.GetLayerClass: TMvLayerClass;
begin
  Result := TMvIntfGraphicsLayer;
end;

function TMvIntfGraphicsDrawingEngine.GetPenColor: TColor;
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      Result := FPColorToTColor(canv.Pen.FPColor)
    else
      Result := 0;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.GetPenWidth: Integer;
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      Result := canv.Pen.Width
    else
      Result := 0;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Line(X1, Y1, X2, Y2: Integer);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Line(X1, Y1, X2, Y2);
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.MergeLayers;
var
  i: Integer;
  output_layer, map_layer, layer: TMvIntfGraphicsLayer;
  output_canv: TLazCanvas;
  canv: TLazCanvas;
  buf: TLazIntfImage;
begin
  LayerListEnter;
  try
    LayerEnter(0);
    try
      output_layer := GetLayer(0);
      output_canv := output_layer.Canvas as TLazCanvas;

      LayerEnter(1);
      try
        map_layer := GetLayer(1);
        output_layer.Buffer.CopyPixels(map_layer.Buffer);
      finally
        LayerLeave(1);
      end;

      for i := 2 to FLayerList.Count-1 do
      begin
        LayerEnter(i);
        try
          layer := GetLayer(i);
          canv := layer.Canvas as TLazCanvas;
          buf := layer.Buffer;
          output_canv.AlphaBlend(canv, 0, 0, 0, 0, buf.Width, buf.Height);
        finally
          LayerLeave(i);
        end;
      end;
    finally
      LayerLeave(0);
    end;
  finally
    LayerListLeave;
  end;
end;


procedure TMvIntfGraphicsDrawingEngine.PaintToCanvas(ACanvas: TCanvas);
var
  bmp: TBitmap;
  output_buf: TLazIntfImage;
begin
  if not Assigned(ACanvas) then Exit;
  MergeLayers;
  LayerEnter(0);
  try
    output_buf := GetLayer(0).Buffer;
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32Bit;
      bmp.SetSize(output_buf.Width, output_buf.Height);
      bmp.LoadFromIntfImage(output_buf);
      ACanvas.Draw(0, 0, bmp);
    finally
      bmp.Free;
    end;
  finally
    Layerleave(0);
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Rectangle(X1,Y1, X2, Y2);
  finally
    layer.LayerLeave;
  end;
end;

// !!!!!!!!!!!!! TO DO --- merge all layers
function TMvIntfGraphicsDrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
var
  buf: TLazIntfImage;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  buf := GetLockActiveBuffer(layer);
  try
    Result := AClass.Create;
    Result.Width := buf.Width;
    Result.Height := buf.Height;
    Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
    Result.LoadFromIntfImage(buf);
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushColor(AValue: TColor);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Brush.FPColor := TColorToFPColor(AValue);
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushStyle(AValue: TBrushStyle);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Brush.Style := AValue;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontColor(AValue: TColor);
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    layer.FontColor := AValue;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontName(AValue: String);
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    layer.FontName := AValue;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontSize(AValue: Integer);
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    layer.FontSize := AValue;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontStyle(AValue: TFontStyles);
var
  layer: TMvIntfGraphicsLayer;
begin
  try
    layer := GetLockActiveLayer;
    layer.FontStyle := AValue;
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenColor(AValue: TColor);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Pen.FPColor := TColorToFPColor(AValue);
  finally
    layer.LayerLeave;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenWidth(AValue: Integer);
var
  canv: TFPCustomCanvas;
  layer: TMvIntfGraphicsLayer;
begin
  layer := Nil;
  canv := GetLockActiveCanvas(layer);
  try
    if Assigned(canv) then
      canv.Pen.Width := AValue;
  finally
    layer.LayerLeave;
  end;
end;

function TMvIntfGraphicsDrawingEngine.TextExtent(const AText: String): TSize;
var
  layer: TMvIntfGraphicsLayer;
  bmp: TBitmap;
  R: TRect;
  fn : String;
  fsz : Integer;
  fst : TFontStyles;
begin
  try
    layer := GetLockActiveLayer;
    fn := layer.FontName;
    fsz := layer.FontSize;
    fst := layer.FontStyle;
  finally
    layer.LayerLeave;
  end;
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Name := fn;
    bmp.Canvas.Font.Size := fsz;
    bmp.Canvas.Font.Style := fst;
    R := Rect(0, 0, DEFAULT_POI_TEXT_WIDTH, 0);
    DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, DT_WORDBREAK + DT_CALCRECT);
    Result := Size(R.Width, R.Height);
  finally
    bmp.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.TextOut(X, Y: Integer; const AText: String);
var
  bmp: TBitmap;
  ex: TSize;
  img: TLazIntfImage;
  i, j: Integer;
  hb, hm: HBitmap;
  c: TColor;
  fc, tc: TFPColor;
  intens, intens0: Int64;
  alpha: Double;
  R: TRect;
  txtFlags: Integer = DT_CENTER + DT_WORDBREAK;
  layer: TMvIntfGraphicsLayer;
begin
  if (AText = '') then
    exit;
  layer := GetLockActiveLayer;
  try
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32Bit;
      bmp.SetSize(1, 1);
      bmp.Canvas.Font.Name := layer.FontName;
      bmp.Canvas.Font.Size := layer.FontSize;
      bmp.Canvas.Font.Style := layer.FontStyle;
      bmp.Canvas.Font.Color := layer.FontColor;
      ex := TextExtent(AText);
      R := Rect(0, 0, ex.CX, ex.CY);
      bmp.SetSize(ex.CX, ex.CY);
      if GetBrushStyle <> bsClear then
      begin
        bmp.Canvas.Brush.Color := GetBrushColor;
        bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
        DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, txtFlags);
        DrawBitmap(X, Y, bmp, false);
      end
      else
      begin
        if layer.FontColor = clWhite then
          bmp.Canvas.Brush.Color := clBlack
        else
          bmp.Canvas.Brush.Color := clWhite;
        bmp.Canvas.FillRect(R); //0, 0, bmp.Width, bmp.Height);
        DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, txtFlags);
  //      bmp.Canvas.TextOut(0, 0, AText);

        img := bmp.CreateIntfImage;
        try
          fc := TColorToFPColor(bmp.Canvas.Font.Color);
          intens0 := Int64(fc.Red) + fc.Green + fc.Blue;
          for j := 0 to img.Height - 1 do
            for i := 0 to img.Width - 1 do begin
              c := bmp.Canvas.Pixels[i, j];
              tc := TColorToFPColor(c);
              if c = bmp.Canvas.Brush.Color then
                tc.Alpha := alphaTransparent
              else if c = layer.FontColor then
                tc.Alpha := alphaOpaque
              else begin
                intens := Int64(tc.Red) + tc.Green + tc.Blue;
                if intens0 = 0 then
                  alpha := (3 * alphaopaque - intens) / (3 * alphaOpaque - intens0)
                else
                  alpha := intens / intens0;
                tc.Alpha := round(alphaOpaque * alpha);
              end;
              img.Colors[i, j] := tc;
            end;
          img.CreateBitmaps(hb, hm);
          bmp.Handle := hb;
          bmp.MaskHandle := hm;
          DrawBitmap(X, Y, bmp, true);
        finally
          img.Free;
        end;
      end;
    finally
      bmp.Free;
    end;
  finally
    layer.LayerLeave;
  end;
end;

end.

