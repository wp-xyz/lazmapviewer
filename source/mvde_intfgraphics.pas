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
  TMvIntfGraphicsDrawingEngine = class(TMvCustomDrawingEngine)
  private
    FBuffer: TLazIntfImage;
    FCanvas: TFPCustomCanvas;
    FFontName: String;
    FFontColor: TColor;
    FFontSize: Integer;
    FFontStyle: TFontStyles;
    procedure CreateLazIntfImageAndCanvas(out ABuffer: TLazIntfImage;
      out ACanvas: TFPCustomCanvas; AWidth, AHeight: Integer);
  protected
    function GetBrushColor: TColor; override;
    function GetBrushStyle: TBrushStyle; override;
    function GetFontColor: TColor; override;
    function GetFontName: String; override;
    function GetFontSize: Integer; override;
    function GetFontStyle: TFontStyles; override;
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
    destructor Destroy; override;
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


{  TMvIntfGraphicsDrawingengine  }

destructor TMvIntfGraphicsDrawingEngine.Destroy;
begin
  FCanvas.Free;
  FBuffer.Free;
  inherited;
end;

procedure TMvIntfGraphicsDrawingEngine.CreateBuffer(AWidth, AHeight: Integer);
begin
  FCanvas.Free;
  FBuffer.Free;
  CreateLazIntfImageAndCanvas(FBuffer, FCanvas, AWidth, AHeight);
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
  ACanvas := TFPImageCanvas.Create(ABuffer);
  ACanvas.Brush.FPColor := colWhite;
  ACanvas.FillRect(0, 0, AWidth, AHeight);
end;

procedure TMvIntfGraphicsDrawingEngine.DrawBitmap(X, Y: Integer;
  ABitmap: TCustomBitmap; UseAlphaChannel: Boolean);
var
  intfImg: TLazIntfImage;
  i, j: Integer;
  cimg, cbuf: TFPColor;
  alpha: Double;
begin
  intfImg := ABitmap.CreateIntfImage;
  try
    if UseAlphaChannel then begin
      for j := 0 to intfImg.Height - 1 do
        if InRange(j + Y, 0, FBuffer.Height - 1) then
          for i := 0 to intfImg.Width - 1 do begin
            cimg := intfImg.Colors[i, j];
            alpha := cimg.Alpha / word($FFFF);
            if InRange(i + X, 0, FBuffer.Width-1) then begin
              cbuf := FBuffer.Colors[i + X, j + Y];
              cbuf.Red := Round(alpha * cimg.Red + (1 - alpha) * cbuf.Red);
              cbuf.Green := Round(alpha * cimg.Green + (1 - alpha) * cbuf.Green);
              cbuf.Blue := Round(alpha * cimg.Blue + (1 - alpha) * cbuf.Blue);
              FBuffer.Colors[i + X, j + Y] := cbuf;
            end;
          end;
    end else
      for j := 0 to intfImg.Height - 1 do
        if InRange(j + Y, 0, FBuffer.Height - 1) then
          for i := 0 to intfImg.Width - 1 do
            if InRange(i + x, 0, FBuffer.Width-1) then
              FBuffer.Colors[i + X, j + Y] := intfImg.Colors[i, j];
  finally
    intfimg.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.DrawLazIntfImage(X, Y: Integer;
  AImg: TLazIntfImage);
begin
  {$IF Lcl_FullVersion < 1090000}
  { Workaround for //http://mantis.freepascal.org/view.php?id=27144 }
  CopyPixels(AImg, FBuffer, X, Y);
  {$ELSE}
  FBuffer.CopyPixels(AImg, X, Y);
  {$IFEND}
end;

{ Scales the rectangle SrcRect of the specified source image (ASrcImg) such
  that it fits into the rectangle DestRect of the Buffer image. }
procedure TMvIntfGraphicsDrawingEngine.DrawScaledLazIntfImage(
  DestRect, SrcRect: TRect; ASrcImg: TLazIntfImage);
var
  img: TLazIntfImage;
  w, h, x, y: Integer;
begin
  if FCanvas = nil then
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
    FCanvas.Interpolation := TFPSharpInterpolation.Create;
    try
      FCanvas.StretchDraw(DestRect.Left, DestRect.Top, DestRect.Width, DestRect.Height, img);
    finally
      FCanvas.Interpolation.Free;
      FCanvas.Interpolation := nil;
    end;
  finally
    img.Free;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Ellipse(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Ellipse(X1,Y1, X2, Y2);
end;

procedure TMvIntfGraphicsDrawingEngine.FillPixels(X1, Y1, X2, Y2: Integer;
  AColor: TColor);
var
  c: TFPColor;
  x, y: Integer;
begin
  if (X1 >= FBuffer.Width) or (X2 < 0) or (Y1 >= FBuffer.Height) or (Y2 < 0) then
    exit;

  if X1 < 0 then X1 := 0;
  if Y1 < 0 then Y1 := 0;
  if X2 >= FBuffer.Width then X2 := FBuffer.Width - 1;
  if Y2 >= FBuffer.Height then Y2 := FBuffer.Height - 1;

  c := TColorToFPColor(ColorToRGB(AColor));
  for y := Y1 to Y2 do
    for x := X1 to X2 do
      FBuffer.Colors[x, y] := c;
end;

procedure TMvIntfGraphicsDrawingEngine.FillRect(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.FillRect(X1,Y1, X2, Y2);
end;

function TMvIntfGraphicsDrawingEngine.GetBrushColor: TColor;
begin
  if FCanvas <> nil then
    Result := FPColorToTColor(FCanvas.Brush.FPColor)
  else
    Result := 0;
end;

function TMvIntfGraphicsDrawingEngine.GetBrushStyle: TBrushStyle;
begin
  if FCanvas <> nil then
    Result := FCanvas.Brush.Style
  else
    Result := bsSolid;
end;

function TMvIntfGraphicsDrawingEngine.GetFontColor: TColor;
begin
  Result := FFontColor
end;

function TMvIntfGraphicsDrawingEngine.GetFontName: String;
begin
  Result := FFontName;
end;

function TMvIntfGraphicsDrawingEngine.GetFontSize: Integer;
begin
  Result := FFontSize;
end;

function TMvIntfGraphicsDrawingEngine.GetFontStyle: TFontStyles;
begin
  Result := FFontStyle;
end;

function TMvIntfGraphicsDrawingEngine.GetPenColor: TColor;
begin
  if FCanvas <> nil then
    Result := FPColorToTColor(FCanvas.Pen.FPColor)
  else
    Result := 0;
end;

function TMvIntfGraphicsDrawingEngine.GetPenWidth: Integer;
begin
  if FCanvas <> nil then
    Result := FCanvas.Pen.Width
  else
    Result := 0;
end;

procedure TMvIntfGraphicsDrawingEngine.Line(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Line(X1, Y1, X2, Y2);
end;

procedure TMvIntfGraphicsDrawingEngine.PaintToCanvas(ACanvas: TCanvas);
var
  bmp: TBitmap;
begin
  if ACanvas <> nil then begin
    bmp := TBitmap.Create;
    try
      bmp.PixelFormat := pf32Bit;
      bmp.SetSize(FBuffer.Width, FBuffer.Height);
      bmp.LoadFromIntfImage(FBuffer);
      ACanvas.Draw(0, 0, bmp);
    finally
      bmp.Free;
    end;
  end;
end;

procedure TMvIntfGraphicsDrawingEngine.Rectangle(X1, Y1, X2, Y2: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Rectangle(X1,Y1, X2, Y2);
end;

function TMvIntfGraphicsDrawingEngine.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := AClass.Create;
  Result.Width := FBuffer.Width;
  Result.Height := FBuffer.Height;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
  Result.LoadFromIntfImage(FBuffer);
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushColor(AValue: TColor);
begin
  if FCanvas <> nil then
    FCanvas.Brush.FPColor := TColorToFPColor(AValue);
end;

procedure TMvIntfGraphicsDrawingEngine.SetBrushStyle(AValue: TBrushStyle);
begin
  if FCanvas <> nil then
    FCanvas.Brush.Style := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontColor(AValue: TColor);
begin
  FFontColor := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontName(AValue: String);
begin
  FFontName := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontSize(AValue: Integer);
begin
  FFontSize := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetFontStyle(AValue: TFontStyles);
begin
  FFontStyle := AValue;
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenColor(AValue: TColor);
begin
  if FCanvas <> nil then
    FCanvas.Pen.FPColor := TColorToFPColor(AValue);
end;

procedure TMvIntfGraphicsDrawingEngine.SetPenWidth(AValue: Integer);
begin
  if FCanvas <> nil then
    FCanvas.Pen.Width := AValue;
end;

function TMvIntfGraphicsDrawingEngine.TextExtent(const AText: String): TSize;
var
  bmp: TBitmap;
  R: TRect;
begin
  bmp := TBitmap.Create;
  try
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
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
begin
  if (FCanvas = nil) or (AText = '') then
    exit;

  bmp := TBitmap.Create;
  try
    bmp.PixelFormat := pf32Bit;
    bmp.SetSize(1, 1);
    bmp.Canvas.Font.Name := FFontName;
    bmp.Canvas.Font.Size := FFontSize;
    bmp.Canvas.Font.Style := FFontStyle;
    bmp.Canvas.Font.Color := FFontColor;
    ex := TextExtent(AText);
    R := Rect(0, 0, ex.CX, ex.CY);
    bmp.SetSize(ex.CX, ex.CY);
    if GetBrushStyle <> bsClear then begin
      bmp.Canvas.Brush.Color := GetBrushColor;
      bmp.Canvas.FillRect(0, 0, bmp.Width, bmp.Height);
      DrawText(bmp.Canvas.Handle, PChar(AText), Length(AText), R, txtFlags);
      DrawBitmap(X, Y, bmp, false);
    end else
    begin
      if FFontColor = clWhite then
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
            else if c = FFontColor then
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
end;

end.

