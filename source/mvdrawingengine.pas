{ Mapviewer drawing engine
  (C) 2019 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDrawingEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, Contnrs, SysUtils, Graphics, Types, IntfGraphics;

const
  // Reserved layer names
  OUTPUT_LAYER = 'output';  // layer into which the other layers are merged
  MAP_LAYER = 'map';        // layer containing the main map

type
  EMvDrawingEngine = class(Exception);

  TMvLayer = class
  private
    FName: String;
    FIndex: Integer;
  public
    constructor Create(const AName: String); virtual;
    property Name: String read FName;
    property Index: Integer read FIndex;
  end;
  TMvLayerClass = class of TMvLayer;

  TMvCustomDrawingEngine = class(TComponent)
  protected
    FActiveLayer: TMvLayer;
    FLayerList: TFPObjectList;
    function GetLayerClass: TMvLayerClass; virtual; abstract;
    procedure UpdateLayerIndices;

  protected
    function GetBrushColor: TColor; virtual; abstract;
    function GetBrushStyle: TBrushStyle; virtual; abstract;
    function GetFontColor: TColor; virtual; abstract;
    function GetFontName: String; virtual; abstract;
    function GetFontSize: Integer; virtual; abstract;
    function GetFontStyle: TFontStyles; virtual; abstract;
    function GetPenColor: TColor; virtual; abstract;
    function GetPenWidth: Integer; virtual; abstract;
    procedure SetBrushColor(AValue: TColor); virtual; abstract;
    procedure SetBrushStyle(AValue: TBrushStyle); virtual; abstract;
    procedure SetFontColor(AValue: TColor); virtual; abstract;
    procedure SetFontName(AValue: String); virtual; abstract;
    procedure SetFontSize(AValue: Integer); virtual; abstract;
    procedure SetFontStyle(AValue: TFontStyles); virtual; abstract;
    procedure SetPenColor(AValue: TColor); virtual; abstract;
    procedure SetPenWidth(AValue: Integer); virtual; abstract;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Layer management
    function AddLayer(const AName: String): Integer;  virtual;
    procedure DeleteLayer(const AName: String);
    function GetActiveLayer: TMvLayer;
    function GetLayer(AIndex: Integer): TMvLayer;
    function GetLayer(const AName: String): TMvLayer;
    function GetLayerCount: Integer;
    function IndexOfLayer(const AName: String): Integer;
    procedure MoveLayer(CurIndex, NewIndex: Integer);
    procedure SetActiveLayer(const AName: String);

    // Graphics operations
    procedure CreateBuffer(AWidth, AHeight: Integer); virtual; abstract;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      UseAlphaChannel: Boolean); virtual; abstract;
    procedure DrawLazIntfImage(X, Y: Integer; AImg: TLazIntfImage); virtual; abstract;
    procedure DrawScaledLazIntfImage(DestRect, SrcRect: TRect; AImg: TLazIntfImage); virtual; abstract;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); virtual; abstract;
    procedure FillPixels(X1, Y1, X2, Y2: Integer; AColor: TColor); virtual; abstract;
    procedure FillRect(X1, Y1, X2, Y2: Integer); virtual; abstract;
    procedure Line(X1, Y1, X2, Y2: Integer); virtual; virtual; abstract;
    procedure PaintToCanvas(ACanvas: TCanvas); virtual; abstract;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); virtual; abstract;
    function SaveToImage(AClass: TRasterImageClass): TRasterImage; virtual; abstract;
    function TextExtent(const AText: String): TSize; virtual; abstract;
    function TextHeight(const AText: String): Integer;
    procedure TextOut(X, Y: Integer; const AText: String); virtual; abstract;
    function TextWidth(const AText: String): Integer;

    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenWidth: Integer read GetPenWidth write SetPenWidth;
  end;

implementation

{ TMvLayer }

constructor TMvLayer.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
end;


{ TMvCustomDrawingEngine }

constructor TMvCustomDrawingEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayerList := TFPObjectList.Create;
end;

destructor TMvCustomDrawingEngine.Destroy;
begin
  FLayerList.Free;
  inherited;
end;

function TMvCustomDrawingEngine.AddLayer(const AName: String): Integer;
var
  layer: TMvLayer;
begin
  if IndexOfLayer(AName) > -1 then
    raise EMvDrawingEngine.Create('Unique layer name required.');

  layer := GetLayerClass.Create(AName);
  Result := FLayerList.Add(layer);
  layer.FIndex := Result;
end;

procedure TMvCustomDrawingEngine.DeleteLayer(const AName: String);
var
  idx: Integer;
begin
  idx := IndexOfLayer(AName);
  if idx > 0 then  // do not delete layer 0, the basic map layer
  begin
    FLayerList.Delete(idx);
    UpdateLayerIndices;
  end;
end;

function TMvcustomDrawingEngine.GetActiveLayer: TMvLayer;
begin
  Result := FActiveLayer;
end;

function TMvCustomDrawingEngine.GetLayer(AIndex: Integer): TMvLayer;
begin
  Result := TMvLayer(FLayerList[AIndex]);
end;

function TMvCustomDrawingEngine.GetLayer(const AName: String): TMvLayer;
var
  idx: Integer;
begin
  idx := IndexOfLayer(AName);
  if idx <> -1 then
    Result := TMvLayer(FLayerList[idx])
  else
    Result := nil;
end;

function TMvCustomDrawingEngine.GetLayerCount: Integer;
begin
  Result := FLayerList.Count;
end;

function TMvCustomDrawingEngine.IndexOfLayer(const AName: String): Integer;
begin
  for Result := 0 to FLayerList.Count-1 do
    if SameText(GetLayer(Result).Name, AName) then
      exit;
  Result := -1;
end;

procedure TMvCustomDrawingEngine.MoveLayer(CurIndex, NewIndex: Integer);
begin
  if (CurIndex = 0) or (NewIndex = 0) then
    raise EMvDrawingEngine.Create('Cannot move map layer (index 0)');
  FLayerList.Move(CurIndex, NewIndex);
  UpdateLayerIndices;
end;

procedure TMvCustomDrawingEngine.SetActiveLayer(const AName: String);
begin
  FActiveLayer := GetLayer(AName);
end;

function TMvCustomDrawingEngine.TextHeight(const AText: String): Integer;
begin
  Result := TextExtent(AText).CX;
end;

function TMvCustomDrawingEngine.TextWidth(const AText: String): Integer;
begin
  Result := TextExtent(AText).CY;
end;

procedure TMvCustomDrawingEngine.UpdateLayerIndices;
var
  i: Integer;
  layer: TMvLayer;
begin
  for i := 0 to FLayerList.Count-1 do
  begin
    layer := GetLayer(i);
    layer.FIndex := i;
  end;
end;


end.

