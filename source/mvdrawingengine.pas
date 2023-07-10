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
  Classes, Contnrs, SysUtils, Graphics, Types, IntfGraphics, syncobjs;

const
  // Reserved layer names
  OUTPUT_LAYER = 'output';  // layer into which the other layers are merged
  MAP_LAYER = 'map';        // layer containing the main map

type
  EMvDrawingEngine = class(Exception);

  { TMvLayer }

  TMvLayer = class
  private
    FName: String;
    FIndex: Integer;
    FLock : TCriticalSection;
    FLockThreadID : TThreadID;
  public
    property Name: String read FName;
    property Index: Integer read FIndex;
    property LockThreadID : TThreadID read FLockThreadID;
    { LayerEnter will try to lock the Layer for sole access. If the layer is locked by
        a different thread, the methods waits until the lock is free.
        LayerLeave must be called as soon as possible.}
    procedure LayerEnter;
    { LayerTryEnter will try to lock the Layer for sole access. If this fails, because
        the layer is locked by a different Thread, then the function will not wait
        but return immediatly.
        The Result is true if the layer is locked, false if not.
        In the result is true, LayerLeave must be called as soon as possible.}
    function LayerTryEnter : Boolean;
    { LayerLeave returns the layer to the unlocked state}
    procedure LayerLeave;
    constructor Create(const AName: String); virtual;
    destructor Destroy;override;
  end;
  TMvLayerClass = class of TMvLayer;

  { TMvCustomDrawingEngine }

  TMvCustomDrawingEngine = class(TComponent)
  protected
    FActiveLayer: TMvLayer;
    FLayerList: TFPObjectList;
    FLayerListLock : TCriticalSection;
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
    {LayerListEnter grants an exclusive access to the layer list (and block other threads) until
      LayerListLeave is called. Always(!) combine the calls with a try...finally-block.}
    procedure LayerListEnter;
    function LayerListTryEnter : Boolean;
    {LayerListLeave terminates the exclusive access to the layer list of the calling thread,
      A prior call to LayerListEnter is mandatory!}
    procedure LayerListLeave;
    {LayerEnter grants an exclusive access to the layer (and blocks other threads) until
      layers LayerLeave-Methods is called.
      It is highle recommended to combine the calls with a try...finally-block!
      Example of usage:
        layer : TMvLayer;
        (...)
        layer := LayerEnter(0);
        if Assigned(layer) then
        try
          // Do something with the layer
        finally
          layer.LayerLeave
        end;
    }
    function LayerEnter(AIndex: Integer) : TMvLayer;
    function LayerEnter(const ALayerName : String) : TMvLayer;

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

procedure TMvLayer.LayerEnter;
var
  ti : Integer;
begin
//  if ti = FLockThreadID then Exit; // This Thread locks the Layer already, so good bye
  FLock.Enter;  // Lock the layer
  ti := GetThreadID();
  FLockThreadID := ti; // remember who is locking the layer.
end;

function TMvLayer.LayerTryEnter: Boolean;
var
  ti : Integer;
begin
  Result := FLock.TryEnter;  // Lock the layer
  if Result then
  begin
    ti := GetThreadID();
    FLockThreadID := ti; // remember who is locking the layer.
  end;
end;

procedure TMvLayer.LayerLeave;
begin
//  if FLockThreadID = 0 then Exit; // The lock is free, so leave should not be called
  FLockThreadID := 0; //After this assignment different threads may goto FLock.Enter
  FLock.Leave; //But since we are leaving here, there will be no longer blocking
end;

constructor TMvLayer.Create(const AName: String);
begin
  inherited Create;
  FLock := TCriticalSection.Create;
  FName := AName;
end;

destructor TMvLayer.Destroy;
begin
  FLock.Free;
  inherited Destroy;
end;


{ TMvCustomDrawingEngine }

constructor TMvCustomDrawingEngine.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLayerListLock := TCriticalSection.Create;
  FLayerList := TFPObjectList.Create;
end;

destructor TMvCustomDrawingEngine.Destroy;
var
  i : Integer;
  ly : TMvLayer;
  cnt : Integer;
begin
  repeat
    cnt := GetLayerCount;
    if cnt <= 0 then Break;
    for i := cnt-1 downto 0 do
    begin
      ly := GetLayer(i);
      if Assigned(ly) then
      begin
        ly.LayerEnter;
        try
          LayerListEnter;
          try
            FLayerList.Extract(ly);
          finally
            LayerListLeave;
          end;
        finally
          ly.LayerLeave;
          ly.Free;
        end;
      end;
    end;
  until False;
  LayerListEnter;
  try
    FLayerList.Free;
  finally
    LayerListLeave;
  end;
  FLayerListLock.Free;
  inherited;
end;

function TMvCustomDrawingEngine.AddLayer(const AName: String): Integer;
var
  layer: TMvLayer;
begin
  LayerListEnter;
  try
    if IndexOfLayer(AName) > -1 then
      raise EMvDrawingEngine.Create('Unique layer name required.');

    layer := GetLayerClass.Create(AName);
    layer.LayerEnter;
    try
      Result := FLayerList.Add(layer);
      layer.FIndex := Result;
    finally
      layer.LayerLeave;
    end;
  finally
    LayerListLeave;
  end;
end;

procedure TMvCustomDrawingEngine.DeleteLayer(const AName: String);
var
  idx: Integer;
  ly : TMvLayer;
begin
  idx := IndexOfLayer(AName);
  if idx > 0 then  // do not delete layer 0, the basic map layer
  begin
    ly := GetLayer(idx);
    if Assigned(ly) then
    begin
      ly.LayerEnter;
      try
        if FActiveLayer = ly then
          FActiveLayer := GetLayer(0); // Activae Default Layer
        LayerListEnter;
        try
          FLayerList.Extract(ly);
        finally
          LayerListLeave;
        end;
      finally
        ly.LayerLeave;
        ly.Free;
      end;
    end;
    UpdateLayerIndices;
  end;
end;

function TMvCustomDrawingEngine.GetActiveLayer: TMvLayer;
begin
  Result := FActiveLayer;
end;

function TMvCustomDrawingEngine.GetLayer(AIndex: Integer): TMvLayer;
begin
  LayerListEnter;
  try
    Result := TMvLayer(FLayerList[AIndex]);
  finally
    LayerListLeave;
  end;
end;

function TMvCustomDrawingEngine.GetLayer(const AName: String): TMvLayer;
var
  idx: Integer;
begin
  LayerListEnter;
  try
    idx := IndexOfLayer(AName);
    if idx <> -1 then
      Result := TMvLayer(FLayerList[idx])
    else
      Result := nil;
  finally
    LayerListLeave;
  end;
end;

function TMvCustomDrawingEngine.GetLayerCount: Integer;
begin
  LayerListEnter;
  try
    Result := FLayerList.Count;
  finally
    LayerListLeave;
  end;
end;

function TMvCustomDrawingEngine.IndexOfLayer(const AName: String): Integer;
begin
  LayerListEnter;
  try
    for Result := 0 to FLayerList.Count-1 do
      if SameText(GetLayer(Result).Name, AName) then
        exit;
  finally
    LayerListLeave;
  end;
  Result := -1;
end;

procedure TMvCustomDrawingEngine.MoveLayer(CurIndex, NewIndex: Integer);
begin
  if (CurIndex = 0) or (NewIndex = 0) then
    raise EMvDrawingEngine.Create('Cannot move map layer (index 0)');
  LayerListEnter;
  try
    FLayerList.Move(CurIndex, NewIndex);
    UpdateLayerIndices;
  finally
    LayerListLeave;
  end;
end;

procedure TMvCustomDrawingEngine.SetActiveLayer(const AName: String);
begin
  LayerListEnter;
  try
    FActiveLayer := GetLayer(AName);
  finally
    LayerListLeave;
  end;
end;

procedure TMvCustomDrawingEngine.LayerListEnter;
begin
  FLayerListLock.Enter;
end;

function TMvCustomDrawingEngine.LayerListTryEnter: Boolean;
begin
  Result := FLayerListLock.TryEnter;
end;

procedure TMvCustomDrawingEngine.LayerListLeave;
begin
  FLayerListLock.Leave;
end;

function TMvCustomDrawingEngine.LayerEnter(AIndex: Integer): TMvLayer;
var
  ly : TMvLayer;
  rnd : Integer;
begin
  Result := Nil;
  RandSeed := (RandSeed + GetTickCount64) and $FFFFFFFF;
  repeat
    repeat
      if LayerListTryEnter then Break;
      rnd := Random(3); // returns 0,1 or 2
      Sleep(rnd);
    until False;
    try
      ly := GetLayer(AIndex);
      if not Assigned(ly) then Exit;
      if ly.LayerTryEnter then
      begin
        Result := ly;
        Exit;
      end;
    finally
      LayerListLeave;
    end;
    rnd := Random(3); // returns 0,1 or 2
    Sleep(rnd);
  until False;
end;

function TMvCustomDrawingEngine.LayerEnter(const ALayerName: String): TMvLayer;
var
  ly : TMvLayer;
  rnd : Integer;
begin
  Result := Nil;
  RandSeed := (RandSeed + GetTickCount64) and $FFFFFFFF;
  repeat
    repeat
      if LayerListTryEnter then Break;
      rnd := Random(3); // returns 0,1 or 2
      Sleep(rnd);
    until False;
    try
      ly := GetLayer(ALayerName);
      if not Assigned(ly) then Exit;
      if ly.LayerTryEnter then
      begin
        Result := ly;
        Exit;
      end;
    finally
      LayerListLeave;
    end;
    rnd := Random(3);
    Sleep(rnd);
  until False;
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
  LayerListEnter;
  try
    for i := 0 to FLayerList.Count-1 do
    begin
      layer := LayerEnter(i);
      if Assigned(layer) then
      try
        layer.FIndex := i;
      finally
        layer.LayerLeave;
      end;
    end;
  finally
    LayerListLeave;
  end;
end;


end.

