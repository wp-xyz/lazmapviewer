{ (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvMapProvider;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_dom;

type

  { TTileId }

  TTileId = record
    X, Y: int64;
    Z: integer;
  end;

  TGetSvrStr = function (id: integer): string;
  TGetValStr = function (const Tile: TTileId): String;
  TProjectionType = (ptEPSG3857, ptEPSG3395);

  TMapProvider = class;

  {TBaseTile}
  TBaseTile= class
    FID:integer;
    FMapProvider:TMapProvider;
    Public
      constructor Create(aProvider:TMapProvider);
      destructor Destroy; override;
      Property ID:integer read FID;
  end;

  { TMapProvider }

  TMapProvider = class
    private
      FLayer: integer;
      idServer: Array of Integer;
      FName: String;
      FUrl: Array of string;
      FProjectionType: Array of TProjectionType;
      FNbSvr: Array of integer;
      FGetSvrStr: Array of TGetSvrStr;
      FGetXStr: Array of TGetValStr;
      FGetYStr: Array of TGetValStr;
      FGetZStr: Array of TGetValStr;
      FMinZoom: Array of integer;
      FMaxZoom: Array of integer;
      FTiles:array of TBaseTile;
      FTileHandling: TRTLCriticalSection;
      function GetLayerCount: integer;
      function GetProjectionType: TProjectionType;
      procedure SetLayer(AValue: integer);
    public
      constructor Create(AName: String);
      destructor Destroy; override;
      function AppendTile(aTile: TBaseTile): integer;
      procedure RemoveTile(aTile: TBaseTile);
      procedure AddURL(Url: String; ProjectionType: TProjectionType; NbSvr, aMinZoom, aMaxZoom: integer;
        GetSvrStr: TGetSvrStr; GetXStr: TGetValStr; GetYStr: TGetValStr;
        GetZStr: TGetValStr);
      procedure GetZoomInfos(out AZoomMin, AZoomMax: integer);
      function GetUrlForTile(id: TTileId): String;
      procedure ToXML(ADoc: TXMLDocument; AParentNode: TDOMNode);
      property Name: String read FName;
      property LayerCount: integer read GetLayerCount;
      property Layer: integer read FLayer write SetLayer;
      property ProjectionType: TProjectionType read GetProjectionType;
  end;


function GetSvrLetter(id: integer): String;
function GetSvrBase1(id: integer): String;
function GetStrYahooY(const Tile: TTileId): string;
function GetStrYahooZ(const Tile: TTileId): string;
function GetStrQuadKey(const Tile: TTileId): string;

const
  SVR_LETTER = 'Letter';
  SVR_BASE1  = 'Base1';
  STR_YAHOOY = 'YahooY'; // Idea: Deprecate, as Yahoo Maps are dead
  STR_YAHOOZ = 'YahooZ'; // Idea: Deprecate, as Yahoo Maps are dead
  STR_QUADKEY = 'QuadKey';


implementation

uses
  TypInfo;

function GetSvrLetter(id: integer): String;
begin
  Result := Char(Ord('a') + id);
end;

function GetStrQuadKey(const Tile: TTileId): string;
var
  i, d, m: Longword;
begin
  { Bing Maps Tile System
    http://msdn.microsoft.com/en-us/library/bb259689.aspx }
  Result := '';
  for i := Tile.Z downto 1 do
  begin
    d := 0;
    m := 1 shl (i - 1);
    if (Tile.x and m) <> 0 then
      Inc(d, 1);
    if (Tile.y and m) <> 0 then
      Inc(d, 2);
    Result := Result + IntToStr(d);
  end;
end;

function GetSvrBase1(id: integer): String;
Begin
  Result := IntToStr(id + 1);
end;

function GetStrYahooY(const Tile : TTileId): string;
begin
  Result := IntToStr( -(Tile.Y - (1 shl Tile.Z) div 2) - 1);
end;

function GetStrYahooZ(const Tile : TTileId): string;
Begin
  result := IntToStr(Tile.Z + 1);
end;

{ TBaseTile }

constructor TBaseTile.Create(aProvider: TMapProvider);
begin
  FMapProvider := aProvider;
  if assigned(aProvider) then
    FID:=aProvider.AppendTile(self);
end;

destructor TBaseTile.Destroy;
begin
  If assigned(FMapProvider) then
    FMapProvider.RemoveTile(self);
  FMapProvider:=nil;
  inherited Destroy;
end;


{ TMapProvider }

function TMapProvider.GetLayerCount: integer;
begin
  Result := Length(FUrl);
end;

function TMapProvider.GetProjectionType: TProjectionType;
begin
  Result := FProjectionType[layer];
end;

procedure TMapProvider.SetLayer(AValue: integer);
begin
  if FLayer = AValue then Exit;
  if (aValue < Low(FUrl)) and (aValue > High(FUrl)) then
  Begin
    Raise Exception.Create('bad Layer');
  end;
  FLayer:=AValue;
end;

constructor TMapProvider.Create(AName: String);
begin
  FName := aName;
  InitCriticalSection(FTileHandling);
end;

destructor TMapProvider.Destroy;
var
  i: Integer;
begin
  Finalize(idServer);
  Finalize(FName);
  Finalize(FProjectionType);
  Finalize(FUrl);
  Finalize(FNbSvr);
  Finalize(FGetSvrStr);
  Finalize(FGetXStr);
  Finalize(FGetYStr);
  Finalize(FGetZStr);
  Finalize(FMinZoom);
  Finalize(FMaxZoom);
  EnterCriticalSection(FTileHandling);
  try
    for i := high(FTiles) downto 0 do
      try
        if Assigned(FTiles[i]) then
        begin
          FTiles[i].FMapProvider := nil; // Avoid removing from the FTiles array since this might change the IDs
          FTiles[i].Free;
          FTiles[i] := nil;
        end;
      except
        FTiles[i] := nil;
      end;
  finally
    LeaveCriticalSection(FTileHandling);
  end;
  DoneCriticalSection(FTileHandling);
  inherited;
end;

function TMapProvider.AppendTile(aTile: TBaseTile): integer;
var
  lNewID: Integer;
begin
  EnterCriticalSection(FTileHandling);
  try
    lNewID :=high(FTiles)+1;
    setlength(FTiles,lNewID+1);
    FTiles[lNewID]:=aTile;
    result := lNewID;
  finally
    LeaveCriticalsection(FTileHandling);
  end;
end;

procedure TMapProvider.RemoveTile(aTile: TBaseTile);
var
  lID, lMaxTile, lTileIdx, i: Integer;
begin
  EnterCriticalSection(FTileHandling);
  {
  try
    if (aTile.ID <= high(FTiles)) and (aTile.ID>0) and (FTiles[aTile.ID]=aTile) then
    begin
      lID := aTile.ID;
      lMaxTile :=High(FTiles);
      aTile.FID := -1;
      FTiles[lID] := FTiles[lMaxTile];
      FTiles[lID].FID := lID;
      SetLength(FTiles,lMaxTile);
    end;
  finally
    LeaveCriticalsection(FTileHandling);
  end;
  }
  try
    lTileIdx := -1;
    for i := 0 to High(FTiles) do
    begin
      if FTiles[i] = aTile then
      begin
        lTileIdx := i;
        Break;
      end;
    end;
    if lTileIdx >= 0 then
    begin
      lMaxTile := High(FTiles);
      aTile.FID := -1;
      FTiles[lTileIdx] := FTiles[lMaxTile];
      FTiles[lTileIdx].FID := lTileIdx;
      SetLength(FTiles, lMaxTile);
    end;
    for i := 0 to High(FTiles) do
    begin
      if FTiles[i].FID <> i then
        FTiles[i].FID := i;
    end;
  finally
    LeaveCriticalSection(FTileHandling);
  end;
end;

procedure TMapProvider.AddURL(Url: String; ProjectionType: TProjectionType;
  NbSvr, aMinZoom, aMaxZoom: integer; GetSvrStr: TGetSvrStr;
  GetXStr: TGetValStr; GetYStr: TGetValStr; GetZStr: TGetValStr);
var
  nb: integer;
begin
  nb := Length(FUrl)+1;
  SetLength(IdServer, nb);
  SetLength(FUrl, nb);
  SetLength(FProjectionType, nb);
  SetLength(FNbSvr, nb);
  SetLength(FGetSvrStr, nb);
  SetLength(FGetXStr, nb);
  SetLength(FGetYStr, nb);
  SetLength(FGetZStr, nb);
  SetLength(FMinZoom, nb);
  SetLength(FMaxZoom, nb);
  nb := High(FUrl);
  FUrl[nb] := Url;
  FProjectionType[nb] := ProjectionType;
  FNbSvr[nb] := NbSvr;
  FMinZoom[nb] := aMinZoom;
  FMaxZoom[nb] := aMaxZoom;
  FGetSvrStr[nb] := GetSvrStr;
  FGetXStr[nb] := GetXStr;
  FGetYStr[nb] := GetYStr;
  FGetZStr[nb] := GetZStr;
  FLayer := Low(FUrl);
end;

procedure TMapProvider.GetZoomInfos(out AZoomMin, AZoomMax: integer);
begin
  AZoomMin := FMinZoom[layer];
  AZoomMax := FMaxZoom[layer];
end;

function TMapProvider.GetUrlForTile(id: TTileId): String;
var
  i: integer;
  XVal, yVal, zVal, SvrVal: String;
  idsvr: integer;
begin
  Result := '';
  i := layer;
  if (i > High(idServer)) or (i < Low(idServer)) or (FNbSvr[i] = 0) then
    exit;

  idsvr := idServer[i] mod FNbSvr[i];
  idServer[i] += 1;

  SvrVal := IntToStr(idsvr);
  XVal := IntToStr(id.X);
  YVal := IntToStr(id.Y);
  ZVal := IntToStr(id.Z);
  if Assigned(FGetSvrStr[i]) then
    SvrVal := FGetSvrStr[i](idsvr);
  if Assigned(FGetXStr[i]) then
    XVal := FGetXStr[i](id);
  if Assigned(FGetYStr[i]) then
    YVal := FGetYStr[i](id);
  if Assigned(FGetZStr[i]) then
    ZVal := FGetZStr[i](id);
  Result := StringReplace(FUrl[i], '%serv%', SvrVal, [rfreplaceall]);
  Result := StringReplace(Result, '%x%', XVal, [rfreplaceall]);
  Result := StringReplace(Result, '%y%', YVal, [rfreplaceall]);
  Result := StringReplace(Result, '%z%', ZVal, [rfreplaceall]);
end;

procedure TMapProvider.ToXML(ADoc: TXMLDocument; AParentNode: TDOMNode);
var
  i: Integer;
  node: TDOMElement;
  layerNode: TDOMElement;
  s: String;
begin
  node := ADoc.CreateElement('map_provider');
  node.SetAttribute('name', FName);
  AParentNode.AppendChild(node);

  for i:=0 to LayerCount-1 do begin
    layerNode := ADoc.CreateElement('layer');
    node.AppendChild(layernode);
    layerNode.SetAttribute('url', FUrl[i]);
    layerNode.SetAttribute('minZoom', IntToStr(FMinZoom[i]));
    layerNode.SetAttribute('maxZoom', IntToStr(FMaxZoom[i]));
    layerNode.SetAttribute('serverCount', IntToStr(FNbSvr[i]));

    s := GetEnumName(TypeInfo(TProjectionType), Ord(FProjectionType[i]));
    if s.StartsWith('pt') then
      s := s.Substring(2);
    layerNode.SetAttribute('projection', s);

    if FGetSvrStr[i] = @GetSvrLetter then
      s := SVR_LETTER
    else if FGetSvrStr[i] = @GetSvrBase1 then
      s := SVR_BASE1
    else
      s := '';
    if s <> '' then
      layerNode.SetAttribute('serverProc', s);

    if FGetXStr[i] = @GetStrQuadKey then
      s := STR_QUADKEY
    else
      s := '';
    if s <> '' then
      layerNode.SetAttribute('xProc', s);

    if FGetYStr[i] = @GetStrQuadKey then
      s := STR_QUADKEY
    else if FGetYStr[i] = @GetStrYahooY then
      s := STR_YAHOOY
    else
      s := '';
    if s <> '' then layerNode.SetAttribute('yProc', s);

    if FGetZStr[i] = @GetStrQuadKey then
      s := STR_QUADKEY
    else if FGetZStr[i] = @GetStrYahooZ then
      s := STR_YAHOOZ
    else
      s := '';
    if s <> '' then
      layerNode.SetAttribute('zProc', s);
  end;
end;

end.

