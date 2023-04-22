{ Map Viewer - basic gps object
  (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvGpsObj;

{$mode objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Graphics, fgl, mvtypes, contnrs, syncobjs;

const
  NO_ELE  = -10000000;
  NO_DATE = 0;

type
  TIdArray = Array of integer;

  { TGPSObj }

  TGPSObj = class
  private
    BBoxSet: Boolean;
    FBoundingBox: TRealArea;
    FExtraData: TObject;
    FName: String;
    FIdOwner: integer;
    function GetBoundingBox: TRealArea;
    procedure SetBoundingBox(AValue: TRealArea);
    procedure SetExtraData(AValue: TObject);
  public
    destructor Destroy; override;
    procedure Assign(AObj: TGPSObj); virtual;
    procedure GetArea(out Area: TRealArea); virtual; abstract;
    property Name: String read FName write FName;
    property ExtraData: TObject read FExtraData write SetExtraData;
    property IdOwner: Integer read FIdOwner;
    property BoundingBox: TRealArea read GetBoundingBox write SetBoundingBox;
  end;

  TGPSObjarray = Array of TGPSObj;

  { TGPSPoint }

  TGPSPoint = class(TGPSObj)
  private
    FRealPt: TRealPoint;
    FElevation: Double;
    FDateTime: TDateTime;
    function GetLat: Double;
    function GetLon: Double;
  public
    constructor Create(ALon,ALat: double; AElevation: double = NO_ELE;
      ADateTime: TDateTime = NO_DATE);
    class function CreateFrom(aPt: TRealPoint; AElevation: Double = NO_ELE;
      ADateTime: TDateTime = NO_DATE): TGPSPoint;

    procedure Assign(AObj: TGPSObj); override;
    procedure GetArea(out Area: TRealArea);override;
    function HasElevation: boolean;
    function HasDateTime: Boolean;
    function DistanceInKmFrom(OtherPt: TGPSPoint; UseElevation: boolean=true): double;
    procedure MoveTo(ALon, ALat: Double; AElevation: double = NO_ELE;
      ADateTime: TDateTime = NO_DATE);

    property Lon: Double read GetLon;
    property Lat: Double read GetLat;
    property Elevation: double read FElevation;
    property DateTime: TDateTime read FDateTime;
    property RealPoint: TRealPoint read FRealPt;
  end;

  TGPSPointList = specialize TFPGObjectList<TGPSPoint>;

  { TGPSPointOfInterest }

  TGPSPointOfInterest = class(TGPSPoint)
  private
    FImageIndex: Integer;
  public
    constructor Create(ALon, ALat: Double; AElevation: Double = NO_ELE;
      ADateTime: TDateTime = NO_DATE);
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
  end;
  { TGPSTrack }

  TGPSTrack = class(TGPSObj)
  private
    FDateTime: TDateTime;
    FPoints: TGPSPointList;
    FLineWidth: Double;  // Line width in mm
    FLineColor: TColor;
    FVisible: Boolean;
    function GetDateTime: TDateTime;
  public
    constructor Create;
    destructor Destroy; override;

    procedure GetArea(out Area: TRealArea); override;
    function TrackLengthInKm(UseEle: Boolean=true): double;

    property Points: TGPSPointList read FPoints;
    property DateTime: TDateTime read GetDateTime write FDateTime;
    property LineColor: TColor read FLineColor write FLineColor;
    property Visible: Boolean read FVisible write FVisible;
    property LineWidth: Double read FLineWidth write FLineWidth;
  end;

  TGPSObjList_ = specialize TFPGObjectList<TGPSObj>;

  { TGPSObjList }

  TGPSObjList = class(TGPSObjList_)
  private
    FRef: TObject;
  public
    destructor Destroy; override;
  end;

  { TGPSObjectList }

  TModifiedEvent = procedure (Sender: TObject; objs: TGPSObjList;
    Adding: boolean) of object;

  TGPSObjectList = class(TGPSObj)
  private
    Crit: TCriticalSection;
    FPending: TObjectList;
    FRefCount: integer;
    FOnModified: TModifiedEvent;
    FUpdating: integer;
    FItems: TGPSObjList;
    function GetCount: integer;
    function GetItem(AIndex: Integer): TGpsObj;
  protected
    procedure _Delete(Idx: Integer; var DelLst: TGPSObjList);
    procedure FreePending;
    procedure DecRef;
    procedure CallModified(lst: TGPSObjList; Adding: boolean);
//    property Items: TGPSObjList read FItems;
    procedure IdsToObj(const Ids: TIdArray; out objs: TGPSObjArray; AIdOwner: integer);
  public
    constructor Create;
    destructor Destroy; override;
    Procedure Clear(OwnedBy: integer);
    procedure ClearExcept(OwnedBy: integer; const ExceptLst: TIdArray;
      out Notfound: TIdArray);
    procedure GetArea(out Area: TRealArea); override;
    function GetObjectsInArea(const Area: TRealArea): TGPSObjList;
    function GetIdsArea(const Ids: TIdArray; AIdOwner: integer): TRealArea;

    function Add(aItem: TGpsObj; AIdOwner: integer): integer;
    procedure DeleteById(const Ids: Array of integer);
    function FindTrackByID(const id: Integer): TGpsTrack;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure Lock;
    procedure UnLock;

    property Count: integer read GetCount;
    property Items[AIndex: Integer]: TGpsObj read GetItem; default;
    property OnModified: TModifiedEvent read FOnModified write FOnModified;
  end;

//function HasIntersectArea(const Area1: TRealArea; const Area2: TRealArea): boolean;
//function IntersectArea(const Area1: TRealArea; const Area2: TRealArea): TRealArea;
function PtInsideArea(const aPoint: TRealPoint; const Area: TRealArea): boolean;
function AreaInsideArea(const AreaIn: TRealArea; const AreaOut: TRealArea): boolean;
//procedure ExtendArea(var AreaToExtend: TRealArea; const Area: TRealArea);
function GetAreaOf(objs: TGPSObjList): TRealArea;


implementation

uses
  mvExtraData;
                                     (*
function hasIntersectArea(const Area1: TRealArea; const Area2: TRealArea): boolean;
begin
  Result := Area1.Intersects(Area2);
end;

function IntersectArea(const Area1: TRealArea; const Area2: TRealArea): TRealArea;
begin
  Result := Area1.Intersection(Area2);
end;
                                       *)
function PtInsideArea(const aPoint: TRealPoint; const Area: TRealArea): boolean;
begin
  Result := (Area.TopLeft.Lon <= aPoint.Lon) and
            (Area.BottomRight.Lon >= aPoint.Lon) and
            (Area.TopLeft.Lat >= aPoint.Lat) and
            (Area.BottomRight.Lat <= aPoint.Lat);
end;

function AreaInsideArea(const AreaIn: TRealArea; const AreaOut: TRealArea): boolean;
begin
  Result := (AreaIn.TopLeft.Lon >= AreaOut.TopLeft.Lon) and
            (AreaIn.BottomRight.Lon <= AreaOut.BottomRight.Lon) and
            (AreaOut.TopLeft.Lat >= AreaIn.TopLeft.Lat) and
            (AreaOut.BottomRight.Lat <= AreaIn.BottomRight.Lat);
end;
  (*
procedure ExtendArea(var AreaToExtend: TRealArea; const Area: TRealArea);
begin
  AreaToExtend := AreaToExtend.Union(Area);
end;
    *)
function GetAreaOf(objs: TGPSObjList): TRealArea;
var
  i: integer;
begin
  Result.Create(0, 0, 0, 0);
  if Objs.Count>0 then
  begin
    Result := Objs[0].BoundingBox;
    for i:=1 to pred(Objs.Count) do
      Result := Result.Union(Objs[i].BoundingBox);
  end;
end;

{ TGPSObjList }

destructor TGPSObjList.Destroy;
begin
  if Assigned(FRef) then
    TGPSObjectList(FRef).DecRef;
  inherited Destroy;
end;

{ TGPSObj }

procedure TGPSObj.Assign(AObj: TGPSObj);
begin
  FName := AObj.Name;
end;

procedure TGPSObj.SetExtraData(AValue: TObject);
begin
  if FExtraData=AValue then Exit;
  if Assigned(FExtraData) then
    FreeAndNil(FExtraData);
  FExtraData := AValue;
end;

function TGPSObj.GetBoundingBox: TRealArea;
begin
  if not(BBoxSet) then
  begin
    GetArea(FBoundingBox);
    BBoxSet := true;
  end;
  Result := FBoundingBox;
end;

procedure TGPSObj.SetBoundingBox(AValue: TRealArea);
begin
  FBoundingBox := AValue;
  BBoxSet := true;
end;

destructor TGPSObj.Destroy;
begin
  FreeAndNil(FExtraData);
  inherited Destroy;
end;

{ TGPSObjectList }

function TGPSObjectList.GetCount: integer;
begin
  Result := FItems.Count
end;

function TGPSObjectList.GetItem(AIndex: Integer): TGpsObj;
begin
  Result := FItems[AIndex];
end;

procedure TGPSObjectList._Delete(Idx: Integer; var DelLst: TGPSObjList);   // wp: was "out"
var
  Item: TGpsObj;
begin
  Lock;
  try
    if not(Assigned(DelLst)) then
    begin
      DelLst := TGpsObjList.Create(False);
      DelLst.FRef := Self;
      inc(FRefCount);
    end;
    if not Assigned(FPending) then
      FPending := TObjectList.Create(true);
    Item := FItems.Extract(FItems[Idx]);
    FPending.Add(Item);
  finally
    UnLock;
  end;
  DelLst.Add(Item);
end;

procedure TGPSObjectList.FreePending;
begin
  if Assigned(FPending) then
  begin
    Lock;
    try
      FreeAndNil(FPending);
    finally
      UnLock;
    end;
  end;
end;

procedure TGPSObjectList.DecRef;
begin
  FRefCount-=1;
  if FRefCount=0 then
    FreePending;
end;

procedure TGPSObjectList.Lock;
begin
  if Assigned(Crit) then
    Crit.Enter;
end;

procedure TGPSObjectList.UnLock;
begin
  if Assigned(Crit) then
    Crit.Leave;
end;

procedure TGPSObjectList.CallModified(lst: TGPSObjList; Adding: boolean);
begin
  if (FUpdating=0) and Assigned(FOnModified) then
    FOnModified(self, lst, Adding)
  else
    lst.Free;
end;

procedure TGPSObjectList.IdsToObj(const Ids: TIdArray; out objs: TGPSObjArray;
  AIdOwner: integer);

  function ToSelect(aId: integer): boolean;
  var
    i: integer;
  begin
    result := false;
    for i:=low(Ids) to high(Ids) do
      if Ids[i]=aId then
      begin
        result := true;
        break;
      end;
  end;

var
  i,nb : integer;
begin
  objs := nil;
  SetLength(objs, Length(Ids));
  nb := 0;
  Lock;
  try
    for i:=0 to pred(FItems.Count) do
    begin
      if (AIdOwner = 0) or (AIdOwner = FItems[i].FIdOwner) then
        if Assigned(FItems[i].ExtraData) and FItems[i].ExtraData.InheritsFrom(TDrawingExtraData) then
        begin
          if ToSelect(TDrawingExtraData(FItems[i].ExtraData).Id) then
          begin
            objs[nb] := FItems[i];
            nb+=1;
          end;
        end;
    end;
  finally
    Unlock;
  end;
  SetLength(objs, nb);
end;

procedure TGPSObjectList.GetArea(out Area: TRealArea);
var
  i: integer;
begin
  Area.Create(0, 0, 0, 0);
  Lock;
  try
    if Count > 0 then
    begin
      Area := Items[0].BoundingBox;
      for i:=1 to pred(Count) do
        Area := Area.Union(Items[i].BoundingBox);
    end;
  finally
    Unlock;
  end;
end;

function TGPSObjectList.GetObjectsInArea(const Area: TRealArea): TGPSObjList;
var
  i: integer;
  ItemArea: TRealArea;
begin
  Result := TGPSObjList.Create(false);
  Lock;
  try
    Inc(FRefCount);
    for i:=0 to pred(Count) do
    begin
      ItemArea := Items[i].BoundingBox;
      if Area.Intersects(ItemArea) then
        Result.Add(Items[i]);
    end;
    if Result.Count > 0 then
      Result.FRef := Self
    else
      Dec(FRefCount);
  finally
    Unlock;
  end;
end;

constructor TGPSObjectList.Create;
begin
  Crit := TCriticalSection.Create;
  FItems := TGPSObjList.Create(true);
end;

destructor TGPSObjectList.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FItems);
  FreeAndNil(FPending);
  FreeAndNil(Crit);
end;

procedure TGPSObjectList.Clear(OwnedBy: integer);
var
  i: integer;
  DelObj: TGPSObjList;
begin
  DelObj := nil;
  Lock;
  try
    for i:=pred(FItems.Count) downto 0 do
      if (OwnedBy = 0) or (FItems[i].FIdOwner = OwnedBy) then
        _Delete(i,DelObj);
  finally
    Unlock;
  end;
  if Assigned(DelObj) then
    CallModified(DelObj, false);
end;

procedure TGPSObjectList.ClearExcept(OwnedBy: integer;
  const ExceptLst: TIdArray; out Notfound: TIdArray);

var
  Found: TIdArray;

  function ToDel(aIt: TGPsObj): boolean;
  var
    i,Id: integer;
  begin
    if (aIt.ExtraData=nil) or not(aIt.ExtraData.InheritsFrom(TDrawingExtraData)) then
      Result := true
    else
    Begin
      Result := true;
      Id := TDrawingExtraData(aIt.ExtraData).Id;
      for i := Low(ExceptLst) to High(ExceptLst) do
       if Id = ExceptLst[i] then
       begin
         Result := false;
         SetLength(Found, Length(Found)+1);
         Found[high(Found)] := Id;
         exit;
       end;
    end;
  end;

var
  i,j: integer;
  IsFound: boolean;
  DelLst: TGPSObjList;
begin
  DelLst := nil;
  SetLength(NotFound{%H-}, 0);
  SetLength(Found, 0);
  Lock;
  try
    for i := pred(FItems.Count) downto 0 do
    begin
      if (FItems[i].FIdOwner = OwnedBy) or (OwnedBy = 0) then
      Begin
         if ToDel(FItems[i]) then
           _Delete(i,DelLst);
      end;
    end;
  finally
    Unlock;
  end;
  for i:=low(ExceptLst) to high(ExceptLst) do
  begin
    IsFound := false;
    for j:=Low(Found) to High(Found) do
      if Found[j] = ExceptLst[i] then
      begin
        IsFound := true;
        break;
      end;
    if not IsFound then
    begin
      SetLength(NotFound, Length(NotFound)+1);
      NotFound[high(NotFound)] := ExceptLst[i];
    end;
  end;
  if Assigned(DelLst) then
    CallModified(DelLst, false);
end;

function TGPSObjectList.GetIdsArea(const Ids: TIdArray; AIdOwner: integer): TRealArea;
var
  Objs: TGPSObjarray;
  i: integer;
begin
  Result.Create(0, 0, 0, 0);
  Lock;
  try
    IdsToObj(Ids, Objs, AIdOwner);
    if Length(Objs) > 0 then
    begin
      Result := Objs[0].BoundingBox;
      for i:=succ(Low(Objs)) to High(Objs) do
        Result := Result.Union(Objs[i].BoundingBox);
    end;
  finally
    Unlock;
  end;
end;

function TGPSObjectList.Add(aItem: TGpsObj; AIdOwner: integer): integer;
var
  mList: TGPSObjList;
begin
  aItem.FIdOwner := AIdOwner;
  Lock;
  try
    Result := FItems.Add(aItem);
    mList := TGPSObjList.Create(false);
    mList.Add(aItem);
    inc(FRefCount);
    mList.FRef := Self;
  finally
    Unlock;
  end;
  CallModified(mList, true);
end;

procedure TGPSObjectList.DeleteById(const Ids: array of integer);

  function ToDelete(const AId: integer): Boolean;
  var
    i: integer;
  begin
    result := false;
    For i:=Low(Ids) to High(Ids) do
      if Ids[i] = AId then
      begin
        result := true;
        exit;
      end;
  end;

var
  Extr: TDrawingExtraData;
  i: integer;
  DelLst: TGPSObjList;
begin
  DelLst := nil;
  Lock;
  try
    for i:=pred(Count) downto 0 do
    begin
      if Assigned(Items[i].ExtraData) then
      begin
        if Items[i].ExtraData.InheritsFrom(TDrawingExtraData) then
        begin
          Extr := TDrawingExtraData(Items[i].ExtraData);
          if ToDelete(Extr.Id) then
            _Delete(i, DelLst);
          // !!! wp: DelLst is a local var and was created by _Delete but is
          //     not destroyed anywhere here !!!
        end;
      end;
    end;
  finally
    Unlock;
  end;
  if Assigned(DelLst) then
// wp: is this missing here:    DelLst.Free;
end;

procedure TGPSObjectList.BeginUpdate;
begin
  inc(FUpdating);
end;

procedure TGPSObjectList.EndUpdate;
begin
  if FUpdating > 0 then
  begin
    Dec(FUpdating);
    if FUpdating = 0 then
      CallModified(nil, true);
  end;
end;

function TGPSObjectList.FindTrackByID(const ID: Integer): TGpsTrack;
var
  i: Integer;
begin
  for i:=0 to pred(FItems.Count) do
    if (ID = FItems[i].IdOwner) and (FItems[i] is TGpsTrack) then
    begin
      Result := TGpsTrack(FItems[i]);
      exit;
    end;
  Result := nil;
end;


{ TGPSTrack }

function TGPSTrack.GetDateTime: TDateTime;
begin
  if FDateTime = 0 then
  Begin
    if FPoints.Count > 0 then
      FDateTime := FPoints[0].DateTime;
  end;
  Result := FDateTime;
end;

constructor TGPSTrack.Create;
begin
  inherited;
  FPoints := TGPSPointList.Create(true);
  FVisible := true;
  FLineColor := clDefault;  // --> use MapView.DefaultTrackColor
  FLineWidth := -1;         // --> use MapView.DefaultTrackWidth
end;

destructor TGPSTrack.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FPoints);
end;

procedure TGPSTrack.GetArea(out Area: TRealArea);
var
  i: integer;
begin
  Area.Create(0, 0, 0, 0);
  if FPoints.Count > 0 then
  begin
    Area := FPoints[0].BoundingBox;
    for i:=1 to pred(FPoints.Count) do
      Area := Area.Union(FPoints[i].BoundingBox);
  end;
end;

function TGPSTrack.TrackLengthInKm(UseEle: Boolean): double;
var
  i: integer;
begin
  Result := 0;
  for i:=1 to pred(FPoints.Count) do
    result += FPoints[i].DistanceInKmFrom(FPoints[pred(i)], UseEle);
end;


{ TGPSPoint }

procedure TGPSPoint.Assign(AObj: TGpsObj);
begin
  if (AObj is TGPSPoint) then
  begin
    inherited Assign(AObj);
    FRealPt := TGPSPoint(AObj).RealPoint;
    FElevation := TGPSPoint(AObj).Elevation;
    FDateTime := TGPSPoint(AObj).DateTime;
  end;
end;

function TGPSPoint.GetLat: Double;
begin
  result := FRealPt.Lat;
end;

function TGPSPoint.GetLon: Double;
begin
  result := FRealPt.Lon;
end;

procedure TGPSPoint.GetArea(out Area: TRealArea);
begin
  Area.Create(FRealPt, FRealPt);
end;

function TGPSPoint.HasElevation: boolean;
begin
  Result := FElevation <> NO_ELE;
end;

function TGPSPoint.HasDateTime: Boolean;
begin
  Result := FDateTime <> NO_DATE;
end;

function TGPSPoint.DistanceInKmFrom(OtherPt: TGPSPoint;
  UseElevation: boolean = true): double;
var
  a: double;
  lat1, lat2, lon1, lon2, t1, t2, t3, t4, t5, rad_dist: double;
  DiffEle: Double;
begin
  a := PI / 180;
  lat1 := lat * a;
  lat2 := OtherPt.lat * a;
  lon1 := lon * a;
  lon2 := OtherPt.lon * a;

  t1 := sin(lat1) * sin(lat2);
  t2 := cos(lat1) * cos(lat2);
  t3 := cos(lon1 - lon2);
  t4 := t2 * t3;
  t5 := t1 + t4;
  rad_dist := arctan(-t5/sqrt(-t5 * t5 +1)) + 2 * arctan(1);
  result := (rad_dist * 3437.74677 * 1.1508) * 1.6093470878864446;
  if UseElevation and (FElevation <> OtherPt.FElevation) then
    if (HasElevation) and (OtherPt.HasElevation) then
    begin
      //FElevation is assumed in meters
      DiffEle := (FElevation - OtherPt.Elevation) / 1000;
      Result := sqrt(DiffEle*DiffEle + result*result);
    end;
end;

procedure TGPSPoint.MoveTo(ALon, ALat: Double; AElevation: double = NO_ELE;
  ADateTime: TDateTime = NO_DATE);
begin
  FRealPt.Lon := ALon;
  FRealPt.Lat := ALat;
  FElevation := AElevation;
  FDateTime := ADateTime;
end;


constructor TGPSPoint.Create(ALon, ALat: double; AElevation: double;
  ADateTime: TDateTime);
begin
  MoveTo(ALon, ALat, AElevation, ADateTime);
end;

class function TGPSPoint.CreateFrom(aPt: TRealPoint; AElevation: Double = NO_ELE;
  ADateTime: TDateTime = NO_DATE): TGPSPoint;
begin
  Result := Create(aPt.Lon, aPt.Lat, AElevation, ADateTime);
end;


{ TGPSPointOfInterest }

constructor TGPSPointOfInterest.Create(ALon, ALat: Double;
  AElevation: Double = NO_ELE; ADateTime: TDateTime = NO_DATE);
begin
  inherited;
  FImageIndex := -1;
end;


end.

