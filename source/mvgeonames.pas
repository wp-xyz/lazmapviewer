{ Map Viewer Geolocation Engine for geonames.org
  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvGeoNames;

interface

uses
  SysUtils, Classes, StrUtils,
  mvTypes, mvDownloadEngine;

type
  TNameFoundEvent = procedure (const AName: string; const ADescr: String;
    const ALoc: TRealPoint) of object;

  TStringArray = array of string;

  TResRec = record
    Name: String;
    Descr: String;
    Loc: TRealPoint;
  end;


  { TMVGeoNames }

  TMVGeoNames = class(TComponent)
  private
    FLocationName: string;
    FInResTable: Boolean;
    FInDataRows: Boolean;
    FNamePending: Boolean;
    FLongitudePending: Boolean;
    FLatitudePending: Boolean;
    FCol: Integer;
    FCountry: String;
    FSmall: Boolean;
    FFirstLocation: TResRec;
    FFoundLocation: TResRec;
    FOnNameFound: TNameFoundEvent;
    procedure FoundTagHandler(NoCaseTag, {%H-}ActualTag: string);
    procedure FoundTextHandler(AText: String);
    function Parse(AStr: PChar): TRealPoint;
//    function RemoveTag(const str: String): TStringArray;
  public
    function Search(ALocationName: String;
      ADownloadEngine: TMvCustomDownloadEngine): TRealPoint;
  published
    property LocationName: string read FLocationName;
    property OnNameFound: TNameFoundEvent read FOnNameFound write FOnNameFound;
  end;


implementation

uses
  FastHtmlParser;

const
  SEARCH_URL = 'http://geonames.org/search.html?q=%s'; //&country=%s';


{ TMVGeoNames }

procedure TMvGeoNames.FoundTagHandler(NoCaseTag, ActualTag: String);
begin
  if not FInResTable and (NoCaseTag = '<TABLE CLASS="RESTABLE">') then begin
    FInResTable := true;
    FInDataRows := false;
    FNamePending := false;
    FLatitudePending := false;
    FLongitudePending := false;
    FSmall := false;
  end else
  if FInResTable and (NoCaseTag = '</TABLE>') then
    FInResTable := false;

  if FInResTable then begin
    if NoCaseTag = '</TH>' then
      FInDataRows := true;

    if FInDataRows then begin
      if NoCaseTag = '<TR>' then begin
        FCol := 0;
        with FFoundLocation do begin
          Name := ''; Descr := ''; Loc.Lon := 0; Loc.Lat := 0;
        end;
      end;

      if NoCaseTag = '<TD>' then
        inc(FCol);

      if FCol = 2 then begin
        if not FNamePending and (pos('<A HREF=', NoCaseTag) = 1) then
          FNamePending := true
        else if FNamePending and (NoCaseTag = '</A>') then
          FNamePending := false;

        if not FLatitudePending and (NoCaseTag = '<SPAN CLASS="LATITUDE">') then
          FLatitudePending := true
        else if FLatitudePending and (NoCaseTag = '</SPAN>') then
          FLatitudePending := false;

        if not FLongitudePending and (NoCaseTag = '<SPAN CLASS="LONGITUDE">') then
          FLongitudePending := true
        else if FLongitudePending and (NoCasetag = '</SPAN>') then
          FLongitudePending := false;
      end;

      if FCol = 3 then
        if not FSmall and (NoCaseTag = '<SMALL>') then
          FSmall := true
        else if FSmall and (NoCaseTag = '</SMALL>') then
          FSmall := false;

      if NoCaseTag = '</TR>' then begin
        if (FFirstLocation.Name = '') then
          FFirstLocation := FFoundLocation;
        if Assigned(FOnNameFound) and (FFoundLocation.Name <> '') then
          with FFoundLocation do
            FOnNameFound(Name, Descr, Loc);
      end;
    end;
  end;
end;

procedure TMvGeoNames.FoundTextHandler(AText: String);
var
  {%H-}code: Integer;
begin
  if not FInDataRows or (AText = #10) then
    exit;

  if FNamePending then
    FFoundLocation.Name := AText
  else if FLatitudePending then
    val(AText, FFoundLocation.Loc.Lat, code)
  else if FLongitudePending then
    val(AText, FFoundLocation.Loc.Lon, code)
  else if (FCol = 3) and not FSmall then
    FCountry := FCountry + AText
  else if FCol = 4 then begin
    if FFoundLocation.Descr = '' then
      FFoundLocation.Descr := AText
    else
      FFoundLocation.Descr := FFoundLocation.Descr + ', ' + aText;
    if FCountry <> '' then
      FFoundLocation.Name := FFoundLocation.Name + ' (' + FCountry + ')';
    FCountry := '';
  end;
end;

function TMVGeonames.Parse(AStr: PChar): TRealPoint;
var
  parser: THtmlParser;
begin
  FFirstLocation.Name := '';
  parser := THtmlParser.Create(AStr);
  try
    parser.OnFoundTag := @FoundTagHandler;
    parser.OnFoundText := @FoundTextHandler;
    parser.Exec;
    Result := FFirstLocation.Loc;
  finally
    parser.Free;
  end;
end;

function TMVGeoNames.Search(ALocationName: String;
  ADownloadEngine: TMvCustomDownloadEngine): TRealPoint;
var
  s: string = '';

  function gs(id: string; Start: integer): string;
  var
    i: Integer;
    ln: Integer;
  begin
    Result := '';
    ln := Length(s);
    i := PosEx(id, s,start) + Length(id);
    while (s[i] <> '<') and (i < ln) do
    begin
      if s[i] = '.' then
        Result := Result + FormatSettings.DecimalSeparator
      else
        Result := Result + s[i];
      Inc(i);
    end;
  end;

var
  ms: TMemoryStream;
  url: String;
begin
  FLocationName := ALocationName;
  ms := TMemoryStream.Create;
  try
    url := Format(SEARCH_URL, [FLocationName]);
    ADownloadEngine.DownloadFile(url, ms);
    ms.Position := 0;
    SetLength(s, ms.Size);
    ms.Read(s[1], ms.Size);
  finally
    ms.Free;
  end;

  Result := Parse(PChar(s));
end;

end.
