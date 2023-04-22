{
  Map Viewer Download Engine for Synapse library
  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}
unit mvDLESynapse;

{$mode objfpc}{$H+}

interface

uses
  mvDownloadEngine, SysUtils, Classes, ssl_openssl, httpsend;

type

  { TMvDESynapse }

  TMvDESynapse = class(TMvCustomDownloadEngine)
  private
    FProxyHost: string;
    FProxyPassword: string;
    FProxyPort: Integer;
    FProxyUsername: string;
    FUseProxy: Boolean;
  protected
    procedure InternalDownloadFile(const Url: string; str: TStream); override;

  published
    property UseProxy: Boolean read FUseProxy write FUseProxy default false;
    property ProxyHost: string read FProxyHost write FProxyHost;
    property ProxyPort: Integer read FProxyPort write FProxyPort default 0;
    property ProxyUsername: string read FProxyUsername write FProxyUsername;
    property ProxyPassword: string read FProxyPassword write FProxyPassword;
  end;

procedure Register;

implementation

uses
  mvTypes;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMvDESynapse]);
end;


{ TMvDESynapse }

procedure TMvDESynapse.InternalDownloadFile(const Url: string; str: TStream);
var
  FHttp: THTTPSend;
  realURL: String;
  i: Integer;
begin
  FHttp := THTTPSend.Create;
  try
    if FUseProxy then
    begin
      FHTTP.ProxyHost := FProxyHost;
      FHTTP.ProxyPort := IntToStr(FProxyPort);
      FHTTP.ProxyUser := FProxyUsername;
      FHTTP.ProxyPass := FProxyPassword;
    end;

    if FHTTP.HTTPMethod('GET', Url) then
    begin
      // If its a 301 or 302 we need to do more processing
      if (FHTTP.ResultCode = 301) or (FHTTP.ResultCode = 302) then
      begin
        // Check the headers for the Location header
        for i := 0 to FHTTP.Headers.Count -1 do
        begin
          // Extract the URL
          if Copy(FHTTP.Headers[i], 1, 8) = 'Location' then
            realURL := copy(FHTTP.Headers[i], 11, Length(FHTTP.Headers[i]) - 10); //11);
        end;
        // If we have a URL, run it through the same function
        if Length(realURL) > 1 then
          DownloadFile(realURL, str);
      end
      else
      begin
        str.Seek(0, soFromBeginning);
        str.CopyFrom(FHTTP.Document, 0);
        str.Position := 0;
      end;
    end;
  finally
    FHttp.Free;
  end;
end;

end.
