{ Map Viewer Download Engine Free Pascal HTTP Client
  Copyright (C) 2011 Maciej Kaczkowski / keit.co

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL

  Taken from:
  https://forum.lazarus.freepascal.org/index.php/topic,12674.msg160255.html#msg160255
}

unit mvDLEFpc;

{$mode objfpc}{$H+}

{.$DEFINE LOG_URL}

interface

uses
  SysUtils, Classes,
  mvDownloadEngine;

type

  { TMVDEFPC }

  TMVDEFPC = class(TMvCustomDownloadEngine)
  {$IF FPC_FullVersion >= 30101}
  private
    FUseProxy: Boolean;
    FProxyHost: string;
    FProxyPort: Word;
    FProxyUserName: String;
    FProxyPassWord: String;
  {$IFEND}
  protected
    procedure InternalDownloadFile(const Url: string; AStream: TStream); override;
  public
  {$IF FPC_FullVersion >= 30101}
  published
    property UseProxy: Boolean read FUseProxy write FUseProxy default false;
    property ProxyHost: String read FProxyHost write FProxyHost;
    property ProxyPort: Word read FProxyPort write FProxyPort;
    property ProxyUsername: String read FProxyUserName write FProxyUserName;
    property ProxyPassword: String read FProxyPassword write FProxyPassword;
  {$IFEND}
  end;


implementation

uses
  {$IFDEF LOG_URL}
  lazlogger,
  {$ENDIF}
  {$IF FPC_FullVersion >= 30200}
  opensslsockets,
  {$IFEND}
  fphttpclient, openssl;

{ TMVDEFPC }

procedure TMVDEFPC.InternalDownloadFile(const Url: string; AStream: TStream);
var
  http: TFpHttpClient;
begin
  {$IFDEF LOG_URL}
  DebugLn(Url);
  {$ENDIF}
  InitSSLInterface;
  http := TFpHttpClient.Create(nil);
  try
    http.ConnectTimeOut := 10000;
   {$IF FPC_FullVersion >= 30000}
    http.AllowRedirect := true;
   {$IFEND}
    http.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
   {$IF FPC_FullVersion >= 30101}
    if UseProxy then begin
      http.Proxy.Host := FProxyHost;
      http.Proxy.Port := FProxyPort;
      http.Proxy.UserName := FProxyUserName;
      http.Proxy.Password := FProxyPassword;
    end;
   {$ENDIF}
    try
      http.Get(Url, AStream);
    except
      // Eat the exception because we don't know on which server the map is found.
    end;
    AStream.Position := 0;
  finally
    http.Free;
  end;
end;

end.
