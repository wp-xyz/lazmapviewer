{ Map Viewer Download Engine Free Pascal HTTP Client

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDLEWin;

{$mode objfpc}{$H+}

interface

{$IFDEF MSWindows}

uses
  Classes, SysUtils,
  mvDownloadEngine;

type
  TMVDEWin = class(TMvCustomDownloadEngine)
  protected
    procedure InternalDownloadFile(const Url: string; AStream: TStream); override;
  end;

{$ENDIF}

implementation

{$IFDEF MSWindows}

uses
  windows, wininet;

procedure TMVDEWin.InternalDownloadFile(const Url: string; AStream: TStream);
const
  KB = 1024;
var
  netHandle: HInternet;
  urlHandle: HInternet;
  buffer: array[0..4*KB-1] of Char;
  bytesRead: dWord = 0;
  errCode: Integer = 0;
  header: String;
begin
  NetHandle := InternetOpen('Mozilla/5.0(compatible; WinInet)', INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  // NetHandle valid?
  if netHandle = nil then
    exit;

  try
    header := '';
    urlHandle := InternetOpenUrl(netHandle, PChar(URL), PChar(header), Length(header), INTERNET_FLAG_RELOAD, 0);

    // UrlHandle valid?
    if urlHandle = nil then
      exit;

    try
      repeat
        InternetReadFile(urlHandle, @buffer, SizeOf(buffer), bytesRead);
        if bytesRead > 0 then
          AStream.Write(buffer, bytesRead);
      until bytesRead = 0;
      AStream.Position := 0;
    finally
      InternetCloseHandle(urlHandle);
    end
  finally
    InternetCloseHandle(netHandle);
  end;
end;

{$ENDIF}

end.

