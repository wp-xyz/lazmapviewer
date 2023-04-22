{
  basic jobs for multi-threading
  (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvJobs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, mvJobQueue;


type

  { TSimpleJob: job with only one task }

  TSimpleJob = class(TJob)
  private
    FRunning, FEnded: boolean;
  protected
    function pGetTask: integer; override;
    procedure pTaskStarted(aTask: integer); override;
    procedure pTaskEnded(aTask: integer; {%H-}aExcept: Exception); override;
  public
    function Running: boolean; override;
  end;

  TJobProc = procedure (Data: TObject; Job: TJob) of object;

  { TEventJob: job with only one task (callback an event) }

  TEventJob = class(TSimpleJob)
  private
    FData: TObject;
    FTask: TJobProc;
    FOwnData: Boolean;
  public
    constructor Create(aEvent: TJobProc; Data: TObject; OwnData: Boolean;
      JobName: String = ''); virtual;
    destructor Destroy; override;
    procedure ExecuteTask(aTask: integer; {%H-}FromWaiting: boolean); override;
  end;


implementation

{ TEventJob }

constructor TEventJob.Create(aEvent: TJobProc; Data: TObject;
  OwnData: Boolean; JobName: String = '');
begin
  Name := JobName;
  FTask := aEvent;
  if Assigned(Data) or OwnData then
  begin
    FData := Data;
    FOwnData := OwnData;
  end
  else
  begin
    FOwnData := false;
    FData := self;
  end;
end;

destructor TEventJob.Destroy;
begin
  if FOwnData then
    if FData <> self then
      FData.Free;
  inherited Destroy;
end;

procedure TEventJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
begin
  if Assigned(FTask) then
    FTask(FData, self);
end;


{ TSimpleJob }

function TSimpleJob.pGetTask: integer;
begin
  if FRunning or Cancelled then
  begin
    if not FRunning then
      Result := ALL_TASK_COMPLETED
    else
      Result := NO_MORE_TASK
  end
  else
  begin
    if FEnded then
      Result := ALL_TASK_COMPLETED
    else
      Result := 1;
  end;
end;

procedure TSimpleJob.pTaskStarted(aTask: integer);
begin
  FEnded := false;
  FRunning := True;
end;

procedure TSimpleJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  FEnded := True;
  FRunning := False;
end;

function TSimpleJob.Running: boolean;
begin
  Result := FRunning;
end;


end.

