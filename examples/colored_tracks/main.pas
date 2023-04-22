unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvGpsObj, mvGpx;

type

  { TMainForm }

  TMainForm = class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    Label1: TLabel;
    ZoomLabel: TLabel;
    Label2: TLabel;
    MapView: TMapView;
    Panel1: TPanel;
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckBox2Change(Sender: TObject);
    procedure CheckBox3Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
  private
    FTrack1: TGpsTrack;
    FTrack2: TGpsTrack;
    FTrack3: TGpsTrack;
    function LoadGPXFile(AFileName: String; AColor: TColor; AWidth: Double): TGPSTrack;
    procedure UpdateInfo;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

uses
  mvTypes, mvEngine;

const
  FILENAME1 = '../../Yosemite Tour 1 - Trans Yosemite.gpx';
  FILENAME2 = '../../Yosemite Tour 2 - Bear Valley.gpx';
  FILENAME3 = '../../Yosemite Tour 3 - Triangle Loop.gpx';

{ TMainForm }

procedure TMainForm.FormActivate(Sender: TObject);
var
  crs: TCursor;
  totalArea: TRealArea;
  trackArea: TRealArea;
begin
  crs := Screen.Cursor;
  Screen.Cursor := crHourglass;
  try
    // Threaded painting interferes with track painting over several tiles
    MapView.UseThreads := true; //false;
    MapView.MapProvider := Combobox1.Text;
    MapView.Active := true;

    // Load GPX files
    FTrack1 := LoadGPXFile(Application.Location + FILENAME1, clRed, 1.0);
    FTrack1.GetArea(totalArea);

    FTrack2 := LoadGPXFile(Application.Location + FILENAME2, clBlue, 1.2);
    FTrack2.GetArea(trackArea);
    ExtendArea(totalArea, trackArea);

    FTrack3 := LoadGPXFile(Application.Location + FILENAME3, clBlack, 0.5);
    FTrack3.GetArea(trackArea);
    ExtendArea(totalArea, trackArea);

    MapView.ZoomOnArea(totalArea);
    UpdateInfo;
  finally
    Screen.Cursor := crs;
  end;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  UpdateInfo;
end;

procedure TMainForm.UpdateInfo;
begin
  ZoomLabel.Caption := 'Zoom ' + MapView.Zoom.ToString;
end;

procedure TMainForm.CheckBox1Change(Sender: TObject);
begin
  FTrack1.Visible := Checkbox1.Checked;
  MapView.Engine.Redraw;
end;

procedure TMainForm.CheckBox2Change(Sender: TObject);
begin
  FTrack2.Visible := Checkbox2.Checked;
  MapView.Engine.Redraw;
end;

procedure TMainForm.CheckBox3Change(Sender: TObject);
begin
  FTrack3.Visible := Checkbox3.Checked;
  MapView.Engine.Redraw;
end;

procedure TMainForm.ComboBox1Change(Sender: TObject);
begin
  MapView.MapProvider := Combobox1.Text;
end;

function TMainForm.LoadGPXFile(AFileName: String;
  AColor: TColor; AWidth: Double): TGPSTrack;
var
  reader: TGpxReader;
  id: Integer;
begin
  reader := TGpxReader.Create;
  try
    id := reader.LoadFromFile(AFileName, MapView.GPSItems);
    Result := MapView.GpsItems.FindTrackByID(id);
    Result.LineColor := AColor;
    Result.LineWidth := AWidth;
  finally
    reader.Free;
  end;
end;

end.

