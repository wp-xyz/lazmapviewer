unit gpsptform;

{$mode objfpc}{$H+}

interface

uses
  LCLIntf, LCLType, Classes, SysUtils, Forms, Controls, Graphics, Types,
  Dialogs, StdCtrls, ExtCtrls, ImgList, ButtonPanel, ColorBox, Spin,
  mvExtraData;

type

  TGPSSymbol = (gpsPlus, gpsCross, gpsFilledCircle, gpsOpenCircle,
    gpsFilledRect, gpsOpenRect);

  TGPSExtraData = class(TDrawingExtraData)
  private
    FSymbol: TGPSSymbol;
    FSize: Integer;
  public
    constructor Create(aID: Integer); override;
    property Symbol: TGPSSymbol read FSymbol write FSymbol;
    property Size: Integer read FSize write FSize;
  end;

  { TGPSPointForm }

  TGPSPointForm = class(TForm)
    ButtonPanel1: TButtonPanel;
    clbSymbolColor: TColorBox;
    cbSymbols: TComboBox;
    cmbImageIndex: TComboBox;
    edGPSPointLabel: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    lblImageIndex: TLabel;
    lblSymbol: TLabel;
    lblSize: TLabel;
    Notebook1: TNotebook;
    pgSymbols: TPage;
    pgImageIndex: TPage;
    Panel1: TPanel;
    seSize: TSpinEdit;
    procedure cmbImageIndexDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure FormShow(Sender: TObject);
  private
    FImageList: TCustomImageList;

  public
    procedure GetSymbolData(var AName: String; var AColor: TColor;
      var ASymbol: TGPSSymbol; var ASize: Integer);
    procedure SetSymbolData(const AName: String; AColor: TColor;
      ASymbol: TGPSSymbol; ASize: Integer);

    procedure GetImageIndexData(var AName: String; var AImgIndex: Integer);
    procedure SetImageIndexData(const AName: String;
      AImageList: TCustomImageList; AImageIndex: Integer);
  end;

var
  GPSPointForm: TGPSPointForm;

implementation

{$R *.lfm}

constructor TGPSExtraData.Create(aID: Integer);
begin
  inherited Create(aID);
  FSymbol := gpsPlus;
  FSize := 10;
end;

procedure TGPSPointForm.FormShow(Sender: TObject);
begin
  edGPSPointLabel.SetFocus;
end;

procedure TGPSPointForm.cmbImageIndexDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
var
  combobox: TCombobox;
begin
  combobox := Control as TCombobox;

  if State * [odSelected, odFocused] <> [] then
    combobox.Canvas.Brush.Color := clHighlight
  else
    combobox.Canvas.Brush.Color := clWindow;
  combobox.Canvas.FillRect(ARect);
  FImageList.Draw(combobox.Canvas, ARect.Left+2, ARect.Top+2, Index);
end;

procedure TGPSPointForm.GetImageIndexData(var AName: String; var AImgIndex: Integer);
begin
  AName := edGPSPointLabel.Text;
  AImgIndex := cmbImageIndex.ItemIndex;
end;

procedure TGPSPointForm.SetImageIndexData(const AName: String;
  AImageList: TCustomImageList; AImageIndex: Integer);
var
  i: Integer;
begin
  Notebook1.PageIndex := 1;

  edGPSPointLabel.Text := AName;
  if FImageList <> AImageList then
  begin
    cmbImageIndex.Items.Clear;
    for i := 0 to AImageList.Count-1 do
      cmbImageIndex.Items.Add(IntToStr(i));
    FImageList := AImageList;
  end;
  cmbImageIndex.ItemIndex := AImageIndex;
end;

procedure TGPSPointForm.GetSymbolData(var AName: String; var AColor: TColor;
  var ASymbol: TGPSSymbol; var ASize: Integer);
begin
  AName := edGPSPointLabel.Text;
  AColor := clbSymbolColor.Selected;
  ASymbol := TGPSSymbol(cbSymbols.ItemIndex);
  ASize := seSize.Value;
end;

procedure TGPSPointForm.SetSymbolData(const AName: String; AColor: TColor;
  ASymbol: TGPSSymbol; ASize: Integer);
begin
  Notebook1.PageIndex := 0;

  edGPSPointLabel.Text := AName;
  clbSymbolColor.Selected := AColor;
  cbSymbols.ItemIndex := ord(ASymbol);
  seSize.Value := ASize
end;

end.

