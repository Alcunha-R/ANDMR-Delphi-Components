unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Vcl.Themes, Vcl.Controls, Vcl.StdCtrls, Winapi.Windows; // Added Vcl.StdCtrls, Winapi.Windows for TFont, TColor etc.

type
  // Delphi type definitions will be moved here from ANDMR_CEdit.pas

  TImagePositionSide = (ipsLeft, ipsRight);
  TImageAlignmentVertical = (iavTop, iavCenter, iavBottom);
  TImagePlacement = (iplInsideBounds, iplOutsideBounds);
  TImageDrawMode = (idmStretch, idmProportional, idmNormal);
  TSeparatorHeightMode = (shmFull, shmAsText, shmAsImage, shmCustom);

  TRoundCornerType = (
    rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight,
    rctTop, rctBottom, rctLeft, rctRight,
    rctTopLeftBottomRight, rctTopRightBottomLeft
  );

  TInputType = (itNormal, itLettersOnly, itNumbersOnly, itNoSpecialChars, itAlphaNumericOnly);
  TTextCase = (tcNormal, tcUppercase, tcLowercase);
  TCaptionPosition = (cpAbove, cpBelow, cpLeft, cpRight);

  TImageMarginsControl = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write SetLeft default 2;
    property Top: Integer read FTop write SetTop default 2;
    property Right: Integer read FRight write SetRight default 2;
    property Bottom: Integer read FBottom write SetBottom default 2;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCaptionSettings = class(TPersistent)
  private
    FVisible: Boolean;
    FText: string;
    FPosition: TCaptionPosition;
    FAlignment: TAlignment; // Vcl.Themes.TAlignment
    FFont: TFont;
    FColor: TColor;
    FOffset: Integer;
    FWordWrap: Boolean;
    FOnChange: TNotifyEvent;
    FOwnerControl: TWinControl; 
    procedure SetVisible(const Value: Boolean);
    procedure SetText(const Value: string);
    procedure SetPosition(const Value: TCaptionPosition);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFont(const Value: TFont);
    procedure SetColor(const Value: TColor);
    procedure SetOffset(const Value: Integer);
    procedure SetWordWrap(const Value: Boolean);
    procedure FontChanged(Sender: TObject);
  protected
    procedure Changed; virtual;
  public
    constructor Create(AOwner: TWinControl); 
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default True;
    property Text: string read FText write SetText;
    property Position: TCaptionPosition read FPosition write SetPosition default cpAbove;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Font: TFont read FFont write SetFont;
    property Color: TColor read FColor write SetColor default clWindowText; 
    property Offset: Integer read FOffset write SetOffset default 2;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  THoverSettings = class(TPersistent)
  private
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FFontColor: TColor;
    FCaptionFontColor: TColor;
    FEnabled: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFontColor(const Value: TColor);
    procedure SetCaptionFontColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure Changed; virtual;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clInfoBk;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clHighlight;
    property FontColor: TColor read FFontColor write SetFontColor default clInfoText;
    property CaptionFontColor: TColor read FCaptionFontColor write SetCaptionFontColor default clInfoText;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TTextMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property Left: Integer read FLeft write SetLeft default 4; 
    property Top: Integer read FTop write SetTop default 2;   
    property Right: Integer read FRight write SetRight default 4; 
    property Bottom: Integer read FBottom write SetBottom default 2; 
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TImageMarginsControl }
constructor TImageMarginsControl.Create;
begin
  inherited Create;
  FLeft := 2; FTop := 2; FRight := 2; FBottom := 2;
end;

procedure TImageMarginsControl.Assign(Source: TPersistent);
begin
  if Source is TImageMarginsControl then
  begin
    Self.FLeft := TImageMarginsControl(Source).FLeft;
    Self.FTop := TImageMarginsControl(Source).FTop;
    Self.FRight := TImageMarginsControl(Source).FRight;
    Self.FBottom := TImageMarginsControl(Source).FBottom;
    Changed; // Ensure OnChange is triggered if assigned
  end else inherited Assign(Source);
end;

procedure TImageMarginsControl.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TImageMarginsControl.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TImageMarginsControl.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TImageMarginsControl.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TImageMarginsControl.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;

{ TCaptionSettings }
constructor TCaptionSettings.Create(AOwner: TWinControl);
begin
  inherited Create;
  FOwnerControl := AOwner;
  FVisible := True;
  FText := '';
  FPosition := cpAbove;
  FAlignment := taLeftJustify;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  if Assigned(AOwner) and (AOwner is TCustomControl) then // Assuming AOwner is the component itself
     FFont.Assign((AOwner as TCustomControl).Font)
  else
  begin
    FFont.Name := 'Segoe UI';
    FFont.Size := 9;
  end;
  FColor := clWindowText;
  FOffset := 2;
  FWordWrap := False;
end;

destructor TCaptionSettings.Destroy;
begin
  FFont.OnChange := nil;
  FFont.Free;
  inherited Destroy;
end;

procedure TCaptionSettings.Assign(Source: TPersistent);
begin
  if Source is TCaptionSettings then
  begin
    FVisible := TCaptionSettings(Source).FVisible;
    FText := TCaptionSettings(Source).FText;
    FPosition := TCaptionSettings(Source).FPosition;
    FAlignment := TCaptionSettings(Source).FAlignment;
    FFont.Assign(TCaptionSettings(Source).FFont);
    FColor := TCaptionSettings(Source).FColor;
    FOffset := TCaptionSettings(Source).FOffset;
    FWordWrap := TCaptionSettings(Source).FWordWrap;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure TCaptionSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCaptionSettings.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TCaptionSettings.SetAlignment(const Value: TAlignment); begin if FAlignment <> Value then begin FAlignment := Value; Changed; end; end;
procedure TCaptionSettings.SetColor(const Value: TColor); begin if FColor <> Value then begin FColor := Value; Changed; end; end;
procedure TCaptionSettings.SetFont(const Value: TFont); begin FFont.Assign(Value); Changed; end;
procedure TCaptionSettings.SetOffset(const Value: Integer); begin if FOffset <> Value then begin FOffset := Value; Changed; end; end;
procedure TCaptionSettings.SetPosition(const Value: TCaptionPosition); begin if FPosition <> Value then begin FPosition := Value; Changed; end; end;
procedure TCaptionSettings.SetText(const Value: string); begin if FText <> Value then begin FText := Value; Changed; end; end;
procedure TCaptionSettings.SetVisible(const Value: Boolean); begin if FVisible <> Value then begin FVisible := Value; Changed; end; end;
procedure TCaptionSettings.SetWordWrap(const Value: Boolean); begin if FWordWrap <> Value then begin FWordWrap := Value; Changed; end; end;

{ THoverSettings }
constructor THoverSettings.Create;
begin
  inherited Create;
  FEnabled := False;
  FBackgroundColor := clInfoBk;
  FBorderColor := clHighlight;
  FFontColor := clInfoText;
  FCaptionFontColor := clInfoText;
end;

procedure THoverSettings.Assign(Source: TPersistent);
begin
  if Source is THoverSettings then
  begin
    FEnabled := THoverSettings(Source).FEnabled;
    FBackgroundColor := THoverSettings(Source).FBackgroundColor;
    FBorderColor := THoverSettings(Source).FBorderColor;
    FFontColor := THoverSettings(Source).FFontColor;
    FCaptionFontColor := THoverSettings(Source).FCaptionFontColor;
    Changed;
  end
  else
    inherited Assign(Source);
end;

procedure THoverSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure THoverSettings.SetBackgroundColor(const Value: TColor); begin if FBackgroundColor <> Value then begin FBackgroundColor := Value; Changed; end; end;
procedure THoverSettings.SetBorderColor(const Value: TColor); begin if FBorderColor <> Value then begin FBorderColor := Value; Changed; end; end;
procedure THoverSettings.SetCaptionFontColor(const Value: TColor); begin if FCaptionFontColor <> Value then begin FCaptionFontColor := Value; Changed; end; end;
procedure THoverSettings.SetEnabled(const Value: Boolean); begin if FEnabled <> Value then begin FEnabled := Value; Changed; end; end;
procedure THoverSettings.SetFontColor(const Value: TColor); begin if FFontColor <> Value then begin FFontColor := Value; Changed; end; end;

{ TTextMargins }
constructor TTextMargins.Create;
begin
  inherited Create;
  FLeft := 4; FTop := 2; FRight := 4; FBottom := 2;
end;

procedure TTextMargins.Assign(Source: TPersistent);
begin
  if Source is TTextMargins then
  begin
    Self.FLeft := TTextMargins(Source).FLeft;
    Self.FTop := TTextMargins(Source).FTop;
    Self.FRight := TTextMargins(Source).FRight;
    Self.FBottom := TTextMargins(Source).FBottom;
    Changed;
  end else inherited Assign(Source);
end;

procedure TTextMargins.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TTextMargins.SetLeft(const Value: Integer); begin if FLeft <> Value then begin FLeft := Value; Changed; end; end;
procedure TTextMargins.SetTop(const Value: Integer); begin if FTop <> Value then begin FTop := Value; Changed; end; end;
procedure TTextMargins.SetRight(const Value: Integer); begin if FRight <> Value then begin FRight := Value; Changed; end; end;
procedure TTextMargins.SetBottom(const Value: Integer); begin if FBottom <> Value then begin FBottom := Value; Changed; end; end;

end.
