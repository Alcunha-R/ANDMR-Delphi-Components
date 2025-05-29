unit ANDMR_ComponentUtils;

interface

uses
  System.SysUtils, System.Classes, Vcl.Graphics, Winapi.Windows, System.Math,
  System.UITypes, Winapi.GDIPOBJ, Winapi.GDIPAPI; // Common units needed

type
  // Common Type Definitions
  TRoundCornerType = (
    rctNone, rctAll, rctTopLeft, rctTopRight, rctBottomLeft, rctBottomRight,
    rctTop, rctBottom, rctLeft, rctRight,
    rctTopLeftBottomRight, rctTopRightBottomLeft
  );

  TEdgeMargins = class(TPersistent)
  private
    FLeft, FTop, FRight, FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetValue(var AField: Integer; const AValue: Integer); // Helper for setters
    procedure SetLeft(const Value: Integer);
    procedure SetTop(const Value: Integer);
    procedure SetRight(const Value: Integer);
    procedure SetBottom(const Value: Integer);
  protected
    procedure Changed; virtual;
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

// Declarations of Helper Functions
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
function DarkerColor(Color: TColor; Percent: Byte = 20): TColor;
function LighterColor(Color: TColor; Percent: Byte = 20): TColor;
function BlendColors(Color1, Color2: TColor; Factor: Single): TColor;

implementation

{ TEdgeMargins }
constructor TEdgeMargins.Create;
begin
  inherited Create;
  FLeft := 2;
  FTop := 2;
  FRight := 2;
  FBottom := 2;
end;

procedure TEdgeMargins.Assign(Source: TPersistent);
begin
  if Source is TEdgeMargins then
  begin
    Self.FLeft := TEdgeMargins(Source).FLeft;
    Self.FTop := TEdgeMargins(Source).FTop;
    Self.FRight := TEdgeMargins(Source).FRight;
    Self.FBottom := TEdgeMargins(Source).FBottom;
    // Assign usually does not trigger OnChange, only direct property changes do.
    // If a change notification is needed after Assign, the caller should handle it.
    // Changed; // Typically not called in Assign
  end
  else
    inherited Assign(Source);
end;

procedure TEdgeMargins.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TEdgeMargins.SetValue(var AField: Integer; const AValue: Integer);
begin
  if AField <> AValue then
  begin
    AField := AValue;
    Changed;
  end;
end;

procedure TEdgeMargins.SetLeft(const Value: Integer);
begin
  SetValue(FLeft, Value);
end;

procedure TEdgeMargins.SetTop(const Value: Integer);
begin
  SetValue(FTop, Value);
end;

procedure TEdgeMargins.SetRight(const Value: Integer);
begin
  SetValue(FRight, Value);
end;

procedure TEdgeMargins.SetBottom(const Value: Integer);
begin
  SetValue(FBottom, Value);
end;

// Implementation of Helper Functions
function ColorToARGB(AColor: TColor; Alpha: Byte = 255): Cardinal;
var
  ColorRef: LongWord;
begin
  if AColor = clNone then
  begin
    Result := (Alpha shl 24);
    Exit;
  end;
  ColorRef := ColorToRGB(AColor); // Vcl.Graphics.ColorToRGB
  Result := (Alpha shl 24) or
            ((ColorRef and $000000FF) shl 16) or // B
            (ColorRef and $0000FF00) or          // G
            ((ColorRef and $00FF0000) shr 16);   // R
end;

procedure CreateGPRoundedPath(APath: TGPGraphicsPath; const ARect: TGPRectF; ARadiusValue: Single; AType: TRoundCornerType);
const
  MIN_RADIUS_FOR_PATH = 0.5; // Ensure this is Single
var
  LRadius, LDiameter: Single;
  RoundTL, RoundTR, RoundBL, RoundBR: Boolean;
begin
  APath.Reset;
  if (ARect.Width <= 0) or (ARect.Height <= 0) then Exit;

  LRadius := ARadiusValue;
  LRadius := Min(LRadius, Min(ARect.Width / 2.0, ARect.Height / 2.0)); // Use 2.0 for float division
  LRadius := Max(0.0, LRadius); // Use 0.0 for float comparison
  LDiameter := LRadius * 2.0;

  if (AType = rctNone) or (LRadius < MIN_RADIUS_FOR_PATH) or (LDiameter <= 0) then
  begin
    APath.AddRectangle(ARect);
    Exit;
  end;

  RoundTL := AType in [rctAll, rctTopLeft, rctTop, rctLeft, rctTopLeftBottomRight];
  RoundTR := AType in [rctAll, rctTopRight, rctTop, rctRight, rctTopRightBottomLeft];
  RoundBL := AType in [rctAll, rctBottomLeft, rctBottom, rctLeft, rctTopRightBottomLeft];
  RoundBR := AType in [rctAll, rctBottomRight, rctBottom, rctRight, rctTopLeftBottomRight];

  APath.StartFigure;
  if RoundTL then APath.AddArc(ARect.X, ARect.Y, LDiameter, LDiameter, 180, 90)
  else APath.AddLine(ARect.X, ARect.Y, ARect.X, ARect.Y);

  APath.AddLine(ARect.X + IfThen(RoundTL, LRadius, 0.0), ARect.Y, ARect.X + ARect.Width - IfThen(RoundTR, LRadius, 0.0), ARect.Y);

  if RoundTR then APath.AddArc(ARect.X + ARect.Width - LDiameter, ARect.Y, LDiameter, LDiameter, 270, 90)
  else APath.AddLine(ARect.X + ARect.Width, ARect.Y, ARect.X + ARect.Width, ARect.Y);

  APath.AddLine(ARect.X + ARect.Width, ARect.Y + IfThen(RoundTR, LRadius, 0.0), ARect.X + ARect.Width, ARect.Y + ARect.Height - IfThen(RoundBR, LRadius, 0.0));

  if RoundBR then APath.AddArc(ARect.X + ARect.Width - LDiameter, ARect.Y + ARect.Height - LDiameter, LDiameter, LDiameter, 0, 90)
  else APath.AddLine(ARect.X + ARect.Width, ARect.Y + ARect.Height, ARect.X + ARect.Width, ARect.Y + ARect.Height);

  APath.AddLine(ARect.X + ARect.Width - IfThen(RoundBR, LRadius, 0.0), ARect.Y + ARect.Height, ARect.X + IfThen(RoundBL, LRadius, 0.0), ARect.Y + ARect.Height);

  if RoundBL then APath.AddArc(ARect.X, ARect.Y + ARect.Height - LDiameter, LDiameter, LDiameter, 90, 90)
  else APath.AddLine(ARect.X, ARect.Y + ARect.Height, ARect.X, ARect.Y + ARect.Height);
  
  APath.CloseFigure;
end;

function DarkerColor(Color: TColor; Percent: Byte = 20): TColor;
var
  R, G, B: Byte;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := Max(0, Round(R * (100 - Percent) / 100.0)); // Use 100.0 for float division
  G := Max(0, Round(G * (100 - Percent) / 100.0));
  B := Max(0, Round(B * (100 - Percent) / 100.0));
  Result := RGB(R, G, B);
end;

function LighterColor(Color: TColor; Percent: Byte = 20): TColor;
var
  R, G, B: Byte;
begin
  if Color = clNone then Exit(clNone);
  Color := ColorToRGB(Color);
  R := GetRValue(Color);
  G := GetGValue(Color);
  B := GetBValue(Color);
  R := Min(255, Round(R + (255 - R) * Percent / 100.0)); // Use 100.0 for float division
  G := Min(255, Round(G + (255 - G) * Percent / 100.0));
  B := Min(255, Round(B + (255 - B) * Percent / 100.0));
  Result := RGB(R, G, B);
end;

function BlendColors(Color1, Color2: TColor; Factor: Single): TColor;
var
  R1, G1, B1, R2, G2, B2, R, G, B: Byte;
begin
  if Factor <= 0.0 then Exit(Color1);
  if Factor >= 1.0 then Exit(Color2);

  // Simplified transparency handling for E1012 fix
  if Color1 = clNone then
  begin
    // If Color1 is transparent, return Color2 as is.
    // This includes Color2's original alpha if it's an TAlphaColor.
    Exit(Color2);
  end;
  if Color2 = clNone then
  begin
    // If Color2 is transparent, return Color1 as is.
    Exit(Color1);
  end;

  // Proceed with RGB blending.
  // ColorToRGB extracts pure RGB, effectively stripping any incoming alpha for these calculations.
  R1 := GetRValue(ColorToRGB(Color1));
  G1 := GetGValue(ColorToRGB(Color1));
  B1 := GetBValue(ColorToRGB(Color1));
  R2 := GetRValue(ColorToRGB(Color2));
  G2 := GetGValue(ColorToRGB(Color2));
  B2 := GetBValue(ColorToRGB(Color2));

  R := Round(R1 + (R2 - R1) * Factor);
  G := Round(G1 + (G2 - G1) * Factor);
  B := Round(B1 + (B2 - B1) * Factor);
  Result := RGB(R, G, B); // Result is an opaque TColor (alpha byte $00).
end;

end.
