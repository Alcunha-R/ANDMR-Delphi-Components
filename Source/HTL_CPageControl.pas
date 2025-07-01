unit HTL_CPageControl;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,
  Vcl.Controls, Vcl.Forms, Vcl.Graphics,
  HTL_CPanel, HTL_CTabBar, HTL_ComponentUtils;

{$IFDEF DESIGNTIME}
uses
  DesignIntf, DesignEditors;
{$ENDIF}

type
  THTL_CPageControl = class;

  { THTL_CPage }
  // Esta classe representa uma pgina individual no PageControl.
  // Herda de THTL_CPanel para que possa conter outros controles.
  THTL_CPage = class(THTL_CPanel)
  private
    FPageControl: THTL_CPageControl;
    procedure SetPageControl(const Value: THTL_CPageControl);
  protected
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create(AOwner: TComponent); override;
    property PageControl: THTL_CPageControl read FPageControl;
  end;

  { THTL_CPageControl }
  THTL_CPageControl = class(THTL_CPanel)
  private
    FTabBar: THTL_CTabBar;
    FPages: TList<THTL_CPage>;
    FActivePageIndex: Integer;

    procedure TabBarChanged(Sender: TObject);
    procedure SetActivePageIndex(const Value: Integer);
    function GetActivePage: THTL_CPage;
    procedure UpdatePageVisibility;

  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure Resize; override;
    function GetTabBar: THTL_CTabBar;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage(ACaption: string = ''): THTL_CPage;
    procedure RemovePage(PageIndex: Integer);
    procedure SelectNextPage(GoToFirst: Boolean = True);
    procedure SelectPrevPage(GoToLast: Boolean = True);

    property ActivePage: THTL_CPage read GetActivePage;

  published
    property ActivePageIndex: Integer read FActivePageIndex write SetActivePageIndex default -1;
    property TabBar: THTL_CTabBar read GetTabBar;
  end;

{$IFDEF DESIGNTIME}
  { THTL_CPageControlEditor }
  THTL_CPageControlEditor = class(TDefaultComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;
{$ENDIF}

procedure Register;

implementation

{$IFDEF DESIGNTIME}
uses System.ComponentModel;
{$ENDIF}

{ THTL_CPage }

constructor THTL_CPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPageControl := nil;
  // Configuraes padro para uma pgina
  Align := alClient;
  Visible := False; // As pginas comeam invisveis
  BorderSettings.Visible := False; // A borda  controlada pelo PageControl
end;

procedure THTL_CPage.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
  if AParent is THTL_CPageControl then
    SetPageControl(THTL_CPageControl(AParent))
  else
    SetPageControl(nil);
end;

procedure THTL_CPage.SetPageControl(const Value: THTL_CPageControl);
begin
  if FPageControl <> Value then
  begin
    FPageControl := Value;
  end;
end;

{ THTL_CPageControl }

constructor THTL_CPageControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPages := TList<THTL_CPage>.Create;
  FActivePageIndex := -1;

  // Cria e configura o TabBar interno
  FTabBar := THTL_CTabBar.Create(Self);
  FTabBar.Parent := Self;
  FTabBar.Align := alTop;
  FTabBar.OnChange := TabBarChanged;

  // A borda principal agora  a do PageControl
  Self.BorderSettings.Visible := True;
  Self.BorderSettings.Thickness := 1;
end;

destructor THTL_CPageControl.Destroy;
begin
  FPages.Free;
  // O FTabBar  um componente filho, ser liberado automaticamente
  inherited Destroy;
end;

procedure THTL_CPageControl.Loaded;
begin
  inherited;
  // Garante que a pgina ativa seja exibida aps o carregamento do formulrio
  UpdatePageVisibility;
  Resize;
end;

procedure THTL_CPageControl.Resize;
begin
  inherited;
  // Garante que o TabBar seja reposicionado corretamente
  if Assigned(FTabBar) then
  begin
    case FTabBar.Align of
      alTop: FTabBar.Height := 40;
      alBottom: FTabBar.Height := 40;
      alLeft: FTabBar.Width := 120;
      alRight: FTabBar.Width := 120;
    end;
  end;
end;

procedure THTL_CPageControl.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent is THTL_CPage) and (AComponent.Owner = Self.Owner) then
  begin
    if Operation = opInsert then
    begin
      var page := THTL_CPage(AComponent);
      if FPages.IndexOf(page) = -1 then
      begin
        FPages.Add(page);
        var tabItem := FTabBar.Items.Add;
        tabItem.Caption := page.Name;
        page.Visible := FPages.Count - 1 = FActivePageIndex;

        if FActivePageIndex = -1 then
        begin
          SetActivePageIndex(0);
        end;
      end;
    end
    else if Operation = opRemove then
    begin
      var pageIndex := FPages.IndexOf(THTL_CPage(AComponent));
      if pageIndex > -1 then
      begin
        FPages.Remove(THTL_CPage(AComponent));
        if pageIndex < FTabBar.Items.Count then
          FTabBar.Items.Delete(pageIndex);

        if FActivePageIndex >= pageIndex then
        begin
          if FActivePageIndex > 0 then
            SetActivePageIndex(FActivePageIndex - 1)
          else if FPages.Count > 0 then
            SetActivePageIndex(0)
          else
            SetActivePageIndex(-1);
        end;
      end;
    end;
  end;
end;

procedure THTL_CPageControl.TabBarChanged(Sender: TObject);
begin
  SetActivePageIndex(FTabBar.ActiveIndex);
end;

procedure THTL_CPageControl.SetActivePageIndex(const Value: Integer);
begin
  if (Value <> FActivePageIndex) and (Value >= -1) and (Value < FPages.Count) then
  begin
    FActivePageIndex := Value;
    FTabBar.ActiveIndex := FActivePageIndex;
    UpdatePageVisibility;
  end;
end;

procedure THTL_CPageControl.UpdatePageVisibility;
var
  i: Integer;
begin
  for i := 0 to FPages.Count - 1 do
  begin
    if Assigned(FPages[i]) then
      FPages[i].Visible := (i = FActivePageIndex);
  end;
end;

function THTL_CPageControl.GetActivePage: THTL_CPage;
begin
  if (FActivePageIndex >= 0) and (FActivePageIndex < FPages.Count) then
    Result := FPages[FActivePageIndex]
  else
    Result := nil;
end;

function THTL_CPageControl.GetTabBar: THTL_CTabBar;
begin
  Result := FTabBar;
end;

function THTL_CPageControl.AddPage(ACaption: string): THTL_CPage;
begin
  Result := THTL_CPage.Create(Self.Owner);
  Result.Parent := Self;
  if ACaption = '' then
    Result.Name := 'Page' + IntToStr(FPages.Count)
  else
    Result.Name := ACaption;
  Result.Caption := Result.Name; // Sincroniza o caption da pgina
  // A lgica em Notification cuidar de adicionar a pgina  lista e criar a aba
end;

procedure THTL_CPageControl.RemovePage(PageIndex: Integer);
begin
  if (PageIndex >= 0) and (PageIndex < FPages.Count) then
  begin
    FPages[PageIndex].Free; // Isso vai disparar a notificao opRemove
  end;
end;

procedure THTL_CPageControl.SelectNextPage(GoToFirst: Boolean);
var
  NextIndex: Integer;
begin
  if FPages.Count = 0 then Exit;
  NextIndex := FActivePageIndex + 1;
  if NextIndex >= FPages.Count then
  begin
    if GoToFirst then
      NextIndex := 0
    else
      Exit; // No faz nada se no for para voltar ao incio
  end;
  ActivePageIndex := NextIndex;
end;

procedure THTL_CPageControl.SelectPrevPage(GoToLast: Boolean);
var
  PrevIndex: Integer;
begin
  if FPages.Count = 0 then Exit;
  PrevIndex := FActivePageIndex - 1;
  if PrevIndex < 0 then
  begin
    if GoToLast then
      PrevIndex := FPages.Count - 1
    else
      Exit; // No faz nada se no for para ir ao final
  end;
  ActivePageIndex := PrevIndex;
end;

{$IFDEF DESIGNTIME}
{ THTL_CPageControlEditor }

function THTL_CPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4; // Adicionar, Remover, Prxima, Anterior
end;

function THTL_CPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Adicionar Pgina';
    1: Result := 'Remover Pgina Ativa';
    2: Result := 'Prxima Pgina';
    3: Result := 'Pgina Anterior';
  end;
end;

procedure THTL_CPageControlEditor.ExecuteVerb(Index: Integer);
var
  PageControl: THTL_CPageControl;
begin
  PageControl := Component as THTL_CPageControl;
  case Index of
    0: // Adicionar Pgina
    begin
      Designer.Modified;
      PageControl.AddPage;
    end;
    1: // Remover Pgina Ativa
    begin
      if PageControl.ActivePageIndex <> -1 then
      begin
        Designer.Modified;
        PageControl.RemovePage(PageControl.ActivePageIndex);
      end;
    end;
    2: // Prxima Pgina
    begin
      Designer.Modified;
      PageControl.SelectNextPage(True);
    end;
    3: // Pgina Anterior
    begin
      Designer.Modified;
      PageControl.SelectPrevPage(True);
    end;
  end;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_CPageControl]);
{$IFDEF DESIGNTIME}
  RegisterComponentEditor(THTL_CPageControl, THTL_CPageControlEditor);
{$ENDIF}
end;

end.
