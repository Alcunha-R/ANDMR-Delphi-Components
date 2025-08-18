unit HTL_CPageControl_Design;

interface

uses
  System.SysUtils, System.Classes,
  DesignIntf, DesignEditors,
  HTL_CPageControl;

type
  THTL_CPageControlEditor = class(TComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  private
    procedure AddPage;
    procedure RemovePage;
    procedure NextPage;
    procedure PrevPage;
  end;

procedure Register;

implementation

uses Vcl.Dialogs;

procedure Register;
begin
  RegisterComponentEditor(THTL_CPageControl, THTL_CPageControlEditor);
end;

{ THTL_CPageControlEditor }

procedure THTL_CPageControlEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: AddPage;
    1: RemovePage;
    2: PrevPage;
    3: NextPage;
  end;
end;

function THTL_CPageControlEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Adicionar Página';
    1: Result := 'Remover Página';
    2: Result := 'Página Anterior';
    3: Result := 'Próxima Página';
  else
    Result := '';
  end;
end;

function THTL_CPageControlEditor.GetVerbCount: Integer;
begin
  Result := 4;
end;

procedure THTL_CPageControlEditor.AddPage;
var
  PageControl: THTL_CPageControl;
  NewPage: THTL_CPage;
begin
  PageControl := Component as THTL_CPageControl;
  Designer.Modify;
  NewPage := THTL_CPage.Create(PageControl);
  NewPage.Parent := PageControl;
  NewPage.Caption := 'Page' + IntToStr(PageControl.PageCount);
  PageControl.ActivePageIndex := PageControl.PageCount - 1;
end;

procedure THTL_CPageControlEditor.RemovePage;
var
  PageControl: THTL_CPageControl;
  ActivePage: THTL_CPage;
begin
  PageControl := Component as THTL_CPageControl;
  ActivePage := PageControl.ActivePage;
  if (ActivePage <> nil) and (MessageDlg('Remover a página "' + ActivePage.Caption + '"?', mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
  begin
    Designer.Modify;
    ActivePage.Free;
  end;
end;

procedure THTL_CPageControlEditor.NextPage;
var
  PageControl: THTL_CPageControl;
begin
  PageControl := Component as THTL_CPageControl;
  if PageControl.PageCount > 0 then
  begin
    Designer.Modify;
    PageControl.ActivePageIndex := (PageControl.ActivePageIndex + 1) mod PageControl.PageCount;
  end;
end;

procedure THTL_CPageControlEditor.PrevPage;
var
  PageControl: THTL_CPageControl;
begin
  PageControl := Component as THTL_CPageControl;
  if PageControl.PageCount > 0 then
  begin
    Designer.Modify;
    PageControl.ActivePageIndex := (PageControl.ActivePageIndex - 1 + PageControl.PageCount) mod PageControl.PageCount;
  end;
end;

end.
