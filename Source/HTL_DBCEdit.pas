unit HTL_DBCEdit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Data.DB, Vcl.DBCtrls,
  Winapi.Messages, Vcl.StdCtrls,
  HTL_CEdit, HTL_ComponentUtils, Winapi.Windows;

type
  THTL_DBCEdit = class(THTL_CEdit)
  private
    FDataLink: TFieldDataLink;
    procedure DoChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure ActiveChange(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    function FieldIsBlob: Boolean;
    property Field: TField read GetField;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property ReadOnly;
    property Enabled;
    property OnChange;
    property OnEnter;
    property OnExit;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('HOTLINE', [THTL_DBCEdit]);
end;

{ THTL_DBCEdit }

constructor THTL_DBCEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Self.OnChange := DoChange;

  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor THTL_DBCEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure THTL_DBCEdit.DoChange(Sender: TObject);
begin
  FDataLink.Modified;
end;

procedure THTL_DBCEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;

// <<< CORREÇÃO: Implementação padrão do método Notification para componentes data-aware
procedure THTL_DBCEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

procedure THTL_DBCEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

procedure THTL_DBCEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field = nil then
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end
  else
  begin
    if FDataLink.Field.IsNull then
      Text := ''
    else if FieldIsBlob then
      Text := FDataLink.Field.DisplayText
    else
      Text := FDataLink.Field.AsString;
  end;
end;

procedure THTL_DBCEdit.UpdateData(Sender: TObject);
begin
  if (FDataLink.Field <> nil) and not FieldIsBlob then
    FDataLink.Field.AsString := Text;
end;

procedure THTL_DBCEdit.ActiveChange(Sender: TObject);
begin
  Enabled := FDataLink.Active;
end;

procedure THTL_DBCEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if not ReadOnly then
    FDataLink.Edit;
  inherited KeyDown(Key, Shift);
end;

procedure THTL_DBCEdit.KeyPress(var Key: Char);
begin
  if (Key >= ' ') and (not ReadOnly) then
    FDataLink.Edit;
  inherited KeyPress(Key);
end;

function THTL_DBCEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function THTL_DBCEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function THTL_DBCEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure THTL_DBCEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure THTL_DBCEdit.SetDataSource(const Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    if FDataLink.DataSource <> nil then
      FDataLink.DataSource.RemoveFreeNotification(Self);
    FDataLink.DataSource := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

function THTL_DBCEdit.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or
            (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;

function THTL_DBCEdit.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or
            (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;

function THTL_DBCEdit.FieldIsBlob: Boolean;
var
  LField: TField;
begin
  LField := FDataLink.Field;
  Result := (LField <> nil) and (LField.DataType in [ftBlob, ftMemo, ftWideMemo, ftGraphic, ftFmtMemo]);
end;

end.

