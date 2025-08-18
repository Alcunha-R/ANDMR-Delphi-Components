unit HTL_DBCCheckBox;

interface

uses
  System.SysUtils, System.Classes, Vcl.Controls, Data.DB, Vcl.DBCtrls,
  System.Types, Winapi.Messages,
  HTL_CCheckBox, Winapi.Windows, System.Variants, HTL_ComponentUtils;

type
  // Classes Interposer para acessar campos privados das classes base.
  // Isso é necessário para a pintura correta dentro de um TDBCtrlGrid sem
  // modificar o código fonte original e sem causar repaints indesejados.
  THTL_CCheckBoxAccess = class(HTL_CCheckBox.THTL_CCheckBox);
  TCaptionSettingsAccess = class(HTL_ComponentUtils.TCaptionSettings);

  THTL_DBCCheckBox = class(THTL_CCheckBox)
  private
    FDataLink: TFieldDataLink;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure DataChange(Sender: TObject);
    procedure UpdateData(Sender: TObject);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Click; override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
  end;

procedure Register;

implementation

uses Vcl.Forms;

procedure Register;
begin
  // Registra o novo componente na paleta 'HOTLINE DB'
  RegisterComponents('HOTLINE DB', [THTL_DBCCheckBox]);
end;

{ THTL_DBCCheckBox }

constructor THTL_DBCCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  // Cria o objeto de link de dados
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnUpdateData := UpdateData;
end;

destructor THTL_DBCCheckBox.Destroy;
begin
  // Libera o objeto de link de dados
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure THTL_DBCCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  // Se o DataSource associado for removido, limpa a referência local
  if (Operation = opRemove) and (FDataLink <> nil) and (AComponent = DataSource) then
    DataSource := nil;
end;

function THTL_DBCCheckBox.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function THTL_DBCCheckBox.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure THTL_DBCCheckBox.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure THTL_DBCCheckBox.SetDataSource(const Value: TDataSource);
begin
  // Associa o DataSource ao nosso link de dados
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  // Registra para receber notificação caso o DataSource seja liberado
  if Value <> nil then
    Value.FreeNotification(Self);
end;

// Evento disparado quando os dados no DataSet mudam
procedure THTL_DBCCheckBox.DataChange(Sender: TObject);
begin
  // Atualiza o estado do checkbox com o valor do campo do dataset
  if (FDataLink.Field <> nil) and (not FDataLink.Field.IsNull) then
  begin
    // A verificação agora é baseada no FDataLink.Field.DataType, que é mais robusto
    case FDataLink.Field.DataType of
      ftBoolean:
        Self.Checked := FDataLink.Field.AsBoolean;
      ftString, ftFixedChar, ftWideString:
        Self.Checked := AnsiSameText(FDataLink.Field.AsString, 'S') or
                        AnsiSameText(FDataLink.Field.AsString, 'Y') or
                        AnsiSameText(FDataLink.Field.AsString, 'T') or
                        (FDataLink.Field.AsString = '1');
      ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
        Self.Checked := FDataLink.Field.AsInteger <> 0;
    else
      // Fallback para campos que não são diretamente suportados, tentando uma conversão genérica
      try
        Self.Checked := FDataLink.Field.AsBoolean;
      except
        Self.Checked := False; // Se a conversão falhar, desmarca.
      end;
    end;
  end
  else
  begin
    // Se não houver campo ou o valor for nulo, desmarca o checkbox
    Self.Checked := False;
  end;
end;

// Evento disparado para salvar os dados do componente no DataSet
procedure THTL_DBCCheckBox.UpdateData(Sender: TObject);
begin
  // Atualiza o campo do dataset com o estado do checkbox
  if (FDataLink.Field <> nil) and FDataLink.Field.CanModify then
  begin
    // Converte o valor 'Checked' para o tipo de dado do campo
    case FDataLink.Field.DataType of
      ftBoolean: FDataLink.Field.AsBoolean := Self.Checked;
      ftString, ftFixedChar, ftWideString:
        if Self.Checked then FDataLink.Field.AsString := 'S'
        else FDataLink.Field.AsString := 'N';
      ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
        if Self.Checked then FDataLink.Field.AsInteger := 1
        else FDataLink.Field.AsInteger := 0;
    end;
  end;
end;

procedure THTL_DBCCheckBox.Click;
begin
  // A condição FDataLink.Edit coloca o dataset em modo de edição e retorna True se for bem-sucedido.
  // Isso impede o clique se o dataset for somente leitura.
  if not (csDesigning in ComponentState) and FDataLink.Edit then
  begin
    // Chama o Click original, que inverte a propriedade 'Checked'
    inherited Click;
    // Notifica o DataLink que o dado foi modificado. ESSA LINHA É CRUCIAL.
    // Sem ela, a alteração não é registrada e se perde ao mudar de registro.
    FDataLink.Modified;
  end
  else if (csDesigning in ComponentState) then
  begin
    // Permite o clique visual em tempo de design sem afetar dados
    inherited Click;
  end;
end;

procedure THTL_DBCCheckBox.Paint;
var
  OriginalCheckedState: Boolean;
  RowCheckedState: Boolean;
  OriginalCaption: string;
begin
  // Salva o estado atual do componente (que corresponde ao registro ativo)
  OriginalCheckedState := THTL_CCheckBoxAccess(Self).Checked;
  OriginalCaption := TCaptionSettingsAccess(Self.CaptionSettings).Text;

  try
    // Determina o estado correto para a linha que o TDBCtrlGrid está desenhando.
    RowCheckedState := False;
    if (FDataLink.DataSource <> nil) and (FDataLink.DataSource.DataSet <> nil) and
       FDataLink.DataSource.DataSet.Active and (FDataLink.Field <> nil) and
       (not FDataLink.Field.IsNull) then
    begin
      case FDataLink.Field.DataType of
        ftBoolean:
          RowCheckedState := FDataLink.Field.AsBoolean;
        ftString, ftFixedChar, ftWideString:
          RowCheckedState := AnsiSameText(FDataLink.Field.AsString, 'S') or
                           AnsiSameText(FDataLink.Field.AsString, 'Y') or
                           AnsiSameText(FDataLink.Field.AsString, 'T') or
                           (FDataLink.Field.AsString = '1');
        ftSmallint, ftInteger, ftWord, ftLargeint, ftAutoInc:
          RowCheckedState := FDataLink.Field.AsInteger <> 0;
      else
        try
          RowCheckedState := FDataLink.Field.AsBoolean;
        except
          RowCheckedState := False;
        end;
      end;
    end;

    // Define o estado interno diretamente, apenas para este ciclo de pintura, sem causar Repaint.
    THTL_CCheckBoxAccess(Self).Checked := RowCheckedState;
    TCaptionSettingsAccess(Self.CaptionSettings).Text := OriginalCaption;

    // Chama o método de pintura original, que usará o estado que acabamos de definir.
    inherited Paint;
  finally
    // Restaura o estado original do componente para que ele corresponda ao registro ativo.
    THTL_CCheckBoxAccess(Self).Checked := OriginalCheckedState;
    TCaptionSettingsAccess(Self.CaptionSettings).Text := OriginalCaption;
  end;
end;

// Responde à mensagem do sistema para fornecer o DataLink
procedure THTL_DBCCheckBox.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := LRESULT(FDataLink);
end;

// Funções de integração com TAction
function THTL_DBCCheckBox.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := inherited ExecuteAction(Action) or (FDataLink <> nil) and FDataLink.ExecuteAction(Action);
end;

function THTL_DBCCheckBox.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := inherited UpdateAction(Action) or (FDataLink <> nil) and FDataLink.UpdateAction(Action);
end;

end.

