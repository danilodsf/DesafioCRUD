unit DesafioCRUD.Controller.Categorias;

interface

  uses
    DesafioCRUD.Model.Categorias, System.Generics.Collections;

  type TCategoria = record
    ID_Interno: Integer;
    ID: Integer;
    Descricao: String;
  end;

  type TCategorias = class
    private
      class var FCategorias: TDictionary<integer, TCategoria>;
      class var FCategoria: TCategoria;
      class var FMsgErro: String;
      class var FInstance: TCategorias;
      class var FModelCategorias: TModelCategorias;
      class function ValidarCampo(Campo, Valor: String): Boolean;
    protected
      class function GetDefaultInstance: TCategorias;
    public
      class property Categorias: TDictionary<integer, TCategoria> read FCategorias write FCategorias;
      class property MsgErro: String read FMsgErro;

      class function Consultar(id: Integer): Boolean; overload;
      class function Consultar: Boolean; overload;
      class function Consultar(Descricao: String): Boolean; overload;

      class function Atualizar(id: Integer): Boolean;
      class function Excluir(id: Integer): Boolean;
      class function Cadastrar: Boolean;

      class function GetInstance: TCategorias;
      class destructor UnInitialize;
  end;

implementation

uses
  System.SysUtils, Data.DB;

{ TCategorias }

class function TCategorias.ValidarCampo(Campo, Valor: String): Boolean;
begin
  Result := True;
  if Valor = '' then begin
    FMsgErro := 'É necessário preencher o campo '+Campo+' para continuar.';
    Result := False;
  end
end;

class function TCategorias.Atualizar(id: Integer): Boolean;
begin
  Result := False;
  FCategoria := Categorias.Items[0];

  if ValidarCampo('Descrição',FCategoria.Descricao) then
    Result := FModelCategorias.Atualizar(id, FCategoria.Descricao, FMsgErro);
end;

class function TCategorias.Cadastrar: Boolean;
begin
  Result := False;
  FCategoria := Categorias.Items[0];

  if ValidarCampo('Descrição', FCategoria.Descricao) then
    Result := FModelCategorias.Cadastrar(FCategoria.Descricao, FMsgErro);
end;

class function TCategorias.Consultar(Descricao: String): Boolean;
begin
  FCategorias.Clear;
  Result := FModelCategorias.Consultar(Descricao, FMsgErro);
  if Result then begin
    if (not FModelCategorias.MemCategorias.IsEmpty) then begin
      FCategoria.ID_Interno := FModelCategorias.MemCategorias.FieldByName('ID_CATEGORIA').AsInteger;
      FCategoria.ID         := FModelCategorias.MemCategorias.FieldByName('ID').AsInteger;
      FCategoria.Descricao  := FModelCategorias.MemCategorias.FieldByName('DESCRICAO').AsString;

      FCategorias.Add(Pred(FModelCategorias.MemCategorias.RecNo), FCategoria);
    end
    else begin
      Result := False;
      FMsgErro := 'Nenhuma categoria encontrado com esta descrição';
    end;
  end;
end;

class function TCategorias.Consultar(id: Integer): Boolean;
begin
  FCategorias.Clear;
  Result := FModelCategorias.Consultar(id, FMsgErro);
  if Result then begin
    if (not FModelCategorias.MemCategorias.IsEmpty) then begin
      FCategoria.ID_Interno := FModelCategorias.MemCategorias.FieldByName('ID_CATEGORIA').AsInteger;
      FCategoria.ID         := FModelCategorias.MemCategorias.FieldByName('ID').AsInteger;
      FCategoria.Descricao  := FModelCategorias.MemCategorias.FieldByName('DESCRICAO').AsString;

      FCategorias.Add(Pred(FModelCategorias.MemCategorias.RecNo), FCategoria);
    end
    else begin
      Result := False;
      FMsgErro := 'Nenhuma categoria encontrado com este ID';
    end;
  end;
end;

class function TCategorias.Consultar: Boolean;
begin
  FCategorias.Clear;
  Result := FModelCategorias.Consultar(FMsgErro);
  if Result then begin
    while not FModelCategorias.MemCategorias.Eof do begin
      FCategoria.ID_Interno := FModelCategorias.MemCategorias.FieldByName('ID_CATEGORIA').AsInteger;
      FCategoria.ID         := FModelCategorias.MemCategorias.FieldByName('ID').AsInteger;
      FCategoria.Descricao  := FModelCategorias.MemCategorias.FieldByName('DESCRICAO').AsString;

      FCategorias.Add(Pred(FModelCategorias.MemCategorias.RecNo), FCategoria);

      FModelCategorias.MemCategorias.Next;
    end;
  end;
end;

class function TCategorias.Excluir(id: Integer): Boolean;
begin
  Result := FModelCategorias.Excluir(id, FMsgErro);
end;

class function TCategorias.GetDefaultInstance: TCategorias;
begin
  if not Assigned(FInstance) then
  begin
    FInstance       := TCategorias.Create;
    FModelCategorias  := TModelCategorias.GetInstance;
    FCategorias       := TDictionary<integer, TCategoria>.Create;
  end;

  Result := FInstance;
end;

class function TCategorias.GetInstance: TCategorias;
begin
  Result := TCategorias.GetDefaultInstance;
end;

class destructor TCategorias.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FCategorias) then
    FreeAndNil(FCategorias);
end;

end.
