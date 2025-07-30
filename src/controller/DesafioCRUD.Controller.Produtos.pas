unit DesafioCRUD.Controller.Produtos;

interface

  uses
    DesafioCRUD.Model.Produtos, System.Generics.Collections,
    FMX.frxClass, FMX.frxDBSet;

  type TProduto = record
    ID_Interno: Integer;
    IDCategoria: Integer;
    ID: Integer;
    Descricao: String;
    Categoria: String;
    Preco: Currency;
    Estoque: Integer;
  end;

  type TProdutos = class
    private
      class var FProdutos: TDictionary<integer, TProduto>;
      class var FProduto: TProduto;
      class var FMsgErro: String;
      class var FInstance: TProdutos;
      class var FModelProdutos: TModelProdutos;
      class function ValidarCampo(Campo, Valor: String): Boolean;
    protected
      class function GetDefaultInstance: TProdutos;
    public
      class property Produtos: TDictionary<integer, TProduto> read FProdutos write FProdutos;
      class property MsgErro: String read FMsgErro;

      class function Consultar(id: Integer): Boolean; overload;
      class function Consultar: Boolean; overload;
      class function Consultar(Descricao: String): Boolean; overload;

      class function Atualizar(id: Integer): Boolean;
      class function Excluir(id: Integer): Boolean;
      class function Cadastrar: Boolean;

      class function Movimentar(id, Tipo, Quantidade, id_Usuario: Integer): Boolean;

      class function ImprimirMovimentacao(id: Integer): Boolean;

      class function GetInstance: TProdutos;
      class destructor UnInitialize;
  end;

implementation

uses
  System.SysUtils, Data.DB, DesafioCRUD.Utils.Lib;

{ TProdutos }

class function TProdutos.ValidarCampo(Campo, Valor: String): Boolean;
begin
  Result := True;
  if ((Valor = '')or(Valor = '0')) then begin
    FMsgErro := 'É necessário preencher o campo '+Campo+' para continuar.';
    Result := False;
  end
end;

class function TProdutos.Atualizar(id: Integer): Boolean;
begin
  Result := False;
  FProduto := Produtos.Items[0];

  if ValidarCampo('Descrição', FProduto.Descricao) and
     ValidarCampo('Preço',FloatToStr(FProduto.Preco)) then

    Result := FModelProdutos.Atualizar(id, FProduto.Descricao, FProduto.Preco, FProduto.IDCategoria, FMsgErro);
end;

class function TProdutos.Cadastrar: Boolean;
begin
  Result := False;
  FProduto := Produtos.Items[0];

  if ValidarCampo('Descrição',FProduto.Descricao) and
     ValidarCampo('Preço',FloatToStr(FProduto.Preco)) then

    Result := FModelProdutos.Cadastrar(FProduto.Descricao, FProduto.Preco, FProduto.IDCategoria, FMsgErro);
end;

class function TProdutos.Consultar(id: Integer): Boolean;
begin
  FProdutos.Clear;
  Result := FModelProdutos.Consultar(id, FMsgErro);
  if Result then begin
    if (not FModelProdutos.MemProdutos.IsEmpty) then begin
      FProduto.ID_Interno  := FModelProdutos.MemProdutos.FieldByName('ID_PRODUTO').AsInteger;
      FProduto.ID          := FModelProdutos.MemProdutos.FieldByName('ID').AsInteger;
      FProduto.Descricao   := FModelProdutos.MemProdutos.FieldByName('PRODUTO').AsString;
      FProduto.Preco       := FModelProdutos.MemProdutos.FieldByName('PRECO').AsFloat;
      FProduto.Estoque     := FModelProdutos.MemProdutos.FieldByName('ESTOQUE').AsInteger;

      FProduto.IDCategoria := FModelProdutos.MemProdutos.FieldByName('ID_CATEGORIA').AsInteger;
      FProduto.Categoria   := FModelProdutos.MemProdutos.FieldByName('CATEGORIA').AsString;

      FProdutos.Add(Pred(FModelProdutos.MemProdutos.RecNo), FProduto);
    end
    else begin
      Result := False;
      FMsgErro := 'Nenhuma categoria encontrado com este ID';
    end;
  end;
end;

class function TProdutos.Consultar(Descricao: String): Boolean;
begin
  FProdutos.Clear;
  Result := FModelProdutos.Consultar(Descricao, FMsgErro);
  if Result then begin
    if (not FModelProdutos.MemProdutos.IsEmpty) then begin
      FProduto.ID_Interno  := FModelProdutos.MemProdutos.FieldByName('ID_PRODUTO').AsInteger;
      FProduto.ID          := FModelProdutos.MemProdutos.FieldByName('ID').AsInteger;
      FProduto.Descricao   := FModelProdutos.MemProdutos.FieldByName('PRODUTO').AsString;
      FProduto.Preco       := FModelProdutos.MemProdutos.FieldByName('PRECO').AsFloat;
      FProduto.Estoque     := FModelProdutos.MemProdutos.FieldByName('ESTOQUE').AsInteger;

      FProduto.IDCategoria := FModelProdutos.MemProdutos.FieldByName('ID_CATEGORIA').AsInteger;
      FProduto.Categoria   := FModelProdutos.MemProdutos.FieldByName('CATEGORIA').AsString;

      FProdutos.Add(Pred(FModelProdutos.MemProdutos.RecNo), FProduto);
    end
    else begin
      Result := False;
      FMsgErro := 'Nenhuma categoria encontrado com este ID';
    end;
  end;
end;

class function TProdutos.Consultar: Boolean;
begin
  FProdutos.Clear;
  Result := FModelProdutos.Consultar(FMsgErro);
  if Result then begin
    while not FModelProdutos.MemProdutos.Eof do begin
      FProduto.ID_Interno  := FModelProdutos.MemProdutos.FieldByName('ID_PRODUTO').AsInteger;
      FProduto.ID          := FModelProdutos.MemProdutos.FieldByName('ID').AsInteger;
      FProduto.Descricao   := FModelProdutos.MemProdutos.FieldByName('PRODUTO').AsString;
      FProduto.Preco       := FModelProdutos.MemProdutos.FieldByName('PRECO').AsFloat;
      FProduto.Estoque     := FModelProdutos.MemProdutos.FieldByName('ESTOQUE').AsInteger;

      FProduto.IDCategoria := FModelProdutos.MemProdutos.FieldByName('ID_CATEGORIA').AsInteger;
      FProduto.Categoria   := FModelProdutos.MemProdutos.FieldByName('CATEGORIA').AsString;

      FProdutos.Add(Pred(FModelProdutos.MemProdutos.RecNo), FProduto);

      FModelProdutos.MemProdutos.Next;
    end;
  end;
end;

class function TProdutos.Excluir(id: Integer): Boolean;
begin
  Result := FModelProdutos.Excluir(id, FMsgErro);
end;

class function TProdutos.GetDefaultInstance: TProdutos;
begin
  if not Assigned(FInstance) then
  begin
    FInstance       := TProdutos.Create;
    FModelProdutos  := TModelProdutos.GetInstance;
    FProdutos       := TDictionary<integer, TProduto>.Create;
  end;

  Result := FInstance;
end;

class function TProdutos.GetInstance: TProdutos;
begin
  Result := TProdutos.GetDefaultInstance;
end;

class function TProdutos.ImprimirMovimentacao(id: Integer): Boolean;
var Report: TfrxReport;
    DataSet: TfrxDBDataset;
    ArqRelatorio: String;
begin
  Result := False;
  if FModelProdutos.ImprimirMovimentacao(id,FMsgErro) then begin
    if (not FModelProdutos.MemProdutos.IsEmpty) then begin
      Report := TfrxReport.Create(nil);
      DataSet := TfrxDBDataset.Create(nil);
      try
        try
          DataSet.DataSet := FModelProdutos.MemProdutos;
          DataSet.Name    := 'frxDBDataSet1';

          ArqRelatorio := TLib.GetRelMovimentacaoPath;
          Report.PrintOptions.ShowDialog := False;
          Report.LoadFromFile(ArqRelatorio);
          Report.PrepareReport;
          Report.ShowPreparedReport;
          Result := True;
        except
          On E: Exception do
            FMsgErro := 'Não foi possível gerar o relatório'+#13#10+E.Message;
        end;
      finally
        FreeAndNil(DataSet);
        FreeAndNil(Report);
      end;
    end
    else
      FMsgErro := 'Não existe movimentação para o produto selecionado';
  end;
end;

class function TProdutos.Movimentar(id, Tipo, Quantidade, id_Usuario: Integer): Boolean;
begin
  Result := False;

  if ValidarCampo('Produto', id.ToString) and
     ValidarCampo('Quantidade',Quantidade.ToString) then
    Result := FModelProdutos.Movimentar(id, Tipo, Quantidade, id_Usuario, FMsgErro);
end;

class destructor TProdutos.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FProdutos) then
    FreeAndNil(FProdutos);
end;

end.
