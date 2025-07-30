unit DesafioCRUD.Model.Produtos;

interface

uses
  DesafioCRUD.DAO,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Param;

  type TModelProdutos = class
    private
      class var FInstance: TModelProdutos;
      class var FDB: TDB;
      class var FMemProdutos: TFDMemTable;
    protected
      class function GetDefaultInstance: TModelProdutos;
    public
      class property MemProdutos: TFDMemTable read FMemProdutos;
      class function Consultar(id: Integer; var MsgErro: String): Boolean; overload;
      class function Consultar(var MsgErro: String): Boolean; overload;
      class function Consultar(Descricao: String; var MsgErro: String): Boolean; overload;
      class function Atualizar(ID: Integer; Descricao: String; Preco: Currency; IDCategoria: Integer; var MsgErro: String): Boolean;
      class function Excluir(id: Integer; var MsgErro: String): Boolean;
      class function Cadastrar(Descricao: String; Preco: Currency; IDCategoria: Integer;  var MsgErro: String): Boolean;
      class function Movimentar(id, Tipo, Quantidade, id_Usuario: Integer; var MsgErro: String): Boolean;

      class function ImprimirMovimentacao(id: Integer; var MsgErro: String): Boolean;

      class function GetInstance: TModelProdutos;
      class destructor UnInitialize;
  end;


implementation

uses System.SysUtils;

{ TModelProdutos }

class function TModelProdutos.Atualizar(ID: Integer; Descricao: String; Preco: Currency; IDCategoria: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('UPDATE PRODUTOS ');
      Query.SQL.Add('SET DESCRICAO = :DESCRICAO');
      Query.SQL.Add(', PRECO = :PRECO');
      Query.SQL.Add(', ID_CATEGORIA = :ID_CATEGORIA');
      Query.SQL.Add('WHERE ID_PRODUTO = :ID_PRODUTO');


      Query.ParamByName('ID_PRODUTO').AsInteger   := ID;
      Query.ParamByName('ID_CATEGORIA').AsInteger := IDCategoria;
      Query.ParamByName('DESCRICAO').AsString     := Descricao;
      Query.ParamByName('PRECO').AsFloat          := Preco;

      if IDCategoria > 0 then
        Query.ParamByName('ID_CATEGORIA').AsInteger  := IDCategoria
      else
        Query.ParamByName('ID_CATEGORIA').Clear;

      Query.ExecSQL;

      if (not Query.IsEmpty) then
        Consultar(Query.Fields[0].AsInteger, MsgErro);

      Result := True;
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelProdutos.Cadastrar(Descricao: String; Preco: Currency; IDCategoria: Integer;  var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT COUNT(1) FROM PRODUTOS');
      Query.SQL.Add('WHERE DESCRICAO = :DESCRICAO');
      Query.ParamByName('DESCRICAO').AsString := Descricao;
      Query.Open;
      if Query.Fields[0].AsInteger = 0 then begin
        Query.Close;
        Query.SQL.Clear;
        Query.SQL.Add('INSERT INTO PRODUTOS(DESCRICAO, PRECO, ID_CATEGORIA) ');
        Query.SQL.Add('VALUES(:DESCRICAO, :PRECO, :ID_CATEGORIA)');
        Query.SQL.Add('RETURNING ID_PRODUTO');


        Query.ParamByName('DESCRICAO').AsString      := Descricao;
        Query.ParamByName('PRECO').AsFloat           := Preco;

        if IDCategoria > 0 then
          Query.ParamByName('ID_CATEGORIA').AsInteger  := IDCategoria
        else
          Query.ParamByName('ID_CATEGORIA').Clear;

        Query.Open;

        if (not Query.IsEmpty) then
           Consultar(Query.Fields[0].AsInteger, MsgErro);

        Result := True;
      end
      else
        MsgErro := 'Já existe um produto cadastrado com esta descrição.';
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelProdutos.Consultar(var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT P.ID_PRODUTO, P.ID, P.DESCRICAO PRODUTO, P.PRECO, C.DESCRICAO CATEGORIA, C.ID_CATEGORIA,');
      Query.SQL.Add('(SELECT ESTOQUE FROM SP_RETORNAR_ESTOQUE(P.ID_PRODUTO)) AS ESTOQUE');
      Query.SQL.Add('FROM PRODUTOS P');
      Query.SQL.Add('LEFT JOIN CATEGORIAS C ON (C.ID_CATEGORIA = P.ID_CATEGORIA)');
      Query.SQL.Add('ORDER BY P.ID_PRODUTO');

      Query.Open;

      FMemProdutos.Close;
      FMemProdutos.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query)
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelProdutos.Consultar(id: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT P.ID_PRODUTO, P.ID, P.DESCRICAO PRODUTO, P.PRECO, C.DESCRICAO CATEGORIA, C.ID_CATEGORIA,');
      Query.SQL.Add('(SELECT ESTOQUE FROM SP_RETORNAR_ESTOQUE(P.ID_PRODUTO)) AS ESTOQUE');
      Query.SQL.Add('FROM PRODUTOS P');
      Query.SQL.Add('LEFT JOIN CATEGORIAS C ON (C.ID_CATEGORIA = P.ID_CATEGORIA)');
      Query.SQL.Add('WHERE P.ID_PRODUTO = :ID_PRODUTO');
      Query.ParamByName('ID_PRODUTO').AsInteger := id;

      Query.Open;

      FMemProdutos.Close;
      FMemProdutos.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;


class function TModelProdutos.Excluir(id: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('DELETE FROM PRODUTOS ');
      Query.SQL.Add('WHERE ID_PRODUTO = :ID_PRODUTO');
      Query.ParamByName('ID_PRODUTO').AsInteger := id;

      Query.ExecSQL;

      Result := True;
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelProdutos.GetDefaultInstance: TModelProdutos;
begin
  if not Assigned(FInstance) then
  begin
    FInstance    := TModelProdutos.Create;
    FDB          := TDB.GetInstance;
    FMemProdutos := TFDMemTable.Create(nil);
  end;

  Result := FInstance;
end;

class function TModelProdutos.GetInstance: TModelProdutos;
begin
  Result := TModelProdutos.GetDefaultInstance;
end;

class function TModelProdutos.ImprimirMovimentacao(id: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT P.DESCRICAO, U.NOME, M.QUANTIDADE, M.DATA_HORA, ');
      Query.SQL.Add('CASE WHEN M.TIPO_MOVIMENTACAO = 0 THEN ''Entrada''');
      Query.SQL.Add('WHEN M.TIPO_MOVIMENTACAO = 1 THEN ''Saída'' ');
      Query.SQL.Add('END AS "TIPO" ');
      Query.SQL.Add('FROM MOVIMENTACAO_ESTOQUE M');
      Query.SQL.Add('JOIN PRODUTOS P ON (P.ID_PRODUTO = M.ID_PRODUTO)');
      Query.SQL.Add('JOIN USUARIOS U ON (U.ID_USUARIO = M.ID_USUARIO)');
      Query.SQL.Add('WHERE M.ID_PRODUTO = :ID_PRODUTO');
      Query.SQL.Add('ORDER BY M.DATA_HORA');
      Query.ParamByName('ID_PRODUTO').AsInteger := id;

      Query.Open;

      FMemProdutos.Close;
      FMemProdutos.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True;
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelProdutos.Movimentar(id, Tipo, Quantidade, id_Usuario: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('EXECUTE PROCEDURE SP_MOVIMENTAR_ESTOQUE(:ID_PRODUTO, :ID_USUARIO, :QUANTIDADE, :TIPO_MOVIMENTACAO) ');
      Query.ParamByName('ID_PRODUTO').AsInteger := id;
      Query.ParamByName('ID_USUARIO').AsInteger := id_Usuario;
      Query.ParamByName('QUANTIDADE').AsInteger := Quantidade;
      Query.ParamByName('TIPO_MOVIMENTACAO').AsInteger := Tipo;

      Query.ExecSQL;

      Result := True;
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class destructor TModelProdutos.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FMemProdutos) then
    FreeAndNil(FMemProdutos);
end;

class function TModelProdutos.Consultar(Descricao: String; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT P.ID_PRODUTO, P.ID, P.DESCRICAO PRODUTO, P.PRECO, C.DESCRICAO CATEGORIA, C.ID_CATEGORIA,');
      Query.SQL.Add('(SELECT ESTOQUE FROM SP_RETORNAR_ESTOQUE(P.ID_PRODUTO)) AS ESTOQUE');
      Query.SQL.Add('FROM PRODUTOS P');
      Query.SQL.Add('LEFT JOIN CATEGORIAS C ON (C.ID_CATEGORIA = P.ID_CATEGORIA)');
      Query.SQL.Add('WHERE Upper(P.DESCRICAO) CONTAINING :DESCRICAO');
      Query.ParamByName('DESCRICAO').AsString := Descricao;

      Query.Open;

      FMemProdutos.Close;
      FMemProdutos.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

end.
