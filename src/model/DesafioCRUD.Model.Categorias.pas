unit DesafioCRUD.Model.Categorias;

interface

uses
  DesafioCRUD.DAO,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Param;

  type TModelCategorias = class
    private
      class var FInstance: TModelCategorias;
      class var FDB: TDB;
      class var FMemCategorias: TFDMemTable;
    protected
      class function GetDefaultInstance: TModelCategorias;
    public
      class property MemCategorias: TFDMemTable read FMemCategorias;
      class function Consultar(id: Integer; var MsgErro: String): Boolean; overload;
      class function Consultar(var MsgErro: String): Boolean; overload;
      class function Consultar(Descricao: String; var MsgErro: String): Boolean; overload;

      class function Atualizar(ID: Integer; Descricao: String; var MsgErro: String): Boolean;
      class function Excluir(id: Integer; var MsgErro: String): Boolean;
      class function Cadastrar(Descricao: String; var MsgErro: String): Boolean;

      class function GetInstance: TModelCategorias;
      class destructor UnInitialize;
  end;


implementation

uses System.SysUtils;

{ TModelCategorias }

class function TModelCategorias.Atualizar(ID: Integer; Descricao: String; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('UPDATE CATEGORIAS ');
      Query.SQL.Add('SET DESCRICAO = :DESCRICAO');
      Query.SQL.Add('WHERE ID_CATEGORIA = :ID_CATEGORIA');


      Query.ParamByName('ID_CATEGORIA').AsInteger := ID;
      Query.ParamByName('DESCRICAO').AsString     := Descricao;

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

class function TModelCategorias.Cadastrar(Descricao: String; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT COUNT(1) FROM CATEGORIAS');
      Query.SQL.Add('WHERE DESCRICAO = :DESCRICAO');
      Query.ParamByName('DESCRICAO').AsString := Descricao;
      Query.Open;
      if Query.Fields[0].AsInteger = 0 then begin
        Query.Close;
        Query.SQL.Clear;
        Query.SQL.Add('INSERT INTO CATEGORIAS(DESCRICAO) ');
        Query.SQL.Add('VALUES(:DESCRICAO)');
        Query.SQL.Add('RETURNING ID_CATEGORIA');


        Query.ParamByName('DESCRICAO').AsString  := Descricao;

        Query.Open;

        if (not Query.IsEmpty) then
           Consultar(Query.Fields[0].AsInteger, MsgErro);

        Result := True;
      end
      else
        MsgErro := 'Já existe uma categoria cadastrado com esta descrição.';
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelCategorias.Consultar(var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_CATEGORIA, ID, DESCRICAO ');
      Query.SQL.Add('FROM CATEGORIAS');
      Query.SQL.Add('ORDER BY ID_CATEGORIA');

      Query.Open;

      FMemCategorias.Close;
      FMemCategorias.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query)
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelCategorias.Consultar(id: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_CATEGORIA, ID, DESCRICAO ');
      Query.SQL.Add('FROM CATEGORIAS');
      Query.SQL.Add('WHERE ID_CATEGORIA = :ID_CATEGORIA');
      Query.ParamByName('ID_CATEGORIA').AsInteger := id;

      Query.Open;

      FMemCategorias.Close;
      FMemCategorias.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelCategorias.Consultar(Descricao: String; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_CATEGORIA, ID, DESCRICAO ');
      Query.SQL.Add('FROM CATEGORIAS');
      Query.SQL.Add('WHERE Upper(DESCRICAO) CONTAINING :DESCRICAO ');
      Query.ParamByName('DESCRICAO').AsString := UpperCase(Descricao);

      Query.Open;

      FMemCategorias.Close;
      FMemCategorias.CopyDataSet(Query,[coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelCategorias.Excluir(id: Integer; var MsgErro: String): Boolean;
var Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('DELETE FROM CATEGORIAS ');
      Query.SQL.Add('WHERE ID_CATEGORIA = :ID_CATEGORIA');
      Query.ParamByName('ID_CATEGORIA').AsInteger := id;

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

class function TModelCategorias.GetDefaultInstance: TModelCategorias;
begin
  if not Assigned(FInstance) then
  begin
    FInstance    := TModelCategorias.Create;
    FDB          := TDB.GetInstance;
    FMemCategorias := TFDMemTable.Create(nil);
  end;

  Result := FInstance;
end;

class function TModelCategorias.GetInstance: TModelCategorias;
begin
  Result := TModelCategorias.GetDefaultInstance;
end;

class destructor TModelCategorias.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FMemCategorias) then
    FreeAndNil(FMemCategorias);
end;



end.
