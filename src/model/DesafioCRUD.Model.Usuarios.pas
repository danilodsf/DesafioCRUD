unit DesafioCRUD.Model.Usuarios;

interface

uses
  DesafioCRUD.DAO,
  Data.DB,
  FireDAC.Comp.Client,
  FireDAC.Comp.DataSet,
  FireDAC.Stan.Param,
  BCrypt;

type
  TModelUsuarios = class
  private
    class var FInstance: TModelUsuarios;
    class var FDB: TDB;
    class var FMemUsuarios: TFDMemTable;
    class var FMsgErro: String;
  protected
    class function GetDefaultInstance: TModelUsuarios;
  public
    class property MsgErro: String read FMsgErro write FMsgErro;
    class property MemUsuarios: TFDMemTable read FMemUsuarios;
    class function Consultar(id: Integer; var MsgErro: String): Boolean; overload;
    class function Consultar(var MsgErro: String): Boolean; overload;

    class function Atualizar(id: Integer; Nome, Login, Senha: String; var MsgErro: String): Boolean;
    class function Excluir(id: Integer; var MsgErro: String): Boolean;
    class function Cadastrar(Nome, Login, Senha: String; var MsgErro: String): Boolean;

    class function Login(Login, Senha: String; var MsgErro: String): Boolean;

    class function GetInstance: TModelUsuarios;
    class destructor UnInitialize;
  end;

implementation

uses System.SysUtils, DesafioCRUD.Utils.Lib;

{ TModelUsuarios }

class function TModelUsuarios.Atualizar(id: Integer; Nome, Login, Senha: String;
  var MsgErro: String): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('UPDATE USUARIOS ');
      Query.SQL.Add('SET NOME = :NOME');
      Query.SQL.Add(',LOGIN = :LOGIN');
      Query.SQL.Add(',SENHA = :SENHA');
      Query.SQL.Add('WHERE ID_USUARIO = :ID_USUARIO');

      Query.ParamByName('ID_USUARIO').AsInteger := id;
      Query.ParamByName('NOME').AsString := Nome;
      Query.ParamByName('LOGIN').AsString := Login;
      Query.ParamByName('SENHA').AsString := TBCrypt.HashPassword(Senha);

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

class function TModelUsuarios.Cadastrar(Nome, Login, Senha: String; var MsgErro: String): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT COUNT(1) FROM USUARIOS');
      Query.SQL.Add('WHERE LOGIN = :LOGIN');
      Query.ParamByName('LOGIN').AsString := Login;
      Query.Open;
      if Query.Fields[0].AsInteger = 0 then
      begin
        Query.Close;
        Query.SQL.Clear;
        Query.SQL.Add('INSERT INTO USUARIOS(NOME, LOGIN, SENHA) ');
        Query.SQL.Add('VALUES(:NOME, :LOGIN, :SENHA)');
        Query.SQL.Add('RETURNING ID_USUARIO');

        Query.ParamByName('NOME').AsString := Nome;
        Query.ParamByName('LOGIN').AsString := Login;
        Query.ParamByName('SENHA').AsString := TBCrypt.HashPassword(Senha);

        Query.Open;

        if (not Query.IsEmpty) then
          Consultar(Query.Fields[0].AsInteger, MsgErro);

        Result := True;
      end
      else
        MsgErro := 'Já existe um usuário cadastrado com este login.';
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelUsuarios.Consultar(var MsgErro: String): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_USUARIO, ID, NOME, LOGIN, SENHA ');
      Query.SQL.Add('FROM USUARIOS');
      Query.SQL.Add('ORDER BY ID_USUARIO');

      Query.Open;

      FMemUsuarios.Close;
      FMemUsuarios.CopyDataSet(Query, [coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query)
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelUsuarios.Consultar(id: Integer; var MsgErro: String): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_USUARIO, ID, NOME, LOGIN, SENHA ');
      Query.SQL.Add('FROM USUARIOS');
      Query.SQL.Add('WHERE ID_USUARIO = :ID_USUARIO');
      Query.ParamByName('ID_USUARIO').AsInteger := id;

      Query.Open;

      FMemUsuarios.Close;
      FMemUsuarios.CopyDataSet(Query, [coStructure, coRestart, coAppend]);

      Result := True
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelUsuarios.Login(Login, Senha: String; var MsgErro: String): Boolean;
var
  Query: TFDQuery;
  ValidouSenha: Boolean;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('SELECT ID_USUARIO, ID, NOME, LOGIN, SENHA ');
      Query.SQL.Add('FROM USUARIOS');
      Query.SQL.Add('WHERE LOGIN = :LOGIN');
      Query.ParamByName('LOGIN').AsString := Login;

      Query.Open;

      if (not Query.IsEmpty) then
      begin
        TBCrypt.CheckPassword(Senha, Query.FieldByName('SENHA').AsString, ValidouSenha);
        if ValidouSenha then
        begin
          FMemUsuarios.CopyDataSet(Query, [coStructure, coRestart, coAppend]);
          Result := True
        end
        else
          MsgErro := 'Usuário ou senha inválida!';
      end
      else
        MsgErro := 'Usuário ou senha inválida!';
    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
    begin
      if Pos('Error while trying to open file', E.Message) > 0 then
        FMsgErro := 'O banco de dados não foi localizado em: ' + TLib.GetDBPath
      else if Pos('Your user name and password are not defined', E.Message) > 0 then
        FMsgErro := 'Usuário ou senha do banco de dados inválida.';
    end;
  end;
end;

class function TModelUsuarios.Excluir(id: Integer; var MsgErro: String): Boolean;
var
  Query: TFDQuery;
begin
  Result := False;
  try
    Query := FDB.GetQuery;
    try
      Query.Close;
      Query.SQL.Clear;
      Query.SQL.Add('DELETE FROM USUARIOS ');
      Query.SQL.Add('WHERE ID_USUARIO = :ID_USUARIO');
      Query.ParamByName('ID_USUARIO').AsInteger := id;

      Query.ExecSQL;

      Result := True

    finally
      FreeAndNil(Query);
    end;
  except
    on E: Exception do
      MsgErro := E.Message;
  end;
end;

class function TModelUsuarios.GetDefaultInstance: TModelUsuarios;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TModelUsuarios.Create;

    FDB := TDB.GetInstance;

    if FDB.MsgErro <> '' then
      FMsgErro := FDB.MsgErro;

    FMemUsuarios := TFDMemTable.Create(nil);
  end;

  Result := FInstance;
end;

class function TModelUsuarios.GetInstance: TModelUsuarios;
begin
  Result := TModelUsuarios.GetDefaultInstance;
end;

class destructor TModelUsuarios.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FMemUsuarios) then
    FreeAndNil(FMemUsuarios);
end;

end.
