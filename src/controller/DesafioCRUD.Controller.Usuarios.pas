unit DesafioCRUD.Controller.Usuarios;

interface

  uses
    DesafioCRUD.Model.Usuarios, System.Generics.Collections, System.IniFiles;

  type TUsuario = record
    ID_Interno: Integer;
    ID: Integer;
    Nome: String;
    Senha: String;
    Login: String;
  end;

  type TUsuarios = class
    private
      class var FUsuarios: TDictionary<integer, TUsuario>;
      class var FUsuario: TUsuario;
      class var FMsgErro: String;
      class var FInstance: TUsuarios;
      class var FModelUsuarios: TModelUsuarios;
      class function ValidarCampo(Campo, Valor: String): Boolean;
      class procedure GravarLogin; static;
    protected
      class function GetDefaultInstance: TUsuarios;
    public
      class property Usuarios: TDictionary<integer, TUsuario> read FUsuarios write FUsuarios;
      class property MsgErro: String read FMsgErro;

      class function Consultar(id: Integer): Boolean; overload;
      class function Consultar: Boolean; overload;

      class function Atualizar(id: Integer): Boolean;
      class function Excluir(id: Integer): Boolean;
      class function Cadastrar: Boolean;

      class function DoLogin: Boolean;
      class procedure LerLogin;
      class function GetInstance: TUsuarios;
      class destructor UnInitialize;
  end;

implementation

uses
  System.SysUtils, Data.DB, DesafioCRUD.Utils.Lib;

{ TUsuarios }

class function TUsuarios.ValidarCampo(Campo, Valor: String): Boolean;
begin
  Result := True;
  if Valor = '' then begin
    FMsgErro := 'É necessário preencher o campo '+Campo+' para continuar.';
    Result := False;
  end
end;

class function TUsuarios.Atualizar(id: Integer): Boolean;
begin
  Result := False;

  FUsuario := Usuarios.Items[0];

  if ValidarCampo('Nome', FUsuario.Nome) and
     ValidarCampo('Login', FUsuario.Login) and
     ValidarCampo('Senha', FUsuario.Senha) then

    Result := FModelUsuarios.Atualizar(id, FUsuario.Nome, FUsuario.Login, FUsuario.Senha, FMsgErro);
end;

class function TUsuarios.Cadastrar: Boolean;
begin
  Result := False;

  FUsuario := Usuarios.Items[0];

  if ValidarCampo('Nome', FUsuario.Nome) and
     ValidarCampo('Login', FUsuario.Login) and
     ValidarCampo('Senha', FUsuario.Senha) then

    Result := FModelUsuarios.Cadastrar(FUsuario.Nome, FUsuario.Login, FUsuario.Senha, FMsgErro);
end;

class function TUsuarios.Consultar(id: Integer): Boolean;
begin
  FUsuarios.Clear;
  Result := FModelUsuarios.Consultar(id, FMsgErro);
  if Result then begin
    if (not FModelUsuarios.MemUsuarios.IsEmpty) then begin
      FUsuario.ID_Interno := FModelUsuarios.MemUsuarios.FieldByName('ID_USUARIO').AsInteger;
      FUsuario.ID         := FModelUsuarios.MemUsuarios.FieldByName('ID').AsInteger;
      FUsuario.Nome       := FModelUsuarios.MemUsuarios.FieldByName('NOME').AsString;
      FUsuario.Login      := FModelUsuarios.MemUsuarios.FieldByName('LOGIN').AsString;
      FUsuario.Senha      := '';

      FUsuarios.Add(Pred(FModelUsuarios.MemUsuarios.RecNo), FUsuario);
    end
    else begin
      Result := False;
      FMsgErro := 'Nenhum usuário encontrado com este ID';
    end;
  end;
end;

class function TUsuarios.Consultar: Boolean;
begin
  FUsuarios.Clear;
  Result := FModelUsuarios.Consultar(FMsgErro);
  if Result then begin
    while not FModelUsuarios.MemUsuarios.Eof do begin
      FUsuario.ID_Interno := FModelUsuarios.MemUsuarios.FieldByName('ID_USUARIO').AsInteger;
      FUsuario.ID         := FModelUsuarios.MemUsuarios.FieldByName('ID').AsInteger;
      FUsuario.Nome       := FModelUsuarios.MemUsuarios.FieldByName('NOME').AsString;
      FUsuario.Login      := FModelUsuarios.MemUsuarios.FieldByName('LOGIN').AsString;
      FUsuario.Senha      := '';

      FUsuarios.Add(Pred(FModelUsuarios.MemUsuarios.RecNo), FUsuario);

      FModelUsuarios.MemUsuarios.Next;
    end;
  end;
end;

class procedure TUsuarios.GravarLogin;
var INI: TIniFile;
begin
  Ini := TIniFile.Create(TLib.GetIniPath);
  try
    INI.WriteString('LOGIN','Login',FUsuario.Login);
  finally
    Ini.Free;
  end;
end;

class procedure TUsuarios.LerLogin;
var INI: TIniFile;
begin
  Ini := TIniFile.Create(TLib.GetIniPath);
  try
    FUsuario.Login  := INI.ReadString('LOGIN','Login','');
    FUsuario.Senha  := INI.ReadString('LOGIN','Senha','');

    Usuarios.Add(0,FUsuario);
  finally
    Ini.Free;
  end;
end;

class function TUsuarios.DoLogin: Boolean;
begin
  Result := False;

  FUsuario := Usuarios.Items[0];

  if ValidarCampo('Login', FUsuario.Login) and
     ValidarCampo('Senha', FUsuario.Senha) then begin

    Result := FModelUsuarios.Login(FUsuario.Login, FUsuario.Senha, FMsgErro);
    if Result then begin
      if (not FModelUsuarios.MemUsuarios.IsEmpty) then begin
        FUsuarios.Clear;
        FUsuario.ID_Interno := FModelUsuarios.MemUsuarios.FieldByName('ID_USUARIO').AsInteger;
        FUsuario.ID         := FModelUsuarios.MemUsuarios.FieldByName('ID').AsInteger;
        FUsuario.Nome       := FModelUsuarios.MemUsuarios.FieldByName('NOME').AsString;
        FUsuario.Login      := FModelUsuarios.MemUsuarios.FieldByName('LOGIN').AsString;
        FUsuario.Senha      := FModelUsuarios.MemUsuarios.FieldByName('SENHA').AsString;

        GravarLogin;

        FUsuarios.Add(Pred(FModelUsuarios.MemUsuarios.RecNo), FUsuario);
      end
    end;
  end;
end;

class function TUsuarios.Excluir(id: Integer): Boolean;
begin
  Result := FModelUsuarios.Excluir(id, FMsgErro);
end;

class function TUsuarios.GetDefaultInstance: TUsuarios;
begin
  if not Assigned(FInstance) then
  begin
    FInstance       := TUsuarios.Create;
    FModelUsuarios  := TModelUsuarios.GetInstance;

    if FModelUsuarios.MsgErro<>'' then
      FMsgErro := FModelUsuarios.MsgErro;

    FUsuarios       := TDictionary<integer, TUsuario>.Create;
  end;

  Result := FInstance;
end;

class function TUsuarios.GetInstance: TUsuarios;
begin
  Result := TUsuarios.GetDefaultInstance;
end;

class destructor TUsuarios.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FUsuarios) then
    FreeAndNil(FUsuarios);
end;

end.
