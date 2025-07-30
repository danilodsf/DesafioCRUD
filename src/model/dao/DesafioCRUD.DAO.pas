unit DesafioCRUD.DAO;


interface

  uses System.SysUtils,
       FireDAC.Stan.Intf,
       FireDAC.Stan.Option,
       FireDAC.Stan.Error,
       FireDAC.UI.Intf,
       FireDAC.Phys.Intf,
       FireDAC.Stan.Def,
       FireDAC.Stan.Pool,
       FireDAC.Stan.Async,
       FireDAC.Phys,
       FireDAC.FMXUI.Wait,
       Data.DB,
       FireDAC.Comp.Client,
       FireDAC.Phys.FBDef,
       FireDAC.Phys.IBBase,
       FireDAC.DAPT,
       FireDAC.Phys.FB;

  type TDB = class
    private
      class var FInstance: TDB;
      class var FConexao: TFDConnection;
      class var FMsgErro: String;
    protected
      class function GetDefaultInstance: TDB;
    public
      class property MsgErro: String read FMsgErro write FMsgErro;
      class function GetQuery: TFDQuery;

      class function GetInstance: TDB;
      class destructor UnInitialize;
  end;


implementation

  uses IniFiles, DesafioCRUD.Utils.Lib;

{ TDB }

class function TDB.GetDefaultInstance: TDB;
var INI: TIniFile;
    IniFile, DBFile: String;
begin
  if not Assigned(FInstance) then
  begin
    FInstance := TDB.Create;
    FConexao  := TFDConnection.Create(nil);

    IniFile := TLib.GetIniPath;
    DBFile  := TLib.GetDBPath;

    INI := TIniFile.Create(IniFile);
    try
      try
        FConexao.Close;
        FConexao.Params.Clear;
        FConexao.Params.Add('Database='+ INI.ReadString('CONFIG-DB','DB_PATH', DBFile));
        FConexao.Params.Add('User_Name='+ INI.ReadString('CONFIG-DB','DB_USER', 'sysdba'));
        FConexao.Params.Add('Password='+ INI.ReadString('CONFIG-DB','DB_PASS', 'masterkey'));
        FConexao.Params.Add('Protocol=TCPIP');
        FConexao.Params.Add('Server='+ INI.ReadString('CONFIG-DB','DB_SERVER', 'localhost'));
        FConexao.Params.Add('DriverID=FB');
        FConexao.DriverName := 'FB';

        FConexao.Open;
      except
        on E: Exception do begin
          if Pos('Error while trying to open file',E.Message)>0 then
            FMsgErro := 'O banco de dados não foi localizado em: '+TLib.GetDBPath
          else if Pos('Your user name and password are not defined',E.Message)>0 then
            FMsgErro := 'Usuário ou senha do banco de dados inválida.';
        end;
      end;
    finally
      FreeAndNil(INI);
    end;
  end;

  Result := FInstance;
end;

class function TDB.GetInstance: TDB;
begin
  Result := TDB.GetDefaultInstance;
end;

class function TDB.GetQuery: TFDQuery;
begin
  Result := TFDQuery.Create(nil);
  Result.Connection := FConexao;
end;

class destructor TDB.UnInitialize;
begin
  if Assigned(FInstance) then
    FreeAndNil(FInstance);

  if Assigned(FConexao) then
    FreeAndNil(FConexao);
end;

end.
