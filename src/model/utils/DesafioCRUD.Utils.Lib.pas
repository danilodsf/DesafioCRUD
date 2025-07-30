unit DesafioCRUD.Utils.Lib;

interface

  type TLib = class
    public
      class function GetDBPath: String;
      class function GetRelMovimentacaoPath: String;
      class function GetIniPath: String;
  end;

implementation

uses
  System.SysUtils;

{ TLib }

class function TLib.GetDBPath: String;
begin
  { Pego a pasta atual, volto duas pastas e em seguida abro a pasta db }

  Result := ExcludeTrailingPathDelimiter(GetCurrentDir);
  Result := ExtractFilePath(Result);
  Result := ExcludeTrailingPathDelimiter(Result);
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Result))+'db\db.fdb';
end;

class function TLib.GetIniPath: String;
begin
  Result := IncludeTrailingPathDelimiter(GetCurrentDir);
  Result := Result + 'CONFIG.ini';
end;

class function TLib.GetRelMovimentacaoPath: String;
begin
  { Pego a pasta atual, volto duas pastas e em seguida abro a pasta db }

  Result := ExcludeTrailingPathDelimiter(GetCurrentDir);
  Result := ExtractFilePath(Result);
  Result := ExcludeTrailingPathDelimiter(Result);
  Result := IncludeTrailingPathDelimiter(ExtractFilePath(Result))+'reports\movimentacao_estoque.fr3';
end;

end.
