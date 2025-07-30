program DesafioCRUD;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  DesafioCRUD.Utils.Lib in 'model\utils\DesafioCRUD.Utils.Lib.pas',
  DesafioCRUD.Controller.Usuarios in 'controller\DesafioCRUD.Controller.Usuarios.pas',
  DesafioCRUD.Model.Usuarios in 'model\DesafioCRUD.Model.Usuarios.pas',
  DesafioCRUD.Frame.Usuario in 'view\frames\DesafioCRUD.Frame.Usuario.pas' {FrameUsuario: TFrame},
  DesafioCRUD.Model.Categorias in 'model\DesafioCRUD.Model.Categorias.pas',
  DesafioCRUD.Controller.Categorias in 'controller\DesafioCRUD.Controller.Categorias.pas',
  DesafioCRUD.DAO in 'model\dao\DesafioCRUD.DAO.pas',
  DesafioCRUD.Model.Produtos in 'model\DesafioCRUD.Model.Produtos.pas',
  DesafioCRUD.Controller.Produtos in 'controller\DesafioCRUD.Controller.Produtos.pas',
  DesafioCRUD.View.Principal in 'view\DesafioCRUD.View.Principal.pas' {FormMain},
  DesafioCRUD.View.Logo in 'view\DesafioCRUD.View.Logo.pas' {FormLogo},
  DesafioCRUD.View.CadBase in 'view\DesafioCRUD.View.CadBase.pas' {FormCadBase},
  DesafioCRUD.View.CadCategorias in 'view\DesafioCRUD.View.CadCategorias.pas' {FormCadCategoria},
  DesafioCRUD.View.Movimentacao in 'view\DesafioCRUD.View.Movimentacao.pas' {FormMovimentacao},
  DesafioCRUD.View.Relatorios in 'view\DesafioCRUD.View.Relatorios.pas' {FormRelatorios},
  DesafioCRUD.Frame.Categoria in 'view\frames\DesafioCRUD.Frame.Categoria.pas' {FrameCategoria: TFrame},
  DesafioCRUD.View.CadProdutos in 'view\DesafioCRUD.View.CadProdutos.pas' {FormCadProdutos},
  DesafioCRUD.View.CadUsuarios in 'view\DesafioCRUD.View.CadUsuarios.pas' {FormCadUsuarios},
  DesafioCRUD.Frame.Produto in 'view\frames\DesafioCRUD.Frame.Produto.pas' {FrameProduto: TFrame},
  Globals in 'model\utils\Globals.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
