program DesafioCRUD;

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Skia,
  DesafioCRUD.Utils.Lib in 'src\model\utils\DesafioCRUD.Utils.Lib.pas',
  DesafioCRUD.Controller.Usuarios in 'src\controller\DesafioCRUD.Controller.Usuarios.pas',
  DesafioCRUD.Model.Usuarios in 'src\model\DesafioCRUD.Model.Usuarios.pas',
  DesafioCRUD.Frame.Usuario in 'src\view\frames\DesafioCRUD.Frame.Usuario.pas' {FrameUsuario: TFrame},
  DesafioCRUD.Model.Categorias in 'src\model\DesafioCRUD.Model.Categorias.pas',
  DesafioCRUD.Controller.Categorias in 'src\controller\DesafioCRUD.Controller.Categorias.pas',
  DesafioCRUD.DAO in 'src\model\dao\DesafioCRUD.DAO.pas',
  DesafioCRUD.Model.Produtos in 'src\model\DesafioCRUD.Model.Produtos.pas',
  DesafioCRUD.Controller.Produtos in 'src\controller\DesafioCRUD.Controller.Produtos.pas',
  DesafioCRUD.View.Principal in 'src\view\DesafioCRUD.View.Principal.pas' {FormMain},
  DesafioCRUD.View.Logo in 'src\view\DesafioCRUD.View.Logo.pas' {FormLogo},
  DesafioCRUD.View.CadBase in 'src\view\DesafioCRUD.View.CadBase.pas' {FormCadBase},
  DesafioCRUD.View.CadCategorias in 'src\view\DesafioCRUD.View.CadCategorias.pas' {FormCadCategoria},
  DesafioCRUD.View.Movimentacao in 'src\view\DesafioCRUD.View.Movimentacao.pas' {FormMovimentacao},
  DesafioCRUD.View.Relatorios in 'src\view\DesafioCRUD.View.Relatorios.pas' {FormRelatorios},
  DesafioCRUD.Frame.Categoria in 'src\view\frames\DesafioCRUD.Frame.Categoria.pas' {FrameCategoria: TFrame},
  DesafioCRUD.View.CadProdutos in 'src\view\DesafioCRUD.View.CadProdutos.pas' {FormCadProdutos},
  DesafioCRUD.View.CadUsuarios in 'src\view\DesafioCRUD.View.CadUsuarios.pas' {FormCadUsuarios},
  DesafioCRUD.Frame.Produto in 'src\view\frames\DesafioCRUD.Frame.Produto.pas' {FrameProduto: TFrame},
  Globals in 'src\model\utils\Globals.pas';

{$R *.res}

begin
  GlobalUseSkia := True;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
