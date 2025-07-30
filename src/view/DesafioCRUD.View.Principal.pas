unit DesafioCRUD.View.Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, System.Skia, FMX.Skia, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Edit, FMX.TabControl, FMX.Ani, Globals;

type TOpMenu = (omVazio, omInicio, omCategoria, omProdutos, omMovimentacao, omRelatorios);

type
  TFormMain = class(TForm)
    lytPrincipal: TLayout;
    rctPrincipal: TRectangle;
    Layout2: TLayout;
    Label3: TLabel;
    lytModulos: TLayout;
    lblCategorias: TLabel;
    btnCategorias: TRectangle;
    btnProdutos: TRectangle;
    lblProdutos: TLabel;
    btnMovimentacao: TRectangle;
    lblMovimentacao: TLabel;
    btnRelatorios: TRectangle;
    lblRelatorios: TLabel;
    TabMain: TTabControl;
    tbContent: TTabItem;
    lbUsuarioLogado: TLabel;
    Rectangle3: TRectangle;
    btnInicio: TRectangle;
    lblInicio: TLabel;
    rctContainerMenu: TRectangle;
    lytLogoMin: TLayout;
    Layout1: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Rectangle1: TRectangle;
    edtLogin: TEdit;
    Rectangle2: TRectangle;
    edtSenha: TEdit;
    Layout3: TLayout;
    btnLogin: TRectangle;
    Label8: TLabel;
    Layout4: TLayout;
    Layout5: TLayout;
    Label4: TLabel;
    Layout6: TLayout;
    btnCadastrar: TRectangle;
    lblCadastro: TLabel;
    Label5: TLabel;
    procedure FormShow(Sender: TObject);
    procedure btnCategoriasMouseEnter(Sender: TObject);
    procedure btnCategoriasMouseLeave(Sender: TObject);
    procedure btnLoginMouseEnter(Sender: TObject);
    procedure btnLoginMouseLeave(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCategoriasClick(Sender: TObject);
    procedure edtLoginKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure edtSenhaKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure btnProdutosClick(Sender: TObject);
    procedure btnMovimentacaoClick(Sender: TObject);
    procedure btnRelatoriosClick(Sender: TObject);
    procedure btnCadastrarClick(Sender: TObject);
    procedure btnInicioClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rctContainerMenuPainting(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  private
    OpAtiva: TOpMenu;
    procedure ReduzirTela;
    procedure MostrarMenu;
    { Private declarations }
  public
    procedure AmpliarTela;
    procedure EsconderMenu;
    procedure AbrirOpcao(opMenu: TOpMenu);
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses DesafioCRUD.View.Logo, DesafioCRUD.Controller.Usuarios, DesafioCRUD.Controller.Categorias,
  DesafioCRUD.View.CadCategorias, DesafioCRUD.View.CadProdutos, DesafioCRUD.View.Movimentacao,
  DesafioCRUD.View.Relatorios, DesafioCRUD.View.CadUsuarios;

procedure TFormMain.edtLoginKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if ((Key = vkReturn) or (Key = vkTab)) then
    edtSenha.SetFocus;
end;

procedure TFormMain.edtSenhaKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if (Key = vkReturn) then
    btnLogin.OnClick(Sender);
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Assigned(FormLogo) then
    FreeAndNil(FormLogo);

  if Assigned(FormCadCategoria) then
    FormCadCategoria.Close;

  if Assigned(FormCadProdutos) then
    FormCadProdutos.Close;

  if Assigned(FormMovimentacao) then
    FormMovimentacao.Close;

  if Assigned(FormRelatorios) then
    FormRelatorios.Close;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  lytModulos.Visible := False;
  EsconderMenu;

  FormLogo := TFormLogo.Create(nil);
  FormLogo.lytPrincipal.Parent := tbContent;

  TabMain.ActiveTab := tbContent;

  lbUsuarioLogado.Visible := False;

  with TUsuarios.GetInstance do begin
    if MsgErro<>'' then begin
      ShowMessage(MsgErro);
      btnLogin.Enabled := False;
      btnCadastrar.Enabled := False;
      lbUsuarioLogado.Text := MsgErro;
      lbUsuarioLogado.Font.Size := 9;
      lbUsuarioLogado.FontColor := TAlphaColorRec.Red;
      lbUsuarioLogado.Visible := True;
    end
    else begin
      LerLogin;
      edtLogin.Text := Usuarios[0].Login;
    end;
  end;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  edtLogin.SetFocus;
  if edtLogin.Text <> '' then
    edtSenha.SetFocus;
end;

procedure TFormMain.btnCadastrarClick(Sender: TObject);
begin
//  ReduzirTela;

  FormCadUsuarios := TFormCadUsuarios.Create(nil);
  FormCadUsuarios.btnIncluirClick(Sender);
  FormCadUsuarios.ShowModal(
    procedure(ModalResult: TModalResult)
    begin
      if ModalResult = mrOk then begin
        edtLogin.Text := FormCadUsuarios.edtCadLogin.Text;
        edtSenha.Text := FormCadUsuarios.edtCadSenha.Text;
      end;
    end);
end;

procedure TFormMain.AmpliarTela;
begin
  rctContainerMenu.Enabled := True;
  rctContainerMenu.Visible := True;
  TAnimator.AnimateFloat(rctContainerMenu,'Width',273);
end;

procedure TFormMain.btnCategoriasClick(Sender: TObject);
begin
  AbrirOpcao(omCategoria);
end;

procedure TFormMain.btnCategoriasMouseEnter(Sender: TObject);
var lbl: TLabel;
begin
  lbl := TLabel(TRectangle(Sender).Children[0]);
  lbl.TextSettings.Font.Style := [TFontStyle.fsBold];
end;

procedure TFormMain.btnCategoriasMouseLeave(Sender: TObject);
var lbl: TLabel;
    Op: TOpMenu;
begin
  case TRectangle(Sender).Tag of
    0: Op := omInicio;
    1: Op := omCategoria;
    2: Op := omProdutos;
    3: Op := omMovimentacao;
    4: Op := omRelatorios;
    else Op := omVazio;
  end;

  if not (OpAtiva = Op) then begin
    lbl := TLabel(TRectangle(Sender).Children[0]);
    lbl.TextSettings.Font.Style := [];
  end;
end;

procedure TFormMain.btnInicioClick(Sender: TObject);
begin
  AbrirOpcao(omInicio);
end;

procedure TFormMain.btnLoginClick(Sender: TObject);
var Usuario: TUsuario;
begin
  with TUsuarios.GetInstance do
  begin
    Usuarios.Clear;

    Usuario.Login := edtLogin.Text;
    Usuario.Senha := edtSenha.Text;

    Usuarios.Add(0,Usuario);

    if DoLogin then begin
      if MsgErro<>'' then
        ShowMessage(MsgErro)
      else begin
        id_Usuario := Usuarios.Items[0].ID_Interno;

        ReduzirTela;
        MostrarMenu;

        lbUsuarioLogado.Text := 'Usuário logado: '+Usuarios.Items[0].Nome;
        lbUsuarioLogado.Visible := True;
        AbrirOpcao(omInicio);

        FormLogo.btnLogout.Visible := True;
      end;
    end
    else
      ShowMessage(MsgErro)
  end;
end;

procedure TFormMain.ReduzirTela;
begin
  rctContainerMenu.Enabled := False;
  TAnimator.AnimateFloat(rctContainerMenu,'Width',0);
end;

procedure TFormMain.EsconderMenu;
begin
  TAnimator.AnimateFloat(lytModulos,'Height',0);
end;

procedure TFormMain.MostrarMenu;
begin
  lytModulos.Height := 0;
  lytModulos.Visible := True;
  TAnimator.AnimateFloat(lytModulos,'Height',73);
end;

procedure TFormMain.rctContainerMenuPainting(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  if rctContainerMenu.Width = 0  then
    rctContainerMenu.Visible := False;
end;

procedure TFormMain.btnLoginMouseEnter(Sender: TObject);
begin
  btnLogin.Fill.Color := $FF6E25C8;
end;

procedure TFormMain.btnLoginMouseLeave(Sender: TObject);
begin
  btnLogin.Fill.Color := $FF8037DA;
end;

procedure TFormMain.btnMovimentacaoClick(Sender: TObject);
begin
  AbrirOpcao(omMovimentacao);
end;

procedure TFormMain.btnProdutosClick(Sender: TObject);
begin
  AbrirOpcao(omProdutos);
end;

procedure TFormMain.btnRelatoriosClick(Sender: TObject);
begin
  AbrirOpcao(omRelatorios);
end;

procedure TFormMain.AbrirOpcao(opMenu: TOpMenu);
begin
  if (opMenu <> OpAtiva) then begin
    case OpAtiva of
      omCategoria: FormCadCategoria.Close;
      omProdutos: FormCadProdutos.Close;
      omMovimentacao: FormMovimentacao.Close;
      omRelatorios: FormRelatorios.Close ;
    end;
    case opMenu of
      omInicio:
      begin
        lblInicio.TextSettings.Font.Style := [TFontStyle.fsBold];
        lblCategorias.TextSettings.Font.Style := [];
        lblProdutos.TextSettings.Font.Style := [];
        lblMovimentacao.TextSettings.Font.Style := [];
        lblRelatorios.TextSettings.Font.Style := [];
      end;
      omCategoria:
      begin
        FormCadCategoria := TFormCadCategoria.Create(nil);
        FormCadCategoria.lytPrincipal.Parent := tbContent;
        FormCadCategoria.edtDescricao.SetFocus;

        lblCategorias.TextSettings.Font.Style := [TFontStyle.fsBold];
        lblInicio.TextSettings.Font.Style := [];
        lblProdutos.TextSettings.Font.Style := [];
        lblMovimentacao.TextSettings.Font.Style := [];
        lblRelatorios.TextSettings.Font.Style := [];
      end;
      omProdutos:
      begin
        FormCadProdutos := TFormCadProdutos.Create(nil);
        FormCadProdutos.lytPrincipal.Parent := tbContent;
        FormCadProdutos.edtDescricao.SetFocus;

        lblProdutos.TextSettings.Font.Style := [TFontStyle.fsBold];
        lblInicio.TextSettings.Font.Style := [];
        lblCategorias.TextSettings.Font.Style := [];
        lblMovimentacao.TextSettings.Font.Style := [];
        lblRelatorios.TextSettings.Font.Style := [];
      end;
      omMovimentacao:
      begin
        FormMovimentacao := TFormMovimentacao.Create(nil);
        FormMovimentacao.lytPrincipal.Parent := tbContent;

        lblMovimentacao.TextSettings.Font.Style := [TFontStyle.fsBold];
        lblInicio.TextSettings.Font.Style := [];
        lblProdutos.TextSettings.Font.Style := [];
        lblCategorias.TextSettings.Font.Style := [];
        lblRelatorios.TextSettings.Font.Style := [];
      end;
      omRelatorios:
      begin
        FormRelatorios := TFormRelatorios.Create(nil);
        FormRelatorios.lytPrincipal.Parent := tbContent;

        lblRelatorios.TextSettings.Font.Style := [TFontStyle.fsBold];
        lblInicio.TextSettings.Font.Style := [];
        lblMovimentacao.TextSettings.Font.Style := [];
        lblProdutos.TextSettings.Font.Style := [];
        lblCategorias.TextSettings.Font.Style := [];
      end;
    end;

    OpAtiva := opMenu;
  end;
end;

end.
