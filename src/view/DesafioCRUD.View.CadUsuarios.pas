unit DesafioCRUD.View.CadUsuarios;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  DesafioCRUD.View.CadBase, System.Skia, FMX.Effects, FMX.ListBox, FMX.Layouts,
  FMX.Skia, FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.TabControl;

type
  TFormCadUsuarios = class(TFormCadBase)
    Label5: TLabel;
    Label9: TLabel;
    Rectangle2: TRectangle;
    edtCadSenha: TEdit;
    Rectangle3: TRectangle;
    edtCadLogin: TEdit;
    procedure btnSairClick(Sender: TObject);
    procedure lbItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnIncluirClick(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure edtCadDescricaoKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edtCadLoginKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure edtCadSenhaKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    procedure CarregarDados;
    procedure AddUsuario(ID_Interno, ID: Integer; Nome, Login: String);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCadUsuarios: TFormCadUsuarios;

implementation

{$R *.fmx}

uses DesafioCRUD.View.Principal, DesafioCRUD.Controller.Usuarios, DesafioCRUD.Frame.Usuario;

procedure TFormCadUsuarios.btnEditarClick(Sender: TObject);
begin
  inherited;
  edtCadDescricao.Enabled := True;
  edtCadLogin.Enabled     := True;
  edtCadDescricao.SetFocus;
  edtCadDescricao.SelStart := Length(edtCadDescricao.Text);
end;

procedure TFormCadUsuarios.btnExcluirClick(Sender: TObject);
begin
  inherited;
  with TUsuarios.GetInstance do begin
    if Excluir(edtCadID.Tag) then begin
      TabControl.ActiveTab := tbPesquisa;
      CarregarDados;
    end
    else
      ShowMessage(MsgErro);
  end;
end;

procedure TFormCadUsuarios.btnIncluirClick(Sender: TObject);
begin
  inherited;
  btnVoltar.Visible       := False;
  edtCadDescricao.Enabled := True;
  edtCadLogin.Enabled     := True;
  edtCadSenha.Enabled     := True;

  edtCadDescricao.SetFocus;
end;

procedure TFormCadUsuarios.btnSairClick(Sender: TObject);
begin
  ModalResult := mrCancel;
  FormMain.AmpliarTela;
end;

procedure TFormCadUsuarios.btnSalvarClick(Sender: TObject);
var Usuario: TUsuario;
begin
  inherited;
  with TUsuarios.GetInstance do begin
    Usuarios.Clear;
    Usuario.ID_Interno := 0;

    Usuario.Nome  := edtCadDescricao.Text;
    Usuario.Login := edtCadLogin.Text;
    Usuario.Senha := edtCadSenha.Text;

    Usuarios.Add(0,Usuario);

    if Cadastrar then
      ModalResult := mrOK
    else
      ShowMessage(MsgErro);
  end;
end;

procedure TFormCadUsuarios.lbItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var i: Integer;
begin
  inherited;
  edtCadID.Tag            := Item.Tag;
  edtCadDescricao.Enabled := False;
  edtCadLogin.Enabled     := False;

  for I := 0 to Pred(Item.ChildrenCount) do begin
    if Item.Children[I] is TFrameUsuario then begin
      edtCadID.Text        := TFrameUsuario(Item.Children[i]).lbID.Text;
      edtCadDescricao.Text := TFrameUsuario(Item.Children[i]).lbNome.Text;
      edtCadLogin.Text     := TFrameUsuario(Item.Children[i]).lbLogin.Text;
    end;
  end;
end;

procedure TFormCadUsuarios.CarregarDados;
var x: Integer;
    Usuario: TUsuario;
begin
  inherited;
  with TUsuarios.GetInstance do begin
    if Consultar then begin
      lb.BeginUpdate;
      lb.Items.Clear;
      for x := 0 to Pred(Usuarios.Count) do begin
        Usuario := Usuarios.Items[x];
        AddUsuario(Usuario.ID_Interno, Usuario.ID, Usuario.Nome, Usuario.Login);
      end;
      lbQtdRegistros.Text := lb.Count.ToString+' registros';
      lb.EndUpdate;
    end;
  end;
end;

procedure TFormCadUsuarios.edtCadDescricaoKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if ((Key = vkReturn) or (Key = vkTab)) then
    edtCadLogin.SetFocus;
end;

procedure TFormCadUsuarios.edtCadLoginKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if ((Key = vkReturn) or (Key = vkTab)) then
    edtCadSenha.SetFocus;
end;

procedure TFormCadUsuarios.edtCadSenhaKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if (Key = vkReturn)  then
    btnSalvarClick(Sender);
end;

procedure TFormCadUsuarios.AddUsuario(ID_Interno, ID: Integer; Nome, Login: String);
var Item: TListBoxItem;
    FrameUsuario: TFrameUsuario;
begin
  Item := TListBoxItem.Create(Self);
  Item.Height :=  37;
  Item.Tag := ID_Interno;
  FrameUsuario := TFrameUsuario.Create(Self);
  FrameUsuario.Parent := Item;
  FrameUsuario.Align := TAlignLayout.Client;
  FrameUsuario.Name := 'Frame_'+ID.ToString;

  FrameUsuario.lbID.Text := ID.ToString;
  FrameUsuario.lbNome.Text := Nome;
  FrameUsuario.lbLogin.Text := Login;

  Item.Parent := lb;
end;

end.
