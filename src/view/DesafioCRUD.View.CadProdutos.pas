unit DesafioCRUD.View.CadProdutos;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  DesafioCRUD.View.CadBase, System.Skia, FMX.Effects, FMX.ListBox, FMX.Layouts,
  FMX.Skia, FMX.Edit, FMX.Objects, FMX.Controls.Presentation, FMX.TabControl;

type
  TFormCadProdutos = class(TFormCadBase)
    Label5: TLabel;
    Label9: TLabel;
    Label12: TLabel;
    Label15: TLabel;
    Rectangle3: TRectangle;
    edtCadPreco: TEdit;
    cboCadCategoria: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure edtCadDescricaoKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edtCadPrecoKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure edtCadPrecoKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSalvarClick(Sender: TObject);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure lbItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnPesquisarClick(Sender: TObject);
  private
    procedure CarregarDados;
    procedure MontarGrid;
    procedure AddProduto(ID_Interno, ID, ID_Categoria, Estoque: Integer; Descricao, Categoria: String; Preco: Currency);
    procedure CarregarCategorias;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCadProdutos: TFormCadProdutos;

implementation

{$R *.fmx}

uses DesafioCRUD.Controller.Produtos, DesafioCRUD.Frame.Produto,
  DesafioCRUD.Controller.Categorias;

procedure TFormCadProdutos.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  FormCadProdutos := nil;
end;

procedure TFormCadProdutos.FormCreate(Sender: TObject);
begin
  lblPesquisa.Text := 'Buscar produto';
  TabControl.ActiveTab := tbPesquisa;
  CarregarCategorias;
  CarregarDados;
end;

procedure TFormCadProdutos.lbItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var i: Integer;
begin
  inherited;
  edtCadID.Tag         := Item.Tag;
  edtCadDescricao.Enabled := False;
  cboCadCategoria.Enabled  := False;
  edtCadPreco.Enabled := False;

  for I := 0 to Pred(Item.ChildrenCount) do begin
    if Item.Children[I] is TFrameProduto then begin
      edtCadID.Text          := TFrameProduto(Item.Children[i]).lbID.Text;
      edtCadDescricao.Text   := TFrameProduto(Item.Children[i]).lbDescricao.Text;
      cboCadCategoria.ItemIndex  := cboCadCategoria.Items.IndexOf(TFrameProduto(Item.Children[i]).lbCategoria.Text);
      edtCadPreco.Text       := TFrameProduto(Item.Children[i]).lbPreco.Text;
    end;
  end;
end;

procedure TFormCadProdutos.btnEditarClick(Sender: TObject);
begin
  inherited;
  edtCadDescricao.Enabled := True;
  cboCadCategoria.Enabled := True;
  edtCadPreco.Enabled     := True;

  edtCadDescricao.SetFocus;
  edtCadDescricao.SelStart := Length(edtCadDescricao.Text);
end;

procedure TFormCadProdutos.btnExcluirClick(Sender: TObject);
begin
  inherited;
  with TProdutos.GetInstance do begin
    if Excluir(edtCadID.Tag) then begin
      TabControl.ActiveTab := tbPesquisa;
      CarregarDados;
    end
    else
      ShowMessage(MsgErro);
  end;
end;

procedure TFormCadProdutos.btnPesquisarClick(Sender: TObject);
begin
  inherited;
  with TProdutos.GetInstance do begin
    Produtos.Clear;
    if edtDescricao.Text = '' then begin
      if Consultar then
        MontarGrid
    end
    else begin
      if Consultar(edtDescricao.Text) then
        MontarGrid
    end;
  end;
end;

procedure TFormCadProdutos.btnSalvarClick(Sender: TObject);
var Produto: TProduto;
begin
  inherited;
  with TProdutos.GetInstance do begin
    Produtos.Clear;
    if edtCadID.Text <> '' then
      Produto.ID_Interno := edtCadID.Tag
    else
      Produto.ID_Interno := 0;

    Produto.Descricao := edtCadDescricao.Text;

    Produto.IDCategoria := cboCadCategoria.ListItems[cboCadCategoria.ItemIndex].Tag;
    Produto.Categoria   := cboCadCategoria.ListItems[cboCadCategoria.ItemIndex].Text;

    Produto.Preco       := StrToFloatDef(edtCadPreco.Text.Replace('.','').Replace('.',''),0);

    Produtos.Add(0,Produto);

    if Produto.ID_Interno = 0 then begin
      if Cadastrar then begin
        TabControl.ActiveTab := tbPesquisa;
        CarregarDados;
      end
      else
        ShowMessage(MsgErro);
    end
    else begin
      if Atualizar(Produto.ID_Interno) then begin
        TabControl.ActiveTab := tbPesquisa;
        CarregarDados;
      end
      else
        ShowMessage(MsgErro);
    end;
  end;
end;

procedure TFormCadProdutos.CarregarDados;
begin
  inherited;
  with TProdutos.GetInstance do begin
    if Consultar then
      MontarGrid;
  end;
end;

procedure TFormCadProdutos.CarregarCategorias;
var i: Integer;
    ListItem: TListBoxItem;
begin
  inherited;
  with TCategorias.GetInstance do begin
    cboCadCategoria.Items.Clear;
    if Consultar then begin
      for I := 0 to Pred(Categorias.Count) do begin
        ListItem := TListBoxItem.Create(cboCadCategoria);
        ListItem.Text := Categorias.Items[I].Descricao;
        ListItem.Tag  := Categorias.Items[I].ID_Interno;
        cboCadCategoria.AddObject(ListItem);
      end;
    end;
  end;
end;

procedure TFormCadProdutos.edtCadDescricaoKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if ((Key = vkReturn) or (Key = vkTab)) then
    cboCadCategoria.SetFocus;
end;

procedure TFormCadProdutos.edtCadPrecoKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if not CharInSet(KeyChar, ['0'..'9', ',', #8, #13]) then
    KeyChar := #0;
end;

procedure TFormCadProdutos.edtCadPrecoKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  inherited;
  if ((Key = vkReturn) or (Key = vkTab)) then
    edtCadDescricao.SetFocus;
end;

procedure TFormCadProdutos.MontarGrid;
var x: Integer;
    Produto: TProduto;
begin
  with TProdutos.GetInstance do begin
    lb.BeginUpdate;
    lb.Items.Clear;
    for x := 0 to Pred(Produtos.Count) do begin
      Produto := Produtos.Items[x];
      AddProduto(Produto.ID_Interno, Produto.ID, Produto.IDCategoria, Produto.Estoque, Produto.Descricao, Produto.Categoria, Produto.Preco);
    end;
    lbQtdRegistros.Text := lb.Count.ToString+' registros';
    lb.EndUpdate;
  end;
end;

procedure TFormCadProdutos.AddProduto(ID_Interno, ID, ID_Categoria, Estoque: Integer; Descricao, Categoria: String; Preco: Currency);
var Item: TListBoxItem;
    FrameProduto: TFrameProduto;
begin
  Item := TListBoxItem.Create(Self);
  Item.Height :=  37;
  Item.Tag := ID_Interno;
  FrameProduto := TFrameProduto.Create(Self);
  FrameProduto.Parent := Item;
  FrameProduto.Align := TAlignLayout.Client;
  FrameProduto.Name := 'Frame_'+ID.ToString;

  FrameProduto.lbID.Text := ID.ToString;
  FrameProduto.lbDescricao.Text := Descricao;
  FrameProduto.lbCategoria.Text := Categoria;
  FrameProduto.lbCategoria.Tag  := ID_Categoria;
  FrameProduto.lbEstoque.Text   := Estoque.ToString;
  FrameProduto.lbPreco.Text     := FormatFloat('#,##0.00',Preco);

  Item.Parent := lb;
end;

end.
