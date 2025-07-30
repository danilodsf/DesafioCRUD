unit DesafioCRUD.View.CadCategorias;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  DesafioCRUD.View.CadBase, System.Skia, FMX.Skia, FMX.ListBox, FMX.Layouts, FMX.Edit,
  FMX.Objects, FMX.Controls.Presentation, FMX.TabControl, FMX.Effects;

type
  TFormCadCategoria = class(TFormCadBase)
    procedure FormCreate(Sender: TObject);
    procedure btnSalvarClick(Sender: TObject);
    procedure lbItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnEditarClick(Sender: TObject);
    procedure btnExcluirClick(Sender: TObject);
    procedure btnIncluirClick(Sender: TObject);
    procedure btnPesquisarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure AddCategoria(ID_Interno, ID: Integer; Descricao: String);
    procedure CarregarDados;
    procedure MontarGrid;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCadCategoria: TFormCadCategoria;

implementation

uses
  DesafioCRUD.Controller.Categorias, DesafioCRUD.Frame.Categoria;

{$R *.fmx}

procedure TFormCadCategoria.btnEditarClick(Sender: TObject);
begin
  inherited;
  edtCadDescricao.Enabled := True;
  edtCadDescricao.SetFocus;
  edtCadDescricao.SelStart := Length(edtCadDescricao.Text);
end;

procedure TFormCadCategoria.btnExcluirClick(Sender: TObject);
begin
  inherited;
  with TCategorias.GetInstance do begin
    if Excluir(edtCadID.Tag) then begin
      TabControl.ActiveTab := tbPesquisa;
      CarregarDados;
    end
    else
      ShowMessage(MsgErro);
  end;
end;

procedure TFormCadCategoria.btnIncluirClick(Sender: TObject);
begin
  inherited;
  edtCadDescricao.Enabled := True;
end;

procedure TFormCadCategoria.btnPesquisarClick(Sender: TObject);
begin
  inherited;
  with TCategorias.GetInstance do begin
    Categorias.Clear;
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

procedure TFormCadCategoria.btnSalvarClick(Sender: TObject);
var Categoria: TCategoria;
begin
  inherited;
  with TCategorias.GetInstance do begin
    Categorias.Clear;
    if edtCadID.Text <> '' then
      Categoria.ID_Interno := edtCadID.Tag
    else
      Categoria.ID_Interno := 0;

    Categoria.Descricao := edtCadDescricao.Text;
    Categorias.Add(0,Categoria);

    if Categoria.ID_Interno = 0 then begin
      if Cadastrar then begin
        TabControl.ActiveTab := tbPesquisa;
        CarregarDados;
      end
      else
        ShowMessage(MsgErro);
    end
    else begin
      if Atualizar(Categoria.ID_Interno) then begin
        TabControl.ActiveTab := tbPesquisa;
        CarregarDados;
      end
      else
        ShowMessage(MsgErro);
    end;
  end;
end;

procedure TFormCadCategoria.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  FormCadCategoria := nil;
end;

procedure TFormCadCategoria.FormCreate(Sender: TObject);
begin
  lblPesquisa.Text := 'Buscar categoria';
  TabControl.ActiveTab := tbPesquisa;
  CarregarDados;
end;

procedure TFormCadCategoria.lbItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var i: Integer;
begin
  inherited;
  edtCadID.Tag         := Item.Tag;
  edtCadDescricao.Enabled := False;

  for I := 0 to Pred(Item.ChildrenCount) do begin
    if Item.Children[I] is TFrameCategoria then begin
      edtCadID.Text        := TFrameCategoria(Item.Children[i]).lbID.Text;
      edtCadDescricao.Text := TFrameCategoria(Item.Children[i]).lbDescricao.Text;
    end;
  end;
end;

procedure TFormCadCategoria.AddCategoria(ID_Interno, ID: Integer; Descricao: String);
var Item: TListBoxItem;
    FrameCategoria: TFrameCategoria;
begin
  Item := TListBoxItem.Create(Self);
  Item.Height :=  37;
  Item.Tag := ID_Interno;
  FrameCategoria := TFrameCategoria.Create(Self);
  FrameCategoria.Parent := Item;
  FrameCategoria.Align := TAlignLayout.Client;
  FrameCategoria.Name := 'Frame_'+ID.ToString;

  FrameCategoria.lbID.Text := ID.ToString;
  FrameCategoria.lbDescricao.Text := Descricao;

  Item.Parent := lb;
end;

procedure TFormCadCategoria.CarregarDados;
begin
  inherited;
  with TCategorias.GetInstance do begin
    if Consultar then
      MontarGrid;
  end;
end;

procedure TFormCadCategoria.MontarGrid;
var x: Integer;
    Categoria: TCategoria;
begin
  with TCategorias.GetInstance do begin
    lb.BeginUpdate;
    lb.Items.Clear;
    for x := 0 to Pred(Categorias.Count) do begin
      Categoria := Categorias.Items[x];
      AddCategoria(Categoria.ID_Interno, Categoria.ID, Categoria.Descricao);
    end;
    lbQtdRegistros.Text := lb.Count.ToString+' registros';
    lb.EndUpdate;
  end;
end;

end.
