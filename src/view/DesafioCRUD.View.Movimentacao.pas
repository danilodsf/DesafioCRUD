unit DesafioCRUD.View.Movimentacao;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Skia, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Skia, FMX.Effects,
  FMX.Objects, FMX.Edit, FMX.ListBox, Globals;

type
  TFormMovimentacao = class(TForm)
    lytPrincipal: TLayout;
    Rectangle6: TRectangle;
    Rectangle9: TRectangle;
    ShadowEffect1: TShadowEffect;
    Layout8: TLayout;
    btnSair: TRectangle;
    SkSvg7: TSkSvg;
    Layout1: TLayout;
    cboProduto: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    cboTipo: TComboBox;
    Label3: TLabel;
    btnConfirmar: TRectangle;
    Label4: TLabel;
    SkSvg1: TSkSvg;
    Rectangle3: TRectangle;
    edtQuantidade: TEdit;
    Rectangle1: TRectangle;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnSairClick(Sender: TObject);
    procedure btnSairMouseEnter(Sender: TObject);
    procedure btnSairMouseLeave(Sender: TObject);
    procedure edtQuantidadeKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure btnConfirmarClick(Sender: TObject);
  private
    procedure CarregarProdutos;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMovimentacao: TFormMovimentacao;

implementation

{$R *.fmx}

uses DesafioCRUD.Controller.Produtos, DesafioCRUD.View.Principal;

procedure TFormMovimentacao.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
  FormMovimentacao := nil;
end;

procedure TFormMovimentacao.FormCreate(Sender: TObject);
begin
  CarregarProdutos;
end;

procedure TFormMovimentacao.btnConfirmarClick(Sender: TObject);
var id_Produto, Quantidade, Tipo: Integer;
begin
  with TProdutos.GetInstance do begin
    Quantidade := StrToIntDef(edtQuantidade.Text,0);
    id_Produto := StrToIntDef(cboProduto.ListItems[cboProduto.ItemIndex].Tag.ToString,0);
    Tipo       := cboTipo.ItemIndex;
    if Movimentar(id_Produto, Tipo, Quantidade, id_Usuario) then begin
      ShowMessage('Operação realizada com sucesso!');
      btnSair.OnClick(Sender);
    end
    else
      ShowMessage(MsgErro);
  end;
end;

procedure TFormMovimentacao.btnSairClick(Sender: TObject);
begin
  FormMain.AbrirOpcao(omInicio);
  Self.Close;
end;

procedure TFormMovimentacao.btnSairMouseEnter(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF6E25C8;
end;

procedure TFormMovimentacao.btnSairMouseLeave(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF8037DA;
end;

procedure TFormMovimentacao.CarregarProdutos;
var i: Integer;
    ListItem: TListBoxItem;
begin
  inherited;
  with TProdutos.GetInstance do begin
    cboProduto.Items.Clear;
    if Consultar then begin
      for I := 0 to Pred(Produtos.Count) do begin
        ListItem := TListBoxItem.Create(cboProduto);
        ListItem.Text := Produtos.Items[I].Descricao;
        ListItem.Tag  := Produtos.Items[I].ID_Interno;
        cboProduto.AddObject(ListItem);
      end;
    end;
  end;
end;



procedure TFormMovimentacao.edtQuantidadeKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if not CharInSet(KeyChar, ['0'..'9', #8, #13]) then
    KeyChar := #0;
end;

end.
