unit DesafioCRUD.View.Relatorios;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Skia, FMX.Edit, FMX.ListBox, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Skia, FMX.Effects, FMX.Objects, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
  FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.FB,
  FireDAC.Phys.FBDef, FireDAC.FMXUI.Wait, FireDAC.Stan.Param, FireDAC.DatS,
  FireDAC.DApt.Intf, FireDAC.DApt, Data.DB, FireDAC.Comp.DataSet,
  FireDAC.Comp.Client, FMX.frxClass;

type
  TFormRelatorios = class(TForm)
    lytPrincipal: TLayout;
    Rectangle6: TRectangle;
    Rectangle9: TRectangle;
    ShadowEffect1: TShadowEffect;
    Layout8: TLayout;
    btnSair: TRectangle;
    SkSvg7: TSkSvg;
    Layout1: TLayout;
    Rectangle1: TRectangle;
    btnConfirmar: TRectangle;
    Label4: TLabel;
    SkSvg1: TSkSvg;
    cboProduto: TComboBox;
    Label2: TLabel;
    frxReport1: TfrxReport;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure btnSairClick(Sender: TObject);
    procedure btnSairMouseEnter(Sender: TObject);
    procedure btnSairMouseLeave(Sender: TObject);
    procedure btnConfirmarClick(Sender: TObject);
  private
    procedure CarregarProdutos;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormRelatorios: TFormRelatorios;

implementation

{$R *.fmx}

uses DesafioCRUD.Controller.Produtos, DesafioCRUD.View.Principal;

procedure TFormRelatorios.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
  FormRelatorios := nil;
end;

procedure TFormRelatorios.FormCreate(Sender: TObject);
begin
  CarregarProdutos;
end;

procedure TFormRelatorios.btnConfirmarClick(Sender: TObject);
var id_Produto: Integer;
begin
  id_Produto := 0;

  if cboProduto.ItemIndex > -1 then
    id_Produto := cboProduto.ListItems[cboProduto.ItemIndex].Tag;

  if id_Produto <> 0 then begin
    with TProdutos.GetInstance do
      if (not ImprimirMovimentacao(id_Produto)) then
        ShowMessage(MsgErro);
  end
  else
    ShowMessage('É necessário selecionar um produto para continuar');
end;

procedure TFormRelatorios.btnSairClick(Sender: TObject);
begin
  FormMain.AbrirOpcao(omInicio);
  Self.Close;
end;

procedure TFormRelatorios.btnSairMouseEnter(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF6E25C8;
end;

procedure TFormRelatorios.btnSairMouseLeave(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF8037DA;
end;

procedure TFormRelatorios.CarregarProdutos;
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

end.
