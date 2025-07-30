unit DesafioCRUD.View.CadBase;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, System.Skia, FMX.Skia,
  FMX.TabControl, FMX.ListBox, FMX.Edit, FMX.Effects;

type TEstadoControles = (ecGet, ecInsert, ecEdit);

type
  TFormCadBase = class(TForm)
    Layout1: TLayout;
    TabControl: TTabControl;
    tbPesquisa: TTabItem;
    tbCadastro: TTabItem;
    Layout2: TLayout;
    lb: TListBox;
    ListBoxHeader1: TListBoxHeader;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Layout3: TLayout;
    lblPesquisa: TLabel;
    Line1: TLine;
    lytPrincipal: TLayout;
    Rectangle6: TRectangle;
    Label10: TLabel;
    Layout4: TLayout;
    btnPesquisar: TRectangle;
    Label11: TLabel;
    Layout5: TLayout;
    Layout6: TLayout;
    lbQtdRegistros: TLabel;
    Layout7: TLayout;
    btnIncluir: TRectangle;
    Label1: TLabel;
    SkSvg1: TSkSvg;
    SkSvg6: TSkSvg;
    Rectangle9: TRectangle;
    ShadowEffect1: TShadowEffect;
    lytCadastro: TLayout;
    Label13: TLabel;
    Rectangle10: TRectangle;
    edtCadID: TEdit;
    Label14: TLabel;
    Rectangle11: TRectangle;
    edtCadDescricao: TEdit;
    Layout9: TLayout;
    btnSalvar: TRectangle;
    Label3: TLabel;
    SkSvg3: TSkSvg;
    btnVoltar: TRectangle;
    SkSvg5: TSkSvg;
    btnEditar: TRectangle;
    Label2: TLabel;
    SkSvg2: TSkSvg;
    btnExcluir: TRectangle;
    Label4: TLabel;
    SkSvg4: TSkSvg;
    Layout8: TLayout;
    btnSair: TRectangle;
    SkSvg7: TSkSvg;
    Rectangle1: TRectangle;
    edtDescricao: TEdit;
    procedure btnPesquisarMouseEnter(Sender: TObject);
    procedure btnPesquisarMouseLeave(Sender: TObject);
    procedure btnIncluirClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnVoltarClick(Sender: TObject);
    procedure lbItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnEditarClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnSairClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure edtDescricaoKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    procedure Controles(Estado: TEstadoControles);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormCadBase: TFormCadBase;

implementation

{$R *.fmx}

uses DesafioCRUD.View.Principal;

procedure TFormCadBase.btnEditarClick(Sender: TObject);
begin
  Controles(ecEdit);
end;

procedure TFormCadBase.btnPesquisarMouseEnter(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF6E25C8;
end;

procedure TFormCadBase.btnPesquisarMouseLeave(Sender: TObject);
begin
  TRectangle(Sender).Fill.Color := $FF8037DA;
end;

procedure TFormCadBase.btnSairClick(Sender: TObject);
begin
  FormMain.AbrirOpcao(omInicio);
  Self.Close;
end;

procedure TFormCadBase.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Action := TCloseAction.caFree;
end;

procedure TFormCadBase.FormCreate(Sender: TObject);
begin
  TabControl.ActiveTab := tbPesquisa;
  edtDescricao.SetFocus;
end;

procedure TFormCadBase.lbItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  Controles(ecGet);

  TabControl.ActiveTab := tbCadastro;

  edtCadDescricao.SetFocus;
end;

procedure TFormCadBase.btnIncluirClick(Sender: TObject);
begin
  edtCadID.Text := '';
  edtCadDescricao.Text := '';

  Controles(ecInsert);

  TabControl.ActiveTab := tbCadastro;

  edtCadDescricao.SetFocus;
end;

procedure TFormCadBase.btnVoltarClick(Sender: TObject);
begin
  TabControl.ActiveTab := tbPesquisa;
end;

procedure TFormCadBase.Button1Click(Sender: TObject);
begin
  WindowState := TWindowState.wsMaximized;
end;

procedure TFormCadBase.Controles(Estado: TEstadoControles);
begin
  case Estado of
    ecGet:
    begin
      btnEditar.Visible := True;
      btnExcluir.Visible := True;
      btnSalvar.Visible := False;
      btnVoltar.Visible := True;
    end;
    ecInsert:
    begin
      btnEditar.Visible := False;
      btnExcluir.Visible := False;
      btnSalvar.Visible := True;
      btnVoltar.Visible := True;
    end;
    ecEdit:
    begin
      btnEditar.Visible := False;
      btnExcluir.Visible := True;
      btnSalvar.Visible := True;
      btnVoltar.Visible := True;
    end;
  end;
end;

procedure TFormCadBase.edtDescricaoKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if key = vkReturn then
    btnPesquisar.OnClick(Sender);
end;

end.
