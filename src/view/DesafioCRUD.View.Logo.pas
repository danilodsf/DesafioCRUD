unit DesafioCRUD.View.Logo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, Globals;

type
  TFormLogo = class(TForm)
    lytPrincipal: TLayout;
    Layout1: TLayout;
    btnLogout: TRectangle;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnLogoutMouseEnter(Sender: TObject);
    procedure btnLogoutMouseLeave(Sender: TObject);
    procedure btnLogoutClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormLogo: TFormLogo;

implementation

{$R *.fmx}

uses DesafioCRUD.View.Principal;

procedure TFormLogo.btnLogoutClick(Sender: TObject);
begin
  id_Usuario := 0;

  FormMain.AmpliarTela;
  FormMain.EsconderMenu;

  FormMain.lbUsuarioLogado.Visible := False;
  btnLogout.Visible := False;
end;

procedure TFormLogo.btnLogoutMouseEnter(Sender: TObject);
begin
  btnLogout.Fill.Color := $FFABABAB;
end;

procedure TFormLogo.btnLogoutMouseLeave(Sender: TObject);
begin
  btnLogout.Fill.Color := $FFC7C7C7;
end;

procedure TFormLogo.FormCreate(Sender: TObject);
begin
  btnLogout.Visible := False;
end;

end.
