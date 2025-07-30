unit DesafioCRUD.Frame.Usuario;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation;

type
  TFrameUsuario = class(TFrame)
    Rectangle1: TRectangle;
    lbID: TLabel;
    lbLogin: TLabel;
    Line1: TLine;
    Path1: TPath;
    lbNome: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
