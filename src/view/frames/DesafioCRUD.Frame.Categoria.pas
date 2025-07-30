unit DesafioCRUD.Frame.Categoria;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Controls.Presentation;

type
  TFrameCategoria = class(TFrame)
    Rectangle1: TRectangle;
    lbID: TLabel;
    Line1: TLine;
    Path1: TPath;
    lbDescricao: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
