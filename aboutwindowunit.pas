unit AboutWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  MyHyperlink;
type

  { About window }

  { TAboutWindow }

  TAboutWindow = class(TForm)
    IconsCredit: TLabel;
    Heading: TLabel;
    Author: TLabel;
    Description: TLabel;
    IconsLink: TMyHyperlink;
    GithubLink: TMyHyperlink;
    EmailLink: TMyHyperlink;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  AboutWindow: TAboutWindow;

implementation

{$R *.lfm}

{ TAboutWindow }

procedure TAboutWindow.FormCreate(Sender: TObject);
var
  a, b, c, d: String;
begin
  a := 'mikolaj.rozwad';
  b := 'put.poznan';
  c := 'student';
  d := 'pl';

  EmailLink.Caption := a+'owski@'+c+'.'+b+'.'+d;
  EmailLink.URL := 'mailto:'+EmailLink.Caption;
end;


end.

