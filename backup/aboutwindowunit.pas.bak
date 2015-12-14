unit AboutWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Windows, ShellAPI;

type

  { About window }

  { TAboutWindow }

  TAboutWindow = class(TForm)
    IconsLink: TLabel;
    IconsCredit: TLabel;
    Heading: TLabel;
    Author: TLabel;
    Description: TLabel;
    Email: TLabel;
    Github: TLabel;
    procedure EmailClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure IconsLinkClick(Sender: TObject);
    procedure GithubClick(Sender: TObject);
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
  a := 'mikolaj.rozwadowski';
  b := 'put.poznan';
  c := 'student';
  d := 'pl';

  Email.Caption := a+'@'+c+'.'+b+'.'+d;
end;

procedure TAboutWindow.IconsLinkClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(IconsLink.Caption), '', '', SW_SHOWNORMAL);
end;

procedure TAboutWindow.GithubClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar(Github.Caption), '', '', SW_SHOWNORMAL);
end;

procedure TAboutWindow.EmailClick(Sender: TObject);
begin
  ShellExecute(0, 'OPEN', PChar('mailto:'+Email.Caption), '', '', SW_SHOWNORMAL);
end;

end.

