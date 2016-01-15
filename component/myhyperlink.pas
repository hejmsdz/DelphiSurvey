unit MyHyperlink;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Windows;

type
  TMyHyperlink = class(TLabel)
  private
    { Private declarations }
    FURL: String;
    FNormalColor: TColor;
    FHoverColor: TColor;
    Hover: Boolean;
    procedure UpdateColor;
    procedure SetNormalColor(Value: TColor);
    procedure SetHoverColor(Value: TColor);
  protected
    { Protected declarations }
    procedure Click; override;
    procedure MouseEnter; override;
    procedure MouseLeave; override;
  public
    { Public declarations }
    constructor Create(TheOwner: TComponent); override;
    function CheckHover: Boolean;
  published
    { Published declarations }
    property URL: String read FURL write FURL;
    property NormalColor: TColor read FNormalColor write SetNormalColor;
    property HoverColor: TColor read FHoverColor write SetHoverColor;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Additional',[TMyHyperlink]);
end;

constructor TMyHyperlink.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Hover := False;
  Cursor := crHandPoint;
  FNormalColor := clHotLight;
  FHoverColor := clHighlight;
  Font.Underline := True;
  UpdateColor;
end;

procedure TMyHyperlink.UpdateColor;
begin
  if Hover then Font.Color := FHoverColor
  else Font.Color := FNormalColor;
end;

procedure TMyHyperlink.Click;
begin
  inherited;
  ShellExecute(0, 'OPEN', PChar(FURL), '', '', SW_SHOWNORMAL);
end;

procedure TMyHyperlink.MouseEnter;
begin
  inherited;
  Hover := True;
  UpdateColor;
end;

procedure TMyHyperlink.MouseLeave;
begin
  inherited;
  Hover := False;
  UpdateColor;
end;

procedure TMyHyperlink.SetNormalColor(Value: TColor);
begin
  FNormalColor := Value;
  UpdateColor;
end;

procedure TMyHyperlink.SetHoverColor(Value: TColor);
begin
  FHoverColor := Value;
  UpdateColor;
end;

function TMyHyperlink.CheckHover: Boolean;
begin
  Result := Hover;
end;

end.
