unit SortWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, StdCtrls;

type

  { TSortWindow }

  TSortWindow = class(TForm)
    TwoCriteriaCheckBox: TCheckBox;
    FirstCriterion: TRadioGroup;
    Direction: TRadioGroup;
    Notice: TLabel;
    OKButton: TBitBtn;
    SecondCriterion: TRadioGroup;
    procedure FirstCriterionClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure TwoCriteriaCheckBoxChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  SortWindow: TSortWindow;

implementation

{$R *.lfm}

{ TSortWindow }

procedure TSortWindow.FirstCriterionClick(Sender: TObject);
var
  first, n, i, j: Integer;
begin
  if SecondCriterion.Enabled then
  begin
    first := FirstCriterion.ItemIndex;
    n := SecondCriterion.Items.Count;
    j := SecondCriterion.ItemIndex;
    for i := 0 to n do
      SecondCriterion.Controls[i].Enabled := (i <> first);
    if first = j then
      SecondCriterion.ItemIndex := (j + 1) mod n;
  end;
end;

procedure TSortWindow.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;

  if FirstCriterion.ItemIndex < 0 then
    ShowMessage('Choose the first sorting criterion!')
  else
    ModalResult := mrOK;
end;

procedure TSortWindow.TwoCriteriaCheckBoxChange(Sender: TObject);
begin
  if TwoCriteriaCheckBox.Checked then
  begin
    SecondCriterion.Enabled := true;
    SecondCriterion.ItemIndex := 0;
    FirstCriterionClick(Sender);
  end
  else
  begin
    SecondCriterion.Enabled := false;
    SecondCriterion.ItemIndex := -1;
  end;
end;

end.

