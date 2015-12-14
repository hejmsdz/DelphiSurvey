unit RecordWindowUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  StdCtrls, ExtCtrls,

  DataStructuresUnit;

type
  { record creation/edition window }
  TRecordWindow = class(TForm)
    SexComboBox: TComboBox;
    SupportsCheckbox: TCheckBox;
    EducationComboBox: TComboBox;
    AgeComboBox: TComboBox;
    OKButton: TBitBtn;
    LocationComboBox: TComboBox;
    VotedCheckbox: TCheckBox;
    SupportsLabel: TLabel;
    SexLabel: TLabel;
    LocationLabel: TLabel;
    EducationLabel: TLabel;
    AgeLabel: TLabel;
    VotedLabel: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure SetSurvey(newSurvey: TSurvey);
  private
    { private declarations }
  public
    Survey: TSurvey;
    { public declarations }
  end;

var
  RecordWindow: TRecordWindow;

implementation

{$R *.lfm}

{ TRecordWindow }

{ prepare the combo boxes }
procedure TRecordWindow.FormCreate(Sender: TObject);
begin
  SexComboBox.Items.AddStrings(SexValues);
  LocationComboBox.Items.AddStrings(LocationValues);
  EducationComboBox.Items.AddStrings(EducationValues);
  AgeComboBox.Items.AddStrings(AgeValues);
end;

{ populate the form with a given record }
procedure TRecordWindow.SetSurvey(newSurvey: TSurvey);
begin
  Survey := newSurvey;
  SupportsCheckbox.Checked := Survey.Supports;
  SexComboBox.ItemIndex := Ord(Survey.Sex);
  LocationComboBox.ItemIndex := Ord(Survey.Location);
  EducationComboBox.ItemIndex := Ord(Survey.Education);
  AgeComboBox.ItemIndex := Ord(Survey.Age);
  VotedCheckbox.Checked := Survey.Supports;
end;

{ validate the input, publish output data and close }
procedure TRecordWindow.OKButtonClick(Sender: TObject);
begin
  ModalResult := mrNone;

  if SexComboBox.ItemIndex < 0 then
    ShowMessage('Fill in the Sex field!')
  else if LocationComboBox.ItemIndex < 0 then
    ShowMessage('Fill in the Location field!')
  else if EducationComboBox.ItemIndex < 0 then
    ShowMessage('Fill in the Education field!')
  else if AgeComboBox.ItemIndex < 0 then
    ShowMessage('Fill in the Age field!')
  else
  begin
    ModalResult := mrOK;

    Survey.Supports := SupportsCheckbox.Checked;
    Survey.Sex := TSex(SexComboBox.ItemIndex);
    Survey.Location := TLocation(LocationComboBox.ItemIndex);
    Survey.Education := TEducation(EducationComboBox.ItemIndex);
    Survey.Age := TAge(AgeComboBox.ItemIndex);
    Survey.Voted := VotedCheckbox.Checked;
  end;
end;
end.

