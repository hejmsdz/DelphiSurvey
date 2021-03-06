unit MainWindowUnit;

{$mode objfpc}{$H+}{$D-}

interface

uses
  Classes, SysUtils, FileUtil, StrUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, Menus, ActnList, LCLType, ExtCtrls, StdCtrls, Windows, LMessages,

  DataStructuresUnit, FileIOUnit, PluginsUnit,

  RecordWindowUnit, SortWindowUnit, AboutWindowUnit;

type

  //procedure ShowWindow; stdcall external 'Example_lib.dll' name 'ShowWindow';
  { main application window }

  { TMainWindow }

  TMainWindow = class(TForm)
    FilterGroupBox: TGroupBox;
    AgeCheckGroup: TCheckGroup;
    LoadDLLItem: TMenuItem;
    DLLDialog: TOpenDialog;
    RecordSortMenuItem: TMenuItem;
    ViewStatusBarMenuItem: TMenuItem;
    ViewClearFiltersMenuItem: TMenuItem;
    SexCheckGroup: TCheckGroup;
    EducationCheckGroup: TCheckGroup;
    StatusBar: TStatusBar;
    SupportsCheckGroup: TCheckGroup;
    SaveDialog: TSaveDialog;
    LocationCheckGroup: TCheckGroup;
    VotedCheckGroup: TCheckGroup;
    SurveyListView: TListView;
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    ExtrasMenu: TMenuItem;
    HelpMenu: TMenuItem;
    FileNewMenuItem: TMenuItem;
    FileOpenMenuItem: TMenuItem;
    FileSaveMenuItem: TMenuItem;
    FileQuitMenuItem: TMenuItem;
    HelpAboutMenuItem: TMenuItem;
    OpenDialog: TOpenDialog;
    RecordRemoveMenuItem: TMenuItem;
    RecordModifyMenuItem: TMenuItem;
    RecordCreateMenuItem: TMenuItem;
    ViewMenu: TMenuItem;
    RecordMenu: TMenuItem;
    procedure FileNewMenuItemClick(Sender: TObject);
    procedure ApplyFilters(Sender: TObject; Index: integer);
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FileOpenMenuItemClick(Sender: TObject);
    procedure FileQuitMenuItemClick(Sender: TObject);
    procedure FileSaveMenuItemClick(Sender: TObject);
    procedure HelpAboutMenuItemClick(Sender: TObject);
    procedure LoadDLLItemClick(Sender: TObject);
    procedure RecordCreateMenuItemClick(Sender: TObject);
    procedure RecordModifyMenuItemClick(Sender: TObject);
    procedure RecordRemoveMenuItemClick(Sender: TObject);
    procedure RecordSortMenuItemClick(Sender: TObject);
    procedure SurveyListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure ViewClearFiltersMenuItemClick(Sender: TObject);
    procedure ViewStatusBarMenuItemClick(Sender: TObject);
  private
    List: TCustomList;
    DataChanged: Boolean;
    Filename: String;
    Plugins: TPluginManager;
    procedure OpenFile(fname: String);
    function AskToSaveChanges: Integer;
    function GetSelectedItem: PListItem;
    function CheckFilters(survey: TSurvey): Boolean;
    procedure UpdateTitle;
    procedure ResetFilters;
    procedure ResetFilter(group: TCheckGroup);
    procedure ReloadFromList;
    procedure SetDataChanged;
  public
    MsgID: LongWord;
    procedure WndProc(var Msg: TLMessage); override;
  end;
var
  MainWindow: TMainWindow;

implementation
{$R *.lfm}

{ TMainWindow }

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  List := TCustomList.Create;
  DataChanged := false;
  Filename := '';
  ReloadFromList;

  { initialize plugin engine }
  Plugins := TPluginManager.Create(ExtrasMenu, List, @SetDataChanged);

  SupportsCheckGroup.Items.AddStrings(BoolValues);
  SexCheckGroup.Items.AddStrings(SexValues);
  LocationCheckGroup.Items.AddStrings(LocationValues);
  EducationCheckGroup.Items.AddStrings(EducationValues);
  AgeCheckGroup.Items.AddStrings(AgeValues);
  VotedCheckGroup.Items.AddStrings(BoolValues);

  ResetFilters;

  { open a file from a command line parameter, if available }
  if ParamCount >= 1 then
    OpenFile(ParamStr(1));
end;

function TMainWindow.AskToSaveChanges: Integer;
begin
  Result := 0;
  if DataChanged then
  begin
    Result := Application.MessageBox('Do you want to save changes?', 'Save changes', MB_ICONQUESTION+MB_YESNOCANCEL);
    if Result = IDYES then FileSaveMenuItemClick(TObject.Create);
  end;
end;

{ on close, ask whether to save changes and prevent from closing if necessary }
procedure TMainWindow.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  if AskToSaveChanges = IDCANCEL then CanClose := false;
end;

procedure TMainWindow.FileNewMenuItemClick(Sender: TObject);
begin
  if AskToSaveChanges = IDCANCEL then Exit;

  List.Clear;
  DataChanged := false;
  Filename := '';
  ReloadFromList;
end;

{ reload the list view when a filter is checked/unchecked }
procedure TMainWindow.ApplyFilters(Sender: TObject; Index: integer);
var
  group: TCheckGroup;
  numChecked, i: Integer;
begin
  {if all the items in one group have been unchecked, fix it,
  so that the list doesn't become empty}
  group := TCheckGroup(Sender);
  if not group.Checked[Index] then
  begin
    numChecked := 0;
    for i := 0 to group.Items.Count-1 do
      if group.Checked[i] then
        Inc(numChecked);

    if numChecked = 0 then
      group.Checked[(Index+1) mod group.Items.Count] := true;
  end;

  ReloadFromList;
end;

{ recalculate the layout when the window resizes }
procedure TMainWindow.FormChangeBounds(Sender: TObject);
begin
  SurveyListView.Width := Width - FilterGroupBox.Width;
  SurveyListView.Height := Height - 55;
  FilterGroupBox.Height := Height - 55;
  FilterGroupBox.Left := Width - FilterGroupBox.Width + 8;
end;

{ load a file into the app }
procedure TMainWindow.OpenFile(fname: String);
var
  num: Integer;
begin
  try
    List.Clear;
    ReadFile(fname, List, num);
    Filename := fname;
    DataChanged := false;
    ReloadFromList;
  except
    on E: Exception do
        Application.MessageBox(PChar('Failed to open the file! '+E.Message), 'Error', MB_ICONSTOP);
  end;
end;

procedure TMainWindow.FileOpenMenuItemClick(Sender: TObject);
begin
  if AskToSaveChanges = IDCANCEL then Exit;

  if OpenDialog.Execute then OpenFile(OpenDialog.FileName);
end;

procedure TMainWindow.FileQuitMenuItemClick(Sender: TObject);
begin
  Close
end;

{ persist the database to the current file or to a specified one if the file has not been saved yet }
procedure TMainWindow.FileSaveMenuItemClick(Sender: TObject);
var
  num: Integer;
begin
  if Filename = '' then
    if SaveDialog.Execute then Filename := SaveDialog.FileName else Exit;

  try
    WriteToFile(Filename, List, num);
    DataChanged := false;
  except
    on E: Exception do
      Application.MessageBox(PChar('Failed to save the file! '+E.Message), 'Error', MB_ICONSTOP);
  end;
  UpdateTitle;
end;

{ display the about window }
procedure TMainWindow.HelpAboutMenuItemClick(Sender: TObject);
var
  win: TAboutWindow;
begin
  win := TAboutWindow.Create(self);
  win.ShowModal;
  win.Free;
end;

procedure TMainWindow.LoadDLLItemClick(Sender: TObject);
begin
  try
    if DLLDialog.Execute then Plugins.Load(PChar(DLLDialog.FileName));
  except
    on E: Exception do
      Application.MessageBox(PChar('Failed to load the library! '+E.Message), 'Error', MB_ICONSTOP);
  end;
end;

{ add a new item }
procedure TMainWindow.RecordCreateMenuItemClick(Sender: TObject);
var
  win: TRecordWindow;
begin
  win := TRecordWindow.Create(self);
  if win.ShowModal = mrOK then
  begin
    List.Append(win.Survey);

    DataChanged := True;
    ReloadFromList;
  end;
  win.Free;
end;

{ modify the selected item }
procedure TMainWindow.RecordModifyMenuItemClick(Sender: TObject);
var
  item: PListItem;
  win: TRecordWindow;
begin
  item := GetSelectedItem;
  if item = nil then Exit;

  win := TRecordWindow.Create(self);
  win.SetSurvey(item^.Survey);
  if win.ShowModal = mrOK then
  begin
    item^.Survey := win.Survey;

    DataChanged := True;
    ReloadFromList;
  end;
  win.Free;
end;

{ remove the selected item }
procedure TMainWindow.RecordRemoveMenuItemClick(Sender: TObject);
var
  item: PListItem;
begin
  item := GetSelectedItem;
  if item = nil then Exit;
  if Application.MessageBox('Are you sure?', 'Remove a record', MB_ICONQUESTION+MB_YESNO) = IDYES then
  begin
    List.Remove(item);
    DataChanged := true;
    ReloadFromList;
  end;
end;

procedure TMainWindow.RecordSortMenuItemClick(Sender: TObject);
var
  win: TSortWindow;
begin
  win := TSortWindow.Create(self);
  if win.ShowModal = mrOK then
  begin
    try
      List.SelectSort(win.FirstCriterion.ItemIndex, win.SecondCriterion.ItemIndex, win.Direction.ItemIndex=0);
      DataChanged := True;
      ReloadFromList;
    except
      on E: Exception do
        Application.MessageBox(PChar('Failed to sort the list! '+E.Message), 'Error', MB_ICONSTOP);
    end;
  end;
  win.Free;
end;

{ on selecting/deselecting a list item, enable/disable the modify and remove menu items }
procedure TMainWindow.SurveyListViewSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  RecordModifyMenuItem.Enabled := Selected;
  RecordRemoveMenuItem.Enabled := Selected;
end;

{ react to the "clear filters" option }
procedure TMainWindow.ViewClearFiltersMenuItemClick(Sender: TObject);
begin
  ResetFilters;
  ReloadFromList;
end;

{ toggle status bar }
procedure TMainWindow.ViewStatusBarMenuItemClick(Sender: TObject);
begin
  StatusBar.Visible := ViewStatusBarMenuItem.Checked;
end;

{ get the pointer that the selected list item corresponds to }
function TMainWindow.GetSelectedItem: PListItem;
begin
  Result := nil;
  if SurveyListView.Selected <> nil then
    Result := PListItem(SurveyListView.Selected.Data);
end;

procedure TMainWindow.UpdateTitle;
begin
  Caption := 'Survey - ';
  if Filename = '' then
    Caption := Caption + 'Untitled'
  else
    Caption := Caption + ExtractFileName(Filename);
  if DataChanged then Caption := Caption + '*';
end;

procedure TMainWindow.ResetFilters;
begin
  ResetFilter(SupportsCheckGroup);
  ResetFilter(SexCheckGroup);
  ResetFilter(LocationCheckGroup);
  ResetFilter(EducationCheckGroup);
  ResetFilter(AgeCheckGroup);
  ResetFilter(VotedCheckGroup);
end;

procedure TMainWindow.ResetFilter(group: TCheckGroup);
var
  i: Integer;
begin
  for i := 0 to group.Items.Count-1 do
    group.Checked[i] := true;
end;

function TMainWindow.CheckFilters(survey: TSurvey): Boolean;
begin
  Result := false;
  if SupportsCheckGroup.Checked[Integer(survey.Supports)] then
  if SexCheckGroup.Checked[Ord(survey.Sex)] then
  if LocationCheckGroup.Checked[Ord(survey.Location)] then
  if EducationCheckGroup.Checked[Ord(survey.Education)] then
  if AgeCheckGroup.Checked[Ord(survey.Age)] then
  if VotedCheckGroup.Checked[Integer(survey.Voted)] then
    Result := true;
end;

procedure TMainWindow.SetDataChanged;
begin
  DataChanged := true;
  ReloadFromList;
end;

procedure TMainWindow.ReloadFromList;
var
  it: TIterator;
  survey: TSurvey;
  matched: Integer;
begin
  UpdateTitle;
  it := List.Iterate();
  SurveyListView.Items.Clear;
  matched := 0;

  while it.Exists do
  begin
    survey := it.GetCurrentItem;
    if CheckFilters(survey) then
    begin
      Inc(matched);
      with SurveyListView.Items.Add do
      begin
        Data := it.getCurrent;
        Caption := IntToStr(it.Counter+1);
        SubItems.Add(BoolValues[survey.Supports]);
        SubItems.Add(SexValues[survey.Sex]);
        SubItems.Add(LocationValues[survey.Location]);
        SubItems.Add(EducationValues[survey.Education]);
        SubItems.Add(AgeValues[survey.Age]);
        SubItems.Add(BoolValues[survey.Voted]);
      end;
    end;
    it.Next;
  end;

  RecordSortMenuItem.Enabled := (it > 1);

  if it <= 0 then
    StatusBar.Panels[0].Text := 'The database is empty'
  else if it > matched then
    StatusBar.Panels[0].Text := 'Showing '+IntToStr(matched)+' out of '+IntToStr(it.Counter)+' row'+IfThen(it.Counter=1, '', 's')
  else
    StatusBar.Panels[0].Text := IntToStr(it.Counter)+' row'+IfThen(it.Counter=1, '', 's')+' in the database';
end;

procedure TMainWindow.WndProc(var Msg: TLMessage);
begin
  inherited WndProc(Msg);
  if Msg.msg = MsgID then
  begin
    Application.Restore;
    SetForegroundWindow(Application.MainForm.Handle);
  end;
end;
end.
