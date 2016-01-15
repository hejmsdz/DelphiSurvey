library csv;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Windows, DataStructuresUnit, LCL, Dialogs;
  { you can add units after this }

const
  Name: PChar = 'CSV';
  NumActions: Integer = 2;

function CSVImport(List: TCustomList): Boolean; stdcall;
var
  rec: TSurvey;
  row: String;
  Tokens: TStrings;
  Dialog: TOpenDialog;
  CSVFile: TextFile;
begin
  Dialog := TOpenDialog.Create(nil);
  Dialog.Title := 'Import from CSV';
  Dialog.Filter := 'CSV|*.csv';
  Dialog.DefaultExt := 'csv';

  Tokens := TStringList.Create;

  if Dialog.Execute then
  begin
    try
      AssignFile(CSVFile, Dialog.FileName);
      Reset(CSVFile);

      while not Eof(CSVFile) do
      begin
        Tokens.Clear;
        ReadLn(CSVFile, row);
        if ExtractStrings([',', ';'], [], PChar(row), Tokens) >= 6 then
        begin
          rec := RecordFromStrings(Tokens[0], Tokens[1], Tokens[2], Tokens[3], Tokens[4], Tokens[5]);
          List.Append(rec);
          Result := True;
        end
        else raise Exception.Create('Not enough fields in the line.');
      end;
    finally
      CloseFile(CSVFile);
    end;
  end;
end;

function CSVExport(List: TCustomList): Boolean; stdcall;
var
  it: TIterator;
  rec: TSurvey;
  row: String;
  Dialog: TSaveDialog;
  CSVFile: TextFile;
begin
  Dialog := TSaveDialog.Create(nil);
  Dialog.Title := 'Export to CSV';
  Dialog.Filter := 'CSV|*.csv';
  Dialog.DefaultExt := 'csv';
  if Dialog.Execute then
  begin
    try
      AssignFile(CSVFile, Dialog.FileName);
      Rewrite(CSVFile);

      it := List.Iterate;
      while it.Exists do
      begin
        rec := it.GetCurrentItem;
        row := BoolValues[rec.Supports] + ';' + SexValues[rec.Sex] + ';' + LocationValues[rec.Location] + ';' + EducationValues[rec.Education] + ';' + AgeValues[rec.Age] + ';' + BoolValues[rec.Voted];
        WriteLn(CSVFile, row);
        it.Next;
      end;
    finally
      CloseFile(CSVFile);
    end;
    MessageBox(0, 'Records have been exported successfully!', 'CSV plugin', MB_OK);
  end;
  Result := false;
end;

procedure GetActions(var Actions: TActionArray); stdcall;
begin
  Actions[0].Caption := 'Import';
  Actions[0].ProcIndex := 2;
  Actions[1].Caption := 'Export';
  Actions[1].ProcIndex := 3;
end;

exports
  Name,
  NumActions,
  GetActions,
  CSVImport index 2,
  CSVExport index 3;

begin
end.

