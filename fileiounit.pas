unit FileIOUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil,
  DataStructuresUnit;

type
  TSurveyFile = File of TSurvey;

procedure ReadFile(const filename: String; var list: TCustomList; out num: Integer);
procedure WriteToFile(const filename: String; list: TCustomList; out num: Integer);

implementation

{ load a file into a linked list }
procedure ReadFile(const filename: String; var list: TCustomList; out num: Integer);
var
  inFile: TSurveyFile;
  survey: TSurvey;
begin
  num := 0;

  try
    AssignFile(inFile, filename);
    Reset(inFile);

    while not Eof(inFile) do
    begin
      Read(inFile, survey);
      list.Append(survey);
      Inc(num);
    end;
  finally
    CloseFile(inFile);
  end;
end;

{ store all the records from a list into a file }
procedure WriteToFile(const filename: String; list: TCustomList; out num: Integer);
var
  outFile: TSurveyFile;
  it: TIterator;
begin
  num := 0;
  it := list.Iterate();

  try
    AssignFile(outFile, filename);
    Rewrite(outFile);

    while it.Exists do
    begin
      Write(outFile, it.GetCurrentItem);
      it.Next;
      Inc(num);
    end;
  finally
    CloseFile(outFile);
  end;
end;

end.

