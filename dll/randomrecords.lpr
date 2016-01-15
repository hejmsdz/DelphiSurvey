library randomrecords;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DataStructuresUnit;
  { you can add units after this }

const
  Name: PChar = 'RandomRecords';
  NumActions: Integer = 1;

function RandomRecord: TSurvey;
begin
  Result.Supports := (Random > 0.5);
  Result.Sex := TSex(Random(Succ(Ord(High(TSex)))));
  Result.Location := TLocation(Random(Succ(Ord(High(TLocation)))));
  Result.Education := TEducation(Random(Succ(Ord(High(TEducation)))));
  Result.Age := TAge(Random(Succ(Ord(High(TAge)))));
  Result.Voted := (Random > 0.5);
end;

function Generate(List: TCustomList): Boolean; stdcall;
var
  i, n: Integer;
begin
  Result := False;
  n := 10;
  Randomize;

  for i:=1 to n do
  begin
    List.Append(RandomRecord);
    Result := True;
  end;
end;

procedure GetActions(var Actions: TActionArray); stdcall;
begin
  Actions[0].Caption := 'Generate';
  Actions[0].ProcIndex := 2;
end;

exports
  Name,
  NumActions,
  GetActions,
  Generate index 2;

begin
end.


