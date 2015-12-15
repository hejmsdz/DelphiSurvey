unit DataStructuresUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { enums present in the record type }
  TSex = (M, F);
  TLocation = (LessThan15K, Over15K, Over50K, Over100K, Over300K);
  TEducation = (Primary, Secondary, Higher);
  TAge = (_18_25, _26_35, _36_45, _46_60, Over60);

  { record managed by the app }
  TSurvey = record
    Supports: Boolean;
    Sex: TSex;
    Location: TLocation;
    Education: TEducation;
    Age: TAge;
    Voted: Boolean;
  end;

  { double linked list item }
  PListItem = ^TListItem;
  TListItem = record
    Survey: TSurvey;
    Prev: PListItem;
    Next: PListItem;
  end;

  { iterator class for convenient processing the list }
  TIterator = class
    public
      constructor Create(list: PListItem);
      function GetCurrent: PListItem;
      function GetCurrentItem: TSurvey;
      function Exists: Boolean;
      procedure Next;
    private
      Current: PListItem;
  end;

  { comparison function signature for sorting }
  FCompareSurveys = function(a, b: TSurvey): Integer;

  { the list }
  TCustomList = class
    public
      constructor Create;
      procedure Append(survey: TSurvey);
      procedure Remove(item: PListItem);
      procedure SelectSort(compare1, compare2: Integer; ascending: Boolean = true);
      procedure SwapItems(a, b: PListItem);
      procedure Clear;
      function Iterate: TIterator;
    private
      Head, Tail: PListItem;
  end;

  function CompareSupports(a, b: TSurvey): Integer;
  function CompareSex(a, b: TSurvey): Integer;
  function CompareLocation(a, b: TSurvey): Integer;
  function CompareEducation(a, b: TSurvey): Integer;
  function CompareAge(a, b: TSurvey): Integer;
  function CompareVoted(a, b: TSurvey): Integer;

const
  { human-readable enum (and bool) value names }
  BoolValues: array[Boolean] of String = ('no', 'yes');
  SexValues: array[TSex] of String = ('male', 'female');
  LocationValues: array[TLocation] of String = ('less than 15k', '15k - 50k', '50k - 100k', '100k - 300k', 'over 300k');
  EducationValues: array[TEducation] of String = ('primary', 'secondary', 'higher');
  AgeValues: array[TAge] of String = ('18 - 25', '26 - 35', '36 - 45', '46 - 60', '60+');
  { comparison functions }
  Comparators: array[0..5] of FCompareSurveys = (@CompareSupports, @CompareSex, @CompareLocation, @CompareEducation, @CompareAge, @CompareVoted);

implementation

{ initiate an iterator }
constructor TIterator.Create(list: PListItem);
begin
  Current := list;
end;

{ access current iterator pointer }
function TIterator.GetCurrent: PListItem;
begin
  Result := Current;
end;

{ access the record that the current pointer corresponds to }
function TIterator.GetCurrentItem: TSurvey;
begin
  Result := Current^.Survey;
end;

{ check whether a loop can be continued }
function TIterator.Exists: Boolean;
begin
  Result := (Current <> nil);
end;

{ proceed to the next item }
procedure TIterator.Next;
begin
  Current := Current^.Next;
end;

{ initiate a list }
constructor TCustomList.Create;
begin
  inherited;

  Head := nil;
  Tail := nil;
end;


{ add an item to the end of the list }
procedure TCustomList.Append(survey: TSurvey);
var
  item: PListItem;
begin
  New(item);
  item^.Survey := survey;
  item^.Prev := nil;
  item^.Next := nil;
  if Tail = nil then // list is empty
  begin
    Head := item;
    Tail := item;
  end
  else
  begin
    Tail^.Next := item;
    item^.Prev := Tail;
  end;
  Tail := item;
end;

{ remove an item from the list (by pointer) }
procedure TCustomList.Remove(item: PListItem);
begin
  if Head = item then // this is the first element
    Head := item^.Next;
  if Tail = item then // this is the last element
    Tail := item^.Prev;
  if item^.Prev <> nil then
    item^.Prev^.Next := item^.Next;
  Dispose(item);
end;

{ remove all the items of the list }
procedure TCustomList.Clear;
var
  tmp: PListItem;
  it: TIterator;
begin
  it := Iterate;

  while it.Exists do
  begin
    tmp := it.GetCurrent;
    it.Next;
    Dispose(tmp);
  end;
  Head := nil;
  Tail := nil;
end;

{ sort the list using two comparison functions, by Selection Sort }
procedure TCustomList.SelectSort(compare1, compare2: Integer; ascending: Boolean = true);
var
  it1, it2: TIterator;
  first, min: PListItem;
  one, d1, d2: Integer;
begin
  if ascending then one := 1 else one := -1;
  it1 := Iterate;
  while it1.Exists do
  begin
    first := it1.GetCurrent;
    min := first;

    it2 := TIterator.Create(it1.GetCurrent^.Next);
    while it2.Exists do
    begin
      d1 := Comparators[compare1](min^.Survey, it2.GetCurrentItem)*one;
      if compare2 >= 0 then
        d2 := Comparators[compare2](min^.Survey, it2.GetCurrentItem)*one
      else
        d2 := 0;

      if (d1 > 0) or ((d1 = 0) and (d2 > 0)) then min := it2.GetCurrent;
      it2.Next;
    end;

    if first <> min then SwapItems(first, min);

    it1.Next;
  end;
end;

{ swap values of the two pointers }
procedure TCustomList.SwapItems(a, b: PListItem);
var
  tmp: TSurvey;
begin
  tmp := a^.Survey;
  a^.Survey := b^.Survey;
  b^.Survey := tmp;
end;

{ create an iterator to the List }
function TCustomList.Iterate: TIterator;
begin
  Result := TIterator.Create(Head);
end;

{ comparison functions }
function CompareSupports(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Supports) - Ord(b.Supports);
end;

function CompareSex(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Sex) - Ord(b.Sex);
end;

function CompareLocation(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Location) - Ord(b.Location);
end;

function CompareEducation(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Education) - Ord(b.Education);
end;

function CompareAge(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Age) - Ord(b.Age);
end;

function CompareVoted(a, b: TSurvey): Integer;
begin
  Result := Ord(a.Voted) - Ord(b.Voted);
end;
end.
