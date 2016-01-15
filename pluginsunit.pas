unit PluginsUnit;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, LCL, Menus, Dialogs, Windows, DataStructuresUnit;

type
  TAction = record
    Caption: PChar;
    ProcIndex: Integer;
  end;
  TActionArray = array of TAction;
  TGetActions = procedure(var Actions: TActionArray); stdcall;
  TCallable = function(List: TCustomList): Boolean; stdcall;
  TOnDataChanged = procedure of object;

  TPluginManager = class
    public
      Menu: TMenuItem;
      List: TCustomList;
      OnDataChanged: TOnDataChanged;
      constructor Create(ExtrasMenu: TMenuItem; RecList: TCustomList; OnChanged: TOnDataChanged);
      procedure Load(path: PChar);
  end;

  TPlugin = class
    public
      constructor Create(DLLHandle: HModule; Name: PChar; Actions: TActionArray; PluginManager: TPluginManager);
      procedure OnClick(Sender: TObject);
      procedure Unload(Sender: TObject);
    private
      DLL: HModule;
      PluginMenu: TMenuItem;
      Manager: TPluginManager;
  end;

implementation

constructor TPluginManager.Create(ExtrasMenu: TMenuItem; RecList: TCustomList; OnChanged: TOnDataChanged);
begin
  Menu := ExtrasMenu;
  List := RecList;
  OnDataChanged := OnChanged;
end;

procedure TPluginManager.Load(path: PChar);
var
  h: HModule;
  PName: ^PChar;
  PNumActions: ^Integer;
  GetActions: TGetActions;
  Actions: TActionArray;
begin
  h := LoadLibrary(path);
  if h <> 0 then
  begin
    PName := GetProcAddress(h, 'Name');
    PNumActions := GetProcAddress(h, 'NumActions');
    @GetActions := GetProcAddress(h, 'GetActions');

    if (PName = nil) or (@PNumActions = nil) or (@CreateMenu = nil) then
      raise Exception.Create('Cannot load the required procedures and values.');

    SetLength(Actions, PNumActions^);
    GetActions(Actions);

    TPlugin.Create(h, PName^, Actions, self);
  end
  else
  begin
    raise Exception.Create('');
  end;
end;

constructor TPlugin.Create(DLLHandle: HModule; Name: PChar; Actions: TActionArray; PluginManager: TPluginManager);
var
  MenuItem: TMenuItem;
  i: Integer;
begin
  DLL := DLLHandle;
  Manager := PluginManager;

  PluginMenu := TMenuItem.Create(Manager.Menu);
  PluginMenu.Caption := Name;
  PluginMenu.Name := 'Plugin'+Name+'MenuItem';
  for i:=0 to Length(Actions)-1 do
  begin
    MenuItem := TMenuItem.Create(PluginMenu);
    MenuItem.Caption := Actions[i].Caption;
    MenuItem.Name := 'Plugin'+Name+'MenuItem'+IntToStr(i);
    MenuItem.Tag := Actions[i].ProcIndex;
    MenuItem.OnClick := OnClick;
    PluginMenu.Add(MenuItem);
  end;
  PluginMenu.AddSeparator();
  MenuItem := TMenuItem.Create(PluginMenu);
  MenuItem.Caption := 'Unload';
  MenuItem.Name := 'Plugin'+Name+'UnloadMenuItem';
  MenuItem.OnClick := Unload;
  PluginMenu.Add(MenuItem);

  Manager.Menu.Add(PluginMenu);
end;

procedure TPlugin.OnClick(Sender: TObject);
var
  Proc: TCallable;
  Changed: Boolean;
begin
  @Proc := GetProcAddress(DLL, PChar(TMenuItem(Sender).Tag));

  try
    Changed := Proc(Manager.List);
  except
    on E: Exception do
      MessageBox(0, PChar('An error occurred! '+E.Message), 'Error', MB_ICONSTOP);
  end;

  if Changed then Manager.OnDataChanged;
end;

procedure TPlugin.Unload(Sender: TObject);
begin
  FreeLibrary(DLL);
  PluginMenu.Destroy;
end;

end.

