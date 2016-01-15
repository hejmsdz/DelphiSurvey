program survey;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, SysUtils, Windows, // this includes the LCL widgetset
  Forms, MainWindowUnit
  { you can add units after this };

const
  GUID: PChar = '6a21a3e2-97fd-4cff-ab8d-4a12387fd8d1';

{$R *.res}

var
  MsgID: LongWord;
  Mutex : THandle;
begin
  MsgID := RegisterWindowMessage(GUID);
  try
    Mutex := CreateMutex(nil, False, GUID);
    if GetLastError = ERROR_ALREADY_EXISTS then
    begin
      SendMessage(HWND_BROADCAST, MsgID, 0, 0);
    end
    else
    begin
      Application.Title := 'Survey';
      RequireDerivedFormResource := True;
      Application.Initialize;
      Application.CreateForm(TMainWindow, MainWindow);
      MainWindow.MsgID := MsgID;
      Application.Run
    end;
  finally
    if Mutex <> 0 then CloseHandle(Mutex);
  end;
end.

